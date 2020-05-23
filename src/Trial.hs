{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Trial Data Type
-}

module Trial
       ( Trial (..)
       , Fatality (..)

         -- * 'Maybe'
       , maybeToTrial
       , trialToMaybe
       ) where

import Control.Applicative (Alternative (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))


data Fatality
    = W
    | E
    deriving stock (Show, Eq, Enum, Bounded)

instance Semigroup Fatality where
    E <> _ = E
    W <> x = x

fatalityE :: [(Fatality, e)] -> [(Fatality, e)]
fatalityE = map (first (const E))

fatalityW :: [(Fatality, e)] -> [(Fatality, e)]
fatalityW = map (first (const W))

data Trial e a
    = Fiasco [(Fatality, e)]
    | Result [e] a
    deriving stock (Show, Eq)

instance Semigroup (Trial e a) where
    Fiasco e1   <> Fiasco e2   = Fiasco $ e1 <> e2
    Fiasco e1   <> Result e2 a = Result (map snd e1 <> e2) a
    Result e1 a <> Fiasco e2   = Result (e1 <> map snd e2) a
    Result e1 _ <> Result e2 b = Result (e1 <> e2) b

instance Functor (Trial e) where
    fmap :: (a -> b) -> Trial e a -> Trial e b
    fmap _ (Fiasco e)   = Fiasco e
    fmap f (Result e a) = Result e $ f a

instance Applicative (Trial e) where
    pure :: a -> Trial e a
    pure = Result []

    (<*>) :: Trial e (a -> b) -> Trial e a -> Trial e b
    Fiasco e1 <*> Fiasco e2   = Fiasco $ e1 <> e2
    Fiasco e1 <*> Result e2 _ = Fiasco $ e1 <> map (W,) e2
    Result e1 _ <*> Fiasco e2 = Fiasco $ map (W,) e1 <> e2
    Result e1 f <*> Result e2 a = Result (e1 <> e2) $ f a

instance Alternative (Trial e) where
    empty :: Trial e a
    empty = Fiasco []

    (<|>) :: Trial e a -> Trial e a -> Trial e a
    r@Result{} <|> _ = r
    _ <|> r@Result{} = r
    (Fiasco e1) <|> (Fiasco e2) = Fiasco $ e1 <> e2

instance Bifunctor Trial where
    bimap :: (e1 -> e2) -> (a -> b) -> Trial e1 a -> Trial e2 b
    bimap ef _ (Fiasco es)   = Fiasco $ map (second ef) es
    bimap ef af (Result e a) = Result (map ef e) (af a)

instance Bifoldable Trial where
    bifoldMap :: (Monoid m) => (e -> m) -> (a -> m) -> Trial e a -> m
    bifoldMap ef _ (Fiasco es)    = foldMap (ef . snd) es
    bifoldMap ef ea (Result es a) = foldMap ef es <> ea a

instance Bitraversable Trial where
    bitraverse :: (Applicative f) => (e1 -> f e2) -> (a -> f b) -> Trial e1 a -> f (Trial e2 b)
    bitraverse ef _ (Fiasco es)    = Fiasco <$> traverse (traverse ef) es
    bitraverse ef ea (Result es a) = Result <$> traverse ef es <*> ea a

maybeToTrial :: e -> Maybe a -> Trial e a
maybeToTrial e = \case
    Just a -> pure a
    Nothing -> Fiasco [(E, e)]

trialToMaybe :: Trial e a -> Maybe a
trialToMaybe (Result _ a) = Just a
trialToMaybe (Fiasco _)   = Nothing
