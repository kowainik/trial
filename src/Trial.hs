{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Trial Data Type
-}

module Trial
       ( Trial (..)
       , TaggedTrial
       , Fatality (..)

         -- * 'Maybe' combinators
       , maybeToTrial
       , trialToMaybe
         -- * 'Either' combinators
       , eitherToTrial
       , trialToEither

         -- * Tag
       , withTag
       , unTag
       ) where

import Control.Applicative (Alternative (..), Applicative (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.DList (DList)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup (..))
import Data.String (IsString (..))
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, symbolVal)

import qualified Data.DList as DL


data Fatality
    = W
    | E
    deriving stock (Show, Eq, Enum, Bounded)

withW :: Functor f => f e -> f (Fatality, e)
withW = fmap (W,)

data Trial e a
    = Fiasco (DList (Fatality, e))
    | Result (DList e) a
    deriving stock (Show, Eq)

type TaggedTrial tag a = Trial tag (tag, a)

instance Semigroup (Trial e a) where
    (<>) :: Trial e a -> Trial e a -> Trial e a
    Fiasco e1   <> Fiasco e2   = Fiasco $ e1 <> e2
    Fiasco e1   <> Result e2 a = Result (DL.map snd e1 <> e2) a
    Result e1 a <> Fiasco e2   = Result (e1 <> DL.map snd e2) a
    Result e1 _ <> Result e2 b = Result (e1 <> e2) b
    {-# INLINE (<>) #-}

    sconcat :: NonEmpty (Trial e a) -> Trial e a
    sconcat (x :| xs) = foldl' (<>) x xs
    {-# INLINE sconcat #-}

instance Functor (Trial e) where
    fmap :: (a -> b) -> Trial e a -> Trial e b
    fmap _ (Fiasco e)   = Fiasco e
    fmap f (Result e a) = Result e $ f a
    {-# INLINE fmap #-}

    (<$) :: a -> Trial e b -> Trial e a
    _ <$ Fiasco e   = Fiasco e
    a <$ Result e _ = Result e a
    {-# INLINE (<$) #-}

instance Applicative (Trial e) where
    pure :: a -> Trial e a
    pure = Result DL.empty
    {-# INLINE pure #-}

    (<*>) :: Trial e (a -> b) -> Trial e a -> Trial e b
    Fiasco e1 <*> trial = Fiasco $ case trial of
        Fiasco e2   -> e1 <> e2
        Result e2 _ -> e1 <> withW e2
    Result e1 _ <*> Fiasco e2   = Fiasco (withW e1 <> e2)
    Result e1 f <*> Result e2 a = Result (e1 <> e2) (f a)
    {-# INLINE (<*>) #-}

    (*>) :: Trial e a -> Trial e b -> Trial e b
    Fiasco e1 *> trial = Fiasco $ case trial of
        Fiasco e2   -> e1 <> e2
        Result e2 _ -> e1 <> withW e2
    Result e1 _ *> Fiasco e2   = Fiasco (withW e1 <> e2)
    Result e1 _ *> Result e2 b = Result (e1 <> e2) b
    {-# INLINE (*>) #-}

    (<*) :: Trial e a -> Trial e b -> Trial e a
    Fiasco e1 <* trial = Fiasco $ case trial of
        Fiasco e2   -> e1 <> e2
        Result e2 _ -> e1 <> withW e2
    Result e1 _ <* Fiasco e2   = Fiasco (withW e1 <> e2)
    Result e1 a <* Result e2 _ = Result (e1 <> e2) a
    {-# INLINE (<*) #-}

    liftA2 :: (a -> b -> c) -> Trial e a -> Trial e b -> Trial e c
    liftA2 _ (Fiasco e1) trial = Fiasco $ case trial of
        Fiasco e2   -> e1 <> e2
        Result e2 _ -> e1 <> withW e2
    liftA2 _ (Result e1 _) (Fiasco e2)   = Fiasco (withW e1 <> e2)
    liftA2 f (Result e1 a) (Result e2 b) = Result (e1 <> e2) (f a b)
    {-# INLINE liftA2 #-}

instance Alternative (Trial e) where
    empty :: Trial e a
    empty = Fiasco DL.empty
    {-# INLINE empty #-}

    (<|>) :: Trial e a -> Trial e a -> Trial e a
    r@Result{} <|> _ = r
    _ <|> r@Result{} = r
    (Fiasco e1) <|> (Fiasco e2) = Fiasco (e1 <> e2)
    {-# INLINE (<|>) #-}

instance Bifunctor Trial where
    bimap :: (e1 -> e2) -> (a -> b) -> Trial e1 a -> Trial e2 b
    bimap ef _ (Fiasco es)   = Fiasco (DL.map (second ef) es)
    bimap ef af (Result e a) = Result (DL.map ef e) (af a)
    {-# INLINE bimap #-}

instance Bifoldable Trial where
    bifoldMap :: (Monoid m) => (e -> m) -> (a -> m) -> Trial e a -> m
    bifoldMap ef _ (Fiasco es)    = foldMap (ef . snd) es
    bifoldMap ef ea (Result es a) = foldMap ef es <> ea a
    {-# INLINE bifoldMap #-}

instance Bitraversable Trial where
    bitraverse :: (Applicative f) => (e1 -> f e2) -> (a -> f b) -> Trial e1 a -> f (Trial e2 b)
    bitraverse ef _ (Fiasco es)    = Fiasco <$> traverseDList (traverse ef) es
    bitraverse ef ea (Result es a) = Result <$> traverseDList ef es <*> ea a
    {-# INLINE bitraverse #-}

{- 'DList' doesn't have a 'Traversable' instance -}
traverseDList :: (Applicative f) => (a -> f b) -> DList a -> f (DList b)
traverseDList f = fmap DL.fromList . traverse f . DL.toList
{-# INLINE traverseDList #-}

maybeToTrial :: e -> Maybe a -> Trial e a
maybeToTrial e = \case
    Just a  -> pure a
    Nothing -> Fiasco (DL.singleton (E, e))

trialToMaybe :: Trial e a -> Maybe a
trialToMaybe (Result _ a) = Just a
trialToMaybe (Fiasco _)   = Nothing

eitherToTrial :: Either e a -> Trial e a
eitherToTrial (Right a) = pure a
eitherToTrial (Left e)  = Fiasco (DL.singleton (E, e))

trialToEither :: Monoid e => Trial e a -> Either e a
trialToEither (Result _ a) = Right a
trialToEither (Fiasco es)  = Left $ foldl' (<>) mempty $ DL.map snd es

withTag :: tag -> Trial tag a -> TaggedTrial tag a
withTag tag = fmap (tag,)

unTag :: TaggedTrial tag a -> Trial tag a
unTag (Fiasco e)          = Fiasco e
unTag (Result e (tag, a)) = Result (DL.snoc e tag) a

instance
       ( HasField label r (Trial tag (tag, a))
       , IsString tag
       , Semigroup tag
       , KnownSymbol label
       )
    => IsLabel label (r -> Trial tag a)
  where
    fromLabel :: r -> Trial tag a
    fromLabel r = let fieldName = fromString $ symbolVal (Proxy @label) <> " is set through the source: " in
        case getField @label r of
            Fiasco e          -> Fiasco e
            Result e (tag, a) -> Result (DL.snoc e $ fieldName <> tag) a
    {-# INLINE fromLabel #-}
