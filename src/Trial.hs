{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Trial Data Type
-}

module Trial
       ( Trial (..)

         -- * 'Maybe'
       , maybeToTrial
       , trialToMaybe
       ) where


data Trial e a
    = Fiasco [e]
    | Result [e] a
    deriving stock (Show, Eq)

instance Semigroup (Trial e a) where
    Fiasco e1 <> Fiasco e2     = Fiasco $ e1 <> e2
    Fiasco e1 <> Result e2 a   = Result (e1 <> e2) a
    Result e1 a <> Fiasco e2   = Result (e1 <> e2) a
    Result e1 _ <> Result e2 b = Result (e1 <> e2) b

instance Functor (Trial e) where
    fmap :: (a -> b) -> Trial e a -> Trial e b
    fmap _ (Fiasco e)   = Fiasco e
    fmap f (Result e a) = Result e $ f a

instance Applicative (Trial e) where
    pure :: a -> Trial e a
    pure = Result []

    (<*>) :: Trial e (a -> b) -> Trial e a -> Trial e b
    Fiasco e1 <*> Fiasco e2 = Fiasco $ e1 <> e2
    Fiasco e1 <*> Result e2 _ = Fiasco $ e1 <> e2
    Result e1 _ <*> Fiasco e2 = Fiasco $ e1 <> e2
    Result e1 f <*> Result e2 a = Result (e1 <> e2) $ f a

maybeToTrial :: e -> Maybe a -> Trial e a
maybeToTrial e = \case
    Just a -> pure a
    Nothing -> Fiasco [e]

trialToMaybe :: Trial e a -> Maybe a
trialToMaybe (Result _ a) = Just a
trialToMaybe (Fiasco _)   = Nothing
