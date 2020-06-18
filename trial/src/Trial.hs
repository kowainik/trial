{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

The 'Trial' Data Structure is a 'Either'-like structure that keeps
events history inside. The data type allows to keep track of the
'Fatality' level of each such event entry ('Warning' or 'Error').

'Trial' has two constructors:

* 'Fiasco': stores the list of events with the explicit 'Fatality'
  level; at least one event has level 'Error'
* 'Result': stores the final result and the list of events where each
  event has implicit 'Fatality' level 'Warning'

@trial@ implements the composable interface for creating and combining
values of type 'Trial', so the history of all events is stored
inside. Fundamental algebraic instances provide the following main
features:

* 'Semigroup': take the last 'Result' and combine all events.
* 'Applicative': return 'Fiasco', if at least one value if 'Fiasco',
  combine all events.
* 'Alternative': return first 'Result', also combine all events for
  all 'Trial's before this 'Result'.
-}

module Trial
       ( -- * Data structures
         Trial (..)
       , TaggedTrial
         -- ** 'Fatality'
       , Fatality
       , pattern Warning
       , pattern Error


         -- * Smart constructors
       , fiasco
       , fiascos
       , result
         -- * Combinators
       , alt
       , isFiasco
       , isResult
       , whenResult
       , whenResult_
       , whenFiasco
       , whenFiasco_

         -- * Work with Lists
         -- $patternList
       , pattern FiascoL
       , pattern ResultL
       , getTrialInfo
       , fiascoErrors
       , fiascoWarnings
       , resultWarnings
       , anyWarnings
       , dlistToList


         -- * 'Maybe' combinators
       , maybeToTrial
       , trialToMaybe
         -- * 'Either' combinators
       , eitherToTrial
       , trialToEither

         -- * Tag
       , withTag
       , unTag

         -- * Pretty printing
       , prettyFatality
       , prettyTrial
       , prettyTrialWith
       , prettyTaggedTrial
       , prettyTaggedTrialWith

         -- * Configuration helpers
         -- $phase
       , Phase (..)
       , (:-)
       , (::-)
       ) where

import Control.Applicative (Alternative (..), Applicative (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.DList (DList)
import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup (..))
import Data.String (IsString (..))
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, symbolVal)

import qualified Colourista as C
import qualified Colourista.Short as C
import qualified Data.DList as DL
import qualified Data.List.NonEmpty as NE


{- | Severity of the event in history.

* 'Error': fatal error that led to the final 'Fiasco'
* 'Warning': non-essential error, which didn't affect the result

You can't create values of type 'Fatality', you can only pattern-match
on them. 'Trial' smart constructors and instances take care of
assigning proper 'Fatality' values.

Use 'Warning' and 'Error' Pattern Synonyms to pattern match on
'Fatality':

>>> :{
showFatality :: Fatality -> String
showFatality Warning = "Warning"
showFatality Error   = "Error"
:}

@since 0.0.0.0
-}
data Fatality
    = W
    | E
    deriving stock (Show, Eq, Enum, Bounded)

{- | 'Warning' pattern synonym.

@since 0.0.0.0
-}
pattern Warning :: Fatality
pattern Warning <- W

{- | 'Error' pattern synonym.

@since 0.0.0.0
-}
pattern Error :: Fatality
pattern Error <- E

{-# COMPLETE Warning, Error #-}

withW :: Functor f => f e -> f (Fatality, e)
withW = fmap (W,)
{-# INLINE withW #-}

{- | 'Trial' is a data type that stores history of all events happened
with a value. In addition, each event is associated with the
'Fatality' level that indicates whether the event is fatal or not.

API provided by @trial@ guarantees the following property:

* If the final value is 'Fiasco', it is either an empty list or a list
  with at least one event with the 'Fatality' level 'Error'.

@since 0.0.0.0
-}
data Trial e a
    -- | Stores list of events with the explicit 'Fatality' level.
    = Fiasco (DList (Fatality, e))
    -- | Store list of events and the final result.
    | Result (DList e) a
    deriving stock (Show, Eq)

{- | In addition to usual 'Trial' capabilities, 'TaggedTrial' allows
attaching a @tag@ to the resulting value, so you can track which event
helped to obtain a value.

@since 0.0.0.0
-}
type TaggedTrial tag a = Trial tag (tag, a)

{- | Combine two 'Trial' values. Returns 'Result' if at least one
argument is 'Result'.

Let's create some default values:

>>> f1 = fiasco "Not initialised..."
>>> f2 = fiasco "Parsing error!"
>>> r1 = result "r1: From CLI" 5
>>> r2 = result "r2: Default" 42

And here is how combination of those values look like:

>>> f1 <> f2
Fiasco (fromList [(E,"Not initialised..."),(E,"Parsing error!")])
>>> f1 <> r1
Result (fromList ["Not initialised...","r1: From CLI"]) 5
>>> f2 <> r2
Result (fromList ["Parsing error!","r2: Default"]) 42
>>> r1 <> r2
Result (fromList ["r1: From CLI","r2: Default"]) 42
>>> f1 <> r1 <> f2 <> r2
Result (fromList ["Not initialised...","r1: From CLI","Parsing error!","r2: Default"]) 42

@since 0.0.0.0
-}
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

-- | @since 0.0.0.0
instance Functor (Trial e) where
    fmap :: (a -> b) -> Trial e a -> Trial e b
    fmap _ (Fiasco e)   = Fiasco e
    fmap f (Result e a) = Result e $ f a
    {-# INLINE fmap #-}

    (<$) :: a -> Trial e b -> Trial e a
    _ <$ Fiasco e   = Fiasco e
    a <$ Result e _ = Result e a
    {-# INLINE (<$) #-}

{- | Combine two 'Trial's but recording all 'Result' events inside
'Fiasco' as 'Warning's.

>>> fiasco "No default" <*> fiasco "No config"
Fiasco (fromList [(E,"No default"),(E,"No config")])
>>> fiasco "No default" *> result "Option deprecated" 10
Fiasco (fromList [(E,"No default"),(W,"Option deprecated")])
>>> (,) <$> result "Redundant" 10 <*> result "No CLI Flag" True
Result (fromList ["Redundant","No CLI Flag"]) (10,True)
>>> result "Option deprecated" 10 *> pure 42
Result (fromList ["Option deprecated"]) 42

@since 0.0.0.0
-}
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

{- | Return the first 'Result' with the whole history before
it. If both are 'Fiasco's, return 'Fiasco's with the histories
combined.

>>> fiasco "No info" <|> pure 42
Result (fromList ["No info"]) 42
>>> pure 42 <|> result "Something" 10
Result (fromList []) 42
>>> fiasco "No info" <|> fiasco "Some info"
Fiasco (fromList [(E,"No info"),(E,"Some info")])

See 'alt' if you want a different behaviour.

@since 0.0.0.0
-}
instance Alternative (Trial e) where
    empty :: Trial e a
    empty = Fiasco DL.empty
    {-# INLINE empty #-}

    (<|>) :: Trial e a -> Trial e a -> Trial e a
    r@Result{} <|> _ = r
    f@Fiasco{} <|> r = f <> r
    {-# INLINE (<|>) #-}

{- | Alternative implementation of the 'Alternative' instance for
'Trial'. Return the first 'Result'. Otherwise, append two histories in
both 'Fiasco's. both 'Fiasco's.

>>> fiasco "No info" `alt` pure 42
Result (fromList []) 42
>>> pure 42 `alt` result "Something" 10
Result (fromList []) 42
>>> fiasco "No info" `alt` fiasco "Some info"
Fiasco (fromList [(E,"No info"),(E,"Some info")])

@since 0.0.0.0
-}
infixl 3 `alt`
alt :: Trial e a -> Trial e a -> Trial e a
alt r@Result{} _            = r
alt _ r@Result{}            = r
alt (Fiasco e1) (Fiasco e2) = Fiasco (e1 <> e2)

-- | @since 0.0.0.0
instance Bifunctor Trial where
    bimap :: (e1 -> e2) -> (a -> b) -> Trial e1 a -> Trial e2 b
    bimap ef _ (Fiasco es)   = Fiasco (DL.map (second ef) es)
    bimap ef af (Result e a) = Result (DL.map ef e) (af a)
    {-# INLINE bimap #-}

-- | @since 0.0.0.0
instance Bifoldable Trial where
    bifoldMap :: (Monoid m) => (e -> m) -> (a -> m) -> Trial e a -> m
    bifoldMap ef _ (Fiasco es)    = foldMap (ef . snd) es
    bifoldMap ef ea (Result es a) = foldMap ef es <> ea a
    {-# INLINE bifoldMap #-}

-- | @since 0.0.0.0
instance Bitraversable Trial where
    bitraverse :: (Applicative f) => (e1 -> f e2) -> (a -> f b) -> Trial e1 a -> f (Trial e2 b)
    bitraverse ef _ (Fiasco es)    = Fiasco <$> traverseDList (traverse ef) es
    bitraverse ef ea (Result es a) = Result <$> traverseDList ef es <*> ea a
    {-# INLINE bitraverse #-}

{- 'DList' doesn't have a 'Traversable' instance -}
traverseDList :: (Applicative f) => (a -> f b) -> DList a -> f (DList b)
traverseDList f = foldr (\a fDlistB -> liftA2 DL.cons (f a) fDlistB) (pure DL.empty)
{-# INLINE traverseDList #-}

{- | Smart constructor for 'Trial'. Returns 'Fiasco' with a single
event and 'Error' 'Fatality'.

@since 0.0.0.0
-}
fiasco :: e -> Trial e a
fiasco e = Fiasco $ DL.singleton (E, e)
{-# INLINE fiasco #-}

{- | Smart constructor for 'Trial'. Returns 'Fiasco' with a list of
events, where each has 'Fatality' 'Error'.

@since 0.0.0.0
-}
fiascos :: NonEmpty e -> Trial e a
fiascos = Fiasco . DL.fromList . map (E,) . NE.toList
{-# INLINE fiascos #-}

{- | Smart constructor for 'Trial'. Returns 'Result' with a single
event of 'Warning' 'Fatality'.

__Hint:__ Use 'pure' to create a 'Result' with an empty list of events.

@since 0.0.0.0
-}
result :: e -> a -> Trial e a
result e = Result $ DL.singleton e
{-# INLINE result #-}

{- | Predicate on if the given 'Trial' is 'Fiasco'.

>>> isFiasco (fiasco 'e')
True
>>> isFiasco (result 'a' 42)
False

@since 0.0.0.0
-}
isFiasco :: Trial e a -> Bool
isFiasco (Fiasco _) = True
isFiasco _          = False

{- | Predicate on if the given 'Trial' is 'Result'.

>>> isResult (result 'a' 42)
True
>>> isResult (fiasco 'e')
False

@since 0.0.0.0
-}
isResult :: Trial e a -> Bool
isResult (Result _ _) = True
isResult _            = False

{- | Applies the given action to 'Trial' if it is 'Result' and returns the
value. In case of 'Fiasco' the default value is returned.

>>> whenResult "bar" (fiasco "foo") (\es a -> "success!" <$ (print a >> print es))
"bar"

>>> whenResult "bar" (result "res" 42) (\es a -> "success!" <$ (print a >> print es))
42
["res"]
"success!"

@since 0.0.0.0
-}
whenResult :: Applicative f => x -> Trial e a -> ([e] -> a -> f x) -> f x
whenResult x (FiascoL _) _    = pure x
whenResult _ (ResultL es a) f = f es a
{-# INLINE whenResult #-}

{- | Applies given action to the 'Trial' content if it is 'Result'.

Similar to 'whenResult' but the default value is @()@.

>>> whenResult_ (fiasco "foo") (\es a -> print a >> print es)
>>> whenResult_ (result "res" 42)  (\es a -> print a >> print es)
42
["res"]

@since 0.0.0.0
-}
whenResult_ :: Applicative f => Trial e a -> ([e] -> a -> f ()) -> f ()
whenResult_ = whenResult ()
{-# INLINE whenResult_ #-}

{- | Applies the given action to 'Trial' if it is 'Fiasco' and returns the
result. In case of 'Result' the default value is returned.

>>> whenFiasco "bar" (fiasco 42) (\es -> "foo" <$ print es)
[(E,42)]
"foo"

>>> whenFiasco "bar" (result "res" 42) (\es -> "foo" <$ print es)
"bar"

@since 0.0.0.0
-}
whenFiasco :: Applicative f => x -> Trial e a -> ([(Fatality, e)] -> f x) -> f x
whenFiasco _ (FiascoL e) f   = f e
whenFiasco a (ResultL _ _) _ = pure a
{-# INLINE whenFiasco #-}

{- | Applies given action to the 'Trial' content if it is 'Fiasco'.

Similar to 'whenFiasco' but the default value is @()@.

>>> whenFiasco_ (result "res" 42) print
>>> whenFiasco_ (fiasco "foo") print
[(E,"foo")]

@since 0.0.0.0
-}
whenFiasco_ :: Applicative f => Trial e a -> ([(Fatality, e)] -> f ()) -> f ()
whenFiasco_ = whenFiasco ()
{-# INLINE whenFiasco_ #-}

{- | Convert 'Maybe' to 'Trial' but assigning 'Error' 'Fatality' when
the value is 'Nothing'.

>>> maybeToTrial "No default" (Just 10)
Result (fromList []) 10
>>> maybeToTrial "No default" Nothing
Fiasco (fromList [(E,"No default")])

Functions 'maybeToTrial' and 'trialToMaybe' satisfy property:

@
'trialToMaybe' . 'maybeToTrial' e ≡ 'id'
@

@since 0.0.0.0
-}
maybeToTrial :: e -> Maybe a -> Trial e a
maybeToTrial e = \case
    Just a  -> pure a
    Nothing -> fiasco e

{- | 'Convert 'Trial' to 'Maybe' by losing all history information.

>>> trialToMaybe $ fiasco "Some info"
Nothing
>>> trialToMaybe $ result "From CLI" 3
Just 3

@since 0.0.0.0
-}
trialToMaybe :: Trial e a -> Maybe a
trialToMaybe (Result _ a) = Just a
trialToMaybe (Fiasco _)   = Nothing

{- | Convert 'Either' to 'Trial' by assigning 'Fatality' 'Warning' to
a 'Left' value.

>>> eitherToTrial (Right 42)
Result (fromList []) 42
>>> eitherToTrial (Left "Missing value")
Fiasco (fromList [(E,"Missing value")])

Functions 'eitherToTrial' and 'trialToEither' satisfy property:

@
'trialToEither' . 'eitherToTrial' ≡ 'id'
@

@since 0.0.0.0
-}
eitherToTrial :: Either e a -> Trial e a
eitherToTrial (Right a) = pure a
eitherToTrial (Left e)  = fiasco e

{- | Convert 'Trial' to 'Either' by concatenating all history events.

>>> trialToEither (result "No info" 42)
Right 42
>>> trialToEither $ fiascos $ "Hello, " :| ["there"]
Left "Hello, there"

@since 0.0.0.0
-}
trialToEither :: Monoid e => Trial e a -> Either e a
trialToEither (Result _ a) = Right a
trialToEither (Fiasco es)  = Left $ foldl' (<>) mempty $ DL.map snd es

{- | Tag a 'Trial'.

>>> withTag "Answer" $ pure 42
Result (fromList []) ("Answer",42)
>>> withTag "Answer" $ fiasco "No answer"
Fiasco (fromList [(E,"No answer")])

@since 0.0.0.0
-}
withTag :: tag -> Trial tag a -> TaggedTrial tag a
withTag tag = fmap (tag,)

{- | Untag a 'Trial' by adding a @tag@ to a history of events.

>>> unTag $ pure ("Chosen randomly",5)
Result (fromList ["Chosen randomly"]) 5
>>> unTag $ fiasco "No random"
Fiasco (fromList [(E,"No random")])

@since 0.0.0.0
-}
unTag :: TaggedTrial tag a -> Trial tag a
unTag (Fiasco e)          = Fiasco e
unTag (Result e (tag, a)) = Result (DL.snoc e tag) a

-- TODO: add usage example of the IsLabel instance
{- | Convenient instance to convert record fields of type
'TaggedTrial' to 'Trial' by appending field names to the history. This
instance automatically combines tags and record field names into human
readable message, so the resulting history has more context.

@since 0.0.0.0
-}
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

{- $patternList

'Trial' stores list of events as 'DList' internally for efficient
appending. But when pattern-matching on the final value, it's more
convenient to work directly with lists. 'FiascoL' and 'ResultL' are
Pattern Synonyms for working with lists. It's recommended to use them
only once at the end, since conversion from 'DList' to list takes some
time.

>>> :{
foo :: Trial String Int -> String
foo (FiascoL []) = "Fiasco list is empty"
foo (ResultL [] _) = "Result list is empty"
foo _ = "Other case"
:}

>>> foo empty
"Fiasco list is empty"
>>> foo $ pure 42
"Result list is empty"
>>> foo $ result "Something" 42
"Other case"
-}

{- | Uni-directional Pattern Synonym for 'Fiasco' that allows
pattern-matching directly on lists.

@since 0.0.0.0
-}
pattern FiascoL :: [(Fatality, e)] -> Trial e a
pattern FiascoL e <- Fiasco (DL.toList -> e)

{- | Uni-directional Pattern Synonym for 'Result' that allows
pattern-matching directly on lists.

@since 0.0.0.0
-}
pattern ResultL :: [e] -> a -> Trial e a
pattern ResultL e a <- Result (DL.toList -> e) a

{-# COMPLETE FiascoL, ResultL #-}
{-# COMPLETE Result,  FiascoL #-}
{-# COMPLETE ResultL, Fiasco  #-}

{- | Get the list of 'Warning's and 'Error's together with the 'Maybe'
'Result' if applicable.

>>> getTrialInfo $ result "Warning" 42
([(W,"Warning")],Just 42)
>>> getTrialInfo $ fiasco "Error"
([(E,"Error")],Nothing)

@since 0.0.0.0
-}
getTrialInfo :: Trial e a -> ([(Fatality, e)], Maybe a)
getTrialInfo = \case
    Fiasco e -> (DL.toList e, Nothing)
    Result e a -> (map (W,) $ DL.toList e, Just a)

{- | Returns all 'Error's in the 'Fiasco' constructor.
If the given 'Trial' is 'Result' then returns an empty list instead.

>>> fiascoErrors $ fiasco "One Error"
["One Error"]
>>> fiascoErrors $ result "Warning" 42
[]
>>> fiascoErrors (fiasco "Error" *> result "Warning" 42)
["Error"]

@since 0.0.0.0
-}
fiascoErrors :: Trial e a -> [e]
fiascoErrors = \case
    Result _ _ -> []
    Fiasco e -> map snd $ filter ((==) E . fst) $ DL.toList e


{- | Returns all 'Warning's in the 'Fiasco' constructor.
If the given 'Trial' is 'Result' then returns an empty list instead.

>>> fiascoWarnings $ fiasco "One Error"
[]
>>> fiascoWarnings $ result "Warning" 42
[]
>>> fiascoWarnings (fiasco "Error" *> result "Warning" 42)
["Warning"]

@since 0.0.0.0
-}
fiascoWarnings :: Trial e a -> [e]
fiascoWarnings = \case
    Result _ _ -> []
    Fiasco e -> map snd $ filter ((==) W . fst) $ DL.toList e


{- | Returns all 'Warning's in the 'Result' constructor.
If the given 'Trial' is 'Fiasco' then returns an empty list instead.

>>> resultWarnings $ fiasco "One Error"
[]
>>> resultWarnings $ result "Warning" 42
["Warning"]
>>> resultWarnings (fiasco "Error" *> result "Warning" 42)
[]

@since 0.0.0.0
-}
resultWarnings :: Trial e a -> [e]
resultWarnings = \case
    Result e _ -> DL.toList e
    Fiasco _ -> []

{- | Returns all 'Warning's in the 'Trial'. These includes both warnings in
'Result' of in 'Fiasco'.

>>> anyWarnings $ fiasco "One Error"
[]
>>> anyWarnings $ result "Warning" 42
["Warning"]
>>> anyWarnings (fiasco "Error" *> result "Warning" 42)
["Warning"]

@since 0.0.0.0
-}
anyWarnings :: Trial e a -> [e]
anyWarnings = \case
    Result e _ -> DL.toList e
    Fiasco e -> map snd $ filter ((==) W . fst) $ DL.toList e

{- | Helper function to convert 'DList' to list.

@since 0.0.0.0
-}
dlistToList :: DList a -> [a]
dlistToList = DL.toList
{-# INLINE dlistToList #-}

{- | Print aligned and colourful 'Fatality':

* 'Warning' in yellow
* 'Error' in red

See 'prettyTrial' for examples.

@since 0.0.0.0
-}
prettyFatality :: (Semigroup str, IsString str) => Fatality -> str
prettyFatality = \case
    E -> C.formatWith [C.red]    "Error  "
    W -> C.formatWith [C.yellow] "Warning"

prettyEntry :: (Semigroup e, IsString e) => (Fatality, e) -> e
prettyEntry (f, e) = "  * [" <> prettyFatality f <> "] " <> e <> "\n"

{- | Colourful pretty-printing of 'Trial'.

![Fiasco](https://user-images.githubusercontent.com/8126674/82759167-830c9b80-9de3-11ea-8e72-c5f6c2cdcb6e.png)

![Result](https://user-images.githubusercontent.com/8126674/82759176-8b64d680-9de3-11ea-8426-e5de941ae9a4.png)

@since 0.0.0.0
-}
prettyTrial
    :: (Show a, Semigroup e, IsString e)
    => Trial e a
    -> e
prettyTrial = prettyTrialWith show

{- | Similar to 'prettyTrial', but accepts a function to show Result in the
provided way.

@since 0.0.0.0
-}
prettyTrialWith
    :: (Semigroup e, IsString e)
    => (a -> String)
    -> Trial e a
    -> e
prettyTrialWith showRes = \case
    Fiasco es -> C.formatWith [C.red, C.bold] "Fiasco:\n"
        <> foldr (\e -> (<>) (prettyEntry e)) "" es
    Result es a -> C.formatWith [C.green, C.bold] "Result:\n"
        <> fromString (unlines $ map ("  " <>) $ lines $ showRes a)
        <> C.i "\nWith the following warnings:\n"
        <> foldr (\e -> (<>) (prettyEntry (W, e))) "" es

{- | Colourful pretty-printing of 'TaggedTrial'. Similar to
'prettyTrial', but also prints the resulting @tag@ for 'Result'.

![Tag](https://user-images.githubusercontent.com/8126674/82759188-93bd1180-9de3-11ea-8a76-337d73cf6cc0.png)

@since 0.0.0.0
-}
prettyTaggedTrial
    :: (Show a, Semigroup e, IsString e)
    => TaggedTrial e a
    -> e
prettyTaggedTrial = prettyTaggedTrialWith show

{- | Similar to 'prettyTaggedTrial', but accepts a function to show the 'Result'
in the provided way.

@since 0.0.0.0
--}
prettyTaggedTrialWith
    :: (Semigroup e, IsString e)
    => (a -> String)
    -> TaggedTrial e a
    -> e
prettyTaggedTrialWith showRes = \case
    Fiasco es -> C.formatWith [C.red, C.bold] "Fiasco:\n"
        <> foldr (\e -> (<>) (prettyEntry e)) "" es
    Result es (tag, a) -> C.formatWith [C.green, C.bold] "Result:\n"
        <> C.formatWith [C.blue] ("  [" <> tag <> "]\n    ")
        <> fromString (unlines $ map ("  " <>) $ lines $ showRes a)
        <> C.i "\nWith the following warnings:\n"
        <> foldr (\e -> (<>) (prettyEntry (W, e))) "" es

----------------------------------------------------------------------------
-- Configurations
----------------------------------------------------------------------------

{- $phase
@trial@ introduced some additional data types and type families for adding __phase__ notion to your data types.

This approach is especially useful when you have a data type with many fields and the goal is to roll up the 'Trial' data type to the one with /pure/ fields.

In this case you can have two options:

1. Use two separate data types:

    @
    __data__ MyType = MyType
        { mtField1 :: 'Int'
        , ...

    __data__ PartialMyType = PartialMyType
        { pmtField1 :: 'Trial' 'String' 'Int'
        , ...

    finalise :: PartialMyType -> Maybe MyType
    @

2. Use 'Phase' notion together with ':-' type family:

    @
    __data__ MyType (p :: Phase String) = MyType
        { mtField1 :: p :- 'Int'
        , ...

    finalise :: MyType \'Partial -> Maybe (MyType \'Final)
    @

    And this will have the same effect

See the usage example in the @trial-example@ package:

* [trial-example-advanced](https://github.com/kowainik/trial/blob/master/trial-example/app-advanced/Main.hs)
-}

{- | The phase of the configurations.
This type is parametrised by the @e@ (error) type of the 'Trial' data type.
It is a phantom parameter.
So it could easily be used in the following way: @Phase Text@.

@since 0.0.0.0
-}
data Phase (e :: Type)
    = Partial
    | Final
    deriving stock (Show, Eq)

{- | Type family to map 'Phase' to the corresponding field for the 'Trial'
approach. This is a Higher-Kinded Data approach specialised to custom
enumeration.

@since 0.0.0.0
-}
infixl 3 :-
type family (phase :: Phase (e :: Type)) :- field where
    ('Partial :: Phase e) :- field = Trial e field
    'Final :- field = field

{- | Type family to map 'Phase' to the corresponding field for the 'TaggedTrial'
approach. This is a Higher-Kinded Data approach specialised to custom
enumeration.

@since 0.0.0.0
-}
infixl 3 ::-
type family (phase :: Phase (tag :: Type)) ::- field where
    ('Partial :: Phase tag) ::- field = TaggedTrial tag field
    'Final ::- field = field
