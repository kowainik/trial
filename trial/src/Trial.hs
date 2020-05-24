{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE UndecidableInstances  #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Trial Data Type
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
       , prettyPrintFatality
       , prettyPrintTrial
       , prettyPrintTaggedTrial
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

import qualified Colourista as C
import qualified Colourista.Short as C
import qualified Data.DList as DL


{- |

@since 0.0.0.0
-}
data Fatality
    = W
    | E
    deriving stock (Show, Eq, Enum, Bounded)

{- |

@since 0.0.0.0
-}
pattern Warning :: Fatality
pattern Warning <- W

{- |

@since 0.0.0.0
-}
pattern Error :: Fatality
pattern Error <- E

{-# COMPLETE Warning, Error #-}

withW :: Functor f => f e -> f (Fatality, e)
withW = fmap (W,)

{- |

@since 0.0.0.0
-}
data Trial e a
    = Fiasco (DList (Fatality, e))
    | Result (DList e) a
    deriving stock (Show, Eq)

{- |

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

{- | Return the first 'Result'. Otherwise, append two histories in
both 'Fiasco's.

>>> fiasco "No info" <|> pure 42
Result (fromList []) 42
>>> pure 42 <|> result "Something" 10
Result (fromList []) 42
>>> fiasco "No info" <|> fiasco "Some info"
Fiasco (fromList [(E,"No info"),(E,"Some info")])

@since 0.0.0.0
-}
instance Alternative (Trial e) where
    empty :: Trial e a
    empty = Fiasco DL.empty
    {-# INLINE empty #-}

    (<|>) :: Trial e a -> Trial e a -> Trial e a
    r@Result{} <|> _ = r
    _ <|> r@Result{} = r
    (Fiasco e1) <|> (Fiasco e2) = Fiasco (e1 <> e2)
    {-# INLINE (<|>) #-}

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

{- |

@since 0.0.0.0
-}
fiasco :: e -> Trial e a
fiasco e = Fiasco $ DL.singleton (E, e)

{- |

@since 0.0.0.0
-}
fiascos :: [e] -> Trial e a
fiascos = Fiasco . DL.fromList . map (E,)

{- |

@since 0.0.0.0
-}
result :: e -> a -> Trial e a
result e = Result $ DL.singleton e

{- | Convert 'Maybe' to 'Trial' but assigning 'E' severity when value
is 'Nothing'.

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

{- | Convert 'Either' to 'Trial' by assigning 'Fatality' 'W' to a
'Left' value.

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
>>> trialToEither $ fiascos ["Hello, ", "there"]
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

{- |

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

>>> :{
foo :: Trial String Int -> String
foo (FiascoL []) = "Fiasco list is empty"
foo (ResultL [] _) = "Result list is empty"
foo _ = "Other case"
:}

>>> foo $ fiascos []
"Fiasco list is empty"
>>> foo $ pure 42
"Result list is empty"
>>> foo $ result "Something" 42
"Other case"
-}

{- |

@since 0.0.0.0
-}
pattern FiascoL :: [(Fatality, e)] -> Trial e a
pattern FiascoL e <- Fiasco (DL.toList -> e)

{- |

@since 0.0.0.0
-}
pattern ResultL :: [e] -> a -> Trial e a
pattern ResultL e a <- Result (DL.toList -> e) a

{-# COMPLETE FiascoL, ResultL #-}

{- | Get the list of 'Warning's and 'Error's together with the 'Maybe' 'Result' is applicable.

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

{- |

@since 0.0.0.0
-}
dlistToList :: DList a -> [a]
dlistToList = DL.toList
{-# INLINE dlistToList #-}

prettyPrintFatality :: (Semigroup str, IsString str) => Fatality -> str
prettyPrintFatality = \case
    E -> C.formatWith [C.red]    "Error  "
    W -> C.formatWith [C.yellow] "Warning"

prettyPrintEntry :: (Semigroup e, IsString e) => (Fatality, e) -> e
prettyPrintEntry (f, e) = "  * [" <> prettyPrintFatality f <> "] " <> e <> "\n"

prettyPrintTrial
    :: (Show a, Semigroup e, IsString e)
    => Trial e a
    -> e
prettyPrintTrial = \case
    Fiasco es -> C.formatWith [C.red, C.bold] "Fiasco:\n"
        <> foldr (\e -> (<>) (prettyPrintEntry e)) "" es
    Result es a -> C.formatWith [C.green, C.bold] "Result:\n  "
        <> fromString (show a)
        <> C.i "\nWith the following warnings:\n"
        <> foldr (\e -> (<>) (prettyPrintEntry (W, e))) "" es

prettyPrintTaggedTrial
    :: (Show a, Semigroup e, IsString e)
    => TaggedTrial e a
    -> e
prettyPrintTaggedTrial = \case
    Fiasco es -> C.formatWith [C.red, C.bold] "Fiasco:\n"
        <> foldr (\e -> (<>) (prettyPrintEntry e)) "" es
    Result es (tag, a) -> C.formatWith [C.green, C.bold] "Result:\n"
        <> C.formatWith [C.blue] ("  [" <> tag <> "]\n    ")
        <> fromString (show a)
        <> C.i "\nWith the following warnings:\n"
        <> foldr (\e -> (<>) (prettyPrintEntry (W, e))) "" es
