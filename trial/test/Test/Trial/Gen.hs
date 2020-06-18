module Test.Trial.Gen
    ( Property
    , genEither
    , genFunction
    , genFunction2
    , genBindFunction
    , genInt
    , genSmallInt
    , genSmallList
    , genTrial

      -- * Trial tree
    , TrialTree (..)
    , evalTrialTree
    , genTrialTree
    ) where

import Control.Applicative (Alternative (..), liftA2)
import Data.DList (DList)
import Data.List.NonEmpty (NonEmpty (..))
import Hedgehog (Gen, PropertyT)

import Trial (Fatality, Trial (..), fiasco, fiascos, result)

import qualified Data.DList as DL
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


-- | Helper alias for tests.
type Property = PropertyT IO ()

-- | Generate an 'Int'.
genInt :: Gen Int
genInt = Gen.enumBounded

-- | Generate a small list of the given generated elements.
genSmallList :: Gen a -> Gen [a]
genSmallList = Gen.list (Range.linear 0 6)


-- | Generate a small list of the given generated elements.
genSmallDList :: Gen a -> Gen (DList a)
genSmallDList = fmap DL.fromList . genSmallList

-- | Generate a positive 'Int' within the range of @1-6@.
genSmallInt :: Gen Int
genSmallInt = Gen.int (Range.linear 1 6)

-- | Generate a 'Validation'.
genTrial :: Gen a -> Gen (Trial Int a)
genTrial genA = Gen.choice
    [ Fiasco <$> genSmallDList (liftA2 (,) genFatality genInt)
    , Result <$> genSmallDList genInt <*> genA
    ]

genFatality :: Gen Fatality
genFatality = Gen.enumBounded

-- | Generate a simple unary function from the list.
genFunction :: Gen (Int -> Int)
genFunction = genInt >>= \n -> Gen.element
    [ id
    , (+ n)
    , (* n)
    , const n
    , (n -)
    , subtract n
    ]

-- | Generate a simple binary function from the list.
genFunction2 :: Gen (Int -> Int -> Int)
genFunction2 = Gen.element
    [ const
    , (+)
    , (*)
    , (-)
    , subtract
    ]

genBindFunction :: Gen (Int -> Trial Int Int)
genBindFunction = genTrial genInt >>= \t -> Gen.element
    [ return
    , result 0
    , fiasco
    , const t
    , \n -> if even n then result 0 n else fiasco 1
    , \n -> t <> if even n then result 0 n else fiasco 1
    ]

-- | Generate 'Either' with more frequent 'Right's.
genEither :: Gen e -> Gen a -> Gen (Either e a)
genEither genE genA = Gen.sized $ \n -> Gen.frequency
    [ (2, Left <$> genE)
    , (1 + fromIntegral n, Right <$> genA)
    ]

{- | Data type that represents 'Trial' event tree to create a value of
type @Trial Int a@.
-}
data TrialTree e a
    = SCFiasco e  -- ^ @fiasco@ smart constructor
    | SCFiascos (NonEmpty e)  -- ^ @fiascos@ smart constructor
    | SCResult e a  -- ^ @result@ smart constructor
    | Pure a  -- @pure@ from Applicative
--    | Empty  -- @empty@ from Alternative
    | Append (TrialTree e a) (TrialTree e a)  -- ^ @(<>)@ from Semigroup
    | SeqR (TrialTree e a) (TrialTree e a)  -- ^ @(*>)@ from Applicative
    | Alt (TrialTree e a) (TrialTree e a)  -- ^ @(<|>)@ from Alternative
    | Then (TrialTree e a) (TrialTree e a)
    deriving stock (Show, Eq)

evalTrialTree :: TrialTree e a -> Trial e a
evalTrialTree = \case
    SCFiasco e -> fiasco e
    SCFiascos es -> fiascos es
    SCResult e a -> result e a
    Pure a -> pure a
--    Empty -> empty
    Append l r -> evalTrialTree l <> evalTrialTree r
    SeqR l r -> evalTrialTree l *> evalTrialTree r
    Alt l r -> evalTrialTree l <|> evalTrialTree r
    Then l r -> evalTrialTree l >>= \_ -> evalTrialTree r

genTrialTree :: forall e a . Gen e -> Gen a -> Gen (TrialTree e a)
genTrialTree genE genA = Gen.recursive
    Gen.choice
    -- non-recursive generators
    [ SCFiasco <$> genE
    , SCFiascos <$> liftA2 (:|) genE (genSmallList genE)
    , SCResult <$> genE <*> genA
    , Pure <$> genA
--    , pure Empty
    ]
    -- recursive generators
    [ Gen.subterm2 genTree genTree Append
    , Gen.subterm2 genTree genTree SeqR
    , Gen.subterm2 genTree genTree Alt
    , Gen.subterm2 genTree genTree Then
    ]
  where
    genTree :: Gen (TrialTree e a)
    genTree = genTrialTree genE genA
