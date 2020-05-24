module Test.Trial.Gen
    ( Property
    , genEither
    , genFunction
    , genFunction2
    , genInt
    , genSmallInt
    , genSmallList
    , genTrial
    ) where

import Control.Applicative (liftA2)
import Data.DList (DList)
import Hedgehog (Gen, PropertyT)
import Trial (Fatality, Trial (..))

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

-- | Generate 'Either' with more frequent 'Right's.
genEither :: Gen e -> Gen a -> Gen (Either e a)
genEither genE genA = Gen.sized $ \n -> Gen.frequency
    [ (2, Left <$> genE)
    , (1 + fromIntegral n, Right <$> genA)
    ]
