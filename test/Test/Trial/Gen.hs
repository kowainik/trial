module Test.Trial.Gen
    ( genFunction
    , genInt
    , genSmallInt
    , genSmallList
    , genTrial
    , genFunction2
    ) where

import Hedgehog (Gen)
import Trial (Trial (..))

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


-- | Generate an 'Int'.
genInt :: Gen Int
genInt = Gen.enumBounded

-- | Generate a small list of the given generated elements.
genSmallList :: Gen a -> Gen [a]
genSmallList = Gen.list (Range.linear 0 6)

-- | Generate a positive 'Int' within the range of @1-6@.
genSmallInt :: Gen Int
genSmallInt = Gen.int (Range.linear 1 6)

-- | Generate a 'Validation'.
genTrial :: Gen a -> Gen (Trial Int a)
genTrial genA = Gen.choice
    [ Fiasco <$> genSmallList genInt
    , Result <$> genSmallList genInt <*> genA
    ]

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