module Test.Trial.Property
    ( propertySpec
    ) where

import Hedgehog (assert, forAll, (===))
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Hedgehog (hedgehog)

import Test.Trial.Gen (Property, evalTrialTree, genEither, genInt, genSmallList, genTrialTree)
import Trial (Trial (..), eitherToTrial, fiascoErrors, maybeToTrial, trialToEither, trialToMaybe)

import qualified Hedgehog.Gen as Gen


propertySpec :: Spec
propertySpec = describe "Trial Property Tests" $ parallel $ do
    it "trialToMaybe  . maybeToTrial e ≡ id" trialMaybeProperty
    it "trialToEither . eitherToTrial  ≡ id" trialEitherProperty
    it "Trial invariant: Fiasco has at least one 'Error'" trialInvariantProperty

trialMaybeProperty :: Property
trialMaybeProperty = hedgehog $ do
    ma <- forAll $ Gen.maybe genInt
    e <- forAll genInt
    trialToMaybe (maybeToTrial e ma) === ma

trialEitherProperty :: Property
trialEitherProperty = hedgehog $ do
    e <- forAll $ genEither (genSmallList genInt) genInt
    trialToEither (eitherToTrial e) === e

trialInvariantProperty :: Property
trialInvariantProperty = hedgehog $ do
    tree <- forAll $ genTrialTree genInt genInt
    case evalTrialTree tree of
        Result _ _    -> pure ()
        f@(Fiasco es) -> assert $ es == mempty || fiascoErrors f /= []
