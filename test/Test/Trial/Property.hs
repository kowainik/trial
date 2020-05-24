module Test.Trial.Property
    ( propertySpec
    ) where

import Hedgehog (forAll, (===))
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Hedgehog (hedgehog)

import Test.Trial.Gen (Property, genEither, genInt, genSmallList)
import Trial (eitherToTrial, maybeToTrial, trialToEither, trialToMaybe)

import qualified Hedgehog.Gen as Gen


propertySpec :: Spec
propertySpec = describe "Trial Property Tests" $ parallel $ do
    it "trialToMaybe  . maybeToTrial e ≡ id" trialMaybeProperty
    it "trialToEither . eitherToTrial  ≡ id" trialEitherProperty

trialMaybeProperty :: Property
trialMaybeProperty = hedgehog $ do
    ma <- forAll $ Gen.maybe genInt
    e <- forAll genInt
    trialToMaybe (maybeToTrial e ma) === ma

trialEitherProperty :: Property
trialEitherProperty = hedgehog $ do
    e <- forAll $ genEither (genSmallList genInt) genInt
    trialToEither (eitherToTrial e) === e
