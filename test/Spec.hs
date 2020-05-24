module Main (main) where

import Test.Hspec (describe, hspec, parallel)

import Test.Trial.Laws (lawsSpec)
import Test.Trial.Property (propertySpec)


main :: IO ()
main = hspec $ describe "Trial Test" $ parallel $ do
    lawsSpec
    propertySpec
