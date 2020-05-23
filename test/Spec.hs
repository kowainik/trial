module Main (main) where

import Test.Hspec (describe, hspec)

import Test.Trial.Laws (lawsSpec)


main :: IO ()
main = hspec $ describe "Trial Test" lawsSpec
