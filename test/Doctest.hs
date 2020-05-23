module Main (main) where

import Test.DocTest (doctest)


main :: IO ()
main = doctest
    $ "-XDeriveAnyClass"
    : "-XDeriveGeneric"
    : "-XDerivingStrategies"
    : "-XGeneralizedNewtypeDeriving"
    : "-XInstanceSigs"
    : "-XLambdaCase"
    : "-XOverloadedStrings"
    : "-XRecordWildCards"
    : "-XScopedTypeVariables"
    : "-XTupleSections"
    : "-XTypeApplications"
    : [ "src/Trial.hs" ]
