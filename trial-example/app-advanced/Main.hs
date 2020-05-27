{-# LANGUAGE ApplicativeDo        #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Applicative (empty)
import Data.Text (Text)
import Options.Applicative (Parser, auto, execParser, info, str)
import Toml (TomlCodec, (.=))

import Trial ((::-), Phase (..), Trial (..), fiasco, prettyPrintTrial, withTag)
import Trial.OptparseApplicative (taggedTrialOption)
import Trial.Tomland (taggedTrialMaybeCodec, taggedTrialStrCodec)

import qualified Data.Text.IO as TIO
import qualified Toml


main :: IO ()
main = parseOptions >>= TIO.putStrLn . prettyPrintTrial

{-
https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67
-}

data OptionsP (p :: Phase Text) = OptionsP
    { oRetryCount    :: !(p ::- Int)
    , oHost          :: !(p ::- Text)
    , oCharacterCode :: !(p ::- Maybe Bool)
    }

deriving stock instance
    ( Show (p ::- Text)
    , Show (p ::- Int)
    , Show (p ::- Maybe Bool)
    ) => Show (OptionsP p)

-- | Incomplete options with 'TaggedTrial'.
type PartialOptions = OptionsP 'Partial

-- | Complete options.
type Options = OptionsP 'Final

instance Semigroup PartialOptions where
    x <> y = OptionsP
        { oRetryCount    = oRetryCount    x <> oRetryCount    y
        , oHost          = oHost          x <> oHost          y
        , oCharacterCode = oCharacterCode x <> oCharacterCode y
        }

finaliseOptions :: PartialOptions -> Trial Text Options
finaliseOptions opts = do
    oRetryCount    <- #oRetryCount opts
    oHost          <- #oHost opts
    oCharacterCode <- #oCharacterCode opts
    pure OptionsP {..}

defaultPartialOptions :: PartialOptions
defaultPartialOptions = OptionsP
    { oRetryCount    = withTag "Default" $ pure 5
    , oHost          = withTag "Default" $ fiasco "No default value for Host"
    , oCharacterCode = withTag "Default" empty
    }

partialOptionsParser :: Parser PartialOptions
partialOptionsParser = OptionsP
    <$> taggedTrialOption "retry-count" auto
    <*> taggedTrialOption "host" str
    <*> taggedTrialOption "character-code" auto

-- TOML

partialOptionsCodec :: TomlCodec PartialOptions
partialOptionsCodec = OptionsP
    <$> taggedTrialStrCodec Toml.int "retry-count"         .= oRetryCount
    <*> taggedTrialStrCodec Toml.text "host"               .= oHost
    <*> taggedTrialMaybeCodec (Toml.bool "character-code") .= oCharacterCode

parseOptions :: IO (Trial Text Options)
parseOptions = do
    cmdLineOptions <- execParser $ info partialOptionsParser mempty
    toml <- Toml.decodeFileEither partialOptionsCodec "trial-example/options.toml"
    let tomlOptions = case toml of
            Left errs -> let e = fiasco $ Toml.prettyTomlDecodeErrors errs in
                OptionsP e e e
            Right a -> a

    let combinedOptions =
               defaultPartialOptions
            <> cmdLineOptions
            <> tomlOptions
    pure $ finaliseOptions combinedOptions
