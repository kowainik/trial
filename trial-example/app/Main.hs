{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import Control.Applicative (empty)
import Data.Text (Text)
import Options.Applicative (Parser, auto, execParser, info, str)
import Toml (TomlCodec, (.=))

import Trial (TaggedTrial, Trial (..), fiasco, prettyTrial, withTag)
import Trial.OptparseApplicative (taggedTrialOption)
import Trial.Tomland (taggedTrialMaybeCodec, taggedTrialStrCodec)

import qualified Data.Text.IO as TIO
import qualified Toml


main :: IO ()
main = parseOptions >>= TIO.putStrLn . prettyTrial

{-
https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67
-}

data Options = Options
    { oRetryCount    :: !Int
    , oHost          :: !Text
    , oCharacterCode :: !(Maybe Bool)
    } deriving stock (Show, Eq)

data PartialOptions = PartialOptions
    { poRetryCount    :: !(TaggedTrial Text Int)
    , poHost          :: !(TaggedTrial Text Text)
    , poCharacterCode :: !(TaggedTrial Text (Maybe Bool))
    } deriving stock (Show, Eq)

instance Semigroup PartialOptions where
    x <> y = PartialOptions
        { poRetryCount    = poRetryCount    x <> poRetryCount    y
        , poHost          = poHost          x <> poHost          y
        , poCharacterCode = poCharacterCode x <> poCharacterCode y
        }

makeOptions :: PartialOptions -> Trial Text Options
makeOptions opts = do
    oRetryCount    <- #poRetryCount opts
    oHost          <- #poHost opts
    oCharacterCode <- #poCharacterCode opts
    pure Options {..}

defaultPartialOptions :: PartialOptions
defaultPartialOptions = PartialOptions
    { poRetryCount    = withTag "Default" $ pure 5
    , poHost          = withTag "Default" $ fiasco "No default value for Host"
    , poCharacterCode = withTag "Default" empty
    }

partialOptionsParser :: Parser PartialOptions
partialOptionsParser =  PartialOptions
    <$> taggedTrialOption "retry-count" auto
    <*> taggedTrialOption "host" str
    <*> taggedTrialOption "character-code" auto

-- TOML

partialOptionsCodec :: TomlCodec PartialOptions
partialOptionsCodec = PartialOptions
    <$> taggedTrialStrCodec Toml.int "retry-count" .= poRetryCount
    <*> taggedTrialStrCodec Toml.text "host" .= poHost
    <*> taggedTrialMaybeCodec (Toml.bool "character-code") .= poCharacterCode

parseOptions :: IO (Trial Text Options)
parseOptions = do
    cmdLineOptions <- execParser $ info partialOptionsParser mempty
    toml <- Toml.decodeFileEither partialOptionsCodec "trial-example/options.toml"
    let tomlOptions = case toml of
            Left errs -> let e = fiasco $ Toml.prettyTomlDecodeErrors errs in
                PartialOptions e e e
            Right a -> a

    let combinedOptions =
               defaultPartialOptions
            <> cmdLineOptions
            <> tomlOptions
    pure $ makeOptions combinedOptions
