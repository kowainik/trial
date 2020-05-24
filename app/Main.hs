{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import Control.Applicative (empty)
import Data.Text (Text)
import Options.Applicative (Parser, ReadM, auto, execParser, info, long, option, optional, str)
import Toml (Key, TomlCodec, (.=))

import Trial (TaggedTrial, Trial (..), fiasco, maybeToTrial, trialToMaybe, withTag)

import qualified Data.Text as T
import qualified Toml


main :: IO ()
main = parseOptions >>= print

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

mToTrial :: Text -> Text -> Maybe a -> TaggedTrial Text a
mToTrial from field = withTag from . maybeToTrial ("No " <> from <> " option specified for " <> field)

trialOption :: Text -> ReadM a -> Parser (TaggedTrial Text a)
trialOption field opt = mToTrial "CLI" field <$>
    optional (option opt (long $ T.unpack field))

partialOptionsParser :: Parser PartialOptions
partialOptionsParser =  PartialOptions
    <$> trialOption "retry-count" auto
    <*> trialOption "host" str
    <*> trialOption "character-code" auto

-- TOML

partialOptionsCodec :: TomlCodec PartialOptions
partialOptionsCodec = PartialOptions
    <$> trialCodec "retry-count" Toml.int  .= poRetryCount
    <*> trialCodec "host"        Toml.text .= poHost
    <*> trialMaybeCodec "character-code"   .= poCharacterCode
  where
    trialMaybeCodec :: Key -> TomlCodec (TaggedTrial Text (Maybe Bool))
    trialMaybeCodec key = Toml.dimap f (withTag "TOML" . pure) $
        Toml.dioptional (Toml.bool key)

    f :: TaggedTrial Text (Maybe a) -> Maybe a
    f = \case
        Result _ a -> snd a
        Fiasco _ -> Nothing

trialCodec :: Key -> (Key -> TomlCodec a) -> TomlCodec (TaggedTrial Text a)
trialCodec key codecA = Toml.dimap (fmap snd . trialToMaybe) (mToTrial "TOML" $ Toml.prettyKey key) $
    Toml.dioptional (codecA key)


parseOptions :: IO (Trial Text Options)
parseOptions = do
    cmdLineOptions <- execParser $ info partialOptionsParser mempty
    toml <- Toml.decodeFileEither partialOptionsCodec "options.toml"
    let tomlOptions = case toml of
            Left errs -> let e = fiasco $ Toml.prettyTomlDecodeErrors errs in
                PartialOptions e e e
            Right a -> a

    let combinedOptions =
               defaultPartialOptions
            <> cmdLineOptions
            <> tomlOptions
    pure $ makeOptions combinedOptions
