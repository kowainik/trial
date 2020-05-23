{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

import Data.Text (Text)
import Options.Applicative (Parser, ReadM, auto, execParser, info, long, option, optional, str)
import Toml (Key, TomlCodec, (.=))

import Trial (Fatality (..), Trial (..), maybeToTrial, trialToMaybe)

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
    { poRetryCount    :: !(Trial Text Int)
    , poHost          :: !(Trial Text Text)
    , poCharacterCode :: !(Trial Text (Maybe Bool))
    } deriving stock (Show, Eq)

instance Semigroup PartialOptions where
    x <> y = PartialOptions
        { poRetryCount    = poRetryCount    x <> poRetryCount    y
        , poHost          = poHost          x <> poHost          y
        , poCharacterCode = poCharacterCode x <> poCharacterCode y
        }

makeOptions :: PartialOptions -> Trial Text Options
makeOptions PartialOptions {..} = do
    oRetryCount    <- poRetryCount
    oHost          <- poHost
    oCharacterCode <- poCharacterCode
    pure Options {..}

defaultPartialOptions :: PartialOptions
defaultPartialOptions = PartialOptions
    { poRetryCount    = pure 5
    , poHost          = Fiasco [(E, "No default value for Host")]
    , poCharacterCode = Fiasco []
    }

mToTrial :: Text -> Text -> Maybe a -> Trial Text a
mToTrial from field = maybeToTrial ("No " <> from <> " option specified for " <> field)

trialOption :: Text -> (ReadM a) -> Parser (Trial Text a)
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
    trialMaybeCodec :: Key -> TomlCodec (Trial Text (Maybe Bool))
    trialMaybeCodec key = Toml.dimap f pure $
        Toml.dioptional (Toml.bool key)

    f :: Trial Text (Maybe a) -> Maybe a
    f = \case
        Result _ a -> a
        Fiasco _ -> Nothing

trialCodec :: Key -> (Key -> TomlCodec a) -> TomlCodec (Trial Text a)
trialCodec key codecA = Toml.dimap trialToMaybe (mToTrial "TOML" $ Toml.prettyKey key) $
    Toml.dioptional (codecA key)


parseOptions :: IO (Trial Text Options)
parseOptions = do
    cmdLineOptions <- execParser $ info partialOptionsParser mempty
    toml <- Toml.decodeFileEither partialOptionsCodec "options.toml"
    let tomlOptions = case toml of
            Left errs -> let e = Fiasco [(E, Toml.prettyTomlDecodeErrors errs)] in
                PartialOptions e e e
            Right a -> a

    let combinedOptions =  defaultPartialOptions
                        <> cmdLineOptions
                        <> tomlOptions
    pure $ makeOptions combinedOptions
