{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Trial helpers for @tomland@.
-}

module Trial.Tomland
    ( trialCodec
    , trialTextCodec
    , taggedTrialCodec
    , taggedTrialTextCodec
    ) where

import Data.Text (Text)
import Toml (Key, TomlCodec)
import Trial (TaggedTrial, Trial (..), fiasco, maybeToTrial, trialToMaybe, unTag, withTag)

import qualified Toml


{- | 'TomlCodec' for 'Trial' that adds a given event @e@ if a given
codec fails.

@since 0.0.0.0
-}
trialCodec :: e -> TomlCodec a -> TomlCodec (Trial e a)
trialCodec e = Toml.dimap trialToMaybe (maybeToTrial e) . Toml.dioptional

{- | 'TomlCodec' for 'Trial' that adds an informative message if a
given codec fails.

@since 0.0.0.0
-}
trialTextCodec :: forall a . (Key -> TomlCodec a) -> Key -> TomlCodec (Trial Text a)
trialTextCodec codecA key =
    Toml.dimap (withTag "TOML") unTag $ taggedTrialTextCodec codecA key

{- | 'TomlCodec' for 'TaggedTrial' that uses given @tag@ in a 'Fiasco'
if a given codec fails, and also adds @tag@ to the result.

@since 0.0.0.0
-}
taggedTrialCodec
    :: forall tag a
    .  tag
    -> (Key -> TomlCodec a)
    -> Key
    -> TomlCodec (TaggedTrial tag a)
taggedTrialCodec tag codecA key =
    Toml.dimap (fmap snd . trialToMaybe) handleMaybe
    $ Toml.dioptional (codecA key)
  where
    handleMaybe :: Maybe a -> TaggedTrial tag a
    handleMaybe = \case
        Nothing -> fiasco tag
        Just a  -> withTag tag $ pure a

{- | 'TomlCodec' for 'TaggedTrial' that adds an informative message if
a given codec fails, and also adds a tag where the field comes from.

@since 0.0.0.0
-}
taggedTrialTextCodec :: (Key -> TomlCodec a) -> Key ->  TomlCodec (TaggedTrial Text a)
taggedTrialTextCodec codecA key =
    Toml.dimap (fmap snd . trialToMaybe) handleMaybe
    $ Toml.dioptional (codecA key)
  where
    handleMaybe :: Maybe a -> TaggedTrial Text a
    handleMaybe = \case
        Nothing -> fiasco $ "No TOML option specified for key: " <> Toml.prettyKey key
        Just a  -> withTag "TOML" $ pure a
