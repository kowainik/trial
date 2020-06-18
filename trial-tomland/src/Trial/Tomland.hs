{-# LANGUAGE ApplicativeDo #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Trial helpers for @tomland@.
-}

module Trial.Tomland
    ( trialCodec
    , trialStrCodec
    , trialMaybeCodec
    , taggedTrialCodec
    , taggedTrialStrCodec
    , taggedTrialMaybeCodec
    , taggedTrialListCodec
    ) where

import Control.Monad (join)
import Data.String (IsString (..))
import Toml (Key, TomlCodec)
import Trial (TaggedTrial, Trial (..), fiasco, maybeToTrial, trialToMaybe, unTag, withTag)

import qualified Data.Text as Text
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
trialStrCodec
    :: forall e a
    .  (IsString e, Semigroup e)
    => (Key -> TomlCodec a)
    -> Key
    -> TomlCodec (Trial e a)
trialStrCodec codecA key =
    Toml.dimap (withTag "TOML") unTag $ taggedTrialStrCodec codecA key

{- | 'TomlCodec' for 'Maybe' inside 'Trial'. Never fails,
doesn't change history of events.

@since 0.0.0.0
-}
trialMaybeCodec :: TomlCodec a -> TomlCodec (Trial e (Maybe a))
trialMaybeCodec = Toml.dimap (join . trialToMaybe) pure . Toml.dioptional

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
taggedTrialStrCodec
    :: forall tag a
    .  (IsString tag, Semigroup tag)
    => (Key -> TomlCodec a)
    -> Key
    -> TomlCodec (TaggedTrial tag a)
taggedTrialStrCodec codecA key =
    Toml.dimap (fmap snd . trialToMaybe) handleMaybe
    $ Toml.dioptional (codecA key)
  where
    keyS :: tag
    keyS = fromString $ Text.unpack $ Toml.prettyKey key

    handleMaybe :: Maybe a -> TaggedTrial tag a
    handleMaybe = \case
        Nothing -> fiasco $ "No TOML option specified for key: " <> keyS
        Just a  -> withTag "TOML" $ pure a

{- | 'TomlCodec' for 'Maybe' inside 'TaggedTrial'. Never fails,
doesn't change history of events, and adds a tag.

@since 0.0.0.0
-}
taggedTrialMaybeCodec :: IsString e => TomlCodec a -> TomlCodec (TaggedTrial e (Maybe a))
taggedTrialMaybeCodec =
    Toml.dimap taggedTrialToMaybe (withTag "TOML" . pure) . Toml.dioptional
  where
    taggedTrialToMaybe :: TaggedTrial e (Maybe a) -> Maybe a
    taggedTrialToMaybe = join . fmap snd . trialToMaybe

{- | 'TomlCodec' that decodes with 'Toml.list' and adds 'fiasco' to
the result if the resulting list is empty. It's helpful to handle the
case when the list is not specified at all.

@since 0.0.0.0
-}
taggedTrialListCodec
    :: forall e a
    .  (IsString e, Semigroup e)
    => Key
    -> TomlCodec a
    -> TomlCodec (TaggedTrial e [a])
taggedTrialListCodec key aCodec = do
    res <- taggedTrialStrCodec (Toml.list aCodec) key
    pure $ case res of
        Result _ (_, []) ->
            res <> fiasco ("No TOML value is specified for key: " <> keyToStr key)
        Result _ _ -> res
        Fiasco _ -> res
  where
    keyToStr :: Key -> e
    keyToStr = fromString . Text.unpack . Toml.prettyKey
