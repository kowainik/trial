{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Trial helper functions for @optparse-applicative@.
-}

module Trial.OptparseApplicative
       ( trialOption
       , taggedTrialOption
       ) where

import Data.String (IsString (..))
import Options.Applicative (Parser, ReadM, long, option, optional)
import Trial (TaggedTrial, Trial, maybeToTrial, withTag)


{- | 'Parser' for 'Trial' data structure.
It uses the provided option 'long' name for better 'Trial.Warning's and
'Trial.Error's.

@since 0.0.0.0
-}
trialOption
    :: (Semigroup e, IsString e)
    => String  -- ^ Option 'long' name
    -> ReadM a
    -> Parser (Trial e a)
trialOption field opt = mToTrial field <$>
    optional (option opt (long field))

{- | Similar to 'trialOption' but returns 'TaggedTrial'  with the tag @CLI@.

'Parser' for 'TaggedTrial' data structure.
It uses the provided option 'long' name for better 'Trial.Warning's and
'Trial.Error's.

@since 0.0.0.0
-}
taggedTrialOption
    :: (Semigroup e, IsString e)
    => String  -- ^ Option 'long' name
    -> ReadM a
    -> Parser (TaggedTrial e a)
taggedTrialOption field opt = mToTaggedTrial field <$>
    optional (option opt (long field))


mToTaggedTrial :: (Semigroup e, IsString e) => String -> Maybe a -> TaggedTrial e a
mToTaggedTrial field = withTag "CLI" . mToTrial field

mToTrial :: (Semigroup e, IsString e) => String -> Maybe a -> Trial e a
mToTrial field = maybeToTrial ("No CLI option specified for " <> fromString field)
