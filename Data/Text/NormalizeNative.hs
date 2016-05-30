-- |
-- Module      : Data.Unicode.Normalize
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Normalization for Haskell @String@s.
--
module Data.Text.NormalizeNative
    (
    -- * Normalization
      NormalizationMode (..)
    , normalize
    ) where

import           Data.Text                             (Text)
import           Data.Text.Internal.Fusion             (stream)
import           Data.Unicode.Internal.NormalizeStream (unstream)
import           Data.Unicode.Types                    (NormalizationMode (..))

-- | Perform Unicode normalization on a @String@ according to the specified
-- normalization mode.
normalize :: NormalizationMode -> Text -> Text
normalize mode =
    case mode of
          NFD  -> unstream . stream
