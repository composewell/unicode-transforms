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
module Data.Unicode.Normalize
    (
    -- * Normalization
      NormalizationMode (..)
    , normalize
    ) where

import           Data.Text                             (unpack)
import           Data.Text.Internal.Fusion.Common      (streamList)
import           Data.Unicode.Internal.NormalizeStream (unstream)
import           Data.Unicode.Types                    (NormalizationMode (..))

-- | Perform Unicode normalization on a @String@ according to the specified
-- normalization mode.
normalize :: NormalizationMode -> String -> String
normalize mode =
    case mode of
          NFD  -> unpack . unstream . streamList
