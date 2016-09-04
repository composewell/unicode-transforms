-- |
-- Module      : Data.Unicode.Normalize
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Unicode normalization for Haskell @String@ data type.
--
module Data.String.Normalize
    (
    -- * Normalization Modes
      NormalizationMode (..)
    -- * Normalization API
    , normalize
    ) where

import           Data.Text                             (unpack)
import           Data.Text.Internal.Fusion.Common      (streamList)
import           Data.Unicode.Internal.NormalizeStream (unstreamD)
import           Data.Unicode.Types                    (NormalizationMode (..))

-- | Perform Unicode normalization on a @String@ according to the specified
-- normalization mode.
normalize :: NormalizationMode -> String -> String
normalize mode =
    case mode of
          NFD  -> unpack . unstreamD . streamList
