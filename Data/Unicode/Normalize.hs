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

import           Data.Unicode.Internal.Normalization (decompose)
import           Data.Unicode.Types                  (NormalizationMode (..))

-- | Perform Unicode normalization on a @String@ according to the specified
-- normalization mode.
normalize :: NormalizationMode -> String -> String
normalize mode str =
    case mode of
          NFD  -> decompose str
