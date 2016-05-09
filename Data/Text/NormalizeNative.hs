-- |
-- Module      : Data.Text.NormalizeNative
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Experimenting with haskell native decomposition implementation
--
module Data.Text.NormalizeNative
    (
    module Data.Unicode.Normalize
    -- * Normalization
    , normalize
    ) where

import           Data.Unicode.Internal.Normalization (decompose)
import           Data.Unicode.Normalize   (NormalizationMode (..))

-- | Perform Unicode normalization on a @String@ according to the specified
-- normalization mode.
normalize :: NormalizationMode -> String -> String
normalize mode str =
    case mode of
          NFD  -> decompose str
