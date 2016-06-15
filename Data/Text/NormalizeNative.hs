-- {-# OPTIONS_GHC -funfolding-fun-discount=90 #-}
-- |
-- Module      : Data.Unicode.Normalize
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Unicode normalization for @Text@ data type.
--
module Data.Text.NormalizeNative
    (
    -- * Normalization Modes
      NormalizationMode(..)
    -- * Normalization API
    , normalize
    ) where

import           Data.Text                             (Text)
-- import           Data.Text.Internal.Fusion             (stream)
import           Data.Unicode.Internal.NormalizeStream (stream, unstream)
import           Data.Unicode.Types                    (NormalizationMode (..))

-- | Perform Unicode normalization on @Text@ according to the specified
-- normalization mode.
normalize :: NormalizationMode -> Text -> Text
normalize mode =
    case mode of
          NFD  -> unstream . stream
