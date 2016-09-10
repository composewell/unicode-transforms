-- |
-- Module      : Data.ByteString.UTF8.Normalize
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Unicode normalization for @ByteString@ data type.
--
module Data.ByteString.UTF8.Normalize
    {-# DEPRECATED "Convert ByteString to Text and then normalize" #-}
    (
    -- * Normalization Modes
      NormalizationMode(..)
    -- * Normalization API
    , normalize
    ) where

import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Unicode.Types (NormalizationMode(..))
import qualified Data.Text.Normalize as T

-- This is now simply a wrapper over Text normalization

-- | Perform Unicode normalization on a UTF8 encoded @ByteString@ according to
-- the specified normalization mode.
normalize :: NormalizationMode -> ByteString -> ByteString
normalize mode = (encodeUtf8 . T.normalize mode . decodeUtf8)
