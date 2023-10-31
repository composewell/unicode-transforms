-- {-# OPTIONS_GHC -funfolding-fun-discount=90 #-}
-- |
-- Module      : Data.Text.Normalize
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-3-Clause
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Unicode normalization for @Text@ data type.
--
module Data.Text.Normalize
    (
    -- * Normalization Modes
      NormalizationMode(..)
    -- * Normalization API
    , normalize
    ) where

import Data.Text (Text)
import Data.Unicode.Types (NormalizationMode(..))

-- Internal modules
import Data.Unicode.Internal.NormalizeStream
    ( DecomposeMode(..)
    , stream
    , unstreamD
    , unstreamC
    )

-- | Perform Unicode normalization on @Text@ according to the specified
-- normalization mode.
normalize :: NormalizationMode -> Text -> Text
normalize mode =
    case mode of
      NFD  -> (unstreamD Canonical) . stream
      NFKD -> (unstreamD Kompat)    . stream
      NFC  -> (unstreamC Canonical) . stream
      NFKC -> (unstreamC Kompat)    . stream
