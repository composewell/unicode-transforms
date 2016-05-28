-- |
-- Module      : Data.Text.Normalize
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Data.Text.Normalize
    (
    module Data.Unicode.Types
    -- * Normalization
    , normalize
    ) where

import           Data.Text             (Text)
import           Data.Text.Foreign     (fromPtr, useAsPtr)
import           Data.Unicode.Types    (NormalizationMode (..))
import           Data.Unicode.UTF8Proc
import           Foreign.Ptr           (castPtr)
import           System.IO.Unsafe      (unsafePerformIO)

-- | Perform Unicode normalization on a @Text@ according to the specified
-- normalization mode.
normalize :: NormalizationMode -> Text -> Text
normalize mode txt =
    case mode of
         NFD  -> tr Decomposed []
         NFKD -> tr Decomposed [Compat]
         NFC  -> tr Composed   []
         NFKC -> tr Composed   [Compat]
    where
        tr nf opts = transformText [StableMode, UTF16Mode]
                                   [Normalize (mkNormalizeOp nf opts)]
                                   txt

transformText :: [TransformMode] -> [TransformOp] -> Text -> Text
transformText modes ops txt =
    unsafePerformIO $ useAsPtr txt transform2txt
    where
        transform2txt buf len16 =
            transform modes ops (castPtr buf, fromIntegral (len16) * 2)
            >>= fromCstringLen
        -- XXX test whether the length is not truncated
        fromCstringLen (str, len8) = fromPtr (castPtr str)
                                             ((fromIntegral len8) `div` 2)
