-- |
-- Module      : Data.ByteString.UTF8.Normalize
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Data.ByteString.UTF8.Normalize
    (
    module Data.Unicode.Types
    -- * Normalization
    , normalize
    ) where

import           Data.ByteString        (ByteString)
import           Data.ByteString.Unsafe (unsafePackMallocCStringLen,
                                         unsafeUseAsCStringLen)
import           Data.Unicode.Types
import           Data.Unicode.UTF8Proc
import           System.IO.Unsafe       (unsafePerformIO)

-- | Perform Unicode normalization on a UTF8 encoded @ByteString@ according to
-- the specified normalization mode.
normalize :: NormalizationMode -> ByteString -> ByteString
normalize mode bs =
    case mode of
         NFD  -> tr Decomposed []
         NFKD -> tr Decomposed [Compat]
         NFC  -> tr Composed   []
         NFKC -> tr Composed   [Compat]
    where
        tr nf opts = transformBS [StableMode]
                                 [Normalize (mkNormalizeOp nf opts)]
                                 bs

transformBS :: [TransformMode] -> [TransformOp] -> ByteString -> ByteString
transformBS modes ops bs =
    let transform2bs str =     transform modes ops str
                           >>= unsafePackMallocCStringLen
    in unsafePerformIO $ unsafeUseAsCStringLen bs transform2bs
