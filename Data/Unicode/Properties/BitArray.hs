-- |
-- Module      : Data.Unicode.Properties.BitArray
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
--
module Data.Unicode.Properties.BitArray
(bitArraySetBits)
where

import Control.Monad
import Control.Monad.ST
import Data.BitArray.ST

import Data.BitArray (BitArray)

-- | Initialize a bitarray with default bits as False and bits supplied in the
-- list argument set as True.
--
bitArraySetBits :: (Int,Int) -> [Int] -> BitArray
bitArraySetBits range xs = runST $ do
  ar <- newBitArray range False
  forM_ xs $ \i -> do
    writeBit ar i True
  unsafeFreezeBitArray ar
