{-# LANGUAGE BangPatterns, MagicHash #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Unicode.Internal.BitArray
-- Copyright   :  Harendra Kumar 2016
--                Edward Kmett 2011
--                Bryan O'Sullivan 2008
-- License     :  BSD3
--
-- Maintainer  :  harendra.kumar@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (BangPatterns, MagicHash)
--
-- Fast ByteString based bit lookups using bit indexes
-----------------------------------------------------------------------------
module Data.Unicode.Internal.BitArray
    (
    -- * Data type
      BitArray(..)
    -- * Construction
    , setFromList
    -- * Lookup
    , unsafeLookup
    ) where

import Data.Bits ((.&.), (.|.))
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC.Base (Int(I#), iShiftRA#, narrow8Word#, shiftL#)
import GHC.Word (Word8(W8#))
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as I
import qualified Data.ByteString.Unsafe as U

-- begin index, bytestring to store bits
data BitArray = BitArray
  { _beg  :: {-# UNPACK #-} !Int
  , _bits :: {-# UNPACK #-} !B.ByteString
  } deriving (Eq, Ord, Show)

-- | Representation of the index of a bit inside a bytestring
-- in terms of a byte index and a bit index inside the byte
data I = I
    {-# UNPACK #-} !Int         -- byte index
    {-# UNPACK #-} !Word8         -- bit index

shiftR :: Int -> Int -> Int
shiftR (I# x#) (I# i#) = I# (x# `iShiftRA#` i#)
{-# INLINE shiftR #-}

shiftL :: Word8 -> Int -> Word8
shiftL (W8# x#) (I# i#) = W8# (narrow8Word# (x# `shiftL#` i#))
{-# INLINE shiftL #-}

-- | Convert a bit index to a byte index and bit index inside the byte
index :: Int -> I
index i = I (i `shiftR` 3) (1 `shiftL` (i .&. 7))
{-# INLINE index #-}

-- FIXME remove the values, just keep indexes
-- List of bits which must be set, others will be left cleared
setFromList :: (Int,Int) -> [(Int,Bool)] -> BitArray
setFromList (beg, end) pairs =
    let nbits = end - beg + 1
        nbytes = (nbits + 7) `div` 8
    in if end < beg
       then
            error "Bounds end cannot be less than begin"
        else
            BitArray beg $ I.unsafeCreate nbytes $ \bs -> do
                _ <- I.memset bs 0 (fromIntegral nbytes)
                setBits bs pairs
    where
        setBits _ []      = return ()
        setBits bs (p:ps) = do
            let (i, _v) = p
            if (i  < beg || i > end)
               then
                    error $ "Index " ++ show i ++ " out of range"
                else do
                    let I byte bit = index (i - beg)
                    prev <- peekByteOff bs byte :: IO Word8
                    pokeByteOff bs byte (prev .|. bit)
                    setBits bs ps

-- | lookup a bit at a given index, does not check the bounds
unsafeLookup :: BitArray -> Int -> Bool
unsafeLookup !(BitArray beg bs) !i = U.unsafeIndex bs byte .&. bit /= 0
  where
    I byte bit = index (i - beg)
{-# INLINE unsafeLookup #-}
