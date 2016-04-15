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

import Data.Bits ((.&.), (.|.), unsafeShiftL, unsafeShiftR)
import Foreign.Storable (peekElemOff, pokeElemOff)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, mallocForeignPtrArray)
import Foreign.Marshal (fillBytes)
import GHC.Base (Int(I#), iShiftRA#, iShiftL#)
import GHC.Word (Word64)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as I
import qualified Data.ByteString.Unsafe as U
import System.IO.Unsafe (unsafePerformIO)

-- begin index, bytestring to store bits
data BitArray = BitArray
  { _beg  :: {-# UNPACK #-} !Int
  , _bits :: ForeignPtr Word64
  } deriving (Eq, Ord, Show)

-- | Representation of the index of a bit inside a bytestring
-- in terms of a byte index and a bit index inside the byte
data I = I
    {-# UNPACK #-} !Int         -- byte index
    {-# UNPACK #-} !Word64      -- bit index

-- | Convert a bit index to a byte index and bit index inside the byte
index :: Int -> I
index i = I (i `unsafeShiftR` 6) (1 `unsafeShiftL` ((fromIntegral i) .&. 63) :: Word64)
{-# INLINE index #-}

-- FIXME remove the values, just keep indexes
-- List of bits which must be set, others will be left cleared
setFromList :: (Int,Int) -> [(Int,Bool)] -> BitArray
setFromList (beg, end) pairs =
    let nbits = end - beg + 1
        nwords = (nbits + 63) `div` 64
    in if end < beg
       then
            error "Bounds end cannot be less than begin"
        else unsafePerformIO $ do
            fptr <- mallocForeignPtrArray nwords
            withForeignPtr fptr $ \p -> do
                fillBytes p 0 (nwords * 8)
                setBits p (map fst pairs)
                return $ BitArray beg fptr
    where
        setBits _ []      = return ()
        setBits ptr (i:rest) = do
            if (i  < beg || i > end)
               then
                    error $ "Index " ++ show i ++ " out of range"
                else do
                    let I word bit = index (i - beg)
                    old <- peekElemOff ptr word
                    pokeElemOff ptr word (old .|. bit)
                    setBits ptr rest

-- | lookup a bit at a given index, does not check the bounds
unsafeLookup :: BitArray -> Int -> Bool
unsafeLookup !(BitArray beg fptr) !i = unsafeIndex fptr word .&. bit /= 0
  where
    I word bit = index (i - beg)
    unsafeIndex ptr w = I.accursedUnutterablePerformIO $
                            withForeignPtr ptr $ \p -> peekElemOff p w
{-# INLINE unsafeLookup #-}
