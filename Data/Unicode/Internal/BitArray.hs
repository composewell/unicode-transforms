{-# LANGUAGE BangPatterns, MagicHash #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.CharSet.ByteSet
-- Copyright   :  Edward Kmett 2011
--                Bryan O'Sullivan 2008
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (BangPatterns, MagicHash)
--
-- Fast set membership tests for byte values. The set representation is
-- unboxed for efficiency and uses one bit per byte to represent the presence
-- or absence of a byte in the set.
--
-- This is a fairly minimal API. You probably want to use CharSet.
-----------------------------------------------------------------------------
module Data.CharSet.ByteSet
    (
    -- * Data type
      ByteSet(..)
    -- * Construction
    , fromList
    -- * Lookup
    , member
    ) where

import Data.Bits ((.&.), (.|.))
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC.Base (Int(I#), iShiftRA#, narrow8Word#, shiftL#)
import GHC.Word (Word8(W8#))
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as I
import qualified Data.ByteString.Unsafe as U

newtype ByteSet = ByteSet B.ByteString deriving (Eq, Ord, Show)

-- | Representation of the index of a bit inside a bytestring
-- in terms of a byte index and a bit index inside the byte
data I = I
    {-# UNPACK #-} !Int         -- byte index
    {-# UNPACK #-} !Word8       -- bit index

shiftR :: Int -> Int -> Int
shiftR (I# x#) (I# i#) = I# (x# `iShiftRA#` i#)

shiftL :: Word8 -> Int -> Word8
shiftL (W8# x#) (I# i#) = W8# (narrow8Word# (x# `shiftL#` i#))

-- | Convert a bit index to a byte index and bit index inside the byte
index :: Int -> I
index i = I (i `shiftR` 3) (1 `shiftL` (i .&. 7))
{-# INLINE index #-}

fromList :: [Word8] -> ByteSet
fromList s0 = ByteSet $ I.unsafeCreate 32 $ \t -> do
  _ <- I.memset t 0 32
  let go [] = return ()
      go (c:cs) = do
        prev <- peekByteOff t byte :: IO Word8
        pokeByteOff t byte (prev .|. bit)
        go cs
        where I byte bit = index (fromIntegral c)
  go s0

-- | Check the set for membership.
member :: Word8 -> ByteSet -> Bool
member w (ByteSet t) = U.unsafeIndex t byte .&. bit /= 0
  where
    I byte bit = index (fromIntegral w)
