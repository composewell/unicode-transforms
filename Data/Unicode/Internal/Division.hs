{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Unicode.Internal.Division
-- Copyright   : (c) 2020 Andrew Lelechenko
--
-- License     : BSD-3-Clause
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast division by known constants.
--
-- Division by a constant can be replaced by a double-word multiplication.
-- Roughly speaking, instead of dividing by x, multiply by 2^64/x,
-- obtaining 128-bit-long product, and take upper 64 bits. The peculiar
-- details can be found in Hacker's Delight, Ch. 10.
--
-- Even GHC 8.10 does not provide a primitive for a signed double-word
-- multiplication, but since our applications does not involve negative
-- integers, we convert 'Int' to 'Word' and use 'GHC.Exts.timesWord#'.
--
-- Textbook unsigned division by 21 or 28 becomes involved, when an argument
-- is allowed to take the full range of 'Word' up to 2^64. Luckily, in our
-- case the argument was casted from 'Int', so we can guarantee that it is
-- below 2^63.

module Data.Unicode.Internal.Division
    ( quotRem21
    , quotRem28
    ) where

import Data.Bits (Bits(..), FiniteBits(..))
import GHC.Exts (Word(..), timesWord2#)

highMul :: Word -> Word -> Word
highMul (W# x#) (W# y#) = W# high#
    where
        !(# high#, _ #) = timesWord2# x# y#

-- Input must be non-negative.
--
-- Instead of division by 21, we compute
-- floor(floor((2^68+17)/21 * n) / 2^68) = floor((2^68+17)/21 * n/2^68) =
-- floor(n/21 + (n/2^63 * 17/32)/21) = floor(n/21),
-- because n/2^63 * 17/32 < 1.
{-# INLINE quotRem21 #-}
quotRem21 :: Int -> (Int, Int)
quotRem21 n
    | finiteBitSize (0 :: Word) /= 64
    = n `quotRem` 21
    | otherwise
    = (fromIntegral q, fromIntegral (w - 21 * q))
    where
        w = fromIntegral n
        high = highMul w 14054662151397753613 -- (2^68+17)/21
        q = high `shiftR` 4

-- Input must be non-negative.
--
-- Instead of division by 28, we compute
-- floor(floor((2^65+3)/7 * n) / 2^67) = floor((2^65+3)/7 * n/2^67) =
-- floor(n/28 + (n/2^63 * 3/4)/28) = floor(n/28),
-- because n/2^63 * 3/4 < 1.
{-# INLINE quotRem28 #-}
quotRem28 :: Int -> (Int, Int)
quotRem28 n
    | finiteBitSize (0 :: Word) /= 64
    = n `quotRem` 28
    | otherwise
    = (fromIntegral q, fromIntegral r)
    where
        w = fromIntegral n
        high = highMul w 5270498306774157605 -- (2^65+3)/7
        q = high `shiftR` 3
        prod = (q `shiftL` 3 - q) `shiftL` 2
        r = w - prod
