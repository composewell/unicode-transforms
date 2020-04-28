{-# LANGUAGE MagicHash #-}
-- |
-- Module      : Data.Unicode.Internal.Bits
-- Copyright   : (c) 2020 Andrew Lelechenko
--               (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast, static bitmap lookup utilities

module Data.Unicode.Internal.Bits
    (
      lookupBit64
    ) where

import GHC.Exts
       (Addr#, Int(..), Word(..), indexWord64OffAddr#, and#, andI#,
        uncheckedIShiftRL#, uncheckedShiftL#)

-- | @lookup64 addr index@ looks up the bit stored at bit index @index@ using a
-- bitmap starting at the address @addr@. Looks up the 64-bit word containing
-- the bit and then the bit in that word. The caller must make sure that the
-- 64-bit word at the byte address (addr + index / 64) * 8 is legally
-- accessible memory.
--
lookupBit64 :: Addr# -> Int -> Bool
lookupBit64 addr# (I# index#) = W# (word## `and#` bitMask##) /= 0
  where
    wordIndex# = index# `uncheckedIShiftRL#` 6#
    word## = indexWord64OffAddr# addr# wordIndex#
    bitIndex# = index# `andI#` 63#
    bitMask## = 1## `uncheckedShiftL#` bitIndex#
