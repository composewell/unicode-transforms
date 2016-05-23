{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Data.Unicode.Properties.DecomposeHangul
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
--
module Data.Unicode.Properties.DecomposeHangul
(decomposeCharHangul, isHangul)
where

import           Data.Char (ord)
import           GHC.Base  (unsafeChr)

-- Hangul characters can be decomposed algorithmically instead of via mappings

-- jamo leading
jamoLFirst, jamoLCount :: Int
jamoLFirst  = 0x1100
jamoLCount = 19

-- jamo vowel
jamoVFirst, jamoVCount :: Int
jamoVFirst  = 0x1161
jamoVCount = 21

-- jamo trailing
jamoTFirst, jamoTCount :: Int
jamoTFirst  = 0x11a7
jamoTCount = 28

-- hangul
hangulFirst, hangulLast :: Int
hangulFirst = 0xac00
hangulLast = hangulFirst + jamoLCount * jamoVCount * jamoTCount - 1

isHangul :: Char -> Bool
isHangul c = n >= hangulFirst && n <= hangulLast
    where n = ord c

{-# INLINE decomposeCharHangul #-}
decomposeCharHangul :: Char -> Either (Char, Char) (Char, Char, Char)
decomposeCharHangul c
    | ti == 0   = Left (l, v)
    | otherwise = Right (l, v, t)
    where
        i = (ord c) - hangulFirst
        ti = i `rem` jamoTCount
        tn = i `quot` jamoTCount

        l = unsafeChr (jamoLFirst + tn `quot` jamoVCount)
        v = unsafeChr (jamoVFirst + tn `rem` jamoVCount)
        t = unsafeChr (jamoTFirst + ti)
