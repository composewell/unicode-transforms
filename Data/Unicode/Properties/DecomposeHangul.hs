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
    (decomposeCharHangul
    , hangulFirst
    , hangulLast
    , isHangul
    , isHangulLV

    , isJamo
    , jamoLFirst
    , jamoLIndex
    , jamoLLast

    , jamoVFirst
    , jamoVIndex
    , jamoVLast

    , jamoTFirst
    , jamoTCount
    , jamoTIndex
    , jamoLast

    , jamoNCount
    )
where

import Control.Exception              (assert)
import Data.Char                      (ord)
import GHC.Base                       (unsafeChr)
import Data.Unicode.Internal.Division (quotRem21, quotRem28)

-- Hangul characters can be decomposed algorithmically instead of via mappings

-------------------------------------------------------------------------------
-- General utilities used by decomposition as well as composition
-------------------------------------------------------------------------------

-- * https://www.unicode.org/versions/Unicode13.0.0/ch03.pdf
-- * https://en.wikipedia.org/wiki/List_of_Hangul_jamo
-- * https://www.unicode.org/reports/tr15/tr15-18.html#Hangul

-- D134   Standard Korean syllable block: A sequence of one or more L followed
-- by a sequence of  one  or  more  V  and  a  sequence  of  zero  or  more  T,
-- or any other sequence that is canonically equivalent

-- jamo leading
jamoLFirst, jamoLCount, jamoLLast :: Int
jamoLFirst  = 0x1100
jamoLCount = 19
jamoLLast = jamoLFirst + jamoLCount - 1

-- jamo vowel
jamoVFirst, jamoVCount, jamoVLast :: Int
jamoVFirst  = 0x1161
jamoVCount = 21
jamoVLast = jamoVFirst + jamoVCount - 1

-- jamo trailing
-- jamoTFirst does not represent a valid T, it represents a missing T i.e. LV
-- without a T. See comments under jamoTIndex .
jamoTFirst, jamoTCount :: Int
jamoTFirst  = 0x11a7
jamoTCount = 28

jamoLast :: Int
jamoLast = jamoTFirst + jamoTCount - 1

-- VCount * TCount
jamoNCount :: Int
jamoNCount = 588

-- hangul
hangulFirst, hangulLast :: Int
hangulFirst = 0xac00
hangulLast = hangulFirst + jamoLCount * jamoVCount * jamoTCount - 1

isHangul :: Char -> Bool
isHangul c = n >= hangulFirst && n <= hangulLast
    where n = ord c

isHangulLV :: Char -> Bool
isHangulLV c = assert (jamoTCount == 28)
    snd (quotRem28 (ord c - hangulFirst)) == 0

isJamo :: Char -> Bool
isJamo c = n >= jamoLFirst && n <= jamoLast
    where n = ord c

-- if it is a jamo L char return the index
jamoLIndex :: Char -> Maybe Int
jamoLIndex c
  | index >= 0 && index < jamoLCount = Just index
  | otherwise = Nothing
    where index = ord c - jamoLFirst

jamoVIndex :: Char -> Maybe Int
jamoVIndex c
  | index >= 0 && index < jamoVCount = Just index
  | otherwise = Nothing
    where index = ord c - jamoVFirst

-- Note that index 0 is not a valid index for a trailing consonant. Index 0
-- means no T, only LV syllable.
-- See Unicode 9.0.0: 3.12 (Hangul Syllable Decomposition)
-- TBase is set to one less than the beginning of the range of trailing
-- consonants, which starts at U+11A8.
jamoTIndex :: Char -> Maybe Int
jamoTIndex c
  | index > 0 && index < jamoTCount = Just index
  | otherwise = Nothing
    where index = ord c - jamoTFirst

-------------------------------------------------------------------------------
-- Hangul decomposition
-------------------------------------------------------------------------------

{-# INLINE decomposeCharHangul #-}
decomposeCharHangul :: Char -> (Char, Char, Char)
decomposeCharHangul c = (l, v, t)
    where
        i = (ord c) - hangulFirst
        !(tn, ti) = assert (jamoTCount == 28) quotRem28 i
        !(li, vi) = assert (jamoVCount == 21) quotRem21 tn

        l = unsafeChr (jamoLFirst + li)
        v = unsafeChr (jamoVFirst + vi)
        t = unsafeChr (jamoTFirst + ti)
