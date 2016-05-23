-- |
-- Module      : Data.Unicode.Properties.Decompose
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
--
module Data.Unicode.Properties.Decompose
(decomposeChar)
where

import           Data.Char                               (chr, ord)
import           Data.Unicode.Properties.DecomposeHangul (decomposeCharHangul,
                                                          hangulFirst,
                                                          hangulLast)
import           Data.Unicode.Properties.Decompositions  (decomposeCharHigh,
                                                          decomposeCharLow)

-- Note: this is a partial function we do not expect to call this if
-- isDecomposable is false.
{-# NOINLINE decomposeChar #-}
decomposeChar :: Char -> [Char]
decomposeChar c
    | n <  hangulFirst = decomposeCharLow c
    | n <= hangulLast  = decomposeCharHangul c
    | otherwise        = decomposeCharHigh c
    where n = ord c
