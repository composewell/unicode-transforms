-- |
-- Module      : Data.Unicode.Properties.Decompose
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
--
module Data.Unicode.Properties.Decompose
(decomposeChar, decomposeCharHangul, DecomposeResult(..), isHangul, isDecomposable)
where

import           Data.BitArray                           (lookupBit)
import           Data.Char                               (ord)

import           Data.Unicode.Properties.Decomposable    (decomposeBitmap,
                                                          decomposeMax,
                                                          decomposeMin)
import           Data.Unicode.Properties.DecomposeHangul (decomposeCharHangul,
                                                          isHangul)
import           Data.Unicode.Properties.Decompositions  (decomposeChar)

-- Hack Alert!
-- When we return just True and False GHC refactors the code to combine two
-- False or True results in a common function call and therefore not inlining
-- the code. To avoid that we return different results even though they
-- semantically mean the same thing.
--
data DecomposeResult = FalseA | FalseB | FalseC | TrueA

{-# INLINE isDecomposable #-}
isDecomposable :: Char -> DecomposeResult
isDecomposable c | (ord c) <  decomposeMin = FalseA
isDecomposable c | (ord c) <= decomposeMax = TrueA
    {-
    case lookupBit decomposeBitmap (ord c) of
      True -> TrueA
      False -> FalseB
-}
isDecomposable _ = FalseC
