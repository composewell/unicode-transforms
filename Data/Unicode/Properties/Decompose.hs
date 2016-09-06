-- |
-- Module      : Data.Unicode.Properties.Decompose
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
--
module Data.Unicode.Properties.Decompose
    ( decomposeChar
    , decomposeCharHangul
    , DecomposeMode(..)
    , DecomposeResult(..)
    , isHangul
    , jamoLFirst
    , isDecomposable
    )
where

import           Data.BitArray                           (BitArray, lookupBit)
import           Data.Char                               (ord)

import qualified Data.Unicode.Properties.Decomposable    as D
import qualified Data.Unicode.Properties.DecomposableK   as K
import           Data.Unicode.Properties.DecomposeHangul ( decomposeCharHangul
                                                         , jamoLFirst
                                                         , isHangul)
import qualified Data.Unicode.Properties.Decompositions  as D
import qualified Data.Unicode.Properties.DecompositionsK as K

data DecomposeMode = DecomposeNFD | DecomposeNFKD

{-# INLINE decomposeChar #-}
decomposeChar :: DecomposeMode -> Char -> [Char]
decomposeChar DecomposeNFD  = D.decomposeChar
decomposeChar DecomposeNFKD = K.decomposeChar

{-# INLINE decomposeMin #-}
decomposeMin :: DecomposeMode -> Int
decomposeMin DecomposeNFD  = D.decomposeMin
decomposeMin DecomposeNFKD = K.decomposeMin

{-# INLINE decomposeMax #-}
decomposeMax :: DecomposeMode -> Int
decomposeMax DecomposeNFD  = D.decomposeMax
decomposeMax DecomposeNFKD = K.decomposeMax

{-# INLINE decomposeBitmap #-}
decomposeBitmap :: DecomposeMode -> BitArray
decomposeBitmap DecomposeNFD  = D.decomposeBitmap
decomposeBitmap DecomposeNFKD = K.decomposeBitmap

-- Hack Alert!
-- When we return just True and False GHC refactors the code to combine two
-- False or True results in a common function call and therefore not inlining
-- the code. To avoid that we return different results even though they
-- semantically mean the same thing.
--
data DecomposeResult = FalseA | FalseB | FalseC | TrueA

{-# INLINE isDecomposable #-}
isDecomposable :: DecomposeMode -> Char -> DecomposeResult
isDecomposable mode c | (ord c) <  decomposeMin mode = FalseA
isDecomposable mode c | (ord c) <= decomposeMax mode =
    case lookupBit (decomposeBitmap mode) (ord c) of
      True -> TrueA
      False -> FalseB
isDecomposable _ _ = FalseC
