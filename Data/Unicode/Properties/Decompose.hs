-- |
-- Module      : Data.Unicode.Properties.Decompose
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-3-Clause
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
--
module Data.Unicode.Properties.Decompose
    ( decomposeChar
    , decomposeCharHangul
    , DecomposeMode(..)
    , isHangul
    , jamoLFirst
    , isDecomposable
    )
where

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

{-# INLINE isDecomposable #-}
isDecomposable :: DecomposeMode -> Char -> Bool
isDecomposable DecomposeNFD  = D.isDecomposable
isDecomposable DecomposeNFKD = K.isDecomposable
