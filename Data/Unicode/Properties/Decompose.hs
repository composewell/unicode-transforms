-- |
-- Module      : Data.Unicode.Properties.Decompose
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
--
module Data.Unicode.Properties.Decompose
(decomposeChar, decomposeCharHangul, isHangul, isDecomposable)
where

import           Data.Unicode.Properties.Decomposable    (isDecomposable)
import           Data.Unicode.Properties.DecomposeHangul (decomposeCharHangul,
                                                          isHangul)
import           Data.Unicode.Properties.Decompositions  (decomposeChar)
