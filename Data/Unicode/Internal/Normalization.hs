-- |
-- Module      : Data.Unicode.Internal.Normalization
-- Copyright   : (c) 2014–2015 Antonio Nikishaev
--               (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
--
-- Experimenting with native haskell decompose implementaiton
--
module Data.Unicode.Internal.Normalization
    (
      decompose
    )
    where

import           Control.Monad                          (ap)
import           Data.List                              (sortBy)
import qualified Data.List.Sequences                    as ListSeq
import           Data.Ord                               (comparing)
import qualified Data.Unicode.Properties.CombiningClass as CC
import qualified Data.Unicode.Properties.Decompositions as NFD
--import qualified Data.Text as T
--import qualified Data.Text.Lazy as TL

decompose :: String -> String
decompose = map fst
            . concatMap (sortBy (comparing snd))
            . splitToCCClusters
            . decomposeString
    where
        decomposeString [] = []
        decomposeString [x] =
            case NFD.isDecomposable x of
                True -> decomposeString (NFD.decomposeChar x)
                False -> [x]
        decomposeString (x : xs) = decomposeString [x] ++ decomposeString xs


        splitToCCClusters :: [Char] -> [[(Char,Int)]]
        splitToCCClusters = ListSeq.splitSeq brk
                            . map (ap (,) CC.getCombiningClass)
            where brk _ (_,0) = False
                  brk _ _     = True
