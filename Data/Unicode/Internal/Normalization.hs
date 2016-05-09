-- |
-- Module      : Data.Unicode.Internal.Normalization
-- Copyright   : (c) 2016 Harendra Kumar
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
import           Data.Ord                               (comparing)
import qualified Data.Unicode.Properties.CombiningClass as CC
import qualified Data.Unicode.Properties.Decompositions as NFD

decompose :: String -> String
decompose = reorder . decomposeChars
--decompose = reorder
--decompose = decomposeChars
--decompose str = (ccOverhead str) ++ (dcOverhead str)
--decompose str = dcOverhead str
    where
        decomposeChars [] = []
        decomposeChars (x : xs) = do
            case NFD.isDecomposable x of
                True ->    decomposeChars (NFD.decomposeChar x)
                        ++ decomposeChars xs
                False -> x : decomposeChars xs

        -- TODO try streaming fusion on lists
            -- can compose or use map efficiently
            -- use takewhile/break on char/cc tuples

        -- sequence of chars with 0 combining class
        reorder (x : xs) | cc == 0 = x : reorder xs
                where cc = CC.getCombiningClass x

        -- one non-zero cc char between two zero cc chars
        reorder (x1 : x2 : xs) | cc2 == 0 = x1 : x2 : reorder xs
                where cc2 = CC.getCombiningClass x2

        reorder [x] = [x]
        reorder []  = []

        -- sequence of two or more nonzero cc chars
        reorder xs = sortCluster ys ++ reorder zs
            where
                (ys, zs) = break ((== 0) . CC.getCombiningClass) xs
                sortCluster =   map fst
                              . sortBy (comparing snd)
                              . map (ap (,) CC.getCombiningClass)

        -- to measure database lookup overhead
        ccOverhead xs = show $ sum $
            map CC.getCombiningClass xs
        dcOverhead xs = show $ sum $
            map (\c -> if NFD.isDecomposable c then 1 else 0 :: Int) xs
