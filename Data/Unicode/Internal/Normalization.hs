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

import           Control.Monad                           (ap)
import           Data.List                               (sortBy)
import           Data.Ord                                (comparing)

import qualified Data.Unicode.Properties.CombiningClass  as CC
import qualified Data.Unicode.Properties.Decompose       as NFD

decompose :: String -> String
decompose = decomposeChars []
--decompose = reorder
--decompose = decomposeChars
--decompose = dcOverhead
--decompose = combinableOverhead
--decompose = ccOverhead
--decompose str = (ccOverhead str) ++ (dcOverhead str)
    where
        decomposeChars buf [] = buf
        decomposeChars buf (x : xs) = do
            case NFD.isHangul x of
                True ->
                    buf ++ case NFD.decomposeCharHangul x of
                        Left  (l, v)    -> (l : v : decomposeChars [] xs)
                        Right (l, v, t) -> (l : v : t : decomposeChars [] xs)
                False ->
                    -- TODO: return fully decomposed form to avoid rechecks on
                    -- recursion or at least do recursive decompose strictly
                    case NFD.isDecomposable x of
                        True  ->  let ys = NFD.decomposeChar x ++ xs
                                  in decomposeChars buf ys
                        False -> reorder buf x xs

        -- TODO: how to sort when the characters have same combining classes
        -- (reorder buffer) (input char) (undecomposed list)
        reorder [] x xs = decomposeChars [x] xs

        -- input char is a starter, flush the reorder buffer
        reorder buf x xs | not (CC.isCombining x) =
            buf ++ (x : decomposeChars [] xs)

        -- input char is combining and there is a starter char in the buffer
        -- flush the starter char and add the combining char to the buffer
        reorder buf@[y] x xs | not (CC.isCombining y) =
                y : decomposeChars [x] xs

        -- more than one combining char in the buffer, sort them
        reorder buf x xs = decomposeChars (sortCluster (buf ++ [x])) xs
            where
                sortCluster =   map fst
                              . sortBy (comparing snd)
                              . map (ap (,) CC.getCombiningClass)

        -- to measure database lookup overhead
        combinableOverhead xs = show $ sum $
            map (\c -> if CC.isCombining c then 1 else 0 :: Int) xs
        ccOverhead xs = show $ sum $
            map CC.getCombiningClass xs
        dcOverhead xs = show $ sum $
            map (\c -> if NFD.isDecomposable c then 1 else 0 :: Int) xs
