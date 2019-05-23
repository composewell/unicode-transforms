{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import BenchShow
import Data.List

selectBench :: (SortColumn -> Either String [(String, Double)]) -> [String]
selectBench f =
    -- reverse
      fmap fst
    $ either error (sortOn snd) $ f $ ColumnIndex 0

main :: IO ()
main = do
    let icu = "Normalize NFD/text-icu/"
    let utrans = "Normalize NFD/unicode-transforms/"

    let icu1 = "text-icu/NFD/"
    let utrans1 = "unicode-transforms/NFD/"
    let utf8proc1 = "utf8proc/NFD/"

    let utrans2 = "unicode-transforms-text/NFD/"

    let cfg = defaultConfig
            { classifyBenchmark = \bs ->
                case stripPrefix icu bs of
                    Just x -> Just ("ICU", x)
                    Nothing ->
                        case stripPrefix icu1 bs of
                            Just x -> Just ("ICU", x)
                            Nothing ->
                                case stripPrefix utrans bs of
                                    Just x -> Just ("unicode-transforms",x)
                                    Nothing ->
                                        case stripPrefix utrans1 bs of
                                            Just x -> Just ("unicode-transforms",x)
                                            Nothing ->
                                                case stripPrefix utrans2 bs of
                                                    Just x -> Just ("unicode-transforms",x)
                                                    Nothing ->
                                                        fmap ("utf8proc",) $ stripPrefix utf8proc1 bs
            , selectBenchmarks = selectBench
            , presentation = Groups PercentDiff
            }
    -- graph "results.csv" "unicode-graph" cfg
    report "results.csv" Nothing cfg
