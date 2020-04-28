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
    $ either error (sortOn fst) $ f $ ColumnIndex 0

main :: IO ()
main = do
    let cfg = defaultConfig
            { classifyBenchmark = \bs ->
                case stripPrefix "text-icu/" bs of
                    Just x -> Just ("ICU", x)
                    Nothing ->
                        case stripPrefix "unicode-transforms-text/" bs of
                            Just x -> Just ("unicode-transforms",x)
                            Nothing -> error "unknown benchmark"
            , selectBenchmarks = selectBench
            , presentation = Groups PercentDiff
            }
    -- graph "results.csv" "unicode-graph" cfg
    report "results.csv" Nothing cfg
