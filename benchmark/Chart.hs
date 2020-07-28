{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import BenchShow
import Data.List

selectBench
    :: (SortColumn -> Maybe GroupStyle -> Either String [(String, Double)])
    -> [String]
selectBench f =
      reverse
    $ fmap fst
    $ either
        (const
            $ either error (sortOn snd)
            $ f (ColumnIndex 0) (Just PercentDiff))
        (sortOn snd)
    $ f (ColumnIndex 1) (Just PercentDiff)

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
            , selectGroups = \gs ->
                filterGroup "unicode-transforms" gs ++ filterGroup "ICU" gs
            , presentation = Groups Absolute
            }
    -- graph "results.csv" "unicode-graph" cfg
    report "results.csv" Nothing cfg

    where

    filterGroup grp gs = filter (\(name,_) -> name == grp) gs
