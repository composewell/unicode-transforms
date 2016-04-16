{-# LANGUAGE TemplateHaskell #-}

-- |
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--

import           Control.DeepSeq           (NFData, deepseq)
import           Criterion.Main            (Benchmark, bench, bgroup,
                                            defaultConfig, nf, runMode)
import           Criterion.Main.Options    (describe)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Internal.Fusion as T
import qualified Data.Text.ICU             as TI
import           Options.Applicative.Extra (execParser)
import           Path                      (Dir, Path, Rel, mkRelDir,
                                            toFilePath, (</>))
import           Path.IO                   (listDir)
import           System.FilePath           (dropExtensions, takeFileName)
import Data.Char (ord, chr)
import Control.Arrow (second)

textICUFuncs :: [(String, Text -> Text)]
textICUFuncs =
    [ ("NFD", TI.normalize TI.NFD) ]

stringOp = map (chr . (+ 1) . ord)
textOp = T.unstream . T.stream

unicodeTransformFuncs :: [(String, String -> String)]
unicodeTransformFuncs =
    [ ("StringOp", stringOp) ]

textFuncs :: [(String, Text -> Text)]
textFuncs =
    [ ("text-stream-unstream", textOp) ]

dataDir :: Path Rel Dir
dataDir = $(mkRelDir "benchmark") </> $(mkRelDir "data")

-- Truncate or expand all datasets to this size to provide a normalized
-- measurement view across all datasets and to reduce the effect of noise
-- because of the datasets being too small.
dataSetSize :: Int
dataSetSize = 1000000

-- XXX read the test data fully before starting the test
-- returns test name and data
getDataSet :: Path b Dir -> IO [(String, String)]
getDataSet dir = do
    dataFiles <- fmap snd (listDir dir)
    dataList  <- mapM (readFile . toFilePath) dataFiles
    return $ zip (map justName dataFiles)
                 (map (take dataSetSize . cycle) dataList)
    where
        justName  = dropExtensions . takeFileName . toFilePath

benchAll :: NFData b => (String, a -> b) -> (String, a) -> Benchmark
benchAll (implName, func) (dataName, txt)=
    bench (implName ++ "/" ++ dataName) (nf func txt)

main :: IO ()
main = do
    mode    <- execParser (describe defaultConfig)
    dataSet <- getDataSet dataDir
    let textDataSet = map (second T.pack) dataSet
    dataSet `deepseq` textDataSet `deepseq` runMode mode
        [ bgroup "text-icu"           $ benchAll <$> textICUFuncs <*> textDataSet
         -- , bgroup "String" $ benchAll <$> unicodeTransformFuncs
         --                                        <*> dataSet
         , bgroup "Text" $ benchAll <$> textFuncs
                                                 <*> textDataSet
        ]
