{-# LANGUAGE TemplateHaskell #-}

-- |
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--

import           Control.DeepSeq           (NFData)
import           Criterion.Main            (Benchmark, bench, bgroup,
                                            defaultConfig, nf, runMode)
import           Criterion.Main.Options    (describe)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.ICU             as TI
import qualified Data.Text.Normalize       as UT
import           Options.Applicative.Extra (execParser)
import           Path                      (Dir, Path, Rel, mkRelDir,
                                            toFilePath, (</>))
import           Path.IO                   (listDir)
import           System.FilePath           (dropExtensions, takeFileName)

textICUFuncs :: [(String, Text -> Text)]
textICUFuncs =
    [ ("NFD", TI.normalize TI.NFD) ]

unicodeTransformFuncs :: [(String, Text -> Text)]
unicodeTransformFuncs =
    [ ("NFD", UT.normalize UT.NFD) ]

dataDir :: Path Rel Dir
dataDir = $(mkRelDir "benchmark") </> $(mkRelDir "data")

-- Truncate or expand all datasets to this size to provide a normalized
-- measurement view across all datasets and to reduce the effect of noise
-- because of the datasets being too small.
dataSetSize :: Int
dataSetSize = 1000000

-- XXX read the test data fully before starting the test
-- returns test name and data
getDataSet :: Path b Dir -> IO [(String, Text)]
getDataSet dir = do
    dataFiles <- fmap snd (listDir dir)
    dataList  <- mapM (readFile . toFilePath) dataFiles
    return $ zip (map justName dataFiles)
                 (map (T.pack . take dataSetSize . cycle) dataList)
    where
        justName  = dropExtensions . takeFileName . toFilePath

benchAll :: NFData b => (String, a -> b) -> (String, a) -> Benchmark
benchAll (implName, func) (dataName, txt)=
    bench (implName ++ "/" ++ dataName) (nf func txt)

main :: IO ()
main = do
    mode    <- execParser (describe defaultConfig)
    dataSet <- getDataSet dataDir
    runMode mode
        [ bgroup "text-icu"           $ benchAll <$> textICUFuncs <*> dataSet
        , bgroup "unicode-transforms" $ benchAll <$> unicodeTransformFuncs
                                                 <*> dataSet
        ]
