{-# LANGUAGE TemplateHaskell #-}

-- |
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--

import           Control.DeepSeq     (NFData)
import           Criterion.Main      (Benchmark, bench, bgroup, defaultMain, nf)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.ICU       as TI
import qualified Data.Text.Normalize as UT
import           Path                (Dir, Path, Rel, mkRelDir, toFilePath,
                                      (</>))
import           Path.IO             (listDir)
import           System.FilePath     (dropExtensions, takeFileName)

implementations :: [(String, Text -> Text)]
implementations =
    [ ("text-icu"           , TI.normalize TI.NFD)
    , ("unicode-transforms" , UT.normalize UT.NFD)
    ]

dataDir :: Path Rel Dir
dataDir = $(mkRelDir "benchmark") </> $(mkRelDir "data")

-- XXX read the test data fully before starting the test
-- returns test name and data
getDataSet :: Path b Dir -> IO [(String, Text)]
getDataSet dir = do
    dataFiles <- fmap snd (listDir dir)
    dataList  <- mapM (readFile . toFilePath) dataFiles
    return $ zip (map justName dataFiles) (map T.pack dataList)
    where
        justName  = dropExtensions . takeFileName . toFilePath

benchImpl :: NFData b => (String, a -> b) -> (String, a) -> Benchmark
benchImpl (implName, func) (dataName, txt)=
    bench (implName ++ "/" ++ dataName) (nf func txt)

main :: IO ()
main = do
    dataSet <- getDataSet dataDir
    defaultMain
        [
            bgroup "Normalize NFD" $
                benchImpl <$> implementations <*> dataSet
        ]
