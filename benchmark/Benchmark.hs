{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-3-Clause
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Control.DeepSeq (NFData)
import Data.Text (Text)
import Path (Dir, Path, Rel, mkRelDir, toFilePath, (</>))
import Path.IO (listDir)
import System.FilePath (dropExtensions, takeFileName)

import Gauge.Main (Benchmark, bench, bgroup, defaultMain, env, nf)

import qualified Data.Text as T
import qualified Data.Text.Normalize as UTText

#ifdef BENCH_ICU
import qualified Data.Text.ICU             as TI

textICUFuncs :: [(String, Text -> Text)]
textICUFuncs =
    [ ("NFD", TI.normalize TI.NFD)
    , ("NFKD", TI.normalize TI.NFKD)
    , ("NFC", TI.normalize TI.NFC)
    , ("NFKC", TI.normalize TI.NFKC)
    ]
#endif

unicodeTransformTextFuncs :: [(String, Text -> Text)]
unicodeTransformTextFuncs =
    [ ("NFD", UTText.normalize UTText.NFD)
    , ("NFKD", UTText.normalize UTText.NFKD)
    , ("NFC", UTText.normalize UTText.NFC)
    , ("NFKC", UTText.normalize UTText.NFKC)
    ]

dataDir :: Path Rel Dir
dataDir = $(mkRelDir "benchmark") </> $(mkRelDir "data")

-- Truncate or expand all datasets to this size to provide a normalized
-- measurement view across all datasets and to reduce the effect of noise
-- because of the datasets being too small.
dataSetSize :: Int
dataSetSize = 1000000

makeBench :: (NFData a, NFData b) => (String, a -> b) -> (String, IO a) -> Benchmark
makeBench (implName, func) (dataName, setup) =
    env setup (\txt -> bench (implName ++ "/" ++ dataName) (nf func txt))

strInput :: FilePath -> (String, IO String)
strInput file = (dataName file,
                 fmap (take dataSetSize . cycle) (readFile file))
    where dataName = dropExtensions . takeFileName

txtInput :: FilePath -> (String, IO Text)
txtInput file = second (fmap T.pack) (strInput file)
    where second f (a, b) = (a, f b)

main :: IO ()
main = do
    dataFiles <- fmap (map toFilePath . snd) (listDir dataDir)
    defaultMain $
        [
#ifdef BENCH_ICU
          bgroup "text-icu"
              $ makeBench <$> textICUFuncs <*> (map txtInput dataFiles)
        ,
#endif
          bgroup "unicode-transforms-text"
            $ makeBench <$> unicodeTransformTextFuncs
                        <*> (map txtInput dataFiles)
        ]
