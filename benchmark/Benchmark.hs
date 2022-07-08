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
#ifdef USE_TASTY_BENCH
import Gauge.Main (bcompare)
#endif

import qualified Data.Text as T
import qualified Data.Text.Normalize as UTText

#ifdef HAS_ICU

#if MIN_VERSION_text_icu(0,8,0)
import qualified Data.Text.ICU.Normalize2 as ICU
#else
import qualified Data.Text.ICU.Normalize as ICU
#endif

textICUFuncs :: [(String, Text -> Text)]
textICUFuncs =
    [ ("NFD", ICU.normalize ICU.NFD)
    , ("NFKD", ICU.normalize ICU.NFKD)
    , ("NFC", ICU.normalize ICU.NFC)
    , ("NFKC", ICU.normalize ICU.NFKC)
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

-- | Create a benchmark
makeBench
    :: (NFData a, NFData b)
    => (String, a -> b)
    -> String -- ^ Test name
    -> a
    -> Benchmark
makeBench (implName, func) dataName =
    \txt -> bench (makeTestName implName dataName) (nf func txt)

-- | Format a test name
makeTestName
    :: String -- ^ Implementation name
    -> String -- ^ Data name
    -> String
makeTestName implName dataName = implName ++ "/" ++ dataName

#if defined(HAS_ICU) || !defined(USE_TASTY_BENCH)
-- | Create refence benchmark
makeBenchRef
    :: (NFData a, NFData b)
    => (String, a -> b)
    -> (String, IO a)
    -> Benchmark
makeBenchRef impl (dataName, setup) = env setup (makeBench impl dataName)
#endif

-- | Create a benchmark which compares to the reference benchmark.
makeBenchComp
    :: (NFData a, NFData b)
    => String -- ^ Reference implementation
    -> (String, a -> b)
    -> (String, IO a)
    -> Benchmark
#ifdef USE_TASTY_BENCH
makeBenchComp implRef impl (dataName, setup) = env setup
    ( bcompare ("$NF == \"" <> (makeTestName (fst impl) dataName)
               <> "\" && $(NF-1) == \"" <> implRef <> "\"")
    . makeBench impl dataName)
#else
makeBenchComp _ = makeBenchRef
#endif

-- [TODO] read as text directly?
-- | Read a file as 'String'.
strInput :: FilePath -> (String, IO String)
strInput file =
    ( dataName file
    , take dataSetSize . cycle <$> readFile file )
    where dataName = dropExtensions . takeFileName

-- | Read a file as 'T.Text'.
txtInput :: FilePath -> (String, IO Text)
txtInput file = fmap T.pack <$> strInput file

main :: IO ()
main = do
    dataFiles <- map toFilePath . snd <$> listDir dataDir
    defaultMain $
        [
#ifdef HAS_ICU
          bgroup "text-icu"
            $ makeBenchRef <$> textICUFuncs <*> (map txtInput dataFiles)
        ,
#endif
          bgroup "unicode-transforms-text"
            $ makeBenchComp "text-icu"
                <$> unicodeTransformTextFuncs
                <*> (map txtInput dataFiles)
        ]
