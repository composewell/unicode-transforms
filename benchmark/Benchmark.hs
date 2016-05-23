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
                                            defaultConfig, env, nf, runMode)
import           Criterion.Main.Options    (describe)
import           Data.Bifunctor            (second)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.ICU             as TI
import qualified Data.Text.Normalize       as UTF8Proc
import qualified Data.Text.NormalizeNative as UT
import           Options.Applicative.Extra (execParser)
import           Path                      (Dir, Path, Rel, mkRelDir,
                                            toFilePath, (</>))
import           Path.IO                   (listDir)
import           System.FilePath           (dropExtensions, takeFileName)

textICUFuncs :: [(String, Text -> Text)]
textICUFuncs =
    [ ("NFD", TI.normalize TI.NFD) ]

utf8ProcFuncs :: [(String, Text -> Text)]
utf8ProcFuncs =
    [ ("NFD", UTF8Proc.normalize UTF8Proc.NFD) ]

unicodeTransformFuncs :: [(String, String -> String)]
unicodeTransformFuncs =
    [ ("NFD", UT.normalize UT.NFD) ]

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

main :: IO ()
main = do
    mode    <- execParser (describe defaultConfig)
    dataFiles <- fmap (map toFilePath . snd) (listDir dataDir)
    runMode mode
        [ bgroup "text-icu"           $ makeBench <$> textICUFuncs
                                                  <*> (map txtInput dataFiles)
        , bgroup "utf8proc"           $ makeBench <$> utf8ProcFuncs
                                                  <*> (map txtInput dataFiles)
        , bgroup "unicode-transforms" $ makeBench <$> unicodeTransformFuncs
                                                  <*> (map strInput dataFiles)
        ]
