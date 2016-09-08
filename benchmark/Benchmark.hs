{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative ((<$>), (<*>))
#endif
import           Control.DeepSeq           (NFData)
import           Criterion.Main            (Benchmark, bench, bgroup,
                                            defaultConfig, env, nf, runMode)
import           Criterion.Main.Options    (describe)
import           Data.Bifunctor            (second)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.NormalizeUTF8Proc as UTF8Proc
import qualified Data.Text.NormalizeNative as UTText
import qualified Data.String.Normalize     as UT
import           Options.Applicative.Extra (execParser)
import           Path                      (Dir, Path, Rel, mkRelDir,
                                            toFilePath, (</>))
import           Path.IO                   (listDir)
import           System.FilePath           (dropExtensions, takeFileName)

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

utf8ProcFuncs :: [(String, Text -> Text)]
utf8ProcFuncs =
    [ ("NFD", UTF8Proc.normalize UTF8Proc.NFD)
    , ("NFKD", UTF8Proc.normalize UTF8Proc.NFKD)
    , ("NFC", UTF8Proc.normalize UTF8Proc.NFC)
    , ("NFKC", UTF8Proc.normalize UTF8Proc.NFKC)
    ]

unicodeTransformTextFuncs :: [(String, Text -> Text)]
unicodeTransformTextFuncs =
    [ ("NFD", UTText.normalize UTText.NFD)
    , ("NFKD", UTText.normalize UTText.NFKD)
    , ("NFC", UTText.normalize UTText.NFC)
    , ("NFKC", UTText.normalize UTText.NFKC)
    ]

unicodeTransformFuncs :: [(String, String -> String)]
unicodeTransformFuncs =
    [ ("NFD", UT.normalize UT.NFD)
    , ("NFKD", UT.normalize UT.NFKD)
    , ("NFC", UT.normalize UT.NFC)
    , ("NFKC", UT.normalize UT.NFKC)
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

main :: IO ()
main = do
    mode    <- execParser (describe defaultConfig)
    dataFiles <- fmap (map toFilePath . snd) (listDir dataDir)
    runMode mode
        [
#ifdef BENCH_ICU
          bgroup "text-icu"
              $ makeBench <$> textICUFuncs <*> (map txtInput dataFiles)
        ,
#endif
          bgroup "utf8proc"
            $ makeBench <$> utf8ProcFuncs <*> (map txtInput dataFiles)
        , bgroup "unicode-transforms-string"
            $ makeBench <$> unicodeTransformFuncs <*> (map strInput dataFiles)
        , bgroup "unicode-transforms-text"
            $ makeBench <$> unicodeTransformTextFuncs
                        <*> (map txtInput dataFiles)
        ]
