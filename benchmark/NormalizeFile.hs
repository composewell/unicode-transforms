{-# LANGUAGE TemplateHaskell #-}

-- |
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--

import           Control.DeepSeq        (deepseq)
import           System.Environment     (getArgs)

import qualified Data.Text.Normalize as UT
import Data.Text (pack, Text)

-- Truncate or expand all datasets to this size to provide a normalized
-- measurement view across all datasets and to reduce the effect of noise
-- because of the datasets being too small.
dataSetSize :: Int
dataSetSize = 1000000

txtInput :: FilePath -> IO Text
txtInput file = fmap (pack . take dataSetSize . cycle) (readFile file)

main :: IO ()
main = do
    [file] <- getArgs
    input <- txtInput file
    UT.normalize UT.NFD input `deepseq` return ()
