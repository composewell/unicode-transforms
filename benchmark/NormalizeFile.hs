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

import qualified Data.Unicode.Normalize as UT

-- Truncate or expand all datasets to this size to provide a normalized
-- measurement view across all datasets and to reduce the effect of noise
-- because of the datasets being too small.
dataSetSize :: Int
dataSetSize = 1000000

strInput :: FilePath -> IO String
strInput file = fmap (take dataSetSize . cycle) (readFile file)

main :: IO ()
main = do
    [file] <- getArgs
    input <- strInput file
    UT.normalize UT.NFD input `deepseq` return ()
