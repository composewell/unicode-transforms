{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text.Normalize as T
import Data.Text.Normalize (NormalizationMode)
import QuickCheckUtils ()
import Test.QuickCheck (maxSuccess, stdArgs, quickCheckWith)

#ifdef HAS_ICU
import qualified Data.Text.ICU as ICU

toICUMode :: NormalizationMode -> ICU.NormalizationMode
toICUMode mode =
    case mode of
       T.NFD  -> ICU.NFD
       T.NFKD -> ICU.NFKD
       T.NFC  -> ICU.NFC
       T.NFKC -> ICU.NFKC

t_normalizeCompareICU :: NormalizationMode -> Text -> Bool
t_normalizeCompareICU mode t =
    T.normalize mode t == ICU.normalize (toICUMode mode) t

#else
import qualified Data.Text as T

-- WARNING! These tests do not check the correctness of the output they
-- only check whether a non empty output is produced.

t_nonEmpty :: (Text -> Text) -> Text -> Bool
t_nonEmpty f t
    | T.null t  = T.null ft
    | otherwise = T.length ft > 0
  where ft = f t

-- Normalization

t_normalize :: NormalizationMode -> Text -> Bool
t_normalize mode = t_nonEmpty $ T.normalize mode
#endif

main :: IO ()
main = do
#ifdef HAS_ICU
    putStrLn "Comparing random strings with ICU..."
    quickCheckWith stdArgs { maxSuccess = 10000 } t_normalizeCompareICU
#else
    putStrLn "Checking non-empty results for random strings..."
    quickCheckWith stdArgs { maxSuccess = 10000 } t_normalize
#endif
