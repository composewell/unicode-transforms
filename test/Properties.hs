{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text.Normalize as T
import Data.Text.Normalize (NormalizationMode)
import Data.Unicode.Internal.Division (quotRem21, quotRem28)
import QuickCheckUtils ()
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.Hspec as H
import Test.QuickCheck (NonNegative(..))

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
main =
      hspec
    $ H.parallel
    $ modifyMaxSuccess (const 10000)
    $ do
        prop "quotRem28" $ \(NonNegative n) -> n `quotRem` 28 == quotRem28 n
        prop "quotRem28 maxBound" $ \(NonNegative n) ->
            let n1 = maxBound - n
             in n1 `quotRem` 28 == quotRem28 n1
        prop "quotRem21" $ \(NonNegative n) -> n `quotRem` 21 == quotRem21 n
        prop "quotRem21 maxBound" $ \(NonNegative n) ->
            let n1 = maxBound - n
             in n1 `quotRem` 21 == quotRem21 n1
#ifdef HAS_ICU
        prop "Comparing random strings with ICU..." t_normalizeCompareICU
#else
        prop "Checking non-empty results for random strings..." t_normalize
#endif
