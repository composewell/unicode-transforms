{-# LANGUAGE OverloadedStrings #-}

module Properties (tests) where

import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.Normalize                  as T
import           Data.Unicode.Normalize               (NormalizationMode (..))
import           QuickCheckUtils                      ()
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)

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

tests :: Test
tests =
  testGroup "Properties" [
    testProperty "t_normalize" t_normalize
  ]
