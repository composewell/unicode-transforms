{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module QuickCheckUtils () where

import           Control.DeepSeq        (NFData (..))
import qualified Data.Text              as T
import           Data.Unicode.Normalize (NormalizationMode (..))
import           Test.QuickCheck        (Arbitrary (..), elements)

instance NFData Ordering where
    rnf !_  = ()

instance Arbitrary T.Text where
    arbitrary = T.pack `fmap` arbitrary
    shrink = map T.pack . shrink . T.unpack

instance Arbitrary NormalizationMode where
    arbitrary = elements [NFD, NFKD, NFC, NFKC]
