-- |
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Data.Test.StreamUnstream
    (
        textOp
    ) where

import           Data.Text                 as T
import qualified Data.Text.Internal.Fusion as T

textOp = T.unstream . T.stream
