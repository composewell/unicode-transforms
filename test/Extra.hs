module Main (main) where

import Control.Exception.Base (assert)
import Data.Text.Normalize (NormalizationMode(..))
import Data.Unicode.Properties.CombiningClass (getCombiningClass)
import QuickCheckUtils ()
import Test.QuickCheck

-- Misc, trivial, boundary test cases

t_eq :: NormalizationMode -> Bool
t_eq mode = mode == NFD || mode /= NFD

t_enum :: NormalizationMode -> Bool
t_enum mode = mode == toEnum (fromEnum mode)
    && if (fromEnum mode == 0)
          then length (enumFromThen mode (succ mode)) /= 0
          else succ (pred mode) == mode
    && length (enumFrom mode) /= 0

t_show :: NormalizationMode -> Bool
t_show mode =
       not (null (showsPrec 0 mode "x"))
    && not (null (show mode))
    && not (null (showList [mode] "x"))


main :: IO ()
main = do
    assert (getCombiningClass '\0' == 0 &&
            getCombiningClass '\125143' == 0)
           (putStrLn "Test out of range combining class")

    -- test mode instances
    quickCheck t_eq
    quickCheck t_enum
    quickCheck t_show
