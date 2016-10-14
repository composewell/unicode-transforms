module Main (main) where

import Data.Text.Normalize (NormalizationMode(NFD))
import QuickCheckUtils ()
import Test.QuickCheck (quickCheck)

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
    -- test "NormalizationMode" instances
    quickCheck t_eq
    quickCheck t_enum
    quickCheck t_show
