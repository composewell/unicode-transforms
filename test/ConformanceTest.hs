{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Reference:  https://www.unicode.org/reports/tr15/tr15-18.html#Conformance%20Testing

import Data.Text (Text)
import Data.Unicode.Properties.Decompose (isHangul, isDecomposable, DecomposeMode(..))
import Data.Unicode.Properties.CombiningClass (getCombiningClass)
import Data.Char (chr, isSpace, ord, toUpper)
import Data.List (isPrefixOf, foldl', isInfixOf)
import Text.Printf (printf)

import Data.Text.Normalize

import qualified Data.Text as T

data ExclusionType = Range Int Int | Singleton Int
type ExclusionTable = [ExclusionType]

inExclusionTable :: Int -> ExclusionTable -> Bool
inExclusionTable i (Range x y:et) = i >= x && i <= y || inExclusionTable i et
inExclusionTable i (Singleton x:et) = i == x || inExclusionTable i et
inExclusionTable _ [] = False

checkConformance :: ExclusionTable -> Char -> Bool
checkConformance execlusionTable c =
  if isDecomposable DecomposeNFD c || isHangul c
  then cwe condInCanonicalOrder "y and z must be in canonical order"
    && cwe condXNEqY "x /= y"
    && cwe condYCharNonDecomposable "No character in y can have a canonical decomposition"
    && if ord c `inExclusionTable` execlusionTable
       then cwe (x /= z) "x is in exclusion table && x /= y"
       else cwe (x == z && w == normalize NFC v) "x is not in exclusion table && x == z && w == normalize NFC v"
  else cwe (x == y && y == z) "x is not decomposable && x == y && y == z)"
  where
    x = T.singleton c
    y = normalize NFD x
    z = normalize NFC x
    condInCanonicalOrder = isInCanonicalOrder y && isInCanonicalOrder z
    condXNEqY = x /= y
    condYCharNonDecomposable = T.foldl' (\x1 c1 -> x1 && not (isDecomposable DecomposeNFD c1)) True y
    w = '#' `T.cons` x `T.snoc` '\0334'
    v = normalize NFD w
    cwe = checkWithError c x y z w v

checkWithError :: Char -> Text -> Text -> Text -> Text -> Text -> Bool -> String -> Bool
checkWithError c x y z w v cond err =
  if cond then True
  else error $ unlines
       [ "failure: "
       , err
       , "c: 0x" ++ chrToHex c
       , "x: " ++ T.unpack x
       , "y: " ++ T.unpack y
       , "z: " ++ T.unpack z
       , "w: " ++ T.unpack w
       , "v: " ++ T.unpack v
       ]

chrToHex :: Char -> [Char]
chrToHex = (map toUpper) . (printf "%.4x") . ord

isInCanonicalOrder :: Text -> Bool
isInCanonicalOrder = chkCO (-1)
  where
    chkCO sim1 txt =
        case T.uncons txt of
            Nothing -> True
            Just (h, t) ->
                let si = getCombiningClass h
                in (not (sim1 > si) || si == 0) && chkCO si t

(&) :: a -> (a -> b) -> b
x & f = f x

-- Highest value of unicode code point representable by Haskell `Char`
maxCharCode :: Int
maxCharCode = 1114111

-- Extract a specific section out of DerivedNormalizationProps.txt
getSection :: String -> [String] -> [String]
getSection y contents = contents
                        & dropWhile (not . isInfixOf y)
                        & takeWhile (not . isInfixOf "===============")

main :: IO ()
main = do
  contents <- readFile "unicode-data/ucd/DerivedNormalizationProps.txt"
  let execlusionTable =
        lines contents                                              -- split into lines
        & getSection "Derived Property: Full_Composition_Exclusion" -- get proper section
        & map (dropWhile isSpace)                                   -- trim leading spaces
        & filter (/= [])                                            -- remove blank lines
        & filter (not . ("#" `isPrefixOf`))                         -- remove comments
        & map (takeWhile (not . isSpace))                           -- trim trailing comments
        & map makeExclusionType                                     -- change type
  if foldl' (\b a -> b && checkConformance execlusionTable (chr a)) True [0..maxCharCode]
    then putStrLn "Conformance test-suite: Passed"
    else error "Conformance test-suite: Failed"
  where
    toInt = read . ("0x" ++) :: String -> Int
    makeExclusionType x =
      if ".." `isInfixOf` x
      then let (a, b) = break (== '.') x
           in Range (toInt a) (toInt $ drop 2 b)
      else Singleton $ toInt x
