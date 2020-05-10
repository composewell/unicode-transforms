{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Text.RawString.QQ

import qualified Data.Text as T

-- Have this somewhere outside?
-- Or extract this info out from https://www.unicode.org/Public/UCD/latest/ucd/DerivedNormalizationProps.txt
derievedProperty_Full_Composition_Exclusion :: String
derievedProperty_Full_Composition_Exclusion = [r|
# Derived Property: Full_Composition_Exclusion
#  Generated from: Composition Exclusions + Singletons + Non-Starter Decompositions

0340..0341    ; Full_Composition_Exclusion # Mn   [2] COMBINING GRAVE TONE MARK..COMBINING ACUTE TONE MARK
0343..0344    ; Full_Composition_Exclusion # Mn   [2] COMBINING GREEK KORONIS..COMBINING GREEK DIALYTIKA TONOS
0374          ; Full_Composition_Exclusion # Lm       GREEK NUMERAL SIGN
037E          ; Full_Composition_Exclusion # Po       GREEK QUESTION MARK
0387          ; Full_Composition_Exclusion # Po       GREEK ANO TELEIA
0958..095F    ; Full_Composition_Exclusion # Lo   [8] DEVANAGARI LETTER QA..DEVANAGARI LETTER YYA
09DC..09DD    ; Full_Composition_Exclusion # Lo   [2] BENGALI LETTER RRA..BENGALI LETTER RHA
09DF          ; Full_Composition_Exclusion # Lo       BENGALI LETTER YYA
0A33          ; Full_Composition_Exclusion # Lo       GURMUKHI LETTER LLA
0A36          ; Full_Composition_Exclusion # Lo       GURMUKHI LETTER SHA
0A59..0A5B    ; Full_Composition_Exclusion # Lo   [3] GURMUKHI LETTER KHHA..GURMUKHI LETTER ZA
0A5E          ; Full_Composition_Exclusion # Lo       GURMUKHI LETTER FA
0B5C..0B5D    ; Full_Composition_Exclusion # Lo   [2] ORIYA LETTER RRA..ORIYA LETTER RHA
0F43          ; Full_Composition_Exclusion # Lo       TIBETAN LETTER GHA
0F4D          ; Full_Composition_Exclusion # Lo       TIBETAN LETTER DDHA
0F52          ; Full_Composition_Exclusion # Lo       TIBETAN LETTER DHA
0F57          ; Full_Composition_Exclusion # Lo       TIBETAN LETTER BHA
0F5C          ; Full_Composition_Exclusion # Lo       TIBETAN LETTER DZHA
0F69          ; Full_Composition_Exclusion # Lo       TIBETAN LETTER KSSA
0F73          ; Full_Composition_Exclusion # Mn       TIBETAN VOWEL SIGN II
0F75..0F76    ; Full_Composition_Exclusion # Mn   [2] TIBETAN VOWEL SIGN UU..TIBETAN VOWEL SIGN VOCALIC R
0F78          ; Full_Composition_Exclusion # Mn       TIBETAN VOWEL SIGN VOCALIC L
0F81          ; Full_Composition_Exclusion # Mn       TIBETAN VOWEL SIGN REVERSED II
0F93          ; Full_Composition_Exclusion # Mn       TIBETAN SUBJOINED LETTER GHA
0F9D          ; Full_Composition_Exclusion # Mn       TIBETAN SUBJOINED LETTER DDHA
0FA2          ; Full_Composition_Exclusion # Mn       TIBETAN SUBJOINED LETTER DHA
0FA7          ; Full_Composition_Exclusion # Mn       TIBETAN SUBJOINED LETTER BHA
0FAC          ; Full_Composition_Exclusion # Mn       TIBETAN SUBJOINED LETTER DZHA
0FB9          ; Full_Composition_Exclusion # Mn       TIBETAN SUBJOINED LETTER KSSA
1F71          ; Full_Composition_Exclusion # L&       GREEK SMALL LETTER ALPHA WITH OXIA
1F73          ; Full_Composition_Exclusion # L&       GREEK SMALL LETTER EPSILON WITH OXIA
1F75          ; Full_Composition_Exclusion # L&       GREEK SMALL LETTER ETA WITH OXIA
1F77          ; Full_Composition_Exclusion # L&       GREEK SMALL LETTER IOTA WITH OXIA
1F79          ; Full_Composition_Exclusion # L&       GREEK SMALL LETTER OMICRON WITH OXIA
1F7B          ; Full_Composition_Exclusion # L&       GREEK SMALL LETTER UPSILON WITH OXIA
1F7D          ; Full_Composition_Exclusion # L&       GREEK SMALL LETTER OMEGA WITH OXIA
1FBB          ; Full_Composition_Exclusion # L&       GREEK CAPITAL LETTER ALPHA WITH OXIA
1FBE          ; Full_Composition_Exclusion # L&       GREEK PROSGEGRAMMENI
1FC9          ; Full_Composition_Exclusion # L&       GREEK CAPITAL LETTER EPSILON WITH OXIA
1FCB          ; Full_Composition_Exclusion # L&       GREEK CAPITAL LETTER ETA WITH OXIA
1FD3          ; Full_Composition_Exclusion # L&       GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
1FDB          ; Full_Composition_Exclusion # L&       GREEK CAPITAL LETTER IOTA WITH OXIA
1FE3          ; Full_Composition_Exclusion # L&       GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
1FEB          ; Full_Composition_Exclusion # L&       GREEK CAPITAL LETTER UPSILON WITH OXIA
1FEE..1FEF    ; Full_Composition_Exclusion # Sk   [2] GREEK DIALYTIKA AND OXIA..GREEK VARIA
1FF9          ; Full_Composition_Exclusion # L&       GREEK CAPITAL LETTER OMICRON WITH OXIA
1FFB          ; Full_Composition_Exclusion # L&       GREEK CAPITAL LETTER OMEGA WITH OXIA
1FFD          ; Full_Composition_Exclusion # Sk       GREEK OXIA
2000..2001    ; Full_Composition_Exclusion # Zs   [2] EN QUAD..EM QUAD
2126          ; Full_Composition_Exclusion # L&       OHM SIGN
212A..212B    ; Full_Composition_Exclusion # L&   [2] KELVIN SIGN..ANGSTROM SIGN
2329          ; Full_Composition_Exclusion # Ps       LEFT-POINTING ANGLE BRACKET
232A          ; Full_Composition_Exclusion # Pe       RIGHT-POINTING ANGLE BRACKET
2ADC          ; Full_Composition_Exclusion # Sm       FORKING
F900..FA0D    ; Full_Composition_Exclusion # Lo [270] CJK COMPATIBILITY IDEOGRAPH-F900..CJK COMPATIBILITY IDEOGRAPH-FA0D
FA10          ; Full_Composition_Exclusion # Lo       CJK COMPATIBILITY IDEOGRAPH-FA10
FA12          ; Full_Composition_Exclusion # Lo       CJK COMPATIBILITY IDEOGRAPH-FA12
FA15..FA1E    ; Full_Composition_Exclusion # Lo  [10] CJK COMPATIBILITY IDEOGRAPH-FA15..CJK COMPATIBILITY IDEOGRAPH-FA1E
FA20          ; Full_Composition_Exclusion # Lo       CJK COMPATIBILITY IDEOGRAPH-FA20
FA22          ; Full_Composition_Exclusion # Lo       CJK COMPATIBILITY IDEOGRAPH-FA22
FA25..FA26    ; Full_Composition_Exclusion # Lo   [2] CJK COMPATIBILITY IDEOGRAPH-FA25..CJK COMPATIBILITY IDEOGRAPH-FA26
FA2A..FA6D    ; Full_Composition_Exclusion # Lo  [68] CJK COMPATIBILITY IDEOGRAPH-FA2A..CJK COMPATIBILITY IDEOGRAPH-FA6D
FA70..FAD9    ; Full_Composition_Exclusion # Lo [106] CJK COMPATIBILITY IDEOGRAPH-FA70..CJK COMPATIBILITY IDEOGRAPH-FAD9
FB1D          ; Full_Composition_Exclusion # Lo       HEBREW LETTER YOD WITH HIRIQ
FB1F          ; Full_Composition_Exclusion # Lo       HEBREW LIGATURE YIDDISH YOD YOD PATAH
FB2A..FB36    ; Full_Composition_Exclusion # Lo  [13] HEBREW LETTER SHIN WITH SHIN DOT..HEBREW LETTER ZAYIN WITH DAGESH
FB38..FB3C    ; Full_Composition_Exclusion # Lo   [5] HEBREW LETTER TET WITH DAGESH..HEBREW LETTER LAMED WITH DAGESH
FB3E          ; Full_Composition_Exclusion # Lo       HEBREW LETTER MEM WITH DAGESH
FB40..FB41    ; Full_Composition_Exclusion # Lo   [2] HEBREW LETTER NUN WITH DAGESH..HEBREW LETTER SAMEKH WITH DAGESH
FB43..FB44    ; Full_Composition_Exclusion # Lo   [2] HEBREW LETTER FINAL PE WITH DAGESH..HEBREW LETTER PE WITH DAGESH
FB46..FB4E    ; Full_Composition_Exclusion # Lo   [9] HEBREW LETTER TSADI WITH DAGESH..HEBREW LETTER PE WITH RAFE
1D15E..1D164  ; Full_Composition_Exclusion # So   [7] MUSICAL SYMBOL HALF NOTE..MUSICAL SYMBOL ONE HUNDRED TWENTY-EIGHTH NOTE
1D1BB..1D1C0  ; Full_Composition_Exclusion # So   [6] MUSICAL SYMBOL MINIMA..MUSICAL SYMBOL FUSA BLACK
2F800..2FA1D  ; Full_Composition_Exclusion # Lo [542] CJK COMPATIBILITY IDEOGRAPH-2F800..CJK COMPATIBILITY IDEOGRAPH-2FA1D

# Total code points: 1120
|]

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

-- Only checks the first plane, There are 17 planes in total
main :: IO ()
main = do
  let execlusionTable =
        lines derievedProperty_Full_Composition_Exclusion -- split into lines
        & map (dropWhile isSpace)                         -- trim leading spaces
        & filter (/= [])                                  -- remove blank lines
        & filter (not . ("#" `isPrefixOf`))               -- remove comments
        & map (takeWhile (not . isSpace))                 -- trim trailing comments
        & map makeExclusionType                           -- change type
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
