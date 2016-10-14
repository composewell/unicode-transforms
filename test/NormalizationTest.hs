{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- |
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--

import Control.Monad (when)
import Data.Char (chr, isSpace, ord, toUpper)
#if MIN_VERSION_base(4,8,0)
import Data.Function ((&))
#endif
import Data.List (intercalate, isPrefixOf)
import Data.List.Split (splitOn)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Normalize (NormalizationMode(NFD, NFKD, NFC, NFKC), normalize)
import Text.Printf (printf)

#if !MIN_VERSION_base(4,8,0)
(&) :: a -> (a -> b) -> b
x & f = f x
#endif

chrToHex :: Char -> [Char]
chrToHex = (map toUpper) . (printf "%.4x") . ord

strToHex :: [Char] -> String
strToHex = unwords . (map chrToHex)

checkEqual :: String -> (Text -> Text) -> (Text, Text) -> IO Bool
checkEqual opName op (c1, c2) =
    if c1 /= op c2 then do
        putStrLn $ opName ++ " "            ++ txtToHex c2
                          ++ " = "          ++ txtToHex (op c2)
                          ++ "; Expected: " ++ txtToHex c1
        return False
    else return True
    where
        txtToHex = strToHex . T.unpack

checkOp :: String -> NormalizationMode -> [(Text, Text)] -> IO Bool
checkOp name op pairs = do
    res <- mapM (checkEqual name ((normalize op))) pairs
    return $ all (== True) res

checkNFC :: (Text, Text, Text, Text, Text) -> IO Bool
checkNFC (c1, c2, c3, c4, c5) =
    checkOp "toNFC" NFC $
            concat [ map (c2,) [c1, c2, c3]
                   , map (c4,) [c4, c5]
                   ]

checkNFD :: (Text, Text, Text, Text, Text) -> IO Bool
checkNFD (c1, c2, c3, c4, c5) =
    checkOp "toNFD" NFD $
            concat [ map (c3,) [c1, c2, c3]
                   , map (c5,) [c4, c5]
                   ]

checkNFKC :: (Text, Text, Text, Text, Text) -> IO Bool
checkNFKC (c1, c2, c3, c4, c5) =
    checkOp "toNFKC" NFKC $ map (c4,) [c1, c2, c3, c4, c5]

checkNFKD :: (Text, Text, Text, Text, Text) -> IO Bool
checkNFKD (c1, c2, c3, c4, c5) =
    checkOp "toNFKD" NFKD $ map (c5,) [c1, c2, c3, c4, c5]

checkAllTestCases :: Int -> String -> IO ()
checkAllTestCases lineno line = do
    case splitOn ";" line of
        c1 : c2 : c3 : c4 : c5 : _ -> do
            let cps = map cpToText [c1, c2, c3, c4, c5]
            mapM_ (checkOneTestCase cps)
                  [checkNFD, checkNFKD, checkNFC, checkNFKC]
        _ -> error $ "Unrecognized line: " ++ line
    where
        cpToText xs = T.pack $ map (chr . read . ("0x" ++)) (words xs)

        checkOneTestCase cps f = do
            res <- f (tuplify cps)
            when (not res) $ do
                putStrLn ("Failed at line: " ++ show lineno)
                putStrLn line
                putStrLn $ codes ++ "; # (" ++ txt
                error "Bailing out"
            where
                strs  = map T.unpack cps
                codes = intercalate ";" $ map strToHex strs
                txt   = intercalate "; " (map T.unpack cps)

        tuplify [c1, c2, c3, c4, c5] = (c1, c2, c3, c4, c5)
        tuplify _ = error "tuplify bad arguments"

checkLine :: (Int, String) -> IO ()
checkLine (lineno, line) = do
    -- marker lines indicating a test block start with @
    if "@" `isPrefixOf` line
        then
            putStrLn line
        else
            checkAllTestCases lineno line

testNormalize :: FilePath -> IO ()
testNormalize file = do
    contents <- readFile file
    let ls = lines contents                        -- split into lines
         & map (dropWhile isSpace)                 -- trim leading spaces
         & zip [1..]                               -- add numbering
         & filter ((/= []) . snd)                  -- remove blank lines
         & filter (not . ("#" `isPrefixOf`) . snd) -- remove comments
    checkAll ls
    where
        checkAll (x:xs) = checkLine x >> checkAll xs
        checkAll []     = return ()

main :: IO ()
main = do
    testNormalize "unicode-data/ucd/NormalizationTest.txt"
    -- Additional test cases not in the unicode standard suite
    testNormalize "unicode-data/extra/NormalizationTest.txt"
