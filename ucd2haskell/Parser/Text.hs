{-# LANGUAGE LambdaCase, MultiWayIf #-}

-- |
-- Module      : Parser.Text
-- Copyright   : (c) 2022 Pierre Le Marre
--               (c) 2020 Composewell Technologies and Contributors
--               (c) 2016-2017 Harendra Kumar
--               (c) 2014-2015 Antonio Nikishaev
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental

-- The original Unicode database parser was taken from
-- https://github.com/composewell/unicode-transforms but was completely
-- rewritten from scratch to parse from UCD text files instead of XML, only
-- some types remain the same. That code in turn was originally taken from
-- https://github.com/llelf/prose (Antonio Nikishaev) and heavily modified by
-- Harendra Kumar.
--
module Parser.Text (genModules) where

import Control.Exception (catch, IOException)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bits (Bits(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Char (chr, isSpace)
-- import Data.Functor ((<&>))
import Data.Function ((&))
import Data.List (intersperse, unfoldr, sortBy)
import Data.Maybe (mapMaybe, isNothing)
import Data.Word (Word8)
import Streamly.Data.Fold (Fold)
import Streamly.Prelude (IsStream, SerialT)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)

-- import qualified Data.Set as Set
import qualified Streamly.Prelude as Stream
import qualified Streamly.Data.Fold as Fold
-- import qualified Streamly.Internal.Data.Fold as Fold
-- import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.FileSystem.Handle as Handle
import qualified System.IO as Sys
import qualified Streamly.Unicode.Stream as Unicode
import qualified Unicode.Char as UC

import Prelude hiding (pred)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data QuickCheck = QuickCheckNo | QuickCheckYes | QuickCheckMaybe
    deriving (Eq, Ord)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

apacheLicense :: Word -> String -> String
apacheLicense year modName =
    unlines
        [ "-- |"
        , "-- Module      : " ++ modName
        , "-- Copyright   : (c) "
            <> show year
            <> " Composewell Technologies and Contributors"
        , "-- License     : Apache-2.0"
        , "-- Maintainer  : streamly@composewell.com"
        , "-- Stability   : experimental"
        ]

-- readCodePoint :: String -> Char
-- readCodePoint = chr . read . ("0x"++)

-- readCodePointM :: String -> Maybe Char
-- readCodePointM "" = Nothing
-- readCodePointM u  = Just (readCodePoint u)

-- genSignature :: String -> String
-- genSignature = (<> " :: Char -> Bool")

-- | Check that var is between minimum and maximum of orderList
genRangeCheck :: String -> [Int] -> String
genRangeCheck var ordList =
    var
        <> " >= "
        <> show (minimum ordList)
        <> " && " <> var <> " <= " <> show (maximum ordList)

-- genBitmap :: String -> [Int] -> String
-- genBitmap funcName ordList =
--     unlines
--         [ "{-# INLINE " <> funcName <> " #-}"
--         , genSignature funcName
--         , funcName <> " = \\c -> let n = ord c in "
--               <> genRangeCheck "n" ordList <> " && lookupBit64 bitmap# n"
--         , "  where"
--         , "    bitmap# = \"" <> bitMapToAddrLiteral (positionsToBitMap ordList) "\"#"
--         ]

positionsToBitMap :: [Int] -> [Bool]
positionsToBitMap = go 0

    where

    go _ [] = []
    go i xxs@(x:xs)
        | i < x = False : go (i + 1) xxs
        | otherwise = True : go (i + 1) xs

bitMapToAddrLiteral
  :: [Bool]
  -- ^ Values to encode
  -> String
  -- ^ String to append
  -> String
bitMapToAddrLiteral bs cs = foldr encode cs (unfoldr mkChunks bs)

    where

    mkChunks :: [a] -> Maybe ([a], [a])
    mkChunks [] = Nothing
    mkChunks xs = Just $ splitAt 8 xs

    encode :: [Bool] -> String -> String
    encode chunk acc = '\\' : shows (toByte (padTo8 chunk)) acc

    padTo8 :: [Bool] -> [Bool]
    padTo8 xs
        | length xs >= 8 = xs
        | otherwise = xs ++ replicate (8 - length xs) False

    toByte :: [Bool] -> Int
    toByte xs = sum $ map (\i -> if xs !! i then 1 `shiftL` i else 0) [0..7]

-- genEnumBitmap
--   :: forall a. (Bounded a, Enum a, Show a)
--   => String
--   -- ^ Function name
--   -> a
--   -- ^ Default value
--   -> [a]
--   -- ^ List of values to encode
--   -> String
-- genEnumBitmap funcName def as = unlines
--     [ "{-# INLINE " <> funcName <> " #-}"
--     , funcName <> " :: Char -> Int"
--     , funcName <> " c = let n = ord c in if n >= "
--                <> show (length as)
--                <> " then "
--                <> show (fromEnum def)
--                <> " else lookupInt8 bitmap# n"
--     , "  where"
--     , "    bitmap# = \"" <> enumMapToAddrLiteral as "\"#"
--     ]

{-| Encode a list of values as a byte map, using their 'Enum' instance.

__Note:__ 'Enum' instance must respect the following:

* @fromEnum minBound >= 0x00@
* @fromEnum maxBound <= 0xff@
-}
enumMapToAddrLiteral
  :: forall a. (Bounded a, Enum a, Show a)
  => [a]
  -- ^ Values to encode
  -> String
  -- ^ String to append
  -> String
enumMapToAddrLiteral xs cs = foldr go cs xs

    where

    go :: a -> String -> String
    go x acc = '\\' : shows (toWord8 x) acc

    toWord8 :: a -> Word8
    toWord8 a = let w = fromEnum a in if 0 <= w && w <= 0xff
        then fromIntegral w
        else error $ "Cannot convert to Word8: " <> show a

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

genNormalizationPropertiesModule
    :: Monad m
    => String
    -> Fold m NormalizationPropertyLine String
genNormalizationPropertiesModule moduleName =
    done <$> Fold.foldl' step mempty

    where

    done (exports, bitmaps) = unlines
        [ apacheLicense 2022 moduleName
        , "{-# LANGUAGE MagicHash, PatternSynonyms #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , "module " <> moduleName
        -- , "( pattern No"
        -- , ", pattern MaybeCombining, pattern MaybeCombiningStarter"
        -- , ", pattern MaybeStarterDecomp, pattern MaybeStarterNoDecomp"
        -- , ", pattern YesStable, pattern YesStarterDecomp"
        -- , ", pattern YesStarterNoDecomp, pattern YesCombining"
        -- , ", pattern YesStable, pattern YesStarter, pattern YesCombining"
        , "( pattern YesStarter"
        -- , ", pattern MaybeCombiningStarter, pattern MaybeStarterNoDecomp"
        , ", pattern MaybeStarterNoDecomp"
        , ", pattern Decomposable"
        , ", pattern Combining"
        -- , "( pattern Decomposable, pattern Combining, pattern CombiningStarter"
        -- , ", pattern Starter, pattern Stable"
        , ", " <> mconcat (intersperse ", " (prop2FuncName <$> exports)) <> ")"
        , "where"
        , ""
        , "import Data.Char (ord)"
        , "import Unicode.Internal.Bits (lookupBit64, lookupIntN)"
        , ""
        -- , "pattern No :: Int"
        -- , "pattern No = " <> show no
        -- , ""
        -- , "pattern MaybeCombining, MaybeCombiningStarter, MaybeStarterDecomp, MaybeStarterNoDecomp :: Int"
        -- , "pattern MaybeCombiningStarter, MaybeStarterNoDecomp :: Int"
        , "pattern MaybeStarterNoDecomp :: Int"
        -- , "pattern MaybeCombining = " <> show maybeCombining
        -- , "pattern MaybeCombiningStarter = " <> show maybeCombiningStarter
        -- , "pattern MaybeStarterDecomp = " <> show maybeStarterDecomp
        , "pattern MaybeStarterNoDecomp = " <> show maybeStarterNoDecomp
        , ""
        -- , "pattern YesStable, YesStarterDecomp, YesStarterNoDecomp, YesCombining :: Int"
        -- , "pattern YesStable, YesStarter, YesCombining :: Int"
        -- , "pattern YesCombining = " <> show yesCombining
        -- , "pattern YesStarterDecomp = " <> show yesStarterDecomp
        -- , "pattern YesStarterNoDecomp = " <> show yesStarterNoDecomp
        , "pattern YesStarter :: Int"
        , "pattern YesStarter = " <> show yesStarter
        -- , "pattern YesStable = " <> show yesStable
        , ""
        , "pattern Decomposable :: Int"
        , "pattern Decomposable = " <> show decomposable
        , ""
        , "pattern Combining :: Int"
        , "pattern Combining = " <> show combining
        , ""
        , mconcat bitmaps
        ]

    step (props, bitmaps) (name, values) =
        ( name : props
        , (: bitmaps) $ case name of
            "NFD_QC"  -> genQuickCheckBitMapD              (prop2FuncName name) values
            "NFKD_QC" -> genQuickCheckBitMapD              (prop2FuncName name) values
            "NFC_QC"  -> genQuickCheckBitMapC UC.Canonical (prop2FuncName name) values
            _         -> genQuickCheckBitMapC UC.Kompat    (prop2FuncName name) values
        )

    prop2FuncName = ("is" <>)

    genQuickCheckBitMapD funcName
        = genBitmapD funcName
        . mapMaybe encodeD

    genQuickCheckBitMapC mode funcName
        = genBitmapC funcName
        . (\(acc, _, maxCP) -> (reverse acc, maxCP))
        . foldr (encodeC mode) (mempty, 0, 0)
        . sortBy (flip compare)

    -- NFD & NFKD: Encode Quick_Check value on 1 bit
    encodeD (cp, quickCheck) = case quickCheck of
        QuickCheckYes   -> Nothing
        QuickCheckNo    -> Just cp
        QuickCheckMaybe -> error ("Unexpected Maybe value for: " <> show cp)

    -- NFC & NFKC: Encode Quick_Check value
    encodeC
        :: UC.DecomposeMode
        -> (Int, QuickCheck)
        -> ([Word8], Int, Int)
        -> ([Word8], Int, Int)
    encodeC mode v@(cp, quickCheck) (acc, expected, _maxCP) =
        if cp > expected
            -- Yes: check if starter
            then let c = chr expected in if
                -- Yes, stable
                | isStable mode c
                -> encodeC mode v
                    ( yesStable : acc
                    , succ expected
                    , expected )
                -- Yes, combining, not decomposable
                | UC.isCombining c
                -> encodeC mode v
                    ( yesCombining : acc
                    , succ expected
                    , expected )
                -- Yes, starter, decomposable
                | UC.isDecomposable mode c
                -> encodeC mode v
                    ( yesStarterDecomp : acc
                    , succ expected
                    , expected )
                -- Yes, starter, not decomposable
                | otherwise
                -> encodeC mode v
                    ( yesStarterNoDecomp : acc
                    , succ expected
                    , expected )
            -- No or Maybe
            else let c = chr cp in case quickCheck of
                QuickCheckYes   -> error
                    ("Unexpected Maybe value for: " <> show cp)
                QuickCheckNo    -> ( no : acc
                                   , succ expected
                                   , cp )
                QuickCheckMaybe -> if
                    | UC.isCombining c
                    -> ( maybeCombining : acc
                       , succ expected
                       , cp )
                    | UC.isCombiningStarter c
                    -> ( maybeCombiningStarter : acc
                       , succ expected
                       , cp )
                    | UC.isDecomposable mode c
                    -> ( maybeStarterDecomp : acc
                       , succ expected
                       , cp )
                    | otherwise
                    -> ( maybeStarterNoDecomp : acc
                       , succ expected
                       , cp )

    decomposable = 0
    combining = 1
    yesStarter = 3
    no = decomposable
    maybeCombining = combining
    maybeCombiningStarter = maybeStarterNoDecomp
    maybeStarterDecomp = decomposable
    maybeStarterNoDecomp = 2
    yesCombining = combining
    yesStarterDecomp = yesStarter
    yesStarterNoDecomp = yesStarter
    yesStable = yesStarter
    defaultValue = yesStable

    combiningStarters = filter UC.isCombiningStarter [minBound..maxBound]
    combiningChars = filter UC.isCombining [minBound..maxBound]
    isStable mode c = notChangedWithAddedChar mode c && notComposable c
    notChangedWithAddedChar mode c
        = not (UC.isCombining c)
        && not (UC.isJamo c && UC.ord c <= UC.jamoLLast)
        && not (UC.isHangul c && UC.isHangulLV c)
        && (  not (UC.isDecomposable mode c)
           || all (not . UC.isCombining) (UC.decompose mode c) )
    notComposable c
        = all (isNothing . UC.composeStarters c) combiningStarters
        && all (isNothing . UC.compose c) combiningChars

    -- Note: No maybe
    genBitmapD :: String -> [Int] -> String
    genBitmapD funcName bits =
        unlines
            [ "{-# INLINE " <> funcName <> " #-}"
            , funcName <> " :: Char -> Bool"
            , funcName <> " = \\c -> let n = ord c in not ("
                <> genRangeCheck "n" bits
                <> " && lookupBit64 bitmap# n)"
            , "  where"
            , "    bitmap# = \"" <> bitMapToAddrLiteral (positionsToBitMap bits) "\"#\n"
            ]

    genBitmapC :: String -> ([Word8], Int) -> String
    genBitmapC funcName (ws, maxCP) =
        unlines
            [ "{-# INLINE " <> funcName <> " #-}"
            , funcName <> " :: Char -> Int"
            , funcName <> " = \\c -> let n = ord c in if n < "
                <> show (length (takeWhile (== defaultValue) ws))
                <> " || n > "
                <> show maxCP
            , "  then " <> show defaultValue
            , "  else lookupIntN bitmap# n"
            , "  where"
            , "    -- Encoding:"
            , "    -- • " <> show decomposable         <> ": Decomposable"
            , "    -- • " <> show combining            <> ": Combining"
            , "    -- • " <> show maybeStarterNoDecomp <> ": Maybe, starter, no decomposition"
            , "    -- • " <> show yesStarter           <> ": Yes, starter"
            , "    bitmap# = \"" <> enumMapToAddrLiteral ws "\"#\n"
            ]

-------------------------------------------------------------------------------
-- Parsing property files
-------------------------------------------------------------------------------

trim :: String -> String
trim = takeWhile (not . isSpace) . dropWhile isSpace

type NormalizationPropertyLine = (String, [(Int, QuickCheck)])

parseNormalizationPropertyLine :: String -> Maybe NormalizationPropertyLine
parseNormalizationPropertyLine ln
    | null ln = Nothing
    | head ln == '#' = Nothing
    | otherwise = parseLineJ ln

    where

    isProp :: String -> Bool
    isProp = \case
        "NFD_QC"  -> True
        "NFKD_QC" -> True
        "NFC_QC"  -> True
        "NFKC_QC" -> True
        _         -> False

    parseLineJ :: String -> Maybe NormalizationPropertyLine
    parseLineJ line =
        let (rangeLn, line1) = first trim (span (/= ';') line)
            (propLn, line2) = first trim (span (/= ';') (tail line1))
            value = trim (takeWhile (/= '#') (tail line2))
        in if isProp propLn
            then Just (propLn, (,parseValue value) <$> parseRange rangeLn)
            else Nothing

    parseValue :: String -> QuickCheck
    parseValue = \case
        "N" -> QuickCheckNo
        "M" -> QuickCheckMaybe
        "Y" -> QuickCheckYes
        v   -> error ("Cannot parse QuickCheck value: " <> v)

    parseRange :: String -> [Int]
    parseRange rng =
        if '.' `elem` rng
        then let low = read $ "0x" ++ takeWhile (/= '.') rng
                 high =
                     read $ "0x" ++ reverse (takeWhile (/= '.') (reverse rng))
              in [low .. high]
        else [read $ "0x" ++ rng]

parseNormalizationPropsLines
    :: (IsStream t, Monad m)
    => t m String
    -> t m NormalizationPropertyLine
parseNormalizationPropsLines
    = Stream.groupsBy isSameProp (Fold.foldl' combine ("", []))
    . Stream.mapMaybe parseNormalizationPropertyLine

    where

    isSameProp (n1, _) (n2, _) = n1 == n2

    combine
        :: NormalizationPropertyLine
        -> NormalizationPropertyLine
        -> NormalizationPropertyLine
    combine l1@(n1, v1) l2@(n2, v2)
        | n1 == "" = l2
        | n2 == "" = l1
        | otherwise = (n1, v1 <> v2)

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

readLinesFromFile :: String -> SerialT IO String
readLinesFromFile file =
    withFile file Sys.ReadMode
        $ \h ->
              Stream.unfold Handle.read h & Unicode.decodeUtf8
                  & unicodeLines Fold.toList

    where

    unicodeLines = Stream.splitOnSuffix (== '\n')

    withFile file_ mode =
        Stream.bracket (liftIO $ Sys.openFile file_ mode) (liftIO . Sys.hClose)


moduleToFileName :: String -> String
moduleToFileName = map (\x -> if x == '.' then '/' else x)

dirFromFileName :: String -> String
dirFromFileName = reverse . dropWhile (/= '/') . reverse

-- ModuleRecipe is a tuple of the module name and a function that generates the
-- module using the module name
type ModuleRecipe a = (String, String -> Fold IO a String)

-- GeneratorRecipe is a list of ModuleRecipe
type GeneratorRecipe a = [ModuleRecipe a]

fileEmitter :: String -> String -> ModuleRecipe a -> Fold IO a ()
fileEmitter file outdir (modName, fldGen) = Fold.rmapM action $ fldGen modName

    where

    pretext version = mconcat
        [ "-- autogenerated from https://www.unicode.org/Public/"
        , version
        , "/ucd/"
        , file
        ,"\n"
        ]
    outfile = outdir <> moduleToFileName modName <> ".hs"
    outfiledir = dirFromFileName outfile
    action c = do
        version <-
            catch
                (getEnv "UNICODE_VERSION")
                (\(_ :: IOException) -> return "<unknown>")
        createDirectoryIfMissing True outfiledir
        writeFile outfile (pretext version ++ c)

runGenerator ::
       String
    -> String
    -> (SerialT IO String -> SerialT IO a)
    -> String
    -> GeneratorRecipe a
    -> IO ()
runGenerator indir file transformLines outdir recipes =
    readLinesFromFile (indir <> file) & transformLines & Stream.fold combinedFld

    where

    generatedFolds = map (fileEmitter file outdir) recipes
    combinedFld = void $ Fold.distribute generatedFolds

genModules :: String -> String -> IO ()
genModules indir outdir = do

    runGenerator
        indir
        "DerivedNormalizationProps.txt"
        parseNormalizationPropsLines
        outdir
        [ derivedNormalizationProperties ]

    where

    derivedNormalizationProperties =
        ("Data.Unicode.Internal.Char.DerivedNormalizationProperties"
        , genNormalizationPropertiesModule)
