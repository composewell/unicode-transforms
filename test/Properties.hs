{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Char (chr, ord)
import Data.Bits (xor)
import Data.Foldable (traverse_)
import Data.Ix (Ix(..))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Normalize (NormalizationMode)
import QuickCheckUtils ()
import Test.Hspec.QuickCheck (modifyMaxSuccess, modifyMaxSize, prop)
import Test.Hspec as H
import Test.QuickCheck (NonNegative(..))
import Unicode.Internal.Division (quotRem21, quotRem28)
import Unicode.Char.Normalization (compose, composeStarters, isCombiningStarter)

import qualified Data.Text.Normalize as T
import qualified Data.Unicode.Internal.Char.DerivedNormalizationProperties as QC
import qualified Unicode.Char as UC

#ifdef HAS_ICU
import Data.Text (pack)

#if MIN_VERSION_text_icu(0,8,0)
import qualified Data.Text.ICU.Normalize2 as ICU
#else
import qualified Data.Text.ICU.Normalize as ICU
#endif

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
main = hspec $ H.parallel $ do
    modifyMaxSuccess (const 10000) $ describe "Divisions" $ do
        prop "quotRem28" $ \(NonNegative n) -> n `quotRem` 28 == quotRem28 n
        prop "quotRem28 maxBound" $ \(NonNegative n) ->
            let n1 = maxBound - n
            in n1 `quotRem` 28 == quotRem28 n1
        prop "quotRem21" $ \(NonNegative n) -> n `quotRem` 21 == quotRem21 n
        prop "quotRem21 maxBound" $ \(NonNegative n) ->
            let n1 = maxBound - n
            in n1 `quotRem` 21 == quotRem21 n1
#ifdef HAS_ICU
    modifyMaxSuccess (max 100000) $ modifyMaxSize (max 500) $
        describe "Compare with ICU" $ do
            it "Compare \127340 with ICU" $
                t_normalizeCompareICU T.NFKD (pack "\127340") `H.shouldBe` True
            prop "Comparing random strings with ICU..." t_normalizeCompareICU
#else
    modifyMaxSuccess (const 10000) $
        prop "Checking non-empty results for random strings..." t_normalize
#endif
    describe "Expected Hangul properties for composition" $ do
        let checkQC f n c = c `shouldSatisfy` ((== n) . f . chr)
        let checkNFC  = checkQC QC.isNFC_QC
        let checkNFKC = checkQC QC.isNFKC_QC
        let checkNoDecomp mode = (`shouldNotSatisfy` UC.isDecomposable mode . chr)
        let checkJamo n c = checkNFC  n c
                         >> checkNFKC n c
                         >> checkNoDecomp UC.Canonical c
                         >> checkNoDecomp UC.Kompat    c
                         >> (c `shouldNotSatisfy` UC.isCombining . chr)
        describe "Jamo L" $ do
            let cs = [UC.jamoLFirst .. UC.jamoLLast]
            it "QC == Yes, no decomposition, starter" $
                traverse_ (checkJamo QC.YesStarter) cs
            it "Compose only with Jamo V" $
                let {
                    check (l, c) =
                        if inRange (UC.jamoVFirst, UC.jamoVLast) (ord c)
                            then pure ()
                            else if UC.isCombining c
                                then l `shouldNotSatisfy` isJust . (`compose` c)
                                else l `shouldNotSatisfy`
                                     (\c' -> isJust (composeStarters c' c)
                                          || isCombiningStarter c')
                } in traverse_ check
                    [ (l, c)
                    | l <- chr <$> cs
                    , c <- [minBound..maxBound]
                    ]
        describe "jamoLLast < cp < jamoVFirst" $ do
            let cs = [succ UC.jamoLLast .. pred UC.jamoVFirst]
            it "QC = Yes, no decomposition, starter" $
                traverse_ (checkJamo QC.YesStarter) cs
            it "Does not compose" $
                let {
                    check (c, j) = if UC.isCombining c
                        then j `shouldNotSatisfy` isJust . (compose c)
                        else j `shouldNotSatisfy`
                             (\c' -> isJust (composeStarters c' c)
                                  || isCombiningStarter c')
                } in traverse_ check
                    [ (c, j)
                    | j <- chr <$> cs
                    , c <- [minBound..maxBound]
                    ]
        describe "Jamo V" $ do
            let cs = [UC.jamoVFirst .. UC.jamoVLast]
            it "QC = Maybe, no decomposition, starter" $
                traverse_ (checkJamo QC.MaybeStarterNoDecomp) cs
            it "Compose only with Jamo L" $
                let {
                    check (c, v) =
                        if inRange (UC.jamoLFirst, UC.jamoLLast) (ord c)
                            then pure ()
                            else if UC.isCombining c
                                then v `shouldNotSatisfy` isJust . (compose c)
                                else v `shouldNotSatisfy`
                                     (\c' -> isJust (composeStarters c' c)
                                          || isCombiningStarter c')
                } in traverse_ check
                    [ (c, v)
                    | v <- chr <$> cs
                    , c <- [minBound..maxBound]
                    ]
        describe "jamoVLast < cp <= jamoTFirst" $ do
            let cs = [succ UC.jamoVLast .. UC.jamoTFirst]
            it "QC = Yes, no decomposition, starter" $
                traverse_ (checkJamo QC.YesStarter) cs
            it "Does not compose" $
                let {
                    check (c, j) = if UC.isCombining c
                        then j `shouldNotSatisfy` isJust . (compose c)
                        else j `shouldNotSatisfy`
                             (\c' -> isJust (composeStarters c' c)
                                  || isCombiningStarter c')
                } in traverse_ check
                    [ (c, j)
                    | j <- chr <$> cs
                    , c <- [minBound..maxBound]
                    ]
        describe "Jamo T" $ do
            let cs = [succ UC.jamoTFirst .. UC.jamoTLast]
            it "QC = Maybe, no decomposition, starter" $
                traverse_ (checkJamo QC.MaybeStarterNoDecomp) cs
            it "Compose only with LV" $ do
                let {
                    check (c, t) =
                        if UC.isHangul c && UC.isHangulLV c
                            then pure ()
                            else if UC.isCombining c
                                then t `shouldNotSatisfy` isJust . (compose c)
                                else t `shouldNotSatisfy`
                                     (\c' -> isJust (composeStarters c' c)
                                          || isCombiningStarter c')
                } in traverse_ check
                    [ (c, t)
                    | t <- chr <$> [succ UC.jamoTFirst .. UC.jamoTLast]
                    , c <- [minBound..maxBound]
                    ]
        describe "Precomposed Hangul" $ do
            it "QC = Yes, no decomposition via ‘decompose’, starter" $ do
                let checkHangul = checkJamo QC.YesStarter
                traverse_ checkHangul [UC.hangulFirst .. UC.hangulLast]
    describe "Expected properties for QC" $ do
        it "isCombiningStarter => not decomposable" $ do
            let mkCheck mode c = if UC.isCombiningStarter c
                                then c `shouldNotSatisfy` UC.isDecomposable mode
                                else pure ()
            let check c = mkCheck UC.Canonical c >> mkCheck UC.Kompat c
            traverse_ check [minBound..maxBound]
        it "NFD & NFKD" $ do
            let {
                mkCheck f mode c = if f c
                    then do
                        c `shouldNotSatisfy` UC.isDecomposable mode
                        c `shouldNotSatisfy` UC.isHangul
                    else c `shouldSatisfy`
                        (\c' -> UC.isDecomposable mode c' `xor` UC.isHangul c')
            }
            let check c =  mkCheck QC.isNFD_QC  UC.Canonical c
                        >> mkCheck QC.isNFKD_QC UC.Kompat    c
            traverse_ check [minBound..maxBound]
        it "NFC & NFKC" $ do
            let {
                mkCheck f mode c = case f c of
                    -- Yes, starter
                    QC.YesStarter -> c `shouldNotSatisfy` UC.isCombining
                    -- Maybe, starter, no decomposition
                    QC.MaybeStarterNoDecomp -> do
                        c `shouldNotSatisfy` UC.isCombining
                        c `shouldNotSatisfy` UC.isDecomposable mode
                    -- Combining
                    QC.Combining -> do
                        c `shouldSatisfy` UC.isCombining
                        c `shouldNotSatisfy` UC.isDecomposable mode
                    -- Decomposable
                    QC.Decomposable -> c `shouldSatisfy` UC.isDecomposable mode
                    qc -> error (show qc)
            }
            let check c =  mkCheck QC.isNFC_QC  UC.Canonical c
                        >> mkCheck QC.isNFKC_QC UC.Kompat    c
            traverse_ check [minBound..maxBound]
