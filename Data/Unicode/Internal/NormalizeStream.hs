{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module      : Data.Unicode.Internal.NormalizeStream
-- Copyright   : (c) 2016 Harendra Kumar
--               (c) 2020 Andrew Lelechenko
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
--
-- Stream based normalization.
--
module Data.Unicode.Internal.NormalizeStream
    (
      D.DecomposeMode(..)
    , stream
    , unstream
    , unstreamC
    )
    where

import           Data.Char                              (chr, ord)
import qualified Data.Text.Array                        as A
import           Data.Text.Internal                     (Text (..))
import qualified Data.Text.Internal.Encoding.Utf16      as U16
import           Data.Text.Internal.Fusion.Size         (betweenSize,
                                                         upperBound)
import           Data.Text.Internal.Fusion.Types        (Step (..), Stream (..))
import           Data.Text.Internal.Private             (runText)
import           Data.Text.Internal.Unsafe.Char         (unsafeWrite)
import           Data.Text.Internal.Unsafe.Char         (unsafeChr)
import           Data.Text.Internal.Unsafe.Shift        (shiftR)
import           GHC.ST                                 (ST (..))
import           GHC.Types                              (SPEC(..))

import qualified Data.Unicode.Properties.CombiningClass  as CC
import qualified Data.Unicode.Properties.Compositions    as C
import qualified Data.Unicode.Properties.Decompose       as D
import qualified Data.Unicode.Properties.DecomposeHangul as H

-------------------------------------------------------------------------------
-- Reorder buffer to hold characters till the next starter boundary
-------------------------------------------------------------------------------

-- | A list of combining characters, ordered by 'CC.getCombiningClass'.
-- Couple of top levels are unrolled and unpacked for efficiency.
data ReBuf = Empty | One !Char | Many !Char !Char ![Char]

{-# INLINE insertIntoReBuf #-}
insertIntoReBuf :: Char -> ReBuf -> ReBuf
insertIntoReBuf c Empty = One c
insertIntoReBuf c (One c0)
    | CC.getCombiningClass c < CC.getCombiningClass c0
    = Many c c0 []
    | otherwise
    = Many c0 c []
insertIntoReBuf c (Many c0 c1 cs)
    | cc < CC.getCombiningClass c0
    = Many c c0 (c1 : cs)
    | cc < CC.getCombiningClass c1
    = Many c0 c (c1 : cs)
    | otherwise
    = Many c0 c1 (cs' ++ (c : cs''))
    where
        cc = CC.getCombiningClass c
        (cs', cs'') = span ((<= cc) . CC.getCombiningClass) cs

writeStr :: A.MArray s -> Int -> [Char] -> ST s Int
writeStr marr di str = go di str
    where
        go i [] = return i
        go i (c : cs) = do
            n <- unsafeWrite marr i c
            go (i + n) cs

{-# INLINE writeReorderBuffer #-}
writeReorderBuffer :: A.MArray s -> Int -> ReBuf -> ST s Int
writeReorderBuffer _ di Empty = return di

writeReorderBuffer marr di (One c) = do
    n <- unsafeWrite marr di c
    return (di + n)

writeReorderBuffer marr di (Many c1 c2 str) = do
    n1 <- unsafeWrite marr di c1
    n2 <- unsafeWrite marr (di + n1) c2
    writeStr marr (di + n1 + n2) str

-------------------------------------------------------------------------------
-- Decomposition of Hangul characters is done algorithmically
-------------------------------------------------------------------------------

-- {-# INLINE decomposeCharHangul #-}
decomposeCharHangul :: A.MArray s -> Int -> Char -> ST s Int
decomposeCharHangul marr j c =
    if t == chr H.jamoTFirst then do
        n1 <- unsafeWrite marr j l
        n2 <- unsafeWrite marr (j + n1) v
        return (j + n1 + n2)
    else do
        n1 <- unsafeWrite marr j l
        n2 <- unsafeWrite marr (j + n1) v
        n3 <- unsafeWrite marr (j + n1 + n2) t
        return (j + n1 + n2 + n3)
    where
        (l, v, t) = D.decomposeCharHangul c

{-# INLINE decomposeChar #-}
decomposeChar
    :: D.DecomposeMode
    -> A.MArray s       -- destination array for decomposition
    -> Int              -- array index
    -> ReBuf            -- reorder buffer
    -> Char             -- char to be decomposed
    -> ST s (Int, ReBuf)
decomposeChar mode marr index reBuf ch
    | D.isHangul ch = do
        j <- writeReorderBuffer marr index reBuf
        (, Empty) <$> decomposeCharHangul marr j ch
    | D.isDecomposable mode ch =
        decomposeAll marr index reBuf (D.decomposeChar mode ch)
    | otherwise =
        reorder marr index reBuf ch

    where

    {-# INLINE decomposeAll #-}
    decomposeAll _ i rbuf [] = return (i, rbuf)
    decomposeAll arr i rbuf (x : xs)
        | D.isDecomposable mode x = do
            (i', rbuf') <- decomposeAll arr i rbuf (D.decomposeChar mode x)
            decomposeAll arr i' rbuf' xs
        | otherwise  = do
            (i', rbuf') <- reorder arr i rbuf x
            decomposeAll arr i' rbuf' xs

    {-# INLINE reorder #-}
    reorder arr i rbuf c
        | CC.isCombining c = return (i, insertIntoReBuf c rbuf)
        | otherwise = do
            j <- writeReorderBuffer arr i rbuf
            n <- unsafeWrite arr j c
            return (j + n, Empty)

-- | /O(n)/ Convert a 'Text' into a 'Stream Char'.
stream :: Text -> Stream Char
stream (Text arr off len) = Stream next off (betweenSize (len `shiftR` 1) len)
    where
      !end = off+len
      {-# INLINE next #-}
      next !i
          | i >= end                   = Done
          -- shift generates only two branches instead of three in case of
          -- range check, works quite a bit faster with llvm backend.
          | (n `shiftR` 10) == 0x36    = Yield (U16.chr2 n n2) (i + 2)
          | otherwise                  = Yield (unsafeChr n) (i + 1)
          where
            n  = A.unsafeIndex arr i
            n2 = A.unsafeIndex arr (i + 1)
{-# INLINE [0] stream #-}

-- | /O(n)/ Convert a 'Stream Char' into a decompose-normalized 'Text'.
unstream :: D.DecomposeMode -> Stream Char -> Text
unstream mode (Stream next0 s0 len) = runText $ \done -> do
  -- Before encoding each char we perform a buffer realloc check assuming
  -- worst case encoding size of two 16-bit units for the char. Just add an
  -- extra space to the buffer so that we do not end up reallocating even when
  -- all the chars are encoded as single unit.
  let margin = 1 + maxDecomposeLen
      mlen = (upperBound 4 len + margin)
  arr0 <- A.new mlen
  let outer !arr !maxi = encode
       where
        -- keep the common case loop as small as possible
        encode !si !di rbuf =
            -- simply check for the worst case
            if maxi < di + margin
            then realloc si di rbuf
            else
                case next0 si of
                    Done -> do
                        di' <- writeReorderBuffer arr di rbuf
                        done arr di'
                    Skip si'    -> encode si' di rbuf
                    Yield c si' -> do
                                (di', rbuf') <- decomposeChar mode arr di rbuf c
                                encode si' di' rbuf'
                                -- n <- unsafeWrite arr di c
                                -- encode si' (di + n) rbuf

        -- keep uncommon case separate from the common case code
        {-# NOINLINE realloc #-}
        realloc !si !di rbuf = do
            let newlen = maxi * 2
            arr' <- A.new newlen
            A.copyM arr' 0 arr 0 di
            outer arr' (newlen - 1) si di rbuf

  outer arr0 (mlen - 1) s0 0 Empty
{-# INLINE [0] unstream #-}

-- we can generate this from UCD
maxDecomposeLen :: Int
maxDecomposeLen = 32

-------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Composition of Hangul Jamo characters, done algorithmically
-------------------------------------------------------------------------------

-- If we are composing we do not need to first decompose Hangul. We can just
-- compose assuming there could be some partially composed syllables e.g. LV
-- syllable followed by a jamo T. We need to compose this case as well.
--
-- XXX The unicode normalization test suite does not seem to have tests for a
-- LV composed hangul syllable followed by a jamo T.

-- Hold an L to wait for V, hold an LV to wait for T.
data JamoBuf
    = JamoLIndex {-# UNPACK #-} !Int
    | JamoLV     {-# UNPACK #-} !Char

data RegBuf
    = RegOne !Char
    | RegMany !Char !Char [Char]

data ComposeState
    = ComposeNone
    | ComposeReg !RegBuf
    | ComposeJamo !JamoBuf

{-# INLINE writeJamoLIndex #-}
writeJamoLIndex :: A.MArray s -> Int -> Int -> ST s Int
writeJamoLIndex arr i li = do
    n <- unsafeWrite arr i (chr (D.jamoLFirst + li))
    return (i + n)

{-# INLINE writeJamoLV #-}
writeJamoLV :: A.MArray s -> Int -> Char -> ST s Int
writeJamoLV arr i lv = do
    n <- unsafeWrite arr i lv
    return (i + n)

{-# INLINE writeJamoBuf #-}
writeJamoBuf :: A.MArray s -> Int -> JamoBuf -> ST s Int
writeJamoBuf arr i = \case
    JamoLIndex li -> writeJamoLIndex arr i li
    JamoLV lv -> writeJamoLV arr i lv

{-# INLINE initHangulLV #-}
initHangulLV :: A.MArray s -> Int -> Char -> ST s (Int, ComposeState)
initHangulLV arr i c
    | H.isHangulLV c = return (i, ComposeJamo (JamoLV c))
    | otherwise = do
        n <- unsafeWrite arr i c
        return (i + n, ComposeNone)

{-# INLINE initJamoLIndex #-}
initJamoLIndex :: A.MArray s -> Int -> Char -> ST s (Int, ComposeState)
initJamoLIndex arr i c =
    case H.jamoLIndex c of
        Just li -> return (i, ComposeJamo (JamoLIndex li))
        Nothing -> do
            n <- unsafeWrite arr i c
            return (i + n, ComposeNone)

{-# INLINE composeCharJamo #-}
composeCharJamo
    :: A.MArray s -> Int -> JamoBuf -> Char -> ST s (Int, ComposeState)
composeCharJamo arr i (JamoLIndex li) c =
    case H.jamoVIndex c of
        Just vi -> do
            let lvi = li * H.jamoNCount + vi * H.jamoTCount
            return (i, ComposeJamo (JamoLV (chr (H.hangulFirst + lvi))))
        Nothing -> do
            j <- writeJamoLIndex arr i li
            initJamoLIndex arr j c
composeCharJamo arr i (JamoLV lv) c =
    case H.jamoTIndex c of
        Just ti -> do
            n <- unsafeWrite arr i (chr ((ord lv) + ti))
            return (i + n, ComposeNone)
        Nothing -> do
            j <- writeJamoLV arr i lv
            initJamoLIndex arr j c

{-# INLINE insertIntoRegBuf #-}
insertIntoRegBuf :: Char -> RegBuf -> RegBuf
insertIntoRegBuf c (RegOne c0)
    | CC.getCombiningClass c < CC.getCombiningClass c0
    = RegMany c c0 []
    | otherwise
    = RegMany c0 c []
insertIntoRegBuf c (RegMany c0 c1 cs)
    | cc < CC.getCombiningClass c0
    = RegMany c c0 (c1 : cs)
    | cc < CC.getCombiningClass c1
    = RegMany c0 c (c1 : cs)
    | otherwise
    = RegMany c0 c1 (cs' ++ (c : cs''))
    where
        cc = CC.getCombiningClass c
        (cs', cs'') = span ((<= cc) . CC.getCombiningClass) cs

{-# INLINE writeRegBuf #-}
writeRegBuf :: A.MArray s -> Int -> RegBuf -> ST s Int
writeRegBuf arr i = \case
    RegOne c -> do
        n <- unsafeWrite arr i c
        return (i + n)
    RegMany st c [] ->
        case C.composePair st c of
            Just x -> do
                n <- unsafeWrite arr i x
                return (i + n)
            Nothing -> do
                n <- unsafeWrite arr i st
                m <- unsafeWrite arr (i + n) c
                return (i + n + m)
    RegMany st0 c0 cs0 -> go [] st0 (c0 : cs0)

    where

    -- arguments: uncombined chars, starter, unprocessed str
    go uncs st [] = writeStr arr i (st : uncs)
    go uncs st (c : cs) = case C.composePair st c of
        Nothing -> go (uncs ++ (c : same)) st bigger
        Just x  -> go uncs x cs
        where
            cc = CC.getCombiningClass c
            (same, bigger) = span ((== cc) . CC.getCombiningClass) cs

{-# INLINE flushComposeState #-}
flushComposeState :: A.MArray s -> Int -> ComposeState -> ST s Int
flushComposeState arr i = \case
    ComposeNone -> pure i
    ComposeReg rbuf -> writeRegBuf arr i rbuf
    ComposeJamo jbuf -> writeJamoBuf arr i jbuf

{-# INLINE composeChar #-}
composeChar
    :: D.DecomposeMode
    -> A.MArray s       -- destination array for composition
    -> Char             -- input char
    -> Int              -- array index
    -> ComposeState
    -> ST s (Int, ComposeState)
composeChar mode marr = go0

    where

    go0 ch !i !st =
        case st of
            ComposeReg rbuf
                | H.isHangul ch -> do
                    j <- writeRegBuf marr i rbuf
                    initHangulLV marr j ch
                | H.isJamo ch -> do
                    j <- writeRegBuf marr i rbuf
                    initJamoLIndex marr j ch
                | D.isDecomposable mode ch ->
                    go (D.decomposeChar mode ch) i st
                | CC.isCombining ch -> do
                    pure (i, ComposeReg (insertIntoRegBuf ch rbuf))
                | RegOne s <- rbuf
                , C.isSecondStarter ch
                , Just x <- C.composeStarterPair s ch ->
                    pure (i, (ComposeReg (RegOne x)))
                | otherwise -> do
                    j <- writeRegBuf marr i rbuf
                    pure (j, ComposeReg (RegOne ch))
            ComposeJamo jbuf
                | H.isJamo ch ->
                    composeCharJamo marr i jbuf ch
                | otherwise -> do
                    j <- writeJamoBuf marr i jbuf
                    case () of
                        _
                            | H.isHangul ch -> do
                                initHangulLV marr j ch
                            | D.isDecomposable mode ch ->
                                go (D.decomposeChar mode ch) j ComposeNone
                            | otherwise -> do
                                pure (j, ComposeReg (RegOne ch))
            ComposeNone
                | H.isHangul ch ->
                    initHangulLV marr i ch
                | H.isJamo ch ->
                   initJamoLIndex marr i ch
                | D.isDecomposable mode ch ->
                    go (D.decomposeChar mode ch) i st
                | otherwise ->
                    pure (i, ComposeReg (RegOne ch))

    go [] !i !st = pure (i, st)
    go (ch : rest) i st =
        case st of
            ComposeReg rbuf
                | H.isHangul ch -> do
                    j <- writeRegBuf marr i rbuf
                    (k, s) <- initHangulLV marr j ch
                    go rest k s
                | H.isJamo ch -> do
                    j <- writeRegBuf marr i rbuf
                    (k, s) <- initJamoLIndex marr j ch
                    go rest k s
                | D.isDecomposable mode ch ->
                    go (D.decomposeChar mode ch ++ rest) i st
                | CC.isCombining ch -> do
                    go rest i (ComposeReg (insertIntoRegBuf ch rbuf))
                | RegOne s <- rbuf
                , C.isSecondStarter ch
                , Just x <- C.composeStarterPair s ch ->
                    go rest i (ComposeReg (RegOne x))
                | otherwise -> do
                    j <- writeRegBuf marr i rbuf
                    go rest j (ComposeReg (RegOne ch))
            ComposeJamo jbuf
                | H.isJamo ch -> do
                    (j, s) <- composeCharJamo marr i jbuf ch
                    go rest j s
                | otherwise -> do
                    j <- writeJamoBuf marr i jbuf
                    case () of
                        _
                            | H.isHangul ch -> do
                                (k, s) <- initHangulLV marr j ch
                                go rest k s
                            | D.isDecomposable mode ch ->
                                go (D.decomposeChar mode ch ++ rest) j
                                   ComposeNone
                            | otherwise ->
                                go rest j (ComposeReg (RegOne ch))
            ComposeNone
                | H.isHangul ch -> do
                    (j, s) <- initHangulLV marr i ch
                    go rest j s
                | H.isJamo ch -> do
                    (j, s) <- initJamoLIndex marr i ch
                    go rest j s
                | D.isDecomposable mode ch ->
                    go (D.decomposeChar mode ch ++ rest) i st
                | otherwise ->
                    go rest i (ComposeReg (RegOne ch))

-- | /O(n)/ Convert a 'Stream Char' into a composed normalized 'Text'.
unstreamC :: D.DecomposeMode -> Stream Char -> Text
unstreamC mode (Stream next0 s0 len) = runText $ \done -> do
  -- Before encoding each char we perform a buffer realloc check assuming
  -- worst case encoding size of two 16-bit units for the char. Just add an
  -- extra space to the buffer so that we do not end up reallocating even when
  -- all the chars are encoded as single unit.
  let margin = 1 + maxDecomposeLen
      mlen = (upperBound 4 len + margin)
  arr0 <- A.new mlen
  let outer !arr !maxi = encode SPEC
       where
        -- keep the common case loop as small as possible
        encode !_ !si !di st =
            -- simply check for the worst case
            if maxi < di + margin
               then realloc si di st
            else
                case next0 si of
                    Done -> do
                        di' <- flushComposeState arr di st
                        done arr di'
                    Skip si'    -> encode SPEC si' di st
                    Yield c si' -> do
                        (di', st') <- composeChar mode arr c di st
                        encode SPEC si' di' st'

        -- keep uncommon case separate from the common case code
        {-# NOINLINE realloc #-}
        realloc !si !di st = do
            let newlen = maxi * 2
            arr' <- A.new newlen
            A.copyM arr' 0 arr 0 di
            outer arr' (newlen - 1) si di st

  outer arr0 (mlen - 1) s0 0 ComposeNone
{-# INLINE [0] unstreamC #-}
