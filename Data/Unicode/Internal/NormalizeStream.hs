{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module      : Data.Unicode.Internal.NormalizeStream
-- Copyright   : (c) 2016 Harendra Kumar
--               (c) 2020 Andrew Lelechenko
--
-- License     : BSD-3-Clause
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
--
-- Stream based normalization.
--
module Data.Unicode.Internal.NormalizeStream
    (
      UC.DecomposeMode(..)
    , stream
    , unstream
    , unstreamC
    )
    where

import Data.Bits (shiftR)
import Data.Char (chr, ord)
import GHC.ST (ST(..))
import GHC.Types (SPEC(..))

import qualified Data.Text.Array as A
import qualified Unicode.Char as UC

-- Internal modules
import Data.Text.Internal (Text(..))
import Data.Text.Internal.Fusion.Size (betweenSize, upperBound)
import Data.Text.Internal.Fusion.Types (Step(..), Stream(..))
import Data.Text.Internal.Private (runText)
import Data.Text.Internal.Unsafe.Char (unsafeChr, unsafeWrite)
import Data.Text.Internal.Encoding.Utf16 (chr2)

-------------------------------------------------------------------------------
-- Reorder buffer to hold characters till the next starter boundary
-------------------------------------------------------------------------------

-- | A list of combining characters, ordered by 'UC.combiningClass'.
-- Couple of top levels are unrolled and unpacked for efficiency.
data ReBuf = Empty | One !Char | Many !Char !Char ![Char]

{-# INLINE insertIntoReBuf #-}
insertIntoReBuf :: Char -> ReBuf -> ReBuf
insertIntoReBuf c Empty = One c
insertIntoReBuf c (One c0)
    | UC.combiningClass c < UC.combiningClass c0
    = Many c c0 []
    | otherwise
    = Many c0 c []
insertIntoReBuf c (Many c0 c1 cs)
    | cc < UC.combiningClass c0
    = Many c c0 (c1 : cs)
    | cc < UC.combiningClass c1
    = Many c0 c (c1 : cs)
    | otherwise
    = Many c0 c1 (cs' ++ (c : cs''))
    where
        cc = UC.combiningClass c
        (cs', cs'') = span ((<= cc) . UC.combiningClass) cs

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
    if t == chr UC.jamoTFirst then do
        n1 <- unsafeWrite marr j l
        n2 <- unsafeWrite marr (j + n1) v
        return (j + n1 + n2)
    else do
        n1 <- unsafeWrite marr j l
        n2 <- unsafeWrite marr (j + n1) v
        n3 <- unsafeWrite marr (j + n1 + n2) t
        return (j + n1 + n2 + n3)
    where
        (l, v, t) = UC.decomposeHangul c

{-# INLINE decomposeChar #-}
decomposeChar
    :: UC.DecomposeMode
    -> A.MArray s       -- destination array for decomposition
    -> Int              -- array index
    -> ReBuf            -- reorder buffer
    -> Char             -- char to be decomposed
    -> ST s (Int, ReBuf)
decomposeChar mode marr index reBuf ch
    | UC.isHangul ch = do
        j <- writeReorderBuffer marr index reBuf
        (, Empty) <$> decomposeCharHangul marr j ch
    | UC.isDecomposable mode ch =
        decomposeAll marr index reBuf (UC.decompose mode ch)
    | otherwise =
        reorder marr index reBuf ch

    where

    {-# INLINE decomposeAll #-}
    decomposeAll _ i rbuf [] = return (i, rbuf)
    decomposeAll arr i rbuf (x : xs)
        | UC.isDecomposable mode x = do
            (i', rbuf') <- decomposeAll arr i rbuf (UC.decompose mode x)
            decomposeAll arr i' rbuf' xs
        | otherwise  = do
            (i', rbuf') <- reorder arr i rbuf x
            decomposeAll arr i' rbuf' xs

    {-# INLINE reorder #-}
    reorder arr i rbuf c
        | UC.isCombining c = return (i, insertIntoReBuf c rbuf)
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
          | (n `shiftR` 10) == 0x36    = Yield (chr2 n n2) (i + 2)
          | otherwise                  = Yield (unsafeChr n) (i + 1)
          where
            n  = A.unsafeIndex arr i
            n2 = A.unsafeIndex arr (i + 1)
{-# INLINE [0] stream #-}

-- | /O(n)/ Convert a 'Stream Char' into a decompose-normalized 'Text'.
unstream :: UC.DecomposeMode -> Stream Char -> Text
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

-- If we are composing we do not need to first decompose Hangul. We can just
-- compose assuming there could be some partially composed syllables e.g. LV
-- syllable followed by a jamo T. We need to compose this case as well.

-- Hold an L to wait for V, hold an LV to wait for T.
data JamoBuf
    = Jamo !Char -- Jamo L, V or T
    | Hangul !Char -- Hangul Syllable LV or LVT
    | HangulLV !Char

data RegBuf
    = RegOne !Char
    | RegMany !Char !Char ![Char]

data ComposeState
    = ComposeNone
    | ComposeReg !RegBuf
    | ComposeJamo !JamoBuf

-------------------------------------------------------------------------------
-- Composition of Jamo into Hangul syllables, done algorithmically
-------------------------------------------------------------------------------

{-# INLINE writeJamoBuf #-}
writeJamoBuf :: A.MArray s -> Int -> JamoBuf -> ST s Int
writeJamoBuf arr i jbuf = do
    n <- unsafeWrite arr i (getCh jbuf)
    return (i + n)

    where

    getCh (Jamo ch) = ch
    getCh (Hangul ch) = ch
    getCh (HangulLV ch) = ch

{-# INLINE initHangul #-}
initHangul :: Char -> Int -> ST s (Int, ComposeState)
initHangul c i = return (i, ComposeJamo (Hangul c))

{-# INLINE initJamo #-}
initJamo :: Char -> Int -> ST s (Int, ComposeState)
initJamo c i = return (i, ComposeJamo (Jamo c))

{-# INLINE insertJamo #-}
insertJamo
    :: A.MArray s -> Int -> JamoBuf -> Char -> ST s (Int, ComposeState)
insertJamo arr i jbuf ch
    | ich <= UC.jamoLLast = do
        j <- writeJamoBuf arr i jbuf
        return (j, ComposeJamo (Jamo ch))
    | ich < UC.jamoVFirst =
        flushAndWrite arr i jbuf ch
    | ich <= UC.jamoVLast = do
        case jbuf of
            Jamo c ->
                case UC.jamoLIndex c of
                    Just li ->
                        let vi = ich - UC.jamoVFirst
                            lvi = li * UC.jamoNCount + vi * UC.jamoTCount
                            lv = chr (UC.hangulFirst + lvi)
                         in return (i, ComposeJamo (HangulLV lv))
                    Nothing -> writeTwo arr i c ch
            Hangul c -> writeTwo arr i c ch
            HangulLV c -> writeTwo arr i c ch
    | ich <= UC.jamoTFirst = do
        flushAndWrite arr i jbuf ch
    | otherwise = do
        let ti = ich - UC.jamoTFirst
        case jbuf of
            Jamo c -> writeTwo arr i c ch
            Hangul c
                | UC.isHangulLV c -> do
                    writeLVT arr i c ti
                | otherwise ->
                    writeTwo arr i c ch
            HangulLV c ->
                writeLVT arr i c ti

    where

    ich = ord ch

    {-# INLINE flushAndWrite #-}
    flushAndWrite marr ix jb c = do
        j <- writeJamoBuf marr ix jb
        n <- unsafeWrite marr j c
        return (j + n, ComposeNone)

    {-# INLINE writeLVT #-}
    writeLVT marr ix lv ti = do
        n <- unsafeWrite marr ix (chr ((ord lv) + ti))
        return (ix + n, ComposeNone)

    {-# INLINE writeTwo #-}
    writeTwo marr ix c1 c2 = do
        n <- unsafeWrite marr ix c1
        m <- unsafeWrite marr (ix + n) c2
        return ((ix + n + m), ComposeNone)

{-# INLINE insertHangul #-}
insertHangul
    :: A.MArray s -> Int -> JamoBuf -> Char -> ST s (Int, ComposeState)
insertHangul arr i jbuf ch = do
    j <- writeJamoBuf arr i jbuf
    return (j, ComposeJamo (Hangul ch))

{-# INLINE insertIntoRegBuf #-}
insertIntoRegBuf :: Char -> RegBuf -> RegBuf
insertIntoRegBuf c (RegOne c0)
    | UC.combiningClass c < UC.combiningClass c0
    = RegMany c c0 []
    | otherwise
    = RegMany c0 c []
insertIntoRegBuf c (RegMany c0 c1 cs)
    | cc < UC.combiningClass c0
    = RegMany c c0 (c1 : cs)
    | cc < UC.combiningClass c1
    = RegMany c0 c (c1 : cs)
    | otherwise
    = RegMany c0 c1 (cs' ++ (c : cs''))
    where
        cc = UC.combiningClass c
        (cs', cs'') = span ((<= cc) . UC.combiningClass) cs

{-# INLINE writeRegBuf #-}
writeRegBuf :: A.MArray s -> Int -> RegBuf -> ST s Int
writeRegBuf arr i = \case
    RegOne c -> do
        n <- unsafeWrite arr i c
        return (i + n)
    RegMany st c [] ->
        case UC.compose st c of
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
    go uncs st (c : cs) = case UC.compose st c of
        Nothing -> go (uncs ++ (c : same)) st bigger
        Just x  -> go uncs x cs
        where
            cc = UC.combiningClass c
            (same, bigger) = span ((== cc) . UC.combiningClass) cs

{-# INLINE flushComposeState #-}
flushComposeState :: A.MArray s -> Int -> ComposeState -> ST s Int
flushComposeState arr i = \case
    ComposeNone -> pure i
    ComposeReg rbuf -> writeRegBuf arr i rbuf
    ComposeJamo jbuf -> writeJamoBuf arr i jbuf

{-# INLINE composeChar #-}
composeChar
    :: UC.DecomposeMode
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
                | ich < UC.jamoLFirst ->
                    composeReg rbuf ch i st
                | ich <= UC.jamoTLast -> do
                    j <- writeRegBuf marr i rbuf
                    initJamo ch j
                | ich < UC.hangulFirst ->
                    composeReg rbuf ch i st
                | ich <= UC.hangulLast -> do
                    j <- writeRegBuf marr i rbuf
                    initHangul ch j
                | otherwise ->
                    composeReg rbuf ch i st
            ComposeJamo jbuf
                | ich < UC.jamoLFirst -> do
                    jamoToReg marr i jbuf ch
                | ich <= UC.jamoTLast -> do
                    insertJamo marr i jbuf ch
                | ich < UC.hangulFirst ->
                    jamoToReg marr i jbuf ch
                | ich <= UC.hangulLast -> do
                    insertHangul marr i jbuf ch
                | otherwise ->
                    jamoToReg marr i jbuf ch
            ComposeNone
                | ich < UC.jamoLFirst ->
                    initReg ch i
                | ich <= UC.jamoTLast ->
                    initJamo ch i
                | ich < UC.hangulFirst ->
                    initReg ch i
                | ich <= UC.hangulLast ->
                    initHangul ch i
                | otherwise ->
                    initReg ch i
        where ich = ord ch

    {-# INLINE jamoToReg #-}
    jamoToReg arr i jbuf ch = do
        j <- writeJamoBuf arr i jbuf
        initReg ch j

    {-# INLINE initReg #-}
    initReg !ch !i
        | UC.isDecomposable mode ch =
            go (UC.decompose mode ch) i ComposeNone
        | otherwise =
            pure (i, ComposeReg (RegOne ch))

    {-# INLINE composeReg #-}
    composeReg rbuf !ch !i !st
        | UC.isDecomposable mode ch =
            go (UC.decompose mode ch) i st
        | UC.isCombining ch = do
            pure (i, ComposeReg (insertIntoRegBuf ch rbuf))
        -- The first char in RegBuf may or may not be a starter. In
        -- case it is not we rely on composeStarters failing.
        | RegOne s <- rbuf
        , UC.isCombiningStarter ch
        , Just x <- UC.composeStarters s ch =
            pure (i, (ComposeReg (RegOne x)))
        | otherwise = do
            j <- writeRegBuf marr i rbuf
            pure (j, ComposeReg (RegOne ch))

    go [] !i !st = pure (i, st)
    go (ch : rest) i st =
        case st of
            ComposeReg rbuf
                | UC.isHangul ch -> do
                    j <- writeRegBuf marr i rbuf
                    (k, s) <- initHangul ch j
                    go rest k s
                | UC.isJamo ch -> do
                    j <- writeRegBuf marr i rbuf
                    (k, s) <- initJamo ch j
                    go rest k s
                | UC.isDecomposable mode ch ->
                    go (UC.decompose mode ch ++ rest) i st
                | UC.isCombining ch -> do
                    go rest i (ComposeReg (insertIntoRegBuf ch rbuf))
                | RegOne s <- rbuf
                , UC.isCombiningStarter ch
                , Just x <- UC.composeStarters s ch ->
                    go rest i (ComposeReg (RegOne x))
                | otherwise -> do
                    j <- writeRegBuf marr i rbuf
                    go rest j (ComposeReg (RegOne ch))
            ComposeJamo jbuf
                | UC.isJamo ch -> do
                    (j, s) <- insertJamo marr i jbuf ch
                    go rest j s
                | UC.isHangul ch -> do
                    (j, s) <- insertHangul marr i jbuf ch
                    go rest j s
                | otherwise -> do
                    j <- writeJamoBuf marr i jbuf
                    case () of
                        _
                            | UC.isDecomposable mode ch ->
                                go (UC.decompose mode ch ++ rest) j
                                   ComposeNone
                            | otherwise ->
                                go rest j (ComposeReg (RegOne ch))
            ComposeNone
                | UC.isHangul ch -> do
                    (j, s) <- initHangul ch i
                    go rest j s
                | UC.isJamo ch -> do
                    (j, s) <- initJamo ch i
                    go rest j s
                | UC.isDecomposable mode ch ->
                    go (UC.decompose mode ch ++ rest) i st
                | otherwise ->
                    go rest i (ComposeReg (RegOne ch))

-- | /O(n)/ Convert a 'Stream Char' into a composed normalized 'Text'.
unstreamC :: UC.DecomposeMode -> Stream Char -> Text
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
