{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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
    , unstreamD
    , unstreamC
    )
    where

import Data.Char (chr, ord)
import GHC.ST (ST(..))
import GHC.Types (SPEC(..))

import qualified Data.Text.Array as A
import qualified Unicode.Char as UC

#if MIN_VERSION_text(2,0,0)
import Data.Text.Internal.Fusion (stream)
#else
import Data.Bits (shiftR)
import Data.Text.Internal.Unsafe.Char (unsafeChr)
import Data.Text.Internal.Fusion.Size (betweenSize)
import Data.Text.Internal.Encoding.Utf16 (chr2)
#endif

-- Internal modules
import Data.Text.Internal (Text(..))
import Data.Text.Internal.Fusion.Size (upperBound)
import Data.Text.Internal.Fusion.Types (Step(..), Stream(..))
import Data.Text.Internal.Private (runText)
import Data.Text.Internal.Unsafe.Char (unsafeWrite)
import qualified Data.Unicode.Internal.Char.DerivedNormalizationProperties as QC

-------------------------------------------------------------------------------
-- Stream text
-------------------------------------------------------------------------------

#if !MIN_VERSION_text(2,0,0)
-- | /O(n)/ Convert a 'Text' into a 'Stream Char'.
stream :: Text -> Stream Char
stream (Text arr off len) = Stream next off (betweenSize (len `shiftR` 1) len)
    where
      !end = off+len
      {-# INLINE next #-}
      next !i
          | i >= end                   = Done
          -- Shift generates only two branches instead of three in case of
          -- range check, works quite a bit faster with llvm backend.
          | (n `shiftR` 10) == 0x36    = Yield (chr2 n n2) (i + 2)
          | otherwise                  = Yield (unsafeChr n) (i + 1)
          where
            n  = A.unsafeIndex arr i
            n2 = A.unsafeIndex arr (i + 1)
{-# INLINE [0] stream #-}
#endif

-------------------------------------------------------------------------------
-- Normalization Forms NFD & NFKD
-------------------------------------------------------------------------------

-- | Buffer to hold sorted combining characters till the next starter boundary.
-- The characters are ordered by their 'UC.combiningClass'.
-- Couple of top levels are unrolled and unpacked for efficiency.
data ReBuf
    = Empty
    | One !Char
    | Many !Char !Char ![Char]

-- | Insert a combining character in its canonical position in a buffer.
{-# INLINE insertIntoReBuf #-}
insertIntoReBuf :: Char -> ReBuf -> ReBuf
insertIntoReBuf c = \case
    Empty -> One c
    One c0
        | UC.combiningClass c < UC.combiningClass c0
        -> Many c c0 []
        | otherwise
        -> Many c0 c []
    Many c0 c1 cs
        | cc < UC.combiningClass c0
        -> Many c c0 (c1 : cs)
        | cc < UC.combiningClass c1
        -> Many c0 c (c1 : cs)
        | otherwise
        -> Many c0 c1 (cs' ++ (c : cs''))
        where
            cc = UC.combiningClass c
            (cs', cs'') = span ((<= cc) . UC.combiningClass) cs

-- | Write a string in an array.
writeStr :: A.MArray s -> Int -> [Char] -> ST s Int
writeStr marr = go
    where
        go i = \case
            []     -> pure i
            c : cs -> do
                n <- unsafeWrite marr i c
                go (i + n) cs

-- | Write combining character buffer in an array.
{-# INLINE writeReorderBuffer #-}
writeReorderBuffer :: A.MArray s -> Int -> ReBuf -> ST s Int
writeReorderBuffer marr i = \case
    Empty -> pure i
    One c -> do
        n <- unsafeWrite marr i c
        pure (i + n)
    Many c1 c2 str -> do
        n1 <- unsafeWrite marr i c1
        n2 <- unsafeWrite marr (i + n1) c2
        writeStr marr (i + n1 + n2) str

-- | Decompose and write Hangul character.
-- Decomposition of Hangul characters is done algorithmically.
-- {-# INLINE decomposeCharHangul #-}
decomposeCharHangul :: A.MArray s -> Int -> Char -> ST s Int
decomposeCharHangul marr j c =
    if t == chr UC.jamoTFirst then do
        n1 <- unsafeWrite marr j l
        n2 <- unsafeWrite marr (j + n1) v
        pure (j + n1 + n2)
    else do
        n1 <- unsafeWrite marr j l
        n2 <- unsafeWrite marr (j + n1) v
        n3 <- unsafeWrite marr (j + n1 + n2) t
        pure (j + n1 + n2 + n3)
    where
        (l, v, t) = UC.decomposeHangul c

-- | Decompose a character and write the result in a target array.
{-# INLINE decomposeChar #-}
decomposeChar
    :: UC.DecomposeMode -- ^ Decomposition mode: Canonical or Kompat
    -> A.MArray s       -- ^ Destination array for decomposition
    -> Int              -- ^ Array index
    -> ReBuf            -- ^ Reorder buffer
    -> Char             -- ^ Char to be decomposed
    -> ST s (Int, ReBuf)
decomposeChar mode marr index reBuf ch
    -- Quick check = Yes
    | quickCheck =
        reorder marr index reBuf ch
    -- Hangul
    | UC.isHangul ch = do
        j <- writeReorderBuffer marr index reBuf
        (, Empty) <$> decomposeCharHangul marr j ch
    -- Other, decomposable
    | otherwise =
        decomposeAll marr index reBuf (UC.decompose mode ch)

    where

    quickCheck = case mode of
        UC.Canonical -> QC.isNFD_QC  ch
        UC.Kompat    -> QC.isNFKD_QC ch

    -- Decompose a character recursively and write the result in the array
    {-# INLINE decomposeAll #-}
    decomposeAll arr i rbuf = \case
        [] -> pure (i, rbuf)
        x : xs
            | UC.isDecomposable mode x -> do
                (i', rbuf') <- decomposeAll arr i rbuf (UC.decompose mode x)
                decomposeAll arr i' rbuf' xs
            | otherwise -> do
                (i', rbuf') <- reorder arr i rbuf x
                decomposeAll arr i' rbuf' xs

    -- Given a normalized char, buffer it if combining else write it
    {-# INLINE reorder #-}
    reorder arr i rbuf c
        | UC.isCombining c = pure (i, insertIntoReBuf c rbuf)
        | otherwise = do
            j <- writeReorderBuffer arr i rbuf
            n <- unsafeWrite arr j c
            pure (j + n, Empty)

-- | /O(n)/ Convert a 'Stream Char' into a /decomposed/-normalized 'Text'.
unstreamD :: UC.DecomposeMode -> Stream Char -> Text
unstreamD mode (Stream next0 s0 len) = runText $ \done -> do
    -- Before encoding each char we perform a buffer realloc check assuming
    -- worst case encoding size of two 16-bit units for the char. Just add an
    -- extra space to the buffer so that we do not end up reallocating even when
    -- all the chars are encoded as single unit.
    let margin = 1 + maxDecomposeLen
        mlen = upperBound 4 len + margin
    arr0 <- A.new mlen
    let outer !arr !maxi = encode SPEC
            where
                -- keep the common case loop as small as possible
                encode !_ !si !di rbuf =
                    -- simply check for the worst case
                    if maxi < di + margin
                    then realloc si di rbuf
                    else
                        case next0 si of
                            Done -> do
                                di' <- writeReorderBuffer arr di rbuf
                                done arr di'
                            Skip si'    -> encode SPEC si' di rbuf
                            Yield c si' -> do
                                (di', rbuf') <- decomposeChar mode arr di rbuf c
                                encode SPEC si' di' rbuf'
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
{-# INLINE [0] unstreamD #-}

-- [TODO] we can generate this from UCD
-- [TODO] is https://www.unicode.org/reports/tr15/#Stream_Safe_Text_Format the source for this?
maxDecomposeLen :: Int
maxDecomposeLen = 32

-------------------------------------------------------------------------------
-- Composition: normalization Forms NFC & NFKC
-------------------------------------------------------------------------------

-- | Generic non-empty composition buffer.
-- The characters are ordered by their 'UC.combiningClass'.
data RegBuf
    = RegOne !Char
    | RegMany !Char !Char ![Char]

-- | Composition state
data ComposeState
    = ComposeNone
    -- ^ Empty buffer
    | ComposeReg !RegBuf
    -- ^ Pending composable string
    | ComposeStarter !Char
    -- ^ Pending starter, QC = Yes

---
--- Composition of Jamo into Hangul syllables, done algorithmically
---

-- | Initialize buffer with conjoining jamo
{-# INLINE initJamo #-}
initJamo :: A.MArray s -> Char -> Int -> ST s (Int, ComposeState)
initJamo marr c i = if ord c <= UC.jamoLLast
    -- Jamo L: may compose with next (jamo V)
    then pure (i, ComposeStarter c)
    -- Other jamo: does not compose with next; write it immediately.
    else do
        n <- unsafeWrite marr i c
        pure (i + n, ComposeNone)

-- | Compose jamo L + jamo V
{-# INLINE composeJamoL #-}
composeJamoL :: Char -> Char -> ComposeState
composeJamoL l v =
    let !li = ord l - UC.jamoLFirst
        !vi = ord v - UC.jamoVFirst
        !lvi = li * UC.jamoNCount + vi * UC.jamoTCount
        !lv = chr (UC.hangulFirst + lvi)
    in ComposeStarter lv

-- | Initialize buffer with a precomposed Hangul character
{-# INLINE initHangul #-}
initHangul :: A.MArray s -> Char -> Int -> ST s (Int, ComposeState)
initHangul marr c i = if UC.isHangulLV c
    -- Hangul LV: may compose with next (jamo T)
    then pure (i, ComposeStarter c)
    -- Hangul LVT: does not compose; write it immediately.
    else do
        n <- unsafeWrite marr i c
        pure (i + n, ComposeNone)

-- | Compose Hangul syllable LV + jamo T
{-# INLINE composeHangulLV #-}
composeHangulLV :: A.MArray s -> Char -> Char -> Int -> ST s (Int, ComposeState)
composeHangulLV marr lv t i = do
    let !ti = ord t - UC.jamoTFirst
    n <- unsafeWrite marr i (chr (ord lv + ti))
    pure (i + n, ComposeNone)

--
-- Generic buffer handling
--

-- | Insert a combining character in its canonical position in a buffer.
{-# INLINE insertIntoRegBuf #-}
insertIntoRegBuf :: Char -> RegBuf -> RegBuf
insertIntoRegBuf c = \case
    RegOne c0
        | UC.combiningClass c < UC.combiningClass c0
        -> {-# SCC insertIntoRegBuf_one_before #-} RegMany c c0 []
        | otherwise
        -> {-# SCC insertIntoRegBuf_one_after #-} RegMany c0 c []
    RegMany c0 c1 cs
        | cc < UC.combiningClass c0
        -> {-# SCC insertIntoRegBuf_many_first #-} RegMany c c0 (c1 : cs)
        | cc < UC.combiningClass c1
        -> {-# SCC insertIntoRegBuf_many_second #-} RegMany c0 c (c1 : cs)
        | otherwise
        ->
            {-# SCC insertIntoRegBuf_many_other #-}
            RegMany c0 c1 (cs' ++ (c : cs''))
        where
            cc = UC.combiningClass c
            (cs', cs'') = span ((<= cc) . UC.combiningClass) cs

-- | Write generic compose buffer to a target array
{-# INLINE writeRegBuf #-}
writeRegBuf :: A.MArray s -> Int -> RegBuf -> ST s Int
writeRegBuf marr i = \case
    RegOne c -> {-# SCC writeRegBuf_one #-} do
        n <- unsafeWrite marr i c
        pure (i + n)
    -- Composition
    RegMany st c0 [] ->
        {-# SCC writeRegBuf_many2 #-}
        case UC.compose st c0 of
            Just c -> {-# SCC writeRegBuf_many2_composable #-} do
                n <- unsafeWrite marr i c
                pure (i + n)
            Nothing -> {-# SCC writeRegBuf_many2_notcomposable #-} do
                n1 <- unsafeWrite marr i st
                n2 <- unsafeWrite marr (i + n1) c0
                pure (i + n1 + n2)
    -- Recursive composition
    RegMany st c0 cs0 ->
        {-# SCC writeRegBuf_manyN #-}
        go [] st (c0 : cs0)

    where

    -- arguments: uncombined chars, starter, unprocessed string
    go uncs st = \case
        c : cs -> case UC.compose st c of
            Nothing -> go (uncs ++ (c : same)) st bigger
            Just x  -> go uncs x cs
            where
                cc = UC.combiningClass c
                (same, bigger) = span ((== cc) . UC.combiningClass) cs
        _ -> writeStr marr i (st : uncs)

--
-- Composition
--

-- | Compose characters if possible and write the result in a target array.
{-# INLINE composeChar #-}
composeChar
    :: UC.DecomposeMode
    -> A.MArray s       -- ^ Destination array for composition
    -> Char             -- ^ Input char
    -> Int              -- ^ Array index
    -> ComposeState
    -> ST s (Int, ComposeState)
composeChar mode marr = go0

    where
    quickCheck = case mode of
        UC.Canonical -> QC.isNFC_QC
        UC.Kompat    -> QC.isNFKC_QC

    -- ch: input char
    -- i: array index
    -- st: compose state

    -- Start normalization with initial compose state
    go0 ch !i !st =
        case st of
            -- Pending starter, QC = Yes
            ComposeStarter s -> {-# SCC compose_YesStarter #-} case quickCheck ch of
                -- QC = Yes, starter (includes Jamo L & Hangul syllables),
                -- may decompose, may compose with next
                QC.YesStarter -> {-# SCC compose_YesStarter_YesStarter #-} do
                    n <- unsafeWrite marr i s
                    pure (i + n, ComposeStarter ch)
                -- QC = Yes or Maybe, combining, not decomposable
                QC.Combining
                    -- Pending decomposition
                    | UC.isDecomposable mode s ->
                        {-# SCC compose_YesStarter_Combining_decomp #-}
                        go (UC.decompose mode s ++ [ch]) i ComposeNone
                    -- Starter + combining
                    | otherwise ->
                        {-# SCC compose_YesStarter_Combining_nodecomp #-}
                        pure (i, ComposeReg (RegMany s ch []))
                -- QC = No or Maybe, decomposable
                QC.Decomposable
                    -- Pending decomposition
                    | UC.isDecomposable mode s ->
                        {-# SCC compose_YesStarter_Decomposable_decomp #-}
                        go (UC.decompose mode s ++ UC.decompose mode ch) i ComposeNone
                    -- Starter + decomposable
                    | otherwise ->
                        {-# SCC compose_YesStarter_Decomposable_nodecomp #-}
                        go (UC.decompose mode ch) i st
                -- QC = Maybe, starter, not decomposable
                _
                    -- Pending decomposition
                    | UC.isDecomposable mode s ->
                        {-# SCC compose_YesStarter_other_decomp_starter #-}
                        go (UC.decompose mode s ++ [ch]) i ComposeNone
                    -- Jamo V or T
                    | UC.isJamo ch -> {-# SCC compose_YesStarter_other_jamo #-} if
                        -- Jamo L + jamo V
                        | UC.jamoLFirst <= cp && cp <= UC.jamoLLast &&
                            UC.jamoVFirst <= ich && ich <= UC.jamoVLast ->
                            pure (i, composeJamoL s ch)
                        -- Hangul LV + T
                        | UC.isHangul s && UC.isHangulLV s &&
                            UC.jamoTFirst < ich && ich <= UC.jamoTLast ->
                            composeHangulLV marr s ch i
                        -- Cannot compose: flush buffer
                        | otherwise -> do
                            n1 <- unsafeWrite marr i s
                            n2 <- unsafeWrite marr (i + n1) ch
                            pure (i + n1 + n2, ComposeNone)
                    -- Combining starter
                    | UC.isCombiningStarter ch
                    , Just x <- UC.composeStarters s ch ->
                        {-# SCC compose_YesStarter_other_composable #-}
                        pure (i, ComposeReg (RegOne x))
                    -- Two non-composable starters
                    | otherwise -> {-# SCC compose_YesStarter_other_other #-}do
                        n <- unsafeWrite marr i s
                        pure (i + n, ComposeReg (RegOne ch))
                    where cp = ord s
            -- Pending composable string
            ComposeReg rbuf -> {-# SCC compose_Reg #-} case quickCheck ch of
                -- QC = Yes, starter (includes Jamo L & Hangul syllables),
                -- may decompose, may compose with next
                QC.YesStarter -> {-# SCC compose_reg_YesStarter #-} do
                    j <- writeRegBuf marr i rbuf
                    pure (j, ComposeStarter ch)
                -- QC = Yes or Maybe, combining, not decomposable
                QC.Combining ->
                    {-# SCC compose_reg_Combining #-}
                    pure (i, ComposeReg (insertIntoRegBuf ch rbuf))
                -- QC = No or Maybe, decomposable
                QC.Decomposable ->
                    {-# SCC compose_reg_Decomposable #-}
                    go (UC.decompose mode ch) i st
                -- QC = Maybe, starter, not decomposable
                _
                    -- Combining starter
                    -- The first char in RegBuf may or may not be a starter. In
                    -- case it is not we rely on composeStarters failing.
                    | RegOne s <- rbuf
                    , UC.isCombiningStarter ch
                    , Just x <- UC.composeStarters s ch ->
                        {-# SCC compose_reg_composable #-}
                        pure (i, ComposeReg (RegOne x))
                    -- Jamo V or T
                    | UC.isJamo ch -> {-# SCC compose_reg_jamo #-} do
                        j <- writeRegBuf marr i rbuf
                        n <- unsafeWrite marr j ch
                        pure (j + n, ComposeNone)
                    -- Cannot compose: flush buffer
                    | otherwise -> {-# SCC compose_reg_other #-} do
                        j <- writeRegBuf marr i rbuf
                        pure (j, ComposeReg (RegOne ch))
            -- Empty buffer
            ComposeNone -> {-# SCC compose_None #-} case quickCheck ch of
                -- QC = Yes, starter (includes Jamo L & Hangul syllables),
                -- may decompose, may compose with next
                QC.YesStarter ->
                    {-# SCC compose_none_YesStarter #-}
                    pure (i, ComposeStarter ch)
                -- QC = No or Maybe, decomposable
                QC.Decomposable ->
                    {-# SCC compose_none_Decomposable #-}
                    go (UC.decompose mode ch) i st
                -- QC = Yes (combining) or Maybe (any), not decomposable
                _
                    -- Jamo V or T
                    | UC.isJamo ch -> {-# SCC compose_none_other_jamo #-} do
                        n <- unsafeWrite marr i ch
                        pure (i + n, ComposeNone)
                    -- Starter or combining
                    | otherwise ->
                        {-# SCC compose_none_other_other #-}
                        pure (i, ComposeReg (RegOne ch))

        where ich = ord ch

    -- Recursive decomposition
    go [] !i !st = pure (i, st)
    go (ch : rest) i st =
        {-# SCC goComposeChar #-}
        case st of
            -- QC = Yes, starter.
            -- [NOTE] Here only: not decomposable or Hangul LV
            ComposeStarter s
                -- Hangul syllable: flush buffer
                | UC.isHangul ch -> {-# SCC go_composeReg_hangul #-} do
                    n <- unsafeWrite marr i s
                    initHangul marr ch (i + n) >>= uncurry (go rest)
                -- Jamo: compose or flush
                | UC.isJamo ch -> if
                    -- Any + jamo L: flush buffer
                    | UC.jamoLFirst <= ich && ich <= UC.jamoLLast -> do
                        n <- unsafeWrite marr i s
                        go rest (i + n) (ComposeStarter ch)
                    -- Any + jamo V
                    | UC.jamoVFirst <= ich && ich <= UC.jamoVLast ->
                        if UC.jamoLFirst <= is && is <= UC.jamoLLast
                            -- Jamo L + jamo V
                            then go rest i (composeJamoL s ch)
                            -- Flush buffer & current
                            else do
                                n1 <- unsafeWrite marr i s
                                n2 <- unsafeWrite marr (i + n1) ch
                                go rest (i + n1 + n2) ComposeNone
                    -- Hangul LV + jamo T: compose Hangul LVT
                    | UC.jamoTFirst < ich && ich <= UC.jamoTLast &&
                        UC.isHangul s && UC.isHangulLV s ->
                            composeHangulLV marr s ch i >>= uncurry (go rest)
                    -- Other: flush buffer & current
                    | otherwise -> do
                        n1 <- unsafeWrite marr i s
                        n2 <- unsafeWrite marr (i + n1) ch
                        go rest (i + n1 + n2) ComposeNone
                -- Decomposable: continue recursive decomposition
                | UC.isDecomposable mode ch ->
                    {-# SCC go_composeReg_decomposable #-}
                    go (UC.decompose mode ch ++ rest) i st
                -- Starter + combining
                | UC.isCombining ch ->
                    {-# SCC go_composeReg_combining #-}
                    go rest i (ComposeReg (RegMany s ch []))
                -- Combining starter
                | UC.isCombiningStarter ch
                , Just x <- UC.composeStarters s ch ->
                    {-# SCC go_composeReg_combining_starter #-}
                    go rest i (ComposeReg (RegOne x))
                -- Two non-composable starters: flush buffer
                | otherwise -> {-# SCC go_composeReg_other #-} do
                    n <- unsafeWrite marr i s
                    go rest (i + n) (ComposeReg (RegOne ch))
                where is = ord s
                      ich = ord ch
            ComposeReg rbuf
                -- Hangul syllable: flush buffer
                | UC.isHangul ch -> {-# SCC go_composeReg_hangul #-}
                    writeRegBuf marr i rbuf >>=
                    initHangul marr ch >>=
                    uncurry (go rest)
                -- Jamo: flush buffer
                | UC.isJamo ch -> {-# SCC go_composeReg_jamo #-}
                    writeRegBuf marr i rbuf >>=
                    initJamo marr ch >>=
                    uncurry (go rest)
                -- Decomposable: continue recursive decomposition
                | UC.isDecomposable mode ch ->
                    {-# SCC go_composeReg_decomposable #-}
                    go (UC.decompose mode ch ++ rest) i st
                -- Add combining to buffer
                | UC.isCombining ch ->
                    {-# SCC go_composeReg_combining #-}
                    go rest i (ComposeReg (insertIntoRegBuf ch rbuf))
                -- Combining starter
                -- The first char in RegBuf may or may not be a starter. In
                -- case it is not we rely on composeStarters failing.
                | RegOne s <- rbuf
                , UC.isCombiningStarter ch
                , Just x <- UC.composeStarters s ch ->
                    {-# SCC go_composeReg_combining_starter #-}
                    go rest i (ComposeReg (RegOne x))
                -- Starter: flush buffer
                | otherwise -> {-# SCC go_composeReg_other #-} do
                    j <- writeRegBuf marr i rbuf
                    go rest j (ComposeReg (RegOne ch))
            ComposeNone
                -- Hangul syllable
                | UC.isHangul ch ->
                    {-# SCC go_composeNone_hangul #-}
                    initHangul marr ch i >>= uncurry (go rest)
                -- Jamo
                | UC.isJamo ch ->
                    {-# SCC go_composeNone_jamo #-}
                    initJamo marr ch i >>= uncurry (go rest)
                -- Decomposable: continue recursive decomposition
                | UC.isDecomposable mode ch ->
                    {-# SCC go_composeNone_decomposable #-}
                    go (UC.decompose mode ch ++ rest) i st
                -- Start buffer
                | otherwise ->
                    {-# SCC go_composeNone_other #-}
                    go rest i (ComposeReg (RegOne ch))

-- | Finalize composition
{-# INLINE flushComposeState #-}
flushComposeState :: A.MArray s -> Int -> ComposeState -> ST s Int
flushComposeState marr i = \case
    ComposeNone      -> pure i
    ComposeStarter s -> (i+) <$> unsafeWrite marr i s
    ComposeReg rbuf  -> writeRegBuf marr i rbuf

-- | /O(n)/ Convert a 'Stream Char' into a /composed/-normalized 'Text'.
unstreamC :: UC.DecomposeMode -> Stream Char -> Text
unstreamC mode (Stream next0 s0 len) = runText $ \done -> do
    -- Before encoding each char we perform a buffer realloc check assuming
    -- worst case encoding size of two 16-bit units for the char. Just add an
    -- extra space to the buffer so that we do not end up reallocating even when
    -- all the chars are encoded as single unit.
    let margin = 1 + maxDecomposeLen
        mlen = upperBound 4 len + margin
    arr0 <- A.new mlen
    let outer !arr !maxi = encode SPEC
            where
            -- si: internal state
            -- di: current char index
            -- st: compose state

            -- Keep the common case loop as small as possible
            encode !_ !si !di st =
                -- simply check for the worst case
                if maxi < di + margin
                then {-# SCC reallocC #-} realloc si di st
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
