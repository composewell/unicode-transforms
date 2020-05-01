{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
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
decomposeChar _ marr i reBuf c | D.isHangul c = do
    j <- writeReorderBuffer marr i reBuf
    (, Empty) <$> decomposeCharHangul marr j c

-------------------------------------------------------------------------------
-- Decomposition of characters other than Hangul
-------------------------------------------------------------------------------

decomposeChar mode marr index reBuf ch = do
    -- TODO: return fully decomposed form
    case D.isDecomposable mode ch of
      False -> reorder marr index reBuf ch
      True  -> decomposeAll marr index reBuf (D.decomposeChar mode ch)
    where
        {-# INLINE decomposeAll #-}
        decomposeAll _ i rbuf [] = return (i, rbuf)
        decomposeAll arr i rbuf (x : xs)  =
            case D.isDecomposable mode x of
                True  -> do
                    (i', rbuf') <- decomposeAll arr i rbuf
                                                (D.decomposeChar mode x)
                    decomposeAll arr i' rbuf' xs
                False -> do
                    -- XXX calling reorder is wrong if decomposition results in
                    -- a further decomposable Hangul char. In that case we will
                    -- not go through the Hangul decompose for that char.
                    -- To be strictly correct we have to call decomposeChar
                    -- recursively here.
                    (i', rbuf') <- reorder arr i rbuf x
                    decomposeAll arr i' rbuf' xs

        -- Unicode 9.0.0: 3.11
        -- D108 Reorderable pair: Two adjacent characters A and B in a coded
        -- character sequence <A,B> are a Reorderable Pair if and only if
        -- ccc(A) > ccc(B) > 0.
        --
        -- (array) (array index) (reorder buffer) (input char)
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

composeAndWrite
    :: A.MArray s
    -> Int
    -> Char
    -> ReBuf
    -> ST s Int -- return new index
composeAndWrite arr di st Empty = do
    n <- unsafeWrite arr di st
    return (di + n)

composeAndWrite arr di st (One c) =
    case C.composePair st c of
        Just x -> do
            n <- unsafeWrite arr di x
            return (di + n)
        Nothing -> do
            n <- unsafeWrite arr di st
            m <- unsafeWrite arr (di + n) c
            return (di + n + m)

composeAndWrite arr di st (Many c1 c2 str) =
    composeAndWrite' arr di st (c1 : c2 : str)

composeAndWrite'
    :: A.MArray s
    -> Int
    -> Char
    -> [Char]
    -> ST s Int
composeAndWrite' arr di = go []
    where
        -- arguments: uncombined chars, starter, unprocessed str
        go uncs st [] = writeStr arr di (st : uncs)
        go uncs st (c : cs) = case C.composePair st c of
            Nothing -> go (uncs ++ (c : same)) st bigger
            Just x  -> go uncs x cs
            where
                cc = CC.getCombiningClass c
                (same, bigger) = span ((== cc) . CC.getCombiningClass) cs

writeStarterRbuf :: A.MArray s
                 -> Int
                 -> Maybe Char
                 -> ReBuf
                 -> ST s Int
writeStarterRbuf marr di st rbuf =
    case st of
        Nothing -> writeReorderBuffer marr di rbuf
        Just starter -> composeAndWrite marr di starter rbuf

-------------------------------------------------------------------------------
-- Composition of Hangul Jamo characters, done algorithmically
-------------------------------------------------------------------------------

-- Hold an L to wait for V, hold an LV to wait for T.
data JamoBuf
    = JamoEmpty
    | JamoLIndex {-# UNPACK #-} !Int
    | JamoLV     {-# UNPACK #-} !Char

{-# INLINE writeJamoBuf #-}
writeJamoBuf :: A.MArray s -> Int -> JamoBuf -> ST s Int
writeJamoBuf _ di JamoEmpty = return di

writeJamoBuf marr di (JamoLIndex i) = do
    n <- unsafeWrite marr di (chr (D.jamoLFirst + i))
    return (di + n)

writeJamoBuf marr di (JamoLV c) = do
    n <- unsafeWrite marr di c
    return (di + n)

composeCharJamo :: A.MArray s -> Int -> JamoBuf -> Char -> ST s (Int, JamoBuf)
composeCharJamo arr i JamoEmpty c =
    case H.jamoLIndex c of
        Just li -> return (i, JamoLIndex li)
        Nothing -> do
            n <- unsafeWrite arr i c
            return (i + n, JamoEmpty)
composeCharJamo arr i jb@(JamoLIndex li) c =
    case H.jamoVIndex c of
        Just vi -> do
            let lvi = li * H.jamoNCount + vi * H.jamoTCount
            return (i, JamoLV (chr (H.hangulFirst + lvi)))
        Nothing -> do
            ix <- writeJamoBuf arr i jb
            composeCharJamo arr ix JamoEmpty c
composeCharJamo arr i jb@(JamoLV lv) c =
    case H.jamoTIndex c of
        Just ti -> do
            n <- unsafeWrite arr i (chr ((ord lv) + ti))
            return (i + n, JamoEmpty)
        Nothing -> do
            ix <- writeJamoBuf arr i jb
            composeCharJamo arr ix JamoEmpty c

composeCharHangul :: A.MArray s -> Int -> JamoBuf -> Char -> ST s (Int, JamoBuf)
composeCharHangul arr i jb c = do
    ix <- writeJamoBuf arr i jb
    case H.isHangulLV c of
        True -> return (ix, JamoLV c)
        False -> do
            n <- unsafeWrite arr ix c
            return (ix + n, JamoEmpty)

-- TODO Unify compose and decompose if possible with good perf
-- TODO try unifying st, rbuf
-- TODO try using Either for (st, rbuf)/jbuf
-- or we can use different functions for hangul and non-hangul composition with
-- diff signatures. In an outer function we check if the char is hangul and
-- flush and switch the buffer before calling the appropriate function.

-- If we are composing we do not need to first decompose Hangul. We can just
-- compose assuming there could be some partially composed syllables e.g. LV
-- syllable followed by a jamo T. We need to compose this case as well.
--
-- XXX The unicode normalization test suite does not seem to have tests for a
-- LV composed hangul syllable followed by a jamo T.

{-# INLINE composeChar #-}

composeChar
    :: D.DecomposeMode
    -> A.MArray s       -- destination array for decomposition
    -> Char             -- char to be decomposed
    -> Int              -- array index
    -> Maybe Char       -- last starter
    -> ReBuf            -- reorder buffer
    -> JamoBuf          -- jamo buffer
    -> ST s (Int, Maybe Char, ReBuf, JamoBuf)
    -- ^ index, starter, reorder buf, jamobuf
composeChar mode marr = go . (: [])
    where
        go [] !index !st !rbuf !jbuf = pure (index, st, rbuf, jbuf)
        go (ch : rest) index st rbuf jbuf
            | H.isHangul ch = do
                j <- writeStarterRbuf marr index st rbuf
                (k, jbuf') <- composeCharHangul marr j jbuf ch
                go rest k Nothing Empty jbuf'

            | H.isJamo ch = do
                j <- writeStarterRbuf marr index st rbuf
                (k, jbuf') <- composeCharJamo marr j jbuf ch
                go rest k Nothing Empty jbuf'

            | D.isDecomposable mode ch = do
                go (D.decomposeChar mode ch ++ rest) index st rbuf jbuf

            | otherwise = do
                index' <- writeJamoBuf marr index jbuf
                (!i, !st', !rbuf') <- reorder marr index' st rbuf ch
                go rest i st' rbuf' JamoEmpty

        {-# INLINE reorder #-}
        reorder _ i st rbuf c
            | CC.isCombining c = return (i, st, insertIntoReBuf c rbuf)
        reorder _ i (Just st) Empty c
            | C.composePairSecondNonCombining c
            , Just x <- C.composePairNonCombining st c = return (i, Just x, Empty)
        reorder arr i st rbuf c = do
            j <- writeStarterRbuf arr i st rbuf
            return (j, Just c, Empty)

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
  let outer !arr !maxi = encode
       where
        -- keep the common case loop as small as possible
        encode !si !di st rbuf jbuf =
            -- simply check for the worst case
            if maxi < di + margin
               then realloc si di st rbuf jbuf
            else
                case next0 si of
                    Done -> do
                        -- Flush any leftover buffers, only one of rbuf/jbuf
                        -- will have contents
                        di'  <- writeStarterRbuf arr di st rbuf
                        di'' <- writeJamoBuf arr di' jbuf
                        done arr di''
                    Skip si'    -> encode si' di st rbuf jbuf
                    Yield c si' -> do
                        (di', st', rbuf', jbuf') <- composeChar mode arr c di st rbuf jbuf
                        encode si' di' st' rbuf' jbuf'

        -- keep uncommon case separate from the common case code
        {-# NOINLINE realloc #-}
        realloc !si !di st rbuf jbuf = do
            let newlen = maxi * 2
            arr' <- A.new newlen
            A.copyM arr' 0 arr 0 di
            outer arr' (newlen - 1) si di st rbuf jbuf

  outer arr0 (mlen - 1) s0 0 Nothing Empty JamoEmpty
{-# INLINE [0] unstreamC #-}
