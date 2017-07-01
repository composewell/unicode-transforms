{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Data.Unicode.Internal.NormalizeStream
-- Copyright   : (c) 2016 Harendra Kumar
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

import           Control.Monad                          (ap)
import           Data.Char                              (chr, ord)
import           Data.List                              (sortBy)
import           Data.Ord                               (comparing)
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

data ReBuf = Empty | One {-# UNPACK #-} !Char | Many [Char]

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

writeReorderBuffer marr di (Many str) = writeStr marr di str

-------------------------------------------------------------------------------
-- Decomposition of Hangul characters is done algorithmically
-------------------------------------------------------------------------------

-- {-# INLINE decomposeCharHangul #-}
decomposeCharHangul :: A.MArray s -> Int -> Char -> ST s (Int, ReBuf)
decomposeCharHangul marr j c = do
    case D.decomposeCharHangul c of
        Left  (l, v)    -> do
            n1 <- unsafeWrite marr j l
            n2 <- unsafeWrite marr (j + n1) v
            return ((j + n1 + n2), Empty)
        Right (l, v, t) -> do
            n1 <- unsafeWrite marr j l
            n2 <- unsafeWrite marr (j + n1) v
            n3 <- unsafeWrite marr (j + n1 + n2) t
            return (j + n1 + n2 + n3, Empty)

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
    decomposeCharHangul marr j c

-------------------------------------------------------------------------------
-- Decomposition of characters other than Hangul
-------------------------------------------------------------------------------

decomposeChar mode marr index reBuf ch = do
    -- TODO: return fully decomposed form
    case D.isDecomposable mode ch of
      D.FalseA -> reorder marr index reBuf ch
      D.TrueA  -> decomposeAll marr index reBuf (D.decomposeChar mode ch)
      _ -> reorder marr index reBuf ch

    where
        {-# INLINE decomposeAll #-}
        decomposeAll _ i rbuf [] = return (i, rbuf)
        decomposeAll arr i rbuf (x : xs)  =
            case D.isDecomposable mode x of
                D.TrueA  -> do
                    (i', rbuf') <- decomposeAll arr i rbuf
                                                (D.decomposeChar mode x)
                    decomposeAll arr i' rbuf' xs
                _ -> do
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
        reorder _ i Empty c = return (i, One c)

        -- input char is a starter, flush the reorder buffer
        reorder arr i (One c0) c | not (CC.isCombining c) = do
            n1 <- unsafeWrite arr i c0
            n2 <- unsafeWrite arr (i + n1) c
            return ((i + n1 + n2), Empty)

        -- input char is combining and there is a starter char in the buffer
        -- flush the starter char and add the combining char to the buffer
        reorder arr i (One c0) c | not (CC.isCombining c0) = do
            n <- unsafeWrite arr i c0
            return (i + n, One c)

        -- optimized ordering for common case of two combining chars
        -- XXX replace many with Two here
        reorder  _ i (One c0) c = return (i, Many orderedPair)
            where
                -- {-# INLINE orderedPair #-}
                orderedPair =
                    case inOrder c0 c of
                        True  -> [c0, c]
                        False -> [c, c0]

                inOrder c1 c2 =
                    CC.getCombiningClass c1 <= CC.getCombiningClass c2

        -- input char is a starter, flush the reorder buffer
        reorder arr i rbuf c | not (CC.isCombining c) = do
            j <- writeReorderBuffer arr i rbuf
            n <- unsafeWrite arr j c
            return (j + n, Empty)

        -- unoptimized generic sort for more than two combining chars
        reorder _ i (Many str) c = return (i, Many (sortCluster (str ++ [c])))
            where
                {-# INLINE sortCluster #-}
                sortCluster =   map fst
                              . sortBy (comparing snd)
                              . map (ap (,) CC.getCombiningClass)

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
    -> Char
    -> ST s (Int, Char) -- return new index, new starter
composeAndWrite arr di st1 Empty st2 = do
    n <- unsafeWrite arr di st1
    return (di + n, st2)

composeAndWrite arr di st1 (One c) st2 =
    composeAndWrite' arr di st1 [c] st2

composeAndWrite arr di st1 (Many str) st2 =
    composeAndWrite' arr di st1 str st2

composeAndWrite'
    :: A.MArray s
    -> Int
    -> Char
    -> [Char]
    -> Char
    -> ST s (Int, Char)
composeAndWrite' arr di st1 str st2 = go di st1 [] 0 str
    where
        -- arguments: index, starter, uncombined chars,
        -- cc of prev uncombined char, unprocessed str
        go i st [] _ [] =
                case C.composePair st st2 of
                    Just x  -> return (i, x)
                    Nothing -> do
                        n <- unsafeWrite arr i st
                        return (i + n, st2)

        go i st uncs _ [] = do
            j <- writeStr arr i (st : uncs)
            return (j, st2)

        go i st [] _ (c : cs) = do
            case C.composePair st c of
                Just x  -> go i x [] 0 cs
                Nothing -> do
                    go i st [c] (CC.getCombiningClass c) cs

        go i st uncs cc (c : cs) = do
            let ccc = CC.getCombiningClass c
            if ccc > cc then
                case C.composePair st c of
                    Just x  -> go i x uncs cc cs
                    Nothing -> do
                        go i st (uncs ++ [c]) ccc cs
            else go i st (uncs ++ [c]) ccc cs

writeStarterRbuf :: A.MArray s
                 -> Int
                 -> Maybe Char
                 -> ReBuf
                 -> ST s Int
writeStarterRbuf marr di st rbuf =
    case st of
        Nothing -> writeReorderBuffer marr di rbuf
        Just starter ->
            -- XXX null char hack
            composeAndWrite marr di starter rbuf '\0' >>= (return . fst)

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
    -> Int              -- array index
    -> Maybe Char       -- last starter
    -> ReBuf            -- reorder buffer
    -> JamoBuf          -- jamo buffer
    -> Char             -- char to be decomposed
    -> ST s (Int, Maybe Char, ReBuf, JamoBuf)
    -- ^ index, starter, reorder buf, jamobuf
composeChar _ marr index st rbuf jbuf ch | H.isHangul ch || H.isJamo ch = do
    j <- writeStarterRbuf marr index st rbuf
    (k, jbuf') <- if H.isJamo ch then
        composeCharJamo marr j jbuf ch
    else
        composeCharHangul marr j jbuf ch
    return (k, Nothing, Empty, jbuf')
    where
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

        composeCharHangul arr i jb c = do
            ix <- writeJamoBuf arr i jb
            case H.isHangulLV c of
                True -> return (ix, JamoLV c)
                False -> do
                    n <- unsafeWrite arr ix c
                    return (ix + n, JamoEmpty)

-------------------------------------------------------------------------------
-- Composition of characters other than Hangul
-------------------------------------------------------------------------------

composeChar mode marr index starter reBuf jbuf ch = do
    index' <- writeJamoBuf marr index jbuf
    case D.isDecomposable mode ch of
        D.FalseA -> do
            (i, st, rbuf) <- reorder marr index' starter reBuf ch
            return (i, st, rbuf, JamoEmpty)
        D.TrueA  -> do
            decomposeAll marr index' starter reBuf jbuf (D.decomposeChar mode ch)
        _ -> do
            (i, st, rbuf) <- reorder marr index' starter reBuf ch
            return (i, st, rbuf, JamoEmpty)
    where
        {-# INLINE decomposeAll #-}
        decomposeAll _ i st rbuf jb [] = return (i, st, rbuf, jb)
        decomposeAll arr i st rbuf jb (x : xs)  =
            case D.isDecomposable mode x of
                D.TrueA  -> do
                    (i', st', rbuf', jb') <- decomposeAll arr i st rbuf jb
                                                (D.decomposeChar mode x)
                    decomposeAll arr i' st' rbuf' jb' xs
                _ -> do
                    -- XXX this recursive call here hurts performance
                    -- We can make the hangul composition a separate function
                    -- and call that or reorder here based on the type fo char
                    (i', st', rbuf', jb') <- composeChar mode arr i st rbuf jb x
                    decomposeAll arr i' st' rbuf' jb' xs

        -- Unicode 9.0.0: 3.11
        -- D108 Reorderable pair: Two adjacent characters A and B in a coded
        -- character sequence <A,B> are a Reorderable Pair if and only if
        -- ccc(A) > ccc(B) > 0.
        --
        -- (array) (array index) (reorder buffer) (input char)
        {-# INLINE reorder #-}
        reorder _ i st Empty c = return (i, st, One c)

        -- Unicode 9.0.0: 3.11
        -- D111: a starter can never become a non-starter after
        -- combining. If that happens we will potentially have to remember all
        -- previous starters so that the new non-starter can be combined with
        -- the previous starter.
        --
        -- To compose, try to combine an unblocked char with the last starter
        -- and remove if combined. A char with combining class equal or lower
        -- than the previous char is blocked. It implies that only adjacent
        -- starters can be combined.
        --
        -- input char is a starter
        -- does it combine with the previous starter?
        -- if no then flush and replace the last starter
        reorder arr i (Just st) (One c0) c | not (CC.isCombining c) = do
            case C.composePair st c0 of
                Just x  -> case C.composePair x c of
                    Just y -> return (i, Just y, Empty)
                    Nothing -> do
                        n <- unsafeWrite arr i x
                        return (i + n, Just c, Empty)
                Nothing -> case CC.isCombining c0 of
                    -- starter1 combining starter2
                    True -> do
                        n1 <- unsafeWrite arr i st
                        n2 <- unsafeWrite arr (i + n1) c0
                        return (i + n1 + n2, Just c, Empty)
                    -- starter1 starter2 starter3
                    False -> do
                        n1 <- unsafeWrite arr i st
                        case C.composePair c0 c of
                            Just y -> return (i + n1, Just y, Empty)
                            Nothing -> do
                                n2 <- unsafeWrite arr (i + n1) c0
                                return (i + n1 + n2, Just c, Empty)

        reorder arr i Nothing (One c0) c | not (CC.isCombining c) =
            case C.composePair c0 c of
                Just x  -> return (i, Just x, Empty)
                Nothing -> do
                    n <- unsafeWrite arr i c0
                    return (i + n, Just c, Empty)

        reorder arr i (Just st) (One c0) c | not (CC.isCombining c0) = do
            case C.composePair st c0 of
                Just x  -> return (i, Just x, One c)
                Nothing -> do
                    n <- unsafeWrite arr i st
                    return (i + n, Just c0, One c)

        -- input char is combining and there is a starter char in the buffer
        -- flush the starter char and add the combining char to the buffer
        reorder _arr i Nothing (One c0) c | not (CC.isCombining c0) = do
            return (i, Just c0, One c)

        -- optimized ordering for common case of two combining chars
        -- XXX replace many with Two here
        reorder  _ i st (One c0) c = return (i, st, Many orderedPair)
            where
                -- {-# INLINE orderedPair #-}
                orderedPair =
                    case inOrder c0 c of
                        True  -> [c0, c]
                        False -> [c, c0]

                inOrder c1 c2 =
                    CC.getCombiningClass c1 <= CC.getCombiningClass c2

        -- input char is a starter, flush the reorder buffer
        reorder arr i (Just st) rbuf c | not (CC.isCombining c) = do
            (j, st2) <- composeAndWrite arr i st rbuf c
            return (j, Just st2, Empty)

        reorder arr i Nothing rbuf c | not (CC.isCombining c) = do
            j <- writeReorderBuffer arr i rbuf
            return (j, Just c, Empty)

        -- unoptimized generic sort for more than two combining chars
        reorder _ i st (Many str) c =
            return (i, st, Many (sortCluster (str ++ [c])))
            where
                {-# INLINE sortCluster #-}
                sortCluster =   map fst
                              . sortBy (comparing snd)
                              . map (ap (,) CC.getCombiningClass)

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
                        (di', st', rbuf', jbuf') <- composeChar mode arr di st rbuf jbuf c
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
