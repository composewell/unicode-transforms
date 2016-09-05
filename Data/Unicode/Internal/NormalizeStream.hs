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
    )
    where

import           Control.Monad                          (ap)
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

import qualified Data.Unicode.Properties.CombiningClass as CC
import qualified Data.Unicode.Properties.Decompose      as D

-- reorder buffer
data ReBuf = Empty | One {-# UNPACK #-} !Char | Many [Char]

{-# INLINE writeReorderBuffer #-}
writeReorderBuffer :: A.MArray s -> Int -> ReBuf -> ST s Int
writeReorderBuffer _ di Empty = return di

writeReorderBuffer marr di (One c) = do
    n <- unsafeWrite marr di c
    return (di + n)

writeReorderBuffer marr di (Many str) = go di str
    where
        go i [] = return i
        go i (c : cs) = do
            n <- unsafeWrite marr i c
            go (i + n) cs

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

-- | /O(n)/ Convert a 'Stream Char' into a decompose normalized 'Text'.
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
