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
      stream
    , unstream
--    , decomposeX
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
import qualified Data.Unicode.Properties.Decompose      as NFD

import Data.Array.ST (STUArray, getElems, newArray_)
import qualified Data.Array.Base as Arr

-- to specialize to empty and non-empty cases we can use a Maybe ReBuf?
type ReBuf s = STUArray s Int Char

{-# INLINE writeReorderBuffer #-}
writeReorderBuffer :: A.MArray s -> Int -> ReBuf s -> Int -> ST s Int
writeReorderBuffer darr di rarr ri = go di 0
    where
        go i i1 | i1 >= ri = return i
        go i i1 = do
            c <- Arr.unsafeRead rarr i1
            n <- unsafeWrite darr i c
            go (i + n) (i1 + 1)

-- {-# INLINE decomposeCharHangul #-}
decomposeCharHangul :: A.MArray s -> Int -> Char -> ST s (Int, Int)
decomposeCharHangul darr j c = do
    case NFD.decomposeCharHangul c of
        Left  (l, v)    -> do
            n1 <- unsafeWrite darr j l
            n2 <- unsafeWrite darr (j + n1) v
            return ((j + n1 + n2), 0)
        Right (l, v, t) -> do
            n1 <- unsafeWrite darr j l
            n2 <- unsafeWrite darr (j + n1) v
            n3 <- unsafeWrite darr (j + n1 + n2) t
            return (j + n1 + n2 + n3, 0)

{-# INLINE decomposeChar #-}
decomposeChar :: A.MArray s -> Int -> ReBuf s -> Int -> Char -> ST s (Int, Int)
decomposeChar darr i rarr ri c | NFD.isHangul c = do
    j <- writeReorderBuffer darr i rarr ri
    decomposeCharHangul darr j c

decomposeChar darray dindex rarray rindex ch = do
    -- TODO: return fully decomposed form
    case NFD.isDecomposable ch of
      -- NFD.FalseA -> reorder darray dindex rarray rindex ch
      NFD.TrueA -> reorder darray dindex rarray rindex ch
      --NFD.TrueA  -> decomposeAll darray dindex rarray rindex (NFD.decomposeChar ch)
      _          -> reorder darray dindex rarray rindex ch

    where
        {-
        {-# INLINE decomposeAll #-}
        decomposeAll _    di _    ri []        = return (di, ri)
        decomposeAll darr di rarr ri (x : xs)  =
            case NFD.isDecomposable x of
                NFD.TrueA  -> do
                    (di', ri') <- decomposeAll darr di rarr ri (NFD.decomposeChar x)
                    decomposeAll darr di' rarr ri' xs
                _ -> do
                    (di', ri') <- reorder darr di rarr ri x
                    decomposeAll darr di' rarr ri' xs
                    -}

        -- TODO: how to sort when the characters have same combining classes
        -- (array) (array index) (reorder buffer) (input char)
        {-# INLINE reorder #-}
        reorder darr di rarr ri c | ri == 0 = do
            -- XXX we can write this to darr as well so that we do not have
            -- read rarr in common case
            -- Arr.unsafeWrite rarr 0 c
            -- return (di, 1)
            --n1 <- unsafeWrite darr di c
            --return (di + n1, 1)
            return (di + 1, 1)

        -- reorder buffer has a single character in it
        -- it could be starter or combining check that and take appropriate
        -- action
        reorder darr di rarr ri c | ri == 1 = do
            -- c0 <- Arr.unsafeRead rarr 0
            case not (CC.isCombining c) of
                -- input char is a starter
                True -> do
                    -- flush the reorder buffer
                    -- n1 <- unsafeWrite darr di c0
                    -- n2 <- unsafeWrite darr (di + n1) c
                    -- return ((di + n1 + n2), 0)
                    --n2 <- unsafeWrite darr (di ) c
                    --return ((di + n2), 0)
                    return (di + 1, 0)
                False -> do
                    --n2 <- unsafeWrite darr (di) c
                    --return ((di + n2), 0)
                    return (di + 2, 0)

                {-
                -- input char is combining
                False ->
                    case not (CC.isCombining c0) of
                        -- there is a starter char in the buffer. flush the
                        -- starter char and add the combining char to the
                        -- buffer
                        True -> do
                            n <- unsafeWrite darr di c0
                            Arr.unsafeWrite rarr 0 c
                            return (di + n, 1)
                        -- there is a combining char in the buffer.
                        -- add the new char to be reorder buffer
                        False -> do
                            let (c1, c2) = orderedPair c0 c
                            Arr.unsafeWrite rarr 0 c1
                            Arr.unsafeWrite rarr 1 c2
                            return (di, 2)
                    where
                        -- {-# INLINE orderedPair #-}
                        orderedPair c1 c2 =
                            case inOrder c1 c2 of
                                True  -> (c1, c2)
                                False -> (c2, c1)

                        inOrder c1 c2 =
                            CC.getCombiningClass c1 <= CC.getCombiningClass c2
                            -}

        -- reorder buffer has 2 or more chars. They are all combining.
        {-
        reorder darr di rarr ri c = do
            case not (CC.isCombining c) of
                -- input char is a starter, flush the reorder buffer
                True -> do
                    j <- writeReorderBuffer darr di rarr ri
                    n <- unsafeWrite darr j c
                    return (j + n, 0)
                -- unoptimized generic sort for more than two combining chars
                False -> do
                    str <- getElems rarr
                    let str' = (sortCluster (take ri str ++ [c]))
                    writeStrToArray rarr 0 str'
                    return (di, ri + 1)
            where
                {-# INLINE sortCluster #-}
                sortCluster =   map fst
                              . sortBy (comparing snd)
                              . map (ap (,) CC.getCombiningClass)

                writeStrToArray _ _ [] = return ()
                writeStrToArray arr i (x : xs) = do
                    Arr.unsafeWrite arr i x
                    writeStrToArray arr (i + 1) xs
                    -}

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
          -- | (n `shiftR` 10) == 0x36    = Yield (U16.chr2 n n2) (i + 2)
          | n >= 0xD800 && n <= 0xDBFF = Yield (U16.chr2 n n2) (i + 2)
          | otherwise                  = Yield (unsafeChr n) (i + 1)
          where
            n  = A.unsafeIndex arr i
            n2 = A.unsafeIndex arr (i + 1)
{-# INLINE [0] stream #-}

-- | /O(n)/ Convert a 'Stream Char' into a normalized 'Text'.
unstream :: Stream Char -> Text
unstream (Stream next0 s0 len) = runText $ \done -> do
  -- Before encoding each char we perform a buffer realloc check assuming
  -- worst case encoding size of two 16-bit units for the char. Just add an
  -- extra space to the buffer so that we do not end up reallocating even when
  -- all the chars are encoded as single unit.
  let margin = 1 + maxDecomposeLen
      mlen = (upperBound 4 len + margin)
  arr0 <- A.new mlen
  rarr <- newArray_ (0, margin)

  let outer !arr !maxi = encode
       where
        -- keep the common case loop as small as possible
        encode !si !di !ri =
            -- simply check for the worst case
            -- XXX adds 5 instructions
            if maxi < di + margin
            then realloc si di ri
            else
                case next0 si of
                    Done -> do
                        -- di' <- writeReorderBuffer arr di rarr ri
                        --done arr di'
                        done arr di
                    Skip si'    -> encode si' di ri
                    Yield c si' -> do
                                (di', ri') <- decomposeChar arr di rarr ri c
                                encode si' di' ri'
                                -- n <- unsafeWrite arr di c
                                -- encode si' (di + n) rbuf

        -- keep uncommon case separate from the common case code
        {-# NOINLINE realloc #-}
        realloc !si !di !ri = do
            let newlen = maxi * 2
            arr' <- A.new newlen
            A.copyM arr' 0 arr 0 di
            outer arr' (newlen - 1) si di ri

  outer arr0 (mlen - 1) s0 0 0
{-# INLINE [0] unstream #-}
-- {-# RULES "STREAM stream/unstream fusion" forall s. stream (unstream s) = s #-}

-- we can generate this from UCD
maxDecomposeLen :: Int
maxDecomposeLen = 10

{-
-- | Unchecked read of an immutable array.  May return garbage or
-- crash on an out-of-bounds access.
unsafeIndex32 :: Array -> Int -> Word32
unsafeIndex32 Array{..} i@(I# i#) =
  CHECK_BOUNDS("unsafeIndex",aLen,i)
    case indexWord32Array# aBA i# of r# -> (W32# r#)
{-# INLINE unsafeIndex32 #-}

-- | Unchecked write of a mutable array.  May return garbage or crash
-- on an out-of-bounds access.
unsafeWrite32 :: MArray s -> Int -> Word32 -> ST s ()
unsafeWrite32 MArray{..} i@(I# i#) (W32# e#) = ST $ \s1# ->
  CHECK_BOUNDS("unsafeWrite",maLen,i)
  case writeWord32Array# maBA i# e# s1# of
    s2# -> (# s2#, () #)
{-# INLINE unsafeWrite32 #-}
-}
