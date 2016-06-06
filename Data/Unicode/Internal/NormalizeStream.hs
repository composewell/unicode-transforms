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
-- import           Data.Text.Internal.Fusion              (Step (..), Stream (..))
import Data.Text.Internal.Fusion.Types
import Data.Text.Internal.Fusion.Size

import qualified Data.Text.Array                        as A
import           Data.Text.Internal                     (Text (..))
import           Data.Text.Internal.Fusion.Size         (upperBound)
import           Data.Text.Internal.Private             (runText)
import           Data.Text.Internal.Unsafe.Char         (unsafeWrite)
import           GHC.ST                                 (ST (..))

import Data.Text.Internal.Unsafe.Char (ord, unsafeChr, unsafeWrite)
import Data.Text.Internal.Unsafe.Shift (shiftL, shiftR)
import qualified Data.Text.Internal.Encoding.Utf16 as U16

import qualified Data.Unicode.Properties.CombiningClass as CC
import qualified Data.Unicode.Properties.Decompose      as NFD

-- We can unfold the loop for the common 2 char case
{-# INLINE writeReorderBuffer #-}
writeReorderBuffer :: A.MArray s -> Int -> [Char] -> ST s Int
writeReorderBuffer marr i rbuf = go i rbuf
    where
        go di [] = return di
        go di (x : xs) = do
            n <- unsafeWrite marr di x
            go (di + n) xs

-- {-# INLINE decomposeCharHangul #-}
decomposeCharHangul :: A.MArray s -> Int -> Char -> ST s (Int, [Char])
decomposeCharHangul marr j c = do
    case NFD.decomposeCharHangul c of
        Left  (l, v)    -> do
            n1 <- unsafeWrite marr j l
            n2 <- unsafeWrite marr (j + n1) v
            return ((j + n1 + n2), [])
        Right (l, v, t) -> do
            n1 <- unsafeWrite marr j l
            n2 <- unsafeWrite marr (j + n1) v
            n3 <- unsafeWrite marr (j + n1 + n2) t
            return (j + n1 + n2 + n3, [])

{-# INLINE decomposeChar #-}
decomposeChar :: A.MArray s -> Int -> [Char] -> Char -> ST s (Int, [Char])
decomposeChar marr i rbuf c | NFD.isHangul c = do
    j <- writeReorderBuffer marr i rbuf
    decomposeCharHangul marr j c

decomposeChar marr index robuf ch = do
    -- TODO: return fully decomposed form
    case NFD.isDecomposable ch of
        True  -> decomposeAll marr index robuf (NFD.decomposeChar ch)
        False -> reorder marr index robuf ch

    where
        {-# NOINLINE decomposeAll #-}
        decomposeAll _ i rbuf [] = return (i, rbuf)
        decomposeAll arr i rbuf (x : xs)  =
            case NFD.isDecomposable x of
                True  -> do
                    (i', rbuf') <- decomposeAll arr i rbuf (NFD.decomposeChar x)
                    decomposeAll arr i' rbuf' xs
                False -> do
                    (i', rbuf') <- reorder arr i rbuf x
                    decomposeAll arr i' rbuf' xs

        -- TODO: how to sort when the characters have same combining classes
        -- (reorder buffer) (input char) (undecomposed list)
        {-# INLINE reorder #-}
        reorder _ i [] c = return (i, [c])

        -- input char is a starter, flush the reorder buffer
        reorder arr i rbuf c | not (CC.isCombining c) =
            -- Unbox the string concatenation for common cases
            case rbuf of
              [y]       -> do
                            n1 <- unsafeWrite arr i y
                            n2 <- unsafeWrite arr (i + n1) c
                            return ((i + n1 + n2), [])
            {-
              [y1, y2]  -> do
                            n1 <- unsafeWrite marr i y1
                            n2 <- unsafeWrite marr (i + n1) y2
                            n3 <- unsafeWrite marr (i + n1 + n2) c
                            return ((i + n1 + n2 + n3), [])
            -}
              _         -> do
                            j <- writeReorderBuffer arr i rbuf
                            n <- unsafeWrite arr j c
                            return (j + n, [])

        -- input char is combining and there is a starter char in the buffer
        -- flush the starter char and add the combining char to the buffer
        reorder arr i [y] c | not (CC.isCombining y) = do
            n <- unsafeWrite arr i y
            return (i + n, [c])

        -- optimized ordering for common case of two combining chars
        reorder  _ i buf@[y] x = return (i, orderedPair)
            where
                -- {-# INLINE orderedPair #-}
                orderedPair =
                    case inOrder y x of
                        True  -> buf ++ [x]
                        -- True  -> [y, x]
                        False -> x : buf

                inOrder c1 c2 =
                    CC.getCombiningClass c1 <= CC.getCombiningClass c2

        -- unoptimized generic sort for more than two combining chars
        reorder _ i buf x = return (i, (sortCluster (buf ++ [x])))
            where
                {-# NOINLINE sortCluster #-}
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
                                (di', rbuf') <- decomposeChar arr di rbuf c
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

  outer arr0 (mlen - 1) s0 0 []
{-# INLINE [0] unstream #-}
-- {-# RULES "STREAM stream/unstream fusion" forall s. stream (unstream s) = s #-}

-- we can generate this from UCD
maxDecomposeLen :: Int
maxDecomposeLen = 10

-- decomposeX = unstream . stream
