{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

#if __GLASGOW_HASKELL__ < 7100
{-# LANGUAGE DeriveDataTypeable       #-}
#endif

-- |
-- Module      : Data.Unicode.UTF8Proc
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- NOTE: This is an internal module with potentially unstable API. Please do
-- not use this directly. Currently only normalization API is exported. Other
-- APIs can be supported on demand.
--
-- This is a lightweight package supporting a majority of frequently used
-- unicode transformation operations. It is based on the @utf8proc@ C utility
-- from the xqilla project (xqilla version 2.3.2)
-- <http://xqilla.sourceforge.net/HomePage>.
-- This package has no external system package dependencies unlike @text-icu@
-- but has limited features. @text-icu@ package provides a complete set of
-- unicode operations as bindings to the @icu@ library.
--
-- It provides the following major features with some limitations mentioned in
-- parenthesis:
--
-- * Unicode normalization
-- * Case folding (does not provide an option for Turkish /__i__/ handling)
-- * Case conversion (no locale specific handling)
-- * Boundary Analysis (no locale specific handling)
--
-- Additional features include:
--
-- * NLF sequence conversion
-- * Stripping certain character classes
-- * Lumping certain characters
--
-- Following features provided by @text-icu@ are missing in this package:
--
-- * Normalization checks
-- * FCD normalization form
-- * String collation
-- * Iteration
-- * Regular expressions
--

module Data.Unicode.UTF8Proc
    (
        -- * Normalization operation parameters
          NormalForm(..)
        , NormalizationOption(..)
        , NormalizeOp
        , mkNormalizeOp

        -- * Unicode transformations
        , TransformMode(..)
        , TransformOp(Normalize)
        , TransformError
        , transform
    ) where

#include "utf8proc.h"

import           Control.Exception     (Exception, throwIO)
import           Data.Int              (Int32, Int64)
import           Data.Typeable         (Typeable)
import           Foreign.C.String      (CStringLen)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr           (Ptr)
import           Foreign.Storable      (peek)

-------------------------------------------------------------------------------
-- Flags to be passed to the C library
-------------------------------------------------------------------------------

-- Note we could have used the C flag definitions directly. The Haskell
-- representation is just to disallow illegal combinations and to group and
-- document the flags for better understanding.

type CFlags = (#type int)

class Flags a where
    toCFlags ::  a -> CFlags

-- | The basic normalized form - composed or decomposed
data NormalForm =
      Composed   -- ^ Return a result with composed characters.
    | Decomposed -- ^ Return a result with decomposed characters.

instance Flags NormalForm where
    toCFlags Composed   = #const UTF8PROC_COMPOSE
    toCFlags Decomposed = #const UTF8PROC_DECOMPOSE

-- | Options that can be combined with basic normalization op
data NormalizationOption =
      -- | Compatiblity decomposition (i.e. formatting information is lost)
      Compat
      -- | Strips all character markings (non-spacing, spacing and enclosing)
      -- (i.e. accents)
    | StripMark

instance Flags NormalizationOption where
    toCFlags Compat = #const UTF8PROC_COMPAT
    toCFlags StripMark  = #const UTF8PROC_STRIPMARK

-- | Data type for normalize operation
data NormalizeOp = NormalizeOp CFlags

instance Flags NormalizeOp where
    toCFlags (NormalizeOp fl) = fl

-- | Constructor for @NormalizeOp@.
mkNormalizeOp :: NormalForm -> [NormalizationOption] -> NormalizeOp
mkNormalizeOp nf opts = NormalizeOp $ toCFlags nf + sum (map toCFlags opts)

-- | Case conversion. Supports on simple case mapping (no locale specific
-- handling). See http://userguide.icu-project.org/transforms/casemappings
data CaseConversionOp =
      UpperCase --  ^ Unicode upper case
    | LowerCase --  ^ Unicode lower case
      -- | Performs unicode case folding, to be able to do a case-insensitive
      -- string comparison.
      -- Note - This implementation does not provide an option for treating the
      -- letter I specially or not for Turkic languages.
    | Casefold

instance Flags CaseConversionOp where
    toCFlags UpperCase = #const UTF8PROC_UPPERCASE
    toCFlags LowerCase = #const UTF8PROC_LOWERCASE
    toCFlags Casefold  = #const UTF8PROC_CASEFOLD

-- | NLF sequence (LF, CRLF, CR, NEL) conversion options
data NLFConversionOp =
      -- | Indicating that NLF-sequences (LF, CRLF, CR, NEL) are
      --   representing a line break, and should be converted to the
      --   unicode character for line separation (LS).
      NLF2LS
      -- | Indicating that NLF-sequences are representing a paragraph
      --   break, and should be converted to the unicode character for
      --   paragraph separation (PS).
    | NLF2PS
      -- | Indicating that the meaning of NLF-sequences is unknown.
    | NLF2LF

instance Flags NLFConversionOp where
    toCFlags NLF2LS = #const UTF8PROC_NLF2LS
    toCFlags NLF2PS = #const UTF8PROC_NLF2PS
    toCFlags NLF2LF = #const UTF8PROC_NLF2LF

-- Note - Char and non-char boundary marking options are exclusive of each 
-- other
--
-- | Boundary marking operations
-- Note - This implementation does not handle locale specific cases
-- http://unicode.org/reports/tr29/
data BoundingMarksOp =
      -- | Inserts 0xFF bytes at the beginning of each sequence which
      -- is representing a single grapheme cluster (see UAX#29).
      MarkCharBounds
      -- | Insert word or sentence bounding marks
    | MarkNonCharBounds [NonCharBoundOp]

instance Flags BoundingMarksOp where
    toCFlags MarkCharBounds = #const UTF8PROC_CHARBOUND
    toCFlags (MarkNonCharBounds ops) =  sum $ map toCFlags ops

-- These options can be combined together
-- | Word or sentence boundary marking
data NonCharBoundOp =
      --  | Insert Word Bounding Marks(0xFFFF) between words in the
      --  given unicode string. (see UAX#29)
      --  For instance:  |a|$|-|34,567.14|%|b|
      MarkWordBounds
      --  | Insert sentences Bounding Marks(0xFFFE) between
      --  sentences in the given unicode string. (see UAX#29)
      --  For instance: |("Go.")|(He did.)|
    | MarkSentenceBounds

instance Flags NonCharBoundOp where
    toCFlags MarkWordBounds     = #const UTF8PROC_WORDBOUND
    toCFlags MarkSentenceBounds = #const UTF8PROC_SENTENCEBOUND

-- TBD - Checks to enforce operation of one type is specified only once in an
-- API call.
-- At least one operation should be specified, multiple modes can be combined
-- together. But an operation must be specified only once in an API call.

-- | Transformation operations.
data TransformOp =
      Normalize NormalizeOp
      -- | Convert to upper or lower case
    | CaseConvert CaseConversionOp
      -- | Insert bounding marks at Char, Word, Sentence boundaries
    | MarkBoundaries BoundingMarksOp

      -- | Specifies how NLF sequences are handled
    | ConvertNLF NLFConversionOp

      -- (See lump.txt for details.)
      -- | Lumps certain characters together
      -- (e.g. HYPHEN U+2010 and MINUS U+2212 to ASCII "-").
      -- If NLF2LF is set, this includes a transformation of
      -- paragraph and line separators to ASCII line-feed (LF).
    | Lump

      -- Stripping characters --

      -- | Strip "default ignorable characters"
    | StripIgnorable
      -- | Strips and/or converts control characters.
      --   NLF-sequences are transformed into space, except if one of
      --   the NLF2LS/PS/LF options is given.
      --   HorizontalTab (HT) and FormFeed (FF) are treated as a
      --   NLF-sequence in this case.
      --   All other control characters are simply removed.
    | StripCC
      -- | Remove the "Diacritic" code points
    | StripDiacritic

instance Flags TransformOp where
    -- Bundles or choices of flags
    toCFlags (Normalize m)      = toCFlags m
    toCFlags (CaseConvert m)    = toCFlags m
    toCFlags (MarkBoundaries m) = toCFlags m
    toCFlags (ConvertNLF m)     = toCFlags m

    -- Invdividual flags
    toCFlags Lump             = #const UTF8PROC_LUMP
    toCFlags StripIgnorable   = #const UTF8PROC_IGNORE
    toCFlags StripCC          = #const UTF8PROC_STRIPCC
    toCFlags StripDiacritic   = #const UTF8PROC_REMOVE_DIACRITIC

-- | Optional modifiers to control the overall transformation behavior
data TransformMode =
    -- | Return an error, if the input contains unassigned code points.
      RejectNA
    -- | Unicode Versioning Stability has to be respected.
    | StableMode
    -- | Indicates that the input is in UTF16 format and so will be the output.
    | UTF16Mode

instance Flags TransformMode where
    toCFlags RejectNA   = #const UTF8PROC_REJECTNA
    toCFlags StableMode = #const UTF8PROC_STABLE
    toCFlags UTF16Mode  = #const UTF8PROC_UTF16

-------------------------------------------------------------------------------
-- Transform a unicode text according to requested parameters
-------------------------------------------------------------------------------

-- | Exception raised when a tranformation operation fails.
data TransformError = TransformError
    deriving (Show, Eq, Typeable)

instance Exception TransformError

-- | Transform a unicode string according to the requested mode and operations.
-- Multiple modes and operations can be combined together. When @modes@ list
-- is empty default modes (complement of the 'TransformMode's) are used.
transform :: [TransformMode] -> [TransformOp] -> CStringLen -> IO CStringLen
transform modes ops (str, strlen) = alloca usingPtr
    where
        usingPtr bufPtr = do
            len <- c_utf8proc_map str (fromIntegral strlen) bufPtr flags
            if len < 0
                then
                    throwIO TransformError
                else do
                    buf <- peek bufPtr
                    return (buf, (fromIntegral len))
        flags = sum (map toCFlags modes) + sum (map toCFlags ops)

foreign import ccall unsafe "utf8proc_map" c_utf8proc_map
    :: Ptr CChar          -- str
    -> (#type ssize_t)    -- strlen
    -> Ptr (Ptr CChar)    -- buffer (result)
    -> (#type int)        -- options
    -> IO (#type ssize_t) -- length or err
