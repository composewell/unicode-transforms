{-# LANGUAGE CPP                #-}
#if __GLASGOW_HASKELL__ < 7100
{-# LANGUAGE DeriveDataTypeable #-}
#endif
-- |
-- Module      : Data.Unicode.Types
-- Copyright   : (c) 2016 Harendra Kumar
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Character set normalization functions for Unicode.  The documentation and
-- API in this module is largely borrowed from @text-icu@.

module Data.Unicode.Types
    (
      NormalizationMode(..)
    ) where

import           Data.Typeable (Typeable)

-- |
-- Normalization transforms Unicode text into an equivalent
-- composed or decomposed form, allowing for easier sorting and
-- searching of text. Standard normalization forms are described in
-- <http://www.unicode.org/unicode/reports/tr15/>,
-- Unicode Standard Annex #15: Unicode Normalization Forms.
--
-- Characters with accents or other adornments can be encoded in
-- several different ways in Unicode.  For example, take the character A-acute.
-- In Unicode, this can be encoded as a single character (the
-- \"composed\" form):
--
-- @
--      00C1    LATIN CAPITAL LETTER A WITH ACUTE
-- @
--
-- or as two separate characters (the \"decomposed\" form):
--
-- @
--      0041    LATIN CAPITAL LETTER A
--      0301    COMBINING ACUTE ACCENT
-- @
--
-- To a user of your program, however, both of these sequences should
-- be treated as the same \"user-level\" character \"A with acute
-- accent\".  When you are searching or comparing text, you must
-- ensure that these two sequences are treated equivalently.  In
-- addition, you must handle characters with more than one accent.
-- Sometimes the order of a character's combining accents is
-- significant, while in other cases accent sequences in different
-- orders are really equivalent.
--
-- Similarly, the string \"ffi\" can be encoded as three separate letters:
--
-- @
--      0066    LATIN SMALL LETTER F
--      0066    LATIN SMALL LETTER F
--      0069    LATIN SMALL LETTER I
-- @
--
-- or as the single character
--
-- @
--      FB03    LATIN SMALL LIGATURE FFI
-- @
--
-- The \"ffi\" ligature is not a distinct semantic character, and
-- strictly speaking it shouldn't be in Unicode at all, but it was
-- included for compatibility with existing character sets that
-- already provided it.  The Unicode standard identifies such
-- characters by giving them \"compatibility\" decompositions into the
-- corresponding semantic characters.  When sorting and searching, you
-- will often want to use these mappings.
--
-- Normalization helps solve these problems by transforming text into
-- the canonical composed and decomposed forms as shown in the first
-- example above.  In addition, you can have it perform compatibility
-- decompositions so that you can treat compatibility characters the
-- same as their equivalents.  Finally, normalization rearranges accents
-- into the proper canonical order, so that you do not have to worry
-- about accent rearrangement on your own.
--
-- The W3C generally recommends to exchange texts in 'NFC'.  Note also
-- that most legacy character encodings use only precomposed forms and
-- often do not encode any combining marks by themselves. For
-- conversion to such character encodings the Unicode text needs to be
-- normalized to 'NFC'.  For more usage examples, see the Unicode
-- Standard Annex.
--
data NormalizationMode
    = NFD    -- ^ Canonical decomposition.
    | NFKD   -- ^ Compatibility decomposition.
    | NFC    -- ^ Canonical decomposition followed by canonical composition.
    | NFKC   -- ^ Compatibility decomposition followed by canonical composition.
      deriving (Eq, Show, Enum, Typeable)
