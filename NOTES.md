# Unicode Resources

* http://unicode.org/
* http://www.unicode.org/versions/Unicode9.0.0/ch03.pdf Normalization spec
* http://www.unicode.org/reports/tr15/ UNICODE NORMALIZATION FORMS
* http://www.unicode.org/reports/tr44/ UNICODE CHARACTER DATABASE
* [Unicode Character Database](http://www.unicode.org/Public/UCD/latest/ucd)

# Unicode related Haskell packages

## Casemapping and Casefolding
The `text` package already provides proper unicode casemapping and casefolding
operations.

## Additional features in text-icu

The Haskell package `text-icu` is a full featured implementation of unicode
operations via bindings to the C++ `icu` libraries.

`text-icu` provides the following additional features:
* Normalization checks
* FCD normalization for collation
* String collation
* Iteration
* Regular expressions

# Haskell Unicode Landscape

Unicode functionality in Haskell is fragmented across various packages.  The
most comprehensive functionality is provided by `text-icu` which is based on
the `icu` C++ libraries. All related packages are listed here, they may or may
not be up to date or useful.

* [text-icu](https://stackage.org/lts/package/text-icu)

## Basic

* [base](https://www.stackage.org/lts/package/base) Data.Char module
* [charset](https://www.stackage.org/lts/package/charset) Fast unicode character sets

## Unicode Character Database
* [unicode-properties](https://hackage.haskell.org/package/unicode-properties) Unicode 3.2.0 character properties
* [hxt-charproperties](http://www.stackage.org/lts/package/hxt-charproperties) Character properties and classes for XML and Unicode
* [unicode-names](http://hackage.haskell.org/package/unicode-names) Unicode 3.2.0 character names
* [unicode](https://hackage.haskell.org/package/unicode) Construct and transform unicode characters

## Unicode Strings
### ByteStrings (UTF8)
* [utf8-string](https://www.stackage.org/lts/package/utf8-string) Support for reading and writing UTF8 Strings
* [utf8-light](https://www.stackage.org/lts/package/utf8-light) Lightweight UTF8 handling
* [hxt-unicode](https://www.stackage.org/lts/package/hxt-unicode) Unicode en-/decoding functions for utf8, iso-latin-\* and other encodings
### Text (UTF16)
* [text](https://www.stackage.org/lts/package/text) An efficient packed Unicode text type
* [text-normal](https://hackage.haskell.org/package/text-normal) Data types for Unicode-normalized text - depends on text-icu
