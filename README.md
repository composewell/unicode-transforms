# Unicode Transforms
This is a lightweight library supporting a limited set of unicode
transformations on `ByteStrings` (UTF-8) and `Text` (UTF-16).
without requiring any other system libraries.

Currently it exposes only normalization API. Support for more features listed
in a later section exists but the API is not exposed intentionally. If you have
a use case for any of these features please raise an issue or send a pull
request.

This package is based on the `utf8proc` C utility. The utf8proc version bundled
with this package is taken from the
[xqilla project](http://xqilla.sourceforge.net/HomePage)
(xqilla version 2.3.2). It should not be too difficult to translate this into a
native Haskell package.

## Comparison with `text-icu`
`text-icu` is a full featured implementation of unicode operations via bindings
to the `icu` libraries. If you do not mind a dependency on the `icu` libraries
(separately installed) or need a comprehensive set of unicode operations then
`text-icu` will be a better choice.

## Features

### Exposed
Normalization in NFC, NFKC, NFD, NFKD forms is fully supported and exposed via
an API.

### Available but not exposed
The following features are available but not exposed via an API. If you need
any of those they can be exposed quickly, please raise an issue or send a pull
request.

Major features:

 Feature               | Limitations
 -------               | -----------
 Case folding          | No option for special handling of `i` in Turkish
 Case conversion       | No locale specific handling
 Boundary Analysis     | No locale specific handling

Additional features:
* NLF sequence conversion
* Stripping certain character classes
* Lumping certain characters

### Missing features
The following features provided by `text-icu` are missing in this package:
* Normalization checks
* FCD normalization for collation
* String collation
* Iteration
* Regular expressions

# Haskell Unicode Landscape

Unicode functionality in Haskell is fragemented across various packages.  The
most comprehensive functionality is provided by `text-icu` which is based on
the `icu` C++ libraries.

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

# What needs to be done?

It will be good to consolidate all native haskell packages into a standard
module structure under a minimum number of packages and evolve those.

Some thoughts on consolidation. A total of 5 packages proposed to cover unicode
support: 
1. **_unicode-properties_**: A single package for character database with
scripts to update it based on unicode standard database updates.
2. **_unicode-transforms_**: A lightweight native Haskell package for basic unicode
string transforms (normalization, case folding etc.) based on unicode-chars.
Not a replacement for text-icu.
3. **_utf8-string_**: A single UTF8 bytestring package including a normalized
string representation like text-normal
4. **_text_**: Existing text package (UTF16 representation). Include normalized
text (text-normal) in the text package based on the native Haskell
unicode-transforms package

# Unicode resources

* [Unicode Character Database](http://www.unicode.org/Public/UCD/latest/ucd)
