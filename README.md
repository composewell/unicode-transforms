# Unicode Transforms
This is a lightweight library supporting a limited set of unicode
transformations on `ByteStrings` (UTF-8) and `Text` (UTF-16).
without requiring any other system libraries.

This package is based on the `utf8proc` C utility. The utf8proc version bundled
with this package is taken from the
[xqilla project](http://xqilla.sourceforge.net/HomePage)
(xqilla version 2.3.2).

# Comparison with `text-icu`
This package provides a subset of the functionality provided by the full
featured `text-icu` package. It can be chosen over `text-icu` when you do
not need the extra functionality and more importantly do not want a
dependency on the `icu` libraries.

# Features & Limitations
Following is a list of major features and limitations of `unicode-transforms`.

 Feature               | Limitations
 -------               | -----------
 Unicode normalization | -
 Case folding          | No option for special handling of `i` in Turkish
 Case conversion       | No locale specific handling
 Boundary Analysis     | No locale specific handling

Additional features include:
* NLF sequence conversion
* Stripping certain character classes
* Lumping certain characters

The following features provided by `text-icu` are missing in this package:
* Normalization checks
* FCD normalization for collation
* String collation
* Iteration
* Regular expressions

Note: Current version exports only normalization operation. But other
operations will be added soon.
