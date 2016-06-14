# Unicode Transforms
This is a lightweight Haskell library supporting commonly used unicode
transformations (currently only normalizations) on `ByteStrings` (UTF-8) and
`Text`.

Haskell package `text-icu` provides a comprehensive set of unicode transforms.
The drawback of `text-icu` is that it requires you to install the ICU library
OS packages first. This package is self contained and aims to provide an API
similar to `text-icu` so that it can be used as a drop-in replacement for the
features it supports.

## Features
Unicode normalization in NFC, NFKC, NFD, NFKD forms is supported. This version
of the library supports unicode versions upto 5.1.0.

## Documentation
Please see the haddock documentation available with the package.

## Implementation

This package is implemented as bindings to the `utf8proc` C utility. The
utf8proc version bundled with this package is taken from the [xqilla
project](http://xqilla.sourceforge.net/HomePage) (xqilla version 2.3.2).

In future the underlying `utf8proc` implementation will get replaced by a
Haskell implementation supporting the latest unicode versions.

## Related stuff
Please see the NOTES.md file shipped with this package for more details on
related packages, missing features and todo etc.

## Contributing
Contributions are welcome! Please use the github repository at
https://github.com/harendra-kumar/unicode-transforms to raise issues, request
features or send pull requests.
