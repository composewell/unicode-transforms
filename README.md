# Unicode Transforms

[![Hackage](https://img.shields.io/hackage/v/unicode-transforms.svg?style=flat)](https://hackage.haskell.org/package/unicode-transforms)
[![Build Status](https://travis-ci.org/composewell/unicode-transforms.svg?branch=master)](https://travis-ci.org/composewell/unicode-transforms)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/5wov8m1m0asvbv32?svg=true)](https://ci.appveyor.com/project/harendra-kumar/unicode-transforms)
[![Coverage Status](https://coveralls.io/repos/composewell/unicode-transforms/badge.svg?branch=master&service=github)](https://coveralls.io/github/composewell/unicode-transforms?branch=master)

Fast Unicode 12.1.0 normalization in Haskell (NFC, NFKC, NFD, NFKD).

## What is normalization?

Unicode characters with adornments (e.g. Á) can be represented in two different
forms, as a single composed character (U+00C1 = Á) or as multiple decomposed
characters (U+0041(A) U+0301( ́ ) = Á). They are differently encoded byte
sequences but for humans they have exactly the same visual appearance.

A regular byte comparison may tell that two strings are different even though
they might be equivalent. We need to convert both the strings in a
[`normalized`](http://unicode.org/reports/tr15/) form using the [Unicode
Character Database](http://www.unicode.org/Public/UCD/latest/) before we can
compare them for equivalence. For example:
```
>> import Data.Text.Normalize
>> normalize NFC "\193" == normalize NFC "\65\769"
True
```

## Contributing
Please use https://github.com/harendra-kumar/unicode-transforms to raise
issues, or send pull requests.
