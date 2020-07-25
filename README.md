# Unicode Transforms

[![Hackage](https://img.shields.io/hackage/v/unicode-transforms.svg?style=flat)](https://hackage.haskell.org/package/unicode-transforms)
[![Build Status](https://travis-ci.com/composewell/unicode-transforms.svg?branch=master)](https://travis-ci.com/composewell/unicode-transforms)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/5wov8m1m0asvbv32?svg=true)](https://ci.appveyor.com/project/harendra-kumar/unicode-transforms)
[![Coverage Status](https://coveralls.io/repos/composewell/unicode-transforms/badge.svg?branch=master&service=github)](https://coveralls.io/github/composewell/unicode-transforms?branch=master)

Fast Unicode 13.0.0 normalization in Haskell (NFC, NFKC, NFD, NFKD).

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

## Performance

Normalization performance comparison of this package (v0.3.7) with
the [text-icu](http://hackage.haskell.org/package/text-icu) package
using the [ICU C++ library](http://site.icu-project.org/download)
version ICU4C 65.1 on macOS. The benchmarks compare the time taken in
milliseconds to normalize files in different languages and normalization
forms using both the packages. In most cases `unicode-transforms`
outperforms ICU.

```
Benchmark       unicode-transforms(ms) ICU(ms)    % Diff
--------------- ---------------------- -------   --------
NFKD/Korean                       7.78   37.10    +376.87
NFD/Korean                        7.86   37.06    +371.50
NFKD/Vietnamese                   6.85   12.48     +82.20
NFKD/Deutsch                      2.17    3.55     +63.30
NFKD/English                      1.71    2.78     +62.30
NFKC/Korean                       4.77    7.65     +60.28
NFD/Deutsch                       2.24    3.53     +57.41
NFD/English                       1.76    2.77     +57.32
NFC/Vietnamese                   10.66   16.63     +56.00
NFKC/Vietnamese                  10.95   16.58     +51.43
NFD/Devanagari                    6.48    8.68     +34.10
NFC/Devanagari                    6.77    8.49     +25.48
NFD/AllChars                      6.18    7.41     +19.91
NFD/Japanese                      7.80    9.20     +17.99
NFKC/Devanagari                   7.33    8.48     +15.74
NFKD/Japanese                     8.71   10.05     +15.39
NFD/Vietnamese                    5.94    6.83     +14.99
NFKD/Devanagari                   7.59    8.68     +14.27
NFKD/AllChars                     9.80   10.66      +8.82
NFKC/Deutsch                      3.21    3.18      -0.72
NFC/Korean                        4.62    4.38      -5.35
NFKC/English                      2.21    2.06      -6.88
NFC/English                       2.19    2.04      -7.21
NFKC/AllChars                    14.67    9.75     -50.51
NFC/Deutsch                       3.02    1.95     -54.39
NFKC/Japanese                    12.46    5.42    -129.93
NFC/AllChars                      9.72    3.58    -171.63
NFC/Japanese                     11.90    3.04    -292.04
```

## Talks

* Talks: [Functional Conf 2018 Video](https://www.youtube.com/watch?v=aJvwORrBJ0o) | [Functional Conf 2018 Slides](https://www.slideshare.net/HarendraKumar10/high-performance-haskell)

## Contributing
Please use https://github.com/harendra-kumar/unicode-transforms to raise
issues, or send pull requests.
