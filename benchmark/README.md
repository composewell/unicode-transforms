# Benchmark library choice

You may choose between two libraries to perform the benchmarks:

- `tasty-bench`: default
- `gauge`: alternative, requires cabal flag `use-gauge`.

# Comparing benchmarks for regression

The following commands are tested with `cabal` version 3.0.

## `tasty-bench`

Run the benchmarks for the baseline code i.e. without the changes:

```
# Remove any old benchmark results file first
$ rm results.csv
$ cabal run bench -- --csv=old.csv
```

It will collect the benchmark results in `old.csv` file.

If you want more accurate benchmark results you can add `--stdev 1`
(default: 5).

You may generate a SVG chart by adding `--svg chart.svg`.

Modify the code and then run:

```
$ cabal run bench -- --csv=new.csv --baseline=old.csv
```

It will collect the new benchmark results in `new.csv` file.

To generate a benchmark comparison in CSV between old and new changes from the
benchmark results:

```
# Source: see “Comparison against baseline” in tasty-bench documentation.
awk 'BEGIN{FS=",";OFS=",";print "Name,Old,New,Ratio"}FNR==1{next}FNR==NR{a[$1]=$2;next}{print $1,a[$1],$2,$2/a[$1];gs+=log($2/a[$1]);gc++}END{print "Geometric mean,,",exp(gs/gc)}' old.csv new.csv
```

## `gauge`

Run the benchmarks for the baseline code i.e. without the changes:

```
# Remove any old benchmark results file first
$ rm results.csv
$ cabal run bench --flag use-gauge -- --csvraw=results.csv --quick
```

It will collect the benchmark results in `results.csv` file.

Now repeat the above step for the new code i.e. with the changes.
It will append the new benchmarks results to the existing `results.csv` file.

If you want more accurate benchmark results remove the `--quick` flag.

To generate a benchmark comparison between old and new changes from the
benchmark results in `results.csv` file:

```
# [NOTE] The path "results.csv" is harcoded in the chart executable.
$ cabal run chart --flag bench-show
```

# Comparing with ICU (text-icu package)

Install the icu library:

```
# On Mac OS using MacPorts
$ sudo port install icu

# On Mac OS using brew
$ brew install icu4c

# On Debian Linux based distributions (the library version suffix may differ)
$ sudo apt-get install libicu65
```

If cabal cannot automatically find the icu library (e.g. when installed
via macports install) then use explicit `LIBRARY_PATH` to tell it where
the library is:

```
$ export LIBRARY_PATH=/usr/lib/:/opt/local/lib

# Alternatively, pass the lib and include path as follows
$ cabal bench --extra-lib-dirs=/usr/local/opt/icu4c/lib --extra-include-dirs=/usr/local/opt/icu4c/include
```

## `tasty-bench`

Remove any old `results.csv` and run benchmarks with `has-icu` flag enabled:

```
$ rm results.csv
$ cabal run bench --flag has-icu -- --csv=results.csv
```

## `gauge`

Remove any old `results.csv` and run benchmarks with `has-icu` flag enabled:

```
$ rm results.csv
$ cabal run bench -f use-gauge -f has-icu -- --csvraw=results.csv --quick
```

The following command will now show the comparison between `text-icu` and
`unicode-transforms`:

```
$ cabal run chart --flag bench-show
```
