# Comparing benchmarks for regression

The following commands are tested with `cabal` version 3.0.

Run the benchmarks for the baseline code i.e. without the changes:

```
# Remove any old benchmark results file first
$ rm results.csv
$ cabal run bench -- --csvraw=results.csv --quick
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

# On Debian Linux distributions
$ sudo apt-get install icu
```

If cabal cannot automatically find the icu library (e.g. when installed
via macports install) then use explicit `LIBRARY_PATH` to tell it where
the library is:

```
$ export LIBRARY_PATH=/usr/lib/:opt/local/lib
```

Remove any old `results.csv` and run benchmarks with `has-icu` flag enabled:

```
$ rm results.csv
$ cabal run bench --flag has-icu -- --csvraw=results.csv --quick
```

The following command will now show the comparison between `text-icu` and
`unicode-transforms`:

```
$ cabal run chart --flag bench-show
```
