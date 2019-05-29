# Comparing benchmarks for regression

Build and run the benchmark executable with `--csvraw=results.csv` option for
the baseline code i.e. without the changes.

```
$ cabal v2-run bench -- --csvraw=results.csv unicode-transforms
```

Now repeat the above step for the new code i.e. with the changes.
It will append the benchmarks results to the existing csv file.

If you want the benchmarks to run quickly for a quick overview then use
`--quick` option when running the benchmarking executable.

Build the bench-show reporting executable `chart`.

```
# [NOTE] The path "results.csv" is harcoded in the chart executable.
$ cabal v2-run chart
```
