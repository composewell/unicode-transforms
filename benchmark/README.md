# Comparing benchmarks for regression

Build and run the benchmark executable with `--csvraw=results.csv` option for
the baseline code i.e. without the changes.

```
# Stop when it starts running. Note the path of the executable from the build
# output.
$ cabal v2-bench

# Run the benchmark executable manually using the full path from the above
# step.
$ bench --csvraw=results.csv --append unicode-transforms
```

Now repeat the above step for the new code i.e. with the changes, make to use
the `--append` option to append the benchmarks results to the existing csv
file. If you want the benchmarks to run quickly for a quick overview then use
`--quick` option when running the benchmarking executable.

Build the bench-show reporting executable `chart`.

```
# Note the path of the executable from the build output.
$ cabal new-build --flag bench-show

# Run the reports executable from the first step above, the path "results.csv"
# is harcoded in the chart executable
$ chart
```
