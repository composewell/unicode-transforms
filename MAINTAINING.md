# Maintainers' Guide

## Syncing unicode version

The test suite uses files included in the UCD. These files should be
synchronized with the version of unicode supported by `unicode-data`.

`download-ucd-files.sh` downloads the required test files. Replace the `VERSION`
with the current version supported by `unicode-data` and run this from the root
of the project.

```
$ ./download-ucd-files.sh
```

Additionally check for the unicode version in the documentation and make sure it
is eqvivalent to the current version supported by `unicode-data`.

## Merge Checklist

* Check if all the tests pass.
* Check and fix any regression in the benchmarks.
* Update the Changelog if required.

## Release Checklist

* Update the dependency version ranges where applicable.
* (Sync unicode version)[syncing-unicode-version].
* Bump the upper bound of `text` to the latest stable minor version.
* Make sure to manually run the test suites with "has-icu" flag enabled. It
  enables a test suite comparing normalized strings using `unicode-transforms`
  vs `libicu`.
* Check if all the tests pass.
* Check and fix any regression in the benchmarks.
* Update the benchmark results table in the README, if any changes impacting
  performance have been made.
* Update the latest Changelog accordingly.
