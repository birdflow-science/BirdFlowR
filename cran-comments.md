## R CMD check results

0 errors | 0 warnings | 1 note

* This is the first submission of this package to CRAN.

* checking for future file timestamps ... NOTE
  unable to verify current time
  This NOTE is environmental (the time-verification service was
  unreachable from the check machine) and unrelated to the package.

## Bioconductor dependencies

* `rhdf5` is a hard dependency (`Imports`), used to read and write the
  package's native HDF5 model-serialization format. It is not
  available on CRAN; it is available on Bioconductor.
* `SparseArray` is an optional dependency (`Suggests`), used only to
  speed up a few sparse-matrix operations when installed. Every call
  site guards it with `requireNamespace("SparseArray", quietly = TRUE)`
  and falls back to base/Matrix equivalents otherwise, so the package
  is fully functional without it.

## Test environments

* local macOS, R 4.5.2
* GitHub Actions (`.github/workflows/R-CMD-check.yaml`), on push and
  pull request:
  * macOS-latest, R release
  * Windows-latest, R release
  * Ubuntu-latest, R release
  * Ubuntu-latest, R devel

## Downstream dependencies

There are currently no downstream dependencies for this package, as
this is its first release.
