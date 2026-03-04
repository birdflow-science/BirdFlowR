# Read and write routes and intervals

These functions provide support reading and writing routes and intervals
from and to HDF5 files while preserving class names and attributes.

## Usage

``` r
write_routes(obj, path)

# S3 method for class 'Routes'
write_routes(obj, path)

# S3 method for class 'BirdFlowRoutes'
write_routes(obj, path)

# Default S3 method
write_routes(obj, path)

read_routes(path)

read_intervals(path)

write_intervals(obj, path)
```

## Arguments

- obj:

  For `write_routes()` a `Routes`, or `BirdFlowRoutes` object. For
  `write_intevals()` a `BirdFlowIntervals` object.

- path:

  Path to write or read from, should end in `.hdf5`

## Value

- `write_routes()` ans `write_invervals()` invisibly return their input
  object (after writing to disk).

- `read_routes()` returns the written object - either `Routes` or
  `BirdFlowRoutes`.

- `read_intervals()` return the written `BirdFlowIntervals` object.

## Details

The Hierarchical Data Format version 5 (HDF5) is an open format to
efficiently store large, heterogeneous data sets. HDF5 files can be
written and read from R and Python among other languages so make a good
cross-language format for routes and intervals.

`read_routes()` and `write_routes()` work on both `Routes` and
`BirdFlowRoutes`. See
[`Routes()`](https://birdflow-science.github.io/BirdFlowR/reference/Routes.md)
and
[`as_BirdFlowRoutes()`](https://birdflow-science.github.io/BirdFlowR/reference/as_BirdFlowRoutes.md)
to create objects of these classes.

`read_intervals()` and `write_intervals()` work on `BirdFlowIntervals`
which are a collection of movements each with a single start and end
derived from
[`BirdFlowRoutes()`](https://birdflow-science.github.io/BirdFlowR/reference/Routes-internal.md).
See
[`as_BirdFlowIntervals()`](https://birdflow-science.github.io/BirdFlowR/reference/as_BirdFlowIntervals.md).

## Internal functions

- `write_r_object_h5()` and `read_r_object_h5()` are internal
  workhorses: they handle the recursive traversal of R lists (including
  data.frames, Date/POSIX/Factor vectors) and store both data and class
  attributes in the HDF5 file hierarchy.

## See also

- [rhdf5::h5createFile](https://rdrr.io/pkg/rhdf5/man/h5_createFile.html),
  [rhdf5::h5write](https://rdrr.io/pkg/rhdf5/man/h5_write.html),
  [rhdf5::h5read](https://rdrr.io/pkg/rhdf5/man/h5_read.html) for
  low-level HDF5 operations.

- [`import_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/export_import_birdflow.md),
  and
  [`export_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/export_import_birdflow.md)
  for reading and writing BirdFlow models. Note
  [`import_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/export_import_birdflow.md)
  also substantially alters the object if it has not previously been
  imported into R.
