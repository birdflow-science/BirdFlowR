# Read geom component of a BirdFlow hdf5

This internal function is called by
[`import_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/export_import_birdflow.md)
and
[`extend_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/extend_birdflow.md)
to read and format the geom component of the model

## Usage

``` r
read_geom(hdf5)
```

## Arguments

- hdf5:

  the path to an hdf5 file

## Value

The geom component of a birdflow model
