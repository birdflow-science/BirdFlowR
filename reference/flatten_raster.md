# Convert a raster bird distribution into its flattened, vector equivalent

This function converts between a raster representation of data in which
rows and columns indicate position in space and a vector representation
that contains only the active (not masked) cells in row major order -
starting at the top left and proceeding left to right along each row.
The collapsed form is used for projecting the flow model while the
expanded form is used to export, import, and visualize the data.

## Usage

``` r
flatten_raster(x, bf)
```

## Arguments

- x:

  Either a matrix representing a single bird distribution or an array
  representing multiple distributions with dimensions: row, col, and
  distribution.

- bf:

  A `BirdFlow` model.

## Value

Either a vector representing a single distribution in its collapsed form
or, if `x` represents multiple distributions, a matrix with one
distribution per column.

## See also

- [`expand_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/expand_distr.md)
  does the opposite of `flatten_raster()`.

- [`rasterize_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/rasterize.md)
  has replaced this function in the public API. With
  `format = "numeric"` it returns an identical object to
  [`expand_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/expand_distr.md)
  but by default (\`format = "SpatRaster" it goes one step further and
  adds spatial metadata to make a
  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html).

- [index_conversions](https://birdflow-science.github.io/BirdFlowR/reference/index_conversions.md)
  for ways to convert among indexes of the data in raster row and
  column, index along the flattened vector, and Cartesian space.
