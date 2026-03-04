# Functions to convert among spatial indices

These functions convert among different ways of referencing locations in
BirdFlow models and output. They convert among the spatial coordinates
(x and y), raster row and col indices, and the index `i` along a
location vector. `latlon_to_xy()` and `xy_to_latlon()` convert between
WGS 1984 latitude and longitude and x and y coordinates in a BirdFlow
object's coordinate reference system (CRS).

## Usage

``` r
x_to_col(x, bf)

y_to_row(y, bf)

row_to_y(row, bf)

col_to_x(col, bf)

i_to_rc(i, bf)

i_to_row(i, bf)

i_to_col(i, bf)

i_to_x(i, bf)

i_to_y(i, bf)

i_to_xy(i, bf)

rc_to_i(row, col, bf)

xy_to_i(x, y, bf)

latlon_to_xy(lat, lon, bf)

xy_to_latlon(x, y, bf)
```

## Arguments

- x, y:

  x and y coordinates in the BirdFlow model's CRS. This typically
  represents an easting and northing in meters. For functions that
  require both `x` and `y` a two column matrix or data.frame containing
  `x` and `y` columns in that order can be passed to `x` in which case
  `y` should be omitted.

- bf:

  A BirdFlow model.

- row, col:

  The row and column index of a cell in the BirdFlow model and
  associated raster data. Alternatively, a two column matrix or
  data.frame containing row and column indices in columns 1 and 2
  respectively can be passed to `row` in which case the `col` argument
  should be omitted.

- i:

  The index along a state vector that contains the data for unmasked
  cells.

- lat, lon:

  The latitude and longitude in WGS 1984 (EPSG:4326). A two column
  matrix or data frame can also be passed to `lat`.

## Value

- `x_to_col(x, bf)` and `y_to_row(y, bf)`:

  Return the column or row index that each `x` or `y` coordinate falls
  within.

- `row_to_y(row, bf)` and `col_to_x(col, bf)`:

  Return the y or x coordinate of the center each row or column.

- `i_to_row(i, bf)` and `i_to_col(i, bf)`:

  Return the row or column index corresponding to the vector state
  index, `i`.

- `i_to_rc(i, bf)`:

  Returns a two column matrix of the row and column index in the raster
  corresponding to the index, `i` of the vector state.

- `i_to_x(i, bf)` and `i_to_y(i, bf)`:

  Return the x or y coordinate from the vector state index, `i`.

- `i_to_xy(i, bf)`:

  Returns a two column matrix of the x and y coordinates corresponding
  to the index, `i` of the vector state space.

- `rc_to_i(row, col, bf)` and `xy_to_i(x, y, bf)`:

  Return the state space index corresponding to x and y coordinates or
  row and column indices.

&nbsp;

- `latlon_to_xy(lat, lon, bf)`:

  Returns a two column matrix of the x and y coordinates corresponding
  to the supplied latitude and longitude. The output is in the CRS of
  `bf` (`crs(bf)`).

- `xy_to_latlon(x, y, bf)`:

  Returns a two column matrix of the latitude and longitude of points in
  WGS84 given their coordinates in the BirdFlow object's CRS.

## Details

In general, these functions mirror NA input in output; and functions
that return rows, columns, or the index (`i`) will return NA if the
input isn't within the extent; or for (`i`) within the active cells.

For most CRSs longitude translates to the x coordinate and latitude to
the y coordinate. The traditional order of coordinates latitude and
longitude; row and column, and x and y are not consistent, but that is
still what is used here, i.e. latitude, row, and y are generally
correlated, and are first, first, and second in argument order.

## See also

- [`expand_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/expand_distr.md)
  converts a vector distribution into it's raster (matrix) equivalent or
  a matrix (representing multiple distributions) into an array
  equivalent.

- [`rasterize_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/rasterize.md)
  converts a vector distribution into a
  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html) -
  similar to those created by
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html).

- [`dmi_to_i()`](https://birdflow-science.github.io/BirdFlowR/reference/dynamic_mask_index_conversions.md)
  and
  [`i_to_dmi()`](https://birdflow-science.github.io/BirdFlowR/reference/dynamic_mask_index_conversions.md)
  convert between location indices (`i`) along cells included by the
  static mask and dynamic mask indices (`dmi`) along the cells included
  in the dynamic mask for a given timestep.
