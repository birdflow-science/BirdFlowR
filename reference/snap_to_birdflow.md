# Assign cells and timesteps to coordinates and dates

This function aligns bird movement data that has both location and date
information with a BirdFlow model's cells and timesteps. Optionally,it
will aggregate multiple points from a track within the same week into a
single point and date prior to determining the location ID.

## Usage

``` r
snap_to_birdflow(
  d,
  bf,
  x_col = "lon",
  y_col = "lat",
  date_col = "date",
  id_cols = "id",
  crs = "EPSG:4326",
  aggregate = NULL
)
```

## Arguments

- d:

  A data frame with bird movement data it must have the columns
  specified by `x_col`, `y_col`, `date_col`, and `id_cols`. The default
  values for the first three are `"lon"`, `"lat"`, and`"date"`.

- bf:

  A BirdFlow model

- x_col:

  The name of the column that holds x or longitude coordinates. Default
  is `"lon"`.

- y_col:

  The name of the y or latitude column, default is `"lat"`.

- date_col:

  Name of the date column, defaults to `"date"`

- id_cols:

  The name of identification columns. One or more columns that
  collectively define a unique track. If `aggregate` is not `NULL` these
  along with `timestep` (calculated from the `date_col`) are used to
  define the groups. If aggregate is `NULL` these columns are not used
  but are retained in the output.

- crs:

  The coordinate reference system used by `x_col` and `y_col`. The
  default `"EPSG:4326"` corresponds to [WGS 1984](https://epsg.io/4326)

- aggregate:

  Leave the default `NULL` for no aggregation - each row in `d` is
  processed separately and represented in the returned object. If
  `aggregate` is set to one of the values below then the locations
  (rows) in `d` that fall in the same week and track (see `id_cols`)
  will be aggregated together such that there is only one location per
  week for each track.

  The possible values for `aggregate` are:

  - `mean` The mean of the "x", "y", and "date" columns are used for the
    week.

  - `median` The median of the "x", "y", and "date" columns are used for
    the week.

  - `midweek` The observation that is closest to the middle of the week
    is used to represent the week. With ties the observation that occurs
    first is used.

  - `random` One observation is randomly selected for each week.

  Pending ideas, not yet implemented:

  - `gmedian` [geometric
    median](https://cran.r-project.org/web/packages/Gmedian/index.html)

  - `central` The point closest to the centroid of all the points is
    used to represent the week.

## Value

A data frame with columns

- id_cols:

  The columns identified with `id_cols` will be retained and be
  leftmost.

- date:

  This column will have information from `date_col` but not retain its
  name or original formatting. If aggregate is NULL the input dates will
  be retained, if not the date will vary with the aggregation method but
  will represent the dates that went into the summary and not the
  mid-week date associated with `timestep`. If the input `date_col` is a
  date-time class then the output date column will be as well. For
  example with `aggregate = "mean"` the date will be the average date of
  the points in the week.

- timestep:

  The model timestep associated with `date`.

- x, y:

  The x and y coordinates of the point. These will always be in
  `crs(bf)` and will represent the original or aggregated location and
  not the cell center.

- i:

  The location index of the cell associated with the `x` and `y` in
  `bf`. See
  [`i_to_xy()`](https://birdflow-science.github.io/BirdFlowR/reference/index_conversions.md).

- n:

  The number of rows in `d` that contributed to each output row. If
  aggregate is NULL every value will be 1.

- error:

  TRUE if there was an error.

- message:

  NA or the error message. The possible messages are:

  - "err_date" - The date could not be parsed with
    [`lubridate::as_date()`](https://lubridate.tidyverse.org/reference/as_date.html)

  - "err_truncated" - `bf` is
    [truncated](https://birdflow-science.github.io/BirdFlowR/reference/truncate_birdflow)
    and the date falls outside of portion of the year the model covers.

  - `"err_coords"` - The coordinates could not be transformed into
    `crs(bf)` and thus likely are corrupt in some way.

  - `"err_not_active"` - the location does not fall within an active
    cell as defined by the static mask.

  - `"err_dynamic"` - the location does not fall within the dynamic mask
    on the associated date.

  - `"err_sparse"` - the location falls within the dynamic mask but that
    location and date combination has been eliminated by
    [sparsification](https://birdflow-science.github.io/BirdFlowR/reference/sparsify).

  The function will always return the error message that appears first
  on this list, even though in some cases multiple errors can be
  triggered.

  With aggregation the first three errors prevent a row from being used
  and it will be dropped prior to aggregation with a warning.

## Details

If `aggregate` is NULL than than each row in the output will correspond
with the same row in the input. With aggregation rows can be lost in two
ways (1) if the coordinates or timestep can't be resolved (first three
errors on list) then the row is dropped prior to aggregation. (2) via
aggregation all the rows within a week for a track will be collapsed to
one. These aggregated locations can still trigger the other three errors
on the list.

A location id will only be assigned if the location is a valid location
for the model on the associated date.

## Examples

``` r
bf <- BirdFlowModels::rewbla |> add_dynamic_mask()
obs <- BirdFlowModels::rewbla_observations
a <- snap_to_birdflow(obs, bf, id_cols = "bird_id")
```
