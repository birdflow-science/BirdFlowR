# Retrieve dates component of a BirdFlow model

`get_dates()` get date information for a BirdFlow model

## Usage

``` r
get_dates(bf)
```

## Arguments

- bf:

  A BirdFlow object to retrieve dates from.

## Value

A data frame with columns:

- timestep:

  The model timestep associated with each row. Will always equal the row
  number.

- date:

  The date associated with the midpoint of the timestep

- label:

  The month and day based label associated with the timestep Consistent
  with eBird's 2022 date scheme will be one day off the `date` after
  February on leap years but are still used to label timesteps. If the
  model was fit with an older version of ebirdst they will never be
  offset

- julian:

  The Julian date (day of year) associated with the timestep center

- week:

  The ebirdst week number associated with the date. For full year models
  this is identical to `timestep` but after
  [truncation](https://birdflow-science.github.io/BirdFlowR/reference/truncate_birdflow)
  they will differ.

Prior to BirdFlowR v. 0.1.0.9040 it returned columns:

- interval:

  The interval or timestep associated with each date. It will range from
  1 to
  [`n_timesteps()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md).
  With full models this is equivalent to the week of the year but with
  truncated models may not be.

- date:

  The date associated with the interval's midpoint.

- midpoint, start, end:

  The midpoint, start, and end of each interval as a proportion of the
  total year that has elapsed.

- doy:

  The day of year associated with the midpoint of each interval.

## Note

- [`truncate_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/truncate_birdflow.md)
  creates models with a subset of the intervals and renames them
  `1:[n_timesteps()]` of the truncated model so if it has been used
  `week` and `timestep` will not have identical values.

## Date scheme

In a non-leap year there are 52 weeks plus 1 day so assigning days to
weeks is a little bit arbitrary. BirdFlowR uses the same conventions as
ebirdst and eBird itself which changed with the eBird 2022 version year.
Thus for some dates the result of
[`lookup_timestep()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_timestep.md)
will change depending on the `ebird_version_year` associated with the
model.

### 2021 date scheme (ebirdst \<= 2.2021.3)

- Date table is derived from `ebirdst::ebirdst_weeks` and added to the
  model object by
  [`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md).

- Julian date is converted to a proportion with `(jd - 0.5)/366`.

- 52 even ranges are assigned to 0 to 1 and the proportional date is
  then compared to these thresholds.

- In a non-leap year two weeks have 8 days and the last week has 6.

- In a leap-year two weeks have 8 days.

- All other weeks have 7 days.

### 2022 date scheme (ebirdst \>= 3.2022.0)

- ebirdst dropped `ebirdst_weeks`

- Weeks are defined by evenly spaced Julian dates (of week center) which
  are always `seq(4, 366, 7)`.

- Dates are assigned to the week for which the Julian date is closest.

- On non-leap-years the last week has 8 days, on leap years it has 9.

- All weeks but the last have 7 days.

- Week labels (month and day) are assigned based on the month and day
  associated with the Julian week center on non-leap years regardless of
  whether it is a leap year. The label on leap years will thus be one
  day off the date of the week center for most of the year.

### Retrieving dates

The date columns stored within a birdflow model changes depending on the
ebirdst version year, so do NOT use `bf$dates` in your code. Instead
use`get_dates()` which will always return the same (newer) column names.
