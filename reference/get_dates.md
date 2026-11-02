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
  [truncation](https://birdflow-science.github.io/BirdFlowR/reference/truncate_birdflow())
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

## Examples

``` r
# \donttest{
bf <- BirdFlowExamples::amewoo
get_dates(bf)
#>    timestep       date        label julian week
#> 1         1 2023-01-04    January 4      4    1
#> 2         2 2023-01-11   January 11     11    2
#> 3         3 2023-01-18   January 18     18    3
#> 4         4 2023-01-25   January 25     25    4
#> 5         5 2023-02-01   February 1     32    5
#> 6         6 2023-02-08   February 8     39    6
#> 7         7 2023-02-15  February 15     46    7
#> 8         8 2023-02-22  February 22     53    8
#> 9         9 2023-03-01      March 1     60    9
#> 10       10 2023-03-08      March 8     67   10
#> 11       11 2023-03-15     March 15     74   11
#> 12       12 2023-03-22     March 22     81   12
#> 13       13 2023-03-29     March 29     88   13
#> 14       14 2023-04-05      April 5     95   14
#> 15       15 2023-04-12     April 12    102   15
#> 16       16 2023-04-19     April 19    109   16
#> 17       17 2023-04-26     April 26    116   17
#> 18       18 2023-05-03        May 3    123   18
#> 19       19 2023-05-10       May 10    130   19
#> 20       20 2023-05-17       May 17    137   20
#> 21       21 2023-05-24       May 24    144   21
#> 22       22 2023-05-31       May 31    151   22
#> 23       23 2023-06-07       June 7    158   23
#> 24       24 2023-06-14      June 14    165   24
#> 25       25 2023-06-21      June 21    172   25
#> 26       26 2023-06-28      June 28    179   26
#> 27       27 2023-07-05       July 5    186   27
#> 28       28 2023-07-12      July 12    193   28
#> 29       29 2023-07-19      July 19    200   29
#> 30       30 2023-07-26      July 26    207   30
#> 31       31 2023-08-02     August 2    214   31
#> 32       32 2023-08-09     August 9    221   32
#> 33       33 2023-08-16    August 16    228   33
#> 34       34 2023-08-23    August 23    235   34
#> 35       35 2023-08-30    August 30    242   35
#> 36       36 2023-09-06  September 6    249   36
#> 37       37 2023-09-13 September 13    256   37
#> 38       38 2023-09-20 September 20    263   38
#> 39       39 2023-09-27 September 27    270   39
#> 40       40 2023-10-04    October 4    277   40
#> 41       41 2023-10-11   October 11    284   41
#> 42       42 2023-10-18   October 18    291   42
#> 43       43 2023-10-25   October 25    298   43
#> 44       44 2023-11-01   November 1    305   44
#> 45       45 2023-11-08   November 8    312   45
#> 46       46 2023-11-15  November 15    319   46
#> 47       47 2023-11-22  November 22    326   47
#> 48       48 2023-11-29  November 29    333   48
#> 49       49 2023-12-06   December 6    340   49
#> 50       50 2023-12-13  December 13    347   50
#> 51       51 2023-12-20  December 20    354   51
#> 52       52 2023-12-27  December 27    361   52
# }
```
