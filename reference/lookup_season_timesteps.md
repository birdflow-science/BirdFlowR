# Lookup breeding, non-breeding, or migration season timesteps

Retrieve the timesteps associated with a season for the species modeled
by a BirdFlow object, possibly with a buffer (in timesteps) added on.
Seasons dates are from
[ebirdst::ebirdst_runs](https://ebird.github.io/ebirdst/reference/ebirdst_runs.html)
and are directly accessible with
[`species_info()`](https://birdflow-science.github.io/BirdFlowR/reference/species_info.md).

## Usage

``` r
lookup_season_timesteps(x, season, season_buffer = 1)
```

## Arguments

- x:

  a BirdFlow object

- season:

  one of the seasons returned by
  [`species_info()`](https://birdflow-science.github.io/BirdFlowR/reference/species_info.md),
  a season alias, or or `"all"` for all timesteps in the model

- season_buffer:

  the number of extra timesteps to add to the beginning and end of the
  season.

## Value

a series of integers indicating which timesteps correspond with the
(possibly buffered) season.

## Season names and aliases

`season` can be `'all'`, one of the the four seasons, or an alias listed
below.

|                          |                                |
|--------------------------|--------------------------------|
| **season**               | **aliases**                    |
| `prebreeding_migration`  | `pre`, `prebreeding`, `spring` |
| `breeding`               | `breed`, `summer`              |
| `postbreeding_migration` | `post`, `postbreeding`, `fall` |
| `nonbreeding`            | `non`, `winter`                |

## Examples

``` r
bf <- BirdFlowModels::rewbla
lookup_season_timesteps(bf, "breeding", season_buffer = 0)
#>  [1] 18 19 20 21 22 23 24 25 26 27 28 29 30 31
```
