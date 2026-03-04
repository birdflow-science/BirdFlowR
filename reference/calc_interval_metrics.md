# Calculate interval metrics

Calculate interval‐based validation metrics—including distance,
likelihood, and energy‐score metrics—for all transition pairs in a
BirdFlowIntervals object.

## Usage

``` r
calc_interval_metrics(birdflow_intervals, bf)
```

## Arguments

- birdflow_intervals:

  A `BirdFlowIntervals` object containing transition data

- bf:

  A fitted `BirdFlow` model

## Value

A list with two elements:

- metrics:

  A named numeric vector of summary metrics across all intervals:

  mean_pred

  :   Mean weighted average distance (km) from BF predictions

  mean_st

  :   Mean weighted average distance (km) from S&T distributions

  mean_win_prob

  :   Mean win probability (BF vs. S&T)

  mean_win_distance

  :   Mean absolute distance improvement (km)

  mean_win_distance_fraction

  :   Mean normalized distance improvement

  mean_global_prob_of_the_starting

  :   Mean relative abundance at start cells

  mean_elapsed_days

  :   Mean elapsed days per interval

  mean_elapsed_km

  :   Mean observed great‐circle distance (km)

  mean_null_ll

  :   Mean log‐likelihood under the S&T null distribution

  mean_ll

  :   Mean log‐likelihood under the BF prediction

  mean_energy_score_bf

  :   Mean energy score of BF predictions

  mean_energy_score_st

  :   Mean energy score of S&T distributions

  mean_energy_improvement

  :   Mean difference in energy score

  mean_pred_elapsed_dist_by_pred

  :   Mean predicted elapsed distance by BF

  mean_pred_elapsed_dist_by_st

  :   Mean predicted elapsed distance by S&T

  weighted_mean_win_prob

  :   Global‐abundance‐weighted mean win probability

  weighted_mean_win_distance

  :   Global‐abundance‐weighted mean win distance

  weighted_mean_win_distance_fraction

  :   Global‐abundance‐weighted mean distance fraction

  weighted_mean_null_ll

  :   Global‐abundance‐weighted mean null log‐likelihood

  weighted_mean_ll

  :   Global‐abundance‐weighted mean log‐likelihood

  weighted_energy_improvement

  :   Global‐abundance‐weighted mean energy improvement

  n_intervals

  :   Number of transition pairs evaluated

- per_interval:

  A `data.frame` of the raw, per‐transition metrics (same fields as
  above without the “mean\_” prefix)

## Examples

``` r
route_df <- data.frame(
route_id = c("001", "001", "001", "001", "001", "003", "003", "003", "004"),
date = as.Date(c("2025-01-01", "2025-01-08", "2025-01-15", "2025-01-21",
                "2025-02-10", "2025-03-01", "2025-05-01", "2025-06-01",
                "2025-05-01")),
lon = c(-75.0060, -75.0060, -74.0060, -87.6298, -87.6298, -87.6298,
        -89.6298, -85.6298, -95.3698),
lat = c(39.7128, 39.7128, 40.7128, 41.8781, 41.8781, 41.8781,
        42.8781, 40.8781, 29.7604),
route_type = c("tracking", "tracking", "tracking", "tracking",
               "tracking", "motus", "motus", "motus", "motus")
)

bf <- BirdFlowModels::amewoo
species1 <- bf$species
source1 <- "Testing"

my_routes <- Routes(route_df,
                    species = species1,
                    source = source1
)
my_bfroutes <- as_BirdFlowRoutes(my_routes, bf = bf)

# Constraints
min_day <- 7
max_day <- 180
min_km <- 200
max_km <- 8000

my_intervals <- as_BirdFlowIntervals(my_bfroutes,
                                     max_n = 1000,
                                     min_day_interval = min_day,
                                     max_day_interval = max_day,
                                     min_km_interval = min_km,
                                     max_km_interval = max_km
)

eval_res <- calc_interval_metrics(my_intervals, bf)
single_value_outputs <- eval_res[[1]]
transition_level_outputs <- eval_res[[2]]
```
