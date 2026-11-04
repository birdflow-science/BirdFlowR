test_that("get_ebird_coverage returns a coverage SpatRaster", {
  skip_if_not_installed("BirdFlowExamples")
  bf <- BirdFlowExamples::amewoo
  expect_no_error(result <- get_ebird_coverage(bf))
  expect_s4_class(result, "SpatRaster")
})

test_that("get_ebird_coverage returns SpatRaster by default", {
  bf <- small_test_bf()
  cov <- get_ebird_coverage(bf)
  expect_s4_class(cov, "SpatRaster")
  expect_equal(terra::nlyr(cov), ncol(bf$distr))
  expect_equal(terra::nrow(cov), nrow(bf))
  expect_equal(terra::ncol(cov), ncol(bf))
})

test_that("get_ebird_coverage SpatRaster values match array", {
  bf <- small_test_bf()
  arr <- bf$metadata$ebird_coverage
  r <- get_ebird_coverage(bf)
  # terra::values() returns layers column-by-column; the mask matrix is
  # row-major in R so compare via t() as in test-get_mask.R
  for (t in seq_len(dim(arr)[3])) {
    expect_equal(as.vector(terra::values(r[[t]])),
                 as.logical(t(arr[, , t])))
  }
})

test_that("get_ebird_coverage returns array for format='array'", {
  bf <- small_test_bf()
  raw <- bf$metadata$ebird_coverage
  arr <- get_ebird_coverage(bf, format = "array")
  expect_true(is.array(arr))
  expect_equal(dim(arr), dim(raw))
  expect_type(arr, "logical")
  # Values are unchanged by reformatting, only the "time" dimnames change
  expect_equal(arr, raw, ignore_attr = "dimnames")
})

test_that("get_ebird_coverage returns correct dataframe", {
  bf <- small_test_bf()
  df <- get_ebird_coverage(bf, format = "dataframe")
  expect_s3_class(df, "data.frame")
  expect_named(df, c("row", "col", "x", "y", "i", "timestep", "label",
                     "coverage"))
  n_timesteps <- dim(bf$metadata$ebird_coverage)[3]
  expect_equal(nrow(df), nrow(bf) * ncol(bf) * n_timesteps)
  # timestep is always canonical ("t1", "t2", ...) regardless of time_format
  expect_setequal(unique(df$timestep), paste0("t", seq_len(n_timesteps)))
  # Inactive cells have NA i
  n_inactive <- sum(!bf$geom$mask)
  expect_true(any(is.na(df$i)))
  expect_equal(sum(is.na(df$i)), n_inactive * n_timesteps)
})

test_that("get_ebird_coverage dataframe label respects time_format", {
  bf <- small_test_bf()

  o_time_format <- birdflow_options("time_format")
  on.exit(birdflow_options(time_format = o_time_format))

  birdflow_options(time_format = "timestep")
  df_t <- get_ebird_coverage(bf, format = "dataframe")
  expect_equal(df_t$label, df_t$timestep)

  birdflow_options(time_format = "month_day")
  df_md <- get_ebird_coverage(bf, format = "dataframe")
  dates <- get_dates(bf)
  expect_equal(df_md$label,
               dates$label[match(df_md$timestep, paste0("t", dates$timestep))])

  birdflow_options(time_format = "date")
  df_date <- get_ebird_coverage(bf, format = "dataframe")
  expect_equal(df_date$label,
              as.character(dates$date[match(df_date$timestep,
                                            paste0("t", dates$timestep))]))

  # timestep and underlying values are unaffected by time_format
  expect_equal(df_t$timestep, df_md$timestep)
  expect_equal(df_t$coverage, df_md$coverage)
  expect_equal(df_t$timestep, df_date$timestep)
  expect_equal(df_t$coverage, df_date$coverage)
})

test_that("get_ebird_coverage respects time_format", {
  bf <- small_test_bf()

  o_time_format <- birdflow_options("time_format")
  on.exit(birdflow_options(time_format = o_time_format))

  birdflow_options(time_format = "timestep")
  cov_t <- get_ebird_coverage(bf)
  expect_equal(names(cov_t), paste0("t", seq_len(ncol(bf$distr))))

  birdflow_options(time_format = "month_day")
  cov_md <- get_ebird_coverage(bf)
  expect_equal(names(cov_md), get_dates(bf)$label)

  birdflow_options(time_format = "date")
  cov_date <- get_ebird_coverage(bf)
  expect_equal(names(cov_date), as.character(get_dates(bf)$date))

  # Reformatting only changes labels, not the underlying values
  expect_equal(terra::values(cov_t), terra::values(cov_md),
               ignore_attr = "dimnames")
  expect_equal(terra::values(cov_t), terra::values(cov_date),
               ignore_attr = "dimnames")
})

test_that("get_mask still works after refactor", {
  skip_if_not_installed("BirdFlowExamples")
  bf <- BirdFlowExamples::amewoo

  r <- get_mask(bf)
  expect_s4_class(r, "SpatRaster")
  expect_equal(names(r), "mask")
  expect_equal(as.vector(terra::values(r)), as.logical(t(bf$geom$mask)))

  m <- get_mask(bf, format = "numeric")
  expect_equal(m, bf$geom$mask)

  df <- get_mask(bf, format = "dataframe")
  expect_named(df, c("row", "col", "x", "y", "i", "mask"))
  expect_equal(sort(unique(df$i)), seq_len(n_active(bf)))
  expect_equal(sum(df$mask), n_active(bf))
})
