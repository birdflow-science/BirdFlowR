
### Back compatibility
### `make_coverage_bf()` constructs a synthetic BirdFlow fixture because the
### amewoo reference model predates ebird_coverage metadata. Remove this
### helper and replace fixture usage with `BirdFlowModels::amewoo` once amewoo
### is updated to include ebird_coverage.
make_coverage_bf <- function() {
  bf <- new_BirdFlow()
  nr <- 3L; nc <- 4L; nt <- 2L
  mask <- matrix(TRUE, nrow = nr, ncol = nc)
  mask[1, 1] <- FALSE  # top-left inactive
  mask[3, 4] <- FALSE  # bottom-right inactive
  bf$geom$nrow <- nr
  bf$geom$ncol <- nc
  bf$geom$mask <- mask
  bf$geom$ext <- c(0, nc, 0, nr)  # xmin, xmax, ymin, ymax
  bf$geom$res <- c(1, 1)
  bf$geom$crs <- ""
  bf$metadata$n_active <- sum(mask)

  cov <- array(TRUE, dim = c(nr, nc, nt),
               dimnames = list(row = NULL, col = NULL,
                               time = paste0("t", seq_len(nt))))
  cov[2, 2, 1] <- FALSE  # one FALSE in t1
  cov[1, 3, 2] <- FALSE  # one FALSE in t2
  bf$metadata$ebird_coverage <- cov
  bf
}

test_that("get_ebird_coverage warns and returns NA for models without coverage", {
  skip_if_not_installed("BirdFlowModels")
  bf <- BirdFlowModels::amewoo
  expect_warning(result <- get_ebird_coverage(bf), "does not have")
  expect_true(is.na(result))
})

test_that("get_ebird_coverage returns SpatRaster by default", {
  bf <- make_coverage_bf()
  cov <- get_ebird_coverage(bf)
  expect_s4_class(cov, "SpatRaster")
  expect_equal(terra::nlyr(cov), 2L)
  expect_equal(names(cov), c("t1", "t2"))
  expect_equal(terra::nrow(cov), 3L)
  expect_equal(terra::ncol(cov), 4L)
})

test_that("get_ebird_coverage SpatRaster values match array", {
  bf <- make_coverage_bf()
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
  bf <- make_coverage_bf()
  arr <- get_ebird_coverage(bf, format = "array")
  expect_true(is.array(arr))
  expect_equal(dim(arr), c(3L, 4L, 2L))
  expect_type(arr, "logical")
  expect_equal(dimnames(arr)$time, c("t1", "t2"))
  # Spot-check planted FALSE values
  expect_false(arr[2, 2, 1])
  expect_false(arr[1, 3, 2])
})

test_that("get_ebird_coverage returns correct dataframe", {
  bf <- make_coverage_bf()
  df <- get_ebird_coverage(bf, format = "dataframe")
  expect_s3_class(df, "data.frame")
  expect_named(df, c("row", "col", "x", "y", "i", "timestep", "coverage"))
  expect_equal(nrow(df), 3L * 4L * 2L)  # all cells × timesteps
  expect_setequal(df$timestep, c("t1", "t2"))
  # Inactive cells have NA i
  expect_true(any(is.na(df$i)))
  expect_equal(sum(is.na(df$i)), 2L * 2L)  # 2 inactive cells × 2 timesteps
  # Planted FALSE values are present in the dataframe
  expect_false(df$coverage[df$row == 2 & df$col == 2 & df$timestep == "t1"])
  expect_false(df$coverage[df$row == 1 & df$col == 3 & df$timestep == "t2"])
})

test_that("get_mask still works after refactor", {
  skip_if_not_installed("BirdFlowModels")
  bf <- BirdFlowModels::amewoo

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
