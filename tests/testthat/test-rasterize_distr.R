test_that(paste0("rasterize_distr, flatten_raster, and get_distr are ",
                 "consistent - multiple distributions"), {
  bf <- BirdFlowModels::amewoo
  sr <- rasterize_distr(get_distr(bf, 1:2), bf)
  a <- terra::as.array(sr)
  dimnames(a) <- list(NULL, NULL, time = names(sr))
  d <- flatten_raster(a, bf)
  expect_equal(d, get_distr(bf, 1:2))
  sr2 <- rast(bf, 1:2)
  expect_equal(terra::values(sr), terra::values(sr2))
  expect_equal(dim(sr), dim(sr2))
  expect_equal(terra::crs(sr), terra::crs(sr2))
})

test_that(paste0("rasterize_distr, flatten_raster, and get_distr are ",
                 "consistent - 1 distribution"), {
  bf <- BirdFlowModels::amewoo
  sr <- rasterize_distr(get_distr(bf, 1), bf)
  a <- terra::as.matrix(sr, wide = TRUE)
  d <- flatten_raster(a, bf)
  expect_equal(d, get_distr(bf, 1), ignore_attr = TRUE)
  sr2 <- rast(bf, 1)
  expect_equal(terra::values(sr), terra::values(sr2))
  expect_equal(dim(sr), dim(sr2))
  expect_equal(terra::crs(sr), terra::crs(sr2))
})

test_that("rasterize_distr() with data.frame output", {
  bf <- BirdFlowModels::amewoo

  ## Single distribution
  expect_no_error(df <- rasterize_distr(get_distr(bf, 1), bf,
                                        format = "data.frame"))
  expect_false(any(is.na(df$x)))
  expect_false(any(is.na(df$y)))
  hdf <- head(df[!is.na(df$value) & df$value != 0, ], 3)
  expect_snapshot(hdf)
  # Convert back to distribution and compare
  vals <- df[!is.na(df$i), ]
  vals <- vals$value[order(vals$i)]
  expect_equal(vals, as.numeric(get_distr(bf, 1)))

  # Multiple distributions
  expect_no_error(df <- rasterize_distr(get_distr(bf, 1:3), bf,
                                        format = "dataframe"))
  expect_false(any(is.na(df$x)))
  expect_false(any(is.na(df$y)))
  hdf <- head(df[!is.na(df$value) & df$value != 0, ], 3)
  expect_snapshot(hdf)
})

test_that("rasterize_distr() works with with numeric output", {

  # 1 distribution
  bf <- BirdFlowModels::amewoo
  expect_no_error(m <- rasterize_distr(get_distr(bf, 1), bf,
                                       format = "numeric"))
  vals <- t(m)[t(bf$geom$mask)]
  expect_equal(vals, as.numeric(get_distr(bf, 1)))

  # multiple distributions
  expect_no_error(m <- rasterize_distr(get_distr(bf, 1:3), bf,
                                       format = "numeric"))
  vals <- t(m[, , 1])[t(bf$geom$mask)]
  expect_equal(vals, as.numeric(get_distr(bf, 1)))

  vals <- t(m[, , 3])[t(bf$geom$mask)]
  expect_equal(vals, as.numeric(get_distr(bf, 3)))
})

test_that("rasterize_distr() to numeric is equal to expand_distr()", {
  bf <- BirdFlowModels::amewoo
  expect_no_error(r <- rasterize_distr(get_distr(bf, 1),
                                       bf = bf,
                                       format = "numeric"))

  m <- terra::as.array(rast(bf, 1))[, , 1]
  expect_equal(r, m, ignore_attr = TRUE)

  m2 <- expand_distr(get_distr(bf, 1), bf)
  expect_equal(r, m2)
})

test_that("rasterize_distr() to dataframe works", {
  bf <- BirdFlowModels::amewoo
  d <- get_distr(bf, 1)
  expect_no_error(df <- rasterize_distr(d, bf = bf, format =  "dataframe"))

  d <- head(df[!df$value == 0 & !is.na(df$value), ], 3)
  expect_snapshot(d)
})
