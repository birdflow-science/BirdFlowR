test_that("rasterize_distr, flatten_raster, and get_distr are consistent - multiple distributions", {
  bf <- BirdFlowModels::amewoo
  sr <- rasterize_distr(get_distr(1:2, bf), bf)
  a <- terra::as.array(sr)
  dimnames(a) <- list(NULL, NULL, timestep = names(sr))
  d <- flatten_raster(a, bf)
  expect_equal(d, get_distr(1:2, bf))
  sr2 <- rast(bf, 1:2)
  expect_equal(terra::values(sr), terra::values(sr2))
  expect_equal(dim(sr), dim(sr2))
  expect_equal(terra::crs(sr), terra::crs(sr2))
})

test_that("rasterize_distr, flatten_raster, and get_distr are consistent - 1 distribution", {
  bf <- BirdFlowModels::amewoo
  sr <- rasterize_distr(get_distr(1, bf), bf)
  a <- terra::as.matrix(sr, wide = TRUE)
  d <- flatten_raster(a, bf)
  expect_equal(d, get_distr(1, bf))
  sr2 <- rast(bf, 1)
  expect_equal(terra::values(sr), terra::values(sr2))
  expect_equal(dim(sr), dim(sr2))
  expect_equal(terra::crs(sr), terra::crs(sr2))
})
