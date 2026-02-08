test_that("rasterize_bmtr() does not throw error with simple BMTR object", {
  local_quiet()

  # Amewoo BirdFlow model
  bf <- BirdFlowModels::amewoo

  # Unweighted BMTR object
  bmtr_uw <- calc_bmtr(bf, weighted = FALSE)

  expect_no_error(raster_uw <- rasterize_bmtr(bmtr_uw, bf))
  expect_no_error(terra::plot(raster_uw))
})

test_that("raster extent matches model extent", {
  local_quiet()

  # Amewoo BirdFlow model
  bf <- BirdFlowModels::amewoo

  # Unweighted BMTR object
  bmtr_uw <- calc_bmtr(bf, weighted = FALSE)

  raster_uw <- rasterize_bmtr(bmtr_uw, bf)

  expect_true(ext(bf) == ext(raster_uw))
})

test_that("number of layers in raster matches number of timesteps in model", {
  local_quiet()

  # Amewoo BirdFlow model
  bf <- BirdFlowModels::amewoo

  # Unweighted BMTR object
  bmtr_uw <- calc_bmtr(bf, weighted = FALSE)

  raster_uw <- rasterize_bmtr(bmtr_uw, bf)

  expect_true(bf$metadata$n_timesteps == terra::nlyr(raster_uw))
})

test_that("rasterization works for weighted BMTR calculation", {
  local_quiet()

  # Amewoo BirdFlow model
  bf <- BirdFlowModels::amewoo

  # Weighted, non-Euclidean BMTR object
  bmtr_sph <- calc_bmtr(bf, weighted = TRUE)

  expect_no_error(raster_sph <- rasterize_bmtr(bmtr_sph, bf))
  expect_no_error(terra::plot(raster_sph))
})
