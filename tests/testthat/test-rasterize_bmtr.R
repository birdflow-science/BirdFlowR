# Todo
# Testing:
# Bare min: Create a bmtr and rasterize it **without an error**
# PBT 1: expect_equal(ext(bf), ext(bmtr_raster)) -> ext is the extent function
# PBT 2: number of layers in raster (layers function) == number of timesteps -> (loose equality constraint, allow some tolerance)

test_that("rasterize_bmtr() does not throw error with simple BMTR object", {
  local_quiet()

  # Amewoo BirdFlow model
  bf <- BirdFlowModels::amewoo

  # Unweighted BMTR object
  bmtr_uw <- calc_bmtr(bf, weighted = FALSE)

  expect_no_error(raster_uw <- rasterize_bmtr(bmtr_uw, bf))
})
