
#------------------------------------------------------------------------------#
# numeric input
#------------------------------------------------------------------------------#

test_that("expand_distr and flatten_raster are reversable - 1 distr", {
  bf <- BirdFlowModels::amewoo
  d <- get_distr(bf, 1)
  r <- expand_distr(d, bf)
  f <- flatten_raster(r, bf)
  expect_equal(d, f)
})

test_that("expand_distr and flatten_raster are reversable - multiple distr", {
  bf <- BirdFlowModels::amewoo
  d <- get_distr(bf, 3:4)
  r <- expand_distr(d, bf)
  f <- flatten_raster(r, bf)
  expect_equal(d, f)
})


test_that("flatten_raster is consistent with i_to_rc() subset", {
  bf <- BirdFlowModels::amewoo
  d <- runif(n = n_active(bf))
  m <- matrix(NA, nrow = nrow(bf), ncol = ncol(bf))
  m[as.matrix(i_to_rc(seq_len(n_active(bf)), bf))] <- d
  d2 <- flatten_raster(m, bf)
  expect_equal(d, d2)
})


#------------------------------------------------------------------------------#
# SpatRaster input
#------------------------------------------------------------------------------#

test_that("rasterize_distr and flatten_raster are reversable - SpatRasters",{

  # Single distribution
  bf <- BirdFlowModels::amewoo
  r <- rast(bf, 1)
  d <- get_distr(bf, 1)
  fd <- flatten_raster(r, bf)
  expect_equal(fd, d)
  r2 <- rasterize_distr(d, bf)

  # Note. To compare SpatRasters use all.equal which has a method defined
  # in terra.  expect_equal() relies on waldo::compare() which will flag
  # immaterial differences.
  expect_true(all.equal(r, r2))

})


