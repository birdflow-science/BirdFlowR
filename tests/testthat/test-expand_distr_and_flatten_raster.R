#------------------------------------------------------------------------------#
####  Setup test objects
#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#### Test with numerics
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
  bf <- make_test_birdflow()
  d <- runif(n = bf$n_active)
  m <- matrix(NA, nrow = bf$geom$nrow, ncol = bf$geom$ncol)
  m[i_to_rc(1:bf$n_active, bf)] <- d
  d2 <- flatten_raster(m, bf)
  expect_equal(d, d2)
})
