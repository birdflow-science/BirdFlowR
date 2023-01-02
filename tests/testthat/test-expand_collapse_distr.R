#------------------------------------------------------------------------------#
####  Setup test objects
#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#### Test
#------------------------------------------------------------------------------#


test_that("expand and collapse distr are reversable", {
  bf <- make_test_birdflow()
  d <- get_distr(1, bf)
  e <- expand_distr(d, bf)
  f <- collapse_distr(e, bf)
  expect_equal(d, f)
})

test_that("collapse_distr is consistent with i_to_rc() subset", {
  bf <- make_test_birdflow()
  d <- runif(n = bf$n_active)
  m <- matrix(NA, nrow = bf$geom$nrow, ncol = bf$geom$ncol)
  m[i_to_rc(1:bf$n_active, bf)] <- d
  d2 <- collapse_distr(m, bf)
  expect_equal(d, d2)
})
