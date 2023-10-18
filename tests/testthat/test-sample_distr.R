test_that("sample_distr() works with single distribution", {
  bf <- BirdFlowModels::amewoo
  d <- get_distr(bf, 1)

  # distr
  set.seed(1)
  expect_no_error(s <- sample_distr(d))
  expect_equal(length(d), length(s))
  expect_true(all(s %in% c(0, 1)))
  expect_equal(sum(s), 1)

  # i
  set.seed(1)
  i <- sample_distr(d, bf = bf, format = "i")
  expect_equal(length(i), 1)

  # xy
  set.seed(1)
  xy <- sample_distr(d, bf = bf, format = "xy")
  expect_true(inherits(xy, "data.frame"))
  expect_equal(dim(xy), c(1, 2))
  expect_equal(colnames(xy), c("x", "y"))

  # latlon
  set.seed(1)
  latlon <- sample_distr(d, bf = bf, format = "latlon")
  expect_true(inherits(latlon, "data.frame"))
  expect_equal(dim(latlon), c(1, 2))
  expect_equal(colnames(latlon), c("lat", "lon"))

  # Compare
  expect_equal(which(as.logical(s)), i)
  expect_equal(xy_to_i(xy, bf = bf), i)
  expect_equal(latlon_to_xy(latlon, bf = bf), xy)

})


test_that("sample_distr() works with multiple distributions", {
  bf <- BirdFlowModels::amewoo
  d <- get_distr(bf, 1:4)

  # distr
  set.seed(1)
  expect_no_error(s <- sample_distr(d))
  expect_equal(dim(d), dim(s))
  expect_true(all(s %in% c(0, 1)))
  expect_equal(as.numeric(apply(s, 2, sum)), rep(1, ncol(s)))

  # i
  set.seed(1)
  i <- sample_distr(d, bf = bf, format = "i")
  expect_equal(length(i), ncol(d))


  # xy
  set.seed(1)
  xy <- sample_distr(d, bf = bf, format = "xy")
  expect_true(inherits(xy, "data.frame"))
  expect_equal(dim(xy), c(ncol(d), 2))
  expect_equal(colnames(xy), c("x", "y"))

  # latlon
  set.seed(1)
  latlon <- sample_distr(d, bf = bf, format = "latlon")
  expect_true(inherits(latlon, "data.frame"))
  expect_equal(dim(latlon), c(ncol(d), 2))
  expect_equal(colnames(latlon), c("lat", "lon"))

  # Compare
  expect_equal(apply(s, 2, function(x) which(as.logical(x))), i)
  expect_equal(xy_to_i(xy, bf = bf), as.vector(i))
  expect_equal(latlon_to_xy(latlon, bf = bf), xy)
})



test_that("sample_distr() works with 3D input", {
  bf <- BirdFlowModels::amewoo
  d <- get_distr(bf, c(1, 1))
  d <- predict(bf, distr = d, start = 1, end = 3)


  # distr
  set.seed(1)
  expect_no_error(s <- sample_distr(d))
  expect_equal(dim(d), dim(s))
  expect_true(all(s %in% c(0, 1)))

  prob_sums <-  apply(s, 2:3, sum)
  dimnames(prob_sums) <- NULL
  expect_equal(prob_sums, matrix(1, nrow = dim(d)[2], ncol = dim(d)[3]))

  # i
  set.seed(1)
  i <- sample_distr(d, bf = bf, format = "i")
  expect_equal(dim(i), dim(d)[-1])

  # xy
  set.seed(1)
  xy <- sample_distr(d, bf = bf, format = "xy")
  expect_true(inherits(xy, "array"))
  expect_equal(dim(xy), c(dim(d)[-1], 2))
  expect_equal(dimnames(xy)[[3]], c("x", "y"))

  # latlon
  set.seed(1)
  latlon <- sample_distr(d, bf = bf, format = "latlon")
  expect_true(inherits(latlon, "array"))
  expect_equal(dim(latlon), c(dim(d)[-1], 2))
  expect_equal(dimnames(latlon)[[3]], c("lat", "lon"))

  # Compare
  expect_equal(apply(s, 2:3, function(x) which(as.logical(x))), i)
  expect_equal(xy_to_i(x = as.vector(xy[, , 1]), y = as.vector(xy[,  , 2]),
                       bf = bf), as.vector(i))
  xy2 <- data.frame(x = as.vector(xy[, , 1]), y = as.vector(xy[, , 2]))
  expect_equal(latlon_to_xy(lat = as.vector(latlon[, , 1]),
                            lon = as.vector(latlon[, , 2]), bf = bf), xy2)
})
