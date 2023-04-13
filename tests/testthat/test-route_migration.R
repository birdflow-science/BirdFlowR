test_that("route_migration() works with n = 1", {
  set.seed(1)
  # expect no error:
  expect_error( rts <- route_migration(BirdFlowModels::amewoo, n = 1), NA )

  # Expect consistent output
  novel_points <- rts$points[!duplicated(rts$points[, c("x", "y", "route")]), ]
  # Expect consistent output
  expect_snapshot(novel_points)
  expect_equal(class(rts$lines), c("sf", "data.frame"))
  expect_equal(length(rts$lines), 2)

})



test_that("route_migration() works with n > 1", {
  set.seed(1)
  # expect no error:
  expect_no_error(
    rts <- route_migration(BirdFlowModels::amewoo, migration = "fall",  n = 3))

  novel_points <- rts$points[!duplicated(rts$points[, c("x", "y", "route")]), ]
  # Expect consistent output
  expect_snapshot(novel_points)
})

test_that("route_migration() works with full (not sparse) marginals", {
  bf <- BirdFlowModels::amewoo
  for (marg in (c("M_01-02", "M_02-03", "M_03-04", "M_04-05"))) {
    bf$marginals[[marg]] <- as.matrix(bf$marginals[[marg]]) # store as standard
  }
  i <- which(as.logical(sample_distr(get_distr(bf, 1))))
  x <- i_to_x(i, bf)
  y <- i_to_y(i, bf)

  # single
  expect_no_error(rts <- route( bf,x_coord = x, y_coord = y, n = 1, start = 1, end = 5))

  # multiple
  i <- apply(sample_distr(get_distr(bf, 1), n = 4), 2,
             function(x) which(as.logical(x)))
  x <- i_to_x(i, bf)
  y <- i_to_y(i, bf)
  expect_no_error(rts <- route( bf,x_coord = x, y_coord = y, n = 1, start = 1, end = 5))

  if(interactive()){
    plot(rast(bf, 1))
    points(x, y)
    plot(rts$lines, add = TRUE, col = "black")
  }
})




