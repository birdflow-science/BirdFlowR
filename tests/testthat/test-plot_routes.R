test_that("plot_routes() works", {
  bf <- BirdFlowModels::amewoo
  n_spring <- n_fall <- 4
  rts <- route_migration(bf, n_spring)
  points <- rts$points

  expect_no_condition(p <- plot_routes(points, bf))
  expect_no_condition(print(p))
  expect_no_condition(p <- plot_routes(points, bf, facet = TRUE))
  expect_no_condition(print(p))

  fall_rts <- route_migration(bf, n_fall, "fall")
  fall_rts$points$route <- fall_rts$points$route + n_spring # for unique routes
  expect_no_condition(p <- plot_routes(rbind(points, fall_rts$points), bf))
  expect_no_condition(print(p))
})
