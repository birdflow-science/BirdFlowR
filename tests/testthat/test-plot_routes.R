test_that("plot_routes() works", {
  bf <- BirdFlowModels::amewoo
  n_spring <- n_fall <- 4
  rts <- route_migration(bf, n_spring)
  points <- rts$points

  expect_no_error(p <- plot_routes(points, bf))
  expect_no_error(print(p))
  expect_no_error(p <- plot_routes(points, bf, facet = TRUE))
  expect_no_error(print(p))

  fall_rts <- route_migration(bf, n_fall, "fall")
  fall_rts$points$route <- fall_rts$points$route + n_spring # for unique routes
  expect_no_error(p <- plot_routes(rbind(points, fall_rts$points), bf))
  expect_no_error(print(p))
})

test_that("plot_routes() works over year boundary", {
  bf <- BirdFlowModels::rewbla
  start <- 40
  end  <- 15
  sd <- get_distr(bf, start)
  n <- 5
  set.seed(1)
  loc <- sample_distr(sd, n = n) |>
    apply( MARGIN = 2, FUN = function(x) which(as.logical(x))) |>
    i_to_xy(bf) |> as.data.frame()
  expect_no_error(rts <- route(bf, loc$x, loc$y, start = start, end = end))
  expect_no_error(p <- plot_routes(rts$points, bf))
  expect_no_error(print(p))

})


