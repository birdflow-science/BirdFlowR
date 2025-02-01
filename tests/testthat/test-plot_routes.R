test_that("plot_routes() works", {
  bf <- BirdFlowModels::amewoo
  n_spring <- n_fall <- 4
  points <- route(bf, n_spring, season = "spring")


  expect_no_error(p <- plot_routes(points, bf))
  expect_no_error(print(p))
  expect_no_error(p <- plot_routes(points, bf, facet = TRUE))
  expect_no_error(print(p))

  fall_rts <- route(bf, n_fall, season = "fall")
  fall_rts$data$route_id <- fall_rts$data$route_id + n_spring # for unique routes
  fall_rts$data <- rbind(fall_rts$data, points$data)
  expect_no_error(p <- plot_routes(fall_rts, bf))
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
    apply(MARGIN = 2, FUN = function(x) which(as.logical(x))) |>
    i_to_xy(bf) |>
    as.data.frame()
  expect_no_error(rts <- route(bf, x_coord = loc$x, y_coord = loc$y,
                               start = start, end = end))
  expect_no_error(p <- plot_routes(rts, bf))
  expect_no_error(print(p))

})

test_that("plot_routes() works without bf", {
  bf <- BirdFlowModels::rewbla
  start <- 40
  end  <- 15
  sd <- get_distr(bf, start)
  n <- 5
  set.seed(1)
  loc <- sample_distr(sd, n = n) |>
    apply(MARGIN = 2, FUN = function(x) which(as.logical(x))) |>
    i_to_xy(bf) |>
    as.data.frame()
  expect_no_error(rts <- route(bf, x_coord = loc$x, y_coord = loc$y,
                               start = start, end = end))
  expect_no_error(p <- plot_routes(rts))
  expect_no_error(print(p))

})
