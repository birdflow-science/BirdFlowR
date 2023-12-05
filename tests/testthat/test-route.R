test_that("route() works with a single distribution", {
  bf <- BirdFlowModels::amewoo

  start <- 5
  end  <- 15
  sd <- get_distr(bf, start)

  set.seed(2)
  n <- 1
  loc <- sample_distr(sd, n = n) |>
    as.logical() |>
    which() |>
    i_to_xy(bf) |> as.data.frame()
  expect_no_error(pts <- route(bf, x_coord = loc$x, y_coord = loc$y,
                               start = start, end = end))
  pts <- as.data.frame(pts)
  pts <- head(pts[!duplicated(pts[, c("route_id", "stay_id")]), ], 4)

  expect_snapshot(pts)

})



test_that("route() works over year boundary", {
  bf <- BirdFlowModels::rewbla
  if (!has_dynamic_mask(bf))
    bf <- add_dynamic_mask(bf)
  n <- 5
  start <- 50
  end  <- 5
  sd <- get_distr(bf, start)
  set.seed(1)
  loc <- sample_distr(sd, n = n) |>
    apply(MARGIN = 2, FUN = function(x) which(as.logical(x))) |>
    i_to_xy(bf) |> as.data.frame()
  expect_no_error(rts <- route(bf, x_coord = loc$x, y_coord = loc$y,
                               start = start, end = end))


  set.seed(1)
  n <- 1
  loc <- sample_distr(sd, n = n) |>
    as.logical() |>
    which() |>
    i_to_xy(bf) |> as.data.frame()
  expect_no_error(rt <- route(bf, x_coord = loc$x, y_coord = loc$y,
                              start = start, end = end))

})

test_that("route() works while sampling the starting locations", {
  bf <- BirdFlowModels::amewoo
  expect_no_error(route(bf, n = 10, season = "prebreeding"))
})


test_that("route() works with full (not sparse) marginals", {
  bf <- BirdFlowModels::amewoo
  for (marg in (c("M_01-02", "M_02-03", "M_03-04", "M_04-05"))) {
    bf$marginals[[marg]] <- as.matrix(bf$marginals[[marg]]) # store as standard
  }
  i <- which(as.logical(sample_distr(get_distr(bf, 1))))
  x <- i_to_x(i, bf)
  y <- i_to_y(i, bf)

  # single
  expect_no_error(rts <- route(bf, x_coord = x, y_coord = y,
                                n = 1, start = 1, end = 5))

  # multiple
  i <- apply(sample_distr(get_distr(bf, 1), n = 4), 2,
             function(x) which(as.logical(x)))
  x <- i_to_x(i, bf)
  y <- i_to_y(i, bf)
  expect_no_error(rts <- route(bf, x_coord = x, y_coord = y,
                                n = 1, start = 1, end = 5))

})
