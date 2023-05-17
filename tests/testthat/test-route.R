test_that("route() works", {
  bf <- BirdFlowModels::amewoo
  if(!has_dynamic_mask(bf))
    bf <- add_dynamic_mask(bf)
  n <- 10
  start <- 5
  end  <- 10
  sd <- get_distr(bf, start)
  set.seed(1)
  loc <- sample_distr(sd, n = n) |>
    apply( MARGIN = 2, FUN = function(x) which(as.logical(x))) |>
    i_to_xy(bf) |> as.data.frame()
  expect_no_error(rts <- route(bf, loc$x, loc$y, start = start, end = end))


  set.seed(1)
  n <- 1
  loc <- sample_distr(sd, n = n) |>
    as.logical() |>
    which() |>
    i_to_xy(bf) |> as.data.frame()
  expect_no_error(rt <- route(bf, loc$x, loc$y, start = start, end = end))

  pts <- rts$points
  pts <- head(pts[!duplicated(pts[, c("route", "stay_id")]), ], 4)
  expect_snapshot(pts)

})


test_that("route() works over year boundary", {
  bf <- BirdFlowModels::rewbla
  if(!has_dynamic_mask(bf))
    bf <- add_dynamic_mask(bf)
  n <- 5
  start <- 50
  end  <- 5
  sd <- get_distr(bf, start)
  set.seed(1)
  loc <- sample_distr(sd, n = n) |>
    apply( MARGIN = 2, FUN = function(x) which(as.logical(x))) |>
    i_to_xy(bf) |> as.data.frame()
  expect_no_error(rts <- route(bf, loc$x, loc$y, start = start, end = end))


  set.seed(1)
  n <- 1
  loc <- sample_distr(sd, n = n) |>
    as.logical() |>
    which() |>
    i_to_xy(bf) |> as.data.frame()
  expect_no_error(rt <- route(bf, loc$x, loc$y, start = start, end = end))

})
