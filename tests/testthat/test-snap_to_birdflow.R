test_that("snap_to_birdflow works", {
  bf <- BirdFlowModels::amewoo

  d <- make_fake_move_data(bf)

  ### Test ###

  # Test alignment using reprojection with no aggregation
  expect_no_error(
    a <- snap_to_birdflow(d, bf, x_col = "x", y_col = "y", crs = crs(bf),
                          id_cols = c("bird_id", "track_id"))
  )

  # Swap out lat and lon for x and y
  d2 <- cbind(d[, !names(d) %in% c("x", "y"), drop = FALSE],
              xy_to_latlon(d[, c("x", "y"), drop = FALSE], bf =  bf))

  # Test align with default lat, lon, and no crs.
  expect_no_error(
    b <- snap_to_birdflow(d2, bf,
                          id_cols = c("bird_id", "track_id"))
  )

  expect_equal(a$error,  b$error)
  expect_equal(a$message, b$message)

  # Aggregate with median
  a1 <-  snap_to_birdflow(d2, bf,
                          id_cols = c("bird_id", "track_id"), aggregate = "median")

  # aggregate with mean
  a2 <-  snap_to_birdflow(d2, bf,
                          id_cols = c("bird_id", "track_id"), aggregate = "mean")

  # aggregate with midweek
  a3 <-  snap_to_birdflow(d2, bf,
                          id_cols = c("bird_id", "track_id"), aggregate = "midweek")


  consistent_cols <- c("bird_id", "track_id", "timestep", "n")
  expect_equal(a1[, consistent_cols], a2[, consistent_cols])
  expect_equal(a1[, consistent_cols], a3[, consistent_cols])


  # Expect median and mean to be identical if less than 3 observations
  sv <- d$n < 3
  expect_equal(a1[sv, c("x", "y")], a2[sv, c("x", "y")])

})

test_that("snap_to_birdflow() works with preprocessed models", {

  local_quiet()  # to suppress preprocess chatter

  # The required preprocesing takes a while
  skip_on_cran()
  skip_on_covr()
  skip_on_ci()

  bf <- preprocess_species("amewoo", res = 150, hdf5 = FALSE)
  d <- make_fake_move_data(bf)
  expect_no_error(
    s <- snap_to_birdflow(d, x = "x", y = "y", crs = crs(bf),  bf = bf,
                          id_cols = c("bird_id", "track_id"))
  )
})

test_that("snap_to_birdflow() works with non standard column names", {

  bf <- BirdFlowModels::amewoo
  d <- make_fake_move_data(bf) |>
    dplyr::rename(Dates = "date", X = "x", Y = "y")
  expect_no_error(
    s <- snap_to_birdflow(d, bf = bf,
                          x_col = "X",
                          y_col = "Y",
                          date_col = "Dates",
                          id_cols = c("bird_id", "track_id"),
                          crs = crs(bf))
  )

})

test_that("snap_to_birdflow()  behaves with odd input", {
  bf <- BirdFlowModels::amewoo
  d <- make_fake_move_data(bf)


  # Wrong projection
  expect_no_error(
    s <- snap_to_birdflow(d, bf, x_col = "x", y_col = "y",
                          id_cols = c("bird_id", "track_id"))
  )
  expect_warning(
    s <- snap_to_birdflow(d, bf, x_col = "x", y_col = "y",
                          id_cols = c("bird_id", "track_id"),
                          aggregate = "mean")
  )

  # Used for reference
  s0 <- snap_to_birdflow(d, bf, x_col = "x", y_col = "y",
                         id_cols = c("bird_id", "track_id"), crs = crs(bf))

  # Empty input data frame
  d <- d[FALSE , , drop = FALSE]

  # Note some weird warnings are thrown during reprojection about
  # infinite extent when there's no data. I think I'm OK with this.
  suppressWarnings({
    expect_no_error(
      s1 <- snap_to_birdflow(d, bf, x_col = "x", y_col = "y",
                            id_cols = c("bird_id", "track_id"), crs = crs(bf))
    )
    expect_no_error(
      s2 <- snap_to_birdflow(d, bf, x_col = "x", y_col = "y",
                             id_cols = c("bird_id", "track_id"), crs = crs(bf),
                             aggregate = "midweek")
    )
    expect_no_error(
      s3 <- snap_to_birdflow(d, bf, x_col = "x", y_col = "y",
                             id_cols = c("bird_id", "track_id"), crs = crs(bf),
                             aggregate = "mean")
    )
  }) # end suppress warnings

  expect_equal(names(s0), names(s1))
  expect_equal(names(s0), names(s2))
  expect_equal(names(s0), names(s3))

  expect_equal(sapply(s0, class), sapply(s1, class))
  expect_equal(sapply(s0, class), sapply(s2, class))
  expect_equal(sapply(s0, class), sapply(s3, class))


})
