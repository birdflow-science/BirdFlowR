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
              xy_to_latlon(d[, c("x","y"), drop = FALSE], bf =  bf))

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
  expect_equal(a1[, consistent_cols], a2[ , consistent_cols])
  expect_equal(a1[, consistent_cols], a3[ , consistent_cols])


  # Expect median and mean to be identical if less than 3 observations
  sv <- d$n < 3
  expect_equal(a1[sv, c("x", "y")], a2[sv, c("x", "y")])

})

test_that("snap_to_birdflow() works with preprocessed models", {

  # The required preprocesing takes a while
  skip_on_cran()
  skip_on_covr()
  skip_on_ci()

  bf <- preprocess_species("amewoo", res = 150, hdf5 = FALSE)
  d <- make_fake_move_data(bf)
  s <- snap_to_birdflow(d, x = "x", y = "y", crs = crs(bf),  bf = bf,
                   id_cols = c("bird_id", "track_id"))

})




