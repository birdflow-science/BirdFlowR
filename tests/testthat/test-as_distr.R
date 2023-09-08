test_that("as_distr() works with data frames", {

  bf <- BirdFlowModels::amewoo
  df <- i_to_xy(1:5, bf)
  df$i <- 1:5

  # No CRS
  expect_no_error( d1 <- as_distr(df, bf) )

  # bf CRS
  expect_no_error( d2 <- as_distr(df, bf, crs = crs(bf)) )

  # Different CRS
  ll <-  xy_to_latlon(df, bf = bf)
  ll <- ll[, c("lon", "lat")]
  names(ll) <- c("x", "y")
  expect_no_error(d3 <- as_distr(ll, bf, crs = "EPSG:4326"))


  expect_equal(d1, d2)
  expect_equal(d1, d3)

  # Single point
  expect_no_error(d4 <- as_distr(df[1, , drop = FALSE], bf))
  expect_true(is.vector(d4))
  expect_true(length(d4) == n_active(bf))

  # Warnings
  # Out of extent
  df <- rbind(df, data.frame(x = 0, y = 0, i = NA)) # out of extent
  expect_warning(d5 <- as_distr(df, bf),
                 regexp = "Not all locations in x are within the BirdFlow mask")
  expect_true(all(is.na(d5[, 6])))

})


test_that("as_distr works with raster objects", {

  bf <- BirdFlowModels::amewoo

  # Get extent of bf in WGS84
  e <- sf::st_bbox(bf) |>
    sf::st_as_sfc() |>
    sf::st_as_sf() |>
    sf::st_transform(crs = "EPSG:4326") |> terra::ext()

  # Create new Raster
  r <- terra::rast(matrix( 1:20, 20, 20), ext = e)
  terra::values(r) <- terra::values(r)/sum(terra::values(r))
  terra::crs(r) <- "EPSG:4326"

  # Convert to distribution
  expect_warning(d <- as_distr(r, bf)) # value lost both to cropping and masking
  expect_equal(sum(d), 1)

  # Multiple layers
  r2 <- c(r, r, r)
  expect_warning(d2 <- as_distr(r2, bf)) # value lost both to cropping and masking
  expect_true(all(apply(d2, 2, sum) == 1))
  expect_equal(nrow(d2), n_active(bf))
  expect_equal(ncol(d2), 3)
  expect_equal(d2[, 1], d, ignore_attr = TRUE)

})


test_that("as_distr() works with sf points", {

  bf <- BirdFlowModels::amewoo
  df <- i_to_xy(1:5, bf)
  df$i <- 1:5

  pts <- sf::st_as_sf(df, coords = c("x", "y"))
  sf::st_crs(pts) <- terra::crs(bf)

  # Same CRS
  expect_no_error( d1 <- as_distr(pts, bf) )

  # Different CRS
  pts2 <- sf::st_transform(pts, "EPSG:4326")
  expect_no_error(d2 <- as_distr(pts, bf))

  # Same result either way
  expect_equal(d1, d2)


  # Single point
  expect_no_error(d3 <- as_distr(pts[1, , drop = FALSE], bf))

})
