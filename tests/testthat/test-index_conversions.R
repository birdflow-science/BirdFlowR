
#------------------------------------------------------------------------------#
####  Setup test objects
#------------------------------------------------------------------------------#

# Note, some tests assume the exact structures below, don't change them!

# mask
# matrix with the same dimensions as the raster with TRUE to indicate
# cells that are active (used in the model)
mask <- matrix(data = TRUE, nrow = 5, ncol = 3)
out <- matrix(c(1, 1, 1, 3, 2, 1, 3, 1, 5, 1, 5, 2), ncol = 2, byrow = TRUE)
mask[out] <- FALSE

# Vector data (just active cells in row-major order)
vect <- 1:sum(mask)

# Full data with raster dimensions
full <- matrix(NA, ncol = nrow(mask), nrow = ncol(mask))
full[t(mask)] <- vect
full <- t(full)

# Generate geometry object with arbitrary coordinates and resolution
cellsize <- 30
geom <- list(nrow = nrow(mask), ncol = ncol(mask), res = rep(cellsize, 2),
             ext = c(0, # xmin
                      cellsize * ncol(mask), # xmax
                      1000, # ymin
                      1000 + cellsize * nrow(mask)), # ymax
             crs = "",
             mask = mask)

bf <- new_BirdFlow()
bf$geom <- geom
bf$metadata$n_active <- sum(bf$geom$mask)

# column and row indices in order
columns <- 1:geom$ncol
rows <- 1:geom$nrow

# calculate x and y coordinates of column and row centers
# (all columns in order)
col_center_x <- geom$ext[1] + (1:geom$ncol - 0.5) * cellsize
row_center_y <- geom$ext[4] - (1:geom$nrow - 0.5) * cellsize  # row 1, high y

# State index values that correspond to column or rows in order
sel1 <- c(6, 1, 8) # correspond to columns 1, 2, 3 respectively
sel2 <- c(1, 2, 5, 6, 9) # correspond to rows 1:nrow

rast <- terra::rast(x = full, ext = geom$ext) # SpatRast object



#------------------------------------------------------------------------------#
####  Tests
#------------------------------------------------------------------------------#

test_that("x_to_col works on cell centers", {
  expect_equal(x_to_col(col_center_x, bf), columns)
})
test_that("y_to_row works on cell centers", {
  expect_equal(y_to_row(row_center_y, bf), rows)
})

# Test boundary conditions on x_to_col an y_to_row
# The cell boundary belongs to the the higher index but all values within
# extent should still be in; i.e. xmax, and ymin will fall within
# the last column and row instead of being out.
x_vals <- c(xmin(bf), xmin(bf) + xres(bf), xmax(bf))
expected_cols <- c(1, 2, ncol(bf))
test_that("x_to_col works on cell boundaries", {
  expect_equal(x_to_col(x_vals, bf), expected_cols)
  expect_equal(terra::colFromX(object = rast, x = x_vals),
               expected_cols)
})

y_vals <- c(ymax(bf), ymax(bf) - yres(bf), ymin(bf))
expected_rows <- c(1, 2, nrow(bf))
test_that("y_to_row works on cell boundaries", {
  expect_equal(y_to_row(y_vals, bf), expected_rows)
  expect_equal(terra::rowFromY(object = rast, y = y_vals),
               expected_rows) # check for consistency with terra on boundaries

})
rm(x_vals, y_vals, expected_cols, expected_rows)

test_that("row_to_y works", {
  expect_equal(row_to_y(seq_len(nrow(bf)), bf), row_center_y)
})

test_that("col_to_x works", {
  expect_equal(col_to_x(seq_len(ncol(bf)), bf), col_center_x)
})

test_that("i_to_rc works", {
  expect_equal(i_to_rc(vect, bf),
               data.frame(row = i_to_row(vect, bf), col = i_to_col(vect, bf)))
  expect_equal(full[as.matrix(i_to_rc(vect, bf))], vect)
})

test_that("i_to_col, i_to_row work", {
    expect_equal(full[cbind(i_to_row(vect, bf), i_to_col(vect, bf))],
                 vect)
})

test_that("i_to_x, i_to_y work", {
  expect_equal(i_to_col(sel1, bf), seq_len(ncol(bf))) # verify sel1
  expect_equal(i_to_x(sel1, bf), col_center_x)  # actual test
  expect_equal(i_to_row(sel2, bf), seq_len(nrow(bf))) # verify sel2
  expect_equal(i_to_y(sel2, bf), row_center_y) # actual test
})

test_that("i_to_xy works", {
  expect_equal(i_to_xy(vect, bf),
               data.frame(x = i_to_x(vect, bf),
                          y =  i_to_y(vect, bf)))
})

rc <- matrix(c(1, 2, 3, 2, 5, 3), ncol = 2, byrow = TRUE)
vals <- c(1, 4, 9) # values in those cells , same as index for testing objects
test_that("rc_to_i works", {
  expect_equal(full[rc], vals) # test rc, vals
  expect_equal(rc_to_i(row = rc[, 1], col = rc[, 2], bf), vals)
  expect_equal(rc_to_i(rc, bf = bf), vals)  # matrix passing
})

x <- col_center_x[rc[, 2]]
y <- row_center_y[rc[, 1]]
xy <- cbind(x, y)
test_that("xy_to_i works", {
  expect_equal(xy_to_i(x = xy[, 1], y = xy[, 2], bf), vals)
  expect_equal(xy_to_i(x = xy, bf = bf), vals)  # matrix passing
})


test_that("latlon_to_xy works", {
  bf <- BirdFlowModels::amewoo
  i <- sample(1:n_active(bf), 10)
  xy <- i_to_xy(i, bf) |> as.data.frame()
  pts <- sf::st_as_sf(xy, coords = c("x", "y"), crs = sf::st_crs(crs(bf))) |>
    sf::st_transform(crs = "EPSG:4326") |>
    sf::st_coordinates() |>
    as.data.frame()
  names(pts) <- c("lon", "lat")

  # Two arguments
  expect_no_error(xy2 <- latlon_to_xy(lat = pts$lat, lon = pts$lon, bf))
  expect_equal(xy, xy2)

  # One argument with names (and columns reversed)
  xy3 <- latlon_to_xy(pts, bf = bf)
  expect_equal(xy, xy3)

  # One argument no column names
  pts <- pts[, c("lat", "lon")]
  pts <- as.matrix(pts)
  names(pts) <- NULL
  xy4 <- latlon_to_xy(pts, bf = bf)
  expect_equal(xy, xy4)

})



test_that("latlon_to_xy and xy_to_latlon are consistant", {
  bf <- BirdFlowModels::amewoo
  i <- sample(1:n_active(bf), 10)
  xy <- i_to_xy(i, bf) |> as.data.frame()
  pts <- sf::st_as_sf(xy, coords = c("x", "y"), crs = sf::st_crs(crs(bf))) |>
    sf::st_transform(crs = "EPSG:4326") |>
    sf::st_coordinates() |>
    as.data.frame()
  names(pts) <- c("lon", "lat")
  pts <- pts[, c(2, 1)] # lat, lon


  # Two arguments
  expect_no_error(xy2 <- latlon_to_xy(lat = pts$lat, lon = pts$lon, bf))

  # one argument
  expect_no_error(latlon <- xy_to_latlon(x = xy2, bf = bf))
  expect_equal(latlon, pts)

})



test_that("latlon_to_xy returns NA for out of range and NA input values", {
  bf <- BirdFlowModels::amewoo
  i <- sample(1:n_active(bf), 5)
  xy <- i_to_xy(i, bf) |> as.data.frame()
  pts <- sf::st_as_sf(xy, coords = c("x", "y"), crs = sf::st_crs(crs(bf))) |>
    sf::st_transform(crs = "EPSG:4326") |>
    sf::st_coordinates() |>
    as.data.frame()
  names(pts) <- c("lon", "lat")

  # Test points
  # 1. Valid
  # 2. NA lon
  # 3. NA lat
  # 4. lon out of range
  # 5. lat out of range
  pts$lon[2] <- NA
  pts$lat[3] <- NA
  pts$lon[4] <- 500
  pts$lat[5] <- 500

  expect_no_error(xy2 <- latlon_to_xy(lat = pts$lat, lon = pts$lon, bf))

  expect_equal(xy2[1, ], xy[1, ])
  expect_true(all(is.na(xy2[2:5, ])))

})

test_that(
  "functions with x and y as inputs return NA if input out of range or NA", {
  bf <- BirdFlowModels::amewoo

  n <- 7
  set.seed(1)
  pts <- sample(1:n_active(bf), n) |> i_to_xy(bf)

  # Test points
  # 1. valid
  # 2. x high
  # 3. x low
  # 4. y high
  # 5.  y low
  # 6. x NA
  # 7. y NA

  pts$x[2] <- xmax(bf) + 1000
  pts$x[3] <- xmin(bf) - 1000
  pts$y[4] <- ymax(bf) + 1000
  pts$y[5] <- ymin(bf) - 1000
  pts$x[6] <- NA
  pts$y[7] <- NA

  expected_na_rows <- c(4, 5, 7)
  expected_na_cols <- c(2, 3, 6)
  expected_na_i <- sort(unique(c(expected_na_rows, expected_na_cols)))

  expect_no_error(na_i <- which(is.na(xy_to_i(pts, bf =  bf))))
  expect_no_error(na_cols <- which(is.na(x_to_col(pts$x, bf))))
  expect_no_error(na_rows <- which(is.na(y_to_row(pts$y, bf))))

  expect_equal(na_i, expected_na_i)
  expect_equal(na_cols, expected_na_cols)
  expect_equal(na_rows, expected_na_rows)

})




test_that("rc_to_i()  returns NA if input out of range or NA", {
  bf <- BirdFlowModels::amewoo

  n <- 9
  set.seed(1)
  i <- sample(1:n_active(bf), size = n)
  rc <- i_to_rc(i, bf) |> as.data.frame()

  # Test points
  # 1. valid
  # 2. r 0
  # 3. r low
  # 4. r high
  # 5. r NA
  # 6. c 0
  # 7. c low
  # 8. c high
  # 9. c NA
  rc$row[2] <- 0
  rc$row[3] <- -2
  rc$row[4] <- nrow(bf) + 5
  rc$row[5] <- NA
  rc$col[6] <- 0
  rc$col[7] <- -2
  rc$col[8] <- ncol(bf) + 5
  rc$col[9] <- NA

  expect_equal(which(is.na(rc_to_i(rc, bf = bf))), 2:9)

})
