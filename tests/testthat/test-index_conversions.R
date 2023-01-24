
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
             ext = c( 0,   # xmin
                      cellsize * ncol(mask),  # xmax
                      1000,   # ymin
                      1000 + cellsize * nrow(mask)),  # ymax
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
col_center_x<- geom$ext[1] + (1:geom$ncol - 0.5) * cellsize
row_center_y <- geom$ext[4] - (1:geom$nrow- 0.5) * cellsize  # first row, high y

# State index values that correspond to column or rows in order
sel1 <- c(6, 1, 8) # correspond to columns 1, 2, 3 respectively
sel2 <- c( 1, 2, 5, 6, 9) # correspond to rows 1:nrow

rast <- terra::rast(x = full, ext = geom$ext) # SpatRast object



#------------------------------------------------------------------------------#
####  Tests
#------------------------------------------------------------------------------#

test_that("x_to_col works on cell centers", {
  expect_equal(x_to_col(col_center_x, bf), columns )
})
test_that("y_to_row works on cell centers", {
  expect_equal(y_to_row(row_center_y, bf), rows )
})

# Test boundary conditions on x_to_col an y_to_row
# The cell boundary belongs to the the higher index but all values within
# extent should still be in; i.e. xmax, and ymin will fall within
# the last column and row instead of being out.
x_vals <- c(xmin(bf), xmin(bf) + xres(bf), xmax(bf))
expected_cols <- c(1, 2, ncol(bf))
test_that("x_to_col works on cell boundaries", {
  expect_equal(x_to_col(x_vals, bf), expected_cols )
  expect_equal(terra::colFromX(object = rast, x = x_vals),
               expected_cols)
})

y_vals <- c(ymax(bf), ymax(bf) - yres(bf), ymin(bf))
expected_rows <- c(1, 2, nrow(bf))
test_that("y_to_row works on cell boundaries", {
  expect_equal(y_to_row(y_vals, bf), expected_rows )
  expect_equal(terra::rowFromY(object = rast, y = y_vals),
               expected_rows) # check for consistency with terra on boundaries

})
rm( x_vals, y_vals, expected_cols, expected_rows)

test_that("x_to_col and y_to_row throw errors with out of range values", {
  expect_error(x_to_col(xmin(bf) - 0.001, bf))
  expect_error(x_to_col(xmax(bf) + 0.001, bf))
  expect_error(y_to_row(ymin(bf) - 0.001, bf))
  expect_error(y_to_row(ymax(bf) + 0.001, bf))
})

test_that("row_to_y works", {
  expect_equal(row_to_y(1:nrow(bf), bf), row_center_y)
})

test_that("col_to_x works", {
  expect_equal(col_to_x(1:ncol(bf), bf), col_center_x)
})

test_that("i_to_rc works", {
  expect_equal(i_to_rc(vect, bf),
               cbind(i_to_row(vect, bf), i_to_col(vect, bf)),
               ignore_attr = TRUE )
  expect_equal(full[i_to_rc(vect, bf)], vect)
})

test_that("i_to_col, i_to_row work", {
    expect_equal(full[ cbind(i_to_row(vect, bf), i_to_col(vect, bf))],
                 vect)
})

test_that("i_to_x, i_to_y work", {
  expect_equal(i_to_col(sel1, bf), 1:ncol(bf)) # verify sel1
  expect_equal(i_to_x(sel1, bf), col_center_x)  # actual test
  expect_equal(i_to_row(sel2, bf), 1:nrow(bf)) # verify sel2
  expect_equal(i_to_y(sel2, bf), row_center_y ) # actual test
})

test_that("i_to_xy works", {
  expect_equal(i_to_xy(vect, bf),
               cbind(i_to_x(vect, bf), i_to_y(vect, bf)),
               ignore_attr = TRUE)
})

rc <- matrix(c(1, 2, 3, 2, 5, 3), ncol = 2, byrow = T)
vals <- c(1, 4, 9) # values in those cells , same as index for testing objects
test_that("rc_to_i works" , {
  expect_equal(full[rc], vals) # test rc, vals
  expect_equal(rc_to_i(row = rc[, 1], col = rc[, 2], bf), vals)
  expect_equal(rc_to_i(rc, bf = bf), vals)  # matrix passing
})

x <- col_center_x[rc[, 2]]
y <- row_center_y[rc[, 1]]
xy <- cbind(x,y)
test_that("xy_to_i works" , {
  expect_equal(xy_to_i(x = xy[, 1], y = xy[, 2], bf), vals)
  expect_equal(xy_to_i(x = xy, bf = bf), vals)  # matrix passing
})




