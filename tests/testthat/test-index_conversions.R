
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
  expect_equal(x_to_col(col_center_x, geom), columns )
})
test_that("y_to_row works on cell centers", {
  expect_equal(y_to_row(row_center_y, geom), rows )
})

# Test boundary conditions on x_to_col an y_to_row
# The cell boundary belongs to the the higher index but all values within
# extent should still be in; i.e. xmax, and ymin will fall within
# the last column and row instead of being out.
xmin <- geom$ext[1]
xmax <- geom$ext[2]
x_vals <- c(xmin, xmin + cellsize, xmax)
expected_cols <- c(1, 2, geom$ncol)
test_that("x_to_col works on cell boundaries", {
  expect_equal(x_to_col(x_vals, geom), expected_cols )
  expect_equal(terra::colFromX(object = rast, x = x_vals),
               expected_cols)
})
ymin <- geom$ext[3]
ymax <- geom$ext[4]
y_vals <- c(ymax, ymax - cellsize, ymin)
expected_rows <- c(1, 2, geom$nrow)
test_that("y_to_row works on cell boundaries", {
  expect_equal(y_to_row(y_vals, geom), expected_rows )
  expect_equal(terra::rowFromY(object = rast, y = y_vals),
               expected_rows) # check for consistency with terra on boundaries

})
rm(xmin, xmax, x_vals, y_vals, ymin, ymax, expected_cols)

test_that("x_to_col and y_to_row throw errors with out of range values", {
  expect_error(x_to_col(geom$ext[1] - 0.001, geom))
  expect_error(x_to_col(geom$ext[2] + 0.001, geom))
  expect_error(y_to_row(geom$ext[3] - 0.001, geom))
  expect_error(y_to_row(geom$ext[4] + 0.001, geom))
})

test_that("row_to_y works", {
  expect_equal(row_to_y(1:geom$nrow, geom), row_center_y)
})

test_that("col_to_x works", {
  expect_equal(col_to_x(1:geom$ncol, geom), col_center_x)
})

test_that("i_to_rc works", {
  expect_equal(i_to_rc(vect, geom),
               cbind(i_to_row(vect, geom), i_to_col(vect, geom)),
               ignore_attr = TRUE )
  expect_equal(full[i_to_rc(vect, geom)], vect)
})

test_that("i_to_col, i_to_row work", {
    expect_equal(full[ cbind(i_to_row(vect, geom), i_to_col(vect, geom))],
                 vect)
})

test_that("i_to_x, i_to_y work", {
  expect_equal(i_to_col(sel1, geom), 1:geom$ncol) # verify sel1
  expect_equal(i_to_x(sel1, geom), col_center_x)  # actual test
  expect_equal(i_to_row(sel2, geom), 1:geom$nrow) # verify sel2
  expect_equal(i_to_y(sel2, geom), row_center_y ) # actual test
})

test_that("i_to_xy works", {
  expect_equal(i_to_xy(vect, geom),
               cbind(i_to_x(vect, geom), i_to_y(vect, geom)),
               ignore_attr = TRUE)
})

rc <- matrix(c(1, 2, 3, 2, 5, 3), ncol = 2, byrow = T)
vals <- c(1, 4, 9) # values in those cells , same as index for testing objects
test_that("rc_to_i works" , {
  expect_equal(full[rc], vals) # test rc, vals
  expect_equal(rc_to_i(row = rc[, 1], col = rc[, 2], geom), vals)
  expect_equal(rc_to_i(rc, obj = geom), vals)  # matrix passing
})

x <- col_center_x[rc[, 2]]
y <- row_center_y[rc[, 1]]
xy <- cbind(x,y)
test_that("xy_to_i works" , {
  expect_equal(xy_to_i(x = xy[, 1], y = xy[, 2], geom), vals)
  expect_equal(xy_to_i(x = xy, obj = geom), vals)  # matrix passing
})




