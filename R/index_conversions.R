#'@name index conversions
#'@rdname index_conversions
#'@aliases index_conversions
#'
#'@title Functions to convert among spatial indices
#'
#'@description These functions convert among different ways of referencing
#'  locations in BirdFlow models and output. They convert among the
#'  spatial coordinates (x and y), raster row and col indices, and the
#'  index `i` along a location vector. `latlon_to_xy()` and `xy_to_latlon()`
#'  convert between WGS 1984 latitude and longitude and x and y coordinates
#'  in a BirdFlow object's coordinate reference system (CRS).
#'
#' @details In general, these functions mirror NA input in output; and
#' functions that return rows, columns, or the index (`i`) will return NA
#' if the input isn't within the extent; or for (`i`) within the active cells.
#'
#' For most CRSs longitude translates to the x coordinate and
#' latitude to the y coordinate. The traditional order of coordinates
#' lat and lon; row and col, and x and y are not consistent, but that is
#' still what is used here, i.e. lat, row, and y are generally correlated,
#' and are first, first, and second in argument order.
#'
#'@param x,y x and y coordinates in the BirdFlow model's CRS.
#'  This typically represents an easting and northing in meters. For functions
#'  that require both `x` and `y` a two column matrix or data.frame
#'  containing `x` and `y` columns in that order can be passed to `x` in
#'  which case `y` should be omitted.
#'@param row,col The row and column index of a cell in the BirdFlow model and
#'  associated raster data. Alternatively, a two column matrix or data.frame
#'  containing row and column indices in columns 1 and 2 respectively can be
#'  passed to `row` in which case the `col` argument should be omitted.
#'@param lat,lon The latitude and longitude in WGS 1984 (EPSG:4326). A two
#'  column matrix or data frame can also be passed to lat.
#'@param i The index along a state vector that contains the data for
#'  unmasked cells.
#'@param bf A BirdFlow model.
#'
#'@return \describe{
#'
#'  \item{`x_to_col(x, bf)` and `y_to_row(y, bf)`}{Return the column or row
#'  index that each `x` or `y` coordinate falls within.}
#'
#'  \item{`row_to_y(row, bf)` and `col_to_x(col, bf)`}{Return the y or x
#'  coordinate of the center each row or column. }
#'
#'  \item{`i_to_row(i, bf)` and `i_to_col(i, bf)`}{Return the row or column
#'  index corresponding to the vector state index, `i`.}
#'
#'  \item{`i_to_rc(i, bf)`}{Returns a two column matrix of the row and column
#'  index in the raster corresponding to the index, `i` of the vector state.}
#'
#'  \item{`i_to_x(i, bf)` and `i_to_y(i, bf)`}{Return the x or y coordinate
#'  from the vector state index, `i`.}
#'
#'  \item{`i_to_xy(i, bf)`}{Returns a two column matrix of the x and y
#'  coordinates corresponding to the index, `i` of the vector state space.}
#'
#'  \item{`rc_to_i(row, col, bf)` and `xy_to_i(x, y, bf)`}{Return the state
#'  space index corresponding to x and y coordinates or row and column indices.}
#'  }
#'
#'  \item{`latlon_to_xy(lat, lon, bf)`}{Returns a two column matrix of the x
#'  and y coordinates corresponding to the supplied latitude and longitude. The
#'  output is in the CRS of `bf` (`crs(bf)`).}
#'
#'  \item{`xy_to_latlon(x, y, bf)`}{Returns a two column matrix of the
#'  latitude and longitude of points in WGS84 given their coordinates in the
#'  BirdFlow object's CRS.}
#'
#' @seealso
#' * [expand_distr()] converts a vector distribution into it's raster (matrix)
#'   equivalent or a matrix (representing multiple distributions) into an array
#'   equivalent.
#' * [rasterize_distr()] converts a vector distribution into a
#'   [terra::SpatRaster] - similar to those created by [terra::rast()].
#'
NULL # required object for above roxygen2 page documentation
# shared by functions below

#' @rdname index_conversions
#' @export
x_to_col <- function(x, bf) {
  # Cell boundary belongs to the the higher column index
  # but xmax (eastern most cell boundary) belongs to last column (the lower
  # index)
  sv <- !is.na(x) & x >= xmin(bf)  &  x <= xmax(bf)
  r <- rep(NA_real_, length(x))
  r[sv] <- floor((x[sv] - xmin(bf)) / xres(bf)) + 1
  r[x == xmax(bf)] <- ncol(bf)
  return(r)
}

#' @rdname index_conversions
#' @export
y_to_row <- function(y, bf) {
  # Cell boundary belong to the higher row index
  # but ymin belongs to last cell.
  sv <- !is.na(y) & y >= ymin(bf) & y <= ymax(bf)
  r <- rep(NA_real_, length(y))
  r[sv] <- floor(1 + (ymax(bf) - y[sv]) / yres(bf))
  r[y == ymin(bf)] <- nrow(bf)
  return(r)
}

#' @rdname index_conversions
#' @export
row_to_y <- function(row, bf) {
  return(ymax(bf) - (row - 0.5) * yres(bf))
}

#' @rdname index_conversions
#' @export
col_to_x <- function(col, bf) {
  return((col - 0.5) * xres(bf) + xmin(bf))
}

#' @rdname index_conversions
#' @export
i_to_rc <- function(i, bf) {
  mask <- bf$geom$mask
  row <- row(mask)
  col <- col(mask)
  rows <- t(row)[t(mask)][i]
  cols <- t(col)[t(mask)][i]
  return(data.frame(row = rows, col = cols))
}

#' @rdname index_conversions
#' @export
i_to_row <- function(i, bf) {
  if ("geom" %in% names(bf)) # allow passing full BirdFlow object
    bf <- bf$geom
  row <- row(bf$mask)
  return(t(row)[t(bf$mask)][i])
}
#' @rdname index_conversions
#' @export
i_to_col <- function(i, bf) {
  if ("geom" %in% names(bf)) # allow passing full BirdFlow object
    bf <- bf$geom
  col_matrix <- col(bf$mask)
  return(t(col_matrix)[t(bf$mask)][i])
}

#' @rdname index_conversions
#' @export
i_to_x <- function(i, bf) {
  col <- i_to_col(i, bf)
  x <- col_to_x(col, bf)
  return(x)
}

#' @rdname index_conversions
#' @export
i_to_y <- function(i, bf) {
  row <- i_to_row(i, bf)
  y <- row_to_y(row, bf)
  return(y)
}

#' @rdname index_conversions
#' @export
i_to_xy <- function(i, bf) {
  data.frame(x = i_to_x(i, bf),
             y = i_to_y(i, bf))
}

#' @rdname index_conversions
#' @export
rc_to_i <- function(row, col, bf) {
  # allow passing to row a matrix with two columns to be used as row and col
  if (missing(col)) {
    nc <- ncol(row)
    if (!is.null(nc) && !is.na(nc) && nc == 2) {
      if (all(c("row", "col") %in% colnames(row))) {
        rc <- row[, c("row", "col")]
      } else {
        rc <- row
        colnames(rc) <- c("row", "col")
      }
    }
    rc <- as.matrix(rc)
  } else {
    rc <- cbind(row, col)
  }


  index <- matrix(NA, nrow = ncol(bf), ncol = nrow(bf)) # intentional reverse
  index[t(bf$geom$mask)] <- 1:n_active(bf)
  index <- t(index)

  i <- rep(NA, nrow(rc))
  sv <- rc[, 1] > 0 & rc[, 1] <= nrow(bf) & rc[, 2] > 0 & rc[, 2] <= ncol(bf)
  sv[is.na(sv)] <- FALSE
  i[sv] <- index[rc[sv, , drop = FALSE]]

  return(i)
}

#' @rdname index_conversions
#' @export
xy_to_i <- function(x, y, bf) {
  if (missing(y)) {
    # allow passing a matrix with two columns as x and y
    nc <- ncol(x)
    if (!is.null(nc) && !is.na(nc) && nc == 2)
      rc <- x
  } else {
    rc <- cbind(x, y)
  }
  row <- y_to_row(rc[, 2], bf)
  col <- x_to_col(rc[, 1], bf)
  return(rc_to_i(row, col, bf))
}

#' @rdname index_conversions
#' @export
latlon_to_xy <- function(lat, lon, bf) {
  if (missing(lon)) {
    # allow passing a matrix with two columns as lat and lon
    nc <- ncol(lat)
    if (!is.null(nc) && !is.na(nc) && nc == 2) {
      if (all(c("lat", "lon") %in% names(lat))) {
        latlon <- lat[, c("lat", "lon")]
      } else { # no names or other names assume first col is lat, second is lon
        latlon <- lat
      }
    }
  } else {
    latlon <- cbind(lat, lon)
  }

  latlon <- as.data.frame(latlon)
  names(latlon) <- c("lat", "lon")

  all_pts <- matrix(NA_real_, nrow = nrow(latlon), ncol = 2)
  sv <-
    !is.na(latlon$lat) &
    !is.na(latlon$lon) &
    latlon$lon >= -180 &
    latlon$lon <= 180 &
    latlon$lat >= -90 &
    latlon$lat <= 90
  pts <-
    sf::st_as_sf(latlon[sv, , drop = FALSE], coords = c("lon", "lat"),
                 crs = sf::st_crs("EPSG:4326")) |>
    sf::st_transform(crs = sf::st_crs(crs(bf))) |>
    sf::st_coordinates()

  all_pts[sv, ] <- pts
  colnames(all_pts) <- c("x", "y")
  return(as.data.frame(all_pts))
}


#' @rdname index_conversions
#' @export
xy_to_latlon <- function(x, y, bf) {
  if (missing(y)) {
    # allow passing a matrix with two columns as lat and lon
    nc <- ncol(x)
    if (!is.null(nc) && !is.na(nc) && nc == 2) {
      xy <- x
    } else if (all(c("x", "y") %in% names(x))) {
      xy <- x[, c("x", "y")]
    } else {
      stop("if y is missing x should contain x and y columns.")
    }
  } else {
    xy <- cbind(x, y)
  }


  xy <- as.data.frame(xy)
  names(xy) <- c("x", "y")

  all_pts <- matrix(NA_real_, nrow = nrow(xy), ncol = 2)
  sv <- !is.na(xy$x) & !is.na(xy$y)

  pts <-
    sf::st_as_sf(xy[sv, , drop = FALSE], coords = c("x", "y"),
                 crs = sf::st_crs(crs(bf))) |>
    sf::st_transform(crs = sf::st_crs("EPSG:4326")) |>
    sf::st_coordinates()

  all_pts[sv, ] <- pts
  colnames(all_pts) <- c("lon", "lat")
  return(as.data.frame(all_pts[, c(2, 1), drop = FALSE]))  # lat, lon
}
