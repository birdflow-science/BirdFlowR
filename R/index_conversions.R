#'@name index conversions
#'@rdname index_conversions
#'
#'@title Functions to convert among spatial indices
#'
#'@description These functions allow conversions among different indices for
#'  locations in BirdFlow models and output. Specifically they convert among the
#'  raster spatial coordinates (x and y), raster row and col indices, and the
#'  index `i` along a location vector.
#'
#'@param x,y The x and y coordinates associated with the BirdFlow model or data.
#'  This will typically be an easting and northing in meters. A two column
#'  matrix containing x and y columns can also be passed to `x` in which case
#'  `y` should be omitted.
#'@param row,col The row and column index of a cell in the BirdFlow model and
#'  associated raster data. Alternatively, a two column matrix containing row and
#'  column indices in columns 1 and 2 respectively can be passed to `row` in
#'  which case `col` should be omitted.
#'@param i The index along a state vector that contains the data for
#'  unmasked cells.
#'@param bf a BirdFlow model
#'
#'@return \describe{
#'
#'  \item{`x_to_col(x, bf)` and `y_to_row(y, bf)`}{return the column or row
#'  index that each `x` or `y` coordinate falls within.}
#'
#'  \item{`row_to_y(row, bf)` and `col_to_x(col, bf)`}{return the y or x
#'  coordinate of the center each row or column. }
#'
#'  \item{`i_to_row(i, bf)` and `i_to_col(i, bf)`}{return the row or column
#'  index corresponding to the vector state index, `i`.}
#'
#'  \item{`i_to_rc(i, bf)`}{returns a two column matrix of the row and column
#'  index in the raster corresponding to the index, `i` of the vector state.}
#'
#'  \item{`i_to_x(i, bf)` and `i_to_y(i, bf)`}{return the x or y coordinate
#'  from the vector state index, `i`.}
#'
#'  \item{`i_to_xy(i, bf)`}{returns a two column matrix of the x and y
#'  coordinates corresponding to the index, `i` of the vector state space.}
#'
#'  \item{`rc_to_i(row, col, bf)` and `xy_to_i(x, y, bf)`}{return the state
#'  space index corresponding to x and y coordinates or row and column indices.}
#'  }
#' @seealso
#' * [expand_distr()] converts a vector distribution into it's raster (matrix)
#'   equivalent or a matrix (representing multiple distributions) into an array
#'   equivalent.
#' * [rasterize_distr()] converts a vector distribution into a [terra::SpatRaster] -
#'   similar to those created by [terra::rast()].
#'
NULL # required object for above roxygen2 page documentation
     # shared by functions below

#' @rdname index_conversions
#' @export
x_to_col <- function(x, bf){
  # Cell boundary belongs to the the higher column index
  # but xmax (eastern most cell boundary) belongs to last column (the lower index)
  stopifnot(all(x >= xmin(bf)), all(x <= xmax(bf)))
  r <- floor((x - xmin(bf))/xres(bf)) + 1
  r[x == xmax(bf)] <- ncol(bf)
  return(r)
}

#' @rdname index_conversions
#' @export
y_to_row <- function(y, bf){
  # Cell boundary belong to the higher row index
  # but ymin belongs to last cell.
  stopifnot(all(y >= ymin(bf), all(y <= ymax(bf))))
  r <- floor(1 + (ymax(bf) - y)/yres(bf))
  r[y == ymin(bf)] <- nrow(bf)
  return(r)
}

#' @rdname index_conversions
#' @export
row_to_y <- function(row, bf){
  return( ymax(bf) - (row - 0.5) * yres(bf) )
}

#' @rdname index_conversions
#' @export
col_to_x <- function(col, bf){
  return( (col - 0.5) * xres(bf) + xmin(bf) )
}

#' @rdname index_conversions
#' @export
i_to_rc <- function(i, bf){
  mask <- bf$geom$mask
  row <- row(mask)
  col <- col(mask)
  rows <- t(row)[t(mask)][i]
  cols <- t(col)[t(mask)][i]
  m <- matrix(c(rows, cols), nrow = length(i), ncol = 2)
  colnames(m) <- c("row", "col")
  return(m)
}

#' @rdname index_conversions
#' @export
i_to_row <- function(i, bf){
  if("geom" %in% names(bf)) # allow passing full BirdFlow object
    bf <- bf$geom
  row <- row(bf$mask)
  return( t(row)[t(bf$mask)][i] )
}
#' @rdname index_conversions
#' @export
i_to_col <- function(i, bf){
  if("geom" %in% names(bf)) # allow passing full BirdFlow object
    bf <- bf$geom
  col_matrix <- col(bf$mask)
  return(t(col_matrix)[t(bf$mask)][i])
}

#' @rdname index_conversions
#' @export
i_to_x <- function(i, bf){
  col <- i_to_col(i, bf)
  x <- col_to_x(col, bf)
  return(x)
}

#' @rdname index_conversions
#' @export
i_to_y <- function(i, bf){
  row <- i_to_row(i, bf)
  y <- row_to_y(row, bf)
  return(y)
}

#' @rdname index_conversions
#' @export
i_to_xy <- function(i, bf){
  m <- cbind(i_to_x(i, bf),
             i_to_y(i, bf))
  colnames(m) <- c("x", "y")
  return(m)
}

#' @rdname index_conversions
#' @export
rc_to_i <- function(row, col, bf){
  # allow passing to row a matrix with two columns to be used as row and col
  if(missing(col)){
    nc <- ncol(row)
    if(!is.null(nc) && !is.na(nc) && nc ==2)   rc <- row
  } else {
    rc <- cbind(row, col)
  }
  index <- matrix(NA, nrow = ncol(bf), ncol = nrow(bf)) # intentional reverse
  index[t(bf$geom$mask)] <- 1:n_active(bf)
  index <- t(index)
  return(index[rc])
}

#' @rdname index_conversions
#' @export
xy_to_i <- function(x, y, bf){
  if(missing(y)){
    # allow passing a matrix with two columns as x and y
    nc <- ncol(x)
    if(!is.null(nc) && !is.na(nc) && nc ==2)   rc <- x
  } else {
    rc <- cbind(x, y)
  }
  row <- y_to_row(rc[, 2], bf)
  col <- x_to_col(rc[, 1], bf)
  return(rc_to_i(row, col, bf))
}

