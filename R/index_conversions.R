
#'@name index_conversions
#'@rdname index_conversions
#'
#'@title Functions to convert among spacial references
#'
#'@description These functions allow conversions among ways of referencing
#'  values in BirdFlow models and output. Specifically they convert among the
#'  raster spacial coordinates, raster row and col indices, and index
#'  along the state space vector.
#'
#'@param x,y The x and y coordinates associated with the BirdFlow model or data.
#'  This will typically be an easting and northing in meters. A two column
#'  matrix containing x and y columns can also be passed to`x` in which case `y`
#'  should be omitted.
#'@param row,col The row and column index of a cell in the BirdFlow model and
#'  associated raster data. Optionally, a two column
#'  matrix containing row and column indices in columns 1 and 2 respectively
#'  can be passed to `row` in which case `col` should be omitted.
#'@param i The index along a state vector that contains the data for
#'  unmasked cells.
#'@param obj Either a full BirdFlow model or the geom component of one.
#'
#'@return \describe{
#'
#'  \item{`x_to_col(x, obj)` and `y_to_row(y, obj)`}{return the column or row
#'  index that each `x` or `y` coordinate falls within.}
#'
#'  \item{`row_to_y(row, obj)` and `col_to_x(col, obj)`}{return the y or x
#'  coordinate of the center each row or column. }
#'
#'  \item{`i_to_row(i, obj)` and `i_to_col(i, obj)`}{return the row or column
#'  index corresponding to the vector state index, `i`.}
#'
#'  \item{`i_to_rc(i, obj)`}{returns a two column matrix of the row and column
#'  index in the raster corresponding to the index, `i` of the vector state.}
#'
#'  \item{`i_to_x(i, obj)` and `i_to_y(i, obj)`}{return the x or y coordinate
#'  from the vector state index, `i`.}
#'
#'  \item{`i_to_xy(i, obj)`}{returns a two column matrix of the x and y
#'  coordinates corresponding to the index, `i` of the vector state space.}
#'
#'  \item{`rc_to_i(row, col, obj)` and `xy_to_i(x, y, obj)`}{return the state
#'  space index corresponding to x and y coordinates or row and column indices.}
#'  }
#' @seealso
#' * [expand_state] converts a state vector into it's matrix equivalent or
#'   a state matrix (representing multiple states) into an array equivalent.
#' * [rasterize_state] converts a state vector into a `SpatRast` - similar to
#' those created by [terra::rast].
#'
#' @examples

NULL # required object for above roxygen2 page documentation
     # shared by functions below

#' @rdname index_conversions
#' @export
x_to_col <- function(x, obj){
  # Cell boundary belongs to the the higher column index
  # but xmax (eastern most cell boundary) belongs to last column (the lower index)
  if("geom" %in% names(obj)) # allows passing full BirdFlow object
    obj <- obj$geom
  xmin <- obj$ext[1]
  xmax <- obj$ext[2]
  xres <- obj$res[1]
  stopifnot(all(x >= xmin), all(x <= xmax))
  r <- floor((x - xmin)/xres) + 1
  r[x == xmax] <- obj$ncol
  return(r)
}

#' @rdname index_conversions
#' @export
y_to_row <- function(y, obj){
  # Cell boundary belong to the higher row index
  # but ymin belongs to last cell.
  if("geom" %in% names(obj)) # allows passing full BirdFlow object
    obj <- obj$geom
  ymin <- obj$ext[3]
  ymax <- obj$ext[4]
  yres <- obj$res[2]
  stopifnot(all(y >= ymin), all(y <= ymax))
  r <- floor(1 + (ymax - y)/yres)
  r[y == ymin] <- obj$nrow
  return(r)
}

#' @rdname index_conversions
#' @export
row_to_y <- function(row, obj){
  if("geom" %in% names(obj)) # allows passing full BirdFlow object
    obj <- obj$geom
  ymax <- obj$ext[4]
  yres <- obj$res[2]
  return( ymax - (row - 0.5) * yres )
}

#' @rdname index_conversions
#' @export
col_to_x <- function(col, obj){
  if("geom" %in% names(obj)) # allows passing full BirdFlow object
    obj <- obj$geom
  xmin <- obj$ext[1]
  xres <- obj$res[1]
  return( (col - 0.5) * xres + xmin )
}

#' @rdname index_conversions
#' @export
i_to_rc <- function(i, obj){
  if("geom" %in% names(obj)) # allow passing full BirdFlow object
    obj <- obj$geom
  row <- row(obj$mask)
  col <- col(obj$mask)
  rows <- t(row)[t(obj$mask)][i]
  cols <- t(col)[t(obj$mask)][i]
  m <- matrix(c(rows, cols), nrow = length(i), ncol = 2)
  colnames(m) <- c("row", "col")
  return(m)
}

#' @rdname index_conversions
#' @export
i_to_row <- function(i, obj){
  if("geom" %in% names(obj)) # allow passing full BirdFlow object
    obj <- obj$geom
  row <- row(obj$mask)
  return( t(row)[t(obj$mask)][i] )
}
#' @rdname index_conversions
#' @export
i_to_col <- function(i, obj){
  if("geom" %in% names(obj)) # allow passing full BirdFlow object
    obj <- obj$geom
  col_matrix <- col(obj$mask)
  return(t(col_matrix)[t(obj$mask)][i])
}

#' @rdname index_conversions
#' @export
i_to_x <- function(i, obj){
  if("geom" %in% names(obj)) # allow passing full BirdFlow object
    obj <- obj$geom
  col <- i_to_col(i, obj)
  x <- col_to_x(col, obj)
  return(x)
}

#' @rdname index_conversions
#' @export
i_to_y <- function(i, obj){
  if("geom" %in% names(obj)) # allow passing full BirdFlow object
    obj <- obj$geom
  row <- i_to_row(i, obj)
  y <- row_to_y(row, obj)
  return(y)
}

#' @rdname index_conversions
#' @export
i_to_xy <- function(i, obj){
  m <- cbind(i_to_x(i, obj),
             i_to_y(i, obj))
  colnames(m) <- c("x", "y")
  return(m)
}

#' @rdname index_conversions
#' @export
rc_to_i <- function(row, col, obj){
  if("geom" %in% names(obj)) # allow passing full BirdFlow object
    obj <- obj$geom
  # allow passing to row a matrix with two columns to be used as row and col
  if(missing(col)){
    nc <- ncol(row)
    if(!is.null(nc) && !is.na(nc) && nc ==2)   rc <- row
  } else {
    rc <- cbind(row, col)
  }
  index <- matrix(NA, nrow = obj$ncol, ncol = obj$nrow) # intentional reverse
  index[t(obj$mask)] <- 1:sum(obj$mask)
  index <- t(index)
  return(index[rc])
}

#' @rdname index_conversions
#' @export
xy_to_i <- function(x, y, obj){
  if("geom" %in% names(obj)) # allow passing full BirdFlow object
    obj <- obj$geom
  if(missing(y)){
    # allow passing a matrix with two columns as x and y
    nc <- ncol(x)
    if(!is.null(nc) && !is.na(nc) && nc ==2)   rc <- x
  } else {
    rc <- cbind(x, y)
  }
  row <- y_to_row(rc[, 2], obj)
  col <- x_to_col(rc[, 1], obj)
  return(rc_to_i(row, col, obj))
}

