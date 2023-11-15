# Don't flag S3 methods as having bad names when linting:
# nolint start: object_name_linter.
#' @rdname dimensions
#' @name BirdFlow dimensions
#' @title Dimensions of a BirdFlow object
#' @description
#' Functions to return BirdFlow model dimensions and other basic information
#' @importFrom terra crs ext res xres yres xmin xmax ymin ymax
#' @importMethodsFrom terra crs ext res xres yres xmin xmax ymin ymax
#' @aliases nrow ncol dim ext
#' @param x A BirdFlow object
#' @inheritParams terra::crs
#' @return `nrow()` returns the number of rows in the raster extent of the
#'   BirdFlow model
#' @method nrow BirdFlow
#' @seealso
#' \pkg{terra} defines the S4 generics for [crs()][terra::crs()],
#' [ext()][terra::ext()], [res()][terra::res()], [xres()][terra::xres()], and
#' [yres()][terra::yres()]
#'
#' [get_distr()] returns distributions from a `BirdFlow` object.
#' @export
nrow.BirdFlow <- function(x) {
  x$geom$nrow
}

#' @rdname dimensions
#' @method ncol BirdFlow
#' @export
#' @return `ncol()` number of columns in the raster extent
ncol.BirdFlow <- function(x) {
  x$geom$ncol
}

#' @rdname dimensions
#' @method dim BirdFlow
#' @export
#' @return `dim()` number of rows and columns in the raster extent.
dim.BirdFlow <- function(x) {
  c(x$geom$nrow, x$geom$ncol)
}

#' @rdname dimensions
#' @export
#' @return `n_timesteps()` number of timesteps
#'  distributions.
n_timesteps <- function(x) {
  x$metadata$n_timesteps
}

#' @rdname dimensions
#' @export
#' @return `n_distr()` number of distributions
n_distr <- function(x) {
  ncol(x$distr)
}

#' @rdname dimensions
#' @export
#' @return `n_transitions()` number of transitions, if the model is circular in
#' time this will equal `n_timesteps()`.
n_transitions <- function(x) {
  x$metadata$n_transitions
}

#' @rdname dimensions
#' @export
#' @return `n_active()` number of active cells (locations).
n_active <- function(x) {
  x$metadata$n_active
}


# The section below defines BirdFlow methods for S4 generic functions from
# terra it allows

#  setOldClass("BirdFlow") allows S4 dispatch on S3 BirdFlow objects.
methods::setOldClass("BirdFlow")

crs.BirdFlow <- function(x, proj = FALSE, describe = FALSE, parse = FALSE) {
  terra::crs(x$geom$crs, proj, describe, parse)
}
#' @rdname dimensions
#' @export
#' @return `crs()` the coordinate reference system, a character with well known
#' text (WKT) by default, but see arguments.
setMethod(crs, "BirdFlow", crs.BirdFlow)

ext.BirdFlow <- function(x) {
  terra::ext(x$geom$ext)
}
#' @rdname dimensions
#' @export
#' @return `ext()` a [SpatExtent][terra::SpatExtent] object,
#' which contains the xmin, xmax, ymin, and ymax of the extent.
setMethod(ext, "BirdFlow", ext.BirdFlow)

res.BirdFlow <- function(x) {
  x$geom$res
}
#' @rdname dimensions
#' @return `res()` two numbers, the cell width and height (x and y resolution).
#' @export
setMethod(res, "BirdFlow", res.BirdFlow)

xres.BirdFlow <- function(x) {
  x$geom$res[1]
}
#' @rdname dimensions
#' @return `xres()` the width of the cells (x resolution).
#' @export
setMethod(xres, "BirdFlow", xres.BirdFlow)

yres.BirdFlow <- function(x) {
  x$geom$res[2]
}
#' @rdname dimensions
#' @return `yres()` the height of the cells (y resolution).
#' @export
setMethod(yres, "BirdFlow", yres.BirdFlow)


xmin.BirdFlow <- function(x) {
  xmin(ext(x))
}
#' @rdname dimensions
#' @return `xmin()` minimum x coordinate of extent.
#' @export
setMethod(xmin, "BirdFlow", xmin.BirdFlow)

ymin.BirdFlow <- function(x) {
  ymin(ext(x))
}
#' @rdname dimensions
#' @return `ymin()` minimum y coordinate of extent.
#' @export
setMethod(ymin, "BirdFlow", ymin.BirdFlow)



xmax.BirdFlow <- function(x) {
  xmax(ext(x))
}
#' @rdname dimensions
#' @return `xmax()` maximum x coordinate of extent.
#' @export
setMethod(xmax, "BirdFlow", xmax.BirdFlow)

ymax.BirdFlow <- function(x) {
  ymax(ext(x))
}
#' @rdname dimensions
#' @return `ymax()` maximum y coordinate of extent.
#' @export
setMethod(ymax, "BirdFlow", ymax.BirdFlow)

#' @rdname dimensions
#' @return `is_cyclical()` returns `TRUE` if the BirdFlow model has a transition
#' from the last timestep to the first and `FALSE` otherwise.
#' @export
is_cyclical <- function(x) {
  n_timesteps(x) == n_transitions(x)
}
# nolint end
