#' Subset `Routes`, `BirdFlowRoutes`, or `BirdFlowIntervals` objects by row
#'
#' @description `[` methods for row-subsetting `Routes`, `BirdFlowRoutes`,
#' and `BirdFlowIntervals` objects. Subsetting works on the underlying `data`
#' component, with the rest of the object (`species`, `metadata`, `geom`,
#' `dates`, `source`) carried over unchanged. `BirdFlowRoutes` are
#' re-validated and have `stay_id`/`stay_len` recomputed on the subsetted
#' data, since those depend on the full sequence of points in a route.
#'
#' @param x A `Routes`, `BirdFlowRoutes`, or `BirdFlowIntervals` object.
#' @param i Row indices, as would be passed to `x$data[i, ]`.
#' @param ... Not used.
#' @return An object of the same class as `x` containing the selected rows.
#' @name subset_routes
#' @examples
#' \donttest{
#' bf <- BirdFlowModels::amewoo
#' bfr <- route(bf, n = 3)
#' bfr[1:5] # BirdFlowRoutes
#'
#' ivl <- as_birdflow_intervals(bfr)
#' ivl[1] # BirdFlowIntervals
#'
#' rts <- as_routes(bfr$data, species = bfr$species)
#' rts[1:5] # Routes
#' }
NULL

#' @rdname subset_routes
#' @export
`[.Routes` <- function(x, i, ...) {
  as_routes(x$data[i, , drop = FALSE], species = x$species, source = x$source)
}

#' @rdname subset_routes
#' @export
`[.BirdFlowRoutes` <- function(x, i, ...) {
  new_birdflow_routes(
    data = x$data[i, , drop = FALSE],
    species = x$species,
    metadata = x$metadata,
    geom = x$geom,
    dates = x$dates,
    source = x$source
  )
}

#' @rdname subset_routes
#' @export
`[.BirdFlowIntervals` <- function(x, i, ...) {
  new_birdflow_intervals(
    data = x$data[i, , drop = FALSE],
    species = x$species,
    metadata = x$metadata,
    geom = x$geom,
    dates = x$dates,
    source = x$source
  )
}
