
# BirdFlow methods for sf functions

#' Get the coordinate reference system of a BirdFlow model
#'
#' @description
#' `st_crs()` method for `BirdFlow` objects. Returns the coordinate
#' reference system stored in the model's geometry (`x$geom$crs`).
#'
#' @param x A `BirdFlow` object.
#' @param ... Not used.
#' @return An [sf::st_crs()] object (class `crs`) describing the
#' coordinate reference system of `x`.
#' @export
#' @importFrom sf st_crs
#' @method st_crs BirdFlow
#' @examples
#' \donttest{
#' bf <- BirdFlowModels::amewoo
#' sf::st_crs(bf)
#' }
st_crs.BirdFlow <- function(x, ...) {
  sf::st_crs(x$geom$crs, ...)
}

#' Get the bounding box of a BirdFlow model
#'
#' @description
#' `st_bbox()` method for `BirdFlow` objects. Returns the spatial extent
#' of the model's raster geometry as an `sf` bounding box, with the
#' model's coordinate reference system attached.
#'
#' @param obj A `BirdFlow` object.
#' @param ... Not used.
#' @return An [sf::st_bbox()] object (class `bbox`) with the
#' coordinate reference system of `obj` attached.
#' @export
#' @importFrom sf st_bbox
#' @method st_bbox BirdFlow
#' @examples
#' \donttest{
#' bf <- BirdFlowModels::amewoo
#' sf::st_bbox(bf)
#' }
st_bbox.BirdFlow <- function(obj, ...) {
  bb <- sf::st_bbox(ext(obj), ...)
  sf::st_crs(bb) <- st_crs(obj)
  return(bb)
}

#' Convert BirdFlowRoutes to an sf object
#'
#' @description
#' `st_as_sf()` method for `BirdFlowRoutes` objects. Converts the routes
#' to either line geometries (one line per route, `type = "line"`) or
#' point geometries (one point per route location, `type = "point"`).
#'
#' @param x A `BirdFlowRoutes` object.
#' @param type Either `"line"` (the default), to return one line per
#' route, or `"point"`, to return one point per row of route data.
#' @param crs The coordinate reference system to assign to the result.
#' If `NULL` (the default) the CRS is taken from `x$geom$crs`
#' (or `x$crs`, if `x` has no `geom` component).
#' @param ... Not used.
#' @return An `sf` object: `LINESTRING` geometries (one row per route)
#' if `type = "line"`, or `POINT` geometries (one row per route
#' location) if `type = "point"`.
#' @export
#' @importFrom sf st_as_sf
#' @method st_as_sf BirdFlowRoutes
#' @examples
#' \donttest{
#' bf <- BirdFlowModels::amewoo
#' rts <- route(bf, n = 3)
#' sf::st_as_sf(rts, type = "line")
#' sf::st_as_sf(rts, type = "point")
#' }
st_as_sf.BirdFlowRoutes <- function(x, type = "line", crs = NULL, ...) {
  type <- match.arg(type, c("line", "point"))

  if (is.null(crs)) {
    if ("geom" %in% names(x)) {
      crs <- x$geom$crs
      bf_msg("Set crs based on geom component.\n")
    } else if ("crs" %in% names(x)) {
      crs <- x$crs
    }
  }
  if (is.null(crs)) {
    stop("The coordinate reference system must be defined in the object ",
         "or via the crs argument.")
  }
  crs <- sf::st_crs(crs)
  if (type == "line") {
    lines <-   x$data |>
      dplyr::group_by(.data$route_id) |>
      dplyr::summarize(
        geometry = sf::st_geometry(convert_to_lines(.data$x, .data$y))) |>
      as.data.frame() |>
      sf::st_as_sf()
    sf::st_crs(lines) <- crs
    return(lines)
  }
  if (type == "point") {
    x <- as.data.frame(x$data)
    points <-  sf::st_as_sf(x, coords = c("x", "y"), crs = crs)
    return(points)
  }
}

# Internal helper function to
# Make x and y vectors into lines
convert_to_lines <- function(x, y) {
  sf::st_linestring(cbind(x, y), "XY")
}
