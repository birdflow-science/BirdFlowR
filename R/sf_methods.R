
# BirdFlow methods for sf functions
#  Currently no help is generated for them


#' @importFrom sf st_crs
#' @method st_crs BirdFlow
#' @export
st_crs.BirdFlow <- function(x, ...) {
  sf::st_crs(x$geom$crs, ...)
}

#' @importFrom sf st_bbox
#' @method st_bbox BirdFlow
#' @export
st_bbox.BirdFlow <- function(obj, ...) {
  bb <- sf::st_bbox(ext(obj), ...)
  sf::st_crs(bb) <- st_crs(obj)
  return(bb)
}
