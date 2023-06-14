
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










if(FALSE){
  # Code taken from route() and stashed here.  It makes lines from points
  lines <- convert_route_to_sf(points)
  sf::st_crs(lines) <- sf::st_crs(crs(bf))
  return(list(points = points, lines = lines))
}



# Internal helper function to
# Make x and y vectors into lines
convert_to_lines <- function(x, y) {
  sf::st_linestring(cbind(x, y), "XY")
}




#' @importFrom sf st_as_sf
#' @method st_as_sf BirdFlowRoutes
#' @export
st_as_sf.BirdFlowRoutes <- function(x, type = "line", crs = NULL, ...){
  type <- match.arg(type, c("line", "point"))

  if(is.null(crs)){
    a <- attributes(x)
    if("geom" %in% names(a)){
      crs <- a$geom$crs
      cat("Set crs based on geom attribute.\n")
    } else if ("crs" %in%  names(a)){
      crs <- a$crs
    }
  }

  if(is.null(crs)){
    stop("The coordinate reference system must be defined in the object attributes or via the crs argument.")
  }
  crs <- sf::st_crs(crs)

  if(type == "line"){
    lines <-   x |>
      dplyr::group_by(.data$route_id) |>
      dplyr::summarize(
        geometry = sf::st_geometry(convert_to_lines(.data$x, .data$y))) |>
      as.data.frame() |>
      sf::st_as_sf()
    sf::st_crs(lines) <- crs
    return(lines)
  }

  if(type == "point"){
    x <- as.data.frame(x)
    points <-  sf::st_as_sf(x, coords = c("x", "y"), crs = crs )
    return(points)
  }


}
