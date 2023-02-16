#' retrieve, crop, and transform Natural Earth data
#'
#' These are convenience wrappers to \pkg{rnaturalearth} functions. They
#' retrieve, crop, format, and project
#' [Natural Earth](https://www.naturalearthdata.com/)
#' data to facilitate plotting with
#' BirdFlow objects. The output is the desired data in the same CRS as the
#' BirdFlow object with a somewhat larger extent.
#'
#' This function eliminates the hassle of clipping Natural Earth data in its
#'  CRS to bounds from the BirdFlow object and CRS, and then transforming the
#'  data to the BirdFlow CRS, which is the necessary workflow to prevent
#'  artifacts caused when polygons or lines are shifted across the bounds of
#'  the CRS.
#'
#' `get_states()` requires \pkg{rnaturalearthhires}. Install with:\cr
#'  \code{ install.packages("devtools") # if you don't have it already
#'  devtools::install_github("ropensci/rnaturalearthhires") }
#'
#'
#' @param bf BirdFlow object
#' @param type The type of data to retrieve. One of "coastline", "country", or
#'   "states" for data included in \pkg{rnaturalearth}; or any value accepted by
#'   [ne_download()][rnaturalearth::ne_download()].
#' @param scale The scale of data to return. Ignored if type is "states",
#'   otherwise passed to one of [ne_download()][rnaturalearth::ne_download()].
#'   [ne_coastline()][rnaturalearth::ne_coastline()],
#'   [ne_countries()][rnaturalearth::ne_countries()], or
#'   [ne_states()][rnaturalearth::ne_states()].
#'   Valid values are 110, 50, 10, small', 'medium', and 'large'.
#' @param buffer A buffer in degrees (latitude and longitude) to add to the
#'   extent of `bf` prior to cropping the Natural Earth data.
#' @param keep_attributes If `FALSE`, the default, attribute columns are dropped
#'   to facilitate clean plotting.
#' @param country if retrieving states with `get_states()` or
#'  `get_naturalearth(type = "states")` this is used to select a country. If
#'   omitted, states from all countries are returned.
#' @param ... Other arguments to be passed to [ne_download()][rnaturalearth::ne_download()].
#'   Quite possibly you will want to use `category = "physical"`.
#' @return [sf][sf::st_sf] object with the same coordinate reference system
#'   (CRS) as `bf` and (with default `buffer`) a somewhat larger extent.
#' @export
#' @examples
#'  bf <- BirdFlowModels::amewoo
#'  coast <- get_coastline(bf)
#'
#'  \dontrun{
#'  library(terra)
#'  library(sf)
#'  plot(rast(bf, 1))
#'  plot(coast, add = TRUE)   }
get_naturalearth <- function(bf,
                             type,
                             scale = "medium",
                             buffer = 15,
                             keep_attributes = FALSE,
                             country,
                             ... ){

  type <- switch(type,
                 "ne_countries" = "countries",
                 "ne_states" = "states",
                 "ne_coastline" = "coastline",
                 "ne_coastlines" = "coastline",
                 "coastlines" = "coastline",
                 "coast" = "coastline",
                 "country" = "countries",
                 "state" = "states",
                 type)

  data <- switch(type,
                 "countries" = rnaturalearth::ne_countries(scale = scale,
                                                           returnclass = "sf"),
                 "coastline" = rnaturalearth::ne_coastline(scale = scale,
                                                           returnclass = "sf"),
                 "states" = {
                   if(missing(country)){
                     rnaturalearth::ne_states(returnclass = "sf")
                   } else {
                     rnaturalearth::ne_states(country = country,
                                              returnclass = "sf")
                   }
                 },
                 rnaturalearth::ne_download(scale = scale, type = type,
                                            returnclass = "sf", ...)
                 )

  # Get buffered bounding box for bf in natural earth's CRS
  ne_crs <- sf::st_crs(data)
  ext_poly <- sf::st_bbox(ext(bf)) %>% sf::st_as_sfc()
  sf::st_crs(ext_poly) <- crs(bf)
  ext_poly <- sf::st_transform(ext_poly, sf::st_crs(ne_crs))
  bb <- sf::st_bbox(ext_poly)  # xmin, ymin, xmax, ymax (not the order of ext() !)
  bb[1:2] <- bb[1:2] - buffer
  bb[3:4] <- bb[3:4] + buffer

  # Crop in natural earth's CRS to avoid artifacts
  # and then transform into bf projection
  data <- sf::st_make_valid(data)
  if(!keep_attributes){
    data <- data[ , "geometry", drop = FALSE]
  }
  cropped  <- sf::st_crop(x = data, y=  bb)
  data <- sf::st_transform(cropped, crs(bf))
  return(data)

}


#' @rdname get_naturalearth
#' @export
get_states <- function(bf, country, scale = "medium", buffer = 15,
                       keep_attributes = FALSE){
  get_naturalearth(bf, type = "states",  country =  country,
                   scale = scale, buffer = buffer,
                   keep_attributes = keep_attributes)
}

#' @rdname get_naturalearth
#' @export
get_coastline <- function(bf, scale = "medium", buffer = 15,
                          keep_attributes = FALSE){
  get_naturalearth(bf, type = "coastline", scale = scale, buffer = buffer,
                   keep_attributes = keep_attributes)
}

#' @rdname get_naturalearth
#' @export
get_countries <- function(bf, scale = "medium", buffer = 15,
                          keep_attributes = FALSE){
  get_naturalearth(bf, type = "countries", scale = scale, buffer = buffer,
                   keep_attributes = keep_attributes)
}

