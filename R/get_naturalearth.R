#' retrieve, crop, and transform Natural Earth data
#'
#' These are convenience wrappers to \pkg{rnaturalearth} functions. They
#' retrieve, crop, format, and project
#' [Natural Earth](https://www.naturalearthdata.com/)
#' data to facilitate plotting with
#' BirdFlow objects. The output is the desired data in the same CRS as the
#' BirdFlow object with a somewhat larger extent.
#'
#'  `get_naturalearth()` does all the work and is called by the other functions.
#'  It converts the bounds of the BirdFlow object to WGS84,
#'  adds a buffer to the converted bounds, breaks the bounding box into
#'  two if it crosses the 180 degree meridian in WGS84, crops to the bounding
#'  box or boxes, projects each cropped section to the BirdFlow model's CRS,
#'  and then combines the pieces into one object. These steps will usually
#'  prevent artifacts caused when polygons or lines are shifted across
#'  the bounds of the CRS. However, it does not work for all extents
#'  in all projections and in particular is unlikely to work with polar
#'  projections and with extents that cover the entire globe.
#'
#'  In the case of a full global extent in projections other than WGS84 an
#'  automated solution would need to cut the naturalearth vector data in WGS84
#'  at the edge of the target projection before projecting. I haven't yet
#'  figured out how to generically find that edge.
#'
#'  In some cases where this fail setting the buffer to zero may be an easy
#'  solution.
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

  # Turn off using the s2 package for spherical geometry it causes
  # cropping large sections of the globe to fail - specifically if the cropped
  # portion spans more than 180 deg of longitude the crop will be inverted.
  original_use_s2 <- sf::sf_use_s2()
  suppressMessages( sf::sf_use_s2(FALSE) )
  on.exit( suppressMessages( sf::sf_use_s2(original_use_s2) ) )

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

  # Project the corners of the boundary box into WGS84
  # The named corners are projected so we can handle the case where the boundary
  # box, after transformation, spans the seam in wgs84

  bb <- sf::st_bbox(ext(bf))
  corners <- data.frame( corner = c("ll", "ul", "ur", "lr"),  # lowerleft, upperleft, upperright, lowerright
                         x = bb[c(1, 1, 3, 3)],
                         y = bb[c(2, 4, 4, 2)])
  corner_pts <- sf::st_as_sf(corners, coords = c("x", "y"), crs = crs(bf))
  corner_pts <- sf::st_transform(corner_pts, crs = ne_crs)
  nc  <- cbind(corners[ , "corner", drop = FALSE], sf::st_coordinates(corner_pts))

  # Note if the box spans the edge than xmin might be greater than xmax
  # And we'd want to crop xmin to right edge and left edge to xmax.
  xmin <- min(nc$X[nc$corner %in% c("ll", "ul")])
  xmax <- max(nc$X[nc$corner %in% c("lr", "ur")])
  ymin <- min(nc$Y[nc$corner %in% c("ll", "lr")])
  ymax <- max(nc$Y[nc$corner %in% c("ul", "ur")])


  # Add buffer and constrain y to +- 90
  ymin <- max(-90, ymin - buffer)
  ymax <- min(90, ymax + buffer)

  # Convert into a list of  boundary boxes
  #  There will be two if the box spans the seam
  bb_list <- list()
  if(xmin > xmax){ # If the original box spans the seam
    bb_list <- list()
    bb_list[[1]] <- sf::st_bbox(c(xmin = xmin - buffer, xmax = 180,
                                  ymin = ymin, ymax = ymax))
    bb_list[[2]] <- sf::st_bbox(c(xmin = -180, xmax = xmax + buffer,
                                  ymin = ymin, ymax = ymax))

    # If overlap and both boxes in middle of the overlap zone
    if(bb_list[[1]][1] < bb_list[[2]][2]){  # left edge of right box <  right edge of left box
      xmid <- mean(bb_list[[1]][1], bb_list[[2]][2])
      bb_list[[1]][1] <- xmid
      bb_list[[2]][2] <- xmid
    }

  } else { # Original box didn't span the seam

    bb <- sf::st_bbox(c(xmin = xmin - buffer, xmax = xmax + buffer,
                        ymin = ymin, ymax = ymax))

    # If the buffered box extend past the edges we might still
    # need to break it into multiple boxes

    # Neither edge - fine as is
    if(bb$xmin > -180  && bb$xmax < 180){
      bb_list <- list(bb)
    }

    # left edge only - two boxes
    if(bb$xmin < -180 && bb$xmax < 180 ){   # left edge only
      bb_list <- list()
      left_box <- bb
      left_box[1] <- -180  # xmin
      right_box <- bb   # the wrapped portion is now on the right edge
      right_box[3] <- 180  # xmax
      right_box[1] <- 360 - bb$xmin # xmin
      bb_list <- list(left_box, right_box)
    }

    # right edge only - two boxes
    if(bb$xmin > -180 && bb$xmax > 180 ){
      bb_list <- list()
      left_box <- bb
      left_box[1] <- -180  # xmin
      left_box[3] <- bb$xmax - 360

      right_box <- bb
      right_box[3] <- 180  # xmax

      bb_list <- list(left_box, right_box)
    }

    # Both extend over edge just one box with whole x range
    if(bb$xmin < -180 && bb$xmax > 180 ){
      bb[1] <- -180
      bb[3] <- 180
      bb_list <- list(bb)
    }

  } # end if original box doesn't span seam

  # Drop attributes if appropriate
  data <- sf::st_make_valid(data)
  if(!keep_attributes){
    data <- data[ , "geometry", drop = FALSE]
  }

  # Crop in natural earth's CRS
  cropped <- list()
  for(i in seq_along(bb_list)){
    poly <- sf::st_as_sfc(bb_list[[i]])
    suppressMessages( suppressWarnings({
      cropped[[i]] <- b <- sf::st_crop(x = data, y= poly)
    } ) )
  }

  # Combine the different cropped pieces together
  cropped <-   do.call(rbind, cropped)

  # Project
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

