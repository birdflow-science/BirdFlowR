#' Retrieve, crop, and transform Natural Earth data
#'
#' These are convenience wrappers to \pkg{rnaturalearth} functions. They
#' retrieve, crop, format, and project
#' [Natural Earth](https://www.naturalearthdata.com/)
#' data to facilitate plotting with
#' BirdFlow and other spatial objects. The output is the desired data in the
#' same coordinate reference system (CRS) and extent as `x`.
#'
#'  `get_naturalearth()` does all the work and is called by the other functions.
#'  There are two distinct calculation methods.
#'
#' 1. For Mollweide, Lambert Azimuthal Equal Area,  Albers Equal Area, and
#' Lambert Conformal Conic projections cut at the seam:
#'    - Find the longitude of projection center ("lon_0" in proj4 string) and
#'     from it determine longitude of the seam.
#'    - Clip a narrow (1 m) strip out of the Natural Earth data before
#'     transforming (in WGS84) at the seam.
#'    - Transform to the CRS of `x`. This is now an artifact free object
#'      containing the global data set minus a narrow strip at the seam.
#'    - Crop in destination to the extent of `x`, or if `keep_buffer = TRUE`,
#'      the extent plus the approximate equivalent of `buffer`.
#'
#'    This should work well for any extent (including global) in any CRS that is
#'    based on the covered projections.
#'
#' 1. For all other projections back transform the bounding box and clip:
#'    - Convert the corners of the bounds of `x` object to WGS84.
#'    - adds a buffer (`buffer`) to the converted corners this is important to
#'      guarantee that we still cover the extent after we transform.
#'    - Check to see if the bounds wrap the seam (180 deg meridian) and break
#'      the bounding box into two if it does.
#'    - Crop to the bounding box or boxes.
#'    - Project each cropped section to `x`'s CRS.
#'    - Combine the pieces into one object.
#'    - If `keep_buffer = FALSE` crop to the exact extent of `x`.
#'
#'    These steps will usually prevent artifacts caused when polygons or lines
#'    are shifted across the bounds of the CRS. However, it does not work for
#'    all extents in all projections and in particular is unlikely to work with
#'    polar projections and with extents that cover the entire globe.
#'
#'    In some cases where this fails setting the buffer to zero may be an easy
#'    solution.
#'
#'    There are many more projections where method 1 or a variant on it
#'    would work. We may eventually cover more of those projections with the
#'    first method.
#'
#'    If you encounter a use case that doesn't work you may
#'    [submit an issue](https://github.com/birdflow-science/BirdFlowR/issues);
#'    please include the output from `crs(x)` and `ext(x)`.
#'
#'
#' `get_states()` requires \pkg{rnaturalearthhires}. Install with:\cr
#'  \code{ install.packages("devtools") # if you don't have it already
#'  devtools::install_github("ropensci/rnaturalearthhires") }
#'
#' @param x A BirdFlow, [terra::SpatRaster], [sf::sf][sf], or any other object
#'  on which you can call [terra::ext()] and [terra::crs()].
#' @param type The type of data to retrieve. One of "coastline", "country", or
#'   "states" for data included in \pkg{rnaturalearth}; or any value accepted by
#'   [ne_download()][rnaturalearth::ne_download()].
#' @param scale The scale of data to return. Ignored if type is "states",
#'   otherwise passed to one of [ne_download()][rnaturalearth::ne_download()].
#'   [ne_coastline()][rnaturalearth::ne_coastline()], or
#'   [ne_countries()][rnaturalearth::ne_countries()].
#'   Valid values are 110, 50, 10, 'small', 'medium', and 'large'.
#' @param buffer A buffer in degrees (latitude and longitude) to add to the
#'   extent of `x` prior to cropping the Natural Earth data in WGS84. This is
#'   needed so that after transformation to the CRS of `x` the data cover all
#'   of the extent of `x`.
#' @param keep_attributes If `FALSE`, the default, attribute columns are dropped
#'   to facilitate clean plotting.
#' @param country If retrieving states with `get_states()` or
#'  `get_naturalearth(type = "states")` this is used to select a country. If
#'   omitted, states from all countries are returned.
#' @param ... Other arguments to be passed to
#'   [ne_download()][rnaturalearth::ne_download()]. Possibly you will
#'   want to use `category = "physical"`.
#' @param keep_buffer If `FALSE`, the default, after transforming the Natural
#'   Earth data it will cropped to the precise extent of `x`. Set to  `TRUE`
#'   to keep the buffer - useful when overlaying Natural Earth data
#'   on an existing base R plot.
#' @param force_old_method This is for internal testing. The default should be
#'   best for all other uses.  If `TRUE` use the back transformed bounding box
#'   method even if the projection is covered by the "new" cut at seam method.
#'
#' @return [sf][sf::st_sf] object with Natural Earth data in the same
#'  CRS as `x`.
#'
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
get_naturalearth <- function(x,
                             type,
                             scale = "medium",
                             buffer = 15,
                             keep_attributes = FALSE,
                             country,
                             keep_buffer = FALSE,
                             force_old_method = FALSE,
                             ...) {

  # Turn off using the s2 package for spherical geometry it causes
  # cropping large sections of the globe to fail - specifically if the cropped
  # portion spans more than 180 deg of longitude the crop will be inverted.
  original_use_s2 <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(original_use_s2)))

  #----------------------------------------------------------------------------#
  # Get the specified data via rnaturalearth
  #----------------------------------------------------------------------------#
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
                   if (missing(country)) {
                     rnaturalearth::ne_states(returnclass = "sf")
                   } else {
                     rnaturalearth::ne_states(country = country,
                                              returnclass = "sf")
                   }
                 },
                 rnaturalearth::ne_download(scale = scale, type = type,
                                            returnclass = "sf", ...)
  )

  # Drop attributes if appropriate
  data <- sf::st_make_valid(data)
  if (!keep_attributes) {
    data <- data[, "geometry", drop = FALSE]
  }

  #----------------------------------------------------------------------------#
  # Determine if it's one of the projections we can cut at the seam
  #----------------------------------------------------------------------------#
  proj4 <- terra::crs(x, proj = TRUE)
  projection <- gsub("^.*\\+proj=([[:alpha:]]*)[[:blank:]]*.*$", "\\1",
                     x = proj4, perl = TRUE)

  # These are the ones I've worked out
  seamed_projections <- c("moll", "laea", "aea", "lcc")


  use_seam_method <- projection %in% seamed_projections && !force_old_method

  if (use_seam_method) {
    # This is the newer method that is more robust but only works for specific
    # projections
    data <- cut_at_seam_and_transform(data, x, buffer, keep_buffer)
  } else {
    # This method is more generalized but not as robust
    data <- crop_to_transformed_extent(data, x, buffer)
  }

  if (!keep_buffer) { # crop to precise extent
    poly <- terra::ext(x) |>  sf::st_bbox() |> sf::st_as_sfc()
    data <- sf::st_crop(x = data, y = poly)
  }

  if (nrow(data) == 0)
    warning("No objects within extent. Returning empty sf object.")

  return(data)

}



cut_at_seam_and_transform <- function(data, x, buffer, keep_buffer) {

  km_per_deg <-  111 # at equator. approximate but doesn't need to be exact.
  ft_per_km  <- 3280.84
  proj4 <- terra::crs(x, proj = TRUE)
  #--------------------------------------------------------------------------#
  # Determine clip longitude
  #--------------------------------------------------------------------------#
  lon_0 <- gsub("^.*\\+lon_0=([-]*[[:digit:]\\.]*)[[:blank:]]*.*$", "\\1",
                x = proj4, perl = TRUE)
  lon_0 <- as.numeric(lon_0)
  stopifnot(length(lon_0) == 1, !is.na(lon_0))
  clip_lon <- lon_0 + 180
  if (clip_lon > 180) clip_lon <- clip_lon - 360

  #--------------------------------------------------------------------------#
  # Cut strip out of Natural Earth data at clip longitude
  #--------------------------------------------------------------------------#
  strip_width_km <- .0001 #  approximate km width
  strip_buffer <-  1 / km_per_deg  / 2 * strip_width_km

  clip_bb <-  sf::st_bbox(c(xmin = clip_lon - strip_buffer,
                            xmax = clip_lon + strip_buffer,
                            ymin = -90,
                            ymax = 90))
  clip_poly <- sf::st_as_sfc(clip_bb)
  sf::st_crs(clip_poly) <- sf::st_crs(data)
  data <- suppressMessages(sf::st_difference(data, clip_poly))

  #--------------------------------------------------------------------------#
  #  transform data and clip to approximate buffer
  #  Currently cover to situations:
  #    1. Has +units=m   I expect the most common case
  #    2. Has +to_meter=  seems to be used if the units aren't meters
  #--------------------------------------------------------------------------#
  data <- sf::st_transform(data, terra::crs(x))


  if (!keep_buffer) {  # No need to crop now if it will be full cropped later
    return(data)
  }

  # Crop to buffered extent. In output CRS and units.

  # Figure out buffer in CRS units
  has_units <- grepl("+units=", proj4, fixed = TRUE)
  if (has_units) {
    crs_units <- gsub("^.*\\+units=([-]*[[:alpha:]\\.-]*)[[:blank:]]*.*$",
                      "\\1", x = proj4, perl = TRUE)
    projected_buffer <- switch(
      crs_units,
      "m" = buffer * km_per_deg * 1000,
      "us-ft" = buffer * km_per_deg * ft_per_km,
      stop("get_naturalearth() cannot process CRS with units '", crs_units,
           "' submit an issue to ",
           "(https://github.com/birdflow-science/BirdFlowR) and we can fix ",
           "it."))
  }
  has_to_meter <- grepl("+to_meter=", proj4, fixed = TRUE)
  if (has_to_meter) {
    to_meter <- gsub("^.*\\+to_meter=([-]*[[:alpha:]\\.]*)[[:blank:]]*.*$",
                     "\\1", x = proj4, perl = TRUE)
    to_meter <- as.numeric(to_meter)
    stopifnot(is.numeric(to_meter),
              length(to_meter) == 1,
              !is.na(to_meter)
    )
    projected_buffer <- buffer * km_per_deg * 1000 / to_meter
  }

  if (!has_to_meter && !has_units) {
    stop("Couldn't understand projection units")
  }

  # Crop to buffered extent - in final CR
  e <- terra::ext(x)  # xmin, xmax, ymin, ymax
  e[c(1, 3)] <- e[c(1, 3)] - projected_buffer
  e[c(2, 4)] <- e[c(2, 4)] + projected_buffer
  poly <- sf::st_bbox(e) |> sf::st_as_sfc()
  data <- sf::st_crop(x = data, y = poly)

} # end seam method



crop_to_transformed_extent <- function(data, x, buffer) {
  # This is the first method I implemented it works by transforming
  # the extent of x into the crs of data, buffering, and then cropping the
  # data, and finally transforming the cropped data to match x

  #----------------------------------------------------------------------------#
  # Get buffered bounding box (or boxes if it crosses 180 deg)
  #  for x in natural earth's CRS
  #----------------------------------------------------------------------------#
  ne_crs <- sf::st_crs(data)

  # Project the corners of the boundary box into WGS84
  # The named corners are projected so we can handle the case where the boundary
  # box, after transformation, spans the seam in wgs84

  bb <- sf::st_bbox(ext(x))
  corners <- data.frame(corner = c("ll", "ul", "ur", "lr"),
                        #  ( lower left, upper left, upper right, lower right )
                        x = bb[c(1, 1, 3, 3)],
                        y = bb[c(2, 4, 4, 2)])
  corner_pts <- sf::st_as_sf(corners, coords = c("x", "y"), crs = crs(x))
  corner_pts <- sf::st_transform(corner_pts, crs = ne_crs)
  nc  <- cbind(corners[, "corner", drop = FALSE],
               sf::st_coordinates(corner_pts))

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
  if (xmin > xmax) { # If the original box spans the seam
    bb_list <- list()
    bb_list[[1]] <- sf::st_bbox(c(xmin = xmin - buffer, xmax = 180,
                                  ymin = ymin, ymax = ymax))
    bb_list[[2]] <- sf::st_bbox(c(xmin = -180, xmax = xmax + buffer,
                                  ymin = ymin, ymax = ymax))

    # If overlap and both boxes in middle of the overlap zone
    if (bb_list[[1]][1] < bb_list[[2]][2]) {
      #  if (left edge of right box <  right edge of left box)
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
    if (bb$xmin > -180  && bb$xmax < 180) {
      bb_list <- list(bb)
    }

    # left edge only - two boxes
    if (bb$xmin < -180 && bb$xmax < 180) {   # left edge only
      bb_list <- list()
      left_box <- bb
      left_box[1] <- -180  # xmin
      right_box <- bb   # the wrapped portion is now on the right edge
      right_box[3] <- 180  # xmax
      right_box[1] <- 360 - bb$xmin # xmin
      bb_list <- list(left_box, right_box)
    }

    # right edge only - two boxes
    if (bb$xmin > -180 && bb$xmax > 180) {
      bb_list <- list()
      left_box <- bb
      left_box[1] <- -180  # xmin
      left_box[3] <- bb$xmax - 360

      right_box <- bb
      right_box[3] <- 180  # xmax

      bb_list <- list(left_box, right_box)
    }

    # Both extend over edge just one box with whole x range
    if (bb$xmin < -180 && bb$xmax > 180) {
      bb[1] <- -180
      bb[3] <- 180
      bb_list <- list(bb)
    }

  } # end if original box doesn't span seam

  #----------------------------------------------------------------------------#
  # Crop in natural earth's CRS
  #----------------------------------------------------------------------------#
  cropped <- list()
  for (i in seq_along(bb_list)) {
    poly <- sf::st_as_sfc(bb_list[[i]])
    suppressMessages(suppressWarnings({
      cropped[[i]] <- sf::st_crop(x = data, y = poly)
    }))
  }

  # Combine the different cropped pieces together
  cropped <- suppressWarnings(do.call(rbind, cropped))

  #----------------------------------------------------------------------------#
  # Project
  #----------------------------------------------------------------------------#
  data <- sf::st_transform(cropped, crs(x))

  return(data)
}


#' @rdname get_naturalearth
#' @export
get_states <- function(x, country, scale = "medium", buffer = 15,
                       keep_attributes = FALSE, keep_buffer = FALSE) {
  get_naturalearth(x, type = "states",
                   country =  country,
                   scale = scale,
                   buffer = buffer,
                   keep_attributes = keep_attributes,
                   keep_buffer = keep_buffer)
}

#' @rdname get_naturalearth
#' @export
get_coastline <- function(x, scale = "medium", buffer = 15,
                          keep_attributes = FALSE, keep_buffer = FALSE) {
  get_naturalearth(x, type = "coastline", scale = scale, buffer = buffer,
                   keep_attributes = keep_attributes,
                   keep_buffer = keep_buffer)
}

#' @rdname get_naturalearth
#' @export
get_countries <- function(x, scale = "medium", buffer = 15,
                          keep_attributes = FALSE, keep_buffer = FALSE) {
  get_naturalearth(x, type = "countries", scale = scale, buffer = buffer,
                   keep_attributes = keep_attributes,
                   keep_buffer = keep_buffer)
}
