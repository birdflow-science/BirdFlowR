#' Retrieve the clip polygon used during preprocessing
#'
#' Reconstructs the clip polygon that was supplied to [preprocess_species()]
#' from the metadata stored on the `BirdFlow` object. Returns an
#' [sf][sf::sf] polygon in the model's CRS (`crs(x)`).
#'
#' The polygon is stored as a flat data frame (see [clip_to_dataframe()]) so
#' it can be serialized as part of the model. `get_clip()` rebuilds a
#' [terra::SpatVector][terra::SpatVector-class] internally and converts it
#' to `sf` for return; pass it through `terra::vect()` if you'd prefer a
#' `SpatVector`.
#'
#' @param x A `BirdFlow` model.
#' @return An `sf` object containing the clip polygon, or `NULL` if the
#'   model is unclipped or the clip metadata is unknown (older models
#'   preprocessed before this metadata was captured).
#' @seealso [is_clipped()], [preprocess_species()].
#' @export
#' @examples
#'   library(BirdFlowModels)
#'   get_clip(amewoo) # NULL — fixture predates clip metadata
get_clip <- function(x) {
  stopifnot(inherits(x, "BirdFlow"))
  cl <- x$metadata$clip

  # Legacy / unset metadata: scalar NA at the slot
  if (is.null(cl) || (is.atomic(cl) && length(cl) == 1 && is.na(cl))) {
    return(NULL)
  }

  if (!isTRUE(cl$clipped)) {
    return(NULL)
  }

  df <- cl$polygon
  if (is.null(df) || !inherits(df, "data.frame") || nrow(df) == 0) {
    return(NULL)
  }

  m <- cbind(object = df$id,
             part = df$part,
             x = df$x,
             y = df$y,
             hole = as.integer(df$hole))
  v <- terra::vect(m, type = "polygons", crs = x$geom$crs)
  sf::st_as_sf(v)
}
