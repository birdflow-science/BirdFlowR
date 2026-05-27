#' Retrieve the clip polygon used during preprocessing
#'
#' Reconstructs the clip polygon that was supplied to [preprocess_species()]
#' from the metadata stored on the `BirdFlow` object. Returns an
#' [sf][sf::sf] polygon in the model's CRS (`crs(x)`).
#'
#' The polygon is stored as a flat data frame (see [clip_to_dataframe()]) so
#' it can be serialized as part of the model. `get_clip()` delegates the
#' data-frame-to-polygon conversion to [dataframe_to_clip()] and wraps the
#' result in an `sf` object; pass the result through `terra::vect()` if you'd
#' prefer a [terra::SpatVector][terra::SpatVector-class].
#'
#' @param x A `BirdFlow` model.
#' @return An `sf` object containing the clip polygon, or `NULL` if the
#'   model is unclipped or the clip metadata is unknown (older models
#'   preprocessed before this metadata was captured).
#' @seealso [is_clipped()], [preprocess_species()], [dataframe_to_clip()].
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

  sf::st_sf(geometry = dataframe_to_clip(df, crs = x$geom$crs))
}
