#' Reconstruct a clip polygon from its data frame representation
#'
#' Inverse of [clip_to_dataframe()]. Takes the flat data frame stored in a
#' `BirdFlow` object's `metadata$clip$polygon` slot and rebuilds an
#' [sf][sf::sfc] polygon. Factored out of [get_clip()] so the round-trip
#' (polygon -> data frame -> polygon) can be exercised in tests without
#' constructing a full `BirdFlow` object.
#'
#' @param df A data frame as produced by [clip_to_dataframe()] with columns
#'   `id`, `part`, `x`, `y`, and `hole`. The `hole` column is the integer
#'   ring index (`0` for outer rings, `1..N` for each distinct hole within
#'   a part); legacy logical values are coerced via `as.integer()`.
#' @param crs Coordinate reference system. Accepts anything [terra::vect()]
#'   accepts (a WKT string, an `"EPSG:..."` code, an [sf::st_crs()] object,
#'   or `NA` for no CRS).
#' @return An [sfc][sf::sfc] polygon (geometry only, no attributes).
#' @seealso [clip_to_dataframe()], [get_clip()].
#' @keywords internal
dataframe_to_clip <- function(df, crs = NA) {
  if (inherits(crs, "crs")) crs <- crs$wkt
  if (is.null(crs) || (is.atomic(crs) && length(crs) == 1 && is.na(crs))) {
    crs <- ""
  }
  m <- cbind(object = df$id,
             part   = df$part,
             x      = df$x,
             y      = df$y,
             hole   = as.integer(df$hole))
  v <- terra::vect(m, type = "polygons", crs = crs)
  sf::st_geometry(sf::st_as_sf(v))
}
