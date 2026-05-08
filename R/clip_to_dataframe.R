#' Convert a clip polygon to a flat data frame for storage
#'
#' Internal helper that converts a polygon (as `SpatVector`, `sf`, or anything
#' [terra::vect()] accepts) into a flat data frame suitable for serializing
#' inside a `BirdFlow` object's metadata. The data frame has columns
#' `id` (polygon / object index), `part` (ring index within polygon), `x`,
#' `y` (vertex coordinates), and `hole` (`TRUE` for hole rings, `FALSE` for
#' outer rings). The schema mirrors the matrix that [terra::vect()] accepts
#' when `type = "polygons"`, which is what [get_clip()] uses to round-trip
#' the polygon back into spatial form.
#'
#' Storing the polygon as plain columns avoids serializing terra/sf objects
#' (which hold C++ pointers / environments and don't survive `saveRDS()` or
#' HDF5 round-trips reliably).
#'
#' @param clip A polygon, in any form accepted by [terra::vect()].
#' @return A data frame with columns `id`, `part`, `x`, `y`, `hole`.
#' @keywords internal
clip_to_dataframe <- function(clip) {
  if (!inherits(clip, "SpatVector")) {
    clip <- terra::vect(clip)
  }
  g <- terra::geom(clip, df = TRUE)
  data.frame(
    id = as.integer(g$geom),
    part = as.integer(g$part),
    x = as.numeric(g$x),
    y = as.numeric(g$y),
    hole = as.logical(g$hole)
  )
}
