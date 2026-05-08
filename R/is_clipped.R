#' Test whether a BirdFlow model was clipped during preprocessing
#'
#' `is_clipped()` reports whether a clip polygon was supplied to
#' [preprocess_species()] when the model was built.
#'
#' Returns `NA` for models built before clip metadata was recorded — there
#' is no way to recover this information from older HDF5 / RDS files. Use
#' [get_clip()] to retrieve the polygon itself.
#'
#' @param x A `BirdFlow` model.
#' @return A logical scalar: `TRUE` if a clip was applied, `FALSE` if the
#'   model was preprocessed without clipping, or `NA` if the metadata is
#'   missing (legacy models).
#' @seealso [get_clip()], [preprocess_species()].
#' @export
#' @examples
#'   library(BirdFlowModels)
#'   is_clipped(amewoo) # NA — fixture predates clip metadata
is_clipped <- function(x) {
  stopifnot(inherits(x, "BirdFlow"))
  cl <- x$metadata$clip
  if (is.null(cl) || (is.atomic(cl) && length(cl) == 1 && is.na(cl))) {
    return(NA)
  }
  v <- cl$clipped
  if (is.null(v) || (length(v) == 1 && is.na(v))) {
    return(NA)
  }
  as.logical(v)
}
