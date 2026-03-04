#' Rasterize BMTR Data
#'
#' Converts a BMTR dataframe into a multi-layer raster,
#' with each unique transition as a separate layer.
#'
#' The `x` and `y` coordinates are mapped to raster columns and rows.
#'
#' @param bmtr A dataframe returned by `calc_bmtr`, containing
#' columns `x`, `y`, `transition`, `bmtr`, and `date`.
#' @param bf A BirdFlow model. For example: `BirdFlowModels::amewoo`
#'
#' @return A `SpatRaster` object (from `terra`) with
#' layers corresponding to unique transitions.
#' Missing values in `bmtr` are represented as `NA`.
#'
#' @examples
#' \dontrun{
#' raster <- rasterize_bmtr(bmtr, bf)
#' plot(raster)
#' }
#'
#' @export
rasterize_bmtr <- function(bmtr, bf) {

  bmtr$col <- bmtr$x |> x_to_col(bf)
  bmtr$row <- bmtr$y |> y_to_row(bf)

  transitions <- unique(bmtr$transition)

  arr <- array(
    NA_real_,
    dim = c(nrow(bf), ncol(bf), length(transitions))
  )

  for (k in seq_along(transitions)) {
    tr <- transitions[k]
    traffic <- bmtr[bmtr$transition == tr, ]
    arr[cbind(traffic$row, traffic$col, k)] <- traffic$bmtr
  }

  r <- terra::rast(arr, ext = ext(bf))

  names(r) <- transitions
  r
}
