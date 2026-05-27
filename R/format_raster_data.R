#' Format raster data from a BirdFlow model for output
#'
#' Internal helper shared by [get_mask()] and [get_ebird_coverage()].
#' Handles format-string normalization, alias resolution, and the three
#' output formats.
#'
#' @param data A 2-D logical matrix (e.g. the model mask) or 3-D logical
#'   array (e.g. `ebird_model_coverage`) aligned to the BirdFlow model grid.
#'   For 3-D arrays the third dimension is time; its `dimnames$time` element
#'   is used to name raster layers and the `timestep` column.
#' @param bf A BirdFlow model (supplies grid geometry).
#' @param format A character string naming the output format. Recognized
#'   values (case-insensitive, with aliases):
#'   * `"SpatRaster"` / `"spatrast"` / `"terra"` — [terra::SpatRaster]
#'   * `"numeric"` / `"array"` / `"matrix"` — raw data returned as-is
#'   * `"dataframe"` / `"data.frame"` / `"raster.data.frame"` — long
#'     [data.frame]
#' @param value_name Column name for the data values in `"dataframe"` output
#'   and layer name for single-layer `"SpatRaster"` output.
#' @return Depends on `format`:
#'   * `"spatraster"` — a [terra::SpatRaster]; one layer per time slice for
#'     3-D input, a single named layer for 2-D input.
#'   * `"numeric"` — `data` returned as-is.
#'   * `"dataframe"` — a [data.frame] with columns `row`, `col`, `x`, `y`,
#'     `i` (NA outside the mask), and `<value_name>`. For 3-D input the
#'     frame is in long form (one row per cell × timestep) with an
#'     additional `timestep` column preceding `<value_name>`.
#' @seealso [get_mask()], [get_ebird_coverage()]
#' @keywords internal
format_raster_data <- function(data, bf, format, value_name) {
  format <- tolower(format)
  format <- switch(format,
                   "spatrast"          = "spatraster",
                   "terra"             = "spatraster",
                   "matrix"            = "numeric",
                   "array"             = "numeric",
                   "raster.data.frame" = "dataframe",
                   "data.frame"        = "dataframe",
                   format)
  stopifnot(
    "format must be one of 'SpatRaster', 'numeric', or 'dataframe'" =
      format %in% c("spatraster", "numeric", "dataframe")
  )

  if (format == "numeric") {
    return(data)
  }

  is_3d <- length(dim(data)) == 3

  if (format == "spatraster") {
    if (is_3d) {
      layers <- lapply(seq_len(dim(data)[3]), function(t) {
        terra::rast(data[, , t], extent = bf$geom$ext, crs = bf$geom$crs)
      })
      r <- terra::rast(layers)
      dn <- dimnames(data)
      if (!is.null(dn[["time"]])) names(r) <- dn[["time"]]
    } else {
      r <- terra::rast(data, extent = bf$geom$ext, crs = bf$geom$crs)
      names(r) <- value_name
    }
    return(r)
  }

  # "dataframe": one row per raster cell (× timestep for 3-D)
  rows <- seq_len(nrow(bf))
  cols <- seq_len(ncol(bf))
  base <- expand.grid(row = rows, col = cols)
  base$x <- col_to_x(base$col, bf)
  base$y <- row_to_y(base$row, bf)
  base$i <- rc_to_i(base$row, base$col, bf)

  if (is_3d) {
    dn <- dimnames(data)
    tsteps <- if (!is.null(dn[["time"]])) dn[["time"]] else
      paste0("t", seq_len(dim(data)[3]))
    result <- lapply(seq_along(tsteps), function(t) {
      layer_df <- base
      layer_df$timestep <- tsteps[t]
      layer_df[[value_name]] <- data[, , t][cbind(base$row, base$col)]
      layer_df
    })
    return(do.call(rbind, result))
  }

  base[[value_name]] <- data[cbind(base$row, base$col)]
  base
}
