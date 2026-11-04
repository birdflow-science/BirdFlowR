#' Format raster data from a BirdFlow model for output
#'
#' Internal helper shared by [get_mask()] and [get_ebird_coverage()].
#' Handles format-string normalization, alias resolution, and the three
#' output formats.
#'
#' @param data A 2-D logical matrix (e.g. the model mask) or 3-D logical
#'   array (e.g. `ebird_coverage`) aligned to the BirdFlow model grid.
#'   For 3-D arrays the third dimension is time; its `dimnames$time`
#'   element supplies the canonical (`"t1"`, `"t2"`, ...) timestep labels,
#'   which are reformatted per
#'   [birdflow_options("time_format")][birdflow_options()] to name raster
#'   layers and populate the `label` column of `"dataframe"` output.
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
#'     3-D input, a single named layer for 2-D input. For 3-D input layer
#'     names follow [birdflow_options("time_format")][birdflow_options()].
#'   * `"numeric"` — `data` returned as-is, except that for 3-D input the
#'     `"time"` dimnames are reformatted per
#'     [birdflow_options("time_format")][birdflow_options()].
#'   * `"dataframe"` — a [data.frame] with columns `row`, `col`, `x`, `y`,
#'     `i` (NA outside the mask), and `<value_name>`. For 3-D input the
#'     frame is in long form (one row per cell × timestep) with two
#'     additional columns preceding `<value_name>`: `timestep`, the
#'     canonical label (e.g. `"t1"`), and `label`, the
#'     [birdflow_options("time_format")][birdflow_options()]-formatted
#'     version (e.g. `"January 4"`).
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

  is_3d <- length(dim(data)) == 3

  canonical_tsteps <- NULL
  formatted_tsteps <- NULL
  if (is_3d) {
    dn <- dimnames(data)
    canonical_tsteps <- if (!is.null(dn[["time"]])) dn[["time"]] else
      paste0("t", seq_len(dim(data)[3]))
    formatted_tsteps <- reformat_timestep(canonical_tsteps, bf)
  }

  if (format == "numeric") {
    if (is_3d) {
      dn <- dimnames(data)
      dn[["time"]] <- formatted_tsteps
      dimnames(data) <- dn
    }
    return(data)
  }

  if (format == "spatraster") {
    if (is_3d) {
      layers <- lapply(seq_len(dim(data)[3]), function(t) {
        terra::rast(data[, , t], extent = bf$geom$ext, crs = bf$geom$crs)
      })
      r <- terra::rast(layers)
      names(r) <- formatted_tsteps
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
    result <- lapply(seq_along(canonical_tsteps), function(t) {
      layer_df <- base
      layer_df$timestep <- canonical_tsteps[t]
      layer_df$label <- formatted_tsteps[t]
      layer_df[[value_name]] <- data[, , t][cbind(base$row, base$col)]
      layer_df
    })
    return(do.call(rbind, result))
  }

  base[[value_name]] <- data[cbind(base$row, base$col)]
  base
}
