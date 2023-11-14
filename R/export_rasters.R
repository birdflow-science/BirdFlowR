#' export distributions and masks from a BirdFlow object to raster files.
#'
#' This function exports the distributions and/or dynamic masks from a
#' BirdFlow object to raster files (TIFF, PNG).
#'
#' This replaces and extends the old behavior of [preprocess_species()] which
#' allowed exporting TIFF files of the distributions while preprocessing.
#'
#' Files are written via [terra::writeRaster] but the data is manipulated prior
#' to export, in particular if `as_integer` is `TRUE` or if the filetype only
#' supports integers than the distribution data which is normally between 0 and
#' 1 is stretched and then converted to integer.  The amount of stretching can
#' be controlled with `factor`. `as_integer` will be set automatically to TRUE
#' for formats that only export integers  (`PGN`) and `multiband`
#' and `singleband` will be set to `FALSE` and `TRUE` respectively for
#' formats that only support single band output (`PNG`).  If the `filetype`
#' supports multiband files than it is possible to export both multiband and
#' single band by setting both arguments to `TRUE`.
#'
#' @param bf A BirdFlow object
#' @param dir The directory where output should be stored
#' @param crs The coordinate reference system to use in the output files,
#'   defaults to  `crs(bf)`
#' @param multiband If `TRUE` export a multiband file. Will be forced to `FALSE`
#'  if the `filetype` doesn't support multiband rasters.
#' @param singleband If `TRUE` export separate files for each week in the model.
#'  Will be forced to `TRUE`
#' @param what Either `"distr"`, `"mask"` or both as a two element vector.
#'   `what` controls what components of `bf` are
#'    exported.
#' @param filetype The filttype to export to, one
#' `"GTiff"` (for GeoTIFF files), or `"PNG"`
#' @param as_integer Should the data be written as integers. With the default,
#' `NULL`, integers will be written for the PNG filetype as they don't
#' support real numbers and with GeoTIFFs floating point numbers
#' will be written.
#' @param factor To create integer output (`as_integer = TRUE`) the floating
#' point distributions will be multiplied by this number prior to output. If
#' `factor` is `NULL` (the default) then the factor will be x / the maximum
#'  value in any distribution in `bf`, where x is 255 for `PNG`
#'  filetypes and 1000 for `GTiff`.  Thus the maximum integer value will be 255
#'  for PNG files, and 1000 for GeoTiff files.
#'  @param overwrite Should pre-existing files be overwritten with new output.
#'
#' @return Nothing is returned, but raster files are written to `dir`
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#'   bf <- load_model("amewoo_prebreeding")
#'   dir <- tempdir()
#'   crs <-"EPSG:4326"
#'   export_tifs(bf, dir = dir, singleband = TRUE, crs = crs)
#'   export_tifs(bf, dir = dir, singleband = TRUE, crs = crs, filetype = "PNG")
#'
#' }
#'
export_rasters <- function(bf,
                           dir,
                           crs = NULL,
                           multiband = !singleband,
                           singleband = TRUE,
                           what = c("distr", "mask"),
                           filetype = "GTiff",
                           as_integer = NULL,
                           factor = NULL,
                           overwrite = TRUE) {
  stopifnot(all(what %in% c("distr", "mask")))

  integerize_fun <- function(x, factor) {
    x <- x * factor
    x[x < 1 & x != 0] <- 1
    x[, ] <- as.integer(x)
    return(x)
  }

  if (length(filetype) != 1)
    stop("Only one filetype can be exported at a time.")

  # For convenience
  if (filetype == "TIFF")
    filetype == "GTiff"

  # Currently only supporting PNG and GTIFF
  # but leaving some code for JPEG in case I want to add it back in
  # JPEG is problematic because of lossy compression and NA values
  # Empirically a bunch of written NA values were read back in as
  # high, non-NA values.
  filetype <- match.arg(filetype, c("GTiff", "PNG"))


  if (is.null(as_integer)) {
    as_integer <- switch(filetype,
                         "GTiff" = FALSE,
                         "PNG" = TRUE,
                         "JPEG" = TRUE
    )
  }

  if (as_integer && is.null(factor)) {
    dmax <- max(get_distr(bf))

    factor <- switch(filetype,
                     "GTIFF" = 1 / dmax * 1000,
                     "JPEG" = 1 / dmax * 255,
                     "PNG" = 1 / dmax * 255
    )

  }


  if (filetype == "PNG" && (multiband == TRUE || singleband == FALSE)) {
    warning("Filetype \"PNG\" only supports single band images so using ",
            "singleband = TRUE and multiband = FALSE.")
    singleband <- TRUE
    multiband <- FALSE

  }


  extension <- switch(filetype,
                      "GTiff" = ".tif",
                      "PNG" = ".png",
                      "JPEG" = ".jpg")


  if (is.null(crs))
    crs <- crs(bf)

  for (i in seq_along(what)) {

    # Extract mask data
    if (what[i] == "mask") {
      r <- get_dynamic_mask(bf)
      r[, ] <- as.integer(r)
      r <- rasterize_distr(r, bf = bf)
      datatype <- "INT1U"
    }

    # Extract and as_integer distribution data and set export datatype
    if (what[i] == "distr") {
      r <- get_distr(bf)
      if (as_integer) {
        r <- integerize_fun(r, factor = factor)
        if (max(r) < 256) {
          datatype <- "INT1U"
        } else if (max < 65536) {
          datatype <- "INT2U"
        } else {
          datatype <- "INT4u"
        }

      } else { # not integerized
        datatype <- "FLT4S"
      }
      r <- rasterize_distr(distr = r, bf)
    } # End distr == what


    names(r) <- 1:n_timesteps(bf)
    crs <- terra::crs(crs)
    if (crs(bf) != crs) {
      r <- terra::project(r, crs)
    }

    if (multiband) {
      file <- file.path(dir, paste0(species(bf, "code"),
                                    "_", what[i],
                                    extension))
      dir.create(dir, showWarnings = FALSE)
      terra::writeRaster(r, filename = file, filetype = filetype,
                         datatype = datatype, overwrite = overwrite)
    }

    if (singleband) {
      files <-  file.path(
        dir,
        paste0(species(bf, "code"),
               "_", what[i], "_",
               pad_timestep(seq_len(n_timesteps(bf)), bf),
               extension))
      for (j in seq_len(n_timesteps(bf))) {
        terra::writeRaster(r[[j]], file = files[j], filetype = filetype,
                           datatype = datatype, overwrite = overwrite)
      }
    } # end if single band
  } # end loop through what (distr, and/or mask)

  # Write extent as text file
  extent <- ext(r)
  text <- sapply(c("xmin", "xmax", "ymin", "ymax"),
                 FUN = function(x) {
                   paste0(x, " = ",
                          do.call(x, args = list(x = extent)))
                 })
  writeLines(text, file.path(dir, "extent.txt"))

  # Write crs as text file
  text <- as.character(crs(r))
  writeLines(text, file.path(dir, "crs.txt"))

  if (birdflow_options("verbose")) {
    n <- length(list.files(dir))
    cat("Wrote ", n, "files to", dir, "\n")
  }


}  # end function
