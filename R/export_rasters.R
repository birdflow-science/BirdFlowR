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
#' @param dir The directory where output should be stored. Can include
#'   aliases; see `mb_file`,`sb_file` below.
#' @param crs The coordinate reference system to use in the output files,
#'   defaults to  `crs(bf)`
#' @param multiband If `TRUE` export a multiband file. Will be forced to `FALSE`
#'  if the `filetype` doesn't support multiband rasters.
#' @param singleband If `TRUE` export separate files for each week in the model.
#'  Will be forced to `TRUE`
#' @param what Either `"distr"`, `"mask"` or both as a two element vector.
#'   `what` controls what components of `bf` are
#'    exported.
#' @param filetype The file type to export: `"GTiff"` for GeoTIFF files,
#' or `"PNG"` for Portable Network Graphics.
#' @param as_integer Should the data be written as integers. With the default,
#' `NULL`, integers will be written for the PNG file type as they don't
#' support real numbers and with GeoTIFFs floating point numbers
#' will be written.
#' @param factor To create integer output (`as_integer = TRUE`) the floating
#' point distributions will be multiplied by this number prior to output. If
#' `factor` is `NULL` (the default) then the factor will be x / the maximum
#'  value in any distribution in `bf`, where x is 255 for `PNG`
#'  filetypes and 1000 for `GTiff`.  Thus the maximum integer value will be 255
#'  for PNG files, and 1000 for GeoTIFF files.
#' @param overwrite Should pre-existing files be overwritten with new output.
#' @param mb_file,sb_file, The multi-band and single-band file name templates.
#'   They control where files are written. Possible aliases are:
#'   * `<ext>` the file extention, required at end of template.
#'   * `<code>` the species code.
#'   * `<common>` the species common name, spaces will be replaced with `"_"`
#'   * `<scientific>` the scientic name, spaces will be replaced with `"_"`
#'   * `<ts>` timestep (without padding)
#'   * `<p_ts>` padded timestep e.g. `"03"`
#'   * `<date>` date in format `year-month-day`  e.g. `"2024-03-14"`
#'   * `<what>` will be one of "distr" or "mask" can be omitted if only one
#'      is to be output.  See `what` argument above.
#'
#'   The two metadata files (CRS, extent) are written using the
#'   multi-band template with `<what>` set
#'   to `"crs"` and  `"extent"`, unless `<what>` isn't in the file name
#'   in which case `"_crs"` and `"_extent"` will be inserted prior to the
#'   extension.
#'
#'   `sb_file` must include one of `<ts>`, `<p_ts>` or `<date>`.
#'
#'   The aliases above may be used in `dir` as well.

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
                           overwrite = TRUE,
                           mb_file = "<code>_<what>.<ext>",
                           sb_file = "<code>_<what>_<p_ts>.<ext>"
                           ) {
  stopifnot(all(what %in% c("distr", "mask")))

  files_written <- 0

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
                     "GTiff" = 1 / dmax * 1000,
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
                      "GTiff" = "tif",
                      "PNG" = "png",
                      "JPEG" = "jpg")



  substitute_aliases <- function(file, bf, ts, what, extension){

    if (!grepl("\\.<ext>$", file))
      stop("File templates should end in \".<ext>\"")

    # handle special case where what is "crs" or "extent" and "<what>"
    # is missing from file template, so the two metadata
    # files don't overwrite each other
    if (what %in% c("crs", "extent") && !grepl("<what>", file, fixed = TRUE)) {
      gsub("\\.*<ext>$", paste0("_", what, ".<ext>"))
    }

    file <- gsub("<code>", species(bf, "code"), file, fixed = TRUE)
    file <- gsub("<common>", species(bf), file, fixed = TRUE)
    file <- gsub("<scientific>", species(bf, "scientific"), file, fixed = TRUE)
    file <- gsub("<ext>", extension, file, fixed = TRUE)
    file <- gsub("<what>", what, file, fixed = TRUE)
    file <- gsub("<ts>", ts, file, fixed = TRUE)
    file <- gsub("<p_ts>", pad_timestep(ts, bf), file, fixed = TRUE)
    file <- gsub("<date>", lookup_date(ts, bf), file, fixed = TRUE)
    file
  }


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
        maxr <- max(r)
        if (maxr < 256) {
          datatype <- "INT1U"
        } else if (maxr < 65536) {
          datatype <- "INT2U"
        } else {
          datatype <- "INT4u"
        }

      } else { # not integerized
        datatype <- "FLT4S"
      }
      r <- rasterize_distr(distr = r, bf)
    } # End distr == what


    names(r) <- 1:n_distr(bf)
    crs <- terra::crs(crs)
    if (crs(bf) != crs) {
      r <- terra::project(r, crs)
    }

    # Drop extra distribution from preprocessed models
    if(!has_marginals(bf) && !has_transitions(bf) && n_distr(bf) == 53){
      r <- r[[-53]]
    }


    if (multiband) {
      file <- file.path(dir, mb_file)
      file <- substitute_aliases(file, bf, 0, what[i], extension)
      dir.create(dirname(file), showWarnings = FALSE)

      terra::writeRaster(r, filename = file, filetype = filetype,
                         datatype = datatype, overwrite = overwrite)
      files_written <- files_written + 1
    }

    if (singleband) {

      for (j in seq_len(n_timesteps(bf))) {
        file <-  file.path(dir, sb_file)
        file <- substitute_aliases(file, bf, j, what[i], extension)
        dir.create(dirname(file), showWarnings = FALSE)

        terra::writeRaster(r[[j]], filename = file, filetype = filetype,
                           datatype = datatype, overwrite = overwrite)
        files_written <- files_written + 1
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
  what <- "extent"
  extension <- "txt"
  file <-  file.path(dir, mb_file)
  file <- substitute_aliases(file, bf, 0, what, extension)
  dir.create(dirname(file), showWarnings = FALSE)

  writeLines(text, file)
  files_written <- files_written + 1


  # Write crs as text file
  text <- as.character(crs(r))
  what <- "crs"
  extension <- "txt"
  file <-  file.path(dir, mb_file)
  file <- substitute_aliases(file, bf, 0, what, extension)
  dir.create(dirname(file), showWarnings = FALSE)
  writeLines(text, file)
  files_written <- files_written + 1

  bf_msg("Wrote ", files_written, " files to ", dir, "\n")


}  # end function
