
#' @rdname export_import_birdflow
#' @export
#' @description
#' `export_birdflow()` saves a BirdFlow object to an
#' HDF5 (Hierarchical Data Format version 5)
#' @param bf A BirdFlow model
#' @param file The file name to write. If not supplied or if the path is to
#' a directory the file name will default to
#' `"<species code>_<ebirdst year>_<resolution in (km)>`.
#' @param format The format to export either `"hdf5"` or `"rds"`.
#' @param overwrite The default `TRUE` will delete a preexisting file and then
#' write a new one in it's place.  if `FALSE`, `export_birdflow()` will throw
#' an error if  `file` already exists.
#' @return `export_birdflow()` writes a file and invisibly returns `TRUE`
#' if successful.
export_birdflow <- function(bf, file = NULL,
                            format = "hdf5", overwrite = TRUE) {
  format <- tolower(format) |>  match.arg(c("hdf5", "rds"))

  extension <- switch(format,
                      hdf5 = ".hdf5",
                      rds = ".rds")

  # If file is a directory or missing add a default file name
  # using [sp. code]_[S&T version year]_[res]km
  if (is.null(file) || grepl("/$|\\\\$", file)) {
    if (is.null(file))
      file <- ""
    file <- paste0(file,  species_info(bf, "species_code"), "_",
                   get_metadata(bf, "ebird_version_year"),
                   "_", xres(bf) / 1000, "km", extension)
  }
  file <- normalizePath(file, winslash = "/", mustWork = FALSE)

  if (!grepl(paste0("\\", extension, "$"), file, ignore.case = TRUE))
    stop(paste0(
      "If file is not NULL or a directory (ending in a slash) it should end",
         'in ".hdf5" or ".rds" consistent with the format argument.'))

  bf_msg("Exporting BirdFlow model for", species(bf), "to:\n\t", file, "\n")


  if (file.exists(file) && overwrite) {
    file.remove(file)
  }

  if (file.exists(file) && overwrite)
    stop(file, "already exists and couldn't be deleted.")

  if (file.exists(file))
    stop(file, " already exists.",
         "Set overwrite to TRUE to delete before writing.")

  # Write .Rds
  if (format == "rds") {
    saveRDS(bf, file = file)
    return()
  }

  # Write HDF5
  ns <- names(bf)
  for (i in seq_along(ns)) {
    n <- ns[i]
    rhdf5::h5write(bf[[n]],
                   file = file,
                   name = n,
                   native = TRUE,
                   write.attributes = FALSE,
                   createnewfile = i == 1)  # TRUE for first object
  }

  return(invisible(TRUE))
}
