

export_birdflow <- function(bf, file = NULL,
                            format = "hdf5", overwrite = TRUE) {
  format <- tolower(format) |>  match.arg(c("hdf5", "rds"))

  extension <- switch(format,
                      hdf5 = ".hdf5",
                      rds = ".Rds")

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

  if (!grepl(paste0("\\", extension, "$"), file))
    stop(paste0(
      "If file is not NULL or a directory (ending in a slash) it should end",
         'in ".hdf5" or ".Rds" consistent with the format argument.'))

  if (birdflow_options("verbose"))
    cat("Exporting BirdFlow model for", species(bf), "to:\n\t",
        file, "\n")


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
}
