#' Read geom component of a BirdFlow hdf5
#'
#' This internal function is called by [import_birdflow()] and
#' [extend_birdflow()] to read and format the geom component of the model
#' @param hdf5 the path to an hdf5 file
#' @return The geom component of a birdflow model
read_geom <-  function(hdf5) {
  geom <- h5read(hdf5, name = "geom", native = TRUE)
  for (a in c("nrow", "ncol", "res", "ext")) {
    geom[[a]] <- as.numeric(geom[[a]])
  }
  geom$crs <- as.character(geom$crs)
  nbf <- new_BirdFlow()

  # Filter to just standard items in standard order
  geom <- geom[names(nbf$geom)]

  return(geom)
}
