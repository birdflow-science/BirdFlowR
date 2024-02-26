

#' Extend BirdFlow extent
#'
#' @param x A single BirdFlow object, or one or more paths to BirdFlow objects
#' stored as either hdf5 or R Data files.
#' @param y An extent or an object that yields an extent when passed
#' to [terra::ext()].
#' @return If `x` is a BirdFlow model object `extend_birdflow()` returns an
#' extended version of the same model.  If `x` is the path to one or more
#'  BirdFlow models than those files are modified and a logical vector of the
#'  same length is returned with TRUE for success.
#' @export
#'
#' @examples
#' bf <- BirdFlowModels::amewoo
#'
#' # Define expanded extent for example
#' e <-  ext(bf)
#' buffer <- 3 * res(bf)
#' e[1] <- e[1] - buffer[1]
#' e[2] <- e[2] + buffer[1]
#' e[3] <- e[3] - buffer[2]
#' e[4] <- e[4] + buffer[2]
#'
#' bf2 <- extend_birdflow(bf, e)
#'
#'# Compare initial and expandeded extents
#'data.frame(item = names(as.vector(ext(bf))),
#'             initial = as.vector(ext(bf)),
#'             final = as.vector(ext(bf2)))
#'
#'\dontrun{
#'# Plot both versions
#'library(terra)
#'plot_distr(get_distr(bf, 1), bf)
#'plot_distr(get_distr(bf2, 1), bf2)
#'   }
#'
extend_birdflow <- function(x, y) {

  if (inherits(x, "BirdFlow")) {
    x$geom <- extend_geom(x$geom, y)
    return(x)
  }

  if (inherits(x, "character")) { # file path

    # Handle vectors of file names
    if (length(x) > 1) {
      success <- rep(FALSE, length(x))
      for (i in seq_along(x)) {
        success[i] <- extend_birdflow(x, y)
      }
      return(success)
    }

    ## Single files

    stopifnot(file.exists(bf))

    # hdf5
    # only read, modify, and write the geometry component
    if (grepl("\\.hdf5$", x, ignore.case = TRUE)) {
      geom <- read_geom(x)
      geom <- extend_geom(geom, y)
      rhdf5::h5write(geom,
                     file = x,
                     name = "geom",
                     native = TRUE,
                     write.attributes = FALSE,
                     createnewfile = FALSE)
      return(TRUE)
    }


    # Rdata file - read - extend - write
    if (grepl("\\.Rdata$|\\.Rda$", x)) {
      bf <- import_birdflow(x) |> extend_birdflow(y = y)
      export_birdflow(bf, file = x, overwrite = TRUE)
      return(TRUE)
    }
    stop("x does not appear to be a hdf5 or R data file as expected.")

  } # end x is path

  stop("x should be a BirdFlow object or a path to a file containing one.")
}



#' Extend geometry component of a BirdFlow object
#'
#' This is an internal helper function called twice by [extend_birdflow()]
#' it adjust the nrow, ncol, ext, and mask elements of the
#' geom component of a BirdFlow model to expand the extent while preserving
#' the same number, location, and alignment of the unmasked cells -
#' thus nothing else in the object needs to change.
#'
#' @param geom  The geometry component of a BirdFlow object
#' @param y An object that returns an extent when passed to [terra::ext()],
#' this can be an extent, a SpatRaster, or a BirdFlow model.
#' @return extended geometry (covering larger area)
#' @keywords internal
extend_geom <- function(geom, y) {

  eg  <- geom$ext  # extent of geom
  ey <- ext(y)  # extent of y
  if(!all(c(
    isTRUE(all.equal(ey[1] %% geom$res[1], 0, check.attributes = FALSE)),
    isTRUE(all.equal(ey[2] %% geom$res[1], 0, check.attributes = FALSE)),
    isTRUE(all.equal(ey[3] %% geom$res[2], 0, check.attributes = FALSE)),
    isTRUE(all.equal(ey[4] %% geom$res[2], 0, check.attributes = FALSE))))) {

    stop("The new extent (y) does not align with cells in the ",
         "BirdFlow model (x) ", call. = FALSE)

  }

  # Check that new extent completely contains old extent
  stopifnot(all(ey[c(1, 3)] <= eg[c(1, 3)]))
  stopifnot(all(ey[c(2, 4)] >= eg[c(2, 4)]))

  geom$ext <- as.numeric(as.vector(ext(y)))

  x_mask <- get_mask(bf, format = "SpatRaster")
  r <- terra::extend(x = x_mask, y = ey, fill = FALSE) # raster
  geom$mask <- matrix(as.logical(values(r, mat = FALSE)),
                   nrow = nrow(r),
                   ncol = ncol(r),
                   byrow = TRUE)
  geom$ncol <- ncol(r)
  geom$nrow <- nrow(r)
  return(geom)
}
