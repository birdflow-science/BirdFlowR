#' Shrink BirdFlow extent
#'
#' @param x A single BirdFlow object, or one or more paths to BirdFlow objects
#' stored as either hdf5 or rds files.
#' @return If `x` is an extended BirdFlow model, see [extend_birdflow()]`
#' than `shrink_birdflow()` returns the same model with its original extent.
#'
#' If `x` is the path to one or more BirdFlow models than those files are
#' shrunk to their original extent and a logical vector of the
#' same length is returned with TRUE for success.
#' @export
#'
#' @examples
#' bf <- BirdFlowModels::amewoo
#'
#' # Define extended extent for example
#' e <-  ext(bf)
#' buffer <- 3 * res(bf)
#' e[1] <- e[1] - buffer[1]
#' e[2] <- e[2] + buffer[1]
#' e[3] <- e[3] - buffer[2]
#' e[4] <- e[4] + buffer[2]
#'
#' bf2 <- extend_birdflow(bf, e)
#'
#' bf3 <- shrink_birdflow(bf2)
#'
#' # Compare extents
#' data.frame(item = names(as.vector(ext(bf))),
#'             initial = as.vector(ext(bf)),
#'             extended = as.vector(ext(bf2)),
#'             shrunk = as.vector(ext(bf3)))
#'
#'\dontrun{
#'# Plot
#'library(terra)
#'plot_distr(get_distr(bf, 1), bf)
#'plot_distr(get_distr(bf2, 1), bf2)
#'plot_distr(get_distr(bf3, 1), bf3)
#'   }
#'
shrink_birdflow <- function(x) {

  if (inherits(x, "BirdFlow")) {
    x$geom <- shrink_geom(x$geom)
    return(x)
  }

  if (inherits(x, "character")) { # file path

    # Handle vectors of file names
    if (length(x) > 1) {
      success <- rep(FALSE, length(x))
      for (i in seq_along(x)) {
        success[i] <- shrink_birdflow(x[i])
      }
      return(success)
    }

    ## Single files

    stopifnot(file.exists(x))

    # hdf5
    # only read, modify, and write the geometry component
    if (grepl("\\.hdf5$", x, ignore.case = TRUE)) {
      geom <- read_geom(x)
      geom <- shrink_geom(geom)
      rhdf5::h5delete(x, name = "geom")
      rhdf5::h5write(geom,
                     file = x,
                     name = "geom",
                     native = TRUE,
                     write.attributes = FALSE,
                     createnewfile = FALSE)
      return(TRUE)
    }


    # Rdata file - read - extend - write
    if (grepl("\\.rds$", x, ignore.case = TRUE)) {
      bf <- readRDS(x)
      bf <- shrink_birdflow(bf)
      saveRDS(bf, file = x)
      return(TRUE)
    }
    stop("if x is not a BirdFlow object it should be the path to an ",
         ".hdf5 or R .rds file.")

  } # end x is path

  stop("x should be a BirdFlow object or a path to a file containing one.")
}



#' Shrink geometry component of a BirdFlow object
#'
#' This is an internal helper function called twice by [shrink_birdflow()]
#' it adjusts the `nrow`, `ncol`, `ext`, and `mask` elements of the
#'`geom` component of a BirdFlow model to the original extent,
#' while preserving the same number, location, and alignment of the
#' unmasked cells - thus nothing else in the object needs to change.
#' If the geometry has not be extended ([shrink_birdflow()], [extend_geom()])
#' then it is returned as is.
#'
#'
#' @param geom  The geometry component of a BirdFlow object
#' @return shrunk (or original) geometry
#' @keywords internal
shrink_geom <- function(geom) {

  e <- geom$ext   # extent
  xr <- geom$res[1] # xres
  yr <- geom$res[2] # yres

  mask <- geom$mask

  # Identify what to keep
  used_rows <- which(apply(mask, 1, any))
  used_cols <- which(apply(mask, 2, any))
  r_min <- min(used_rows)
  r_max <- max(used_rows)
  c_min <- min(used_cols)
  c_max <- max(used_cols)

  # Number of rows to drop from left, right, top, and bottom edges
  left_drop <- c_min - 1
  right_drop <- ncol(mask) - c_max
  top_drop <- r_min - 1
  bottom_drop <- nrow(mask) - r_max

  # Shrink extent
  e[1] <- e[1] + xr * left_drop
  e[2] <- e[2] - xr * right_drop
  e[3] <- e[3] + yr * bottom_drop
  e[4] <- e[4] - yr * top_drop

  # Shrink mask
  mask <- mask[r_min:r_max, c_min:c_max]

  # Update the geom
  geom$mask <- mask
  geom$ncol <- ncol(mask)
  geom$nrow <- nrow(mask)
  geom$ext <- e

  return(geom)
}
