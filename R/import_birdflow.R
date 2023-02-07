#' import BirdFlow model from an hdf5 file
#'
#' This function imports a BirdFlow model data from an HDF5 file written by
#' python.
#'
#' @details  The hdf5 file should have
#' * `date` the date the model was exported from python
#' * `densities` the training distributions in their vector forms stored in a
#'    matrix
#' * `dtuple` (metadata list) with items:
#'   - `cells` the number of active (non-masked) cells
#'
#'   -  `nan_mask` a factor matrix of the raster extent with 'TRUE' for masked
#'       cells, and 'FALSE' for unmasked. This is the reverse of the BirdFlow
#'       internal representation which uses TRUE for active cells.
#'    - `weeks` the number of timesteps in the model
#'    - `x_dim` the number of columns in the model raster
#'    - `y_dim` the number of rows in the model raster
#' * `marginals` the marginal probabilities between adjacent timesteps in the
#'      birdflow model. R reads these in column dominant format even though
#'      they are stored in row-dominant so they must be transposed.
#' * `version` an integer indicating the version of the BirdFlow hdf5 file
#'        format.  We will increment this when we change what's in the file.
#'
#' The geoTiff is required for the coordinate reference system (CRS) and the
#' spatial extent. In the future we hope to add this information, along with the
#' species code to the hdf5 file.  Currently the mask and some other attributes
#' from the hdf5 are checked against redundant versions in the geoTiff.
#'
#' @param hdf5 Path to an hdf5 file
#' @param tiff  Path to the model geotiff
#' @param species An eBird species code.  It should appear in the
#'   `species_code` column of the data.frame returned by
#'   [auk::get_ebird_taxonomy()]
#' @return a BirdFlow object
#' @export
#' @importFrom Matrix Matrix
#' @importFrom rhdf5 h5ls
#' @importFrom rhdf5 h5read
import_birdflow <- function(hdf5, tiff, species){

  stopifnot(file.exists(hdf5), file.exists(tiff))

  # Define expected hdf5 contents
  expected_contents <- c("marginals",
                         "version",
                         "dtuple",
                         "densities",
                         "dtuple/cells",
                         "dtuple/nan_mask",
                         "dtuple/weeks",
                         "dtuple/x_dim",
                         "dtuple/y_dim",
                         "date")

  expected_version <- 1  # of HDF5 BirdFlow export

  #----------------------------------------------------------------------------#
  #   Construct empty object
  #----------------------------------------------------------------------------#
  bf <- new_BirdFlow()

  #----------------------------------------------------------------------------#
  #   Process geoTiff
  #----------------------------------------------------------------------------#

  # The model raster, r, contains the species distribution at each timestep
  # Dimensions are row (y), col (x), and time
  # Slicing on the 3rd dimension gives the distribution of the population at
  # the associated timestep in the model.
  # The only information in the raster that isn't currently in the HDF5 is
  #  the extent, cellsize, and the CRS.
  r <- terra::rast(tiff)
  bf$geom <- list(nrow = terra::nrow(r),
                  ncol = terra::ncol(r),
                  res = terra::res(r),
                  ext = as.vector(terra::ext(r)),
                  crs = terra::crs(r),
                  mask = NA)
  vals <-  terra::values(r[[1]])
  vals <- as.vector(!is.na(vals) & !is.nan(vals))
  mask <-matrix(vals,
                nrow = nrow(r), ncol = ncol(r),
                byrow = TRUE)
  bf$geom$mask <- mask
  bf$metadata$n_active <- sum(mask)

  #----------------------------------------------------------------------------#
  #   Process HDF5
  #----------------------------------------------------------------------------#

  # Check hdf5 for version consistency and missing contents
  contents <- h5ls(hdf5)
  contents <- paste0(contents$group, "/", contents$name)
  contents <- gsub("^/*", "", contents)
  absent <- setdiff(expected_contents, contents)
  if(length(absent) != 0){
    stop("hdf5 file:", hdf5, " is missing expected contents '",
         paste(absent, collapse = "', '"), "'")
  }
  version <- h5read(hdf5, "version")
  if(!version == expected_version)
    warning("import_birdflow expects BirdFlow hdf5 version = ",
            expected_version, " but found version = ", version)

  # Check for spatial consistency between HDF5 metadata in dtuple and the TIFF
  md <- h5read(hdf5, "dtuple") # meta data
  stopifnot(bf$n == md$cells)  # consistency in number of active cells
  stopifnot(bf$geom$nrow == md$y_dim)
  stopifnot(bf$feom$ncol == md$x_dim)

  # Convert mask to logical
  # Note: we've already defined mask based on the spatial object above
  #  so here I'm just checking for consistency
  m <- md$nan_mask == "FALSE"   # R Mask has TRUE for cells in analysis.
  m <- matrix(m, nrow = md$y_dim, ncol = md$x_dim, byrow = TRUE)
  stopifnot(sum(m) == bf$n)
  identical(m, bf$geom$mask)

  # Look up taxonomy
  tax <- auk::get_ebird_taxonomy()
  stopifnot(species %in% tax$species_code)
  sel <- which(tax$species_code == species)
  bf$species$species_code <-  species
  bf$species$common_name  <- tax$common_name[sel]
  bf$species$scientific_name <-  tax$scientific_name[sel]

  bf$metadata$birdflow_model_date <- h5read(hdf5, "date")

  # Add species information from ebirdst_runs
  # This is a little hacky because the models are currently
  # From 2019 ebirdst but the runs table is 2021
  # The species are the same though!

  runs <- ebirdst::ebirdst_runs
  si <- as.list(runs[runs$species_code == species(bf, "code"), ])
  si_fields <- intersect(x = names(bf$species), y = names(si))
  for(field in si_fields){
    a <- si[[field]]
    if(lubridate::is.Date(a))
      a <- as.character(a)
    bf$species[[field]] <- a
  }




  # Add dates - in pending workflow these will be in the hdf5
  bf$dates <- get_dates(year = 2019, n = dim(r)[3])

  # Save marginals into list
  marg <- h5read(hdf5, "marginals")
  nt <- dim(marg)[3]
  pad <- function(x){
    stringr::str_pad(x, width = nchar(nt), pad = 0)
  }
  nt <- dim(marg)[3]
  bf$marginals <- vector(mode = "list", length = nt)
  for(i in seq_len(nt)){
    label <- paste0("M_", pad(i), "-",pad(i+1))
    bf$marginals[[i]] <- t(marg[ , , i])
    names(bf$marginals)[i] <- label
    #  bf$marginals[[label]] <- marg[ , , i]
  }
  rm(marg)


  bf$metadata$has_marginals <- TRUE
  bf$metadata$has_transitions <- FALSE
  bf$metadata$has_distr <- TRUE
  bf$metadata$n_transitions <- nt

  # Save distributions
  bf$distr <- h5read(hdf5, "densities")
  bf$metadata$n_timesteps <- ncol(bf$distr)
  dimnames(bf$distr) <- list(i = NULL,
                             timestep = paste0("t", 1:bf$metadata$n_timesteps))

  # Save marginal index - allows looking up a marginal, and direction from
  # a transition code
  # Columns:
  #   from : timstep
  #    to : timestemp
  #    direction : forward or backward
  #    transition : transition code e.g. ("T_01-02", is directional)
  #    marginal : marginal code e.g. "M_01-02", lacks directionality, smaller
  #               number always first
  circular <- n_transitions(bf) == n_timesteps(bf)
  if(circular){
    index <- data.frame(from = 1:n_timesteps(bf), to = c(2:n_timesteps(bf), 1),
                        direction = "forward")
  } else {
    index <- data.frame(from = 1:(n_timesteps(bf) - 1), to = 2:n_timesteps(bf),
                        direction = "forward")
  }
  index <- rbind(index, data.frame(from = index$to, to = index$from,
                                   direction = "backward"))
  index$transition <- paste0("T_", pad(index$from), "-", pad(index$to))
  get_marginal_name <- function(x){
    x <- sort(x)
    paste0("M_", pad(x[1]), "-", pad(x[2]))
  }
  index$marginal <- apply(index[ , c("from", "to")], 1, get_marginal_name )
  bf$marginals$index <- index

  return(bf)
}




