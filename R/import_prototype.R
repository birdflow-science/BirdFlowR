#' Function to import BirdFlow model from the Shiny App
#'
#' This function imports the BirdfFlow model data from the prototype Shiny
#' application. It is a stopgap until a cleaner workflow is developed to export
#' models from python in a standard format that can be read by R.
#'
#' @param base_dir The base directory of the shiny app
#' @param species  The ebird species code for the model
#'
#' @return a BirdFlow object
#' @export
#' @importFrom Matrix Matrix
import_prototype <- function(base_dir, species){


  #----  Global constants from shiny app
  # These are used to lookup paths, and
  # define the temporal extent and resolution
  tile_res <- 150
  ebirdst_version <- 2019 # ebirdst:::ebirdst_version()["data_version"]),
  horizon.length.weeks <- 16
  crop_type <- 'crop5ducks'

  #---- Set paths
  base_dir <- normalizePath(base_dir, winslash = "/")

  # transition matrix directory
  trans_dir <- file.path(base_dir, "models", species, "rds_sparse")
  if(!file.exists(trans_dir))
    stop("Model directory missing: ", trans_dir)

  # Path to raster - used to create mask and define spatial extent
  model_raster_path <- file.path(base_dir, 'tifs', species,
                                 paste0(species,
                                        "-moll-", ebirdst_version,
                                        '-',crop_type,'-',
                                        tile_res,'.tif'))

  stopifnot(file.exists(model_raster_path))

  ##-----  Construct Object

  bf <- list(geom = list(),
             trans = list(),
             dates = NA,
             nt = NA,
             n = NA,
             states = NA,
             metadata = list())  #

  # The model raster, r, contains the species distribution at each timestep
  # Dimensions are row (y), col (x), and time (states)
  # Slicing on the 3rd dimension gives the distribution of the population at
  # the asociated timestep in the model.

  r <- terra::rast(model_raster_path)

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
  #image(mask, useRaster = TRUE)
  # plot(raster(mask))
  bf$geom$mask <- mask

  bf$n <- sum(mask)

  # Convert the model raster into state spaces for each step
  # 1 col per week
  # 1 row per modeled cell
  states <- terra::values(r)[vals]
  states <- matrix(states, ncol = dim(r)[3], nrow = sum(vals))
  states <- Matrix(states, sparse = TRUE)
  bf$states <- states

  if(FALSE){
    # This extracts one row of states and puts it into a matrix for the full
    # extent.  Doing this is awkward because states is a subset of the full
    # matrix in row dominant order and if you assign values to a subset of a
    # matrix in R it fills the values in column dominant order.
    # Filling a matrix and then transposing is one work around.
    s1 <- matrix(nrow = ncol(r), ncol = nrow(r)) # reversed on purpose
    s1[t(vals)] <- states[, 1]
    s1 <- t(s1)
    image(s1, useRaster = TRUE)
    plot(rast(s1))
  }

  tax <- auk::get_ebird_taxonomy()
  stopifnot(species %in% tax$species_code)
  sel <- which(tax$species_code == species)
  bf$metadata <- list(species = tax$common_name[sel],
                      scientific = tax$scientific_name[sel],
                      code = species)

  bf$dates <- get_dates(year = ebirdst_version, n = dim(r)[3])

  bf$nt <- dim(r)[3] - 1
  pb <- progress::progress_bar$new(format = "loading [:bar]:percent",
                                   total = bf$nt * 2)
  pb$tick(0)
  for(i in 1:bf$nt){
    # i is looping through transitions
    # we are going to name based on the starting and ending state
    # e.g. forward_01  becomes T_00_01
    # this leaves us open for larger steps if we want to add them eg:
    #   T_00-01
    pad <- function(x){
      stringr::str_pad(x, width = nchar(bf$nt), pad = 0)
    }
    # Read forward file
    file <- file.path(trans_dir, paste0("forward_", pad(i), ".Rds"))
    label <- paste0("T_", pad(i), "-",pad(i+1))
    bf$trans[[label]] <- Matrix(readRDS(file), sparse = TRUE)
    pb$tick(1)

    # Read backward file for transition
    bfile <- gsub("forward", "backward", file)
    label <- paste0("T_", pad(i+1), "-",pad(i))
    bf$trans[[label]] <- Matrix(readRDS(bfile), sparse = TRUE)
    pb$tick(1)
  }
  pb$terminate()
  class(bf) <- c("BirdFlow", class(bf))
  return(bf)
}



