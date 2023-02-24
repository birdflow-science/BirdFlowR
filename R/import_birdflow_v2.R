#' import BirdFlow model from an hdf5 file
#'
#' This function imports a BirdFlow model data from an HDF5 file written by
#' python. It works for version 2 hdf5.
#'
#' @param hdf5 Path to an hdf5 file
#' @return a BirdFlow object
#' @importFrom Matrix Matrix
#' @importFrom rhdf5 h5ls
#' @importFrom rhdf5 h5read
import_birdflow_v2 <- function(hdf5){

  stopifnot(file.exists(hdf5))

  #----------------------------------------------------------------------------#
  #   Construct empty object
  #----------------------------------------------------------------------------#
  bf <- new_BirdFlow()

  #----------------------------------------------------------------------------#
  #   Verify contents
  #----------------------------------------------------------------------------#
  if(FALSE){
    # Code to generate expected contents from new_BirdFlow
    list_structure <- function(x){
      if(is.list(x)){
        n <- names(x)
        res <- vector(mode = "list", length = length(n))
        for(i in seq_along(n)){
          res[[i]] <- c(n[i], paste0(n[i], "/", list_index(x[[i]])))
        }
        r <- do.call(c, args = res)
        return(gsub("/$", "", r))
      } else {
        return("")
      }
    }
    expected_contents <- c(list_structure(bf), "marginals")
  }

  # Fixed for V2 (for now)
  expected_contents <- c(
    "geom", "geom/nrow", "geom/ncol", "geom/res", "geom/ext", "geom/crs",
    "geom/mask", "transitions", "transitions", "marginals", "marginals",
    "dates", "dates", "distr", "distr", "species", "species/species_code",
    "species/scientific_name", "species/common_name",
    "species/breeding_quality",
    "species/breeding_start", "species/breeding_end",
    "species/nonbreeding_quality",
    "species/nonbreeding_start", "species/nonbreeding_end",
    "species/postbreeding_migration_quality",
    "species/postbreeding_migration_start",
    "species/postbreeding_migration_end",
    "species/prebreeding_migration_quality",
    "species/prebreeding_migration_start",
    "species/prebreeding_migration_end", "metadata", "metadata/has_marginals",
    "metadata/has_transitions", "metadata/has_distr", "metadata/n_transitions",
    "metadata/n_active", "metadata/n_timesteps", "metadata/ebird_version_year",
    "metadata/ebird_release_year", "metadata/ebird_access_end_date",
    "metadata/birdflow_preprocess_date", "metadata/birdflow_model_date",
    "metadata/is_sparse", "marginals")



  # Check hdf5 for version consistency and missing contents
  contents <- h5ls(hdf5)
  contents <- paste0(contents$group, "/", contents$name)
  contents <- gsub("^/*", "", contents)
  absent <- setdiff(expected_contents, contents)
  extra <- setdiff(contents, expected_contents)

  if(length(absent) != 0){
    stop("hdf5 file:", hdf5, " is missing expected contents '",
         paste(absent, collapse = "', '"), "'")
  }

  # Not al version2 files have a version number!
  expected_version <- 2  # of HDF5 BirdFlow export

  #----------------------------------------------------------------------------#
  #   Process HDF5
  #----------------------------------------------------------------------------#

  # Process geometry
  geom <- h5read(hdf5, name = "geom", native = TRUE)
  for(a in c("nrow", "ncol", "res", "ext")){
    bf$geom[[a]] <- as.numeric(geom[[a]])
  }
  bf$geom$crs <- as.character(geom$crs)
  bf$geom$mask <- geom$mask

  # Process species information
  # They are read as one dimensional arrays (a strange type for R)
  # Here I'm using as.vector to force to a standard vector (of length 1)
  species <- h5read(hdf5, "species")
  for(a in names(bf$species)){
    bf$species[[a]] <- as.vector(species[[a]])
  }

  # Process metadata
  metadata <- h5read(hdf5, "metadata")
  for(a in names(bf$metadata)){
    if(a %in% names(metadata))
      bf$metadata[[a]] <- as.vector(metadata[[a]])
  }

  #Add dates - in pending workflow these will be in the hdf5
  dates <- h5read(hdf5, "dates")
  for(a in colnames(dates)){
    dates[[a]] <- as.vector(dates[[a]])
  }
  bf$dates <- dates


  # Save marginals into list
  marg <- h5read(hdf5, "marginals")  # Not using native but transforming later
  nt <- dim(marg)[3]
  pad <- function(x){
    stringr::str_pad(x, width = nchar(nt), pad = 0)
  }
  nt <- dim(marg)[3]
  circular <- nt == length(unique(dates$date))
  bf$marginals <- vector(mode = "list", length = nt)
  for(i in seq_len(nt)){
    if(circular && i == nt){
      label <- paste0("M_", pad(i), "-",pad(1))
    } else {
      label <- paste0("M_", pad(i), "-",pad(i+1))
    }
    bf$marginals[[i]] <- t(marg[ , , i])
    names(bf$marginals)[i] <- label
  }
  bf$metadata$has_marginals <- TRUE
  bf$metadata$n_transitions <- nt

  # Save distributions
  bf$distr <- h5read(hdf5, "distr", native = TRUE)

  # Cleanup duplicated distribution and date added to input to
  # force circular model fitting.

  # Cleanup duplicated date row
  sv <- duplicated(bf$dates$date)
  if(any(sv)){
    bf$dates <- bf$dates[!sv, ]
  }
  bf$metadata$n_timesteps <- nrow(bf$dates)

  # Delete duplicated distribution
  d <- bf$distr
  if(ncol(d) == n_timesteps(bf) + 1 ){
    if(!all(d[, 1] == d[, ncol(d)]))
      stop("Expected extra distribution to matrch first distribution")
    d <- d[ , 1:(ncol(d) - 1)]
    bf$distr <- d
  }

  # Save marginal index - allows looking up a marginal, and direction from
  # a transition code
  # Columns:
  #   from : timstep
  #    to : timestemp
  #    direction : forward or backward
  #    transition : transition code e.g. ("T_01-02", is directional)
  #    marginal : marginal code e.g. "M_01-02", order follows
  #      forward transition order, so smaller number is generally first
  #      except with the last marginal in a circular model eg "M_52-01"
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
  index$marginal <- NA
  sv <- index$direction == "forward" # selection vector
  index$marginal[sv] <- paste0("M_", pad(index$from[sv]), "-", pad(index$to[sv]))
  sv <- index$direction == "backward"
  index$marginal[sv] <- paste0("M_", pad(index$to[sv]), "-", pad(index$from[sv]))
  bf$marginals$index <- index

  return(bf)
}




