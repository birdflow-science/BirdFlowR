# nolint start: cyclocomp_linter.
#' import BirdFlow model from an hdf5 file
#'
#' This function imports a BirdFlow model data from an HDF5 file written by
#' python. It works for version 2 hdf5.
#'
#' @param hdf5 Path to an HDF5 file
#' @return a BirdFlow object
#' @importFrom Matrix Matrix
#' @importFrom rhdf5 h5ls
#' @importFrom rhdf5 h5read
#' @keywords internal
import_birdflow_v3 <- function(hdf5) {

  stopifnot(file.exists(hdf5))

  #----------------------------------------------------------------------------#
  #   Construct empty object
  #----------------------------------------------------------------------------#
  bf <- new_BirdFlow()

  #----------------------------------------------------------------------------#
  #   Verify contents
  #----------------------------------------------------------------------------#
  if (FALSE) {
    # Code to generate expected contents from new_BirdFlow
    list_structure <- function(x) {  # nolint: object_usage_linter
      if (is.list(x)) {
        n <- names(x)
        res <- vector(mode = "list", length = length(n))
        for (i in seq_along(n)) {
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

  # Fixed for V3 (for now)
  expected_contents <- c(
    "geom",
    "geom/nrow",
    "geom/ncol",
    "geom/res",
    "geom/ext",
    "geom/crs",
    "geom/mask",
    "transitions",
    "transitions",
    "marginals",
    "dates",
    "dates",
    "distr",
    "species",
    "species/species_code",
    "species/scientific_name",
    "species/common_name",
    "species/breeding_quality",
    "species/breeding_start",
    "species/breeding_end",
    "species/nonbreeding_quality",
    "species/nonbreeding_start",
    "species/nonbreeding_end",
    "species/postbreeding_migration_quality",
    "species/postbreeding_migration_start",
    "species/postbreeding_migration_end",
    "species/prebreeding_migration_quality",
    "species/prebreeding_migration_start",
    "species/prebreeding_migration_end",
    "metadata",
    "metadata/has_marginals",
    "metadata/has_transitions",
    "metadata/has_distr",
    "metadata/hyperparameters",
    "metadata/loss_values",
    "metadata/n_transitions",
    "metadata/n_active",
    "metadata/n_timesteps",
    "metadata/ebird_version_year",
    "metadata/ebird_release_year",
    "metadata/ebird_access_end_date",
    "metadata/birdflow_preprocess_date",
    "metadata/birdflow_model_date",
    "metadata/is_sparse",
    "marginals")

  # Check HDF5 for version consistency and missing contents
  contents <- h5ls(hdf5)
  contents <- paste0(contents$group, "/", contents$name)
  contents <- gsub("^/*", "", contents)
  absent <- setdiff(expected_contents, contents)
  extra <- setdiff(contents, expected_contents) # nolint: object_usage_linter

  if (length(absent) != 0) {
    stop("hdf5 file:", hdf5, " is missing expected contents '",
         paste(absent, collapse = "', '"), "'")
  }

  expected_version <- 3  # of HDF5 BirdFlow export
  version <- as.vector(h5read(hdf5, "metadata/birdflow_version"))
  if (version != expected_version) {
    warning("Reading as version ", expected_version,
            " but file indicates ", version)
  }

  #----------------------------------------------------------------------------#
  #   Process HDF5
  #----------------------------------------------------------------------------#

  # Process geometry
  geom <- h5read(hdf5, name = "geom", native = TRUE)
  for (a in c("nrow", "ncol", "res", "ext")) {
    bf$geom[[a]] <- as.numeric(geom[[a]])
  }
  bf$geom$crs <- as.character(geom$crs)
  bf$geom$mask <- geom$mask
  bf$geom$dynamic_mask <- geom$dynamic_mask

  # Process species information
  # They are read as one dimensional arrays (a strange type for R)
  # Here I'm using as.vector to force to a standard vector (of length 1)
  species <- h5read(hdf5, "species")
  for (a in names(bf$species)) {
    bf$species[[a]] <- as.vector(species[[a]])
  }

  # metadata
  metadata <- h5read(hdf5, "metadata")
  for (a in names(bf$metadata)) {
    if (a %in% names(metadata))
      bf$metadata[[a]] <- as.vector(metadata[[a]])
  }

  # hyperparameters
  hp <- h5read(hdf5, "metadata/hyperparameters")
  # hdf5 seems to store logical as a factor or at least R reads them as such.
  # The code below looks for factors that store logical values and
  # explicitly converts them to logical
  for (i in seq_along(hp)) {
    if (is.factor(hp[[i]]) &&
       all(tolower(levels(hp[[i]])) %in% c("true", "false"))) {
      hp[[i]] <- as.logical(hp[[i]])
    }
  }
  bf$metadata$hyperparameters <- hp

  # loss values
  bf$metadata$loss_values <- as.data.frame(h5read(hdf5, "metadata/loss_values"))

  # dates
  dates <- h5read(hdf5, "dates")
  for (a in colnames(dates)) {
    dates[[a]] <- as.vector(dates[[a]])
  }
  colnames(dates) <- gsub("^week_", "", colnames(dates))
  bf$dates <- dates

  # Save marginals into list
  marg <- h5read(hdf5, "marginals", native = TRUE)
  nt <- length(marg)
  bf$metadata$n_transitions <- nt
  if (is.null(bf$metadata$timestep_padding))
    bf$metadata$timestep_padding <- nchar(nt)
  circular <- nt == length(unique(dates$date))
  bf$marginals <- vector(mode = "list", length = nt)

  # Copy and rename marginals
  for (i in seq_len(nt)) {
    python_label <- paste0("Week", i, "_to_", i + 1)
    if (circular && i == nt) {
      label <- paste0("M_", pad_timestep(i, bf), "-", pad_timestep(1, bf))
    } else {
      label <- paste0("M_", pad_timestep(i, bf), "-", pad_timestep(i + 1, bf))
    }
    bf$marginals[[i]] <- marg[[python_label]]
    names(bf$marginals)[i] <- label
  }
  bf$metadata$has_marginals <- TRUE

  # Save distributions
  bf$distr <- h5read(hdf5, "distr", native = TRUE)

  # Cleanup duplicated distribution, dynamic_mask row, and date added to
  # input to force circular model fitting.

  # Cleanup duplicated date row
  sv <- duplicated(bf$dates$date)
  if (any(sv)) {
    bf$dates <- bf$dates[!sv, ]
  }
  bf$metadata$n_timesteps <- nrow(bf$dates)

  # Delete duplicated distribution
  d <- bf$distr
  if (ncol(d) == n_timesteps(bf) + 1) {
    if (!all(d[, 1] == d[, ncol(d)]))
      stop("Expected extra distribution to matrch first distribution")
    d <- d[, 1:(ncol(d) - 1)]
  }
  dimnames(d) <- list(i = NULL, time = paste0("t", bf$dates$interval))
  bf$distr <- d

  # Delete duplicated dynamic mask row
  dm <- bf$geom$dynamic_mask
  if (ncol(dm) == n_timesteps(bf) + 1) {
    if (!all(dm[, 1] == dm[, ncol(dm)]))
      stop("Expected first and last dynamic mask columns to matrch in circular",
           "BirdFlow model")
    dm <- dm[, 1:(ncol(dm) - 1)]
  }
  dimnames(dm) <- list(i = NULL, time = paste0("t", bf$dates$interval))
  bf$geom$dynamic_mask <- dm

  # Make and save marginal index
  bf$marginals$index <- make_marginal_index(bf)

  return(bf)
}
# nolint end
