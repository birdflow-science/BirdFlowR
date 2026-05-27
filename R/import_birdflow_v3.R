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
#' @importFrom rhdf5 h5closeAll
#' @keywords internal
import_birdflow_v3 <- function(hdf5) {

  stopifnot(file.exists(hdf5))

  # See note in import_birdflow(): close any rhdf5 handles opened by the
  # h5read() calls below, even on error. Closes #197.
  on.exit(h5closeAll(), add = TRUE)

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
    "metadata/n_transitions",
    "metadata/n_active",
    "metadata/n_timesteps",
    "metadata/ebird_version_year",
    "metadata/ebird_release_year",
    "metadata/ebird_access_end_date",
    "metadata/birdflow_preprocess_date",
    "metadata/birdflow_model_date",
    "metadata/is_sparse"
  )

  fit_model_items <- c(
    "metadata/hyperparameters",
    "metadata/loss_values",
    "marginals"
  )


  # Check HDF5 for version consistency and missing contents
  contents <- h5ls(hdf5)
  contents <- paste0(contents$group, "/", contents$name)
  contents <- gsub("^/*", "", contents)

  is_fitted_model <- "marginals" %in% contents
  if (is_fitted_model) {
    expected_contents <- c(expected_contents, fit_model_items)
  }

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
  bf$geom <- read_geom(hdf5)

  # Process species information
  # They are read as one dimensional arrays (a strange type for R)
  # Here I'm using as.vector to force to a standard vector (of length 1)
  species <- h5read(hdf5, "species")
  for (a in names(bf$species)) {
    bf$species[[a]] <- as.vector(species[[a]])
  }

  # Replace empty strings in species with NA
  # The inverse was done while writing to avoid writing NA
  is_empty <- sapply(bf$species, function(x) x == "") |> as.logical()
  bf$species[is_empty] <- NA


  # metadata
  metadata <- h5read(hdf5, "metadata")
  for (a in names(bf$metadata)) {
    if (a %in% names(metadata))
      bf$metadata[[a]] <- as.vector(metadata[[a]])
  }

  # Replace empty strings in metadata with NA of the right class
  is_scalar_empty <- function(x) {
    is.atomic(x) && length(x) == 1 && is.character(x) && x == ""
  }
  empty_elements <- sapply(bf$metadata, is_scalar_empty) |>
    as.logical() |> which()
  ref_md <- new_BirdFlow()$metadata
  for(i in empty_elements) {
    name <- names(bf$metadata)[i]
    if(name %in% names(ref_md)) {
    new_na_value <- switch(class(ref_md[[i]]),
                           "integer" = NA_integer_,
                           "real" = NA_real_,
                           "numeric" = NA_real_,
                           "character" = NA_character_,
                           "logical" = NA,
                           NA)
    } else {
      new_na_value <- NA
    }

    bf$metadata[[i]] <- new_na_value
  }

  # Clean nested clip metadata (introduced after birdflow_version 3 was
  # standardized; older HDF5 files leave bf$metadata$clip as scalar NA via
  # the schema default and are skipped here).
  if (is.list(bf$metadata$clip)) {
    cl <- bf$metadata$clip
    if (!is.null(cl$clipped)) {
      v <- cl$clipped
      if (is.factor(v)) v <- as.logical(as.character(v))
      cl$clipped <- as.logical(v)[1]
    }
    if (inherits(cl$polygon, "data.frame")) {
      cl$polygon <- clean_hdf5_dataframe(cl$polygon)
      if (!is.null(cl$polygon$hole)) {
        # hole is an integer ring index (0 = outer, 1..N = each distinct
        # hole). Legacy files from before #235 stored it as logical;
        # `as.integer()` on a logical maps FALSE -> 0L and TRUE -> 1L,
        # which is correct for the no-hole and single-hole cases the
        # earlier code could round-trip.
        cl$polygon$hole <- as.integer(cl$polygon$hole)
      }
    }
    if (!is.null(cl$percent_lost)) {
      cl$percent_lost <- as.numeric(as.vector(cl$percent_lost))
    }
    bf$metadata$clip <- cl
  }

  # Clean abundance metadata
  if (is.list(bf$metadata$abundance)) {
    ab <- bf$metadata$abundance
    if (!is.null(ab$totals)) {
      ab$totals <- as.numeric(as.vector(ab$totals))
    }
    bf$metadata$abundance <- ab
  }

  # ebird_model_coverage is stored as a 3D logical array
  # [row, col, time]. The simple metadata loop above strips its dim
  # attribute via as.vector(), and rhdf5's default reader transposes
  # arrays on the way back in, so re-read it directly with native =
  # TRUE. rhdf5 doesn't preserve dimnames, so restore them from the
  # array's third dimension.
  if ("metadata/ebird_model_coverage" %in% contents) {
    cov <- h5read(hdf5, "metadata/ebird_model_coverage", native = TRUE)
    if (is.array(cov) && length(dim(cov)) == 3) {
      storage.mode(cov) <- "logical"
      dimnames(cov) <- list(row = NULL,
                            col = NULL,
                            time = paste0("t", seq_len(dim(cov)[3])))
    } else if (is.matrix(cov)) {
      storage.mode(cov) <- "logical"
    } else {
      cov <- as.logical(cov)
    }
    bf$metadata$ebird_model_coverage <- cov
  }


  # hyperparameters
  if (is_fitted_model) {

    hp <- h5read(hdf5, "metadata/hyperparameters")
    # hdf5 seems to store logical as a factor or at least R reads them as such.
    # The code below looks for factors that store logical values and
    # explicitly converts them to logical
    for (i in seq_along(hp)) {
      a <- hp[[i]] # this hyper parameter
      if (is.factor(a) && all(tolower(levels(a)) %in% c("true", "false"))) {
        a <- as.logical(a)
      }
      if (inherits(a, "array")) {
        a <- as.vector(a)
      }
      hp[[i]] <- a
    }
    bf$metadata$hyperparameters <- hp

    # loss values
    lv <- as.data.frame(h5read(hdf5, "metadata/loss_values"))
    for (i in seq_len(ncol(lv))) {
      # IF R re-exports an imported hdf5 the loss values columns are each
      # arrays.  This returns them to standard data.frame columns
      lv[[i]] <- as.vector(lv[[i]])
    }
    bf$metadata$loss_values <- lv

  } # end only for fitted models


  # dates
  dates <- h5read(hdf5, "dates")
  for (a in colnames(dates)) {
    dates[[a]] <- as.vector(dates[[a]])
  }
  colnames(dates) <- gsub("^week_", "", colnames(dates))
  bf$dates <- dates

  if (is_fitted_model) {

    # Save marginals into list
    marg <- h5read(hdf5, "marginals", native = TRUE)
    nt <- length(marg[!names(marg) == "index"])
    bf$metadata$n_transitions <- nt
    if (is.null(bf$metadata$timestep_padding))
      bf$metadata$timestep_padding <- nchar(nt)
    circular <- nt == length(unique(dates$date))
    bf$marginals <- vector(mode = "list", length = nt)

    # If the hdf5 has been re-exported from R, just copy the marginals over
    if ("index" %in% names(marg)) {
      bf$marginals <- marg
    } else {
      # If this hdf5 was written by python then we need to copy and rename
      # marginals
      for (i in seq_len(nt)) {
        python_label <- paste0("Week", i, "_to_", i + 1)
        if (circular && i == nt) {
          label <- paste0("M_", pad_timestep(i, bf), "-", pad_timestep(1, bf))
        } else {
          label <- paste0("M_", pad_timestep(i, bf), "-",
                          pad_timestep(i + 1, bf))
        }
        bf$marginals[[i]] <- marg[[python_label]]
        names(bf$marginals)[i] <- label
      }
      bf$metadata$has_marginals <- TRUE
    }
  }

  # Save distributions
  bf$distr <- h5read(hdf5, "distr", native = TRUE)

  if (is_fitted_model) {
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
        stop("Expected extra distribution to match first distribution")
      d <- d[, 1:(ncol(d) - 1)]
    }

    bf$distr <- d

    # Delete duplicated dynamic mask row
    dm <- bf$geom$dynamic_mask
    if (ncol(dm) == n_timesteps(bf) + 1) {
      if (!all(dm[, 1] == dm[, ncol(dm)]))
        stop("Expected first and last dynamic mask columns to matrch in ",
             "circular BirdFlow model")
      dm <- dm[, 1:(ncol(dm) - 1)]
    }

    bf$geom$dynamic_mask <- dm

    # Per-timestep metadata vectors written by preprocess_species() carry
    # the same cyclical extra entry that distr / dynamic_mask do. Trim it
    # here so each vector ends up at length n_timesteps. The trailing
    # entry is expected to equal the first by construction; mismatch
    # signals a corrupted file, so error rather than silently trimming.
    nt <- n_timesteps(bf)
    if (is.list(bf$metadata$abundance) &&
        is.numeric(bf$metadata$abundance$totals) &&
        length(bf$metadata$abundance$totals) == nt + 1) {
      tot <- bf$metadata$abundance$totals
      if (!isTRUE(all.equal(tot[1], tot[length(tot)])))
        stop("Expected first and last abundance$totals entries to match ",
             "in circular BirdFlow model")
      bf$metadata$abundance$totals <- tot[seq_len(nt)]
    }
    if (is.list(bf$metadata$clip) &&
        is.numeric(bf$metadata$clip$percent_lost) &&
        length(bf$metadata$clip$percent_lost) == nt + 1) {
      pl <- bf$metadata$clip$percent_lost
      if (!isTRUE(all.equal(pl[1], pl[length(pl)])))
        stop("Expected first and last clip$percent_lost entries to match ",
             "in circular BirdFlow model")
      bf$metadata$clip$percent_lost <- pl[seq_len(nt)]
    }
    cov <- bf$metadata$ebird_model_coverage
    if (is.array(cov) && length(dim(cov)) == 3 &&
        dim(cov)[3] == nt + 1) {
      if (!isTRUE(all.equal(cov[, , 1], cov[, , dim(cov)[3]],
                            check.attributes = FALSE)))
        stop("Expected first and last ebird_model_coverage layers to ",
             "match in circular BirdFlow model")
      new_cov <- cov[, , seq_len(nt), drop = FALSE]
      dimnames(new_cov) <- list(row = NULL,
                                col = NULL,
                                time = paste0("t", seq_len(nt)))
      bf$metadata$ebird_model_coverage <- new_cov
    }

    # Make and save marginal index
    bf$marginals$index <- make_marginal_index(bf)

  } # end fitted model only


  # Restore distr and dynamic mask dimnames (lost in hdf5 write+read)
  ts_col <- ifelse(bf$metadata$ebird_version_year < 2022,
                   "interval",
                   "timestep"
  )      ### back compatibility code

  # Set dimnames for distr and dynamic mask
  dn <- list(i = NULL, time = paste0("t", bf$dates[[ts_col]]))
  dimnames(bf$distr) <- dn
  dimnames(bf$geom$dynamic_mask) <- dn

  if (!is_fitted_model) {
    # Need to import some stuff here
    bf$marginals <- NULL
    bf$distances <- as.numeric(h5read(hdf5, "distances"))
    bf$uci <- h5read(hdf5, "uci", native = TRUE)
    bf$lci <- h5read(hdf5, "lci", native = TRUE)
    dimnames(bf$uci) <- dn
    dimnames(bf$lci) <- dn
  }

  # Convert sparse matricies (if present) back into sparse matrices
  # (only relevant if reimporting a previously imported and exported model)
  if (has_marginals(bf) && bf$metadata$is_sparse) {
    mn <- setdiff(names(bf$marginals), "index")
    for (m in mn) {
      bf$marginals[[m]] <- Matrix::Matrix(bf$marginals[[m]], sparse = TRUE)
    }

    # Clean up metadata$sparse attriubtes and order problems

    sparse <- bf$metadata$sparse

    # Restore standard order to sparse if names are as expected
    sparse_order <-
      c("fix_stats", "method", "arguments", "stats",
        "pct_zero", "pct_density_lost")
    if (setequal(sparse_order, names(sparse))) {
      sparse <- sparse[sparse_order]
    }

    # Remove extra attributes hidden in data frame columns and vectors
    for (i in seq_along(bf$metadata$sparse)) {
      if (inherits(sparse[[i]], "data.frame")) {
        sparse[[i]] <- clean_hdf5_dataframe(sparse[[i]])
      } else {
        sparse[[i]] <- as.vector(sparse[[i]])
       }
    }
    # Cleanup extra attributes in argument list
    if ("arguments" %in% names(sparse))
      sparse$arguments <- lapply(sparse$arguments, as.vector)

    bf$metadata$sparse <- sparse
  } # end if sparse

  return(bf)
}
