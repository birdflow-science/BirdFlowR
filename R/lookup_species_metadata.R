#' Lookup eBird species metadata
#'
#' `lookup_species_metadata()` uses \pkg{ebirdst} to generate
#' a list  identical to the `species` component of a BirdFlow model.
#' It is an internal function used by [preprocess_species()] and [Routes()].
#' See [species_info()] for a description of the list items.
#'
#' @param species An eBird species code, common name, or scientific name. It
#' will be processed by [ebirdst::get_species()].
#' @param skip_checks Set to `TRUE` to skip the checks for eBird model quality
#' and for migrant status. Defaults to `TRUE` - do the tests.
#' @param min_season_quality If `skip_checks = FALSE` and the model
#' quality for any of the four seasons drops below this threshold an error
#' is thrown.
#' @param quiet Set to `TRUE` to suppress messages.
#' @returns A list of species information derived from ebirdst.
#' See [species_info()] for a description of the items.
#' @seealso
#' * [species()] and [species_info()] for getting species information
#' from a BirdFlow model.
#' * [ebirdst::get_species()] to resolve a species name or code.
#' * [ebirdst::ebirdst_runs] for the source of the information.
#' @keywords internal
lookup_species_metadata <- function(species,
                                    skip_checks = FALSE,
                                    min_season_quality = 3,
                                    quiet = FALSE) {
  if (is.na(ebirdst_pkg_ver())) {
    stop("Cannot lookup species metadata unless ebirdst is installed")
  }

  #----------------------------------------------------------------------------#
  # format species metadata                                                 ####
  #----------------------------------------------------------------------------#
  species <- ebirdst::get_species(species) # convert to ebirst species code

  if (is.na(species)) {
    stop("Unable to resolve species: ", species, " with ebirdst::get_species()")
  }

  er <- ebirdst::ebirdst_runs
  names(er)[names(er) == "is_resident"] <- "resident" # restore 2021 name
  spmd <- as.list(er[er$species_code == species, , drop = FALSE])

  if (!quiet) {
    bf_msg("Species resolved to: '", species, "' (", spmd$common_name, ")\n")
  }
  # Reformat dates as strings
  date_to_char <- function(x) {
    if (inherits(x, "Date")) {
      x <- as.character(x)
    }
    return(x)
  }
  spmd <- lapply(spmd, date_to_char)

  # Enforce column formatting
  # in ebirdst 2.2021.1 all columns are stored as characters.
  # it was fixed in 2.2021.3 so this is now extra security against
  # future format changes
  logical_variables <- intersect(
    c(
      "resident",
      "breeding_range_modeled",
      "nonbreeding_range_modeled",
      "postbreeding_migration_range_modeled",
      "prebreeding_migration_range_modeled"
    ),
    names(spmd)
  )

  numeric_variables <- intersect(
    c(
      "breeding_quality",
      "nonbreeding_quality",
      "postbreeding_migration_quality",
      "prebreeding_migration_quality"
    ),
    names(spmd)
  )

  spmd[logical_variables] <- as.logical(spmd[logical_variables])
  spmd[numeric_variables] <- as.numeric(spmd[numeric_variables])


  # used for check prior to ebirds 3.2022.0 and to drop these columns (any v.)
  model_coverage_variables <- c(
    "breeding_range_modeled",
    "nonbreeding_range_modeled",
    "postbreeding_migration_range_modeled",
    "prebreeding_migration_range_modeled"
  )

  # used for checks with ebirdst >= 3.2022.0
  model_quality_variables <- c(
    "breeding_quality",
    "nonbreeding_quality",
    "postbreeding_migration_quality",
    "prebreeding_migration_quality"
  )


  # Check that ebirdst species data supports BirdFlow modeling
  if (!skip_checks) {
    if (spmd$resident) {
      stop(
        spmd$common_name, " (", spmd$species_code, ") is a resident ",
        "(non-migratory) species and is therefore a poor candidate for ",
        "BirdFlow modeling."
      )
    }

    # Check eBird data quality
    if (ebirdst_pkg_ver() < "3.2022.0") {
      if (!all(unlist(spmd[model_coverage_variables]))) {
        stop(
          "eBird status and trends models do not cover the full range for ",
          spmd$common_name, " (", spmd$species_code, ")"
        )
      }
    } else {
      # ebirdst >= 3.2022.0
      if (any(unlist(spmd[model_quality_variables]) < min_season_quality)) {
        stop("eBird status and trends model quality is less than ",
          min_season_quality,
          " in one or more seasons for ",
          spmd$common_name, " (", spmd$species_code, ")",
          sep = ""
        )
      }
    }
  } # end checks

  # Drop the variables that aren't relevant to BirdFlow
  # * The model_coverage_variables were dropped from ebirdst with v 3.2022
  #   and have always been dropped from BirdFlow objects
  # * Resident information dropped b/c we only fit BirdFlow models to migrants
  spmd <- spmd[!names(spmd) %in% model_coverage_variables]
  spmd$resident <- NULL
  spmd$resident_quality <- NULL
  spmd$resident_end <- NULL
  spmd$resident_start <- NULL

  # As of ebirdst 3.2022 we also need to drop trends variables
  spmd[grep("trends", names(spmd))] <- NULL
  spmd$rsquared <- NULL # trends fit quality
  spmd$beta0 <- NULL # trends model intercept


  new_bf <- new_BirdFlow()
  # check contents against new_BirdFlow() for consistency
  stopifnot(all(names(spmd) == names(new_bf$species)))

  return(spmd)
}
