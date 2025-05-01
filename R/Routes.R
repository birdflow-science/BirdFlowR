# Public functions for creating Routes and BirdFlowRoutes

#' `Routes()` is used to convert data frames containing bird movement
#' data into a formal `Routes` object with the same data.
#'
#' @param data A data frame with data on bird movement. Likely
#' tracking, banding, or Motus data on real birds; or possibly
#' synthetic versions of the same.It must have the following columns:
#' \describe{
#' \item{`date`}{Date or Date Time object of class `Date`, `POSIXlt`,
#'  or `POSIXct`}
#' \item{`lat`,`lon`}{The latitude and longitude of the location in
#' WGS84 (EPSG::4326)}
#' \item{`route_type`}{The type of route - one of
#' `"tracking"`, `"banding"`, `"motus"`, `"unknown"`, or `"synthetic"`
#' Types can be mixed in the column.
#' }
#' }
#' Other columns are permitted and will be retained in `Routes` object
#' but dropped if they are converted to `BirdFlowRoutes`.
#'
#' @param species Either: a character scalar suitable for use with
#' [ebirdst::get_species()]; or a list with
#' species metadata which must include `common_name` and can optionally
#' also include `scientific_name` and  `species_code` and any other standard
#' BirdFlow species metadata. See [species_info()] for a description
#' of the full list. Note list input is not checked against eBird species
#' codes and names. Scalar input is preferred unless the species does not
#' conform to eBird's taxonomy.
#' @param source Optional text describing the source of the data.
#' `source()` must be of class `character` can have one or more elements.
#' @inheritParams lookup_species_metadata
#' @returns An object of class `Routes` which has the following components
#' \item{data}{A data frame with the input `data`}
#' \item{species}{A list with, at a minimum items
#' `common_name`,  `scientific_name`, and `species_code` and depending on
#' the `species` argument potentially having all the items returned by
#' [lookup_species_metadata()]}
#' \item{source}{Same as the input `source`}
#'
#' @export
Routes <- function(data, species = NULL, source = NULL, skip_checks=FALSE, min_season_quality = 3) {
  # Check input
  stopifnot(is.data.frame(data))
  validate_Routes_route_df(data)

  # Resolve species
  if (!is.list(species) && !is.null(species) && !is.na(species) &&
     length(species == 1)) {
    species <- lookup_species_metadata(species, quiet = TRUE, skip_checks, min_season_quality)
  } else {
    if (!is.list(species) || !"common_name" %in% names(species)) {
      stop("Routes() requires a species either as valid input to ",
           "ebirdst::get_species() or a list with at a minimum a ",
           "\"common_name\" element.")
    }
    # Back fill required names with NA if missing and then
    # drop all species list items that aren't standard
    required_names <- c("species_code", "scientific_name", "common_name")
    missing_names <- setdiff(required_names, names(species))
    for (name in missing_names)
      species[[name]] <- NA
    allowed_names <- names(new_BirdFlow()$species)
    final_names <- allowed_names[allowed_names %in% names(species)]
    species <- species[final_names]
  }

  if (is.null(source)) {
    source <- NA_character_
  } else {
    if (!is.character(source)) {
      stop("source should be a character, or character vector")
    }
  }

  validate_BirdFlowRoutes_species(species)
  # Make new Routes object
  obj <- new_Routes(data, species, source)
  return(obj)
}
