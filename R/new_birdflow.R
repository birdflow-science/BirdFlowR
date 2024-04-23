# Don't flag S3 methods as having bad names when linting:
# nolint start: object_name_linter.
#' Private function to create the empty shell of a BirdFlow object
#'
#' @return An empty BirdFlow object
#' @keywords internal
new_BirdFlow <- function() {
  structure(list(geom = list(nrow = NA_integer_,
                             ncol = NA_integer_,
                             res = rep(NA_real_, 2),
                             ext = rep(NA_real_, 4),
                             crs = NA_character_,
                             mask = NA,
                             dynamic_mask = NA),
                 transitions = NA,
                 marginals = NA,
                 dates = NA,
                 distr = NA,
                 distances = NA,
                 species = list(
                   species_code = NA_character_,
                   scientific_name = NA_character_,
                   common_name = NA_character_,
                   breeding_quality = NA_integer_,
                   breeding_start = NA,
                   breeding_end = NA,
                   nonbreeding_quality = NA_integer_,
                   nonbreeding_start = NA,
                   nonbreeding_end = NA,
                   postbreeding_migration_quality = NA_integer_,
                   postbreeding_migration_start = NA,
                   postbreeding_migration_end = NA,
                   prebreeding_migration_quality = NA_integer_,
                   prebreeding_migration_start = NA,
                   prebreeding_migration_end = NA
                 ),
                 metadata = list(
                   has_marginals = FALSE,
                   has_transitions = FALSE,
                   has_distr = FALSE,
                   n_transitions = NA_integer_,
                   n_active = NA_integer_,
                   n_timesteps = NA_integer_,
                   timestep_padding = NA_integer_,
                   ebird_version_year = NA_integer_,
                   ebird_release_year = NA_integer_,
                   ebird_access_end_date = NA,
                   ebirdst_version = NA,
                   birdflow_preprocess_date = NA,
                   birdflow_model_date = NA,
                   birdflow_version = 3,
                   birdflowr_preprocess_version = NA,
                   birdflowr_version =
                     as.character(utils::packageVersion("BirdFlowR")),
                   is_sparse = FALSE,
                   sparse = NA
                  )),
            class = "BirdFlow")



}
# nolint end
