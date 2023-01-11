#' Private function to create the empty shell of a BirdFlow object
#'
#' @return An empty BirdFlow object
new_BirdFlow <- function(){
  structure(list(geom = list(nrow = NA_real_,
                             ncol = NA_real_,
                             res = rep(NA_real_, 2),
                             ext = rep(NA_real_, 4),
                             crs = NA_character_,
                             mask = NA),
                 trans = NA,
                 marginals = NA,
                 dates = NA,
                 distr = NA,
                 species = list(
                   code = NA,
                   scientific_name = NA,
                   common_name = NA,
                   resident = NA,
                   breeding_quality = NA,
                   breeding_range_modeled = NA,
                   breeding_start = NA,
                   breeding_end = NA,
                   nonbreeding_quality = NA,
                   nonbreeding_range_modeled = NA,
                   nonbreeding_start = NA,
                   nonbreeding_end = NA,
                   postbreeding_migration_quality = NA,
                   postbreeding_migration_range_modeled = NA,
                   postbreeding_migration_start = NA,
                   postbreeding_migration_end = NA,
                   prebreeding_migration_quality = NA,
                   prebreeding_migration_range_modeled = NA,
                   prebreeding_migration_start = NA,
                   prebreeding_migration_end = NA,
                   resident_quality = NA,
                   resident_start = NA,
                   resident_end = NA
                 ),
                 metadata = list(
                   has_marginals = FALSE,
                   has_trans = FALSE,
                   has_distr = FALSE,
                   n_trans = NA,
                   n_active = NA,
                   n_timesteps = NA,
                   ebird_version_year = NA,
                   ebird_release_year = NA,
                   ebird_access_end_date = NA,
                   birdflow_preprocess_date = NA,
                   birdflow_model_date = NA
                  ) ),
            class = "BirdFlow")



}
