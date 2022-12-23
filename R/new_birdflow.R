#' Private function to create the empty shell of a BirdFlow object
#'
#' @return An empty BirdFlow object
new_BirdFlow <- function(){
  structure(list(geom = list( list(nrow = NA_real_,
                                   ncol = NA_real_,
                                   res = rep(NA_real_, 2),
                                   ext = rep(NA_real_, 4),
                                   crs = NA_character_,
                                   mask = NA) ),

                 trans = NA,
                 marginals = NA,
                 dates = NA,
                 n_trans = NA,
                 n_active = NA,
                 n_timesteps = NA,
                 distr = NA,
                 metadata = list(species = NA_character_,
                                 scientifoic = NA_character_,
                                 species_code = NA_character_,
                                 mode_date = NA_character_,
                                 has_marginals = FALSE,
                                 has_trans = FALSE,
                                 has_distr = FALSE) ),
            class = "BirdFlow")



}
