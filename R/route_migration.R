
#' Deprecated function to generate migration routes from a BirdFlow model
#'
#' This function is now deprecated and will eventually be deleted. Please
#' transition to using  `route()` which can now both generate starting locations
#' by sampling the distributions in `bf` and use a season name to specify the
#' time period to route over.  The only adjustment that needs to be made is to
#' use the `season` argument to `route()` in place of the `migration` argument
#' to `route_migration()`.
#'
#' @param bf `BirdFlow` model
#' @param n the number of routes to generate
#' @param migration "prebreeding", "pre", or "spring" for the prebreeding
#'   migration; or "postbreeding", "post", or "fall" for the postbreeding
#'   migration.
#' @param season_buffer a buffer in timesteps (likely weeks) to add to the
#'   beginning and end of the season. The default of 1 means we start a week
#'   before the metadata suggests the season starts and continue one week past
#'   the end.
#' @inherit route return
#' @seealso
#' * [route()] should be used instead of this function.
#' @export
#' @keywords internal
route_migration <- function(bf, n, migration = "prebreeding",
                            season_buffer = 1) {

  warning("route_migration() is deprecated please transition to route() instead. ",
          "Change the migration argument to season.")

  rts <- route(bf = bf, n = n, season = migration,
               season_buffer = season_buffer)

  return(rts)
}
