
#' generate migration routes from a BirdFlow model
#'
#' create stochastic migration routes for a species by sampling appropriate
#' starting locations and then [routing][route()] for the
#' duration of the migration window.
#'
#' The two migration periods are defined based on the species information in the
#' BirdFlow model:
#' \describe{
#'  \item{`prebreeding`}{ migration starts at `species_info("nonbreeding_end")` and ends at `species_info("breeding_start")`}
#'  \item{`postbreeding`}{ migration starts at `species_info("breeding_end")` and ends at `species_info("nonbreeding_start")`}
#'}
#' @param bf `BirdFlow` model
#' @param n the number of routes to generate
#' @param migration "prebreeding", "pre", or "spring" for the prebreeding
#'   migration; or "postbreeding", "post", or "fall" for the postbreeding
#'   migration.
#' @param buffer a buffer in timesteps (likely weeks) to add to the beginning
#'   and end of the season. The default of 1 means we start a week before the
#'   metadata suggests the season starts and continue one week past the end.
#' @return This will likely change. Currently returns a list with:
#'   \item{points}{A data.frame with coordinates, date, and route id}
#'   \item{lines}{a [sf][sf::sf] object containing one line per route.}
#' @seealso
#' `route_migration()` is a convenience wrapper for [route()].
#' [predict()][predict.BirdFlow()] projects future or past distributions based
#'  on a starting location or distribution.
#'  [lookup_season_timesteps()] does what its name suggests.
#' @export
#' @examples
#'   bf <-  BirdFlowModels::amewoo
#'   rts <- route_migration(bf, 2)
#'   plot(rts$lines)
#'   head(rts$points)
#'
route_migration <- function(bf, n, migration = "prebreeding", buffer = 1){

  timesteps <- lookup_season_timesteps(bf, migration, buffer)
  start <- timesteps[1]
  end <- timesteps[length(timesteps)]

  # Sample starting positions from distributions and convert to
  # xy coordinates
  locations  <- sample_distr(get_distr(bf, start, from_marginals = TRUE), n = n)
  if(n == 1){
    ind <- which(as.logical(locations) )
  } else {
    ind <- apply(locations, 2, function(x) which( as.logical(x) ) )
  }
  x <- i_to_x(ind, bf)
  y <- i_to_y(ind, bf)

  # Generate n routes with both the full and sparse models
  rts <- route(bf, x_coord = x, y_coord = y, start = start,
                end = end)

  return(rts)
}

