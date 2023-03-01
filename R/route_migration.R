
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
#' @return This will likely change. Currently returns a list with:
#'   \item{points}{A data.frame with coordinates, date, and route id}
#'   \item{lines}{a [sf][sf::sf] object containing one line per route.}
#' @seealso
#' `route_migration()` is a convenience wrapper for [route()].
#' [`forecast()`] projects future or past distributions based on a starting
#'    location or distribution.
#' @export
#' @examples
#'   bf <-  BirdFlowModels::amewoo
#'   rts <- route_migration(bf, 2)
#'   plot(rts$lines)
#'   head(rts$points)
#'
route_migration <- function(bf, n, migration = "prebreeding"){
  migration <- tolower(migration)
  migration <- switch(migration,
                      "pre" = "prebreeding",
                      "post" = "postbreeding",
                      "fall" = "postbreeding", # Northern hemisphere bias
                      "spring" = "prebreeding", # Northern meisphere bias
                      migration)

  stopifnot(migration %in% c("prebreeding", "postbreeding"))
  stopifnot(length(migration) == 1)
  stopifnot(is.numeric(n), !is.na(n), length(n) == 1 )
  stopifnot(inherits(bf, "BirdFlow"))

  # Set starting and ending dates
  if(migration == "prebreeding"){
    start <- species(bf, "nonbreeding_end")
    end <- species(bf, "breeding_start")
  }

  if(migration == "postbreeding"){
    start <- species(bf, "breeding_end")
    end <- species(bf, "nonbreeding_start")
  }

  if(is.na(start) || is.na(end)){
    stop("Migration timing information is missing from the BirdFlow model.")
  }

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

