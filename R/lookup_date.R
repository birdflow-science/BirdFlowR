
#' Retrieve date associated with a timestep
#'
#' @param timestep Integer between 1 and `n_timesteps(bf)`
#' @param bf A BirdFlow object
#'
#' @return A Date object
#' @seealso [get_dates()], [lookup_timestep()], [lookup_timestep_sequence()]
#' @export
#'
#'
#' @examples
#' bf <- BirdFlowModels::amewoo
#' lookup_date(1:5, bf)
lookup_date <- function(timestep, bf){
  dates <- get_dates(bf)
  lubridate::as_date(dates$date[match(timestep, dates$timestep)])
}
