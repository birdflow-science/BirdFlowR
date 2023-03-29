
#' lookup timestep
#'
#'  This function returns the timestep or timesteps associated with `x` in a
#'  particular BirdFlow model, where `x` dates or timesteps in various formats.
#'
#'  So far all BirdFlow objects have had timesteps corresponding to weeks of
#'  the year and matching the S&T timesteps. However, it is likely that
#'  we will add the ability to make BirdFlow objects that only model part of the
#'  year. If we do this the timestep values will not necessarily match weeks.
#'
#'  If `x` is numeric it is assumed to already be a timestep. This is useful
#'  when using this function internally within [route()] and [predict()].
#'
#' @param x a character object representing date as year-month-day e.g.
#'  "2023-03-29", date object, or a numeric timestep.
#' @param bf a BirdFlow object
#' @return a vector of timesteps corresponding to elements in `x`
#' @export
#' @examples
#' bf <- BirdFlowModels::amewoo
#' lookup_timestep(c("2001-3-23", "2022-12-05"), bf)
#'
lookup_timestep <- function(x, bf){
  stopifnot(inherits(bf, "BirdFlow"))
  dates <- bf$dates
  original_x <- x


  if(is.character(x) || inherits(x, "POSIXt") ){
    x <- lubridate::as_date(x)
  }

  if(lubridate::is.Date(x)) {
    doy <- lubridate::yday(x) + 0.5
    ### We should switch over to using find_interval on week_start and week_end
    ### but the current (dated) example model doesn't have those columns
    ### Because it's old and they are derived from the S&T metadata.
    ### The code below doesn't work if the whole year isn't modeled.
    if(nrow(bf$dates) != 52)
      stop("This function assumes whole year is modeled")
    x <- sapply(doy, function(doy) which.min(abs(dates$doy - doy)) )
  }

  if(!is.numeric(x) || any(is.na(x)) || !all(x %in% dates$interval)){
    xtext <- ifelse(length(original_x) > 3,
                    paste0(paste(original_x[1:3], collapse = ", "),
                           ", ..."),
                    paste0(original_x, collpase = ", "))
    stop("Date lookup failed for x = ", xtext)
  }
  if(is.integer(x)) x <- as.numeric(x)
  return(x)
}
