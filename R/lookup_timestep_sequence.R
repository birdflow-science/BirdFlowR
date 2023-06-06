#'
#' Lookup a sequence of timesteps
#'
#' `lookup_timestep_sequence()` returns an ordered vector of timesteps possibly
#' crossing over the year boundary.
#'
#' `start` and `end` will fall into one of four categories which are treated
#'   differently:
#'
#'  1. If `start` and `end` are numeric than they are treated as timesteps and
#'  `direction` is used to route either forward (default) or backwards between
#'  them, possibly passing over the year boundary.
#'
#'  2. If `start` and `end` are date objects or both characters they are
#'  treated as dates, and the direction is determined from the dates:"forward"
#'  if in chronological order, "backwards" otherwise.  If the `direction`
#'  argument is used along with date input and it is not consistent with the
#'  implicit direction from the dates, than an error will be thrown.
#'
#'  3. If `start` is a character and `end` is missing than it is assumed that
#'  `start` is a key word that is either "all" for all timesteps, or a season
#'  name. Season names are passed to [lookup_season_timesteps()] along with
#'  `season_buffer`. `direction` will be followed and will default to "forward".
#'
#'  4. If `start` is a timestep or date, `end` is missing, and `n` is not
#'   missing than `n` transitions from start are added to the sequence in
#'   `direction` which defaults to "forward". This results in `n + 1` timesteps
#'   in the sequence.  The sequence will wrap around the year boundary when
#'   appropriate if `x` is cyclical.
#
#' @param x A BirdFlow object
#' @param start The starting points in time specified as
#' timesteps, character dates, or date objects; or  may be set to "all" or a
#' season name to be interpreted by [lookup_season_timesteps()].
#' @param end If start is a timestep or date `end` should be a timestep or date
#' indicating the ending point in time.
#' @param direction Either "forward" or "backward".
#'
#'   If `start` and `end`
#'   represent dates and `direction` is used an error will
#'   be thrown if `direction` isn't consistent with direction indicated by the
#'   dates.
#'
#'   If `start` and `end` are not dates, `direction` defaults to "forward" and
#'   `start` and `end` should either both be timesteps (numeric); or
#'   `end` should be omitted and start should be "all" or a season name.
#'
#'   If `start` is a timestep or date, `end` is omitted, and `n` is an integer
#'   than `direction` defaults to "forward".
#'
#' @param season_buffer Only used if `start` is a season. `season_buffer` is
#'   passed to [lookup_season_timesteps()] and defaults to 1; it is the number
#'   of timesteps to extend the season by at each end.
#' @param n Alternative to `end` for specifying when a sequence should end.
#' `n` indicates how many transitions should be in the sequence in `direction`
#'  which defaults to "forward" if `n` is used.  The sequence will have `n + 1`
#'  timesteps.
#' @return An integer sequence of timesteps.
#' @export
#' @examples
#' bf <- BirdFlowModels::rewbla
#'
#' # Using timesteps - direction defaults to "forward"
#' lookup_timestep_sequence(bf, 50, 3)  # c(50:52, 1:3)
#' lookup_timestep_sequence(bf, 50, 3, direction = "backward") # 50:3
#'
#' # Using dates - order of dates determines direction
#' lookup_timestep_sequence(bf, "2023-12-1", "2024-01-20")
#' lookup_timestep_sequence(bf, "2024-01-20", "2023-12-1")
#'
#' # "all" - direction defaults to "forward"
#' lookup_timestep_sequence(bf, "all")
#' lookup_timestep_sequence(bf, "all", direction = "backward")
#'
#' # Season - direction defaults to "forward", season_buffer defaults to 1
#' lookup_timestep_sequence(bf, "prebreeding_migration")
#' lookup_timestep_sequence(bf, "prebreeding_migration", season_buffer = 0,
#'                          direction = "backward")
#'
lookup_timestep_sequence <- function (x, start = "all", end, direction,
                                      season_buffer, n) {

  stopifnot(inherits(x, "BirdFlow"))
  dates <- x$dates

  if(!missing(direction))
    direction <- match.arg(direction, c("forward", "backward"))

  if(!missing(end) && !missing(n))
    stop("end and n and mutually exclusive.  Use only one of these arguments.")

  # Handle special cases where start is "all" or a season name
  if(is.character(start) && missing(end) && missing(n)){
    stopifnot(length(start) == 1)
    if(missing(direction))
      direction <- "forward"
    if(missing(season_buffer))
      season_buffer <- 1

    s <- lookup_season_timesteps(x, start, season_buffer)

    if(direction == "backward")
      s <-  rev(s)
    return(s)
  }

  if(!missing(season_buffer))
    warning("season_buffer is only used if start is a season name",
         "and end is missing.")

  # Handle start and offset (n)
  if(!missing(start) && missing(end) && !missing(n)){
    if(missing(direction))
      direction <- "forward"
    if(n > n_timesteps(x))
      stop("n must be less than n_timesteps(x)")

    start <- lookup_timestep(start, bf = x)
    stopifnot(direction %in% c("forward", "backward"))
    if (direction == "forward") {
      end <- start + n
      if (end > n_timesteps(x)) {
        if (!is_cyclical(x))
          stop("x is not cyclical and n is large enough to extend beyond the",
               " last timestep.")
        end <- end - n_timesteps(x)
      }
    }
    if (direction == "backward") {
      end <- start - n
      if (end < 1) {
        if (!is_cyclical(x))
          stop("x is not cyclical and n is large enough to extend back past ",
               "the first timestep.")
        end <- n_timesteps(x) + start - n
      }
    }
  }

  if(is.integer(start))
    start <- as.numeric(start)
  if(is.integer(end))
    end <- as.numeric(end)

  if((is.numeric(start) && !is.numeric(end)) ||
     (is.numeric(end) && !is.numeric(start)))
    stop("start and end must both be timesteps or both be dates.")


  if(!is.numeric(start)){
    start <- lubridate::as_date(start)
    end <- lubridate::as_date(end)
  }

  # Process date input (in any of it's forms)
  if(lubridate::is.Date(start)){
    implicit_direction <- ifelse(end > start, "forward", "backward")
    if(missing(direction)){
      direction <- implicit_direction
    } else if(implicit_direction != direction){
        stop("Implicit direction in start and end dates (", implicit_direction,
             ") is in conflict with direction argument (", direction, ").")
    }

    # Convert to timesteps
    start <- lookup_timestep(start, x)
    end <-  lookup_timestep(end, x)
  }

  stopifnot(is.numeric(start),
            is.numeric(end),
            length(start) == 1,
            length(end) == 1 )

  # Direction still might not be set if there is timestep input
  if(missing(direction))
    direction <- "forward"
  stopifnot(direction %in% c("forward", "backward"))

  if(!start %in% dates$interval)
    stop("Start resolved to timestep", start, "which isn't a modeled timestep.")
  if(!end %in% dates$interval)
    stop("Start resolved to timestep", start, "which isn't a modeled timestep.")
  if(start == end)
    stop("Start and stop resolved to same timestep (", start, ")")


  # loops is a flag that indicates we pass over the year boundary from last
  # timestep to first or from first to last
  is_backward <- direction == "backward"
  loops <- start == end | ( start < end & is_backward) |
    start > end & !is_backward

  # circular is a flag that indicates whether the model is circular
  circular <- n_timesteps(x) == n_transitions(x)
  if(loops && !circular)
    stop("Input indicates crossing the year boundary but the model",
         " does not support that.")

  step <- ifelse(is_backward, -1, 1)
  if(loops){
    last_ts <- n_timesteps(x)
    edge1 <- ifelse(is_backward, 1, last_ts)
    edge2 <- ifelse(is_backward, last_ts, 1)
    steps <- c(seq(start, edge1, step), seq(edge2, end, step))
  } else {
    steps <- seq(start, end, by = step)
  }
  stopifnot(all(steps %in% dates$interval))

  return(steps)
}



#' Lookup breeding, non-breeding, or migration season timesteps
#'
#' Retrieve the timesteps associated with a season for the species modeled by
#' a BirdFlow object, possibly with a buffer (in timesteps) added on.
#'
#'  `season` should be `'all'` one of the the four seasons or their
#'  alternate names listed below.
#'
#'  | season | alternate names |
#'  |--------|----------------|
#'  |`prebreeding_migration` | `pre`, `prebreeding`, `spring`|
#'  | `breeding` | `breed`, `summer` |
#'  | `postbreeding_migration` | `post`, `postbreeding`, `fall`|
#'  | `nonbreeding` | `non`, `winter` |
#'
#' @param x a BirdFlow object
#' @param season the season to lookup timesteps for one of the four seasons
#' returned by [species_info()]; one of the alternative  names listed  in
#' details; or `"all"` for all timesteps in the model.
#' @param season_buffer the number of extra timesteps to add to the beginning
#' and end of the season.
#' @return a series of integers indicating which timesteps correspond with the
#' (possibly buffered) season.
#'
#' @export
#' @examples
#' bf <- BirdFlowModels::rewbla
#' lookup_season_timesteps(bf, "breeding", season_buffer = 0)
#'
lookup_season_timesteps <- function (x, season, season_buffer = 1) {
  stopifnot("x must be a BirdFlow object" = inherits(x, "BirdFlow"))
  season <- tolower(season)
  season = switch(season,
                  "pre" = "prebreeding_migration",
                  "post" = "postbreeding_migration",
                  "spring" = "prebreeding_migration",
                  "fall" = "postbreeding_migration",
                  "winter" = "nonbreeding",
                  "summer" = "breeding",
                  "prebreeding" = "prebreeding_migration",
                  "postbreeding" = "postbreeding_migration",
                  "breed" = "breeding",
                  "non" = "nonbreeding",
                  season)


  if(tolower(season) == "all"){
    return(seq_len(n_timesteps(x)))
  }



  stopifnot(season %in% c("prebreeding_migration",
                          "postbreeding_migration",
                          "breeding",
                          "nonbreeding") )

  start = species_info(x, paste0(season, "_start")) |> lookup_timestep(x)
  end = species_info(x, paste0(season, "_end")) |> lookup_timestep(x)

  circular <- n_timesteps(x) == n_transitions(x)

  if(circular){

    # Buffered season should at most add up to n_timesteps
    n <- ifelse(end > start, end - start + 1, n_timesteps(x) - start + 1 + end)
    if((n + 2 * season_buffer) > n_timesteps(x))
      stop("season_buffer is too large.")

    # Add season buffer to start and end
    start <-  start - season_buffer
    if(start <= 0){
      start = n_timesteps(x) - start
    }
    end <- end + season_buffer
    if(end > n_timesteps(x)){
      end <- end - n_timesteps(x)
    }
  } else {
    if(end < start)
      stop("Cannot resolve timesteps for ", season,
           " for non-circular BirdFlow model.")
    start <- max(1, start - season_buffer)
    end <- min(end + season_buffer, n_timesteps(x))
  }

  if(start < end){
    s <- start:end
  } else {
    s <- c(start:n_timesteps(x), 1:end)
  }
  stopifnot(all(s) %in% x$dates$interval)

  return(s)
}







