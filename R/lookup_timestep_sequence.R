#'
#' Lookup a sequence of timesteps
#'
#' `lookup_timestep_sequence()` returns an ordered vector of timesteps, possibly
#' crossing over the year boundary.
#'
#' @param x A BirdFlow object
#' @param season a season name, season alias, or "all".  See
#' [lookup_season_timesteps()] for options.
#' @param start The starting point in time specified as a
#' timestep, character date, or date object.
#' @param end  The ending point in time as a date or timestep.
#' @param direction Either "forward" or "backward" defaults to
#'   `"forward"` if not processing dates.  If using date input `direction` is
#'   optional and is only used to verify the direction implicit in the dates.
#' @param season_buffer Only used with `season` input. `season_buffer` is
#'   passed to [lookup_season_timesteps()] and defaults to 1; it is the number
#'   of timesteps to extend the season by at each end.
#' @param n_steps Alternative to `end`  The end will be `n_steps` away from
#' `start` in `direction`; and the resulting sequence will have `n_step`
#' transitions and `n_steps + 1`  timesteps.
#' @return An integer sequence of timesteps.
#' @export
#' @details
#'
#' `lookup_timestep_sequence()` is unlikely to be called directly but it's
#'  arguments will likely be passed from other functions like [predict()] and
#'  [route()].
#'
#'  Whether called directly or via another function `lookup_timestep_sequence()`
#'  is a flexible function that allows several ways of defining the sequence.
#'
#'  1. **Dates**.  Input character dates (e.g. "2023-06-21") or date objects to
#'  both `start` and `end`. The direction will be determined from the dates so
#'  `direction` is optional.  If `direction` is used an error will be thrown if
#'  it doesn't conform with the direction implicit in the dates.
#'
#'  2. **Timesteps**.  Use numeric `start` and `end` to indicate a starting and
#'  ending timestep.  Since many models are circular `direction` is used to
#'  determine whether to go forward or backwards from `start` to `end`;
#'  `direction` will default to forward.
#'
#'  3. **Season**. Input a season name (or alias) into `season`.  `"all"` can
#'  also be used to indicate all timesteps, in which case with cyclical models
#'  the last timestep will match the first.  If `season` is used
#'  (and isn't `"all`) `season_bufffer` indicates the number of timesteps to
#'  extend the season by.  The default of 1 means that the sequence will start
#'  1 timestep (week) before and end 1 timestep after the dates for the season
#'  returned by `[species_info()]`. `direction` is followed and defaults to
#'  forward.
#'
#'  4. **Start and offset**. Use `start` with a timestep or date input and
#'  `n_steps` to create a sequence that starts at `start` and then proceeds
#'  `n_steps` in `direction` which default to "forward". The returned
#'   object will have `n_steps + 1` timesteps in the sequence.
#'
#'  5. **Default** If `season` and `start` are both NULL (or omitted) the
#'  default is to return all timesteps in the model, equivalent to
#'  `season = "all".`
#'
#' @examples
#' bf <- BirdFlowModels::rewbla
#'
#' # 1. Dates - order of dates determines direction
#' lookup_timestep_sequence(bf, start = "2023-12-1", end = "2024-01-20")
#' lookup_timestep_sequence(bf, start = "2024-01-20", end = "2023-12-1")
#'
#' # 2. Timesteps - direction defaults to "forward"
#' lookup_timestep_sequence(bf, start = 50, end = 3)
#' lookup_timestep_sequence(bf, start = 50, end = 3, direction = "backward")
#'
#' # 3. Season - direction defaults to "forward", season_buffer defaults to 1
#' lookup_timestep_sequence(bf, "prebreeding_migration")
#' lookup_timestep_sequence(bf, "prebreeding_migration", season_buffer = 0,
#'                          direction = "backward")
#'
#' # 4. start & n_steps  (start can be date or timestep)
#' lookup_timestep_sequence(bf, start = "2022-04-11", n_steps = 5)
#' lookup_timestep_sequence(bf, start = 10, n_steps = 5)
#'
#' # 5.  No time arguments, equivalent to season = "all"
#' lookup_timestep_sequence(bf)
#' lookup_timestep_sequence(bf, season = "all", direction = "backward")
#'
lookup_timestep_sequence <- function(x,
                                     season = NULL,
                                     start = NULL,
                                     end = NULL,
                                     direction = NULL,
                                     season_buffer = 1,
                                     n_steps = NULL) {

  # Throw useful error if old usage (season passed vis start) is used
  # Changed on 6/8/2023.  Delete this code after three months.
  season_values <- c(
    "all", "prebreeding_migration", "breeding", "postbreeding_migration",
    "nonbreeding", "pre", "post", "spring", "fall", "winter",  "nonbreeding",
    "summer", "breeding", "prebreeding", "postbreeding", "breed", "non")
  if (is.character(start) && length(start) == 1 && start %in% season_values) {
    stop("It looks like you are supplying a season name to start. ",
         "Use season argument instead.")
  }

  stopifnot(inherits(x, "BirdFlow"))
  dates <- get_dates(x)

  no_direction <- is.null(direction)
  if (is.null(direction))
    direction <- "forward"
  direction <- match.arg(direction, c("forward", "backward"))



  # Check for valid combinations of arguments
  if (!is.null(end) && !is.null(n_steps)) {
    stop("end and n_steps are mutually exclusive. ",
         "Use only one of these arguments.")
  }

  if (is.null(start)) {
    stopifnot("end should not be used without start" = is.null(end),
              "n_steps should not be used without start" = is.null(n_steps))
  }

  # Season defaults to "all" if both "start" and "season" are omitted.
  # Because function defaults to all timesteps
  if (is.null(season) && is.null(start)) {
    season <- "all"
  }

  # Handle season input
  if (!is.null(season)) {
    # Input validation is done in lookup_season_timesteps()
    s <- lookup_season_timesteps(x, season, season_buffer)

    if (direction == "backward")
      s <-  rev(s)
    return(s)
  }


  # Handle start and offset (n_steps)
  if (!is.null(start) && is.null(end) && !is.null(n_steps)) {
    if (n_steps > n_timesteps(x))
      stop("n_steps must be less than n_timesteps(x)")

    start <- lookup_timestep(start, bf = x)
    stopifnot(direction %in% c("forward", "backward"))
    if (direction == "forward") {
      end <- start + n_steps
      if (end > n_timesteps(x)) {
        if (!is_cyclical(x))
          stop("x is not cyclical and n_steps is large enough to extend ",
               "beyond the last timestep.")
        end <- end - n_timesteps(x)
      }
    }
    if (direction == "backward") {
      end <- start - n_steps
      if (end < 1) {
        if (!is_cyclical(x))
          stop("x is not cyclical and n_steps is large enough to extend ",
               "back past the first timestep.")
        end <- n_timesteps(x) + start - n_steps
      }
    }
  }

  if (is.integer(start))
    start <- as.numeric(start)
  if (is.integer(end))
    end <- as.numeric(end)

  if ((is.numeric(start) && !is.numeric(end)) ||
     (is.numeric(end) && !is.numeric(start)))
    stop("start and end must both be timesteps or both be dates.")


  if (!is.numeric(start)) {
    start <- lubridate::as_date(start)
    end <- lubridate::as_date(end)
  }

  # Process date input (in any of it's forms)
  if (lubridate::is.Date(start)) {
    implicit_direction <- ifelse(end > start, "forward", "backward")
    if (no_direction) {
      direction <- implicit_direction
    } else if (implicit_direction != direction) {
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
            length(end) == 1)


  is_backward <- direction == "backward"

  # loops is a flag that indicates we pass over the year boundary from last
  # timestep to first or from first to last
  loops <- (start < end & is_backward) ||
    (start > end & !is_backward) ||
    start == end



  if (loops && !is_cyclical(x))
    stop("Input indicates a connection between the last and first timestep ",
        "(probably crossing the year boundary) but the model is not cyclical.")

  step <- ifelse(is_backward, -1, 1)
  if (loops) {
    last_ts <- n_timesteps(x)
    edge1 <- ifelse(is_backward, 1, last_ts)
    edge2 <- ifelse(is_backward, last_ts, 1)
    steps <- c(seq(start, edge1, step), seq(edge2, end, step))
  } else {
    steps <- seq(start, end, by = step)
  }
  stopifnot(all(steps %in% dates$timestep))

  return(steps)
}

#' Lookup breeding, non-breeding, or migration season timesteps
#'
#' Retrieve the timesteps associated with a season for the species modeled by
#' a BirdFlow object, possibly with a buffer (in timesteps) added on. Seasons
#'  dates are from [ebirdst::ebirdst_runs] and are directly accessible
#' with [species_info()].
#'
#' @section Season names and aliases:
#'
#'  `season` can be `'all'`, one of the the four seasons, or an
#'  alias listed below.
#'
#'  | **season** | **aliases** |
#'  |--------|----------------|
#'  |`prebreeding_migration` | `pre`, `prebreeding`, `spring`|
#'  | `breeding` | `breed`, `summer` |
#'  | `postbreeding_migration` | `post`, `postbreeding`, `fall`|
#'  | `nonbreeding` | `non`, `winter` |
#'
#' @param x a BirdFlow object
#' @param season one of the seasons
#' returned by [species_info()], a season alias, or  or `"all"`
#' for all timesteps in the model
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
lookup_season_timesteps <- function(x, season, season_buffer = 1) {
  stopifnot("x must be a BirdFlow object" = inherits(x, "BirdFlow"))
  stopifnot("season should not be NULL " = !is.null(season),
            "season must not be NA" = !is.na(season),
            "season must be character" = is.character(season),
            "season must have one element" = length(season) == 1)
  season <- tolower(season)
  season <- switch(season,
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


  if (season == "all") {
    s <- seq_len(n_timesteps(x))
    if (is_cyclical(x))
      s <- c(s, 1)
    return(s)
  }

  stopifnot(!is.null(season_buffer),
            length(season_buffer) == 1,
            !is.na(season_buffer),
            is.numeric(season_buffer))

  season_buffer <- round(season_buffer)
  stopifnot(season_buffer >= 0)


  stopifnot(season %in% c("prebreeding_migration",
                          "postbreeding_migration",
                          "breeding",
                          "nonbreeding"))

  start <- species_info(x, paste0(season, "_start")) |>
    lookup_timestep(x, allow_failure = TRUE)
  end <- species_info(x, paste0(season, "_end")) |>
    lookup_timestep(x, allow_failure = TRUE)

  if (is.na(start) || is.na(end))
    stop(season, " season has NA for start and/or end date so season lookup",
    " is impossible. This is derived from eBird metadata.")

  circular <- n_timesteps(x) == n_transitions(x)

  if (circular) {

    # Buffered season should at most add up to n_timesteps
    n <- ifelse(end >= start, end - start + 1, n_timesteps(x) - start + 1 + end)
    if ((n + 2 * season_buffer) > n_timesteps(x))
      stop("season_buffer is too large.")

    # Add season buffer to start and end
    start <-  start - season_buffer
    if (start <= 0) {
      start <- n_timesteps(x) - start
    }
    end <- end + season_buffer
    if (end > n_timesteps(x)) {
      end <- end - n_timesteps(x)
    }
  } else {
    if (end < start)
      stop("Cannot resolve timesteps for ", season,
           " for non-circular BirdFlow model.")
    start <- max(1, start - season_buffer)
    end <- min(end + season_buffer, n_timesteps(x))
  }

  if (start < end) {
    s <- start:end
  } else {
    s <- c(start:n_timesteps(x), 1:end)
  }
  stopifnot(all(s) %in% get_dates(x)$timestep)

  return(s)
}
