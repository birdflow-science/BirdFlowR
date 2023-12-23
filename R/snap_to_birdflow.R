# nolint start: line_length_linter
#' Assign cells and timesteps to coordinates and dates
#'
#' This function aligns bird movement data that has both location and date
#' information with a BirdFlow model's cells and timesteps.
#' Optionally,it will aggregate multiple points from a track within the same
#' week into a single point and date prior to determining the location ID.
#'
#' If `aggregate` is NULL than than each row in the output will correspond with
#' the same row in the input.  With aggregation rows can be lost in two ways
#' (1) if the coordinates or timestep can't be resolved (first three errors
#' on list) then the row is dropped prior to aggregation.
#' (2) via aggregation all the rows within a week for a track will be collapsed
#' to one. These aggregated locations can still trigger the other three
#' errors on the list.
#'
#' A location id will only be assigned if the location is a valid location for
#' the model on the associated date.
#'
#' @param d A data frame with bird movement data it must have the columns
#' specified by `x_col`, `y_col`, `date_col`, and `id_cols`.  The default
#' values for the first three are  `"lon"`, `"lat"`, and`"date"`.
#' @param bf A BirdFlow model
#' @param x_col The name of the column that holds x or longitude coordinates.
#'  Default is `"lon"`.
#' @param y_col The name of the y or latitude column, default is `"lat"`.
#' @param date_col Name of the date column, defaults to `"date"`
#' @param id_cols The name of identification columns. One or more columns that
#' collectively define a unique track. If `aggregate` is not `NULL` these along
#' with `timestep` (calculated from the `date_col`) are used to define the
#' groups.  If aggregate is `NULL` these columns are not used but are retained
#' in the output.
#' @param crs The coordinate reference system used by `x_col` and
#' `y_col`. The default `"EPSG:4326"` corresponds to
#' [WGS 1984](https://epsg.io/4326)
#' @param aggregate Leave the default `NULL` for no aggregation - each row in
#' `d` is processed separately and represented in the returned object.
#'  If `aggregate` is set to one of the values below then the locations (rows)
#'  in `d` that fall in the same week and track (see `id_cols`) will be
#'  aggregated together such that there is only one location per week for each
#'  track.
#'
#'  The possible values for `aggregate` are:
#'  * `mean` The mean of the "x", "y", and "date" columns are used for the week.
#'  * `median` The median of the "x", "y", and "date" columns are  used for the
#'  week.
#'  * `midweek` The observation that is closest to the middle of the week is
#'  used to represent the week. With ties the observation that occurs first
#'  is used.
#'
#'  Pending ideas, not yet implemented:
#'  * `gmedian` [geometric median](https://cran.r-project.org/web/packages/Gmedian/index.html)
#'  * `central` The point closest to the centroid of all the points is used to
#'  represent the week.
#' @return A data frame with columns
#' \item{`<id_cols>`}{The columns identified with `id_cols` will be retained
#' and be leftmost.}
#' \item{date}{This column will have information from `date_col` but not retain
#' its name or original formatting. If aggregate is NULL the input dates
#' will be retained, if not the date will vary with the aggregation method but
#' will represent the dates that went into the summary and not the mid-week
#' date associated with `timestep`.
#' For example with `aggregate = "mean"` the date will be the average date of
#' the points in the week.}
#'  \item{timestep}{The model timestep associated with `date`.}
#'  \item{x, y}{The x and y coordinates of the point. These will always be in
#'  `crs(bf)` and will represent the original or aggregated location and
#'   not the cell center.}
#'  \item{i}{The location index of the cell associated with the `x` and `y` in
#'  `bf`.  See [i_to_xy()].}
#'  \item{n}{The number of rows in `d` that contributed to each output row.
#'  If aggregate is NULL every value will be 1. }
#'  \item{error}{TRUE if there was an error.}
#'  \item{message}{NA or the error message. The possible messages are:
#'  * "err_date" - The date could not be parsed with [lubridate::as_date()]
#'  * "err_truncated" - `bf` is [truncated](truncate_birdflow) and the date
#'  falls outside of portion of the year the model covers.
#'  * `"err_coords"` - The coordinates could not be transformed into `crs(bf)`
#'    and thus likely are corrupt in some way.
#'  * `"err_not_active"` - the location does not fall within an active cell
#'  as defined by the static mask.
#'  * `"err_dynamic"` - the location does not fall within the dynamic mask on
#'  the associated date.
#'  * `"err_sparse"` - the location falls within the dynamic mask but that
#'  location and date combination has been eliminated by
#'  [sparsification](sparsify).
#'
#'  The function will always return the error message that appears first on
#'  this list, even though in some cases multiple errors can be triggered.
#'
#'  With aggregation the first three errors prevent a row from being used and
#'  it will be dropped prior to aggregation with a warning.
#'  }
#' @export
#' @importFrom stats median
#' @examples
#' bf <- BirdFlowModels::rewbla |> add_dynamic_mask()
#' obs <- BirdFlowModels::rewbla_observations
#' a <- snap_to_birdflow(obs, bf, id_cols = "bird_id")
# nolint end
snap_to_birdflow <- function(d, bf,
                        x_col = "lon", y_col = "lat",
                        date_col = "date",
                        id_cols = "id",
                        crs = "EPSG:4326",
                        aggregate = NULL) {

  # Check input columns
  for (arg in c("x_col", "y_col", "date_col", "id_cols")) {
    value <- get(arg)
    if (!all(value %in% names(d)))
      stop("\"", setdiff(value, names(d)), "\" is not a column in d")
  }

  stopifnot(is.numeric(d[[x_col]]),
            is.numeric(d[[y_col]]))

  # Expected output columns
  expected_cols <- c(id_cols, "date", "timestep", "x", "y", "i", "n",
                     "error", "message")

  # Handle special case of no input rows
  if (nrow(d)== 0) {
    d <- cbind(d[, id_cols, drop = FALSE],
               date = lubridate::as_date(integer(0)),
               timestep = numeric(0),
               x = numeric(0),
               y = numeric(0),
               i = integer(0),
               n = numeric(0),
               error = logical(0),
               message = character(0))
    return(d[, expected_cols, drop = FALSE])
  }

  # Helper - make an empty error table
  make_error_table <- function(n) {
    f <- rep(FALSE, n)
    data.frame(error = f,
               err_date = f,
               err_truncated = f,
               err_coords = f,
               err_not_active = f,
               err_dynamic = f,
               err_sparse = f)
  }

  # helper - propagate an error in any "err_..." column to "error" column
  update_errors <- function(e) {
    e$error <- apply(e[, !names(e) == "error", drop = FALSE], 1, any)
    e
  }

  errors <- make_error_table(nrow(d))

  # Standardize date column name, date format, and add timesteps
  d$date <- suppressWarnings(lubridate::as_date(d[[date_col]]))
  errors$err_date[is.na(d$date)]  <- TRUE
  errors <- update_errors(errors)
  d$timestep <- lookup_timestep(d$date, bf, allow_failure = TRUE)
  errors$err_truncated <- !errors$err_date & is.na(d$timestep)
  errors <- update_errors(errors)

  # Convert to sf
  d_sf <- sf::st_as_sf(d, coords = c(x_col, y_col))
  sf::st_crs(d_sf) <- sf::st_crs(crs)

  # Transform to bf crs
  d_t <- sf::st_transform(d_sf, sf::st_crs(crs(bf)))
  coords <- sf::st_coordinates(d_t)
  colnames(coords) <- c("x", "y")
  errors$err_coords <- apply(coords, 1, anyNA)
  errors <- update_errors(errors)

  # Add transformed coordinates as "x" and "y" columns
  names(d)[names(d) == "x"] <- "original_x"
  names(d)[names(d) == "y"] <- "original_y"
  d <- cbind(d, coords)
  rm(coords)

  if (is.null(aggregate)) {
    # Number of observations is 1 if no aggregation
     d$n <- rep(1, nrow(d))
  } else {  # aggregation

    #### Aggregate ####

    # Note: since it's unclear what to do with rows that have unresolved
    # dates or locations when we aggregate I'm dropping them here.
    keep <- !errors$error
    if (!all(keep)) {
      warning(sum(!keep), " input rows have dates or coordinates that can't ",
              "be resolved and will be dropped prior to aggregation.")
      d <- d[keep, , drop = FALSE]
      errors <- errors[keep, , drop = FALSE]
    }

    # Add coordinates  - and drop any pre-existing x or y columns
    # work with a temporary copy
    d <- dplyr::group_by(d, dplyr::pick({{id_cols}}), .data$timestep)

    d <- switch(aggregate,
           "mean" = {
             dplyr::summarize(d,
                              x = mean(.data$x),
                              y = mean(.data$y),
                              date = mean(.data$date),
                              n = dplyr::n())},
           "median" = {
             dplyr::summarize(d,
                              x = median(.data$x),
                              y = median(.data$y),
                              date = median(.data$date),
                              n = dplyr::n())},
           "midweek" = {
              d$mid <- lookup_date(d$timestep, bf)

              # find which element in x is closest to y
              closest <- function(x, y) {
                which.min(abs(x - y))
              }

              d <- dplyr::summarize(
                d,
                x = .data$x[closest(.data$date, .data$mid)],
                y = .data$y[closest(.data$date, .data$mid)],
                date = .data$date[closest(.data$date, .data$mid)],
                n = dplyr::n())
              d$mid <- NULL
              d
           },

           # If none of the above match
           stop("aggregate should be \"mean\" or \"median\"")
           )

    d <- as.data.frame(d)

    # Make new error for remaining rows
    # We dropped all the errors before aggregation so there are none (yet)
    errors <- make_error_table(nrow(d))
  }

  # resolve i (location index)
  d$i <- xy_to_i(d[, c("x", "y"), drop = FALSE], bf = bf)
  errors$err_not_active <- is.na(d$i)
  errors <- update_errors(errors)

  # Check against dynamic mask
  dm <- get_dynamic_mask(bf)
  no_err <- !errors$error  # only evaluate where there aren't other problems
  errors$err_dynamic[no_err] <- !dm[cbind(d$i[no_err], d$timestep[no_err])]
  errors <- update_errors(errors)

  # Check against sparsification
  # sm is the sparsified mask, locations where the dynamic mask is 1
  # but the marginal derived distribution is 0
  if (has_marginals(bf)) {
    sm <- (get_distr(bf, from_marginals = TRUE) > 0) & dm
    no_err <- !errors$error  # only evaluate where there aren't other problems
    errors$err_sparse[no_err] <- !sm[cbind(d$i[no_err], d$timestep[no_err])]
    errors <- update_errors(errors)
  }

  # Add error information to d
  d$error <- errors$error
  errors$error <- NULL
  d$message <- names(errors)[apply(errors, 1, function(x) which(x)[1])]

  # Standardize output

  # Replace Nan with NA in coordinates (reprojection errors can produce NaN)
  d$x[is.na(d$x)] <- NA_real_
  d$y[is.na(d$y)] <- NA_real_

  # Filter and order columns
  stopifnot(all(expected_cols %in% names(d)))
  d <- d[, expected_cols, drop = FALSE]

  return(d)
}
