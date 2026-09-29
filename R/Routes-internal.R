#' @name Routes-internal
#' @title Internal (private) routes and intervals class creation functions
#' @description Internal (private) functions to create and validate
#' `Routes`, `BirdFlowRoutes`, and `BirdFlowIntervals` objects.
#'
#' These functions ensure input data meets the required structure and standard
#' for use within BirdFlow models.
#'
#' @details
#' - **`new_routes()`**: Creates a `Routes` object from a data frame.
#' - **`new_birdflow_routes()`**: Creates a `BirdFlowRoutes` object,
#' extending `Routes` with additional BirdFlow-specific spatial and
#' temporal information.
#' - **`new_birdflow_intervals()`**: Creates a `BirdFlowIntervals` object,
#' representing intervals between timesteps in BirdFlow data.
#'
#' All objects are internally validated during creation, ensuring required
#' columns, valid data types, and proper formats.
#'
#' @param data A data frame containing route/interval data for `Routes`,
#' `BirdFlowRoutes` or `BirdFlowIntervals`.
#' @param species Either a single character that will be passed to
#' [ebirdst::get_species()] to lookup species information or a list with
#' species metadata which must include `common_name` and can optionally
#' also include `scientific_name` and  `species_code` or any other standard
#' BirdFlow species metadata. See [species_info()] for a description
#' of the full list.
#'
#' @param metadata A list with additional metadata.
#' @param geom A list describing spatial geometry,
#' such as `nrow`, `ncol`, `crs`, and `mask`.
#' @param dates A data frame with date-related information,
#' including `date`, `start`, `end`, and `timestep`.
#' @param source A character string indicating the source of the data.
#' @param sort_id_and_dates Logical. Should the data be sorted by `route_id`
#' and `dates`?
#' @param reset_index Logical. Should the index of the data frame be reset
#' after sorting?
#' @param stay_calculate_col The column name for calculating the `stay_id` and
#' `stay_len` in `BirdFlowRoutes` object. Defaults to `date`.
#' @return Each function returns an S3 object of the corresponding class
#' (`Routes`, `BirdFlowRoutes`, or `BirdFlowIntervals`).
#' @keywords internal
#' @seealso
#' - [as_routes()] Create a `Routes` object
#' - [as_birdflow_routes()] Convert `Routes` to `BirdFlowRoutes`
#' - [as_birdflow_intervals()] Extract movement between pairs of locations
#'   from `BirdFlowRoutes` for use with model evaluation.
#' - [Object Validators](?object_validators) Private functions for validating
#'   routes and intervals.
#'
NULL

#' @rdname Routes-internal
#' @keywords internal
new_routes <- function(data, species, source) {
  # Check input
  stopifnot(is.data.frame(data))
  validate_route_df(data, class = "Routes")

  # Resolve species
  if (!is.list(species) && !is.null(species) && !is.na(species) &&
     length(species == 1)) {
    species <- lookup_species_metadata(species, quiet = TRUE,
                                        skip_checks = TRUE,
                                        min_season_quality = 0)
  } else {
    if (!is.list(species) || !"common_name" %in% names(species)) {
      stop("new_routes() requires a species either as valid input to ",
           "ebirdst::get_species() or a list with at a minimum a ",
           "\"common_name\" element.")
    }
    # Back fill required names with NA if missing and then
    # drop all species list items that aren't standard
    required_names <- c("species_code", "scientific_name", "common_name")
    missing_names <- setdiff(required_names, names(species))
    for (name in missing_names)
      species[[name]] <- NA
    allowed_names <- names(new_BirdFlow()$species)
    final_names <- allowed_names[allowed_names %in% names(species)]
    species <- species[final_names]
  }

  if (is.null(source)) {
    source <- NA_character_
  } else {
    if (!is.character(source)) {
      stop("source should be a character, or character vector")
    }
  }

  validate_BirdFlowRoutes_species(species)

  # Sort columns
  target_ordered_columns <- get_target_columns_Routes(type = "output")
  data <- data[
    ,
    c(
      target_ordered_columns,
      setdiff(names(data), target_ordered_columns)
    )
  ]
  obj <- list(
    data = data,
    species = species,
    source = source
  )

  class(obj) <- c("Routes")
  return(obj)
}

#' @rdname Routes-internal
#' @keywords internal
new_birdflow_routes <- function(data,
                                 species,
                                 metadata,
                                 geom,
                                 dates,
                                 source = NULL,
                                 sort_id_and_dates = TRUE,
                                 reset_index = FALSE,
                                 stay_calculate_col = "date") {

  # Check input
  stopifnot(inherits(data, "data.frame"))
  validate_route_df(data, class = "BirdFlowRoutes")
  validate_BirdFlowRoutes_species(species)
  validate_BirdFlowRoutes_metadata(metadata)
  validate_geom(geom, n_active = metadata$n_active)
  validate_BirdFlowRoutes_dates(dates)

  # BirdFlowRoutes stay units are by definition weeks
  stay_calculate_timediff_unit <- "weeks"

  if (sort_id_and_dates) {
    data <- sort_by_id_and_dates(data)
  }

  ## Add stay id
  data <- data |>
    dplyr::group_by(.data$route_id) |>
    add_stay_id_with_varied_intervals(
      date_col = stay_calculate_col,
      timediff_unit = stay_calculate_timediff_unit
      ) |>
    # Here, using add_stay_id_with_varied_intervals, rather than add_stay_id.
    # It takes 'timestep' as input so account for varying intervals,
    # if the data is not sampled in a frequency.
    dplyr::ungroup() |>
    as.data.frame()

  # Some eBird weeks have 8 days, rounding to make all weeks equal
  data$stay_len <- round(data$stay_len)

  # Sort columns
  target_ordered_columns <- get_target_columns_BirdFlowRoutes(type = "output")
  data <- data[
    ,
    c(
      target_ordered_columns,
      setdiff(
        names(data),
        target_ordered_columns
      )
    )
  ]
  obj <- list(
    data = data,
    species = species,
    metadata = metadata,
    geom = geom,
    dates = dates,
    source = source
  )

  class(obj) <- c("BirdFlowRoutes", "Routes")

  # Sort & reindex
  if (reset_index) {
    obj$data <- obj$data |> reset_index()
  }

  return(obj)
}

#' @rdname Routes-internal
#' @keywords internal
new_birdflow_intervals <- function(data,
                                    species,
                                    metadata,
                                    geom,
                                    dates,
                                    source = NULL) {
  validate_interval_df(data)
  validate_BirdFlowRoutes_species(species)
  validate_BirdFlowRoutes_metadata(metadata)
  validate_geom(geom, n_active = metadata$n_active)
  validate_BirdFlowRoutes_dates(dates)

  # Sort columns
  target_ordered_columns <-
    get_target_columns_BirdFlowIntervals(type = "output")
  data <- data[
    ,
    c(
      target_ordered_columns,
      setdiff(
        names(data),
        target_ordered_columns
      )
    )
  ]
  obj <- list(
    data = data,
    species = species,
    metadata = metadata,
    geom = geom,
    dates = dates,
    source = source
  )

  class(obj) <- c("BirdFlowIntervals")

  return(obj)
}



## For Routes and BirdFlowRoutes -----------------------------------------------

#' Reset Route Indices
#'
#' @description Resets the route IDs in a `Routes`
#' object to a new sequential numbering.
#'
#' @param routes A `Routes` or data frame object.
#'
#' @return A data frame with updated route IDs.
#' @keywords internal
reset_index <- function(routes) {
  stopifnot(inherits(routes, "data.frame"))
  # Get unique route_ids and create a mapping
  unique_ids <- unique(routes$route_id)
  new_ids <- paste0("route_", seq_along(unique_ids))

  # Create a lookup table
  id_mapping <- stats::setNames(new_ids, unique_ids)

  # Replace
  routes$route_id <- id_mapping[routes$route_id]

  return(routes)
}

#' Sort Routes by ID and Date
#'
#' @description Sorts a `Routes` or data frame object by route ID and date.
#'
#' @param routes A `Routes` or data frame object.
#'
#' @return A sorted data frame.
#' @export
#'
#' @examples
#' routes <- data.frame(list(
#'   route_id = c(2, 2, 1, 1),
#'   date = as.Date(c("2024-01-05", "2024-01-01", "2024-01-06", "2024-01-02"))
#' ))
#' sort_by_id_and_dates(routes)
sort_by_id_and_dates <- function(routes) {
  stopifnot(inherits(routes, "data.frame"))
  sorted_routes <- routes |>
    dplyr::arrange(.data[["route_id"]], .data[["date"]])
  return(sorted_routes)
}


#' Add Stay IDs
#'
#' @description Adds stay IDs to a data frame based on
#' changes in spatial indices.
#'
#' @param df A data frame with spatial indices.
#'
#' @return A data frame with `stay_id` and `stay_len` columns added.
#' @export
#'
#' @examples
#' routes <- data.frame(list(
#'   route_id = c(1, 1, 1, 2, 2, 3, 3, 3),
#'   i = c(1, 1, 2, 2, 3, 4, 4, 5),
#'   date = as.Date(c(
#'     "2024-01-01", "2024-01-02", "2024-01-03",
#'     "2024-01-04", "2024-01-05", "2024-01-06",
#'     "2024-01-07", "2024-01-08"
#'   ))
#' ))
#' routes$i <- as.integer(routes$i)
#' df_with_stay_ids <- add_stay_id(routes)
add_stay_id <- function(df) {
  new_df <- df |>
    dplyr::mutate(
      stay_id = cumsum(c(1, as.numeric(diff(.data$i)) != 0)),
      stay_len = rep(
        rle(.data$stay_id)$lengths,
        times = rle(.data$stay_id)$lengths
      )
    )
  return(new_df)
}

#' Add Stay IDs with Temporal Thresholds
#'
#' @description Adds stay IDs to a data frame,
#' considering changes in spatial indices.
#' Should only be applied on a single route, not multiple.
#' Using `add_stay_id_with_varied_intervals()`, rather than `add_stay_id()`:
#' It takes `date` as input so account for varying intervals,
#' if the data is not sampled in the same frequency.
#'
#' @param df A data frame with spatial and temporal data.
#' @param date_col The name of the column containing the
#' date information. Defaults to `"date"`.
#' @param timediff_unit The unit of `stay_len`.
#' @return A data frame with `stay_id` and `stay_len` columns added.
#' @export
#'
#' @examples
#' routes <- data.frame(list(
#'   route_id = c(1, 1, 1, 2, 2, 3, 3, 3),
#'   i = as.integer(c(1, 1, 2, 2, 3, 4, 4, 5)), # Spatial index
#'   date = as.Date(c(
#'     "2010-01-01", "2010-01-02", "2010-01-05", "2010-01-06",
#'     "2010-01-10", "2010-01-15", "2010-01-16", "2010-01-20"
#'   )) # Time steps with varying intervals
#' ))
#' df_with_varied_stay_ids <-
#'  add_stay_id_with_varied_intervals(routes, "date", "days")
add_stay_id_with_varied_intervals <- function(
  df, date_col = "date", timediff_unit = "days"
  ) {
  # Ensure the data is sorted by timestep

  new_df <- df |>
    dplyr::mutate(
      timestep_diff = c(1, as.numeric(diff(.data[[date_col]]),
      units = timediff_unit)), # Time differences
      i_change = c(1, as.numeric(diff(.data$i)) != 0), # Changes in 'i'
      stay_id = cumsum(.data[["i_change"]])
    ) |>
    # Now the stay_id is assigned,
    # calculate the duration (time difference) of each stay
    dplyr::group_by(.data[["route_id"]], .data[["stay_id"]]) |>
    dplyr::mutate(
      stay_len = as.numeric(max(.data[[date_col]]) -
                  min(.data[[date_col]]), units = timediff_unit)
    ) |>
    dplyr::select(-dplyr::all_of(c("timestep_diff", "i_change")))

  return(new_df)
}
