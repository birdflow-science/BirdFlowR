
# S3 generic methods --------------------------------------------------------

#' Print a Routes Object
#'
#' @description Custom print method for `Routes` objects, summarizing their contents and metadata.
#'
#' @param x A `Routes` object to print.
#' @param ... Additional arguments passed to `print.data.frame`.
#' 
#' @return Invisibly returns the input `routes` object.
#' @method print Routes
#' @export
#'
#' @examples
#' # Create a Routes object
#' route_df <- data.frame(
#'   route_id = 1:3,
#'   date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
#'   lon = c(-90, -89, -88),
#'   lat = c(40, 41, 42),
#'   route_type = c("tracking", "banding", "unknown")
#' )
#' 
#' routes <- Routes(route_df)
#' print(routes)
print.Routes <- function(x, ...){
  stopifnot(inherits(x,'Routes'))
  crossline <- '---------------------------------------------'
  cat(crossline,'\n')
  cat(sprintf("%s Object:", class(x)[1]), '\n\n')
  
  # Print the data.frame part
  print.data.frame(x$data, ...)
  cat('\n')
  
  pad_width <- 18
  cat(format("Number of routes: ", width = pad_width), length(unique(x$data$route_id)), "\n")
  cat(format("Number of points: ", width = pad_width), length(x$data$date), "\n")
  cat(format("Date range: ", width = pad_width), format(min(x$data$date)), "to", format(max(x$data$date)), "\n")
  cat(format("Longitude range: ", width = pad_width), range(x$data$lon), "\n")
  cat(format("Latitude range: ", width = pad_width), range(x$data$lat), "\n")
  cat(crossline,'\n')
  
  formatted_summary_route_type <- format_summary_route_type(summarize_route_type(x$data))
  cat(formatted_summary_route_type, '\n')
  
  if ('info' %in% colnames(x$data)){
    cat(format("Info: ", width = pad_width), ifelse(length(x$data$info)>20, paste0(substr(x$data$info, 1, 20),' ...'), x$data$info), "\n") # only print the head 20 characters
  }
  cat(crossline,'\n')
  cat("Species:\n", toString(x$species), "\n\n", sep = "")
  cat(crossline,'\n')
  cat("Source:\n", paste(utils::capture.output(print(x$source)), collapse = "\n"), "\n", sep = "")
  cat(crossline,'\n')
  invisible(x)
}

#' Print a BirdFlowRoutes Object
#'
#' @description Custom print method for `BirdFlowRoutes` objects, summarizing their contents, metadata, 
#' and BirdFlow-specific attributes.
#'
#' @param x A `BirdFlowRoutes` object to print.
#' @param ... Additional arguments passed to `print.data.frame`.
#' 
#' @return Invisibly returns the input `birdflow_routes` object.
#' @method print BirdFlowRoutes
#' @export
#'
#' @examples
#' # Create a BirdFlowRoutes object
#' route_df <- data.frame(
#'   route_id = 1:3,
#'   date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
#'   lon = c(-90, -89, -88),
#'   lat = c(40, 41, 42),
#'   route_type = c("tracking", "banding", "unknown")
#' )
#' routes <- Routes(route_df)
#' bf <- BirdFlowModels::amewoo
#' birdflow_routes <- as_BirdFlowRoutes(routes, bf)
#' print(birdflow_routes)
print.BirdFlowRoutes <- function(x, ...){
  stopifnot(inherits(x,'BirdFlowRoutes'))
  crossline <- '---------------------------------------------'
  cat(crossline,'\n')
  cat(sprintf("%s Object:", class(x)[1]), '\n\n')
  # Print the data.frame part
  print.data.frame(x$data, ...)
  cat('\n')
  
  pad_width <- 18
  cat(format("Number of routes: ", width = pad_width), length(unique(x$data$route_id)), "\n")
  cat(format("Number of points: ", width = pad_width), length(x$data$date), "\n")
  cat(format("Date range: ", width = pad_width), format(min(x$data$date)), "to", format(max(x$data$date)), "\n")
  cat(format("Longitude range: ", width = pad_width), range(x$data$lon), "\n")
  cat(format("Latitude range: ", width = pad_width), range(x$data$lat), "\n")
  cat(crossline,'\n')
  
  formatted_summary_route_type <- format_summary_route_type(summarize_route_type(x$data))
  cat(formatted_summary_route_type, '\n')
  
  if ('info' %in% colnames(x$data)){
    cat(format("Info: ", width = pad_width), ifelse(length(x$data$info)>20, paste0(substr(x$data$info, 1, 20),' ...'), x$data$info), "\n") # only print the head 20 characters
  }
  cat(crossline,'\n')
  cat(sprintf("Species: %s / %s / %s \n", 
              x$species$species_code, x$species$scientific_name, x$species$common_name),
      "\n\n")
  cat(crossline,'\n')
  cat("Source:\n", paste(utils::capture.output(print(x$source)), collapse = "\n"), "\n", sep = "")
  cat(crossline,'\n')
  invisible(x)
}


#' Print a BirdFlowIntervals Object
#'
#' @description Custom print method for `BirdFlowIntervals` objects, summarizing interval data 
#' and metadata, including temporal and spatial ranges.
#'
#' @param x A `BirdFlowIntervals` object to print.
#' @param ... Additional arguments passed to `print.data.frame`.
#' 
#' @return Invisibly returns the input `birdflow_intervals` object.
#' @method print BirdFlowIntervals
#' @export
#'
#' @examples
#' # Create a BirdFlowIntervals object
#' interval_df <- data.frame(
#'   interval_id = 1:3,
#'   route_id = c("route1", "route1", "route2"),
#'   lon1 = c(-90, -89, -88),
#'   lon2 = c(-89, -88, -87),
#'   lat1 = c(40, 41, 42),
#'   lat2 = c(41, 42, 43),
#'   x1 = c(1000, 1100, 1200),
#'   x2 = c(1100, 1200, 1300),
#'   y1 = c(500, 600, 700),
#'   y2 = c(600, 700, 800),
#'   i1 = as.integer(c(1, 2, 3)),
#'   i2 = as.integer(c(2, 3, 4)),
#'   date1 = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
#'   date2 = as.Date(c("2024-01-02", "2024-01-03", "2024-01-04")),
#'   timestep1 = as.integer(c(1, 2, 3)),
#'   timestep2 = as.integer(c(2, 3, 4)),
#'   route_type = c("tracking", "tracking", "banding")
#' )
#' bf <- BirdFlowModels::amewoo
#' birdflow_intervals <- BirdFlowIntervals(interval_df, species = bf$species, 
#' metadata = bf$metadata, geom = bf$geom, dates = get_dates(bf))
print.BirdFlowIntervals <- function(x, ...){
  stopifnot(inherits(x,'BirdFlowIntervals'))
  crossline <- '---------------------------------------------'
  cat(crossline,'\n')
  cat(sprintf("%s Object:", class(x)[1]), '\n\n')
  # Print the data.frame part
  print.data.frame(x$data, ...)
  cat('\n')
  
  pad_width <- 18
  cat(format("Number of intervals: ", width = pad_width), nrow(x$data), "\n")
  cat(format("Number of routes: ", width = pad_width), length(unique(x$data$route_id)), "\n")
  cat(format("Date range: ", width = pad_width), format(min(x$data$date1, x$data$date2)), "to", format(max(x$data$date1, x$data$date2)), "\n")
  cat(format("Longitude range: ", width = pad_width), range(x$data$lon1, x$data$lon2), "\n")
  cat(format("Latitude range: ", width = pad_width), range(x$data$lat1, x$data$lat2), "\n")
  cat(format("Minimum interval size: ", width = pad_width), min(as.numeric(x$data$date2 - x$data$date1, units = "days")), 'days / ', min(x$data$timestep2 - x$data$timestep1), 'timesteps', "\n")
  cat(format("MAximum interval size: ", width = pad_width), max(as.numeric(x$data$date2 - x$data$date1, units = "days")), 'days / ', max(x$data$timestep2 - x$data$timestep1), 'timesteps', "\n")
  cat(crossline,'\n')
  
  formatted_summary_route_type <- format_summary_route_type(summarize_route_type(x$data))
  cat(formatted_summary_route_type, '\n')
  
  if ('info' %in% colnames(x$data)){
    cat(format("Info: ", width = pad_width), ifelse(length(x$data$info)>20, paste0(substr(x$data$info, 1, 20),' ...'), x$data$info), "\n") # only print the head 20 characters
  }
  cat(crossline,'\n')
  cat(sprintf("Species: %s / %s / %s \n", 
              x$species$species_code, x$species$scientific_name, x$species$common_name),
      "\n\n")
  cat(crossline,'\n')
  cat("Source:\n", paste(utils::capture.output(print(x$source)), collapse = "\n"), "\n", sep = "")
  cat(crossline,'\n')
  invisible(x)
}

# Non-S3-generic methods for presentation (print, summary, plot) --------------------------------------------------------

#' Summarize Route Types
#'
#' @description Summarizes the number of unique routes and points for each route type.
#'
#' @param routes A `Routes` or `BirdFlowIntervals` object.
#'
#' @return A data frame with columns `route_type`, `unique_route_count`, and `unique_point_count`.
#' @keywords internal
summarize_route_type <- function(routes) {
  stopifnot(inherits(routes,c('data.frame')))
  routes |>
    dplyr::group_by(.data[['route_type']]) |>
    dplyr::summarize(
      unique_route_count = dplyr::n_distinct(.data[['route_id']]), 
      unique_point_count = dplyr::n(),
      .groups = "drop"
    )
}

#' Format Route Type Summary
#'
#' @description Formats a summary of route types into a human-readable string.
#'
#' @param summary_route_type A data frame containing route type summaries.
#'
#' @return A formatted string summarizing route types.
#' @keywords internal
format_summary_route_type <- function(summary_route_type) {
  stopifnot(inherits(summary_route_type,'data.frame'))
  
  route_type_summary_str <- apply(summary_route_type, 1, function(row) {
    paste0(
      sprintf("Route Type: %s\n", row["route_type"]),
      sprintf("Unique Routes: %s; ", row["unique_route_count"]),
      sprintf("Unique Points: %s\n", row["unique_point_count"])
    )
  })
  
  summary_str <- paste(route_type_summary_str, collapse = "\n")
  return(summary_str)
}


# Non-S3-generic methods for conversion of data and objects --------------------------------------------------------
## For Routes and BirdFlowRoutes --------------------------------------------------------

#' Convert Routes to BirdFlowRoutes
#'
#' @description Converts a `Routes` object to a `BirdFlowRoutes` object, adding BirdFlow-specific spatiotemporal coordinates.
#'
#' @param routes A `Routes` object.
#' @param bf A `BirdFlow` object for spatial and temporal reference.
#' @param valid_only Logical. Should only valid points be included? Defaults to `TRUE`.
#' @param sort_id_and_dates Logical. Should data be sorted by route ID and date? Defaults to `TRUE`.
#' @param reset_index Logical. Should indices be reset after sorting? Defaults to `FALSE`.
#'
#' @return A `BirdFlowRoutes` object.
#' @export
#'
#' @examples
#' route_df <- data.frame(
#'   route_id = 1:3,
#'   date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
#'   lon = c(-90, -89, -88),
#'   lat = c(40, 41, 42),
#'   route_type = c("tracking", "banding", "unknown")
#' )
#' routes_obj <- Routes(route_df)
#' bf <- BirdFlowModels::amewoo
#' birdflow_routes <- as_BirdFlowRoutes(routes_obj, bf)
as_BirdFlowRoutes <- function(routes, bf, valid_only = TRUE, sort_id_and_dates = TRUE, reset_index=FALSE){
  # Check input
  stopifnot(inherits(routes, 'Routes'))
  stopifnot(inherits(bf, 'BirdFlow'))
  stopifnot(is.logical(sort_id_and_dates))
  stopifnot(is.logical(reset_index))
  stopifnot(is.logical(valid_only))
  
  # Sort & reindex
  if (sort_id_and_dates){
    routes$data <- (routes$data |> sort_by_id_and_dates())
  }
  if (reset_index){
    routes$data <- (routes$data |> reset_index())
  }
  
  # Conversion
  ## Spatial
  xy <- BirdFlowR::latlon_to_xy(lat = routes$data$lat, lon = routes$data$lon, bf = bf)
  routes$data$x <- xy$x
  routes$data$y <- xy$y
  routes$data$i <- as.integer(BirdFlowR::xy_to_i(x = routes$data$x, y = routes$data$y, bf))
  
  ## Temporal
  routes$data$date <- lubridate::as_date(routes$data[['date']])
  routes$data$timestep <- as.integer(BirdFlowR::lookup_timestep(routes$data$date, bf, allow_failure = TRUE))
  
  # Only sucessfully converted spatiotemporal points will be included
  if (valid_only){
    routes$data <- routes$data |>
      dplyr::filter(!is.na(.data[['x']]) & !is.na(.data[['y']]) & !is.na(.data[['i']]) & !is.na(.data[['timestep']]))
  }
  
  # Randomly select only one data point per timestep per routes
  routes$data <- routes$data |> dplyr::group_by(.data[['route_id']], .data[['timestep']]) |> dplyr::slice_sample(n = 1) |> dplyr::ungroup() |> as.data.frame()
  
  # Transform species to the BirdFlow species list
  species <- bf$species # Regardless of what the species in the `routes` is -- if using bf, then the species is the species of bf model.
  metadata <- bf$metadata
  geom <- bf$geom
  dates <- get_dates(bf) # use the up-to-date dates dataframe
  
  # Transform to BirdFlowRoutes
  routes <- BirdFlowRoutes(data = routes$data,
                           species = species,
                           metadata = metadata,
                           geom = geom,
                           dates = dates,
                           source = routes$source)
  return(routes)
}

#' Reset Route Indices
#'
#' @description Resets the route IDs in a `Routes` object to a new sequential numbering.
#'
#' @param routes A `Routes` or data frame object.
#'
#' @return A data frame with updated route IDs.
#' @keywords internal
reset_index <- function(routes) {
  stopifnot(inherits(routes,'data.frame'))
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
sort_by_id_and_dates <- function(routes){
  stopifnot(inherits(routes,'data.frame'))
  sorted_routes <- routes |> dplyr::arrange(.data[["route_id"]], .data[["date"]])
  return(sorted_routes)
}


#' Add Stay IDs
#'
#' @description Adds stay IDs to a data frame based on changes in spatial indices.
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
#'   )))
#' )
#' routes$i <- as.integer(routes$i)
#' df_with_stay_ids <- add_stay_id(routes)
add_stay_id <- function(df) {
  new_df <- df |>
    dplyr::mutate(stay_id = cumsum(c(1, as.numeric(diff(.data$i)) != 0)),
                  stay_len = rep(
                    rle(.data$stay_id)$lengths,
                    times = rle(.data$stay_id)$lengths)
    )
  return(new_df)
}

#' Add Stay IDs with Temporal Thresholds
#'
#' @description Adds stay IDs to a data frame, considering changes in spatial indices and temporal thresholds.
#' Using add_stay_id_with_varied_intervals, rather than add_stay_id: It takes 'date' as input so account for varying intervals, if the data is not sampled in the same frequency.
#'
#' @param df A data frame with spatial and temporal data.
#' @param timestep_col The name of the column containing time steps. Defaults to `"date"`.
#' @param timediff_unit The unit of 'stay_len'.
#' @return A data frame with `stay_id` and `stay_len` columns added.
#' @export
#'
#' @examples
#' routes <- data.frame(list(
#'   route_id = c(1, 1, 1, 2, 2, 3, 3, 3),
#'   i = as.integer(c(1, 1, 2, 2, 3, 4, 4, 5)),  # Spatial index
#'   date = as.Date(c('2010-01-01', '2010-01-02', '2010-01-05', '2010-01-06', 
#'   '2010-01-10', '2010-01-15', '2010-01-16', '2010-01-20'))  # Time steps with varying intervals
#' ))
#' df_with_varied_stay_ids <- add_stay_id_with_varied_intervals(routes, "date", "days")
add_stay_id_with_varied_intervals <- function(df, timestep_col = "date", timediff_unit = "days") {
  
  # Ensure the data is sorted by timestep
  df <- df |> dplyr::arrange(.data[[timestep_col]])
  
  new_df <- df |>
    dplyr::mutate(
      timestep_diff = c(1, as.numeric(diff(.data[[timestep_col]]), units = timediff_unit)),  # Time differences
      i_change = c(1, as.numeric(diff(.data$i)) != 0),    # Changes in 'i'
      stay_id = cumsum(.data[['i_change']])
    ) |>
    # Now the stay_id is assigned, calculate the duration (time difference) of each stay
    dplyr::group_by(.data[['route_id']], .data[['stay_id']]) |>
    dplyr::mutate(
      stay_len = as.numeric(max(.data[[timestep_col]]) - min(.data[[timestep_col]]), units = timediff_unit)
    ) |>
    dplyr::select(-dplyr::all_of(c('timestep_diff', 'i_change')))
  
  return(new_df)
}


## For as_BirdFlowIntervals --------------------------------------------------------

#' Convert BirdFlowRoutes to BirdFlowIntervals
#'
#' @description Converts a `BirdFlowRoutes` object into a `BirdFlowIntervals` object, 
#' sampling interval pairs between time points.
#'
#' @param birdflow_routes A `BirdFlowRoutes` object.
#' @param max_n The maximum number of intervals to sample. Defaults to 1000.
#'
#' @return A `BirdFlowIntervals` object.
#' @export
#'
#' @examples
#'route_df <- data.frame(
#'  route_id = c("001", "001", "001", "001", "001", "003", "003", "003", "004"),
#'  date = as.Date(c("2025-01-01", "2025-01-08", "2025-01-15",
#'  "2025-01-21", "2025-02-10", "2025-03-01", "2025-05-01", "2025-06-01", "2025-05-01")),
#'  lon = c(-75.0060, -75.0060, -74.0060, -87.6298, -87.6298, -87.6298, -89.6298, -85.6298, -95.3698),
#'  lat = c(39.7128, 39.7128, 40.7128, 41.8781, 41.8781, 41.8781, 42.8781, 40.8781, 29.7604),
#'  route_type = c("tracking", 'tracking', "tracking", 'tracking', 'tracking', 
#'  "motus", "motus", "motus", "motus")
#')
#' routes_obj <- Routes(route_df)
#' bf <- BirdFlowModels::amewoo
#' birdflow_routes <- routes_obj |> as_BirdFlowRoutes(bf=bf)
#' birdflow_intervals <- as_BirdFlowIntervals(birdflow_routes, max_n = 1000)
as_BirdFlowIntervals <- function(birdflow_routes, max_n=1000) {
  stopifnot(inherits(birdflow_routes, 'BirdFlowRoutes'))
  stopifnot(is.numeric(max_n))
  
  # Conversion
  sampling_strategy_df <- calculate_interval_sampling_strategy(birdflow_routes$data, max_n)

  # sampling_strategy_df: a dataframe with columns `time_points`, `interval_pairs`, and `intervals_to_sample`
  all_interval_df <- list()
  for (row_id in seq_len(nrow(sampling_strategy_df))){
    this_row <- sampling_strategy_df[row_id, ]
    this_route <- birdflow_routes$data[birdflow_routes$data$route_id==this_row$route_id,]
    
    all_pairs <- as.data.frame(t(utils::combn(seq_len(nrow(this_route)), 2)))
    sampled_pairs <- all_pairs[sample(seq_len(nrow(all_pairs)), size = this_row$interval_pairs, replace = FALSE), ]
    
    
    extract_intervals <- function(idx_pair) {
      row1 <- this_route[idx_pair[1], ]
      row2 <- this_route[idx_pair[2], ]
      data.frame(
        lon1 = row1$lon, lon2 = row2$lon,
        lat1 = row1$lat, lat2 = row2$lat,
        x1 = row1$x, x2 = row2$x,
        y1 = row1$y, y2 = row2$y,
        i1 = row1$i, i2 = row2$i,
        date1 = row1$date, date2 = row2$date,
        timestep1 = row1$timestep, timestep2 = row2$timestep,
        route_id = row1$route_id,
        route_type = row1$route_type,
        stringsAsFactors = FALSE # Avoid factor conversion
      )
    }
    
    intervals <- lapply(seq_len(nrow(sampled_pairs)), function(i) {
      idx_pair <- as.numeric(sampled_pairs[i, ]) # Ensure indices are numeric
      extract_intervals(idx_pair)
    })
    
    formatted_intervals <- do.call(rbind, intervals)
    all_interval_df[[row_id]] <- formatted_intervals
  }
  
  all_interval_df <- do.call(rbind, all_interval_df)

  target_columns <- get_target_columns_BirdFlowIntervals(type='input')

  if (is.null(all_interval_df)) {
    
    return(NULL)
    
  } else {
    all_interval_df$i1 <- as.integer(all_interval_df$i1)
    all_interval_df$i2 <- as.integer(all_interval_df$i2)
    all_interval_df$interval_id <- paste0("interval_", seq_len(nrow(all_interval_df)))
    rownames(all_interval_df) <- NULL
    all_interval_df <- all_interval_df[, c(target_columns, setdiff(names(all_interval_df), target_columns))]
    
    obs <- BirdFlowIntervals(data = all_interval_df,
                             species = birdflow_routes$species,
                             metadata = birdflow_routes$metadata,
                             geom = birdflow_routes$geom,
                             dates = birdflow_routes$dates,
                             source = birdflow_routes$source)
    return(obs)
  }
}


#' Calculate Interval Sampling Strategy
#'
#' @description Determines how to sample intervals for each route based on the total number of intervals requested. 
#' Ensures an even distribution across routes when possible.
#'
#' @param routes A `Routes` or similar object with `route_id` and time point data.
#' @param n The total maximum number of intervals to sample. Notice: The actual output of intervals might be less than n, because of data deficiency. But never larger than n.
#'
#' @return A data frame with the columns:
#' - `route_id`: The route ID.
#' - `time_points`: The number of time points in the route.
#' - `interval_pairs`: The total number of possible interval pairs for the route.
#' - `intervals_to_sample`: The number of intervals to sample for the route.
#'
#' @export
#'
#' @examples
#' # Example usage
#' routes <- data.frame(route_id = c("A", "A", "B", "B", "B"),
#'                      date = as.Date("2024-01-01") + 0:4)
#' sampling_strategy <- calculate_interval_sampling_strategy(routes, n = 10)
calculate_interval_sampling_strategy <- function(routes, n) {
  # Group by route_id and count the number of time points in each route
  route_counts <- routes |>
    dplyr::group_by(.data[['route_id']]) |>
    dplyr::summarize(time_points = dplyr::n())
  
  # Calculate interval pairs for each route
  route_counts <- route_counts |>
    dplyr::mutate(interval_pairs = .data[['time_points']] * (.data[['time_points']] - 1) / 2) |> 
    dplyr::filter(.data[['interval_pairs']]!=0)
  
  if (n <= nrow(route_counts)){
    # The requested interval count is smaller than the total routes, so sample one interval per route
    sampled_route_counts <- route_counts |> dplyr::slice_sample(n = n, replace = FALSE)
    sampled_route_counts$intervals_to_sample <- 1
  } else if (n >= sum(route_counts$interval_pairs)){
    # Don't need to think -- We can't generate that much intervals! So sample all of them
    sampled_route_counts <- route_counts
    sampled_route_counts$intervals_to_sample <- sampled_route_counts$interval_pairs
  } else {
    # Calculate how many intervals to sample for each route
    ## Fist, we should have one sample per route, to give an even sampling
    part1 <- nrow(route_counts)
    rest <- n - part1
    
    rest_samplable_route_counts <- route_counts
    rest_samplable_route_counts$sampable_amount <- rest_samplable_route_counts$interval_pairs - 1
    rest_samplable_route_counts$proportion <- rest_samplable_route_counts$sampable_amount / sum(rest_samplable_route_counts$sampable_amount)
    rest_samplable_route_counts$to_sample <- pmin(ceiling(rest_samplable_route_counts$proportion * rest) + 1, rest_samplable_route_counts$sampable_amount)
    
    if (sum(rest_samplable_route_counts$to_sample) > rest){
      # Because of the ceiling rounding, the total sampling planned might exceed the `rest`,
      # In that case, reduce the amount based on proportion, step by step
      to_reduce <- sum(rest_samplable_route_counts$to_sample) - rest
      
      while (to_reduce > 0) {
        # Apply proportional reduction, constrained by `to_reduce`
        sampled_index <- sample(
          seq_len(nrow(rest_samplable_route_counts)),
          size = 1, 
          prob = rest_samplable_route_counts$proportion
        )
        if (rest_samplable_route_counts[sampled_index, 'to_sample'] <= 1){
          # Should leave at least one sample for that route
          next
        }
        rest_samplable_route_counts[sampled_index, 'to_sample'] <- rest_samplable_route_counts[sampled_index, 'to_sample'] - 1
        to_reduce <- to_reduce - 1
      }
      
      sampled_route_counts <- route_counts
      sampled_route_counts$intervals_to_sample <- rest_samplable_route_counts$to_sample
      
    } else if (sum(rest_samplable_route_counts$to_sample) < rest) {
      stop('Not possible error. The sampling has problems.')
    }
  }
  return(sampled_route_counts)
}

