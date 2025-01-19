
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
#' routes <- Routes(route_df)
#' print(routes)
print.Routes <- function(x, ...){
  stopifnot(inherits(x,'Routes'))
  crossline <- '---------------------------------------------'
  cat(crossline,'\n')
  cat(glue::glue("{class(x)[1]} Object:"), '\n\n')
  # Print the data.frame part
  print.data.frame(x, ...)
  cat('\n')
  
  pad_width <- 18
  cat(format("Number of routes: ", width = pad_width), length(unique(x$route_id)), "\n")
  cat(format("Number of points: ", width = pad_width), length(x$date), "\n")
  cat(format("Date range: ", width = pad_width), format(min(x$date)), "to", format(max(x$date)), "\n")
  cat(format("Longitude range: ", width = pad_width), range(x$lon), "\n")
  cat(format("Latitude range: ", width = pad_width), range(x$lat), "\n")
  cat(crossline,'\n')
  
  formatted_summary_route_type <- format_summary_route_type(summarize_route_type(x))
  cat(formatted_summary_route_type, '\n')
  
  if ('info' %in% colnames(x)){
    cat(format("Info: ", width = pad_width), ifelse(length(x$info)>20, paste0(substr(x$info, 1, 20),' ...'), x$info), "\n") # only print the head 20 characters
  }
  cat(crossline,'\n')
  cat("Species:\n", toString(attr(x, "species")), "\n\n", sep = "")
  cat(crossline,'\n')
  cat("Source:\n", paste(capture.output(print(attr(x, "source"))), collapse = "\n"), "\n", sep = "")
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
  cat(glue::glue("{class(x)[1]} Object:"), '\n\n')
  # Print the data.frame part
  print.data.frame(x, ...)
  cat('\n')
  
  pad_width <- 18
  cat(format("Number of routes: ", width = pad_width), length(unique(x$route_id)), "\n")
  cat(format("Number of points: ", width = pad_width), length(x$date), "\n")
  cat(format("Date range: ", width = pad_width), format(min(x$date)), "to", format(max(x$date)), "\n")
  cat(format("Longitude range: ", width = pad_width), range(x$lon), "\n")
  cat(format("Latitude range: ", width = pad_width), range(x$lat), "\n")
  cat(crossline,'\n')
  
  formatted_summary_route_type <- format_summary_route_type(summarize_route_type(x))
  cat(formatted_summary_route_type, '\n')
  
  if ('info' %in% colnames(x)){
    cat(format("Info: ", width = pad_width), ifelse(length(x$info)>20, paste0(substr(x$info, 1, 20),' ...'), x$info), "\n") # only print the head 20 characters
  }
  cat(crossline,'\n')
  cat(glue::glue("Species: {attr(x, 'species')$species_code} / {attr(x, 'species')$scientific_name} / {attr(x, 'species')$common_name} \n", "\n\n", sep = ""))
  cat(crossline,'\n')
  cat("Source:\n", paste(capture.output(print(attr(x, "source"))), collapse = "\n"), "\n", sep = "")
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
#'   i1 = c(1, 2, 3),
#'   i2 = c(2, 3, 4),
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
  cat(glue::glue("{class(x)[1]} Object:"), '\n\n')
  # Print the data.frame part
  print.data.frame(x, ...)
  cat('\n')
  
  pad_width <- 18
  cat(format("Number of intervals: ", width = pad_width), nrow(x), "\n")
  cat(format("Number of routes: ", width = pad_width), length(unique(x$route_id)), "\n")
  cat(format("Date range: ", width = pad_width), format(min(x$date1, x$date2)), "to", format(max(x$date1, x$date2)), "\n")
  cat(format("Longitude range: ", width = pad_width), range(x$lon1, x$lon2), "\n")
  cat(format("Latitude range: ", width = pad_width), range(x$lat1, x$lat2), "\n")
  cat(format("Minimum interval size: ", width = pad_width), min(as.numeric(x$date2 - x$date1, units = "days")), 'days / ', min(x$timestep2 - x$timestep1), 'timesteps', "\n")
  cat(format("MAximum interval size: ", width = pad_width), max(as.numeric(x$date2 - x$date1, units = "days")), 'days / ', max(x$timestep2 - x$timestep1), 'timesteps', "\n")
  cat(crossline,'\n')
  
  formatted_summary_route_type <- format_summary_route_type(summarize_route_type(x))
  cat(formatted_summary_route_type, '\n')
  
  if ('info' %in% colnames(x)){
    cat(format("Info: ", width = pad_width), ifelse(length(x$info)>20, paste0(substr(x$info, 1, 20),' ...'), x$info), "\n") # only print the head 20 characters
  }
  cat(crossline,'\n')
  cat(glue::glue("Species: {attr(x, 'species')$species_code} / {attr(x, 'species')$scientific_name} / {attr(x, 'species')$common_name} \n", "\n\n", sep = ""))
  cat(crossline,'\n')
  cat("Source:\n", paste(capture.output(print(attr(x, "source"))), collapse = "\n"), "\n", sep = "")
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
  stopifnot(inherits(routes,c('Routes', 'BirdFlowIntervals')))
  routes |>
    dplyr::group_by(route_type) |>
    dplyr::summarize(
      unique_route_count = dplyr::n_distinct(route_id), 
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
    routes <- routes |> sort_by_id_and_dates()
  }
  if (reset_index){
    routes <- routes |> reset_index()
  }
  
  # Conversion
  ## Spatial
  xy <- BirdFlowR::latlon_to_xy(lat = routes$lat, lon = routes$lon, bf = bf)
  routes$x <- xy$x
  routes$y <- xy$y
  routes$i <- BirdFlowR::xy_to_i(x = routes$x, y = routes$y, bf)
  
  ## Temporal
  routes$date <- lubridate::as_date(routes[['date']])
  routes$timestep <- as.integer(BirdFlowR::lookup_timestep(routes$date, bf, allow_failure = TRUE))
  
  # Only sucessfully converted spatiotemporal points will be included
  if (valid_only){
    routes <- routes |>
      dplyr::filter(!is.na(x) & !is.na(y) & !is.na(i) & !is.na(timestep))
  }
  
  # Randomly select only one data point per timestep per routes
  routes <- routes |> dplyr::group_by(route_id, timestep) |> dplyr::slice_sample(n = 1) |> dplyr::ungroup() |> as.data.frame()
  
  # Transform species to the BirdFlow species list
  species <- bf$species # Regardless of what the species in the `routes` is -- if using bf, then the species is the species of bf model.
  metadata <- bf$metadata
  geom <- bf$geom
  dates <- get_dates(bf) # use the up-to-date dates dataframe
  
  # Transform to BirdFlowRoutes
  routes <- BirdFlowRoutes(birdflow_route_df = routes,
                           species = species,
                           metadata = metadata,
                           geom = geom,
                           dates = dates,
                           source = attr(routes, 'source'))
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
  id_mapping <- setNames(new_ids, unique_ids)
  
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
#' @keywords internal
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
#' routes <- data.frame(
#'   route_id = c(1, 1, 1, 2, 2, 3, 3, 3),
#'   i = as.integer(c(1, 1, 2, 2, 3, 4, 4, 5)),
#'   date = as.Date(c(
#'     "2024-01-01", "2024-01-02", "2024-01-03", 
#'     "2024-01-04", "2024-01-05", "2024-01-06",
#'     "2024-01-07", "2024-01-08"
#'   ))
#' )
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
#' Using add_stay_id_with_varied_intervals, rather than add_stay_id: It takes 'timestep' as input so account for varying intervals, if the data is not sampled in the same frequency.
#'
#' @param df A data frame with spatial and temporal data.
#' @param timestep_col The name of the column containing time steps. Defaults to `"timestep"`.
#' @param time_threshold A numeric threshold for time differences. Defaults to `Inf`.
#'
#' @return A data frame with `stay_id` and `stay_len` columns added.
#' @export
#'
#' @examples
#' routes <- data.frame(
#'   route_id = c(1, 1, 1, 2, 2, 3, 3, 3),
#'   i = as.integer(c(1, 1, 2, 2, 3, 4, 4, 5)),  # Spatial index
#'   timestep = as.integer(c(1, 2, 5, 6, 10, 15, 16, 20))  # Time steps with varying intervals
#' )
#' df_with_varied_stay_ids <- add_stay_id_with_varied_intervals(routes, "timestep", time_threshold = Inf)
add_stay_id_with_varied_intervals <- function(df, timestep_col = "timestep", time_threshold = Inf) {
  
  # Ensure the data is sorted by timestep
  df <- df |> dplyr::arrange(.data[[timestep_col]])
  
  new_df <- df |>
    dplyr::mutate(
      timestep_diff = c(1, diff(.data[[timestep_col]])),  # Time differences
      i_change = c(1, as.numeric(diff(.data$i)) != 0),    # Changes in 'i'
      stay_id = cumsum(i_change | (timestep_diff > time_threshold))
    ) |>
    # Now the stay_id is assigned, calculate the duration (time difference) of each stay
    dplyr::group_by(route_id, stay_id) |>
    dplyr::mutate(
      stay_len = max(.data[[timestep_col]]) - min(.data[[timestep_col]]) + 1
    ) |>
    dplyr::select(-timestep_diff, -i_change)
  
  return(new_df)
}

#' Preserve S3 Attributes Between Objects
#'
#' @description Copies custom S3 attributes from an original object to a modified object while 
#' avoiding overwriting structural attributes like `class`, `names`, and `dim`.
#'
#' @param original The original object from which attributes are preserved. Must be of class 
#' `BirdFlowRoutes`, `Routes`, or `data.frame`.
#' @param modified The modified object to which attributes will be applied.
#'
#' @return The `modified` object with the preserved attributes from the `original` object.
#' @keywords internal
preserve_s3_attributes <- function(original, modified) {
  
  # Class check
  stopifnot(inherits(original,c('BirdFlowRoutes', 'Routes', 'data.frame')))
  
  # Preserve the class
  class(modified) <- class(original)
  
  # Preserve custom attributes while avoiding overwriting structural attributes
  original_attrs <- attributes(original)
  modified_attrs <- attributes(modified)
  
  # Exclude attributes that should not be overwritten
  excluded <- c("names", 'class', "row.names", "dim", "dimnames")
  preserved_attrs <- original_attrs[!(names(original_attrs) %in% excluded)]
  
  # Add preserved attributes to modified object
  attributes(modified) <- c(modified_attrs, preserved_attrs)
  
  return(modified)
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
#' route_df <- data.frame(
#'   route_id = 1:3,
#'   date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
#'   lon = c(-90, -89, -88),
#'   lat = c(40, 41, 42),
#'   route_type = c("tracking", "banding", "unknown")
#' )
#' routes_obj <- Routes(route_df)
#' bf <- BirdFlowModels::amewoo
#' birdflow_routes <- routes_obj |> as_BirdFlowRoutes(bf=bf)
#' birdflow_intervals <- as_BirdFlowIntervals(birdflow_routes, max_n = 1000)
as_BirdFlowIntervals <- function(birdflow_routes, max_n=1000) {
  stopifnot(inherits(birdflow_routes, 'BirdFlowRoutes'))
  stopifnot(is.numeric(max_n))
  
  # Conversion
  sampling_strategy_df <- calculate_interval_sampling_strategy(birdflow_routes, max_n)
  # sampling_strategy_df: a dataframe with columns `time_points`, `interval_pairs`, and `intervals_to_sample`
  all_interval_df <- list()
  for (row_id in seq_len(nrow(sampling_strategy_df))){
    this_row <- sampling_strategy_df[row_id, ]
    this_route <- birdflow_routes[birdflow_routes$route_id==this_row$route_id,]
    
    all_pairs <- as.data.frame(t(combn(seq_len(nrow(this_route)), 2)))
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
  
  target_columns <- get_target_columns_BirdFlowIntervals(type='input')
  
  all_interval_df <- do.call(rbind, all_interval_df)
  if (is.null(all_interval_df)) {
    all_interval_df <- data.frame(
      interval_id = character(),
      lon1 = numeric(),
      lon2 = numeric(),
      lat1 = numeric(),
      lat2 = numeric(),
      x1 = numeric(),
      x2 = numeric(),
      y1 = numeric(),
      y2 = numeric(),
      i1 = numeric(),
      i2 = numeric(),
      date1 = as.Date(character()),
      date2 = as.Date(character()),
      timestep1 = numeric(),
      timestep2 = numeric(),
      route_id = character(),
      route_type = character(),
      stringsAsFactors = FALSE # Avoid factor conversion
    )
  } else {
    all_interval_df$interval_id <- paste0("interval_", seq_len(nrow(all_interval_df)))
  }
  
  rownames(all_interval_df) <- NULL
  all_interval_df <- all_interval_df[, c(target_columns, setdiff(names(all_interval_df), target_columns))]
  
  obs <- BirdFlowIntervals(all_interval_df,
                           species = attr(birdflow_routes, 'species'),
                           metadata = attr(birdflow_routes, 'metadata'),
                           geom = attr(birdflow_routes, 'geom'),
                           dates = attr(birdflow_routes, 'dates'),
                           source = attr(birdflow_routes, 'source'))
  return(obs)
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
    dplyr::group_by(route_id) |>
    dplyr::summarize(time_points = dplyr::n())
  
  # Calculate interval pairs for each route
  route_counts <- route_counts |>
    dplyr::mutate(interval_pairs = time_points * (time_points - 1) / 2) |> 
    dplyr::filter(interval_pairs!=0)
  
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

