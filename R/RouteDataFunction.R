
# S3 generic methods --------------------------------------------------------
print.Routes <- function(routes){
  stopifnot(inherits(routes,'Routes'))
  crossline <- '---------------------------------------------'
  cat(crossline,'\n')
  cat(glue("{class(routes)[1]} Object:"), '\n\n')
  # Print the data.frame part
  NextMethod("print")
  cat('\n')
  
  pad_width <- 18
  cat(format("Number of routes: ", width = pad_width), length(unique(routes$route_id)), "\n")
  cat(format("Number of points: ", width = pad_width), length(routes$date), "\n")
  cat(format("Date range: ", width = pad_width), format(min(routes$date)), "to", format(max(routes$date)), "\n")
  cat(format("Longitude range: ", width = pad_width), range(routes$lon), "\n")
  cat(format("Latitude range: ", width = pad_width), range(routes$lat), "\n")
  cat(crossline,'\n')
  
  formatted_summary_route_type <- format_summary_route_type(summarize_route_type(routes))
  cat(formatted_summary_route_type, '\n')
  
  if ('info' %in% colnames(routes)){
    cat(format("Info: ", width = pad_width), ifelse(length(routes$info)>20, paste0(substr(routes$info, 1, 20),' ...'), routes$info), "\n") # only print the head 20 characters
  }
  cat(crossline,'\n')
  cat("Species:\n", attr(routes, "species"), "\n\n", sep = "")
  cat(crossline,'\n')
  cat("Source:\n", paste(capture.output(print(attr(routes, "source"))), collapse = "\n"), "\n", sep = "")
  cat(crossline,'\n')
}


# Non-S3-generic methods for presentation (print, summary, plot)
summarize_route_type <- function(routes) {
  stopifnot(inherits(routes,'Routes'))
  routes |>
    dplyr::group_by(route_type) |>
    dplyr::summarize(
      unique_route_count = n_distinct(route_id), 
      unique_point_count = n(),
      .groups = "drop"
    )
}

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


# Non-S3-generic methods for conversion of data and objects

#' @export
as_BirdFlowRoutes <- function(routes, bf, valid_only = TRUE, sort_id_and_dates = TRUE, reset_index=FALSE){
  # Check input
  stopifnot(inherits(routes, 'Routes'))
  stopifnot(inherits(bf, 'BirdFlow'))
  stopifnot(is.logical(sort_id_and_dates))
  stopifnot(is.logical(reset_index))
  stopisnot(is.logical(valid_only))
  
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
  routes$timestep <- BirdFlowR::lookup_timestep(routes$date, bf, allow_failure = TRUE)
  
  # Only sucessfully converted spatiotemporal points will be included
  if (valid_only){
    routes <- df |>
      dplyr::filter(!is.na(x) & !is.na(y) & !is.na(i) & !is.na(timestep))
  }
  
  # Transform to BirdFlowRoutes
  routes <- BirdFlowRoutes(birdflow_route_df = routes,
                           species = attr(routes, 'species'),
                           source = attr(routes, 'source'))
  return(routes)
}

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

sort_by_id_and_dates <- function(routes){
  stopifnot(inherits(routes,'data.frame'))
  sorted_routes <- routes |> dplyr::arrange(.data[["route_id"]], .data[["date"]])
  return(sorted_routes)
}


add_stay_id <- function(df) {
  # Benjamin's function
  new_df <- df |>
    dplyr::mutate(stay_id = cumsum(c(1, as.numeric(diff(.data$i)) != 0)),
                  stay_len = rep(
                    rle(.data$stay_id)$lengths,
                    times = rle(.data$stay_id)$lengths)
    )
  return(new_df)
}

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



