
# S3 generic methods --------------------------------------------------------
print.Tracks <- function(tracks){
  stopifnot(inherits(tracks,'Tracks'))
  crossline <- '---------------------------------------------'
  cat(crossline,'\n')
  cat(glue("{class(tracks)[1]} Object:"), '\n\n')
  # Print the data.frame part
  NextMethod("print")
  cat('\n')
  
  pad_width <- 18
  cat(format("Number of tracks: ", width = pad_width), length(unique(tracks$track_id)), "\n")
  cat(format("Number of points: ", width = pad_width), length(tracks$date), "\n")
  cat(format("Date range: ", width = pad_width), format(min(tracks$date)), "to", format(max(tracks$date)), "\n")
  cat(format("Longitude range: ", width = pad_width), range(tracks$lon), "\n")
  cat(format("Latitude range: ", width = pad_width), range(tracks$lat), "\n")
  cat(crossline,'\n')
  
  formatted_summary_track_type <- format_summary_track_type(summarize_track_type(tracks))
  cat(formatted_summary_track_type, '\n')
  
  if ('info' %in% colnames(tracks)){
    cat(format("Info: ", width = pad_width), ifelse(length(tracks$info)>20, paste0(substr(tracks$info, 1, 20),' ...'), tracks$info), "\n") # only print the head 20 characters
  }
  cat(crossline,'\n')
  cat("Species:\n", attr(tracks, "species"), "\n\n", sep = "")
  cat(crossline,'\n')
  cat("Source:\n", paste(capture.output(print(attr(tracks, "source"))), collapse = "\n"), "\n", sep = "")
  cat(crossline,'\n')
}


# Non-S3-generic methods for presentation (print, summary, plot)
summarize_track_type <- function(tracks) {
  stopifnot(inherits(tracks,'Tracks'))
  tracks |>
    dplyr::group_by(track_type) |>
    dplyr::summarize(
      unique_track_count = n_distinct(track_id), 
      unique_point_count = n(),
      .groups = "drop"
    )
}

format_summary_track_type <- function(summary_track_type) {
  stopifnot(inherits(summary_track_type,'data.frame'))
  
  track_type_summary_str <- apply(summary_track_type, 1, function(row) {
    paste0(
      sprintf("Track Type: %s\n", row["track_type"]),
      sprintf("Unique Tracks: %s; ", row["unique_track_count"]),
      sprintf("Unique Points: %s\n", row["unique_point_count"])
    )
  })
  
  summary_str <- paste(track_type_summary_str, collapse = "\n")
  return(summary_str)
}


# Non-S3-generic methods for conversion of data and objects

#' @export
as_BirdFlowTracks <- function(tracks, bf, valid_only = TRUE, sort_id_and_dates = TRUE, reset_index=FALSE){
  # Check input
  stopifnot(inherits(tracks, 'Tracks'))
  stopifnot(inherits(bf, 'BirdFlow'))
  stopifnot(is.logical(sort_id_and_dates))
  stopifnot(is.logical(reset_index))
  stopisnot(is.logical(valid_only))
  
  # Sort & reindex
  if (sort_id_and_dates){
    tracks <- tracks |> sort_by_id_and_dates()
  }
  if (reset_index){
    tracks <- tracks |> reset_index()
  }
  
  # Conversion
  ## Spatial
  xy <- BirdFlowR::latlon_to_xy(lat = tracks$lat, lon = tracks$lon, bf = bf)
  tracks$x <- xy$x
  tracks$y <- xy$y
  tracks$i <- BirdFlowR::xy_to_i(x = tracks$x, y = tracks$y, bf)
  
  ## Temporal
  tracks$date <- lubridate::as_date(tracks[['date']])
  tracks$timestep <- BirdFlowR::lookup_timestep(tracks$date, bf, allow_failure = TRUE)
  
  # Only sucessfully converted spatiotemporal points will be included
  if (valid_only){
    tracks <- df |>
      dplyr::filter(!is.na(x) & !is.na(y) & !is.na(i) & !is.na(timestep))
  }
  
  # Transform to BirdFlowTracks
  tracks <- BirdFlowTracks(birdflow_track_df = tracks,
                           species = attr(tracks, 'species'),
                           source = attr(tracks, 'source'))
  return(tracks)
}

reset_index <- function(tracks) {
  stopifnot(inherits(tracks,'data.frame'))
  # Get unique track_ids and create a mapping
  unique_ids <- unique(tracks$track_id)
  new_ids <- paste0("track_", seq_along(unique_ids))
  
  # Create a lookup table
  id_mapping <- setNames(new_ids, unique_ids)
  
  # Replace
  tracks$track_id <- id_mapping[tracks$track_id]
  
  return(tracks)
}

sort_by_id_and_dates <- function(tracks){
  stopifnot(inherits(tracks,'data.frame'))
  sorted_tracks <- tracks |> dplyr::arrange(.data[["track_id"]], .data[["date"]])
  return(sorted_tracks)
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
    dplyr::group_by(track_id, stay_id) |>
    dplyr::mutate(
      stay_len = max(.data[[timestep_col]]) - min(.data[[timestep_col]]) + 1
    ) |>
    dplyr::select(-timestep_diff, -i_change)
  
  return(new_df)
}

preserve_s3_attributes <- function(original, modified) {
  
  # Class check
  stopifnot(inherits(original,c('BirdFlowTracks', 'Tracks', 'data.frame')))
  
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



