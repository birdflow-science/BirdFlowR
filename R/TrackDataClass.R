#' @export
Tracks <- function(track_df, species = NULL, source = NULL){
  # Check input
  stopifnot(is.data.frame(track_df))
  validate_Tracks_input_track_df(track_df)
  
  # Make new Tracks object
  obj <- new_Tracks(track_df, species, source)
  return(obj)
}

new_Tracks <- function(track_df, species, source){
  # Sort columns
  target_ordered_columns <- c('track_id', 'date', 'lon', 'lat', 'track_type')
  track_df <- track_df[, c(target_ordered_columns, setdiff(names(track_df), target_ordered_columns))]
  
  obj <- structure(
    track_df, 
    class = c("Tracks", class(track_df)),
    species = species, 
    source = source
  )
  return(obj)
}


#' @export
BirdFlowTracks <- function(birdflow_track_df,
                           species = NULL,
                           geom = NULL,
                           dates = NULL,
                           source = NULL,
                           sort_id_and_dates = TRUE,
                           reset_index=FALSE){
  # Check input
  stopifnot(inherits(bf, 'BirdFlow'))
  stopifnot(inherits(birdflow_track_df, 'data.frame'))
  validate_BirdFlowTracks_input_birdflow_track_df(birdflow_track_df)
  
  # Sort & reindex
  if (sort_id_and_dates){
    birdflow_track_df <- birdflow_track_df |> sort_by_id_and_dates()
  }
  if (reset_index){
    birdflow_track_df <- birdflow_track_df |> reset_index()
  }
  
  # Make the BirdFlowTracks object
  obj <- new_BirdFlowTracks(birdflow_track_df=birdflow_track_df, 
                            species=species,
                            geom=geom,
                            dates=dates,
                            source=source)
  
  return(obj)
}

new_BirdFlowTracks <- function(birdflow_track_df, species, geom, dates, source){
  
  ## Add stay id
  birdflow_track_df <- birdflow_track_df |>
    dplyr::group_by(.data$track_id) |>
    add_stay_id_with_varied_intervals(timestep_col = "timestep") |> # Here, using add_stay_id_with_varied_intervals, rather than add_stay_id. It takes 'timestep' as input so account for varying intervals, if the data is not sampled in a frequency.
    dplyr::ungroup() |>
    as.data.frame() |>
    preserve_s3_attributes(original=birdflow_track_df)
  
  # Sort columns
  target_ordered_columns <- c('track_id', 'x', 'y', 'i', 'timestep', 'date','track_type','stay_id','stay_len')
  birdflow_track_df <- birdflow_track_df[, c(target_ordered_columns, setdiff(names(birdflow_track_df), target_ordered_columns))]
  
  obj <- structure(
    birdflow_track_df, 
    class = unique(c('BirdFlowTracks', 'Tracks', class(birdflow_track_df))),
    species = species, 
    geom = geom,
    dates = dates,
    source = source
  )
  return(obj)
}




## test
track_df1 <- data.frame(
  track_id = c("001", "001", "001", "001", "001", "003", "004"),          
  date = as.Date(c("2025-01-01", "2025-01-08", "2025-01-15", "2025-01-21", "2025-02-10", "2025-03-01", "2025-05-01")),
  lon = c(-75.0060, -75.0060, -74.0060, -87.6298, -87.6298, -87.6298, -95.3698),         
  lat = c(39.7128, 39.7128, 40.7128, 41.8781, 41.8781, 41.8781, 29.7604), 
  track_type = c("tracking", 'tracking', "tracking", 'tracking', 'tracking', "motus", "motus")
)

# track_df20 <- do.call(rbind, replicate(200, track_df1, simplify = FALSE))
bf <- BirdFlowModels::amewoo
species1 = 'aa'
source1 = list(a=c('1'), b=c('2'))
my_tracks <- Tracks(track_df1, species1, source1)
my_bftracks <- as_BirdFlowTracks(my_tracks, bf=bf)



# 
# bf <- BirdFlowModels::amewoo
# bf <- add_dynamic_mask(bf)  # To ease transition pain
# dyn_mask <- bf$geom$dynamic_mask
# from_coordinates <- !is.null(x_coord) && !is.null(y_coord)
# 
# # Time
# timesteps <-  lookup_timestep_sequence(bf)
# transitions <- as_transitions(timesteps, bf)
# start <- timesteps[1]
# n=10
# loc <- get_distr(bf, start, from_marginals = FALSE) |> sample_distr(n = n, format = "i", bf = bf)
# row <- i_to_row(loc, bf)
# col <- i_to_col(loc, bf)
# 
# initial_distr <- Matrix::Matrix(0, nrow = n_active(bf), ncol = length(row))
# indices <- rc_to_i(row, col, bf)
# sel <- cbind(indices,  seq_len(length(indices)))
# initial_distr[sel] <- 1
# 
# 
# s <- 1:n_active(bf)
# positions <- apply(dyn_mask, 2, function(x) s[x])
# extract_positions <- function(x, timestep) {
#   # given a dynamically masked distribution generate state space index i
#   
#   pos <- positions[[timestep]]  # positions associated with the d. masked
#   # distribution for this timestep
#   if (is.null(dim(x)))
#     return(pos[as.logical(x)])
#   
#   apply(x, 2, function(vals)  pos[as.logical(vals)])
# }
# 
# distr <- initial_distr[dyn_mask[, start], ]
# 
# 
# trajectory <- matrix(nrow = length(transitions) + 1, ncol = length(row))
# dimnames(trajectory) <-  list(timestep = NULL, route = NULL)
# trajectory[1, ] <- extract_positions(distr, timestep = start)
# 
# distr <- Matrix::Matrix(distr, sparse = TRUE)
# for (i in seq_along(transitions)) {
#   tm <- get_transition(bf,  transitions[i])  # transition matrix
#   distr <- tm %*% distr           # project
#   distr <- sample_distr(distr)  # "one hot"
#   trajectory[i + 1, ] <- extract_positions(distr, timestep = timesteps[i + 1])
# }
# 
# 
# rts <- BirdFlowR:::format_trajectory(trajectory, bf, timesteps)
# 
