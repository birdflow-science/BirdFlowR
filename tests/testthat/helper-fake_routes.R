#' Make fake routes
#'
#' `make_fake_routes()` is a helper function used in testing `Routes`, `BirdFlowRoutes`, and `BirdFlowIntervals`.
#'
#' @return a dataframe with routes info

make_fake_routes <- function(){
  route_df1 <- data.frame(
    route_id = c("001", "001", "001", "001", "001", "003", "003", "003", "004"),
    date = as.Date(c("2025-01-01", "2025-01-08", "2025-01-15", "2025-01-21", "2025-02-10", "2025-03-01", "2025-05-01", "2025-06-01", "2025-05-01")),
    lon = c(-75.0060, -75.0060, -74.0060, -87.6298, -87.6298, -87.6298, -89.6298, -85.6298, -95.3698),
    lat = c(39.7128, 39.7128, 40.7128, 41.8781, 41.8781, 41.8781, 42.8781, 40.8781, 29.7604),
    route_type = c("tracking", 'tracking', "tracking", 'tracking', 'tracking', "motus", "motus", "motus", "motus")
  )
  return(route_df1)
}

make_fake_routes_one_point_per_route <- function(){
  route_df <- data.frame(
    route_id = 1:3,
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    lon = c(-90, -89, -88),
    lat = c(40, 41, 42),
    route_type = c("tracking", "banding", "unknown")
  )

  return(route_df)
}


# Make synthetic routes and then interpolate to a defined
# interval while adding a little noise.

make_fake_tracking_data <- function(bf, n,
                                    interval =  as.difftime(.1, units = "days"),
                                    year_sd = 5,
                                    bandwidth = 100,
                                    sd = 3000,
                                    ...) {

  bf <- BirdFlowModels::amewoo
  n_rts <- n
  rts <- route(bf = bf, n = n_rts, ...)
  d <- rts$data


  # sd sets the magnitude of noise, bandwidt (in points) sets
  # how far the autocorrelations spreads
  # autocorrelation bandwidth in the added noise

   # year_sd controls random difftime offset to entire track

  from <- min(d$date) |> lubridate::as_datetime()
  to <- max(d$date) |> lubridate::as_datetime()
  date_times <- seq(from = from, to = to, by = interval)


  interp_list <- vector(mode = "list", length = n_rts)
  for (i in seq_len(n_rts)) {
    route <- d[d$route_id == i, ]
  #  new_location <- which(c(TRUE, route$i[-1] != route$i[-nrow(route)]))
  #  route <- route[new_location, , drop = FALSE]
    route$date <- lubridate::as_datetime(route$date)
    interp <- data.frame(date_time = date_times,
                         x = approx(x = route$date,
                                    y = route$x,
                                     xout = date_times)$y,
                         y = approx(x = route$date,
                                    y = route$y,
                                    xout = date_times)$y,
                         route_id = i)


    x_noise <- filter(rnorm(nrow(interp), sd = sd),
                      filter = rep(1, bandwidth),
                      circular = TRUE)
    y_noise <- filter(rnorm(nrow(interp), sd = sd),
                      filter = rep(1, bandwidth),
                      circular = TRUE)

    interp$x <- interp$x + as.numeric(x_noise)
    interp$y <- interp$y + as.numeric(y_noise)

    interp$date_time <- interp$date_time +
      as.difftime(rnorm(1, sd = year_sd) * 365, units = "days")

    interp_list[[i]] <- interp

  }

  track <- do.call(rbind, args = interp_list)

  lat_lon <- xy_to_latlon(track[c("x", "y")], bf = bf)
  track <- dplyr::rename(track, date = "date_time")  |>
    cbind(lat_lon)
  track$route_type <- "tracking"

  track <- track[!is.na(track$lat) & !is.na(track$lon), , drop = FALSE]

  track <- track[, !names(track) %in% c("x", "y")]

  return(track)
}

