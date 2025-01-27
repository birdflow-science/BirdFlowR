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


