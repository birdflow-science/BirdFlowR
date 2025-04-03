
test_that("Reset index in converting Routes to BirdFlowRoutes works", {
  set.seed(42)
  fake_routes <- make_fake_routes_one_point_per_route()
  bf <- BirdFlowModels::amewoo
  species1 <- "amewoo"
  source1 <- "Unkown"

  expect_no_error(my_routes <- Routes(fake_routes, species = species1,
                                      source = source1))
  expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf = bf,
                                                   reset_index = TRUE))
})

test_that("Reset index in BirdFlowRoutes works", {

  bf <- BirdFlowModels::amewoo

  birdflow_route_df <- data.frame(
    route_id = c("001", "001", "001", "001", "001", "003", "003",
                 "003", "004"),
    date = as.Date(c("2025-01-01", "2025-01-08", "2025-01-15",
                     "2025-01-21", "2025-02-10", "2025-03-01",
                     "2025-05-01", "2025-06-01", "2025-05-01")),
    lon = c(-75.0060, -75.0060, -74.0060, -87.6298, -87.6298, -87.6298,
            -89.6298, -85.6298, -95.3698),
    lat = c(39.7128, 39.7128, 40.7128, 41.8781, 41.8781, 41.8781, 42.8781,
            40.8781, 29.7604),
    x = c(1000, 2000, 1000, 2000, 1000, 2000, 1000, 2000, 1000),
    y = c(1000, 2000, 1000, 2000, 1000, 2000, 1000, 2000, 1000),
    i = as.integer(c(1, 2, 1, 2, 1, 2, 1, 2, 1)),
    timestep = as.integer(c(1, 2, 3, 4, 5, 1, 2, 3, 1)),
    route_type = c("tracking", 'tracking', "tracking", 'tracking',
                   'tracking', "motus", "motus", "motus", "motus")
  )
  geom <- bf$geom

  dates <- data.frame(
    timestep = 1:2,
    date = as.Date(c("2022-01-04", "2022-01-11")),
    label = c("January 4", "January 11"),
    julian = c(4, 11),
    week = c(1, 2)
  )
  species <- list(
    species_code = "amewoo",
    scientific_name = "Scolopax minor",
    common_name = "American Woodcock"
  )

  metadata <- bf$metadata[c("n_active", "ebird_version_year")]

  expect_no_error(
    birdflowroutes_object <- BirdFlowRoutes(
      birdflow_route_df,
      species = species,
      metadata = metadata,
      geom = geom,
      dates = dates,
      source = "example_source",
      reset_index = TRUE)
  )


})

test_that("Extra columns are retained and don't cause problems", {
  set.seed(42)

  fake_routes <- make_fake_routes()
  fake_routes$info <- 'AABB some info'
  bf <- BirdFlowModels::amewoo
  species1 <- "amewoo"

  source1 <- c("eBird", "BirdFlowR")

  expect_no_error(
    my_routes <- Routes(fake_routes, species = species1, source = source1))

  expect_no_error(
    my_bfroutes <- as_BirdFlowRoutes(my_routes, bf=bf))

  expect_no_error(
    my_intervals <- as_BirdFlowIntervals(my_bfroutes))

  my_intervals$data$info <- 'Some random info for the intervals'

})

