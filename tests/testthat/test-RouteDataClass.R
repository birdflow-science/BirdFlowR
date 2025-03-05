test_that("Routes() -> as_BirdFlowRoutes() -> as_BirdFlowIntervals() works", {
    set.seed(42)

    fake_routes <- make_fake_routes()
    bf <- BirdFlowModels::amewoo
    species1 <- 'aa'
    metadata1 <- NULL
    source1 <- list(a=c('1'), b=c('2'))

    expect_no_error(my_routes <- Routes(fake_routes, species = species1, metadata = metadata1, source = source1))
    expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf=bf))
    expect_no_error(my_intervals <- as_BirdFlowIntervals(my_bfroutes))

    expect_snapshot(my_routes)
    expect_snapshot(my_bfroutes)
    expect_snapshot(my_intervals)

    # Here we will randomly select only one data point for route 001 at timestep 1,
    # so the snapshot is not necessarily the same.
    # But we are using seed, so it will always be the same.
})

## More to add -- print, etc
test_that("If no intervals can be sampled, return NULL", {
    set.seed(42)
    fake_routes <- make_fake_routes_one_point_per_route()
    bf <- BirdFlowModels::amewoo
    species1 <- 'aa'
    metadata1 <- NULL
    source1 <- list(a=c('1'), b=c('2'))

    expect_no_error(my_routes <- Routes(fake_routes, species = species1, metadata = metadata1, source = source1))
    expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf=bf))
    expect_no_error(my_intervals <- as_BirdFlowIntervals(my_bfroutes))
    expect_null(my_intervals)

    expect_snapshot(my_routes)
    expect_snapshot(my_bfroutes)
    expect_snapshot(my_intervals)

})

test_that("Reset index in converting Routes to BirdFlowRoutes works", {
    set.seed(42)
    fake_routes <- make_fake_routes_one_point_per_route()
    bf <- BirdFlowModels::amewoo
    species1 <- 'aa'
    metadata1 <- NULL
    source1 <- list(a=c('1'), b=c('2'))

    expect_no_error(my_routes <- Routes(fake_routes, species = species1, metadata = metadata1, source = source1))
    expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf=bf, reset_index=TRUE))
})

test_that("Reset index in BirdFlowRoutes works", {
    birdflow_route_df <- data.frame(
        route_id = c("001", "001", "001", "001", "001", "003", "003", "003", "004"),
        date = as.Date(c("2025-01-01", "2025-01-08", "2025-01-15", "2025-01-21", "2025-02-10",
        "2025-03-01", "2025-05-01", "2025-06-01", "2025-05-01")),
        lon = c(-75.0060, -75.0060, -74.0060, -87.6298, -87.6298, -87.6298, -89.6298, -85.6298, -95.3698),
        lat = c(39.7128, 39.7128, 40.7128, 41.8781, 41.8781, 41.8781, 42.8781, 40.8781, 29.7604),
        x = c(1000, 2000, 1000, 2000, 1000, 2000, 1000, 2000, 1000),
        y = c(1000, 2000, 1000, 2000, 1000, 2000, 1000, 2000, 1000),
        i = as.integer(c(1, 2, 1, 2, 1, 2, 1, 2, 1)),
        timestep = as.integer(c(1, 2, 3, 4, 5, 1, 2, 3, 1)),
        route_type = c("tracking", 'tracking', "tracking", 'tracking',
        'tracking', "motus", "motus", "motus", "motus")
    )
    geom <- list(nrow = 100, ncol = 200, res = 1, ext = NULL, crs = NULL, mask = NULL, dynamic_mask = NULL)
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
    metadata <- list()
    expect_no_error(birdflowroutes_object <- BirdFlowRoutes(
        birdflow_route_df,
        species = species,
        metadata = metadata,
        geom = geom,
        dates = dates,
        source = "example_source",
        reset_index = TRUE
    ))
})

test_that("Print Routes & BirdFlowRoutes object with `info` works", {
    set.seed(42)

    fake_routes <- make_fake_routes()
    fake_routes$info <- 'AABB some info'
    bf <- BirdFlowModels::amewoo
    species1 <- 'aa'
    metadata1 <- NULL
    source1 <- list(a=c('1'), b=c('2'))

    expect_no_error(my_routes <- Routes(fake_routes, species = species1, metadata = metadata1, source = source1))
    expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf=bf))
    expect_no_error(my_intervals <- as_BirdFlowIntervals(my_bfroutes))
    my_intervals$data$info <- 'Some random info for the intervals'
    expect_snapshot(my_routes)
    expect_snapshot(my_bfroutes)
    expect_snapshot(my_intervals)
})


test_that("Test Interval sampling strategy", {
    set.seed(42)

    fake_routes <- make_fake_routes()
    bf <- BirdFlowModels::amewoo
    species1 <- 'aa'
    metadata1 <- NULL
    source1 <- list(a=c('1'), b=c('2'))

    expect_no_error(my_routes <- Routes(fake_routes, species = species1, metadata = metadata1, source = source1))
    expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf=bf))
    expect_snapshot(my_routes)
    expect_snapshot(my_bfroutes)
    expect_no_error(my_intervals <- as_BirdFlowIntervals(my_bfroutes, max_n=1000, min_day_interval=7, min_km_interval=200))
    expect_snapshot(my_intervals)
    expect_no_error(my_intervals <- as_BirdFlowIntervals(my_bfroutes, max_n=1, min_day_interval=7, min_km_interval=200))
    expect_equal(nrow(my_intervals$data), 1)
    expect_snapshot(my_intervals)
    expect_no_error(my_intervals <- as_BirdFlowIntervals(my_bfroutes, max_n=3, min_day_interval=7, min_km_interval=200))
    expect_snapshot(my_intervals)
    expect_no_error(my_intervals <- as_BirdFlowIntervals(my_bfroutes, max_n=3, min_day_interval=7, min_km_interval=20000))
    expect_snapshot(my_intervals)
    expect_no_error(my_intervals <- as_BirdFlowIntervals(my_bfroutes, max_n=3, min_day_interval=7, min_km_interval=0))
    expect_snapshot(my_intervals)
})


test_that("Routes() -> as_BirdFlowRoutes() with diffrent aggregations works", {
    set.seed(42)

    fake_routes <- make_fake_routes()
    bf <- BirdFlowModels::amewoo
    species1 <- 'aa'
    metadata1 <- NULL
    source1 <- list(a=c('1'), b=c('2'))

    expect_no_error(my_routes <- Routes(fake_routes, species = species1, metadata = metadata1, source = source1))
    expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf=bf, aggregate='mean'))
    expect_snapshot(my_bfroutes)
    expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf=bf, aggregate='median'))
    expect_snapshot(my_bfroutes)
    expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf=bf, aggregate='midweek'))
    expect_snapshot(my_bfroutes)
    expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf=bf, aggregate='random'))
    expect_snapshot(my_bfroutes)
})
