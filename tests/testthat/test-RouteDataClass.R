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