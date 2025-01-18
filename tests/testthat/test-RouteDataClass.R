test_that("Routes() -> as_BirdFlowRoutes() -> as_BirdFlowIntervals() works", {
    fake_routes <- make_fake_routes()
    bf <- BirdFlowModels::amewoo
    species1 = 'aa'
    source1 = list(a=c('1'), b=c('2'))

    expect_no_error(my_routes <- Routes(fake_routes, species1, source1))
    expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf=bf))
    expect_no_error(my_intervals <- as_BirdFlowIntervals(my_bfroutes))

    expect_snapshot(my_routes)
    expect_snapshot(my_bfroutes)
    expect_snapshot(my_intervals)
})

## More to add -- print, etc