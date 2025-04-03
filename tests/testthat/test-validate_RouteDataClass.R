test_that("Validations of Routes, BirdFlowRoutes, and BirdFlowIntervals work", {
    set.seed(42)

    fake_routes <- make_fake_routes()
    bf <- BirdFlowModels::amewoo
    species <- list(
        species_code = "amewoo",
        scientific_name = "Scolopax minor",
        common_name = "American Woodcock"
    )
    source1 <- "Maine"

    expect_no_error(my_routes <- Routes(fake_routes, species = species,
                                        source = source1))

    expect_no_error(validate_Routes(my_routes))
    expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf = bf))
    expect_no_error(validate_BirdFlowRoutes(my_bfroutes))
    expect_no_error(my_intervals <- as_BirdFlowIntervals(my_bfroutes))
    expect_no_error(validate_BirdFlowIntervals(my_intervals))
})
