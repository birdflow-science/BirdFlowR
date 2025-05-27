test_that("Data reading and writing from/to hdf5 files work", {
  set.seed(42)
  
  fake_routes <- make_fake_routes()
  bf <- BirdFlowModels::amewoo
  species1 <- bf$species
  source1 <- "BirdFlow"
  
  expect_no_error({
    # Routes
    my_routes1 <- Routes(fake_routes,
                        species = species1,
                        source = source1
    )
    write_Rotues(my_routes1, './my_routes.hdf5')
    my_routes2 <- read_Rotues('./my_routes.hdf5')
    expect_true(identical(my_routes1, my_routes2))
    if (file.exists('./my_routes.hdf5')) {file.remove('./my_routes.hdf5')}
  })
  
  expect_no_error({
    # BirdFlowRoutes
    my_bfroutes1 <- as_BirdFlowRoutes(my_routes1, bf = bf)
    write_BirdFlowRotues(my_bfroutes1, './my_birdflowroutes.hdf5')
    my_bfroutes2 <- read_BirdFlowRotues('./my_birdflowroutes.hdf5')
    expect_true(identical(my_bfroutes1, my_bfroutes2))
    if (file.exists('./my_birdflowroutes.hdf5')) {file.remove('./my_birdflowroutes.hdf5')}
  })
  
  expect_no_error({
    # BirdFlowIntervals
    my_intervals1 <- as_BirdFlowIntervals(my_bfroutes1)
    write_BirdFlowIntervals(my_intervals1, './my_birdflowintervals.hdf5')
    my_intervals2 <- read_BirdFlowIntervals('./my_birdflowintervals.hdf5')
    expect_true(identical(my_intervals1, my_intervals2))
    if (file.exists('./my_birdflowintervals.hdf5')) {file.remove('./my_birdflowintervals.hdf5')}
  })
})


