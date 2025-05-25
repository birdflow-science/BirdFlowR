test_that("Data reading and writing from/to hdf5 files work", {
  set.seed(42)
  
  fake_routes <- make_fake_routes()
  bf <- BirdFlowModels::amewoo
  species1 <- bf$species
  source1 <- "BirdFlow"
  
  expect_no_error({
    # Routes
    my_routes <- Routes(fake_routes,
                        species = species1,
                        source = source1
    )
    write_Rotues(my_routes, './my_routes.hdf5')
    my_routes <- read_Rotues('./my_routes.hdf5')
    if (file.exists('./my_routes.hdf5')) {file.remove('./my_routes.hdf5')}
  })
  
  expect_no_error({
    # BirdFlowRoutes
    my_bfroutes <- as_BirdFlowRoutes(my_routes, bf = bf)
    write_BirdFlowRotues(my_bfroutes, './my_birdflowroutes.hdf5')
    my_bfroutes <- read_BirdFlowRotues('./my_birdflowroutes.hdf5')
    if (file.exists('./my_birdflowroutes.hdf5')) {file.remove('./my_birdflowroutes.hdf5')}
  })
  
  expect_no_error({
    # BirdFlowIntervals
    my_intervals <- as_BirdFlowIntervals(my_bfroutes)
    write_BirdFlowIntervals(my_intervals, './my_birdflowintervals.hdf5')
    my_intervals <- read_BirdFlowIntervals('./my_birdflowintervals.hdf5')
    if (file.exists('./my_birdflowintervals.hdf5')) {file.remove('./my_birdflowintervals.hdf5')}
  })
})



