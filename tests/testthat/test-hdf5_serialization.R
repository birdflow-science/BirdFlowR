test_that("Reading and writing routes and intervals works", {
  set.seed(42)

  fake_routes <- make_fake_routes()
  bf <- BirdFlowModels::amewoo
  species1 <- bf$species
  source1 <- "BirdFlow"

  dir <- withr::local_tempdir()
  routes_file <- file.path(dir, "routes.hdf5")
  bf_routes_file <- file.path(dir, "bf_routes.hdf5")
  intervals_file <- file.path(dir, "intervals_routes.hdf5")


  expect_no_error({
    # Routes
    my_routes1 <- Routes(fake_routes,
                        species = species1,
                        source = source1
    )
    write_routes(my_routes1, routes_file)
    my_routes2 <- read_routes(routes_file)
    expect_true(identical(my_routes1, my_routes2))
  })

  expect_no_error({
    # BirdFlowRoutes
    my_bfroutes1 <- as_BirdFlowRoutes(my_routes1, bf = bf)
    write_routes(my_bfroutes1, bf_routes_file)
    my_bfroutes2 <- read_routes(bf_routes_file)
    expect_true(identical(my_bfroutes1, my_bfroutes2))
  })

  expect_no_error({
    # BirdFlowIntervals
    my_intervals1 <- as_BirdFlowIntervals(my_bfroutes1)
    write_intervals(my_intervals1, intervals_file)
    my_intervals2 <- read_intervals(intervals_file)
    expect_true(identical(my_intervals1, my_intervals2))
  })
})


test_that("Reading and writing routes and intervals works with NA", {
  set.seed(42)

  fake_routes <- make_fake_routes()
  bf <- BirdFlowModels::amewoo
  species1 <- bf$species
  source1 <- "BirdFlow"

  dir <- withr::local_tempdir()
  routes_file <- file.path(dir, "routes.hdf5")
  bf_routes_file <- file.path(dir, "bf_routes.hdf5")
  intervals_file <- file.path(dir, "intervals_routes.hdf5")

  expect_no_error({
    # Routes
    my_routes1 <- Routes(fake_routes,
                         species = species1,
                         source = source1
    )
    my_routes1$species$prebreeding_migration_end <- NA
    write_routes(my_routes1, routes_file)
    my_routes2 <- read_routes(routes_file)
    expect_true(identical(my_routes1, my_routes2))

    # BirdFlowRoutes
    my_bfroutes1 <- as_BirdFlowRoutes(my_routes1, bf = bf)
    my_bfroutes1$species$prebreeding_migration_end <- NA
    write_routes(my_bfroutes1, bf_routes_file)
    my_bfroutes2 <- read_routes(bf_routes_file)
    expect_true(identical(my_bfroutes1, my_bfroutes2))

    # BirdFlowIntervals
    my_intervals1 <- as_BirdFlowIntervals(my_bfroutes1)
    my_intervals1$species$prebreeding_migration_end <- NA
    write_intervals(my_intervals1, intervals_file)
    my_intervals2 <- read_intervals(intervals_file)
    expect_true(identical(my_intervals1, my_intervals2))
  })
})


test_that("Reading and writing routes and intervals works with NULL", {

    skip("Always skipped. Writing NULL values is not supported.")


    set.seed(42)

    dir <- withr::local_tempdir()
    routes_file <- file.path(dir, "routes.hdf5")
    bf_routes_file <- file.path(dir, "bf_routes.hdf5")
    intervals_file <- file.path(dir, "intervals_routes.hdf5")


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


      my_routes1$species['prebreeding_migration_end'] <- list(NULL)
      write_routes(my_routes1, routes_file)
      my_routes2 <- read_routes(routes_file)
      expect_true(identical(my_routes1, my_routes2))

      # BirdFlowRoutes
      my_bfroutes1 <- as_BirdFlowRoutes(my_routes1, bf = bf)
      my_bfroutes1$species['prebreeding_migration_end'] <- list(NULL)
      write_routes(my_bfroutes1, bf_routes_file)
      my_bfroutes2 <- read_routes(bf_routes_file)
      expect_true(identical(my_bfroutes1, my_bfroutes2))

      # BirdFlowIntervals
      my_intervals1 <- as_BirdFlowIntervals(my_bfroutes1)
      my_intervals1$species['prebreeding_migration_end'] <- list(NULL)
      write_intervals(my_intervals1, intervals_file)
      my_intervals2 <- read_intervals(intervals_file)
      expect_true(identical(my_intervals1, my_intervals2))

    })
 })
