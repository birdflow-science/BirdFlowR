test_that("lookup_timestep_sequence() throws useful error if start = season", {
  bf <- BirdFlowModels::amewoo
  expect_error(lookup_timestep_sequence(bf, start = "prebreeding"),
               "It looks like you are supplying a season name to start.")

})

test_that("lookup_timestep_sequence() works with timestep input", {
  bf <- BirdFlowModels::rewbla

  # Not over boundary
  expect_equal(lookup_timestep_sequence(bf, start = 1, end = 5), 1:5) # Forward
  expect_equal(lookup_timestep_sequence(bf, start = 5, end = 1,
                                        direction = "backward"), 5:1)

  # Forward over year boundary
  s <- lookup_timestep_sequence(bf, start = 50, end =  3, direction = "forward")
  expect_equal(s, c(50:52, 1:3))

  # Backward over year boundary
  s <- lookup_timestep_sequence(bf, start = 3, end = 50, direction = "backward")
  expect_equal(s, c(3:1, 52:50))

})


test_that("lookup_timestep_sequence() works with date input", {
  bf <- BirdFlowModels::rewbla

  ###  Not over boundary
  d1 <- as.Date("2023-04-11")
  d2 <- as.Date("2023-6-01")
  t1 <- lookup_timestep(d1, bf)
  t2 <- lookup_timestep(d2, bf)

  # With appropriate direction argument
  s1 <-   lookup_timestep_sequence(bf, start = d1, end = d2,
                                   direction = "forward")
  expect_equal(s1, t1:t2) # Forward
  s2 <- lookup_timestep_sequence(bf, start = d2, end = d1,
                                 direction = "backward")
  expect_equal(s2, t2:t1)

  # With no direction argument
  s3 <-   lookup_timestep_sequence(bf, start = d1, end = d2)
  expect_equal(s3, t1:t2) # Forward
  s4 <- lookup_timestep_sequence(bf, start = d2, end = d1)
  expect_equal(s4, t2:t1)

  # With conflicting direction argument (expect errors)
  expect_error(lookup_timestep_sequence(bf, start = d1, end = d2,
                                        direction = "backward"))
  expect_error(lookup_timestep_sequence(bf, start = d2, end =  d1,
                                        direction = "forward"))

  ### Crossing year boudary
  d1 <- as.Date("2023-12-25")
  d2 <- as.Date("2024-1-12")
  t1 <- lookup_timestep(d1, bf)
  t2 <- lookup_timestep(d2, bf)


  # Forward over year boundary
  s <- lookup_timestep_sequence(bf, start = d1, end = d2,
                                direction = "forward")
  expect_equal(s, c(t1:52, 1:t2))

  # Backward over year boundary
  s <- lookup_timestep_sequence(bf, start = d2, end = d1,
                                direction = "backward")
  expect_equal(s, c(t2:1, 52:t1))

})



test_that("lookup_timestep_sequence() works with POSIX date input", {
  bf <- BirdFlowModels::rewbla

  ###  Not over boundary
  d1 <- as.POSIXct(as.Date("2023-04-11"))
  d2 <- as.POSIXct(as.Date("2023-6-01"))
  t1 <- lookup_timestep(d1, bf)
  t2 <- lookup_timestep(d2, bf)

  # With appropriate direction argument
  s1 <-   lookup_timestep_sequence(bf, start = d1, end = d2,
                                   direction = "forward")
  expect_equal(s1, t1:t2) # Forward
  s2 <- lookup_timestep_sequence(bf, start = d2, end = d1,
                                 direction = "backward")
  expect_equal(s2, t2:t1)

  # With no direction argument
  s3 <-   lookup_timestep_sequence(bf, start = d1, end = d2)
  expect_equal(s3, t1:t2) # Forward
  s4 <- lookup_timestep_sequence(bf, start = d2, end = d1)
  expect_equal(s4, t2:t1)

  # With conflicting direction argument (expect errors)
  expect_error(lookup_timestep_sequence(bf, start = d1, end =  d2,
                                        direction = "backward"))
  expect_error(lookup_timestep_sequence(bf, start = d2, end = d1,
                                        direction = "forward"))

  ### Crossing year boundary
  d1 <- as.POSIXct("2023-12-25")
  d2 <- as.POSIXct("2024-1-12")
  t1 <- lookup_timestep(d1, bf)
  t2 <- lookup_timestep(d2, bf)


  # Forward over year boundary
  s <- lookup_timestep_sequence(bf, start = d1, end = d2, direction = "forward")
  expect_equal(s, c(t1:52, 1:t2))

  # Backward over year boundary
  s <- lookup_timestep_sequence(bf, start = d2, end = d1,
                                direction = "backward")
  expect_equal(s, c(t2:1, 52:t1))

})



test_that("lookup_timestep_sequence() works with character date input", {
  bf <- BirdFlowModels::rewbla

  ###  Not over boundary
  d1 <- "2023-04-11"
  d2 <- "2023-6-01"
  t1 <- lookup_timestep(d1, bf)
  t2 <- lookup_timestep(d2, bf)

  # With appropriate direction argument
  s1 <-   lookup_timestep_sequence(bf, start = d1, end = d2,
                                   direction = "forward")
  expect_equal(s1, t1:t2) # Forward
  s2 <- lookup_timestep_sequence(bf, start = d2, end = d1,
                                 direction = "backward")
  expect_equal(s2, t2:t1)

  # With no direction argument
  s3 <-   lookup_timestep_sequence(bf, start = d1, end = d2)
  expect_equal(s3, t1:t2) # Forward
  s4 <- lookup_timestep_sequence(bf, start = d2, end = d1)
  expect_equal(s4, t2:t1)

  # With conflicting direction argument (expect errors)
  expect_error(lookup_timestep_sequence(bf, start = d1, end =  d2,
                                        direction = "backward"))
  expect_error(lookup_timestep_sequence(bf, start = d2, end = d1,
                                        direction = "forward"))

  ### Crossing year boundary
  d1 <- "2023-12-25"
  d2 <- "2024-1-12"
  t1 <- lookup_timestep(d1, bf)
  t2 <- lookup_timestep(d2, bf)


  # Forward over year boundary
  s <- lookup_timestep_sequence(bf, start = d1, end = d2, direction = "forward")
  expect_equal(s, c(t1:52, 1:t2))

  # Backward over year boundary
  s <- lookup_timestep_sequence(bf, start = d2, end = d1,
                                direction = "backward")
  expect_equal(s, c(t2:1, 52:t1))

})


test_that("lookup_timestep_sequence() works with 'all'", {

  bf <- BirdFlowModels::rewbla

  # Forward
  s <- lookup_timestep_sequence(bf, "all")
  expect_equal(s, c(1:52, 1))

  # Backward
  s <- lookup_timestep_sequence(bf, "all", direction = "backward")
  expect_equal(s, c(1, 52:1))

})


test_that("lookup_timestep_sequence() works with season input", {

  bf <- BirdFlowModels::rewbla

  # Four seasons buffer = 0  cover entire year with no overlap
  spring  <- lookup_timestep_sequence(bf, "spring", season_buffer = 0)
  summer <-  lookup_timestep_sequence(bf, "summer", season_buffer = 0)
  fall <-    lookup_timestep_sequence(bf, "fall", season_buffer = 0)
  winter <-  lookup_timestep_sequence(bf, "winter", season_buffer = 0)
  all <- c(spring, summer, fall, winter)
  expect_false(any(duplicated(all)))
  expect_true(setequal(all, 1:n_timesteps(bf)))

  # Four seasons, buffer = 1, cover the entire year.  overlap will be 2 at each
  # transition, 8 total duplicated values
  spring  <- lookup_timestep_sequence(bf, "spring", season_buffer = 1)
  summer <-  lookup_timestep_sequence(bf, "summer", season_buffer = 1)
  fall <-    lookup_timestep_sequence(bf, "fall", season_buffer = 1)
  winter <-  lookup_timestep_sequence(bf, "winter", season_buffer = 1)
  all <- c(spring, summer, fall, winter)
  expect_equal(sum(duplicated(all)), 8)
  expect_true(setequal(all, 1:n_timesteps(bf)))

  ### Winter with buffer tested explicitly
  buffer <- 2
  start <- lookup_timestep(species_info(bf, "nonbreeding_start"), bf)
  end <-   lookup_timestep(species_info(bf, "nonbreeding_end"), bf)
  end <- end + buffer
  start <- start - buffer
  winter <- c(start:n_timesteps(bf), 1:end)

  # Forward
  expect_equal(lookup_timestep_sequence(bf, "winter", season_buffer = buffer),
               winter)

  # Backward
  s <- lookup_timestep_sequence(bf, "winter", season_buffer = buffer,
                               direction = "backward")
  expect_equal(s, rev(winter))


  ### Pre breeding migration tested explicitly
  buffer <- 1
  start <- lookup_timestep(species_info(bf, "Prebreeding_migration_start"), bf)
  end <-   lookup_timestep(species_info(bf, "Prebreeding_migration_end"), bf)
  end <- end + buffer
  start <- start - buffer
  prebreeding_migration <- start:end

  # Forward (With default buffer of 1)
  s <- lookup_timestep_sequence(bf, "prebreeding_migration")
  expect_equal(s, prebreeding_migration)

  # Backward with default buffer of 1
  s <- lookup_timestep_sequence(bf, "prebreeding_migration",
                                direction = "backward")
  expect_equal(s, rev(prebreeding_migration))

})


test_that("lookup_timestep() works with start and n input", {
  bf <- BirdFlowModels::rewbla

  # Forward over year boundary
  expect_no_error(a <- lookup_timestep_sequence(bf, start = 50, n_steps = 10))
  expect_equal(a, c(50:52, 1:8))
  expect_equal(length(a), 11)

  # Backward over year boundary
  expect_no_error(a <- lookup_timestep_sequence(bf, start = 3, n_steps = 5,
                                                 direction = "backward"))
  expect_equal(a, c(3:1, 52:50))
  expect_equal(length(a), 6)
})

test_that("lookup_timestep() throws expected errors with non-cyclical models", {
  bf <- BirdFlowModels::amewoo
  expect_false(is_cyclical(bf))
  expect_error(a <- lookup_timestep_sequence(bf, start = 50, n_steps = 10),
               regexp = "x is not cyclical and n_steps is large enough")
  expect_error(a <- lookup_timestep_sequence(bf, start = 3, n_steps = 10,
                                             direction = "backward"),
               regexp = "x is not cyclical and n_steps is large enough")
})
