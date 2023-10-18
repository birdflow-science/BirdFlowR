test_that("lookup_transitions works with timesteps", {
  bf <- BirdFlowModels::rewbla

  # Forward
  a <- lookup_transitions(bf, start = 1,  end = 4)
  expect_equal(a, c("T_01-02", "T_02-03", "T_03-04"))

  # Backward
  a <- lookup_transitions(bf, start = 4,  end = 1, direction = "backward")
  expect_equal(a, c("T_04-03", "T_03-02", "T_02-01"))

  # Across year boundary
  a <- lookup_transitions(bf, start = 51, end = 2)
  expect_equal(a, c("T_51-52", "T_52-01", "T_01-02"))
  a <- lookup_transitions(bf, start = 2, end = 51, direction = "backward")
  expect_equal(a, c("T_02-01", "T_01-52", "T_52-51"))

  # Forward dates across year boundary
  a <- lookup_transitions(bf, start = "2022-12-10", end = "2023-01-13")
  expect_equal(a, c("T_49-50", "T_50-51", "T_51-52", "T_52-01", "T_01-02"))

  # Backward dates across year boundary
  a <- lookup_transitions(bf,  start = "2023-01-13", end = "2022-12-10")
  expect_equal(a, c("T_02-01", "T_01-52", "T_52-51", "T_51-50", "T_50-49"))
  expect_equal(lookup_timestep("2023-01-13", bf), 2)

  # Season buffer = 0
  a <- lookup_transitions(bf, "prebreeding", season_buffer = 0)
  start <- lookup_timestep(species_info(bf, "prebreeding_migration_start"), bf)
  end <- lookup_timestep(species_info(bf, "prebreeding_migration_end"), bf)
  expect_equal(a[1], paste0("T_0", start, "-0", start + 1))
  expect_equal(a[length(a)], paste0("T_", end - 1, "-", end))

  # season with default buffer of 1
  a <- lookup_transitions(bf, "prebreeding")
  start <- lookup_timestep(species_info(bf, "prebreeding_migration_start"), bf)
  end <- lookup_timestep(species_info(bf, "prebreeding_migration_end"), bf)
  start <- start - 1  # add buffer
  end <- end + 1
  expect_equal(a[1], paste0("T_0", start, "-0", start + 1))
  expect_equal(a[length(a)], paste0("T_", end - 1, "-", end))

})

test_that("lookup_transition() works with example from github issue #66", {
  bf <- BirdFlowModels::rewbla
  a <- lookup_transitions(bf, start = "2021-12-15", end = "2022-01-15",
                          direction = "forward")
  expect_equal(a, c("T_50-51", "T_51-52", "T_52-01", "T_01-02", "T_02-03"))
})




test_that("lookup_transitions() behaves on edge cases and errors conditions", {
  bf <- BirdFlowModels::rewbla

  # This is here to close gaps in code coverage

  # integer input
  expect_no_error(lookup_transitions(bf, start = 1L, end = 4L))

  # Timestep combined with date
  expect_error(lookup_transitions(bf, start = 1, end = "2022-03-05"))


  ### STILL need to test out of range but I haven't implemented truncated
  # models yet.






})
