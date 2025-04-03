test_that("lookup_species_metadata() has consistent output", {
  bf <- BirdFlowModels::amewoo
  expect_equal(names(lookup_species_metadata("amewoo")),
               names(new_BirdFlow()$species))

  v <- ebirdst_pkg_ver()
  v_year <- v[1, 2]
  if (is.na(v) || !v_year == 2022)
    skip("Test expects ebirdst year to be 2022")

  expect_snapshot(lookup_species_metadata("amewoo"))

})
