test_that("lookup_species_metadata() has consistent output", {
  skip_if_not_installed("BirdFlowModels")

  skip_if_unsupported_ebirdst_version(use = "lookup_species_metadata")


  bf <- BirdFlowModels::amewoo
  expect_equal(names(lookup_species_metadata("amewoo")),
               names(new_BirdFlow()$species))


  skip_if_wrong_ebirdst_for_snapshot()

  expect_snapshot(lookup_species_metadata("amewoo"))

})
