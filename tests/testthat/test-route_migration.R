test_that("route_migration() works and throws deprecated warning", {
  skip_if_not_installed("BirdFlowModels")
  bf <- BirdFlowModels::amewoo
  expect_warning(rts <- route_migration(bf, 4))
})
