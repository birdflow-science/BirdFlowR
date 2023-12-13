test_that("route_migration() works and throws deprecated warning", {
  bf <- BirdFlowModels::amewoo
  expect_warning(rts <- route_migration(bf, 4))
})
