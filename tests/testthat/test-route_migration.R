test_that("route_migration() works with n = 1", {
  set.seed(1)
  # expect no error:
  expect_error( rts <- route_migration(BirdFlowModels::amewoo, n = 1), NA )

  # Expect consistent output
  expect_snapshot(rts)

})
