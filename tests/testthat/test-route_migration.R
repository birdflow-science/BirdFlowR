test_that("route_migration() works with n = 1", {
  set.seed(1)
  # expect no error:
  expect_error( rts <- route_migration(BirdFlowModels::amewoo, n = 1), NA )

  # Expect consistent output
  expect_snapshot(rts)

})



test_that("route_migration() works with n > 1", {
  set.seed(1)
  # expect no error:
  expect_error( rts <- route_migration(BirdFlowModels::amewoo, migration = "fall",  n = 3), NA )

  novel_points <- rts$points[!duplicated(rts$points[, c("x", "y", "route")]), ]
  # Expect consistent output
  expect_snapshot(novel_points)
})

