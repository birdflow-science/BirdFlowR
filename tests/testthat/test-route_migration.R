test_that("route_migration() works with n = 1", {
  set.seed(1)
  # expect no error:
  expect_error( rts <- route_migration(BirdFlowModels::amewoo, n = 1), NA )

  # Expect consistent output
  novel_points <- rts[!duplicated(rts[, c("x", "y", "route")]), ]
  novel_points <- as.data.frame(novel_points)
  # Expect consistent output
  expect_snapshot(novel_points)

})



test_that("route_migration() works with n > 1", {
  set.seed(1)
  # expect no error:
  expect_no_error(
    rts <- route_migration(BirdFlowModels::amewoo, migration = "fall",
                           n = 3))

  novel_points <- rts$points[!duplicated(rts$points[, c("x", "y", "route")]), ]
  # Expect consistent output
  expect_snapshot(novel_points)
})




