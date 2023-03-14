test_that("route_migration() works with n = 1", {
  set.seed(1)
  # expect no error:
  expect_error( rts <- route_migration(BirdFlowModels::amewoo, n = 1), NA )

  # Expect consistent output
  novel_points <- rts$points[!duplicated(rts$points[, c("x", "y", "route")]), ]
  # Expect consistent output
  expect_snapshot(novel_points)
  expect_equal(class(rts$lines), c("sf", "data.frame"))
  expect_equal(length(rts$lines), 2)

})



test_that("route_migration() works with n > 1", {
  set.seed(1)
  # expect no error:
  expect_error( rts <- route_migration(BirdFlowModels::amewoo, migration = "fall",  n = 3), NA )

  novel_points <- rts$points[!duplicated(rts$points[, c("x", "y", "route")]), ]
  # Expect consistent output
  expect_snapshot(novel_points)
})

