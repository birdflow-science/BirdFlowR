test_that("calc_movement_vectors is consistent", {
  bf <- BirdFlowModels::amewoo
  expect_no_error(mv <- calc_movement_vectors(bf, 5))
  mv <- mv[1:2, !names(mv) == "width"]
  expect_snapshot(mv)
})
