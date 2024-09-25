test_that("weight_between() works", {

  local_quiet()

  # Sparsifying and truncating to speed things up
  bf <- BirdFlowModels::amewoo
  bf <- truncate_birdflow(bf, start = 1, end = 5)
  bf <- sparsify(bf, "conditional", .9, p_protected = 0.05)

  expect_no_error(bw <- weight_between(bf))


})
