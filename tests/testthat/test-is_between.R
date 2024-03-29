test_that("is_between() works", {

  # Sparsifying and truncating to speed things up
  bf <- BirdFlowModels::amewoo
  bf <- truncate_birdflow(bf, start = 1, end = 5)
  bf <- sparsify(bf, "conditional", .9, p_protected = 0.05)

  between <- is_between(bf)

  expect_snapshot(sum(between[[1]]))


})
