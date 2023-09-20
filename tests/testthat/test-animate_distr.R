test_that("animate_distr() works with default args", {
  bf <- BirdFlowModels::amewoo

  expect_no_error(a1 <- animate_distr(get_distr(bf, c(1, 10, 20)), bf) )
  expect_no_error(print(a1))

})

test_that("animate_distr() works with dynamic masking", {
  bf <- BirdFlowModels::amewoo

  expect_no_error(a2 <- animate_distr(get_distr(bf, c(1,10, 20)), bf,
                                      show_dynamic_mask = TRUE) )
  expect_no_error(print(a2))
})
