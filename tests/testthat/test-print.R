test_that("print works with BirdFlow model", {
 skip_if_not_installed("BirdFlowModels")
 bf <- BirdFlowModels::amewoo
 expect_output(print(bf))
})

test_that("print works with BirdFlowRoutes object", {
  skip_if_not_installed("BirdFlowModels")
  bf <- BirdFlowModels::amewoo
  rts <- route(bf, start = 10, end = 15)
  expect_output(print(rts))
})
