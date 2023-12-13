test_that("print works with BirdFlow model", {
 bf <- BirdFlowModels::amewoo
 expect_output(print(bf))
})

test_that("print works with BirdFlowRoutes object", {
  bf <- BirdFlowModels::amewoo
  rts <- route(bf, start = 10, end = 15)
  expect_output(print(rts))
})
