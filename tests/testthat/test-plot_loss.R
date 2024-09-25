test_that("get_loss() works", {
  bf  <- BirdFlowModels::amewoo
  expect_no_error(loss <- get_loss(bf))
  expect_equal(names(loss), c("dist", "ent", "obs", "total"))
  expect_s3_class(loss, "data.frame")
})


test_that("plot_loss() works", {
  bf  <- BirdFlowModels::amewoo
  expect_no_error(p <- plot_loss(bf))
  expect_no_error(print(p))
})
