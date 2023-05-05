test_that("animate_movement_vectors runs cleanly", {
  skip_on_cran()
  bf <- BirdFlowModels::amewoo
  expect_no_error(a <- animate_movement_vectors(bf, 1, 4))
  expect_no_error(
    gif <- gganimate::animate(a, fps = 1, device = "ragg_png",
                              width = 6, height = 5,
                              res = 150, units = "in")
    )

})
