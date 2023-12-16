
test_that("animate_movement_vectors runs cleanly", {
  skip_on_cran()

  local_quiet()

  bf <- BirdFlowModels::amewoo
  expect_no_error(a <- animate_movement_vectors(bf, start = 1, end = 4))

  # Setup and promise to delete temporary directory
  t_dir <- local_test_dir("animation_test")

  expect_no_error(bf_suppress_msg(
    gif <-
      gganimate::animate(
        a,
        fps = 1,
        renderer = gganimate::file_renderer(dir = t_dir),
        device = "ragg_png",
        width = 6,
        height = 5,
        res = 150,
        units = "in")
    ))
})
