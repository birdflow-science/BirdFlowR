
test_that("animate_movement_vectors runs cleanly", {
  skip_on_cran()

  ov <- birdflow_options("verbose")
  birdflow_options(verbose = FALSE)
  on.exit(birdflow_options(verbose = ov))

  bf <- BirdFlowModels::amewoo
  expect_no_error(a <- animate_movement_vectors(bf, start = 1, end = 4))
  t_dir <- tempdir()
  on.exit({
    # file cleanup from file_renderer
    l <- list.files(path = t_dir, pattern = "^gganim_plot", full.names = TRUE)
    for (f in l){
      file.remove(f)
    }
  }, add = TRUE)
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
