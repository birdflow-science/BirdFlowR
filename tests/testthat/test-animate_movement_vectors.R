test_that("animate_movement_vectors runs cleanly", {
  skip_on_cran()
  bf <- BirdFlowModels::amewoo
  expect_no_error(a <- animate_movement_vectors(bf, 1, 4))
  t_dir <- tempdir()
  on.exit({
    # file cleanup from file_renderer
    l <- list.files(path = t_dir, pattern = '^gganim_plot', full.names = TRUE)
    for (f in l){
      file.remove(f)
    }
  })
  expect_no_error(
    gif <-
      gganimate::animate(
        a,
        fps = 1,
        renderer = gganimate::file_renderer(dir = t_dir),
        device = "ragg_png",
        width = 6,
        height = 5,
        res = 150,
        units = "in"
      )
  )
})
