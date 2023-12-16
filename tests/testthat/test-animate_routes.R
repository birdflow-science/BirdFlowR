test_that("animate_routes() runs", {

  bf <- BirdFlowModels::amewoo

  start <- lookup_timestep(species_info(bf, "prebreeding_migration_start"), bf)
  end <- start + 3

  xy <-  get_distr(bf, start) |>
    sample_distr(n = 3) |>
    apply(2, function(x) which(as.logical(x))) |>
    i_to_xy(bf) |>
    as.data.frame()

  # Short routes
  set.seed(1)
  rts <- route(bf, x_coord = xy$x, y_coord = xy$y, start = start, end = end)
  expect_no_error(anim <- animate_routes(rts, bf))

  timesteps <- sort(unique(rts$timestep))

  skip_if_not_installed("ragg")

  suppressMessages(
    expect_no_error(
      gif <-
        gganimate::animate(anim,
                           device = "ragg_png", # ragg_png is fast and pretty
                           width = 6, height = 5,
                           res = 150, units = "in",
                           nframes = length(timesteps) * 2, fps = 2)
    ))

})
