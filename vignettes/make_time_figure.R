# Generate vignettes/TimeStructure.png
# A circular diagram showing BirdFlow's cyclical 52-timestep year.
# Colors match the seasonal gradient used by plot_routes().
# Season label positions are derived from BirdFlowModels::amewoo metadata.
# Run with: Rscript vignettes/make_time_figure.R

library(ggplot2)
library(BirdFlowR)
bf <- BirdFlowModels::amewoo

n_ts <- 52

# ── Color palette ──────────────────────────────────────────────────────────
# Mirror of what plot_routes() does with use_seasonal_colors = TRUE
year_cols <- ggthemes::tableau_color_pal("Classic Cyclic")(13)
pal_colors <- c(year_cols, year_cols, year_cols[1])  # two full cycles, 0-1

# hpy (half-proportion-of-year) for timestep t, same formula as plot_routes()
hpy <- function(t) 0.5 * (t - 1) / n_ts

# ── Geometry helpers ────────────────────────────────────────────────────────
# Clockwise-from-top angle for a timestep (can be non-integer for arc endpoints)
ts_angle <- function(t) pi / 2 - (t - 1) / n_ts * 2 * pi

# Ring sector polygon for one timestep slot
sector_poly <- function(t, r_in, r_out, n = 30) {
  a1 <- ts_angle(t - 0.5)
  a2 <- ts_angle(t + 0.5)
  th <- seq(a1, a2, length.out = n)
  rbind(
    data.frame(x = cos(th) * r_out, y = sin(th) * r_out),
    data.frame(x = cos(rev(th)) * r_in,  y = sin(rev(th)) * r_in)
  )
}

# ── Ring data: 52 sectors, each colored by its hpy value ───────────────────
ring_df <- do.call(rbind, lapply(seq_len(n_ts), function(t) {
  p <- sector_poly(t, r_in = 0.76, r_out = 1.22)
  p$hpy   <- hpy(t)
  p$group <- t
  p
}))

# ── Timestep dots ───────────────────────────────────────────────────────────
r_dots <- 0.99
ts_df <- data.frame(
  t   = seq_len(n_ts),
  x   = cos(sapply(seq_len(n_ts), ts_angle)) * r_dots,
  y   = sin(sapply(seq_len(n_ts), ts_angle)) * r_dots,
  hpy = hpy(seq_len(n_ts))
)

# ── Transition lines through the ring, midway between timestep dots ──────────
tr_angles <- pi / 2 - (seq_len(n_ts) - 0.5) / n_ts * 2 * pi
trans_df <- data.frame(
  x    = cos(tr_angles) * 0.76,
  y    = sin(tr_angles) * 0.76,
  xend = cos(tr_angles) * 1.22,
  yend = sin(tr_angles) * 1.22
)

# Year-boundary line (between week 52 and week 1; extends beyond ring)
yr_angle <- pi / 2 + 0.5 / n_ts * 2 * pi
yr_line_df <- data.frame(
  x    = cos(yr_angle) * 0.64,
  y    = sin(yr_angle) * 0.64,
  xend = cos(yr_angle) * 1.40,
  yend = sin(yr_angle) * 1.40
)

# ── Week number labels just outside the outer ring ───────────────────────────
r_weeknum <- 1.30
week_df <- data.frame(
  t     = seq_len(n_ts),
  x     = cos(sapply(seq_len(n_ts), ts_angle)) * r_weeknum,
  y     = sin(sapply(seq_len(n_ts), ts_angle)) * r_weeknum,
  label = as.character(seq_len(n_ts))
)

# ── Month labels ─────────────────────────────────────────────────────────────
month_mid_week <- c(2.5, 6.9, 11.1, 15.4, 19.6, 23.8, 28.0, 32.3, 36.6, 40.9, 45.1, 49.4)
month_angles   <- pi / 2 - (month_mid_week - 1) / n_ts * 2 * pi
month_df <- data.frame(
  label = month.abb,
  x     = cos(month_angles) * 1.50,
  y     = sin(month_angles) * 1.50
)

# ── Season name labels inside the ring hole ──────────────────────────────────
# Mid-timestep for each season derived from species metadata (amewoo).
# Seasons that wrap across the year boundary (nonbreeding) are handled by
# allowing mid_t > n_ts; ts_angle() accepts non-integer values.
season_bounds <- list(
  list(name = "Nonbreeding",             start = "nonbreeding_start",            end = "nonbreeding_end"),
  list(name = "Prebreeding\nmigration",  start = "prebreeding_migration_start",  end = "prebreeding_migration_end"),
  list(name = "Breeding",                start = "breeding_start",               end = "breeding_end"),
  list(name = "Postbreeding\nmigration", start = "postbreeding_migration_start", end = "postbreeding_migration_end")
)
seasons <- lapply(season_bounds, function(s) {
  ts_start <- lookup_timestep(species_info(bf, s$start), bf)
  ts_end   <- lookup_timestep(species_info(bf, s$end),   bf)
  span     <- (ts_end - ts_start) %% n_ts
  mid_t    <- ts_start + span / 2          # may exceed n_ts for wrap-around seasons
  list(name = s$name, mid_t = mid_t)
})
r_slabel <- 0.50
slabel_df <- do.call(rbind, lapply(seasons, function(s) {
  ang <- ts_angle(s$mid_t)
  data.frame(
    x     = cos(ang) * r_slabel,
    y     = sin(ang) * r_slabel,
    label = s$name,
    hpy   = hpy(s$mid_t)
  )
}))

# ── Centre key position (change these two to move the whole key) ─────────────
key_x <- -0.25
key_y <-  0.00

# ── Direction arrow ──────────────────────────────────────────────────────────
arr_th <- seq(pi / 2 + 0.50, pi / 2 - 0.12, length.out = 80)
arr_df <- data.frame(x = cos(arr_th) * r_dots, y = sin(arr_th) * r_dots)

# ── Plot ─────────────────────────────────────────────────────────────────────
p <- ggplot() +

  # Gradient ring (one sector per timestep)
  geom_polygon(
    data = ring_df,
    aes(x = x, y = y, group = group, fill = hpy),
    colour = NA
  ) +
  scale_fill_gradientn(
    colors   = pal_colors,
    limits   = c(0, 1),
    oob      = scales::squish,
    rescaler = scales::rescale_none,
    guide    = "none"
  ) +

  # Transition lines through the ring
  geom_segment(
    data = trans_df,
    aes(x = x, y = y, xend = xend, yend = yend),
    colour = "gray55", linewidth = 0.4
  ) +

  # Year boundary line (thicker, extends beyond ring on both sides)
  geom_segment(
    data = yr_line_df,
    aes(x = x, y = y, xend = xend, yend = yend),
    colour = "gray35", linewidth = .75
  ) +

  # Timestep dots (white border so they read against the coloured ring)
  geom_point(data = ts_df, aes(x = x, y = y),
             size = 2.8, colour = "white") +
  geom_point(data = ts_df, aes(x = x, y = y),
             size = 1.8, colour = "gray15") +

  # Clockwise direction arrow
  geom_path(
    data = arr_df, aes(x = x, y = y),
    arrow    = arrow(length = unit(0.09, "in"), type = "closed", ends = "last"),
    colour   = "gray20", linewidth = 0.8
  ) +

  # Week number labels
  geom_text(data = week_df, aes(x = x, y = y, label = label),
            size = 2, colour = "gray15") +

  # Month labels
  geom_text(data = month_df, aes(x = x, y = y, label = label),
            size = 2.7, colour = "gray15") +

  # Season names inside the ring hole, colored to match the gradient
  geom_text(
    data = slabel_df,
    aes(x = x, y = y, label = label, colour = hpy),
    size = 2.5, fontface = "bold", hjust = 0.5, vjust = 0.5,
    show.legend = FALSE
  ) +
  scale_colour_gradientn(
    colors   = pal_colors,
    limits   = c(0, 1),
    oob      = scales::squish,
    rescaler = scales::rescale_none,
    guide    = "none"
  ) +

  # Centre key
  annotate("point",   x = key_x,        y = key_y,        size = 2.2, colour = "gray15") +
  annotate("text",    x = key_x + 0.14, y = key_y,        hjust = 0,  size = 2.5, colour = "gray25",
           label = "timestep") +
  annotate("segment", x = key_x, xend = key_x, y = key_y - 0.12, yend = key_y - 0.27,
           colour = "gray55", linewidth = 0.4) +
  annotate("text",    x = key_x + 0.14, y = key_y - 0.20, hjust = 0, size = 2.5, colour = "gray55",
           label = "transition") +

  coord_equal(xlim = c(-1.75, 1.75), ylim = c(-1.75, 1.75)) +
  theme_void() +
  theme(plot.margin = margin(4, 4, 4, 4))

ggsave(
  file.path("vignettes", "TimeStructure.png"),
  plot = p, width = 5.5, height = 5.5, dpi = 150, bg = "white"
)
message("Saved vignettes/TimeStructure.png")
