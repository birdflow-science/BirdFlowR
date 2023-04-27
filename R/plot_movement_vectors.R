

plot_movement_vectors <- function(vf, bf){

  vf$width <- range_rescale(vf$weight, .15, .7)
  coast <- get_coastline(bf)


  ggplot(vf, aes(x = start_x, y = start_y, col = weight, linewidth = width)) +
    geom_segment(aes(xend = end_x, yend = end_y),
                 arrow = arrow(length=unit(0.1,"cm"),
                               ends="last", type = "open")) +
    scale_color_gradient( low = rgb(0, 0, 0, .25), high= rgb(0, 0, 0, 1)) +
    geom_sf(data = coast, inherit.aes = FALSE,
            mapping =  aes(linewidth = 0.1)) +
    scale_linewidth_identity() +
    coord_sf(expand = FALSE)  +
    guides(col = FALSE) + theme_bw() +
    theme(axis.title = element_blank())


}
