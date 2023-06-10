#------------------------------------------------------------------------------#
#  This file creates the BirdFlowR logo
#
#  It is part of the git repository but has been added to .rbuildignore
#  along with the higher resolution log files so it is not a part of the
#  R Package itself.
#
#  It depends on BirdFlowR code and the example American Woodcock model
#  currently in BirdFlowExamples - that will likely be replaced soon. If that
#  model changes, or the plot_routes() functions changes the resulting logo
#  will change too.
#
#  Files produced:
#   1.  "man/figures/BirdFlowR.png"  higher resolution png logo
#   2.  "man/figures/BirdFlowR_transparent.png" (optional output) same as 1
#        but with corners transparent.
#   3.   "man/figures/logo.png" lower resolution log it is created from 1. with
#        usethis::use_logo()
#
#------------------------------------------------------------------------------#



library(hexSticker)
library(shadowtext)
library(BirdFlowModels)
library(BirdFlowR)
library(svglite)  # unstated dependency of hexSticker?
library(ggplot2)
library(magick)
library(usethis)

png_file <- "man/figures/BirdFlowR.png"  # white background


transparent_png_file <- "man/figures/BirdFlow_transparent.png"  # transparent background
make_transparent_copy <- FALSE

# Make some routes - set a seed to keep the figure constant
bf <- BirdFlowModels::amewoo
set.seed(1)
rts <- route_migration(bf, n = 10)$points

# I picked the text color from the year pallete I used
# in plot_routes
pal <- ggthemes::tableau_color_pal("Classic Cyclic")
year_cols <- pal(13)
text_color <- year_cols[12]

# And then decided to make it a little darker
text_color <- col2rgb(text_color)
text_color <- round(text_color * .8)
text_color <- do.call(rgb, args = as.list(text_color[ , 1]/255))

# This is the end result but set directly
text_color <- "#6E4D8D"

# ggplot2 plot object with route plots
p <- plot_routes(rts, bf)

# Make sticker
# Note I don't want this to write a file yet but it does
# it gets overwritten later though
s <- sticker(p,
             p_color =  text_color,
             p_size = 25,
             p_y = 1.3,
             spotlight = TRUE,
             package = "BirdFlowR",
             s_width = 5,
             s_height = 5,
             white_around_sticker = TRUE,
             p_fontface = "plain",
             filename = png_file,
             s_x = 1.2,
             s_y = 0.8
)


# Add package name as shadowtext
# I tried to get the shadow to have an alpha but couldn't
# Instead I made the shadow the same color as the active cell color
# Used in plot_routes()
s2 <- s + geom_shadowtext(data = data.frame(x = 1, y = 1.25, label
                                            = "BirdFlowR"),
                          mapping = aes(x = x, y = y, label = label),
                          family = "Aller_Rg", fontface = "bold",
                          color = text_color, size = 25,
                          bg.color = rgb(.875, .875, .875), bg.r = .06)


# Drop original "BirdFlowR" text layer created by sticker()
s2$layers <- s2$layers[-5]
hexSticker::save_sticker( png_file, s2)

# Birdflow font is WAY too big when I save as svg so skipping svg for now
# hexSticker::save_sticker( "man/figures/birdflow.svg", s2)


# Convert white to transparent
# See: https://github.com/GuangchuangYu/hexSticker/issues/39#issuecomment-889786759


# This sets the corners to transparent based on a color matched fill.
# It works if I set the fuzz high otherwise there are weird slivers.
# The transparent mask that defines the corners, however, seems to be pixel
# based even though the rest of the png is vector. If you zoom in it doesn't
# look as good as the white background version.
if(make_transparent_copy){

  fuzz <- 50 # original = 4
  p <- image_read(png_file)
  pp <- p %>%
    image_fill(color = "transparent", refcolor = "white", fuzz = fuzz , point = "+1+1") %>%
    image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = "+517+1") %>%
    image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = "+1+599") %>%
    image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = "+517+599")
  image_write(image = pp, path = transparent_png_file)
  file.show(transparent_png_file)
}

# Resize and copy to logo.png
logo_file <- "man/figures/logo.png"  # standard location used by use_logo()
if(file.exists(logo_file)) file.remove(logo_file)
usethis::use_logo(png_file)


