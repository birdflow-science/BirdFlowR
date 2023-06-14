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
#   1.  "man/figures/<version>.png"  higher resolution png logo
#   2.  "man/figures/<version>_transparent.png" (optional output) same as 1
#        but with corners transparent.
#   3.   "man/figures/<version>_logo.png" lower resolution log it is created from 1. with
#        usethis::use_logo()
#   4.  "man/figures/BirdFlow_logo_routes.png
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
library(ggthemes)

png_file <- "man/figures/BirdFlowR.png"  # white background
transparent_png_file <- "man/figures/BirdFlow_transparent.png"  # transparent background
make_transparent_copy <- FALSE
routes_rds <- "man/figures/BirdFlow_logo_routes.Rds"

# Note changes made to route() shortly after the logo was first made
# resulted in different stochastic routes despite the same seed.
# I saved the route file for conistency.

# Make some routes - set a seed to keep the figure constant
if(file.exists(routes_rds)){
  cat('reading saved rts files\n')
  rts <- readRDS(routes_rds)
} else {
  bf <- BirdFlowModels::amewoo
  set.seed(1)
  rts <- route_migration(bf, n = 10)$points
  saveRDS(rts, routes_rds)
}

# I picked the text color from the year palette I used
# in plot_routes
pal <- ggthemes::tableau_color_pal("Classic Cyclic")
year_cols <- pal(13)
text_color <- year_cols[12]


rgb2col <- function(x) do.call(rgb, args = as.list(x[ , 1]/255))
col_adjust <- function(x, factor) round(x * factor)

# And then decided to make it a little darker
text_color <- col2rgb(year_cols[12]) |> col_adjust(0.65) |> rgb2col()
"#6E4D8D"  # 0.8  # First merged logo
"#61437B"  # 0.7  #
"#533A6A"  # 0.6  # Chosen
"#453058"  # 0.5


border_color <-  year_cols[3] |> col2rgb() |> col_adjust(0.7) |> rgb2col()
"#87B13F"  # First merged logo (hexSticker default h_color)
"#238026"  # 0.8
"#1A601D"  # 0.6   # V2
"#165018"  # 0.5
"#18581A"  # 0.55


# Version 1
text_color <- "#6E4D8D"
border_color <- "#87B13F" # default
h_size = 1.2  # default

# Version 2
text_color <-"#533A6A"  # 0.6
border_color <- "#1A601D"  #0.6
h_size = 2

# Version 3
text_color <-"#61437B"  # 0.7
border_color <- "#87B13F"   # # default
h_size = 1.5

# Version 4
text_color <-"#61437B"  # 0.7
border_color <- "#238026"   # # default
h_size = 1.5

##  Slight revision of original logo
text_color <-"#61437B"  # 0.7
border_color <- "#87B13F"   # # default
h_size = 1.5
primary_text <- "BirdFlowR"
p_size <- 25
p_x = 1
p_y = 1.3
text2 <- ""
size2 <- 40
x2 <- 1.5
y2 <- .8

font_family = "Aller_Rg"  # "Aller_Rg" is default for hexSticker
font_face = "bold"

versions <- c("BirdFlow", "BirdFlowR", "BirdFlowPy")

for(version in versions){

  switch(version,
         "BirdFlowR" = {
           # BirdFlow R
           primary_text <- "BirdFlow"
           p_size <- 30
           text2 <- "R"
           size2 <- 55
           x2 <- 1.4
           y2 <- .75},

         "BirdFlowPy" = {
           # BirdFlow Py
           primary_text <- "BirdFlow"
           p_size <- 30
           text2 <- "Py"
           size2 <- 45
           x2 <- 1.4
           y2 <- .78},

         "BirdFlow" = {
           # BirdFlow
           primary_text <- "BirdFlow"
           p_size <- 30
           text2 <- ""
           size2 <- 50
           x2 <- 1.3
           y2 <- .78
         })

  png_file <- paste0("man/figures/", version, ".png")
  transparent_png_file <- paste0("man/figures/", version, "_transparent.png")


  # ggplot2 plot object with route plots
  p <- plot_routes(rts, bf)

  # Make sticker
  # This writes a file that I overwrite below; I couldn't figure out how to skip
  # the write.
  s <- sticker(p,
               p_color =  text_color,
               p_size = p_size,
               p_x = p_x,
               p_y = p_y,
               spotlight = TRUE,
               package = primary_text,
               s_width = 5,
               s_height = 5,
               white_around_sticker = TRUE,
               p_family = font_family,
               p_fontface = font_face,
               filename = png_file,
               s_x = 1.2,
               s_y = 0.8,
               h_color = border_color,
               h_size = h_size
  )


  # Add package name as shadowtext
  # I tried to get the shadow to have an alpha but couldn't
  # Instead I made the shadow the same color as the active cell color
  # Used in plot_routes()
  s2 <- s + geom_shadowtext(data = data.frame(x = p_x, y = p_y,
                                              label = primary_text),
                            mapping = aes(x = x, y = y, label = label),
                            family = font_family,
                            fontface = font_face,
                            color = text_color, size = p_size,
                            bg.color = rgb(.875, .875, .875), bg.r = .04)

  if(text2 != ""){
    s2 <- s2 + geom_shadowtext(data = data.frame(x = x2, y = y2, label
                                                 = text2),
                               mapping = aes(x = x, y = y, label = label),
                               family = font_family,
                               fontface = font_face,
                               color = text_color, size = size2,
                               bg.color = rgb(.875, .875, .875), bg.r = .015)
  }


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

} # End version loop

# Resize and copy to man/figures/logo.png

# Clear old log file to avoid dialog
logo_file <- paste0("man/figures/logo.png")  # standard location used by use_logo()
if(file.exists(logo_file)) file.remove(logo_file)

# Pick version to use as package logo and deploy it
version <- "BirdFlowR"
png_file <- paste0("man/figures/", version, ".png")
usethis::use_logo(png_file)

