
# Function to return americas - used for setting up "big run" clipping boundary
# here so we can reproduce the clipping issue in that run.
get_americas <- function(clip_to_mainland_us = FALSE, include_hawaii = FALSE){
  earth <- rnaturalearth::ne_countries(scale = 50)
  americas <- earth[grep("America", earth$continent), , drop = FALSE]
  if(clip_to_mainland_us){
    extent <- c(ymax = 50, ymin = 25, xmin = -130, xmax = -55 )
    americas <- sf::st_crop(americas, extent)
    americas <- americas[americas$name == "United States", , drop = FALSE]
  }
  # Drop Hawaii
  if(!include_hawaii){
    clip <- sf::st_bbox(c(ymax = 25, ymin = 15, xmin = -165, xmax = -150 )) |> sf::st_as_sfc()
    sf::st_crs(clip) <- "EPSG:4326"
    americas <- sf::st_difference(americas, clip)
  }
  americas
}
