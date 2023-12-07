
# This private function is here to avoid warnings and notes for packages
# that I want to import but don't directly call.
# This approach was suggested by:
# https://r-pkgs.org/dependencies-in-practice.html#how-to-not-use-a-package-in-imports # nolint
# Closes: https://github.com/birdflow-science/BirdFlowR/issues/102
ignore_unused_imports <- function() {

  gifski::gifski
  # Without gifski gganimate will sometimes use a default file based
  # renderer to create animations. This results in unexpected files
  # written to the users home directory.
  # I don't call gifski directly but by having the package available
  # it becomes the default renderer used by gganimate::animate

  rnaturalearthdata::coastline50
  # rnaturalearthdata
  # rnaturalearth functions are called by get_coastline() and others.
  # get_coastline() is called by plot_routes() a core function of BirdFlowR
  # However, rnaturalearth doesn't import rnaturalearthdata instead it
  # checks for that package when needed and then attempts to install it.
  # This installation mechanism doesn't consistently work, and thus causes
  # problems especially in automated environments where the required
  # packes are installed by code.
  #
  # TLDS Although in theory BirdFlowR only directly depends on
  # rnaturalearth it imports rnaturalearthdata to streamline
  # installation and avoid relying on rnaturalearth::install_rnaturalearthdata.
}
