FROM --platform=linux/amd64 rocker/geospatial:4.2.3
RUN R -q -e 'install.packages(c("rnaturalearth", "rnaturalearthdata", "ebirdst"))' \
    && R -q -e 'remotes::install_github("birdflow-science/BirdFlowModels")' \
    && R -q -e 'remotes::install_github("birdflow-science/BirdFlowR", build_vignettes = TRUE, upgrade = "never")'
