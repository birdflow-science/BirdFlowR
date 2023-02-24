FROM rocker/geospatial:4.2.2
RUN R -q -e 'install.packages(c("rnaturalearth", "rnaturalearthdata"))' \
    && R -q -e 'remotes::install_github("birdflow-science/BirdFlowModels")' \
    && R -q -e 'remotes::install_github("birdflow-science/BirdFlowR", build_vignettes = TRUE, upgrade = "never")'
