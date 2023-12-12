FROM --platform=linux/amd64 rocker/geospatial:4.3.2
RUN install2.r --error --skipinstalled --ncpus -1 \
    rnaturalearth \
    rnaturalearthdata \
    ebirdst \
    && rm -rf /tmp/downloaded_packages \
    && strip /usr/local/lib/R/site-library/*/libs/*.so \
    && R -q -e 'remotes::install_github("birdflow-science/BirdFlowModels")' \
    && R -q -e 'remotes::install_github("birdflow-science/BirdFlowR", build_vignettes = TRUE, upgrade = "never")'
