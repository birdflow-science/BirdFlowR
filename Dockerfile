FROM rocker/geospatial:4.6.1
ENV PKG_BUILD_VIGNETTES=true
RUN R -q -e 'pak::pak("birdflow-science/BirdFlowR", ask = FALSE, dependencies = TRUE)' \
    && R -q -e 'pak::cache_clean()' \
    && strip /usr/local/lib/R/site-library/*/libs/*.so
