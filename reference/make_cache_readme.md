# Internal function to create or update readme files within the local BirdFlow model cache.

This is called anytime new files are downloaded to the cache by
[`load_collection_index()`](https://birdflow-science.github.io/BirdFlowR/reference/load_collection_index.md)
or
[`load_model()`](https://birdflow-science.github.io/BirdFlowR/reference/load_model.md).

## Usage

``` r
make_cache_readme(collection_url = birdflow_options("collection_url"))
```

## Value

Nothing is returned
