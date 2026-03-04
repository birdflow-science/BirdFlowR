# Internal function to get the local path to the cache for the current model collection.

Use
[birdflow_options("cache")](https://birdflow-science.github.io/BirdFlowR/reference/birdflow_options())
to get the main cache directory (parent of the collection directory) and
[birdflow_options("collection_url")](https://birdflow-science.github.io/BirdFlowR/reference/birdflow_options())
for the current model collection url.

## Usage

``` r
cache_path(collection_url = birdflow_options("collection_url"))
```

## Arguments

- collection_url:

  The url of a collection. Should be the path to the base directory (not
  an index.html file).

## Value

Path to the local cache for the current collection including a trailing
delimiter.
