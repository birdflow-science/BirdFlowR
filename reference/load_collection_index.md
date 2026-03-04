# Load the index to a remote model collection

Load the collection index for the current model collection. This
function will return an up-to-date index for the current BirdFlow
collection. It caches the index locally and will update the cached
version if it is not up-to-date.

## Usage

``` r
load_collection_index(
  update = TRUE,
  collection_url = birdflow_options("collection_url")
)
```

## Arguments

- update:

  If `TRUE` (the default) then both the index and cached model file are
  checked against the server's version to make sure they are up-to-date
  and downloaded again if they are not. If `FALSE` then neither the
  index nor the model will be checked. Set to `FALSE` after downloading
  the model(s) you need if you want to make sure the model does not
  change during your analysis (even if updated on the server); or if
  working offline.

- collection_url:

  The url of a collection. Should be the path to the base directory (not
  an index.html file).

## Value

A data frame with a row for every model in the collection.

## Details

The collection will default to the main BirdFlow model collection and
most users will not need to set it.

The local cache directory (for all collections) defaults to
[birdflow_options("cache")](https://birdflow-science.github.io/BirdFlowR/reference/birdflow_otions())
the cache directory for the current collection will be in a
subdirectory. Both of the above options can be changed for the duration
of the session with
[`birdflow_options()`](https://birdflow-science.github.io/BirdFlowR/reference/birdflow_options.md),
but the defaults should be suitable for most users.
