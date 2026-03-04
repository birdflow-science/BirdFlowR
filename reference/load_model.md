# Load BirdFlow models from a collection

Load a named model from the current (likely the default) model
collection. If the model doesn't exist in the local cache or isn't
up-to-date the the cache will be updated prior to loading.

## Usage

``` r
load_model(
  model,
  update = TRUE,
  collection_url = birdflow_options("collection_url"),
  timeout = 600
)
```

## Arguments

- model:

  The model name to load.

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

- timeout:

  The number of seconds to allow for downloading. The default is 600
  (ten minutes).

## Value

The designated BirdFlow model is returned.

## See also

[`load_collection_index()`](https://birdflow-science.github.io/BirdFlowR/reference/load_collection_index.md)

## Examples

``` r
if (FALSE) { # \dontrun{

 index <- load_collection_index()
 bf <- load_model(index$model[1])

} # }
```
