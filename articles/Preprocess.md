# Preprocess and sparsify

### Load R packages and set species.

The “example_data” allows processing the **ebirdst** example data, which
doesn’t require an access code.

If you plan on preprocessing any other species be sure to [setup an
ebirdst access
code](https://cornelllabofornithology.github.io/ebirdst/articles/ebirdst.html#access).

``` r
library(BirdFlowR)
library(ebirdst)
library(terra)
species <- "example_data"  # optionally, set species here
```

You’ll also have to setup a destination folder for the purpose of the
vignette I’m going to use a temporary folder.

``` r
dir <- tempdir()
```

You could optionally set to a directory of your choice that is somewhere
more permanent and accessible. The code below should add a
BirdFlowModels directory to your home directory, to use for model
output. This block is not run when building the vignette.

``` r
dir <- "~/BirdFlowModels"
dir.create(dir, showWarnings = FALSE)
```

### Preprocess

Now we can preprocess the species. The `gb` parameter is one way of
setting the resolution; it specifies the amount of ram available on the
machine where we intend to fit the BirdFlow model - with python and Jax,
and probably on a cluster. Here, we set `gb` to a small value to keep
file size and memory requirements small.

``` r
# Note:
#   amewoo 100km resolution is equivalent to gpu_ram = 1
#   amewoo 75 km resolution is equivalent to gpu_ram = 2.9

bf <- preprocess_species(species, dir, gpu_ram = 1)
```

### Fit models

In python. Come back after you’re done.

### Import

The rest of this vignette is predicated on having an .hdf5 file with a
fitted model. Set the path to the file and import it.

``` r
model_file <- file.path(dir, "fitted.hdf5")
bf <- import_birdflow(model_file)
```

### Sparsify

It’s a good idea to sparsify immediately after importing to reduce
memory usage and processing time for the models.

The state method eliminates all transitions into and out of model states
(locations in space and time) for which the ebirdst distribution is
zero. Those states are thus removed from the model.

You can read more about sparsification methods in the help for
`sparasify()`.

``` r
bf <- sparsify(bf, method = "state")
```

### Save sparse model

Saving the sparse model will save time and disk space if we want to use
it later. Here we write it as a serialized R object which is fast and
efficient but not at all portable to other software.

``` r
sparse_file <- file.path(dir, "sparse.Rds")  # change
saveRDS(bf, file = sparse_file)
```

In a later session we could then read it with.

``` r
bf <- readRDS(sparse_file)
```

See
[`vignette('BirdFlowR')`](https://birdflow-science.github.io/BirdFlowR/articles/BirdFlowR.md)
for examples of routing and forecasting with BirdFlow objects.
