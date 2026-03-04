# Export and import BirdFlow models in HDF5 format

`export_birdflow()` saves a BirdFlow object to an HDF5 (Hierarchical
Data Format version 5)

`import_birdflow()` imports a BirdFlow model from an HDF5 (Hierarchical
Data Format version 5) file.

## Usage

``` r
export_birdflow(bf, file = NULL, format = "hdf5", overwrite = TRUE)

import_birdflow(hdf5, ..., version)
```

## Arguments

- bf:

  A BirdFlow model

- file:

  The file name to write. If not supplied or if the path is to a
  directory the file name will default to
  `"<species code>_<ebirdst year>_<resolution in (km)>`.

- format:

  The format to export either `"hdf5"` or `"rds"`.

- overwrite:

  The default `TRUE` will delete a preexisting file and then write a new
  one in it's place. if `FALSE`, `export_birdflow()` will throw an error
  if `file` already exists.

- hdf5:

  Path to an HDF5 file containing a fitted BirdFlow model.

- ...:

  Deprecated, arguments to be passed to a version specific internal
  functions.

- version:

  (optional) force reading of BirdFlow models as a particular version.
  Normally, this will be determined from metadata in the HDF5 file.

## Value

`export_birdflow()` writes a file and invisibly returns `TRUE` if
successful.

`import_birdflow()` returns a BirdFlow object.

## Details

The standard workflow for generating a fitted BirdFlow model is:

1.  Use
    [`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)
    to download and format eBird data into an HDF5 file (this calls
    `export_birdflow()` internally).

2.  Fit the model and add marginals and hyperparameters to the HDF5 file
    with [BirdFlowPy](https://github.com/birdflow-science/BirdFlowPy)

3.  Import the model with `import_birdflow()`.

4.  Save the model to a new file either with:

    - [`saveRDS()`](https://rdrr.io/r/base/readRDS.html) to a .rds file,
      or

    - `export_birdflow()` to a new .hdf5 file.

During the first import after fitting with
[BirdFlowPy](https://github.com/birdflow-science/BirdFlowPy)
`import_birdflow()` does a fair amount of renaming and reformatting of
the data - for example it renames the marginals and adds a marginal
index. If the resulting BirdFlow model is exported again with
`export_birdflow()` these updates are retained, so the internal
structure of the HDF5 file will be different. `import_birdflow()` can
handle either format.

## HDF5 file version

The HDF5 BirdFlow model files have an internal version number that is
incremented on major changes to the HDF5 file structure. The file
version is retained in the imported object.

- Version 1 predates [BirdFlowR
  preprocessing](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)
  and only contains marginals and a few other python objects, importing
  it requires having an associated TIFF file that has the extent and
  distribution data. The version number (1) is kept in `"/version"`
  within the HDF5 file.

- Version 2 is the first version that includes preprocessing in
  BirdFlowR. It does not include dynamic masking. The version number (2)
  is stored in `"/metadata/birdflow_version"`. Some version 2 files
  include hyper parameters in the HDF5 but these are not read into R.

- Version 3 marks the transition to dynamic masking, the HDF5 created
  during preprocessing gained `"/geom/dynamic_mask"` and `"/distances"`
  (great circle distance matrix). As in version 2 the version is stored
  in `"/metadata/birdflow_version"` but unlike previous versions the R
  package version that did the preprocessing is saved in character
  format to `"/metdata/birdflowr_version"`. When version 3 files are
  imported into R with model fits the hyperparameters are saved to
  `$metadata$hyperparameters` (a list).

## See also

- [`sparsify()`](https://birdflow-science.github.io/BirdFlowR/reference/sparsify.md)
  to reduce the object size after importing.

- [`build_transitions()`](https://birdflow-science.github.io/BirdFlowR/reference/build_transitions.md)
  to add transition matrices for quicker processing at the cost of
  larger file sizes.

- [`export_rasters()`](https://birdflow-science.github.io/BirdFlowR/reference/export_rasters.md)
  to save the distributions and mask from a BirdFlow model to raster
  files.
