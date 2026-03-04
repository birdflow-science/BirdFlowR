# sparsify BirdFlow models

Set low values to zero in a BirdFlow model marginals to reduce object
size.

## Usage

``` r
sparsify(x, method, p = 0.99, fix = TRUE, p_protected = 0.1)
```

## Arguments

- x:

  A BirdFlow model.

- method:

  One of “"conditional"`, `"marginal"\`, or \`"model"\`. See "Methods"
  section below for details.

- p:

  Required to control the proportion of the probability density retained
  in the sparsification process. See "Methods" below.

- fix:

  If TRUE call
  [`fix_dead_ends()`](https://birdflow-science.github.io/BirdFlowR/reference/fix_dead_ends.md)
  to eliminate dead ends in the sparse model, but only honored if the
  method produces dead ends.

- p_protected:

  Only used with `"conditional` method. The proportion of **cells** in
  each row and column that are protected from being zeroed out. Any
  value of `p_protected` above 0 protects all the non-dynamically-masked
  (NDM) locations from being dropped from the model. The default value
  of `0.10` means that from any NDM location there will always be
  transitions retained to at least 10% of the next timestep's NDM
  locations.

## Value

A BirdFlow object with some values in the marginals set to zero. The
metadata will also be updated with sparsification statistics. The
marginals will be standardized so that they sum to 1.

## Details

The BirdFlow model fitting algorithm cannot predict a complete zero,
however many of the marginal values are very close to zero and have
little impact on the model predictions. `sparsify()` forces small values
to zero with the goal of saving memory, reducing file size, and
decreasing run time. Marginals are stored as sparse matrices
([Matrix::Matrix(x , sparse =
TRUE)](https://rdrr.io/pkg/Matrix/man/Matrix.html) ) so only non-zero
values consume memory.

## Methods

There are three sparsification methods that are all based on proportion.
They use `p` to control the amount of sparsification; where `p` is the
target proportion of the density to retain after eliminating all values
below a (calculated) threshold.

The thresholds are calculated and applied either to the whole model
(`model`) or repeatedly to its components (`conditional`, `marginal`).

- `model`:

  In model sparsification the values from all marginals are pooled and
  then a threshold is chosen for that entire model such that zeroing
  values below the threshold results in the target proportion, `p`, of
  the model's density remaining.

- `marginal`:

  A threshold is chosen and applied separately to each marginal in the
  model. Ultimately, `p` is achieved for the model as a whole but the
  threshold below which cells are set to zero varies across marginals.

- `conditional`:

  This method targets (`1 - p`) of both the forward and backward
  **conditional probabilities** to be zeroed out but also guarantees
  that at least `p_protected` proportion of the **cells** in each row
  and column will not be zeroed out.

  In this method thresholds are chosen independently for each row and
  each column of a marginal prior to any zeroing and then the cells that
  fall below either their row or column thresholds are set to zero as
  long as they aren't within the `p_protected` proportion of cells
  (highest value cells) that are protected from zeroing based on either
  their row or column. `p_protected` thus prevents the number of
  transitions in the sparse model from any state falling below the given
  proportion of the transitions in the full model. The default value
  means that for every location at every timestep at least 10% of the
  transitions to locations in the next timestep are retained.

  This method does not hit its target `p` density retained. In theory
  twice as much density as `p` implies could could be cut from the model
  if the cells targeted based on row and column do not overlap, or much
  more than `p` could be retained with high values of `p_protected`

## Examples

``` r
if (FALSE) { # \dontrun{
# Full models are huge so we don't distribute them.
# Assuming you have an hdf5 file with a full model you could run:
bf <- import_birdflow(hdf5_path)
sbf <- sparsify(bf, method = "marginal+state", p = 0.99)
} # }
```
