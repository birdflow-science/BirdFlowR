# find and fix inconsistencies in sparse BirdFlow models

With sparsification
([`sparsify()`](https://birdflow-science.github.io/BirdFlowR/reference/sparsify.md))
it's possible to create models that have dead ends - states that can be
entered but not exited. This occurs when one marginal encodes
transitions into a state for which the next marginal has no transitions
out. These functions find and fix those states.

## Usage

``` r
find_dead_ends(x)

fix_dead_ends(bf, max_attempts = 100)
```

## Arguments

- x:

  `BirdFlow` model

- bf:

  A BirdFlow model.

- max_attempts:

  The maximum number of iterations to try before giving up.

## Value

\`find_dead_ends() returns a data.frame with columns:

|                                                                                                                                                                |                                                      |
|----------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------|
| `timestep`                                                                                                                                                     | the timestep associated with the dead end            |
|                                                                                                                                                                |                                                      |
|                                                                                                                                                                | `direction`                                          |
| either `"forward"` or `"backward"` indicating which direction the dead end is encountered in                                                                   |                                                      |
|                                                                                                                                                                |                                                      |
| `i`                                                                                                                                                            | the index of the model state that has a dead end     |
|                                                                                                                                                                |                                                      |
|                                                                                                                                                                | `mar`                                                |
| the marginal which leads into the dead end (this marginal has non-zero value in the i'th column if direction is forward and i'th row if direction is backward) |                                                      |
|                                                                                                                                                                |                                                      |
| `x`, and `y`                                                                                                                                                   | the x and y coordinates corresponding with state `i` |

There will be a row for each dead end state, if no dead ends are found
an empty (zero row) data.frame is returned.

`fix_dead_ends()` returns a BirdFlow model with additional marginal row
and columns zeroed out. If successful it will have no dead ends. It also
adds a data.frame `fix_stats` to `bf$metadata$sparse`.

## Details

Consider two adjacent marginals; the rows of the second and the columns
of the first both correspond with the species distribution for the
timestep between them. For every location in the model at that timestep
there are four possibilities (1) the first marginal's column has
non-zero values and the second marginal's row is all zero: there is a
forward transition into that state but no forward transition out and
it's a forward dead end; (2) the situation is reversed and the first
marginals column is all zeros and the second marginal's corresponding
row has non-zero values, a backward dead end (encountered when
projecting backwards in time); (3) if they both have only zeros: the
model is fine but that state is dropped; and (4) if they both have
non-zero values than the corresponding state is valid and can be reached
and exited when projecting forward or backwards.

Dead ends result in lost density with
[predict()](https://birdflow-science.github.io/BirdFlowR/reference/predict.BirdFlow.md)
and errors when they are entered with
[`route()`](https://birdflow-science.github.io/BirdFlowR/reference/route.md).
Based on initial testing the transitions into the dead end are often low
probability so routing may work most of the time but occasionally fail.
The error will occur with the subsequent iteration when attempting to
sample from a bunch of zero probability states.

## See also

[`sparsify()`](https://birdflow-science.github.io/BirdFlowR/reference/sparsify.md)
calls `fix_dead_ends()`, which in turn calls `find_dead_ends()` and
[`fix_current_dead_ends()`](https://birdflow-science.github.io/BirdFlowR/reference/fix_current_dead_ends.md).
