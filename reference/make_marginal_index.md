# Internal function to make a marginal index for a BirdFlow object.

`make_marginal_index()` is called by `[import_birdflow()]` and
[`truncate_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/truncate_birdflow.md)
to add a marginal index to a BirdFlow object.

## Usage

``` r
make_marginal_index(bf)
```

## Arguments

- bf:

  A BirdFlow object

## Value

A data.frame that facilitates looking up marginals from transition
names. It has columns:

- from:

  starting timestep for transition (direction matters)

- to:

  ending timestep for transition

- direction:

  transition direction, either "forward", or "backward"

- transition:

  transition name e.g. "T_02-02"

- marginal:

  marginal name e.g. "M_01-02", order matches forward transition order,
  so smaller number is generally first except with the last marginal in
  a circular model e.g. "M_52-01"

## Details

[`n_transitions()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md),
[`n_timesteps()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md),
[`is_cyclical()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md),
and
[`get_timestep_padding()`](https://birdflow-science.github.io/BirdFlowR/reference/get_timestep_padding.md)
need to work on the object, which means that `metadata$n_timesteps`,
`metatdata$n_transitions`, and `metadata$timestep_padding` should all be
properly set. The marginals don't have to exist yet.
