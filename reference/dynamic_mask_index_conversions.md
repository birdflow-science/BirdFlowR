# Convert between dynamic mask index and static location index

`dmi_to_i()` and `i_to_dmi()` are for internal and advanced use; they
are not likely to be helpful to most users. See [index
conversions](https://birdflow-science.github.io/BirdFlowR/reference/i_to_x)
for, likely, more useful conversions. These two functions convert
between indices along the cells that are included in the dynamic mask
(`dmi`) and standard location indices (`i`) along the cells that are
included by the static mask. This conversion requires knowing the
`timestep` associated with each `dmi` or `i` value as the mapping
between the two is different for each timestep.

## Usage

``` r
dmi_to_i(dmi, timestep, bf)

i_to_dmi(i, timestep, bf)
```

## Arguments

- dmi:

  Dynamic mask index values. These will always be integers between `1`
  and the sum of the dynamic mask for the given timestep.

- timestep:

  Either a single timestep to be used for all conversions or a vector of
  the timesteps associated with each input value: `dmi` for `dmi_to_i()`
  or `i` for `i_to_dmi()`.

- bf:

  A BirdFlowR model. Note the conversion is specific to this model and
  not valid for any others.

- i:

  Location index. This indicates a location based on an index of the
  cells included by the static mask. These start in the top left
  location and fill in by row.

## Value

The location index,`i`, of each location indicated by `dmi` or vice
versa.

## Examples

``` r
 bf <- BirdFlowModels::amewoo
 dmi <- c(11:20)
 timesteps <- c(1, 1, 1, 1, 3, 3, 5,7:9 )

 i <- dmi_to_i(dmi, timesteps, bf)
 dmi2 <- i_to_dmi(i, timesteps, bf)

 isTRUE(all.equal(dmi, dmi2))
#> [1] TRUE
```
