# compare names of two nested lists

Look for difference in the list item names at any nested level. Used
internally to verify BirdFlow object structure

## Usage

``` r
compare_list_item_names(x, y, map = "x", differences)
```

## Arguments

- x:

  list

- y:

  reference list

- map:

  used during recursion to tracking where in x differences are found

- differences:

  used during recursion to collecting differences

## Value

data frame with where and difference columns will have 0 rows if no
differences found.
