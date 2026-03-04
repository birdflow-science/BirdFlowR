# Conditionally suppress messages from expressions in BirdFlowR code

This internal functions is used to suppress messages thrown by functions
called in BirdFlowR code if `birdflow_options("verbose")` is `FALSE`.

## Usage

``` r
bf_suppress_msg(exp)
```

## Arguments

- exp:

  R code that might throw a message (originating outside of BirdFlowR.

## See also

[`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)
uses this when calling ebirdst functions that display messages. When
BirdFlowR functions generate messages they should use
[`bf_msg()`](https://birdflow-science.github.io/BirdFlowR/reference/bf_msg.md)
so that `birdflow_options("verbose")` is honored.

## Examples

``` r
if (FALSE) { # \dontrun{
# bf_suppress_msg isn't exported so can't be run in examples
# in internal code or after  devtools::load_all() example will work
   ob <- birdflow_options("verbose")
   birdflow_options(verbose = FALSE)
   bf_suppress_msg( message("hi" ))
   birdflow_options(verbose = TRUE)
   bf_suppress_msg( message("hi" ))
   birdflow_options(ob)
 } # }
```
