# Internal function to send a message from within BirdFlow functions

It checks to see if BirdFlowR is in verbose mode
(`birdflow_options("verbose"`) and if so pastes it's arguments together
and prints the result with [`cat()`](https://rdrr.io/r/base/cat.html).
In the future it might be updated to use
[`message()`](https://rdrr.io/r/base/message.html)

## Usage

``` r
bf_msg(..., sep = "")
```

## Arguments

- ...:

  Text that will be pasted together to make a message.

- sep:

  (optional) separator between text elements in `...` defaults to no
  separation.

## Value

Nothing is returned if verbose is TRUE the message is printed.

## See also

[`birdflow_options()`](https://birdflow-science.github.io/BirdFlowR/reference/birdflow_options.md)
for changing verbosity.
