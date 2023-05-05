
# Helper function for plotting vector fields
range_rescale <- function(x, min = 0, max = 1){
  x / max(x)  * (max - min) + min
}
