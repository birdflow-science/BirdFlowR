rasterize_bmtr <- function(bmtr, bf) {

  bmtr$col <- bmtr$x |> x_to_col(bf)
  bmtr$row <- bmtr$y |> y_to_row(bf)

  transitions <- unique(bmtr$transition)

  arr <- array(
    NA_real_,
    dim = c(nrow(bf), ncol(bf), length(transitions))
  )

  for (k in seq_along(transitions)) {
    tr <- transitions[k]
    traffic <- bmtr[bmtr$transition == tr, ]
    arr[cbind(traffic$row, traffic$col, k)] <- traffic$bmtr
  }

  r <- terra::rast(arr, ext = ext(bf))

  names(r) <- transitions
  r
}
