
#' compare names of two nested lists
#'
#' Look for difference in the list item names at any nested level.
#' Used internally to verify BirdFlow object structure
#'
#' @param x list
#' @param y reference list
#' @param map used during recursion to tracking where in x differences are found
#' @param differences used during recursion to collecting differences
#'
#' @return data frame with where and difference columns will have 0 rows if no
#' differences found.
#' @keywords internal
compare_list_item_names <- function(x, y, map = "x", differences) {
  if (missing(differences))
    differences <- data.frame(where = character(0), difference = character(0))
  if (!is.list(x) && !is.list(y))
    return(differences)
  if (sum(is.list(x), is.list(y)) == 1) { # only one is a list
    if (is.list(x))
      return(rbind(differences,
                   data.frame(where = map,
                              differences = "should not be a list")))
    if (is.list(y))
      return(rbind(differences,
                   data.frame(where = map,
                              differences = "should be a list")))
  }
  if (!setequal(names(x), names(y))) {
    lost <- setdiff(names(x), names(y))
    gained <- setdiff(names(y), names(x))
    if (length(lost) != 0)
      differences <- rbind(
        differences,
        data.frame(where = map,
                   differences = paste0("extra:",
                                        paste0(lost, collapse = ", "))))
    if (length(gained) != 0)
      differences <- rbind(
        differences,
        data.frame(where = map,
                   differences = paste0("missing:",
                                        paste0(gained, collapse = ", "))))
    return(differences)
  }

  if (!all(names(x) == names(y)))
    return(rbind(differences,
                 data.frame(where = map, differences = "Wrong order")))

  for (n in names(x)) {
    differences <- compare_list_item_names(x[[n]],
                                           y[[n]],
                                           map = paste0(map, "$", n),
                                           differences = differences)
  }
  return(differences)

}  # end compare list item names
