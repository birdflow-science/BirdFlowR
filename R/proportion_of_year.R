proportion_of_year <- function(x) {
  (as.POSIXlt(x)$yday + 0.5) / 366  # proportion of year
  # 366 was used in ebirdst.  max(yday+0.5) = 365.5  (on leap yr)
}
