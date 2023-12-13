
change_date_format <- function(bf, version_year = NULL) {

  if (is.null(version_year)) {
    # If nothing specified switch to the opposite format from e
    # what is present
    version_year <- ifelse(get_metadata(bf, "ebird_version_year") < 2022,
                          2022, 2021)
  }

  stopifnot(is.numeric(version_year),
            length(version_year) == 1,
            !is.na(version_year))

  date_format <- ifelse(version_year < 2021.5, 2021, 2022)

  weeks <- bf$dates$week
  new_dates <- make_dates(version_year)
  new_dates <- new_dates[match(weeks, new_dates$week), ]
  rownames(new_dates) <- NULL

  if (date_format == 2021) {
    new_dates$interval <- seq_len(nrow(new_dates))
  } else {
    new_dates$timestep <- seq_len(nrow(new_dates))
  }

  bf$dates <- new_dates
  bf$metadata$ebird_version_year <- version_year

  bf
}
