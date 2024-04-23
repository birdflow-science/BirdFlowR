
# This strips weird extra within column attributes from dataframes returned
# by h5read
# It's used by import_birdflow_v3.
clean_hdf5_dataframe <- function(df) {
  if (!inherits(df, "data.frame"))
    return(df)

  for (i in seq_len(ncol(df)))
    df[[i]] <- as.vector(df[[i]])
 return(df)
}
