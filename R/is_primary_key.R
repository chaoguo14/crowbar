is_primary_key <- function(df, column_names) {
  nrow(unique(df[, column_names])) == nrow(df)
}
