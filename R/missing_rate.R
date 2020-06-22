missing_rate <- function(df, include_non_missing = FALSE) {
  #' Summarize the missing rate of a data frame
  #'
  #' @description
  #' This function summarizes the missing rate of the given data frame. It simply computes the number of rows having NA (as
  #' well as the percentage) under each column. The result is sorted by percentage of missing values.

  ret <- as.data.frame(t(sapply(df, function(col) c(sum(is.na(col)), sum(is.na(col))/nrow(df)))))
  colnames(ret) <- c("Row Counts", "Percentage")
  ret <- ret[order(ret[["Percentage"]]), ]

  if (!include_non_missing) {
    ret[ret[, 1] > 0, ]
  } else {
    ret
  }
}
