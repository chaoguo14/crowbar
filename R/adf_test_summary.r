adf_test_summary <- function(df, max_lag = 5, type = "nc") {
  #' Run Augmented Dickey--Fuller test for unit roots on all columns of a data frame.
  type_explanation <- c("no intercept, no time trend",
                        "with intercept, no time trend",
                        "with intercept, with time trend")
  names(type_explanation) <- c("nc", "ct", "c")
  cat(paste("Augmented Unit-Root Test. Maximum Lag:", max_lag, "\n"))
  cat(paste("Type: ", type_explanation[type], "\n"))
  cat("p-value< 0.05 implies no unit root exists (i.e. stationary)\n")
  data.frame(
    lapply(df,
           FUN = function(x)
             purrr::map_dbl(1:5,
                            function(l) fUnitRoots::adfTest(x, lags = l, type = type)@test$p.value)
    )
  )
}
