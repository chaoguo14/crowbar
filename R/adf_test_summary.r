adf_test_summary <- function(df, type = "none", p_select = "BIC") {
  #' Run Augmented Dickey--Fuller test for unit roots on all columns of a data frame.
  #' @param type "none", "drift" (intercept), "trend" (intercept + linear time trend)
  #' @param p_select How to determin number of lags to include in the ADF test regression? c("AIC", "BIC")
  type_explanation <- c("no intercept, no time trend",
                        "with intercept, no time trend",
                        "with intercept, with linear time trend")
  urca_to_adftest <- c("nc", "ct", "c")
  names(urca_to_adftest) <- c("none","drift","trend")
  names(type_explanation) <- c("none", "drift", "trend")
  
  cat(paste("Augmented Unit-Root Test.\n"))
  cat(paste("Type: ", type_explanation[type], "\n"))
  cat("p-value< 0.05 implies no unit root exists (i.e. stationary)\n")
  
  data.frame(
    lapply(df, FUN = function(x) fUnitRoots::adfTest(x,
                            lags = urca::ur.df(x, type = type, selectlags = p_select)@lags,
                            type = urca_to_adftest[type])@test$p.value
    )
  )
}
