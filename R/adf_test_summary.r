adf_test_summary <- function(df, p_select = "BIC") {
  #' Run ADF test for unit roots on all columns of a data frame.
  #'
  #' @param p_select Method for selecting number of lags to include in the test regression. Possible values: "AIC", "BIC"
  #'
  #' @return A data frame with 3 rows and ncol(df) columns.
  #'
  #' @examples
  #' # Example 1: We test it on iid white noises and a random walk.
  #' # As expected, white_noise's p-value is smaller than 0.01, so it is stationary. But random walk is not.
  #' set.seed(10)
  #' df <- data.frame(white_noise = rnorm(100), random_walk = cumsum(rnorm(100)))
  #' adf_test_summary(df)
  #'
  #' # Example 2: We test it on a "trend stationary" time series.
  #' set.seed(10)
  #' df <- data.frame(trend_s = 0.8 * seq(1:20) + rnorm(20))
  #' adf_test_summary(df)
  type_explanation <- c("no intercept, no time trend",
                        "with intercept, no time trend",
                        "with intercept, with linear time trend")
  names(type_explanation) <- c("none", "drift", "trend")

  urca_to_adftest <- c("nc", "c", "ct")
  names(urca_to_adftest) <- c("none","drift","trend")
  
  cat(paste("Augmented Unit-Root Test.\n"))
  # cat(paste("Type: ", type_explanation[type], "\n"))
  cat("  [1]. p-value < 0.05 implies no unit root exists (i.e. stationary).\n")
  cat("  [2]. Use domain knowledge to determine if including intercept/trend is appropriate.\n\n")
  
  data.frame(
    lapply(df, FUN = function(x) c("None" = fUnitRoots::adfTest(x,
                            lags = urca::ur.df(x, type = "none", selectlags = p_select)@lags,
                            type = urca_to_adftest["none"])@test$p.value,
                            "Intercept" = fUnitRoots::adfTest(x,
                            lags = urca::ur.df(x, type = "drift", selectlags = p_select)@lags,
                            type = urca_to_adftest["drift"])@test$p.value,
                            "Trend" = fUnitRoots::adfTest(x,
                            lags = urca::ur.df(x, type = "trend", selectlags = p_select)@lags,
                            type = urca_to_adftest["trend"])@test$p.value)
    )
  )
}
