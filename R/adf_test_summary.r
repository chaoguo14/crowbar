adf_test_summary <- function(df, type = "none", p_select = "BIC") {
  #' Run Augmented Dickey--Fuller test for unit roots on all columns of a data frame.
  #'
  #' @param type Type of test equation. Possible values: "none", "drift" (intercept), "trend" (intercept + linear time trend)
  #' @param p_select Method for selecting number of lags to include in the test regression. Possible values: "AIC", "BIC"
  #'
  #' @return A data frame with p-values for each column
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
  #' adf_test_summary(df)  # p-value > 0.05, but we are not using the correct test
  #' adf_test_summary(df, type = "trend")  # This returns p-value < 0.05
  type_explanation <- c("no intercept, no time trend",
                        "with intercept, no time trend",
                        "with intercept, with linear time trend")
  names(type_explanation) <- c("none", "drift", "trend")

  urca_to_adftest <- c("nc", "c", "ct")
  names(urca_to_adftest) <- c("none","drift","trend")
  
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
