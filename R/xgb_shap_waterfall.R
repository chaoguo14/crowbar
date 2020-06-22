xgboost_shap_waterfall <- function(xgb_model, X_train, name_map = NULL) {
  # name_map: sometimes column names of X_train are difficult to understand.
  #           for example, they might contain abbreviations. name_map is a named
  #           vector to translate those.
  #           e.g. c("csmt" = "consumer sentiment") where csmt is one of the columns
  #           if not provided, will use column names. if provided without names, will
  #           map according to order

  require(SHAPforxgboost)
  require(waterfalls)

  # X_train might include more variables than xgb_model was trained on
  X_train <- X_train[, xgb_model$feature_names]

  shap_values <- shap.values(xgb_model, X_train)
  y_hat <- predict(xgb_model, xgb.DMatrix(as.matrix(X_train)))

  # Create x-axis label for the plot
  x_axis_labels <- mapply(FUN = function(x, y) paste(x, y, sep = "\n"),
                          name_map[xgb_model$feature_names], X_train[1, ])

  waterfall_df <- data.frame(vals = c(as.numeric(shap_values$BIAS0[1, 1]),
                                      as.numeric(shap_values$shap_score)),
                             label = c("Bias", x_axis_labels))
  waterfall_df$vals <- sapply(waterfall_df$vals, FUN = function(x) round(x, 3))

  waterfall(waterfall_df, calc_total = TRUE) +
    theme_minimal() +
    ggtitle("SHAP Explainer") +
    ylab(expression("Predicted Value "~hat(y))) +
    xlab("Explanatory Variables") +
    theme(plot.title = element_text(size = 16, family = "sans"),
          axis.text.x = element_text(size = 12, family = "sans"),
          axis.title.x.bottom = element_text(size = 14, family = "sans"),
          axis.title.y.left = element_text(size = 14, family = "sans")
    )
}
