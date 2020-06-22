xgb_pdp <- function(model_obj, train_df, pred_var, type = "regression",
                    trim = 0.1, precision = 0.05, mode = "Both") {

  require(dplyr)
  require(ggplot2)
  require(pdp)

  # Trim data
  train_df <- train_df[train_df[[pred_var]] >= quantile(train_df[[pred_var]], na.rm = TRUE, probs = trim) &
                         train_df[[pred_var]] <= quantile(train_df[[pred_var]], na.rm = TRUE, probs = (1-trim)), ]

  # PDP based on sample quantile
  pred_grid <- data.frame(quantile(train_df[[pred_var]], na.rm = TRUE, probs = seq(0, 1, precision)))
  colnames(pred_grid) <- c(pred_var)
  u <- pdp::partial(object = model_obj, pred.var = pred_var, pred.grid = pred_grid, train = train_df, type = type)
  u$Method <- "Quantile"

  # PDP based on uniform sampling of X
  v <- pdp::partial(object = model_obj, pred.var = pred_var, train = train_df, type = type, quantiles = FALSE)
  v$Method <- "Uniform"

  uv <- rbind(u, v)

  # Generate PDP plot
  if (mode == "Both") {
    mode_to_display <- c("Quantile", "Uniform")
  } else {
    mode_to_display <- c(mode)
  }

  q <- ggplot(data = uv %>% filter(Method %in% mode_to_display),
              aes_string(x = pred_var, y = "yhat", colour = "Method")) +
    geom_line(size = 0.7) +
    geom_point() +
    geom_rug(data = uv %>% filter(Method == "Quantile"), sides = "b") +
    labs(title = paste("Partial Dependence Plot:", pred_var),
         subtitle = paste0("Trimmed top/bottom", trim*100, "%. ",
                           "Precision ", precision*100, "%.")) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, family = "sans"),
          axis.title.y.left = element_text(size = 12, family = "sans"),
          axis.title.x.bottom = element_text(size = 12, family = "sans"))

  q
}
