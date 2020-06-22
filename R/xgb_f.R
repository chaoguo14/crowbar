xgb_f <- function(formula, data, weights = NULL,
                      params = NULL, nrounds = 50) {
  # weights: characters
  require(fastDummies)
  require(xgboost)

  response_name <- all.vars(formula)[1]

  indpt_var_names <- labels(terms(formula, data = data))
  if (!is.null(weights)) {
    indpt_var_names <- indpt_var_names[indpt_var_names != weight]
  }

  if (is.null(weights)) {
    weights <- rep(1, dim(data)[1])
  }


  # Is there any non-numeric column?
  if (any(sapply(data, class) %in% c("factor", "character"))) {
    data <- dummy_cols(data, remove_selected_columns = TRUE)
  }

  # Prepare xgb.DMatrix for xgb.train
  dtrain <- xgb.DMatrix(data = as.matrix(data[, indpt_var_names]),
                        label = data[[response_name]],
                        weight = weights)

  # Prepare arguments for xgboost::xgb.train
  xgb_args <- list(data = dtrain, nrounds = nrounds)
  if (!is.null(params)) {
    xgb_args[["params"]] <- params
  }
  xgb_md <- do.call(what = "xgb.train", args = xgb_args)

  return(xgb_md)
}
