to_sparse_matrix <- function(df) {
  #' Convert a data frame to a dgCMatrix object
  #'
  #' @description
  #' This function converts a data frame to a dgCMatrix object. This is a sparse matrix object and can be
  #' feeded to, for example, xgboost().
  #'
  #' The input data frame should only contain numeric and/or factor. The returned dgCMatrix will have column names.
  #'
  #' If a column is numeric, then the function keeps its original column name. If a column is a factor, it will
  #' be expanded into multiple columns using one-hot encoding. The new column names will look like factor_level.
  #' For example, if a column named "gender" contains "M" and "F", then this column will become two columns, with
  #' names "gender_M" and "gender_F".

  df_numeric_part <- as.matrix(df[, !sapply(df, is.factor), drop = FALSE])
  df_numeric_part <- as(df_numeric_part, "sparseMatrix")

  df_factor_part <- df[, sapply(df, is.factor), drop = FALSE]
  factor_names <- colnames(df_factor_part)
  ind_matrices <- lapply(df[, sapply(df, is.factor), drop = FALSE], function(col) Matrix::t(as(col, "sparseMatrix")))
  df_factor_part <- Reduce(cbind, ind_matrices)
  colnames(df_factor_part) <- paste(rep(factor_names, lapply(ind_matrices, ncol)),
                                    colnames(df_factor_part),
                                    sep = "_")

  cbind(df_factor_part, df_numeric_part)
}
