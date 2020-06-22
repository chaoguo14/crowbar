test_that("basic test 1", {
  test_df_1 <- data.frame(w = c(1, 2, 3, -1),
                          y = as.factor(c("High", "Low", NA, "High")),
                          z = c(0.2, 0.5, NA, -0.9)
  )

  test_df_1_result <- matrix(c(1, 0, 1, 0.2,
                               0, 1, 2, 0.5,
                               0, 0, 3, NA,
                               1, 0, -1, -0.9),
                             byrow = TRUE,
                             nrow = 4)
  colnames(test_df_1_result) <- c("y_High", "y_Low", "w", "z")
  test_df_1_result <- as(test_df_1_result, "sparseMatrix")

  expect_identical(to_sparse_matrix(test_df_1), test_df_1_result)
})
