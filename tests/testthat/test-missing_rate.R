test_that("multiplication works", {
  test_df_1 <- data.frame(x = c(1, NA, 3),
                          y = as.factor(c("M", "M", "F")),
                          z = c(NA, NA, 4)
  )

  test_df_1_result <- data.frame(c(1, 2),
                                 c(1/3, 2/3)
  )
  colnames(test_df_1_result) <- c("Row Counts", "Percentage")
  rownames(test_df_1_result) <- c("x", "z")

  expect_equal(missing_rate(test_df_1), test_df_1_result)
})
