correlation_cluster <- function(df, threshold = 0.7) {
  cor_matrix <- cor(df[, as.logical(lapply(df, is.numeric))], method = "pearson")
  candidates <- colnames(df[,as.logical(lapply(df, is.numeric))])
  ret <- list()

  while (length(candidates) > 0) {
    curr_group <- c()

    for (i in 1:length(candidates)) {
      if (length(curr_group) == 0) {
        curr_group <- c(curr_group, candidates[i])
      } else {
        accept <- TRUE

        for (j in 1:length(curr_group)) {
          if (cor(x = df[[curr_group[j]]], y = df[[candidates[i]]], use = "complete.obs") < threshold) {
            accept <- FALSE
            break
          }
        }

        if (accept) {
          curr_group <- c(curr_group, candidates[i])
        }
      }
    }

    if (length(curr_group) > 1) {
      ret[[curr_group[1]]] <- curr_group
    }

    candidates <- candidates[-which(candidates %in% curr_group)]
  }

  ret
}
