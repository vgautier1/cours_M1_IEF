# Significativité des corrélations (pvalue)
calculate_pvalues <- function(cor_matrix) {
  pvalues = matrix(NA, nrow = ncol(cor_matrix), ncol = ncol(cor_matrix))
  for (i in seq_along(cor_matrix)) {
    for (j in seq_along(cor_matrix)) {
      if (i != j) {
        test_result   = cor.test(dataset[[i]], dataset[[j]])
        pvalues[i, j] = test_result$p.value
      }
    }
  }
  return(pvalues)
}

pvalues = round(calculate_pvalues(dataset),2)