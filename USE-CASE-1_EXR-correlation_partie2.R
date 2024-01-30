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

edge_colors = ifelse(cor_matrix > 0, "green", "red")
edge_colors[pvalues > 0.05 ] = "white"  # Liens de corrélations blanc pour les corrélations NS (seuil 5%)

qgraph(cor_matrix,
       labels = colnames(cor_matrix), # Label dans les bulles
       layout = "spring",             # Sortie sous forme de réseau
       edge.color = edge_colors       # Intégre la condition sur les liens NS
)