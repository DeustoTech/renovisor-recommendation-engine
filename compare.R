
library(dplyr)

# centros de cluster con D6 
centers_df <- read.csv("resultsD_plot/D_6/greedy_cluster_centers.csv", stringsAsFactors = FALSE)
# quitar columna "cluster" para comparar solo determinantes
centers_mat <- as.matrix(centers_df[,-1])


### los clusters resultabntes setan co 1 y 0, y los de /data estan con la palabra --> adaptar estos para poder haecr la comparacion

compare_centers_with_experts <- function(centers_mat, expert_csv) {
  expert_df <- read.csv(expert_csv, header = TRUE, stringsAsFactors = FALSE)
  expert_mat <- as.matrix(expert_df[,-1])  # quitar columna de indices si existe
  expert_mat <- t(expert_mat)  # cada fila = arquetipo experto, cada columna = determinante

  results <- data.frame()
  
  for(i in 1:nrow(centers_mat)) {
    center <- centers_mat[i, ]
    
    # % coincidencia con cada arquetipo experto
    similarity <- apply(expert_mat, 1, function(expert) {
      sum(center == expert) / length(center) * 100
    })
    
    # Mejor coincidencia
    best_match <- names(similarity)[which.max(similarity)]
    best_pct   <- max(similarity)
    
    results <- rbind(results, data.frame(
      cluster = centers_df$cluster[i],
      best_match = best_match,
      pct_match = best_pct
    ))
  }
  
  return(results)
}

res_kmeans <- compare_centers_with_experts(centers_mat, "data/archetypeKmeans.csv")
res_sis    <- compare_centers_with_experts(centers_mat, "data/archetypeSINS.csv")
res_experts <- compare_centers_with_experts(centers_mat, "data/archetypeExperts.csv")

write.csv(res_kmeans, "resultsD_plot/cluster_vs_expertKmeans.csv", row.names = FALSE)
write.csv(res_sis, "resultsD_plot/cluster_vs_expertSINS.csv", row.names = FALSE)
write.csv(res_experts, "resultsD_plot/cluster_vs_expertExperts.csv", row.names = FALSE)


