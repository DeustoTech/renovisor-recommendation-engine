library(dplyr)

centers_df <- read.csv("resultsD_plot/D_6/greedy_cluster_centers.csv", stringsAsFactors = FALSE)

# determinantes reales usados por el greedy
determinantes <- colnames(centers_df)[-1]

# matriz binaria de centros
centers_mat <- as.matrix(centers_df[ , -1])


expert_csv_to_binary <- function(expert_csv, determinantes) {
  
  expert_df <- read.csv(expert_csv, stringsAsFactors = FALSE)
  
  # quitar columnas de índice si existen
  drop_cols <- c("cluster", "Cluster", "id", "ID", "X")
  expert_df <- expert_df[ , !(names(expert_df) %in% drop_cols), drop = FALSE]
  
  # para cada columna (arquetipo experto), sacar determinantes no vacíos
  expert_bin <- sapply(expert_df, function(col) {
    vals <- col[!is.na(col) & col != ""]
    sapply(determinantes, function(det) as.integer(det %in% vals))
  })
  
  # filas = arquetipos, columnas = determinantes
  expert_bin <- t(expert_bin)
  
  return(expert_bin)
}


compare_centers_with_experts <- function(centers_mat, expert_csv) {
  
  expert_bin <- expert_csv_to_binary(expert_csv, determinantes)
  
  # chequeo de seguridad
  if (ncol(expert_bin) != ncol(centers_mat)) {
    stop(
      paste(
        "Dimensiones incompatibles:",
        "centers =", ncol(centers_mat),
        "expert =", ncol(expert_bin)
      )
    )
  }
  
  results <- data.frame()
  
  for (i in 1:nrow(centers_mat)) {
    
    center <- centers_mat[i, ]
    
    similarity <- apply(expert_bin, 1, function(expert_row) {
      sum(center == expert_row) / length(center) * 100
    })
    
    results <- rbind(
      results,
      data.frame(
        cluster    = centers_df$cluster[i],
        best_match = names(similarity)[which.max(similarity)],
        pct_match  = max(similarity)
      )
    )
  }
  
  return(results)
}


res_kmeans <- compare_centers_with_experts(centers_mat, "data/archetypeKmeans.csv")
res_sins <- compare_centers_with_experts(  centers_mat, "data/archetypeSINS.csv")
res_experts <- compare_centers_with_experts( centers_mat, "data/archetypeExperts.csv")

write.csv( res_kmeans, "resultsD_plot/cluster_vs_expertKmeans_D6.csv",row.names = FALSE)
write.csv( res_sins, "resultsD_plot/cluster_vs_expertSINS_D6.csv", row.names = FALSE)
write.csv( res_experts, "resultsD_plot/cluster_vs_expertExperts_D6.csv", row.names = FALSE)
