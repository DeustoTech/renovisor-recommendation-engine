library(dplyr)

# Centros de cluster con D6
centers_df <- read.csv("resultsD_plot/D_6/greedy_cluster_centers.csv", stringsAsFactors = FALSE)
# quitar columna "cluster" para comparar solo determinantes
centers_mat <- as.matrix(centers_df[,-1])

# Lista completa de determinantes
determinantes <- c(
  "Profits","Credit Score","Risk Profile","Added Value",
  "Frugality","Legal","Trust","Safety","Climate Protection",
  "Cost-Efficiency","Knowledge","Own Competence","Technical Fit",
  "Self-Satisfaction","Wellbeing","Commitment","Adherence",
  "Autarky","Socialising","Fun","Brag","Trends","Authority",
  "Own Significance","Poseur","Peer-Pressure","Cozyness","Rights and Duties"
)

# Función para convertir CSV de expertos a matriz binaria 0/1
expert_csv_to_binary <- function(expert_csv, determinantes) {
  expert_df <- read.csv(expert_csv, header = TRUE, stringsAsFactors = FALSE)
  expert_mat <- as.matrix(expert_df[,-1])  # quitar columna índice
  expert_mat <- t(expert_mat)              # filas = arquetipos, columnas = determinantes
  
  # Convertir a 0/1
  expert_bin <- t(apply(expert_mat, 1, function(row) {
    sapply(determinantes, function(det) as.integer(det %in% row))
  }))
  
  rownames(expert_bin) <- rownames(expert_mat)
  return(expert_bin)
}

# Función de comparación
compare_centers_with_experts <- function(centers_mat, expert_csv) {
  expert_bin <- expert_csv_to_binary(expert_csv, determinantes)
  
  results <- data.frame()
  
  for(i in 1:nrow(centers_mat)) {
    center <- centers_mat[i, ]
    
    # % coincidencia con cada arquetipo experto
    similarity <- apply(expert_bin, 1, function(expert_row) {
      sum(center == expert_row) / length(center) * 100
    })
    
    # Mejor coincidencia
    best_match <- rownames(expert_bin)[which.max(similarity)]
    best_pct   <- max(similarity)
    
    results <- rbind(results, data.frame(
      cluster = centers_df$cluster[i],
      best_match = best_match,
      pct_match = best_pct
    ))
  }
  
  return(results)
}

# Comparaciones
res_kmeans  <- compare_centers_with_experts(centers_mat, "data/archetypeKmeans.csv")
res_sis     <- compare_centers_with_experts(centers_mat, "data/archetypeSINS.csv")
res_experts <- compare_centers_with_experts(centers_mat, "data/archetypeExperts.csv")

# Guardar resultados
write.csv(res_kmeans, "resultsD_plot/cluster_vs_expertKmeans.csv", row.names = FALSE)
write.csv(res_sis, "resultsD_plot/cluster_vs_expertSINS.csv", row.names = FALSE)
write.csv(res_experts, "resultsD_plot/cluster_vs_expertExperts.csv", row.names = FALSE)
