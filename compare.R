library(dplyr)


centers_df <- read.csv( "resultsD_plot/D_12/greedy_cluster_centers.csv", stringsAsFactors = FALSE)

# quitar columna cluster
determinantes_raw <- colnames(centers_df)[-1]

# matriz de centros
centers_mat <- as.matrix(centers_df[, -1])


norm <- function(x) {
  x <- tolower(x)
  x <- gsub("[[:punct:]]", "", x)
  x <- gsub("[[:space:]]+", "", x)
  x
}

determinantes <- norm(determinantes_raw)
colnames(centers_mat) <- determinantes


# CSV experto → matriz binaria
expert_csv_to_binary <- function(expert_csv, determinantes) {
  expert_df <- read.csv(expert_csv, stringsAsFactors = FALSE)

  # eliminar columnas de indica si existen
  drop_cols <- c("cluster", "Cluster", "id", "ID", "X")
  expert_df <- expert_df[, !(names(expert_df) %in% drop_cols), drop = FALSE]
  
  expert_bin <- sapply(expert_df, function(col) {
    vals <- col[!is.na(col) & col != ""]
    vals <- norm(vals)
    sapply(determinantes, function(det) {
      as.integer(det %in% vals)
    })
  })
  
  # filas = arquetipos expertos
  expert_bin <- t(expert_bin)
  return(expert_bin)
}

#  Comparar centros con expertos
compare_centers_with_experts <- function(centers_mat, expert_csv, determinantes) {
  expert_bin <- expert_csv_to_binary(expert_csv, determinantes)
  results <- data.frame()
  for (i in seq_len(nrow(centers_mat))) {
    center <- centers_mat[i, ]
    center_active <- center > 0
    if (sum(center_active) == 0) {
      best_match <- NA
      pct_match  <- NA
    } else {
      similarity <- apply(expert_bin, 1, function(expert_row) {
        sum(center_active & expert_row == 1) / sum(center_active) * 100
      })
      best_match <- rownames(expert_bin)[which.max(similarity)]
      pct_match  <- max(similarity)
    }
    
    results <- rbind(
      results,
      data.frame(
        cluster    = centers_df$cluster[i],
        best_match = best_match,
        pct_match  = pct_match
      )
    )
  }
  
  return(results)
}

res_kmeans <- compare_centers_with_experts(centers_mat, "data/archetypeKmeans.csv", determinantes)
res_sins <- compare_centers_with_experts( centers_mat, "data/archetypeSINS.csv", determinantes)
res_experts <- compare_centers_with_experts( centers_mat, "data/archetypeExperts.csv", determinantes)

write.csv( res_kmeans, "resultsD_plot/cluster_vs_expertKmeans_D12.csv", row.names = FALSE)
write.csv( res_sins, "resultsD_plot/cluster_vs_expertSINS_D12.csv", row.names = FALSE)
write.csv( res_experts, "resultsD_plot/cluster_vs_expertExperts_D12.csv", row.names = FALSE)
