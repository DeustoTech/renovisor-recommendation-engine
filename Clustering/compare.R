
library(clue)  # solve_LSAP --> maximiza global!!!!

centers_df <- read.csv("resultsD_plot/D_6/greedy_cluster_centers.csv", stringsAsFactors = FALSE)
centers_mat <- as.matrix(centers_df[, -1])
determinantes_raw <- colnames(centers_df)[-1]

# Normalización de texto
norm <- function(x) {
  x <- tolower(x)
  x <- gsub("[[:punct:]]", "", x)
  x <- gsub("[[:space:]]+", "", x)
  x
}

determinantes <- norm(determinantes_raw)
colnames(centers_mat) <- determinantes

expert_csv_to_binary <- function(expert_csv, determinantes) {
  expert_df <- read.csv(expert_csv, stringsAsFactors = FALSE, check.names = FALSE)
  
  # eliminar columnas vacías
  expert_df <- expert_df[, colSums(!is.na(expert_df) & expert_df != "") > 0]
  
  expert_mat <- t(as.matrix(expert_df))
  expert_bin <- t(apply(expert_mat, 1, function(row) {
    sapply(determinantes, function(det) as.integer(det %in% norm(row)))
  }))
  rownames(expert_bin) <- rownames(expert_mat)
  expert_bin
}

jaccard_similarity <- function(cluster_row, expert_row) {
  inter <- sum(cluster_row == 1 & expert_row == 1)
  uni   <- sum(cluster_row == 1 | expert_row == 1)
  if (uni == 0) return(0)
  inter / uni * 100
}

build_similarity_matrix <- function(centers_mat, expert_csv, determinantes) {
  expert_bin <- expert_csv_to_binary(expert_csv, determinantes)
  
  n_clusters <- nrow(centers_mat)
  n_archetypes  <- nrow(expert_bin)
  
  sim_mat <- matrix(0, n_clusters, n_archetypes)
  rownames(sim_mat) <- centers_df$cluster
  colnames(sim_mat) <- rownames(expert_bin)
  
  for (i in 1:n_clusters) {
    cluster_row <- centers_mat[i, ] > 0
    if (sum(cluster_row) == 0) next
    
    for (j in 1:n_archetypes) {
      sim_mat[i, j] <- jaccard_similarity(cluster_row, expert_bin[j, ])
    }
  }
  
  sim_mat
}

assign_clusters_exclusive <- function(sim_mat, centers_df, min_pct = 0) {
  
  n_clusters <- nrow(sim_mat)
  n_archetypes  <- ncol(sim_mat)
  
  # si hay más clusters que expertos, agregamos columnas ficticias "None"
  if (n_clusters > n_archetypes) {
    n_add <- n_clusters - n_archetypes
    sim_mat <- cbind(sim_mat, matrix(0, n_clusters, n_add))
    colnames(sim_mat)[(n_archetypes + 1):ncol(sim_mat)] <- paste0("None_", 1:n_add)
  }
  
  assignment <- solve_LSAP(sim_mat, maximum = TRUE)
  
  results <- data.frame(
    cluster    = centers_df$cluster,
    best_match = NA,
    pct_match  = NA
  )
  
  for (i in 1:n_clusters) {
    expert_idx <- assignment[i]
    expert_name <- colnames(sim_mat)[expert_idx]
    pct <- sim_mat[i, expert_idx]
    
    if (!grepl("^None_", expert_name) && pct >= min_pct) {
      results$best_match[i] <- expert_name
      results$pct_match[i]  <- pct
    }
  }
  
  results
}


sim_kmeans  <- build_similarity_matrix(centers_mat, "data/archetypeKmeans.csv", determinantes)
sim_sins    <- build_similarity_matrix(centers_mat, "data/archetypeSINS.csv", determinantes)
sim_experts <- build_similarity_matrix(centers_mat, "data/archetypeExperts.csv", determinantes)

res_kmeans_exclusive  <- assign_clusters_exclusive(sim_kmeans, centers_df, min_pct = 0)
res_sins_exclusive    <- assign_clusters_exclusive(sim_sins, centers_df, min_pct = 0)
res_experts_exclusive <- assign_clusters_exclusive(sim_experts, centers_df, min_pct = 0)

write.csv(res_kmeans_exclusive,  "resultsD_plot/cluster_vs_expertKmeans_D6.csv", row.names = FALSE)
write.csv(res_sins_exclusive,    "resultsD_plot/cluster_vs_expertSINS_D6.csv", row.names = FALSE)
write.csv(res_experts_exclusive, "resultsD_plot/cluster_vs_expertExperts_D6.csv", row.names = FALSE)
