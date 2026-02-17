library(dplyr)
library(ggplot2)
library(clue)  # solve_LSAP

norm <- function(x) {
  x <- tolower(x)
  x <- gsub("[[:punct:]]", "", x)
  x <- gsub("[[:space:]]+", "", x)
  x
}

expert_csv_to_binary <- function(expert_csv, determinantes) {
  expert_df <- read.csv(expert_csv, stringsAsFactors = FALSE, check.names = FALSE)
  expert_df <- expert_df[, colSums(!is.na(expert_df) & expert_df != "") > 0]
  expert_mat <- t(as.matrix(expert_df))
  
  expert_bin <- t(apply(expert_mat, 1, function(row) {
    sapply(determinantes, function(det) as.integer(det %in% norm(row)))
  }))
  
  rownames(expert_bin) <- rownames(expert_mat)
  expert_bin
}


# compara cada cluster con cada experto
compute_coverage_binary <- function(cluster_mat, expert_bin, cluster_names = NULL, expert_names = NULL, cluster_dets = NULL) {
  
  n_clusters <- nrow(cluster_mat)
  n_experts <- nrow(expert_bin)
  
  if (is.null(cluster_names)) cluster_names <- 1:n_clusters
  if (is.null(expert_names)) expert_names <- rownames(expert_bin)
  if (is.null(cluster_dets)) cluster_dets <- colnames(cluster_mat)
  
  coverage_list <- list()
  
  for (i in 1:n_clusters) {
    cluster_row <- cluster_mat[i, ] > 0
    vol_cluster <- sum(cluster_row)
    active_dets <- cluster_dets[cluster_row]
    cluster_all <- paste(active_dets, collapse = ", ")
    
    for (j in 1:n_experts) {
      expert_row <- expert_bin[j, ] > 0
      vol_expert <- sum(expert_row)
      expert_dets <- cluster_dets[expert_row]
      expert_all <- paste(expert_dets, collapse = ", ")
      
      common_dets <- intersect(active_dets, expert_dets)
      vol_common <- length(common_dets)
      
      coverage_list[[length(coverage_list)+1]] <- data.frame(
        cluster = cluster_names[i],
        expert_row = expert_names[j],
        vol_cluster = vol_cluster, # número de determinantes activos en el cluster
        #cluster_dets_all = cluster_all, # que det
        vol_expert = vol_expert, # número de determinantes activos en el experto
        #expert_dets_all = expert_all,
        vol_common = vol_common, # cuántos determinantes comparten
        #common_dets = paste(common_dets, collapse = ", "),
        pct_cluster_covered = ifelse(vol_cluster == 0, 0, vol_common / vol_cluster * 100), # qué % del cluster está presente en el experto
        pct_expert_covered = ifelse(vol_expert == 0, 0, vol_common / vol_expert * 100), # qué % del experto coincide con el cluster
        stringsAsFactors = FALSE
      )
    }
  }
  
  do.call(rbind, coverage_list)
}

jaccard_similarity <- function(cluster_row, expert_row) {
  inter <- sum(cluster_row == 1 & expert_row == 1)
  uni   <- sum(cluster_row == 1 | expert_row == 1)
  if (uni == 0) return(0)
  inter / uni * 100
}

build_similarity_matrix <- function(cluster_mat, expert_csv, determinantes) {
  expert_bin <- expert_csv_to_binary(expert_csv, determinantes)
  
  n_clusters <- nrow(cluster_mat)
  n_experts <- nrow(expert_bin)
  
  sim_mat <- matrix(0, n_clusters, n_experts)
  rownames(sim_mat) <- rownames(cluster_mat)
  colnames(sim_mat) <- rownames(expert_bin)
  
  for (i in 1:n_clusters) {
    cluster_row <- cluster_mat[i, ] > 0
    if (sum(cluster_row) == 0) next
    
    for (j in 1:n_experts) {
      sim_mat[i, j] <- jaccard_similarity(cluster_row, expert_bin[j, ])
    }
  }
  
  sim_mat # matriz donde cada fila = cluster, cada columna = experto
  # cada celda = Jaccard (%) entre ese cluster y ese experto
}

assign_clusters_exclusive <- function(sim_mat, cluster_names, min_pct = 0) {
# Usa solve_LSAP para asignar cada cluster a un solo experto, maximizando la suma total de similitudes
  n_clusters <- nrow(sim_mat)
  n_experts <- ncol(sim_mat)
  
  # si hay más clusters que expertos, agregamos columnas ficticias "None"
  if (n_clusters > n_experts) {
    n_add <- n_clusters - n_experts
    sim_mat <- cbind(sim_mat, matrix(0, n_clusters, n_add))
    colnames(sim_mat)[(n_experts + 1):ncol(sim_mat)] <- paste0("None_", 1:n_add)
  }
  
  assignment <- solve_LSAP(sim_mat, maximum = TRUE)
  
  results <- data.frame(
    cluster = cluster_names,
    best_match = NA,
    pct_match = NA
  )
  
  for (i in 1:n_clusters) {
    expert_idx <- assignment[i]
    expert_name <- colnames(sim_mat)[expert_idx]
    pct <- sim_mat[i, expert_idx]
    
    if (!grepl("^None_", expert_name) && pct >= min_pct) {
      results$best_match[i] <- expert_name
      results$pct_match[i] <- pct
    }
  }
  
  results
}

for (D in seq(2, 20, by = 2)) {
  cat("Procesando D =", D, "\n")
  
  cluster_path <- paste0("resultsD_plot/D_", D, "/greedy_cluster_centers.csv")
  
  if (!file.exists(cluster_path)) {
    cat("No existe:", cluster_path, "\n")
    next
  }
  
  clusters_df <- read.csv(cluster_path, stringsAsFactors = FALSE)
  cluster_dets <- norm(colnames(clusters_df)[-1])
  cluster_mat <- as.matrix(clusters_df[, -1])
  rownames(cluster_mat) <- clusters_df$cluster
  
  expert_bin <- expert_csv_to_binary("data/archetypeExperts.csv", cluster_dets)
  coverage_experts <- compute_coverage_binary(cluster_mat, expert_bin, cluster_dets = cluster_dets)
  
  kmeans_bin <- expert_csv_to_binary("data/archetypeKmeans.csv", cluster_dets)
  coverage_kmeans <- compute_coverage_binary(cluster_mat, kmeans_bin, cluster_dets = cluster_dets)
  
  sins_bin <- expert_csv_to_binary("data/archetypeSINS.csv", cluster_dets)
  coverage_sins <- compute_coverage_binary(cluster_mat, sins_bin, cluster_dets = cluster_dets)
  
  write.csv(coverage_experts, paste0("resultsD_plot/D_", D, "/coverage_experts_D", D, ".csv"), row.names = FALSE)
  write.csv(coverage_kmeans, paste0("resultsD_plot/D_", D, "/coverage_kmeans_D", D, ".csv"), row.names = FALSE)
  write.csv(coverage_sins, paste0("resultsD_plot/D_", D, "/coverage_sins_D", D, ".csv"), row.names = FALSE)
  
  sim_experts <- build_similarity_matrix(cluster_mat, "data/archetypeExperts.csv", cluster_dets)
  sim_kmeans <- build_similarity_matrix(cluster_mat, "data/archetypeKmeans.csv", cluster_dets)
  sim_sins <- build_similarity_matrix(cluster_mat, "data/archetypeSINS.csv", cluster_dets)
  
  res_experts_excl <- assign_clusters_exclusive(sim_experts, rownames(cluster_mat))
  res_kmeans_excl <- assign_clusters_exclusive(sim_kmeans, rownames(cluster_mat))
  res_sins_excl <- assign_clusters_exclusive(sim_sins, rownames(cluster_mat))
  
  write.csv(res_experts_excl, paste0("resultsD_plot/D_", D, "/cluster_vs_expertExperts_D", D, ".csv"), row.names = FALSE)
  write.csv(res_kmeans_excl, paste0("resultsD_plot/D_", D, "/cluster_vs_expertKmeans_D", D, ".csv"), row.names = FALSE)
  write.csv(res_sins_excl, paste0("resultsD_plot/D_", D, "/cluster_vs_expertSINS_D", D, ".csv"), row.names = FALSE)
  
  p <- ggplot(coverage_experts, aes(x=cluster, y=pct_cluster_covered, fill=expert_row)) +
    geom_bar(stat="identity", position="dodge") +
    labs(title=paste("Cobertura clusters vs expertos - D =", D),
         y="% de cluster cubierto") +
    theme_minimal()
  
  ggsave(paste0("resultsD_plot/D_", D, "/plot_experts_D", D, ".png"),
         plot = p, width = 10, height = 6)
  
}
