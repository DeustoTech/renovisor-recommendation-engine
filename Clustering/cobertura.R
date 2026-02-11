library(dplyr)

########################## preprocess 

norm <- function(x) {
  x <- tolower(x)
  x <- gsub("[[:punct:]]", "", x)
  x <- gsub("[[:space:]]+", "", x)
  x
}

#poner apra todos
clusters_df <- read.csv("resultsD_plot/D_6/greedy_cluster_centers.csv", stringsAsFactors = FALSE)

cluster_dets <- norm(colnames(clusters_df)[-1])

cluster_mat <- as.matrix(clusters_df[, -1])

expert_csv_to_binary <- function(expert_csv, determinantes) {
  expert_df <- read.csv(expert_csv, stringsAsFactors = FALSE, check.names = FALSE)
  expert_df <- expert_df[, colSums(!is.na(expert_df) & expert_df != "") > 0]  # quitar columnas vacías
  expert_mat <- t(as.matrix(expert_df))
  
  expert_bin <- t(apply(expert_mat, 1, function(row) {
    sapply(determinantes, function(det) as.integer(det %in% norm(row)))
  }))
  
  rownames(expert_bin) <- rownames(expert_mat)
  expert_bin
}

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
    active_dets <- cluster_dets[cluster_row]       # determinantes activos del cluster
    cluster_all <- paste(active_dets, collapse = ", ")  # todos los determinantes del cluster
    
    for (j in 1:n_experts) {
      expert_row <- expert_bin[j, ] > 0
      vol_expert <- sum(expert_row)
      expert_dets <- cluster_dets[expert_row]       # determinantes activos del experto
      expert_all <- paste(expert_dets, collapse = ", ") # todos los determinantes del experto
      
      common_dets <- intersect(active_dets, expert_dets)
      vol_common <- length(common_dets)
      
      coverage_list[[length(coverage_list)+1]] <- data.frame(
        cluster = cluster_names[i],
        expert_row = expert_names[j],
        vol_cluster = vol_cluster,
        cluster_dets_all = cluster_all,
        vol_expert = vol_expert,
        expert_dets_all = expert_all,
        vol_common = vol_common,
        common_dets = paste(common_dets, collapse = ", "),
        pct_cluster_covered = ifelse(vol_cluster == 0, 0, vol_common / vol_cluster * 100),
        pct_expert_covered = ifelse(vol_expert == 0, 0, vol_common / vol_expert * 100),
        stringsAsFactors = FALSE
      )
    }
  }
  
  do.call(rbind, coverage_list)
}

expert_bin <- expert_csv_to_binary("data/archetypeExperts.csv", cluster_dets)
coverage_experts <- compute_coverage_binary(cluster_mat, expert_bin)

kmeans_bin <- expert_csv_to_binary("data/archetypeKmeans.csv", cluster_dets)
coverage_kmeans <- compute_coverage_binary(cluster_mat, kmeans_bin)

sins_bin <- expert_csv_to_binary("data/archetypeSINS.csv", cluster_dets)
coverage_sins <- compute_coverage_binary(cluster_mat, sins_bin)

write.csv(coverage_experts, "resultsD_plot/coverage_experts_D6.csv", row.names = FALSE)
write.csv(coverage_kmeans, "resultsD_plot/coverage_kmeans_D6.csv", row.names = FALSE)
write.csv(coverage_sins, "resultsD_plot/coverage_sins_D6.csv", row.names = FALSE)


################################################ plot
library(ggplot2)
ggplot(coverage_experts, aes(x=cluster, y=pct_cluster_covered, fill=expert_row)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Cobertura de clusters vs expertos", y="% de cluster cubierto") +
  theme_minimal()
