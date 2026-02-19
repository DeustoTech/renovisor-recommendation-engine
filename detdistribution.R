
library(dplyr)

D_values <- seq(2, 20, 2) 

for(D in D_values){
  cat("Procesando D =", D, "\n")
  dir_D <- file.path("resultsD_plot", paste0("D_", D))
  
  centers_df <- read.csv(file.path(dir_D, "greedy_cluster_centers.csv"), stringsAsFactors = FALSE)
  assignments_df <- read.csv(file.path(dir_D, "greedy_cluster_assignments.csv"), stringsAsFactors = FALSE)
  
  determinantes <- colnames(centers_df)[-1]
  
  # Cuántos clusters contiene cada determinante
  det_in_clusters <- sapply(determinantes, function(det) {
    sum(sapply(1:nrow(centers_df), function(r) centers_df[r, det] == 1))
  })
  det_in_clusters_df <- data.frame(
    determinante = determinantes,
    n_clusters = det_in_clusters
  )
  write.csv(det_in_clusters_df, file.path(dir_D, "determinantes_in_clusters.csv"), row.names = FALSE)
  
}

