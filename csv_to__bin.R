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

expert_bin <- expert_csv_to_binary("data/archetypeExperts.csv", cluster_dets)
write.csv(expert_bin, paste0("data/dataPreproc/binarchetypeExperts.csv"), row.names = FALSE)

kmeans_bin <- expert_csv_to_binary("data/archetypeKmeans.csv", cluster_dets)
write.csv(kmeans_bin, paste0("data/dataPreproc/binarchetypeKmeans.csv"), row.names = FALSE)

sins_bin <- expert_csv_to_binary("data/archetypeSINS.csv", cluster_dets)
write.csv(sins_bin, paste0("data/dataPreproc/binarchetypeSINS.csv"), row.names = FALSE)
