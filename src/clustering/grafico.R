library(dplyr)
library(ggplot2)
library(reshape2)
library(r2r)

m <- readRDS("data/cluster_hash.rds.xz")
keys_list <- keys(m)
freqs <- sapply(keys_list, function(k) query(m, k))
keys_mat <- do.call(rbind, keys_list)

norm <- function(x) {
  x <- tolower(x)
  x <- gsub("[[:punct:]]", "", x)
  x <- gsub("[[:space:]]+", "", x)
  x
}

expert_csv_to_binary <- function(expert_csv, determinantes) {
  expert_df <- read.csv(expert_csv, stringsAsFactors = FALSE, check.names = FALSE)
  expert_df <- expert_df[, colSums(!is.na(expert_df) & expert_df != "") > 0]
  expert_df[] <- lapply(expert_df, norm)
  determinantes_norm <- norm(determinantes)
  expert_mat <- t(as.matrix(expert_df))
  t(apply(expert_mat, 1, function(row) {
    sapply(determinantes_norm, function(det) as.integer(det %in% row))
  }))
}

compute_assignment_curve <- function(keys_mat, centers_mat, D_values) {
  n_arch <- nrow(keys_mat)
  results <- data.frame(D = D_values,
                        assigned_pct = numeric(length(D_values)),
                        unassigned_pct = numeric(length(D_values)))
  
  # convertir a lógico para acelerar
  keys_mat <- keys_mat == 1
  centers_mat <- centers_mat == 1
  
  for(d_idx in seq_along(D_values)) {
    D <- D_values[d_idx]
    
    # matriz de diferencias: cada fila = arquetipo, cada columna = centro
    diff_mat <- sapply(1:nrow(centers_mat), function(j) rowSums(keys_mat != centers_mat[j,]))
    
    # si alguna columna (centro) cumple <= D → asignado
    assigned <- sum(apply(diff_mat, 1, min) <= D)
    
    results$assigned_pct[d_idx]   <- assigned / n_arch * 100
    results$unassigned_pct[d_idx] <- 100 - results$assigned_pct[d_idx]
  }
  
  results
} 

determinantes <- colnames(keys_mat)
D_values <- seq(2, 20, 2)

#experts_bin <- expert_csv_to_binary("data/archetypeExperts.csv", determinantes)
experts_bin <- read.csv("data/dataPreproc/binarchetypeExperts.csv")


#kmeans_bin <- expert_csv_to_binary("data/archetypeKmeans.csv", determinantes)
kmeans_bin <- read.csv("data/dataPreproc/binarchetypeKmeans.csv")

#sins_bin <- expert_csv_to_binary("data/archetypeSINS.csv", determinantes)
sins_bin <- read.csv("data/dataPreproc/binarchetypeSINS.csv")


curve_experts <- compute_assignment_curve(keys_mat, experts_bin, D_values)
curve_kmeans <- compute_assignment_curve(keys_mat, kmeans_bin, D_values)
curve_sins <- compute_assignment_curve(keys_mat, sins_bin, D_values)

prepare_long <- function(curve_df) {
  df_long <- melt(curve_df, id.vars = "D", variable.name = "Type", value.name = "Percent")
  df_long$Type <- factor(df_long$Type,
                         levels = c("assigned_pct", "unassigned_pct"),
                         labels = c("Asignados", "No asignados"))
  df_long$D <- factor(df_long$D)
  df_long
}

long_experts <- prepare_long(curve_experts)
long_kmeans  <- prepare_long(curve_kmeans)
long_sins    <- prepare_long(curve_sins)

plot_method <- function(df_long, title, filename) {
  p <- ggplot(df_long, aes(x = D, y = Percent, color = Type, group = Type)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    scale_y_continuous(breaks = seq(0, 100, by = 5),
                       labels = function(x) paste0(round(x,1), "%"),
                       limits = c(0,105)) +
    geom_hline(yintercept = seq(0,100,by=5), color = "grey90", linetype = "dashed") +
    labs(title = title,
         x = "Distancia máxima D",
         y = "Porcentaje de arquetipos",
         color = "Tipo") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
  print(p)
  ggsave(filename, plot = p, width = 8, height = 5)
}

plot_method(long_experts, "Asignación de arquetipos vs D - Experts", "resultsD_plot/experts_vs_D_pct_lines.png")
plot_method(long_kmeans,  "Asignación de arquetipos vs D - KMeans", "resultsD_plot/kmeans_vs_D_pct_lines.png")
plot_method(long_sins,    "Asignación de arquetipos vs D - SINS",   "resultsD_plot/sins_vs_D_pct_lines.png")

