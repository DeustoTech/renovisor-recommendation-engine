library(r2r)
library(ggplot2)
library(reshape2)

m <- readRDS("cluster_hash.rds.xz")

K <- 8                     # número de clusters
D_values <- seq(2, 20, 2)  # SOLO D PARES

keys_list <- keys(m)
freqs <- sapply(keys_list, function(k) query(m, k))
keys_mat <- do.call(rbind, keys_list)
n_arch <- nrow(keys_mat)

# Distancia de Hamming
hamming_dist <- function(x, y) sum(x != y)

# Encontrar arquetipos similares
find_similar <- function(center, keys_mat, freqs, used, D) {
  free_idx <- which(!used)
  if(length(free_idx) == 0) return(list(indices = integer(0), score = 0))
  dists <- apply(keys_mat[free_idx, , drop = FALSE], 1, function(y) hamming_dist(center, y))
  sel <- free_idx[dists <= D]
  list(indices = sel, score = sum(freqs[sel]))
}

# Algoritmo greedy
greedy_clusters <- function(keys_mat, freqs, K, D) {
  n <- nrow(keys_mat)
  used <- rep(FALSE, n)
  clusters <- vector("list", K)
  scores <- numeric(K)
  for(i in 1:K) {
    if(all(used)) break
    candidates <- which(!used)
    center_idx <- candidates[which.max(freqs[candidates])]
    center <- keys_mat[center_idx, ]
    sim <- find_similar(center, keys_mat, freqs, used, D)
    used[sim$indices] <- TRUE
    clusters[[i]] <- list(center_index = center_idx, center = center, members = sim$indices)
    scores[i] <- sim$score
  }
  list(clusters = clusters, scores = scores, used = used)
}

results <- data.frame(D = integer(), assigned_pct = numeric(), unassigned_pct = numeric())
dir.create("resultsD_plot", showWarnings = FALSE)

for(D in D_values) {
  cat("Probando D =", D, "\n")
  res <- greedy_clusters(keys_mat, freqs, K, D)
  
  dir_D <- file.path("resultsD_plot", paste0("D_", D))
  dir.create(dir_D, showWarnings = FALSE)
  
  centers_df <- do.call(rbind, lapply(seq_along(res$clusters), function(i) {
    cl <- res$clusters[[i]]
    if(is.null(cl$center) || length(cl$center)==0) return(NULL)
    cbind(cluster = i, as.data.frame(as.list(cl$center)))
  }))
  write.csv(centers_df, file.path(dir_D, "greedy_cluster_centers.csv"), row.names = FALSE)
  
  assignments_df <- do.call(rbind, lapply(seq_along(res$clusters), function(i) {
    cl <- res$clusters[[i]]
    if(length(cl$members) == 0) return(NULL)
    data.frame(archetype_id = cl$members,
               cluster = i)
  }))
  write.csv(assignments_df, file.path(dir_D, "greedy_cluster_assignments.csv"), row.names = FALSE)
  
  total_arch <- n_arch
  cumulative_pct <- 0   # <- para el pct_rest_arch estilo “resta general”
  summary_list <- list()
  
  for(i in seq_along(res$clusters)) {
    cl <- res$clusters[[i]]
    if(length(cl$members) == 0) next
    
    n_cluster <- length(cl$members)
    
    pct_total <- n_cluster / total_arch * 100              # % del total de arquetipos
    pct_rest  <- 100 - cumulative_pct                       # % restante general
    cumulative_pct <- cumulative_pct + pct_total           # actualizar acumulado
    
    summary_list[[i]] <- data.frame(
      cluster = i,
      n_archetypes = n_cluster,
      pct_total_arch = pct_total,
      pct_rest_arch  = pct_rest
    )
  }
  
  summary_df <- do.call(rbind, summary_list)
  write.csv(summary_df, file.path(dir_D, "greedy_cluster_summary_by_archetypes.csv"), row.names = FALSE)
  
  # Arquetipos no asignados
  unassigned_df <- data.frame(
    archetype_id = which(!res$used)
  )
  write.csv(unassigned_df, file.path(dir_D, "greedy_unassigned_archetypes.csv"), row.names = FALSE)
  
  # Porcentaje asignados vs no asignados
  assigned <- sum(res$used)
  unassigned <- n_arch - assigned
  results <- rbind(results, data.frame(
    D = D,
    assigned_pct   = assigned / n_arch * 100,
    unassigned_pct = unassigned / n_arch * 100
  ))
}

# Plot 
results_long <- melt(results, id.vars = "D", variable.name = "Type", value.name = "Percent")
results_long$Type <- factor(results_long$Type, levels = c("assigned_pct", "unassigned_pct"),
                            labels = c("Asignados", "No asignados"))
results_long$D <- factor(results_long$D)

p <- ggplot(results_long, aes(x = D, y = Percent, color = Type, group = Type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 5),
    labels = function(x) paste0(round(x,1), "%"),
    limits = c(0, 105)
  ) +
  geom_hline(yintercept = seq(0,100,by=5), color = "grey90", linetype = "dashed") +
  labs(
    title = "Asignación de arquetipos vs distancia máxima D",
    x = "Distancia máxima D",
    y = "Porcentaje de arquetipos",
    color = "Tipo"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

print(p)
ggsave("resultsD_plot/assigned_vs_D_pct_lines.png", plot = p, width = 8, height = 5)
