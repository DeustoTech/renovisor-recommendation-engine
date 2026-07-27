library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)

out_root <- "comparison_experts"

scenarios <- c("100-32", "10-32", "100-9", "10-9")
types <- c("pos", "ext")

# =========================
# 1. FUNCIÓN HEATMAP
# =========================
plot_similarity_heatmap <- function(sim_file, out_file, title_txt = NULL) {
  sim_df <- fread(sim_file)
  
  sim_long <- sim_df %>%
    as.data.frame() %>%
    pivot_longer(cols = -cluster, names_to = "expert", values_to = "jaccard")
  
  p <- ggplot(sim_long, aes(x = expert, y = cluster, fill = jaccard)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "steelblue", limits = c(0, 100)) +
    labs(
      title = title_txt,
      x = "Experto",
      y = "Patrón cluster",
      fill = "Jaccard (%)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
  
  ggsave(out_file, p, width = 10, height = 7)
}

# =========================
# 2. BARRAS POR EXPERTO
# =========================
plot_summary_by_expert <- function(summary_file, out_file, title_txt = NULL) {
  df <- fread(summary_file) %>% as.data.frame()
  
  if (nrow(df) == 0) return(NULL)
  
  df <- df %>%
    arrange(total_freq) %>%
    mutate(best_match = factor(best_match, levels = best_match))
  
  p <- ggplot(df, aes(x = best_match, y = total_freq)) +
    geom_col() +
    coord_flip() +
    labs(
      title = title_txt,
      x = "Experto",
      y = "Frecuencia total asignada"
    ) +
    theme_minimal(base_size = 11)
  
  ggsave(out_file, p, width = 8, height = 5)
}

# =========================
# 3. BARRAS MEJOR MATCH POR PATRÓN
# =========================
plot_best_match_patterns <- function(best_match_file, out_file, title_txt = NULL) {
  df <- fread(best_match_file) %>% as.data.frame()
  
  df <- df %>%
    filter(!is.na(best_match)) %>%
    arrange(freq) %>%
    mutate(cluster = factor(cluster, levels = cluster))
  
  if (nrow(df) == 0) return(NULL)
  
  p <- ggplot(df, aes(x = cluster, y = freq, fill = pct_match)) +
    geom_col() +
    coord_flip() +
    scale_fill_gradient(low = "grey80", high = "darkgreen", limits = c(0, 100)) +
    labs(
      title = title_txt,
      x = "Patrón cluster",
      y = "Frecuencia",
      fill = "Jaccard (%)"
    ) +
    theme_minimal(base_size = 11)
  
  ggsave(out_file, p, width = 9, height = 6)
}

# =========================
# 4. RESUMEN GLOBAL
# =========================
plot_global_comparison <- function(global_file, out_file) {
  df <- fread(global_file) %>% as.data.frame()
  
  df$label <- paste(df$scenario, df$type, sep = " / ")
  df$label <- forcats::fct_reorder(df$label, df$weighted_mean_jaccard)
  
  p1 <- ggplot(df, aes(x = label, y = weighted_mean_jaccard)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Comparación global: similitud media ponderada",
      x = "Escenario",
      y = "Weighted mean Jaccard (%)"
    ) +
    theme_minimal(base_size = 12)
  
  ggsave(gsub("\\.png$", "_weighted_jaccard.png", out_file), p1, width = 8, height = 5)
  
  p2 <- ggplot(df, aes(x = label, y = pct_experts_covered)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Comparación global: cobertura de expertos",
      x = "Escenario",
      y = "% expertos cubiertos"
    ) +
    theme_minimal(base_size = 12)
  
  ggsave(gsub("\\.png$", "_experts_covered.png", out_file), p2, width = 8, height = 5)
  
  p3 <- ggplot(df, aes(x = label, y = weighted_pct_good_matches)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Comparación global: % de buenos matches ponderado",
      x = "Escenario",
      y = "% buenos matches"
    ) +
    theme_minimal(base_size = 12)
  
  ggsave(gsub("\\.png$", "_good_matches.png", out_file), p3, width = 8, height = 5)
}

# =========================
# 5. BUCLE PRINCIPAL
# =========================
for (sc in scenarios) {
  for (tp in types) {
    cat("Graficando:", sc, tp, "\n")
    
    dir_i <- file.path(out_root, sc, tp)
    if (!dir.exists(dir_i)) next
    
    sim_file        <- file.path(dir_i, "similarity_matrix_jaccard.csv")
    summary_file    <- file.path(dir_i, "summary_by_expert.csv")
    best_match_file <- file.path(dir_i, "best_match_nonexclusive.csv")
    
    if (file.exists(sim_file)) {
      plot_similarity_heatmap(
        sim_file,
        file.path(dir_i, "plot_heatmap_similarity.png"),
        paste("Heatmap similitud -", sc, "-", tp)
      )
    }
    
    if (file.exists(summary_file)) {
      plot_summary_by_expert(
        summary_file,
        file.path(dir_i, "plot_summary_by_expert.png"),
        paste("Frecuencia total por experto -", sc, "-", tp)
      )
    }
    
    if (file.exists(best_match_file)) {
      plot_best_match_patterns(
        best_match_file,
        file.path(dir_i, "plot_best_match_patterns.png"),
        paste("Patrones y mejor match -", sc, "-", tp)
      )
    }
  }
}

# gráfico global
global_file <- file.path(out_root, "GLOBAL_COMPARISON.csv")
if (file.exists(global_file)) {
  plot_global_comparison(
    global_file,
    file.path(out_root, "GLOBAL_COMPARISON.png")
  )
}

cat("Gráficos terminados.\n")