library(data.table)
library(dplyr)
library(ggplot2)

norm_txt <- function(x) {
  x <- tolower(x)
  x <- gsub("[[:punct:]]", "", x)
  x <- gsub("[[:space:]]+", "", x)
  x
}

read_freq_clusters <- function(file_path) {
  df <- fread(file_path)
  
  if (!"N" %in% names(df)) {
    stop(paste("No existe columna N en", file_path))
  }
  
  freq <- df$N
  mat <- as.matrix(df[, !"N"])
  rownames(mat) <- paste0("pattern_", seq_len(nrow(mat)))
  
  list(mat = mat, freq = freq, df = df)
}

expert_csv_to_binary <- function(expert_csv, det_names) {
  expert_df <- read.csv(expert_csv, stringsAsFactors = FALSE, check.names = FALSE)
  expert_df <- expert_df[, colSums(!is.na(expert_df) & expert_df != "") > 0, drop = FALSE]
  
  expert_mat <- t(as.matrix(expert_df))
  dets_norm <- norm_txt(det_names)
  
  expert_bin <- t(apply(expert_mat, 1, function(row) {
    row_norm <- norm_txt(row)
    sapply(dets_norm, function(det) as.integer(det %in% row_norm))
  }))
  
  expert_bin <- as.matrix(expert_bin)
  colnames(expert_bin) <- det_names
  
  rn <- rownames(expert_mat)
  if (is.null(rn) || any(is.na(rn)) || any(rn == "")) {
    rn <- paste0("expert_", seq_len(nrow(expert_bin)))
  }
  rownames(expert_bin) <- rn
  
  expert_bin
}

hamming_dist <- function(x, y) sum(x != y)

base_dir <- "~/Desktop/master/PFM_extra/100000"
out_root <- "results/results_no_greedy_from_freq"
if (!dir.exists(out_root)) dir.create(out_root, recursive = TRUE)

scenarios <- c("100-32", "10-32", "100-9", "10-9")
types <- c("pos", "ext")

D_values <- seq(0, 20, by = 1)

expert_files <- list(
  experts = "data/archetypeExperts.csv",
  kmeans  = "data/archetypeKmeans.csv",
  sins    = "data/archetypeSINS.csv"
)

all_results <- list()

for (sc in scenarios) {
  for (tp in types) {
    
    cat("Procesando:", sc, tp, "\n")
    
    freq_file <- file.path(base_dir, sc, paste0("freq_cluster_det_", tp, ".csv"))
    if (!file.exists(freq_file)) next
    
    cl_obj <- read_freq_clusters(freq_file)
    pattern_mat <- cl_obj$mat
    freqs <- cl_obj$freq
    total_freq <- sum(freqs)
    
    for (ref_name in names(expert_files)) {
      expert_file <- expert_files[[ref_name]]
      if (!file.exists(expert_file)) next
      
      expert_bin <- expert_csv_to_binary(expert_file, colnames(pattern_mat))
      n_exp <- nrow(expert_bin)
      if (n_exp == 0) next
      
      # matriz de distancias patrón x experto
      dist_mat <- matrix(NA_real_, nrow = nrow(pattern_mat), ncol = n_exp)
      rownames(dist_mat) <- rownames(pattern_mat)
      colnames(dist_mat) <- rownames(expert_bin)
      
      for (j in seq_len(n_exp)) {
        exp_vec <- expert_bin[j, ]
        dist_mat[, j] <- apply(pattern_mat, 1, function(p) hamming_dist(p, exp_vec))
      }
      
      # para cada D, ordenar expertos por cobertura individual
      # y calcular cobertura conjunta de los top-K expertos
      for (D in D_values) {
        
        # cobertura individual por experto a este D
        expert_cov <- sapply(seq_len(n_exp), function(j) {
          covered <- dist_mat[, j] <= D
          sum(freqs[covered])
        })
        
        ord <- order(expert_cov, decreasing = TRUE)
        
        for (K in seq_len(n_exp)) {
          sel_experts <- ord[1:K]
          
          # unión de patrones cubiertos por los K expertos seleccionados
          covered_union <- apply(dist_mat[, sel_experts, drop = FALSE] <= D, 1, any)
          freq_cov <- sum(freqs[covered_union])
          pct_cov <- if (total_freq == 0) 0 else 100 * freq_cov / total_freq
          
          all_results[[length(all_results) + 1]] <- data.frame(
            scenario = sc,
            type = tp,
            reference = ref_name,
            D = D,
            K = K,
            freq_covered = freq_cov,
            pct_covered = pct_cov,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
}

results_df <- bind_rows(all_results)
write.csv(results_df, file.path(out_root, "ALL_REFERENCE_COVERAGE.csv"), row.names = FALSE)


make_pdf <- function(df, type_value, ref_value, outfile) {
  df_sub <- df %>%
    filter(type == type_value, reference == ref_value) %>%
    mutate(
      scenario = factor(scenario, levels = c("100-32", "10-32", "100-9", "10-9")),
      K = factor(K)
    )
  
  p <- ggplot(df_sub, aes(x = D, y = pct_covered, color = K, group = K)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    facet_wrap(~ scenario, ncol = 2, scales = "fixed") +
    scale_x_continuous(breaks = D_values) +
    scale_y_continuous(
      breaks = seq(0, 100, by = 10),
      limits = c(0, 100),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = paste("Cobertura conjunta vs D -", toupper(type_value), "-", ref_value),
      x = "D (distancia de Hamming patrón-experto)",
      y = "% frecuencia cubierta",
      color = "K"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
  
  ggsave(outfile, plot = p, width = 11, height = 8.5)
}

for (tp in types) {
  for (ref_name in names(expert_files)) {
    if (any(results_df$type == tp & results_df$reference == ref_name)) {
      make_pdf(
        results_df,
        tp,
        ref_name,
        file.path(out_root, paste0("coverage_vs_D_", tp, "_", ref_name, ".pdf"))
      )
    }
  }
}

cat("finnn\n")