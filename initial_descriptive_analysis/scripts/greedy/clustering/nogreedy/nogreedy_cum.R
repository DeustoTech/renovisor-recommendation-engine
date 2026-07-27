library(data.table)
library(dplyr)
library(ggplot2)

norm_txt <- function(x) {
  x <- toupper(as.character(x))
  x <- trimws(x)
  x <- gsub("[[:punct:]]", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  x
}

recode_dets <- function(x) {
  x <- norm_txt(x)
  x[x == "BRAG"]     <- "RECOGNITION"
  x[x == "POSEUR"]   <- "APPROVAL"
  x[x == "COZYNESS"] <- "COZINESS"
  x
}

hamming_dist <- function(x, y) sum(x != y)

jaccard_sim <- function(x, y) {
  inter <- sum(x == 1 & y == 1)
  uni   <- sum(x == 1 | y == 1)
  if (uni == 0) return(1)
  inter / uni
}

read_freq_clusters <- function(file_path) {
  df <- fread(file_path)
  
  if (!"N" %in% names(df)) {
    stop(paste("No existe columna N en", file_path))
  }
  
  freq <- df$N
  mat <- as.matrix(df[, !"N"])
  storage.mode(mat) <- "integer"
  colnames(mat) <- recode_dets(colnames(mat))
  rownames(mat) <- paste0("pattern_", seq_len(nrow(mat)))
  
  list(mat = mat, freq = freq, df = df)
}

read_binary_archetypes <- function(file_path, target_cols = NULL) {
  df <- fread(file_path)
  df <- as.data.frame(df, check.names = FALSE)
  
  if (!"Archetype" %in% names(df)) {
    stop(paste("No existe columna Archetype en", file_path))
  }
  
  rownames(df) <- df$Archetype
  df$Archetype <- NULL
  colnames(df) <- recode_dets(colnames(df))
  
  mat <- as.matrix(df)
  mode(mat) <- "numeric"
  
  if (!is.null(target_cols)) {
    target_cols <- recode_dets(target_cols)
    miss <- setdiff(target_cols, colnames(mat))
    if (length(miss) > 0) {
      stop(paste("Faltan columnas en", file_path, ":", paste(miss, collapse = ", ")))
    }
    mat <- mat[, target_cols, drop = FALSE]
  }
  
  mat
}

make_pdf <- function(df, type_value, ref_value, outfile, D_values, D_order) {
  df_sub <- df %>%
    filter(type == type_value, reference == ref_value) %>%
    mutate(
      #scenario = factor(scenario, levels = c("100-32", "10-32")),
      #scenario = factor(scenario, levels = c("100-9", "10-9")),
      scenario = factor(scenario, levels = c("100-9-top3", "10-9-top3")),
      legend_lab = paste0("K=", K)
    )
  
  p <- ggplot(df_sub, aes(x = D, y = pct_covered, color = legend_lab, group = legend_lab)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.1) +
    facet_wrap(~ scenario, ncol = 2, scales = "fixed") +
    scale_x_continuous(breaks = D_values) +
    scale_y_continuous(
      breaks = seq(0, 100, by = 10),
      limits = c(0, 100),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = paste0("Cobertura acumulativa tras ordenar arquetipos por cobertura individual en D=", D_order,
                     " - ", toupper(type_value), " - ", ref_value),
      x = "D (margen de distancia de Hamming)",
      y = "% frecuencia cubierta",
      color = "K"
    ) +
    guides(color = guide_legend(
      ncol = 1,
      byrow = TRUE,
      keyheight = unit(0.35, "cm"),
      keywidth  = unit(0.45, "cm")
    )) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "right",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 6, lineheight = 0.85),
      legend.key.height = unit(0.35, "cm"),
      legend.key.width = unit(0.45, "cm"),
      legend.spacing.y = unit(0.03, "cm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0)
    )
  
  ggsave(outfile, plot = p, width = 12, height = 8.5)
}

make_cumulative_pdf_by_scenario <- function(df, type_value, scenario_value, ref_value, outfile, D_values, D_order) {
  df_sub <- df %>%
    filter(type == type_value, scenario == scenario_value, reference == ref_value) %>%
    mutate(
      legend_lab = paste0("K=", K)
    )
  
  p <- ggplot(df_sub, aes(x = D, y = pct_covered, color = legend_lab, group = legend_lab)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.1) +
    scale_x_continuous(breaks = D_values) +
    scale_y_continuous(
      breaks = seq(0, 100, by = 10),
      limits = c(0, 100),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = paste0(ref_value, " - ", type_value, " - ", scenario_value, " (orden D=", D_order, ")"),
      x = "D",
      y = "% cubierto",
      color = "K"
    ) +
    guides(color = guide_legend(
      ncol = 1,
      byrow = TRUE,
      keyheight = unit(0.35, "cm"),
      keywidth  = unit(0.45, "cm")
    )) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 5.5, lineheight = 0.8),
      legend.key.height = unit(0.35, "cm"),
      legend.key.width = unit(0.45, "cm"),
      legend.spacing.y = unit(0.02, "cm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0)
    )
  
  ggsave(outfile, p, width = 8.5, height = 6)
}

#base_dir <- "~/Desktop/master/PFM_extra/1000000"
base_dir <- "data/processed/9_top3"
#out_root <- "results/det32/results_no_greedy_from_freq"
#out_root <- "results/det9/results_no_greedy_from_freq_9"
out_root <- "results/det9_top3/results_no_greedy_from_freq_9_top3"

if (!dir.exists(out_root)) dir.create(out_root, recursive = TRUE)

#scenarios <- c("100-32", "10-32")
#scenarios <- c("100-9", "10-9")
scenarios <- c("100-9-top3", "10-9-top3")

types <- c("pos", "ext")
D_values <- seq(0, 10, by = 2)
D_order <- 4

expert_files <- list(
  #experts = "data/raw/archetypeExperts.csv",
  #kmeans  = "data/raw/archetypeKmeans.csv",
  #sins    = "data/raw/archetypeSINS.csv"
  experts = "data/processed/archetypeExperts_9.csv",
  kmeans  = "data/processed/archetypeKmeans_9.csv",
  sins    = "data/processed/archetypeSINS_9.csv"
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
    pattern_names <- rownames(pattern_mat)
    
    ord_pat <- order(freqs, decreasing = TRUE)
    pattern_mat <- pattern_mat[ord_pat, , drop = FALSE]
    freqs <- freqs[ord_pat]
    pattern_names <- pattern_names[ord_pat]
    
    for (ref_name in names(expert_files)) {
      
      expert_file <- expert_files[[ref_name]]
      if (!file.exists(expert_file)) next
      
      expert_bin <- read_binary_archetypes(expert_file, target_cols = colnames(pattern_mat))
      n_exp <- nrow(expert_bin)
      if (n_exp == 0) next
      
      expert_names <- rownames(expert_bin)
      
      ref_dir <- file.path(out_root, sc, tp, ref_name)
      dir.create(ref_dir, recursive = TRUE, showWarnings = FALSE)
      
      dist_mat <- matrix(NA_real_, nrow = nrow(pattern_mat), ncol = n_exp)
      sim_mat  <- matrix(NA_real_, nrow = nrow(pattern_mat), ncol = n_exp)
      
      rownames(dist_mat) <- pattern_names
      colnames(dist_mat) <- expert_names
      rownames(sim_mat)  <- pattern_names
      colnames(sim_mat)  <- expert_names
      
      for (j in seq_len(n_exp)) {
        exp_vec <- expert_bin[j, ]
        dist_mat[, j] <- apply(pattern_mat, 1, function(p) hamming_dist(p, exp_vec))
        sim_mat[, j]  <- apply(pattern_mat, 1, function(p) jaccard_sim(p, exp_vec))
      }
      
      write.csv(
        dist_mat,
        file.path(ref_dir, "dist_matrix_pattern_x_archetype.csv"),
        row.names = TRUE
      )
      
      write.csv(
        sim_mat,
        file.path(ref_dir, "jaccard_matrix_pattern_x_archetype.csv"),
        row.names = TRUE
      )
      
      individual_cov <- sapply(seq_len(n_exp), function(j) {
        covered_j <- dist_mat[, j] <= D_order
        sum(freqs[covered_j], na.rm = TRUE)
      })
      
      order_df <- data.frame(
        order_rank = seq_len(n_exp),
        archetype = expert_names,
        freq_covered_at_D_order = individual_cov,
        pct_covered_at_D_order = if (total_freq == 0) 0 else 100 * individual_cov / total_freq,
        stringsAsFactors = FALSE
      ) %>%
        arrange(desc(freq_covered_at_D_order), archetype) %>%
        mutate(order_rank = row_number())
      
      write.csv(
        order_df,
        file.path(ref_dir, paste0("archetype_order_by_individual_coverage_D", D_order, ".csv")),
        row.names = FALSE
      )
      
      ordered_idx <- match(order_df$archetype, colnames(dist_mat))
      dist_mat_ord <- dist_mat[, ordered_idx, drop = FALSE]
      sim_mat_ord  <- sim_mat[, ordered_idx, drop = FALSE]
      ordered_names <- colnames(dist_mat_ord)
      
      k_map_df <- data.frame(
        K = seq_len(n_exp),
        archetypes_used = sapply(seq_len(n_exp), function(k) paste(ordered_names[1:k], collapse = " | ")),
        stringsAsFactors = FALSE
      )
      
      write.csv(
        k_map_df,
        file.path(ref_dir, paste0("K_meaning_order_D", D_order, ".csv")),
        row.names = FALSE
      )
      
      for (K in seq_len(n_exp)) {
        
        dist_sub <- dist_mat_ord[, 1:K, drop = FALSE]
        sim_sub  <- sim_mat_ord[, 1:K, drop = FALSE]
        arch_sub_names <- ordered_names[1:K]
        
        best_idx_k  <- apply(dist_sub, 1, which.min)
        best_dist_k <- apply(dist_sub, 1, min)
        best_arch_k <- arch_sub_names[best_idx_k]
        best_sim_k  <- sim_sub[cbind(seq_len(nrow(sim_sub)), best_idx_k)]
        
        pattern_summary_k <- data.frame(
          pattern = pattern_names,
          freq = freqs,
          best_archetype_within_K = best_arch_k,
          best_dist_within_K = best_dist_k,
          best_sim_jaccard_within_K = best_sim_k,
          stringsAsFactors = FALSE
        )
        
        write.csv(
          pattern_summary_k,
          file.path(ref_dir, paste0("patterns_ranked_by_freq_best_archetype_K", K, ".csv")),
          row.names = FALSE
        )
        
        for (D in D_values) {
          covered <- best_dist_k <= D
          
          freq_cov <- sum(freqs[covered], na.rm = TRUE)
          pct_cov  <- if (total_freq == 0) 0 else 100 * freq_cov / total_freq
          
          all_results[[length(all_results) + 1]] <- data.frame(
            scenario = sc,
            type = tp,
            reference = ref_name,
            K = K,
            D = D,
            D_order = D_order,
            archetypes_used = paste(arch_sub_names, collapse = " | "),
            n_patterns_covered = sum(covered, na.rm = TRUE),
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

write.csv(
  results_df,
  file.path(out_root, paste0("ALL_RESULTS_ORDER_D", D_order, "_THEN_CUMULATIVE.csv")),
  row.names = FALSE
)

for (tp in types) {
  for (ref_name in names(expert_files)) {
    if (any(results_df$type == tp & results_df$reference == ref_name)) {
      make_pdf(
        results_df,
        tp,
        ref_name,
        file.path(out_root, paste0("order_D", D_order, "_then_cumulative_", tp, "_", ref_name, ".pdf")),
        D_values,
        D_order
      )
    }
  }
}

out_dir_by_scenario <- file.path(out_root, "by_scenario")
dir.create(out_dir_by_scenario, recursive = TRUE, showWarnings = FALSE)

for (tp in types) {
  for (sc in scenarios) {
    for (ref_name in names(expert_files)) {
      outfile_single <- file.path(
        out_dir_by_scenario,
        paste0("cumulative_", tp, "_", sc, "_", ref_name, ".pdf")
      )
      
      make_cumulative_pdf_by_scenario(
        results_df,
        tp,
        sc,
        ref_name,
        outfile_single,
        D_values,
        D_order
      )
      
      cat("OK acumulativo escenario:", outfile_single, "\n")
    }
  }
}

cat("finn\n")