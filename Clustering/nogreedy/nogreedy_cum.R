library(data.table)
library(dplyr)
library(ggplot2)

norm_txt <- function(x) {
  x <- tolower(as.character(x))
  x <- trimws(x)
  x <- gsub("[[:punct:]]", "", x)
  x <- gsub("[[:space:]]+", "", x)
  x
}

alias_map <- c(
  "cozyness" = "coziness",
  "brag"     = "recognition",
  "poseur"   = "approval"
)

apply_alias <- function(x_norm) {
  x_out <- x_norm
  hit <- x_norm %in% names(alias_map)
  x_out[hit] <- alias_map[x_norm[hit]]
  x_out
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
  rownames(mat) <- paste0("pattern_", seq_len(nrow(mat)))
  
  list(mat = mat, freq = freq, df = df)
}

expert_csv_to_binary <- function(expert_csv, det_names) {
  expert_df <- read.csv(expert_csv, stringsAsFactors = FALSE, check.names = FALSE)
  
  drop_cols <- grepl("^Unnamed", names(expert_df), ignore.case = TRUE) |
    names(expert_df) %in% c("", "X", "...1")
  
  if (any(drop_cols)) {
    expert_df <- expert_df[, !drop_cols, drop = FALSE]
  }
  
  if (ncol(expert_df) > 0) {
    first_col <- expert_df[[1]]
    suppressWarnings(num_col <- as.numeric(first_col))
    if (all(!is.na(num_col)) && all(num_col == seq_along(num_col))) {
      expert_df <- expert_df[, -1, drop = FALSE]
    }
  }
  
  names(expert_df)[names(expert_df) == "8"] <- "Cluster8"
  
  expert_df <- expert_df[, colSums(!is.na(expert_df) & expert_df != "") > 0, drop = FALSE]
  
  det_norm <- norm_txt(det_names)
  
  expert_bin <- sapply(seq_len(ncol(expert_df)), function(j) {
    vals <- expert_df[[j]]
    vals <- vals[!is.na(vals) & vals != ""]
    vals_norm <- apply_alias(norm_txt(vals))
    as.integer(det_norm %in% vals_norm)
  })
  
  expert_bin <- as.matrix(expert_bin)
  
  if (nrow(expert_bin) == length(det_names) && ncol(expert_bin) == ncol(expert_df)) {
    expert_bin <- t(expert_bin)
  }
  
  colnames(expert_bin) <- det_names
  
  expert_names <- names(expert_df)
  expert_names[is.na(expert_names) | expert_names == ""] <- paste0("expert_", seq_len(ncol(expert_df)))
  rownames(expert_bin) <- expert_names
  
  expert_bin
}

make_pdf <- function(df, type_value, ref_value, outfile, D_values, D_order) {
  df_sub <- df %>%
    filter(type == type_value, reference == ref_value) %>%
    mutate(
      scenario = factor(scenario, levels = c("100-32", "10-32")),
      legend_lab = paste0("K=", K, ": ", archetypes_used)
    )
  
  p <- ggplot(df_sub, aes(x = D, y = pct_covered, color = legend_lab, group = legend_lab)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.3) +
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
      color = "Arquetipos usados"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
  
  ggsave(outfile, plot = p, width = 12, height = 8.5)
}

make_cumulative_pdf_by_scenario <- function(df, type_value, scenario_value, ref_value, outfile, D_values, D_order) {
  df_sub <- df %>%
    filter(type == type_value, scenario == scenario_value, reference == ref_value) %>%
    mutate(
      legend_lab = paste0("K=", K, ": ", archetypes_used)
    )
  
  p <- ggplot(df_sub, aes(x = D, y = pct_covered, color = legend_lab, group = legend_lab)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.3) +
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
      color = "Arquetipos usados"
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
  
  ggsave(outfile, p, width = 8, height = 6)
}

base_dir <- "~/Desktop/master/PFM_extra/1000000"
out_root <- "results/results_no_greedy_from_freq"

if (!dir.exists(out_root)) dir.create(out_root, recursive = TRUE)

scenarios <- c("100-32", "10-32")
types <- c("pos", "ext")
D_values <- seq(0, 10, by = 2)
D_order <- 4

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
    pattern_names <- rownames(pattern_mat)
    
    ord_pat <- order(freqs, decreasing = TRUE)
    pattern_mat <- pattern_mat[ord_pat, , drop = FALSE]
    freqs <- freqs[ord_pat]
    pattern_names <- pattern_names[ord_pat]
    
    for (ref_name in names(expert_files)) {
      
      expert_file <- expert_files[[ref_name]]
      if (!file.exists(expert_file)) next
      
      expert_bin <- expert_csv_to_binary(expert_file, colnames(pattern_mat))
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
      
      # ORDEN INDIVIDUAL EN D = 4
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
      
      # ACUMULATIVO CON ESE ORDEN
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

# PDFs como hasta ahora (con dos escenarios juntos)
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

# NUEVO: PDFs por escenario
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