library(data.table)
library(dplyr)
library(ggplot2)
library(patchwork)

##### individual --> medir la capacidad explicativa individual de cada arquetipo

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

make_combined_individual_pdf <- function(df, type_value, scenario_value, outfile) {
  
  refs <- c("experts", "kmeans", "sins")
  
  plots <- lapply(refs, function(ref_name) {
    
    df_ref <- df %>%
      filter(type == type_value, scenario == scenario_value, reference == ref_name)
    
    ggplot(df_ref, aes(x = D, y = pct_covered, color = archetype, group = archetype)) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.2) +
      scale_x_continuous(breaks = sort(unique(df_ref$D))) +
      scale_y_continuous(
        breaks = seq(0, 100, by = 10),
        limits = c(0, 100),
        labels = function(x) paste0(x, "%")
      ) +
      labs(
        title = ref_name,
        x = "D",
        y = "% cubierto"
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none")
  })
  
  empty_plot <- ggplot() + theme_void()
  
  combined <- (plots[[1]] | plots[[2]]) /
    (plots[[3]] | empty_plot)
  
  ggsave(outfile, combined, width = 12, height = 10)
}

make_pdf <- function(df, type_value, ref_value, outfile, D_values) {
  df_sub <- df %>%
    filter(type == type_value, reference == ref_value) %>%
    mutate(
      scenario = factor(scenario, levels = c("100-32", "10-32"))
    )
  
  p <- ggplot(df_sub, aes(x = D, y = pct_covered, color = archetype, group = archetype)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.2) +
    facet_wrap(~ scenario, ncol = 2, scales = "fixed") +
    scale_x_continuous(breaks = D_values) +
    scale_y_continuous(
      breaks = seq(0, 100, by = 10),
      limits = c(0, 100),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = paste("Cobertura individual por arquetipo -", toupper(type_value), "-", ref_value),
      x = "D (margen de distancia de Hamming)",
      y = "% frecuencia cubierta",
      color = "Arquetipo"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
  
  ggsave(outfile, plot = p, width = 12, height = 8.5)
}

make_individual_pdf_by_scenario <- function(df, type_value, scenario_value, ref_value, outfile) {
  df_sub <- df %>%
    filter(type == type_value, scenario == scenario_value, reference == ref_value)
  
  p <- ggplot(df_sub, aes(x = D, y = pct_covered, color = archetype, group = archetype)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.2) +
    scale_x_continuous(breaks = sort(unique(df_sub$D))) +
    scale_y_continuous(
      breaks = seq(0, 100, by = 10),
      limits = c(0, 100),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = paste(ref_value, "-", type_value, "-", scenario_value),
      x = "D",
      y = "% cubierto",
      color = "Arquetipo"
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
  
  ggsave(outfile, p, width = 8, height = 6)
}

base_dir <- "~/Desktop/master/PFM_extra/1000000"
out_root <- "results/results_each_archetype_separate"

if (!dir.exists(out_root)) dir.create(out_root, recursive = TRUE)

scenarios <- c("100-32", "10-32") # añadir cuando eso lo de 9
types <- c("pos", "ext")
D_values <- seq(0, 10, by = 2)

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
      
      archetype_summary <- lapply(seq_len(n_exp), function(j) {
        data.frame(
          pattern = pattern_names,
          freq = freqs,
          archetype = expert_names[j],
          dist = dist_mat[, j],
          sim_jaccard = sim_mat[, j],
          stringsAsFactors = FALSE
        )
      }) %>% bind_rows()
      
      write.csv(
        archetype_summary,
        file.path(ref_dir, "patterns_by_archetype_individual.csv"),
        row.names = FALSE
      )
      
      for (j in seq_len(n_exp)) {
        arch_name <- expert_names[j]
        dist_j <- dist_mat[, j]
        
        for (D in D_values) {
          covered <- dist_j <= D
          freq_cov <- sum(freqs[covered], na.rm = TRUE)
          pct_cov <- if (total_freq == 0) 0 else 100 * freq_cov / total_freq
          
          all_results[[length(all_results) + 1]] <- data.frame(
            scenario = sc,
            type = tp,
            reference = ref_name,
            archetype = arch_name,
            D = D,
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
  file.path(out_root, "ALL_RESULTS_EACH_ARCHETYPE_SEPARATE.csv"),
  row.names = FALSE
)

# PDFs como hasta ahora
for (tp in types) {
  for (ref_name in names(expert_files)) {
    if (any(results_df$type == tp & results_df$reference == ref_name)) {
      make_pdf(
        results_df,
        tp,
        ref_name,
        file.path(out_root, paste0("each_archetype_separate_", tp, "_", ref_name, ".pdf")),
        D_values
      )
    }
  }
}

out_dir_combined <- "results/combined_individuals"
dir.create(out_dir_combined, recursive = TRUE, showWarnings = FALSE)

out_dir_by_scenario <- file.path(out_root, "by_scenario")
dir.create(out_dir_by_scenario, recursive = TRUE, showWarnings = FALSE)

for (tp in types) {
  for (sc in scenarios) {
    
    # combinado 2x2 con experts/kmeans/sins
    outfile_comb <- file.path(out_dir_combined, paste0("individual_", tp, "_", sc, ".pdf"))
    make_combined_individual_pdf(results_df, tp, sc, outfile_comb)
    cat("OK combinado:", outfile_comb, "\n")
    
    # individuales por escenario
    for (ref_name in names(expert_files)) {
      outfile_single <- file.path(out_dir_by_scenario, paste0("individual_", tp, "_", sc, "_", ref_name, ".pdf"))
      make_individual_pdf_by_scenario(results_df, tp, sc, ref_name, outfile_single)
      cat("OK individual escenario:", outfile_single, "\n")
    }
  }
}

cat("fin\n")