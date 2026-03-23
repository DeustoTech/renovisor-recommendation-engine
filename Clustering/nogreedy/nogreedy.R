library(data.table)
library(dplyr)

# 1. fun
norm_txt <- function(x) {
  x <- tolower(x)
  x <- gsub("[[:punct:]]", "", x)
  x <- gsub("[[:space:]]+", "", x)
  x
}

# Lee freq_cluster_det_pos/ext.csv
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

# Convierte expertos a binario
expert_csv_to_binary <- function(expert_csv, cluster_dets) {
  expert_df <- read.csv(expert_csv, stringsAsFactors = FALSE, check.names = FALSE)
  expert_df <- expert_df[, colSums(!is.na(expert_df) & expert_df != "") > 0, drop = FALSE]
  
  expert_mat <- t(as.matrix(expert_df))
  dets_norm <- norm_txt(cluster_dets)
  
  expert_bin <- t(apply(expert_mat, 1, function(row) {
    row_norm <- norm_txt(row)
    sapply(dets_norm, function(det) as.integer(det %in% row_norm))
  }))
  
  colnames(expert_bin) <- cluster_dets
  rownames(expert_bin) <- rownames(expert_mat)
  expert_bin
}

jaccard_similarity <- function(a, b) {
  a <- a > 0
  b <- b > 0
  inter <- sum(a & b)
  uni   <- sum(a | b)
  if (uni == 0) return(0)
  100 * inter / uni
}

# Matriz de similitud patrón x experto
build_similarity_matrix <- function(cluster_mat, expert_bin) {
  sim_mat <- matrix(0, nrow(cluster_mat), nrow(expert_bin))
  rownames(sim_mat) <- rownames(cluster_mat)
  colnames(sim_mat) <- rownames(expert_bin)
  
  for (i in seq_len(nrow(cluster_mat))) {
    for (j in seq_len(nrow(expert_bin))) {
      sim_mat[i, j] <- jaccard_similarity(cluster_mat[i, ], expert_bin[j, ])
    }
  }
  
  sim_mat
}


# 2. MATCHING RECURSIVO SIN GREEDY
# criterion:
# - "jaccard"      -> elige el par con mayor Jaccard
# - "freq_jaccard" -> elige el par con mayor freq * Jaccard

recursive_best_matching <- function(cluster_mat, cluster_freq, expert_bin, criterion = "freq_jaccard") {
  
  remaining_patterns <- rownames(cluster_mat)
  remaining_experts  <- rownames(expert_bin)
  
  matches <- list()
  step <- 1
  
  while (length(remaining_patterns) > 0 && length(remaining_experts) > 0) {
    
    sub_mat <- cluster_mat[remaining_patterns, , drop = FALSE]
    sub_exp <- expert_bin[remaining_experts, , drop = FALSE]
    sub_freq <- cluster_freq[remaining_patterns]
    
    sim_mat <- build_similarity_matrix(sub_mat, sub_exp)
    
    # construir tabla de todos los pares restantes
    pair_list <- list()
    k <- 1
    
    for (i in seq_len(nrow(sim_mat))) {
      for (j in seq_len(ncol(sim_mat))) {
        patt <- rownames(sim_mat)[i]
        expn <- colnames(sim_mat)[j]
        jac  <- sim_mat[i, j]
        frq  <- sub_freq[patt]
        
        score <- if (criterion == "freq_jaccard") frq * jac else jac
        
        pair_list[[k]] <- data.frame(
          step = step,
          pattern = patt,
          expert = expn,
          freq = frq,
          jaccard = jac,
          score = score,
          stringsAsFactors = FALSE
        )
        k <- k + 1
      }
    }
    
    pair_df <- bind_rows(pair_list)
    
    # elegir el mejor par global
    best_idx <- which.max(pair_df$score)
    best_pair <- pair_df[best_idx, ]
    
    matches[[step]] <- best_pair
    
    # quitar patrón y experto usados
    remaining_patterns <- setdiff(remaining_patterns, best_pair$pattern)
    remaining_experts  <- setdiff(remaining_experts, best_pair$expert)
    
    step <- step + 1
  }
  
  bind_rows(matches)
}

# 3. DETALLES INTERPRETABLES
active_determinants_from_mat <- function(cluster_mat) {
  sapply(seq_len(nrow(cluster_mat)), function(i) {
    dets <- colnames(cluster_mat)[cluster_mat[i, ] > 0]
    paste(dets, collapse = "; ")
  })
}

# Para guardar tabla de todos los pares patrón-experto
all_pairs_table <- function(cluster_mat, cluster_freq, expert_bin) {
  sim_mat <- build_similarity_matrix(cluster_mat, expert_bin)
  
  out <- list()
  k <- 1
  for (i in seq_len(nrow(sim_mat))) {
    for (j in seq_len(ncol(sim_mat))) {
      patt <- rownames(sim_mat)[i]
      expn <- colnames(sim_mat)[j]
      jac  <- sim_mat[i, j]
      frq  <- cluster_freq[patt]
      
      out[[k]] <- data.frame(
        pattern = patt,
        expert = expn,
        freq = frq,
        jaccard = jac,
        freq_jaccard = frq * jac,
        stringsAsFactors = FALSE
      )
      k <- k + 1
    }
  }
  bind_rows(out) %>% arrange(desc(freq_jaccard), desc(jaccard), desc(freq))
}


# 4. PARÁMETROS
base_dir <- "~/Desktop/master/PFM_extra/100000"
out_root <- "results_recursive_no_greedy"

if (!dir.exists(out_root)) dir.create(out_root, recursive = TRUE)

scenarios <- c("100-32", "10-32", "100-9", "10-9")
types <- c("pos", "ext")

expert_files <- list(
  experts = "data/archetypeExperts.csv"
  # kmeans = "data/archetypeKmeans.csv",
  # sins   = "data/archetypeSINS.csv"
)

# "jaccard" o "freq_jaccard"
criterion_to_use <- "freq_jaccard"


# 5. BUCLE PRINCIPAL
global_results <- list()

for (sc in scenarios) {
  for (tp in types) {
    
    cat("Procesando:", sc, "-", tp, "\n")
    
    freq_file <- file.path(base_dir, sc, paste0("freq_cluster_det_", tp, ".csv"))
    case_dir  <- file.path(out_root, sc, tp)
    if (!dir.exists(case_dir)) dir.create(case_dir, recursive = TRUE)
    
    if (!file.exists(freq_file)) {
      cat("No existe:", freq_file, "\n")
      next
    }
    
    # leer patrones
    cl_obj <- read_freq_clusters(freq_file)
    cluster_mat <- cl_obj$mat
    cluster_freq <- cl_obj$freq
    names(cluster_freq) <- rownames(cluster_mat)
    
    # determinantes activos de cada patrón
    active_dets <- active_determinants_from_mat(cluster_mat)
    names(active_dets) <- rownames(cluster_mat)
    n_active <- rowSums(cluster_mat > 0)
    names(n_active) <- rownames(cluster_mat)
    
    for (ref_name in names(expert_files)) {
      expert_file <- expert_files[[ref_name]]
      if (!file.exists(expert_file)) next
      
      ref_dir <- file.path(case_dir, ref_name)
      if (!dir.exists(ref_dir)) dir.create(ref_dir, recursive = TRUE)
      
      # expertos binarios
      expert_bin <- expert_csv_to_binary(expert_file, colnames(cluster_mat))
      write.csv(expert_bin, file.path(ref_dir, "experts_binary.csv"), row.names = TRUE)
      
      # tabla de todos los pares
      pairs_df <- all_pairs_table(cluster_mat, cluster_freq, expert_bin)
      write.csv(pairs_df, file.path(ref_dir, "all_pattern_expert_pairs.csv"), row.names = FALSE)
      
      # matching recursivo
      recursive_df <- recursive_best_matching(
        cluster_mat = cluster_mat,
        cluster_freq = cluster_freq,
        expert_bin = expert_bin,
        criterion = criterion_to_use
      )
      
      recursive_df$active_determinants <- active_dets[recursive_df$pattern]
      recursive_df$n_active_dets <- n_active[recursive_df$pattern]
      
      write.csv(
        recursive_df,
        file.path(ref_dir, paste0("recursive_matching_", criterion_to_use, ".csv")),
        row.names = FALSE
      )
      
      # resumen simple
      summary_df <- data.frame(
        scenario = sc,
        type = tp,
        reference = ref_name,
        criterion = criterion_to_use,
        n_experts_total = nrow(expert_bin),
        n_matches = nrow(recursive_df),
        mean_jaccard = mean(recursive_df$jaccard, na.rm = TRUE),
        weighted_mean_jaccard = weighted.mean(recursive_df$jaccard, w = recursive_df$freq, na.rm = TRUE),
        sum_score = sum(recursive_df$score, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      
      write.csv(summary_df, file.path(ref_dir, "recursive_matching_summary.csv"), row.names = FALSE)
      global_results[[paste(sc, tp, ref_name, sep = "_")]] <- summary_df
    }
  }
}

# 6. TABLA GLOBAL FINAL
if (length(global_results) > 0) {
  global_table <- bind_rows(global_results) %>%
    arrange(reference, scenario, type)
  
  write.csv(global_table, file.path(out_root, "GLOBAL_RECURSIVE_NO_GREEDY.csv"), row.names = FALSE)
}

cat("fin\n")