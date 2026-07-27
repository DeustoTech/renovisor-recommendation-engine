library(data.table)
library(dplyr)
library(clue)
library(ggplot2)
library(reshape2)

####### greedy y solo con k=8.

# 1. func
norm_txt <- function(x) {
  x <- tolower(x)
  x <- gsub("[[:punct:]]", "", x)
  x <- gsub("[[:space:]]+", "", x)
  x
}

# Lee freq_cluster_det_pos/ext.csv
# Devuelve:
# - mat: patrones binarios
# - freq: frecuencia N
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

# distancia de Hamming entre patrones binarios
hamming_dist <- function(x, y) sum(x != y)

# encuentra patrones similares a un centro con distancia <= D
find_similar <- function(center, keys_mat, freqs, used, D) {
  free_idx <- which(!used)
  if (length(free_idx) == 0) {
    return(list(indices = integer(0), score = 0))
  }
  
  dists <- apply(keys_mat[free_idx, , drop = FALSE], 1, function(y) hamming_dist(center, y))
  sel <- free_idx[dists <= D]
  
  list(indices = sel, score = sum(freqs[sel]))
}

# Greedy clustering !!!!!!!!!!!!!!!
# construye hasta K clusters escogiendo como centro el patrón libre más frecuente
greedy_clusters <- function(keys_mat, freqs, K, D) {
  n <- nrow(keys_mat)
  used <- rep(FALSE, n)
  clusters <- vector("list", K)
  scores <- numeric(K)
  
  for (i in 1:K) {
    if (all(used)) break
    
    candidates <- which(!used)
    center_idx <- candidates[which.max(freqs[candidates])]
    center <- keys_mat[center_idx, ]
    
    sim <- find_similar(center, keys_mat, freqs, used, D)
    used[sim$indices] <- TRUE
    
    clusters[[i]] <- list(
      center_index = center_idx,
      center = center,
      members = sim$indices
    )
    scores[i] <- sim$score
  }
  
  list(clusters = clusters, scores = scores, used = used)
}

# convierte CSV de expertos a matriz binaria comparable
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

# cobertura cluster vs experto
compute_coverage_binary <- function(cluster_mat, expert_bin, cluster_names = NULL, expert_names = NULL, cluster_dets = NULL) {
  n_clusters <- nrow(cluster_mat)
  n_experts  <- nrow(expert_bin)
  
  if (is.null(cluster_names)) cluster_names <- rownames(cluster_mat)
  if (is.null(expert_names)) expert_names <- rownames(expert_bin)
  if (is.null(cluster_dets)) cluster_dets <- colnames(cluster_mat)
  
  coverage_list <- list()
  
  for (i in 1:n_clusters) {
    cluster_row <- cluster_mat[i, ] > 0
    vol_cluster <- sum(cluster_row)
    active_dets <- cluster_dets[cluster_row]
    
    for (j in 1:n_experts) {
      expert_row <- expert_bin[j, ] > 0
      vol_expert <- sum(expert_row)
      expert_dets <- cluster_dets[expert_row]
      
      common_dets <- intersect(active_dets, expert_dets)
      vol_common <- length(common_dets)
      
      coverage_list[[length(coverage_list) + 1]] <- data.frame(
        cluster = cluster_names[i],
        expert = expert_names[j],
        vol_cluster = vol_cluster,
        vol_expert = vol_expert,
        vol_common = vol_common,
        pct_cluster_covered = ifelse(vol_cluster == 0, 0, 100 * vol_common / vol_cluster),
        pct_expert_covered  = ifelse(vol_expert == 0, 0, 100 * vol_common / vol_expert),
        jaccard = jaccard_similarity(cluster_row, expert_row),
        stringsAsFactors = FALSE
      )
    }
  }
  
  do.call(rbind, coverage_list)
}

# matriz cluster x experto de similitud Jaccard
build_similarity_matrix <- function(cluster_mat, expert_bin) {
  n_clusters <- nrow(cluster_mat)
  n_experts  <- nrow(expert_bin)
  
  sim_mat <- matrix(0, n_clusters, n_experts)
  rownames(sim_mat) <- rownames(cluster_mat)
  colnames(sim_mat) <- rownames(expert_bin)
  
  for (i in 1:n_clusters) {
    for (j in 1:n_experts) {
      sim_mat[i, j] <- jaccard_similarity(cluster_mat[i, ], expert_bin[j, ])
    }
  }
  
  sim_mat
}

# matching exclusivo 1-1 maximizando suma total
assign_clusters_exclusive <- function(sim_mat, cluster_names, min_pct = 0) {
  n_clusters <- nrow(sim_mat)
  n_experts  <- ncol(sim_mat)
  
  if (n_clusters > n_experts) {
    n_add <- n_clusters - n_experts
    sim_mat <- cbind(sim_mat, matrix(0, n_clusters, n_add))
    colnames(sim_mat)[(n_experts + 1):ncol(sim_mat)] <- paste0("None_", 1:n_add)
  }
  
  assignment <- solve_LSAP(sim_mat, maximum = TRUE)
  
  results <- data.frame(
    cluster = cluster_names,
    best_match = NA_character_,
    pct_match = NA_real_,
    stringsAsFactors = FALSE
  )
  
  for (i in 1:n_clusters) {
    expert_idx  <- assignment[i]
    expert_name <- colnames(sim_mat)[expert_idx]
    pct <- sim_mat[i, expert_idx]
    
    if (!grepl("^None_", expert_name) && pct >= min_pct) {
      results$best_match[i] <- expert_name
      results$pct_match[i]  <- pct
    }
  }
  
  results
}


# 2. PARÁMETROS
base_dir <- "~/Desktop/master/PFM_extra/100000"
out_root <- "results_greedy_from_freq"

if (!dir.exists(out_root)) dir.create(out_root, recursive = TRUE)

scenarios <- c("100-32", "10-32", "100-9", "10-9")
types <- c("pos", "ext")

K <- 8
D_values <- seq(2, 20, by = 2)

expert_files <- list(
  experts = "data/archetypeExperts.csv"
  # kmeans = "data/archetypeKmeans.csv",
  # sins   = "data/archetypeSINS.csv"
)

# 3. bucl pr
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
    
    # leer patrones binarios + frecuencia
    cl_obj <- read_freq_clusters(freq_file)
    keys_mat <- cl_obj$mat
    freqs <- cl_obj$freq
    det_names <- colnames(keys_mat)
    n_arch <- nrow(keys_mat)
    

    # Greedy para D
    results_D <- data.frame(
      D = integer(),
      assigned_pct = numeric(),
      unassigned_pct = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (D in D_values) {
      cat("  D =", D, "\n")
      
      res <- greedy_clusters(keys_mat, freqs, K, D)
      dir_D <- file.path(case_dir, paste0("D_", D))
      if (!dir.exists(dir_D)) dir.create(dir_D, recursive = TRUE)
      
      # guardar centros greedy
      centers_df <- do.call(rbind, lapply(seq_along(res$clusters), function(i) {
        cl <- res$clusters[[i]]
        if (is.null(cl$center) || length(cl$center) == 0) return(NULL)
        cbind(cluster = i, as.data.frame(as.list(cl$center)))
      }))
      
      if (!is.null(centers_df)) {
        write.csv(centers_df, file.path(dir_D, "greedy_cluster_centers.csv"), row.names = FALSE)
      }
      
      # guardar asignaciones
      assignments_df <- do.call(rbind, lapply(seq_along(res$clusters), function(i) {
        cl <- res$clusters[[i]]
        if (is.null(cl) || length(cl$members) == 0) return(NULL)
        data.frame(
          pattern_id = cl$members,
          cluster = i
        )
      }))
      
      if (!is.null(assignments_df)) {
        write.csv(assignments_df, file.path(dir_D, "greedy_cluster_assignments.csv"), row.names = FALSE)
      }
      
      # Resumen de cobertura de patrones
      total_arch <- n_arch
      cumulative_pct <- 0
      summary_list <- list()
      
      for (i in seq_along(res$clusters)) {
        cl <- res$clusters[[i]]
        if (is.null(cl) || length(cl$members) == 0) next
        
        n_cluster <- length(cl$members)
        pct_total <- n_cluster / total_arch * 100
        pct_rest  <- 100 - cumulative_pct
        cumulative_pct <- cumulative_pct + pct_total
        
        summary_list[[length(summary_list) + 1]] <- data.frame(
          cluster = i,
          n_patterns = n_cluster,
          pct_total_patterns = pct_total,
          pct_rest_patterns = pct_rest
        )
      }
      
      summary_df <- if (length(summary_list) > 0) do.call(rbind, summary_list) else NULL
      
      n_rest <- n_arch - if (!is.null(summary_df)) sum(summary_df$n_patterns) else n_arch
      if (n_rest > 0) {
        pct_total_rest <- n_rest / n_arch * 100
        rest_df <- data.frame(
          cluster = "Restantes",
          n_patterns = n_rest,
          pct_total_patterns = pct_total_rest,
          pct_rest_patterns = 0
        )
        summary_df <- if (is.null(summary_df)) rest_df else rbind(summary_df, rest_df)
      }
      
      if (!is.null(summary_df)) {
        write.csv(summary_df, file.path(dir_D, "greedy_cluster_summary_by_patterns.csv"), row.names = FALSE)
      }
      
      # no asignados
      unassigned_df <- data.frame(pattern_id = which(!res$used))
      write.csv(unassigned_df, file.path(dir_D, "greedy_unassigned_patterns.csv"), row.names = FALSE)
      
      # porcentaje asignados/no asignados
      assigned <- sum(res$used)
      unassigned <- n_arch - assigned
      
      results_D <- rbind(results_D, data.frame(
        D = D,
        assigned_pct = assigned / n_arch * 100,
        unassigned_pct = unassigned / n_arch * 100
      ))
      
      # PASO 2: COMPARAR LOS 8 REPRESENTANTES CON EXPERTOS
      if (!is.null(centers_df)) {
        cluster_mat <- as.matrix(centers_df[, -1, drop = FALSE])
        rownames(cluster_mat) <- paste0("cluster_", centers_df$cluster)
        colnames(cluster_mat) <- det_names
        
        # guardar determinantes activos de cada centro
        active_dets <- apply(cluster_mat, 1, function(x) {
          dets <- colnames(cluster_mat)[x > 0]
          paste(dets, collapse = "; ")
        })
        
        centers_active_df <- data.frame(
          cluster = rownames(cluster_mat),
          n_active_dets = rowSums(cluster_mat > 0),
          active_determinants = active_dets,
          stringsAsFactors = FALSE
        )
        write.csv(centers_active_df, file.path(dir_D, "greedy_cluster_centers_active_determinants.csv"), row.names = FALSE)
        
        # comparar con cada referencia
        for (ref_name in names(expert_files)) {
          expert_file <- expert_files[[ref_name]]
          if (!file.exists(expert_file)) next
          
          expert_bin <- expert_csv_to_binary(expert_file, colnames(cluster_mat))
          write.csv(expert_bin, file.path(dir_D, paste0("experts_binary_", ref_name, ".csv")), row.names = TRUE)
          
          # cobertura
          coverage_df <- compute_coverage_binary(cluster_mat, expert_bin, cluster_dets = colnames(cluster_mat))
          write.csv(coverage_df, file.path(dir_D, paste0("coverage_", ref_name, "_D", D, ".csv")), row.names = FALSE)
          
          # similitud
          sim_mat <- build_similarity_matrix(cluster_mat, expert_bin)
          sim_df <- as.data.frame(sim_mat)
          sim_df <- cbind(cluster = rownames(sim_mat), sim_df)
          write.csv(sim_df, file.path(dir_D, paste0("similarity_matrix_", ref_name, "_D", D, ".csv")), row.names = FALSE)
          
          # matching exclusivo 1-1
          res_excl <- assign_clusters_exclusive(sim_mat, rownames(cluster_mat), min_pct = 0)
          
          # añadir determinantes activos para interpretar
          res_excl$active_determinants <- active_dets[res_excl$cluster]
          res_excl$n_active_dets <- rowSums(cluster_mat > 0)[res_excl$cluster]
          
          write.csv(res_excl, file.path(dir_D, paste0("cluster_vs_", ref_name, "_exclusive_D", D, ".csv")), row.names = FALSE)
          
          # resumen global simple por D y referencia
          matched_ok <- sum(!is.na(res_excl$best_match))
          mean_match <- mean(res_excl$pct_match, na.rm = TRUE)
          
          global_results[[paste(sc, tp, ref_name, D, sep = "_")]] <- data.frame(
            scenario = sc,
            type = tp,
            reference = ref_name,
            D = D,
            n_clusters = nrow(cluster_mat),
            n_matched = matched_ok,
            pct_matched = 100 * matched_ok / nrow(cluster_mat),
            mean_jaccard_exclusive = mean_match,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    

    # guardar curva assigned/unassigned vs D
    write.csv(results_D, file.path(case_dir, "assigned_vs_D.csv"), row.names = FALSE)
    
    results_long <- melt(results_D, id.vars = "D", variable.name = "Type", value.name = "Percent")
    results_long$Type <- factor(
      results_long$Type,
      levels = c("assigned_pct", "unassigned_pct"),
      labels = c("Asignados", "No asignados")
    )
    results_long$D <- factor(results_long$D)
    
    p <- ggplot(results_long, aes(x = D, y = Percent, color = Type, group = Type)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_y_continuous(
        breaks = seq(0, 100, by = 5),
        labels = function(x) paste0(round(x, 1), "%"),
        limits = c(0, 105)
      ) +
      geom_hline(yintercept = seq(0, 100, by = 5), color = "grey90", linetype = "dashed") +
      labs(
        title = paste("Asignación de patrones vs D -", sc, tp),
        x = "Distancia máxima D",
        y = "Porcentaje de patrones",
        color = "Tipo"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
    
    ggsave(file.path(case_dir, "assigned_vs_D_pct_lines.png"), plot = p, width = 8, height = 5)
  }
}

# 4. tabla final
if (length(global_results) > 0) {
  global_table <- bind_rows(global_results) %>%
    arrange(reference, scenario, type, D)
  
  write.csv(global_table, file.path(out_root, "GLOBAL_GREEDY_COMPARISON.csv"), row.names = FALSE)
}

cat("fin\n")