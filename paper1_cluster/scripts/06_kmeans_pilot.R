
# Objetivo:
# Ejecutar K-means sobre las muestras bootstrap. SE HACE SOBRE LAS MUESTRAS DE BOOTSTRAP CON 
# 1000 MUESTRAS CON DETERMINANTES 04_2b!!!!!!!!!!
#
# Para cada bootstrap b(
# El 06 junta esas dos cosas.
# 
# Para cada bootstrap, hace:
#   
#   cojo los integrated_row_id de bootstrap_id = 1
# busco sus determinantes en la matriz del 05
# con esas 1000 filas hago K-means
# 
# Luego repite:
#   
#   bootstrap_id = 2
# bootstrap_id = 3
# ...
# bootstrap_id = 1000
# 
# Y además lo repite para varias matrices y varios K.):
#   para cada matriz:
#     para K = 4,...,8:
#       1. seleccionar las filas del bootstrap
#       2. unirlas con la matriz de determinantes por integrated_row_id
#       3. ejecutar K-means
#       4. guardar clusters, centroides, distancias y heatmaps
#
# preguntas a las que responde:
# A) ¿Los clusters son estables?
# B) ¿Qué K funciona mejor?
# C) ¿Qué transformación de matriz funciona mejor?



suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(stringr)
  library(ggplot2)
})

set.seed(123)


# Parámetros
project_root <- path.expand("~/Desktop/MASTER/recommendation-engine/TFM")
processed_root <- file.path(project_root, "paper1_cluster/data/processed")

# Escenario principal elegido:
# PFE + ESN fusionados
# GREENS_EFA + RENEW fusionados

# hecho SOLO con filas que SI tienen determinantes 
BOOTSTRAP_SCENARIO <- "04_2b_propensity_bootstrap_eu_pfe_esn_renew_greens_merged_clustering_usable"

bootstrap_file <- file.path(
  processed_root,
  BOOTSTRAP_SCENARIO,
  "bootstrap_samples_index.csv"
)

matrix_dir <- file.path(
  processed_root,
  "05_clustering_matrices"
)

out_dir <- file.path(
  processed_root,
  "06_kmeans_bootstrap"
)

fig_dir <- file.path(out_dir, "figures")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# Matrices de 32 determinantes
MATRICES_TO_RUN <- c(
  "matrix_32_raw_0_1",
  "matrix_32_pos_0_1",
  "matrix_32_ext_0_1",
  "matrix_32_z_abs"
)

# K = número de clusters / perfiles
K_GRID <- 4:8

# Parámetros de K-means
NSTART <- 25
ITER_MAX <- 100

# Para usar todos los bootstraps disponibles,  Inf
MAX_BOOTSTRAPS <- Inf
#MAX_BOOTSTRAPS <- 5
#MAX_BOOTSTRAPS <- 20
#MAX_BOOTSTRAPS <- 100



# Guardar asignaciones fila-cluster.
# Con 200 bootstraps:
# 200 x 1000 x 4 matrices x 5 K = aprox. 4 millones de filas.
#SAVE_ASSIGNMENTS <- TRUE
SAVE_ASSIGNMENTS <- FALSE

# Guardar heatmaps medios de centroides
SAVE_HEATMAPS <- TRUE

#  Leer bootstrap
if (!file.exists(bootstrap_file)) {
  stop("No encuentro el archivo bootstrap: ", bootstrap_file)
}

bootstrap_index <- read_csv(
  bootstrap_file,
  show_col_types = FALSE
)

if (!"bootstrap_id" %in% names(bootstrap_index)) {
  stop("El bootstrap_samples_index.csv no tiene columna bootstrap_id.")
}

if (!"integrated_row_id" %in% names(bootstrap_index)) {
  stop("El bootstrap_samples_index.csv no tiene columna integrated_row_id.")
}

if (!"draw_id" %in% names(bootstrap_index)) {
  bootstrap_index <- bootstrap_index %>%
    group_by(bootstrap_id) %>%
    mutate(draw_id = row_number()) %>%
    ungroup()
}

bootstrap_index <- bootstrap_index %>%
  mutate(
    bootstrap_id = as.integer(bootstrap_id),
    draw_id = as.integer(draw_id),
    integrated_row_id = as.character(integrated_row_id)
  )

boot_ids <- sort(unique(bootstrap_index$bootstrap_id))

if (is.finite(MAX_BOOTSTRAPS)) {
  boot_ids <- head(boot_ids, MAX_BOOTSTRAPS)
}

bootstrap_index <- bootstrap_index %>%
  filter(bootstrap_id %in% boot_ids)

cat("\n============================================================\n")
cat("BOOTSTRAP CARGADO\n")
cat("============================================================\n")
cat("Escenario bootstrap:", BOOTSTRAP_SCENARIO, "\n")
cat("N bootstraps usados:", length(boot_ids), "\n")
cat("N draws totales:", nrow(bootstrap_index), "\n")


# Funciones auxiliares
read_matrix_file <- function(matrix_name) {
  
  file <- file.path(matrix_dir, paste0(matrix_name, ".csv"))
  
  if (!file.exists(file)) {
    stop("No encuentro la matriz: ", file)
  }
  
  mat_df <- read_csv(
    file,
    show_col_types = FALSE
  ) %>%
    mutate(
      integrated_row_id = as.character(integrated_row_id)
    ) %>%
    distinct(integrated_row_id, .keep_all = TRUE)
  
  feature_cols <- names(mat_df)[str_detect(names(mat_df), "^det_\\d{2}_")]
  
  if (length(feature_cols) != 32) {
    stop(
      "La matriz ",
      matrix_name,
      " debería tener 32 determinantes y tiene ",
      length(feature_cols)
    )
  }
  
  mat_df <- mat_df %>%
    select(integrated_row_id, all_of(feature_cols)) %>%
    mutate(across(all_of(feature_cols), as.numeric))
  
  list(
    data = mat_df,
    feature_cols = feature_cols
  )
}

safe_kmeans <- function(x, k, nstart = 25, iter.max = 100) {
  tryCatch(
    kmeans(
      x = x,
      centers = k,
      nstart = nstart,
      iter.max = iter.max
    ),
    error = function(e) e
  )
}

make_distance_long <- function(centers_ordered, matrix_name, bootstrap_id, k) {
  
  feature_cols <- names(centers_ordered)[
    str_detect(names(centers_ordered), "^det_\\d{2}_")
  ]
  
  if (length(feature_cols) == 0) {
    return(
      tibble(
        matrix_name = character(),
        bootstrap_id = integer(),
        k = integer(),
        cluster_a = integer(),
        cluster_b = integer(),
        euclidean_distance = numeric()
      )
    )
  }
  
  center_matrix <- centers_ordered %>%
    arrange(cluster_rank) %>%
    select(all_of(feature_cols)) %>%
    as.matrix()
  
  dmat <- as.matrix(dist(center_matrix, method = "euclidean"))
  
  # Cogemos solo la parte superior de la matriz de distancias:
  # cluster 1 vs 2, 1 vs 3, 2 vs 3, etc.
  idx <- which(upper.tri(dmat), arr.ind = TRUE)
  
  out <- tibble(
    matrix_name = matrix_name,
    bootstrap_id = bootstrap_id,
    k = k,
    cluster_a = as.integer(idx[, "row"]),
    cluster_b = as.integer(idx[, "col"]),
    euclidean_distance = as.numeric(dmat[idx])
  )
  
  out
}

run_one_kmeans <- function(
    bootstrap_id_current,
    matrix_name_current,
    matrix_index_current,
    matrix_df,
    feature_cols,
    k_current
) {
  
  meta_cols <- c(
    "bootstrap_id",
    "draw_id",
    "integrated_row_id",
    "target_sample_type",
    "target_electoral_group",
    "dataset_source",
    "source_survey",
    "row_quality_final",
    "usable_for_clustering"
  )
  
  boot_b <- bootstrap_index %>%
    filter(bootstrap_id == bootstrap_id_current) %>%
    select(any_of(meta_cols))
  
  sample_df <- boot_b %>%
    left_join(matrix_df, by = "integrated_row_id")
  
  sample_use <- sample_df %>%
    filter(if_all(all_of(feature_cols), ~ !is.na(.x)))
  
  n_bootstrap_draws <- nrow(boot_b)
  n_rows_used <- nrow(sample_use)
  n_rows_dropped <- n_bootstrap_draws - n_rows_used
  prop_rows_used <- n_rows_used / n_bootstrap_draws
  
  if (n_rows_used < k_current) {
    
    metrics <- tibble(
      matrix_name = matrix_name_current,
      bootstrap_id = bootstrap_id_current,
      k = k_current,
      status = "skipped_too_few_rows",
      n_bootstrap_draws = n_bootstrap_draws,
      n_rows_used = n_rows_used,
      n_rows_dropped = n_rows_dropped,
      prop_rows_used = prop_rows_used,
      totss = NA_real_,
      tot_withinss = NA_real_,
      betweenss = NA_real_,
      between_over_total = NA_real_,
      mean_cluster_distance = NA_real_,
      min_cluster_distance = NA_real_,
      max_cluster_distance = NA_real_,
      iter = NA_integer_,
      ifault = NA_integer_,
      error_message = "Too few rows for K"
    )
    
    return(list(
      metrics = metrics,
      centers_long = tibble(),
      cluster_sizes = tibble(),
      assignments = tibble(),
      distances = tibble(),
      composition = tibble()
    ))
  }
  
  x <- sample_use %>%
    select(all_of(feature_cols)) %>%
    as.matrix()
  
  set.seed(
    1000000 +
      bootstrap_id_current * 1000 +
      matrix_index_current * 100 +
      k_current
  )
  
  km <- safe_kmeans(
    x = x,
    k = k_current,
    nstart = NSTART,
    iter.max = ITER_MAX
  )
  
  if (inherits(km, "error")) {
    
    metrics <- tibble(
      matrix_name = matrix_name_current,
      bootstrap_id = bootstrap_id_current,
      k = k_current,
      status = "error",
      n_bootstrap_draws = n_bootstrap_draws,
      n_rows_used = n_rows_used,
      n_rows_dropped = n_rows_dropped,
      prop_rows_used = prop_rows_used,
      totss = NA_real_,
      tot_withinss = NA_real_,
      betweenss = NA_real_,
      between_over_total = NA_real_,
      mean_cluster_distance = NA_real_,
      min_cluster_distance = NA_real_,
      max_cluster_distance = NA_real_,
      iter = NA_integer_,
      ifault = NA_integer_,
      error_message = km$message
    )
    
    return(list(
      metrics = metrics,
      centers_long = tibble(),
      cluster_sizes = tibble(),
      assignments = tibble(),
      distances = tibble(),
      composition = tibble()
    ))
  }
  
  centers_df <- as_tibble(km$centers, rownames = "cluster_original") %>%
    mutate(
      cluster_original = as.integer(cluster_original)
    )
  
  # Ordenamos los clusters por intensidad media del centroide.
  # Esto ayuda a comparar heatmaps entre bootstraps.
  # Nota: no resuelve perfectamente el label switching, pero es suficiente
  # para un primer análisis estable y legible.
  center_order <- centers_df %>%
    mutate(
      center_mean = rowMeans(
        as.matrix(across(all_of(feature_cols))),
        na.rm = TRUE
      )
    ) %>%
    arrange(desc(center_mean), cluster_original) %>%
    mutate(
      cluster_rank = row_number()
    ) %>%
    select(cluster_original, cluster_rank, center_mean)
  
  assignments <- tibble(
    matrix_name = matrix_name_current,
    bootstrap_id = bootstrap_id_current,
    k = k_current,
    draw_id = sample_use$draw_id,
    integrated_row_id = sample_use$integrated_row_id,
    target_sample_type = if ("target_sample_type" %in% names(sample_use)) sample_use$target_sample_type else NA_character_,
    target_electoral_group = if ("target_electoral_group" %in% names(sample_use)) sample_use$target_electoral_group else NA_character_,
    dataset_source = if ("dataset_source" %in% names(sample_use)) sample_use$dataset_source else NA_character_,
    row_quality_final = if ("row_quality_final" %in% names(sample_use)) sample_use$row_quality_final else NA_character_,
    cluster_original = as.integer(km$cluster)
  ) %>%
    left_join(center_order, by = "cluster_original") %>%
    select(
      matrix_name,
      bootstrap_id,
      k,
      draw_id,
      integrated_row_id,
      target_sample_type,
      target_electoral_group,
      dataset_source,
      row_quality_final,
      cluster_rank,
      cluster_original
    )
  
  cluster_sizes <- assignments %>%
    count(
      matrix_name,
      bootstrap_id,
      k,
      cluster_rank,
      name = "n_cluster"
    ) %>%
    complete(
      matrix_name,
      bootstrap_id,
      k,
      cluster_rank = seq_len(k_current),
      fill = list(n_cluster = 0)
    ) %>%
    group_by(matrix_name, bootstrap_id, k) %>%
    mutate(
      prop_cluster = n_cluster / sum(n_cluster)
    ) %>%
    ungroup()
  
  centers_ordered <- centers_df %>%
    left_join(center_order, by = "cluster_original") %>%
    mutate(
      matrix_name = matrix_name_current,
      bootstrap_id = bootstrap_id_current,
      k = k_current
    ) %>%
    select(
      matrix_name,
      bootstrap_id,
      k,
      cluster_rank,
      cluster_original,
      center_mean,
      all_of(feature_cols)
    )
  
  centers_long <- centers_ordered %>%
    pivot_longer(
      cols = all_of(feature_cols),
      names_to = "determinant",
      values_to = "center_value"
    )
  
  distances <- make_distance_long(
    centers_ordered = centers_ordered,
    matrix_name = matrix_name_current,
    bootstrap_id = bootstrap_id_current,
    k = k_current
  )
  
  composition <- assignments %>%
    mutate(
      target_sample_type = replace_na(target_sample_type, "NO_TARGET"),
      target_electoral_group = replace_na(target_electoral_group, "NO_TARGET")
    ) %>%
    count(
      matrix_name,
      bootstrap_id,
      k,
      cluster_rank,
      target_sample_type,
      target_electoral_group,
      name = "n"
    ) %>%
    group_by(matrix_name, bootstrap_id, k, cluster_rank) %>%
    mutate(
      prop_within_cluster = n / sum(n)
    ) %>%
    ungroup()
  
  metrics <- tibble(
    matrix_name = matrix_name_current,
    bootstrap_id = bootstrap_id_current,
    k = k_current,
    status = "ok",
    n_bootstrap_draws = n_bootstrap_draws,
    n_rows_used = n_rows_used,
    n_rows_dropped = n_rows_dropped,
    prop_rows_used = prop_rows_used,
    totss = km$totss,
    tot_withinss = km$tot.withinss,
    betweenss = km$betweenss,
    between_over_total = km$betweenss / km$totss,
    mean_cluster_distance = mean(distances$euclidean_distance, na.rm = TRUE),
    min_cluster_distance = min(distances$euclidean_distance, na.rm = TRUE),
    max_cluster_distance = max(distances$euclidean_distance, na.rm = TRUE),
    iter = km$iter,
    ifault = ifelse(is.null(km$ifault), NA_integer_, km$ifault),
    error_message = NA_character_
  )
  
  list(
    metrics = metrics,
    centers_long = centers_long,
    cluster_sizes = cluster_sizes,
    assignments = assignments,
    distances = distances,
    composition = composition
  )
}


# Ejecutar K-means sobre bootstraps
metrics_list <- list()
centers_list <- list()
sizes_list <- list()
assignments_list <- list()
distances_list <- list()
composition_list <- list()

run_counter <- 0

for (matrix_index in seq_along(MATRICES_TO_RUN)) {
  
  matrix_name <- MATRICES_TO_RUN[matrix_index]
  
  cat("\n============================================================\n")
  cat("Matriz:", matrix_name, "\n")
  cat("============================================================\n")
  
  matrix_obj <- read_matrix_file(matrix_name)
  matrix_df <- matrix_obj$data
  feature_cols <- matrix_obj$feature_cols
  
  for (k in K_GRID) {
    
    cat("  K =", k, "\n")
    
    for (b in boot_ids) {
      
      run_counter <- run_counter + 1
      
      res <- run_one_kmeans(
        bootstrap_id_current = b,
        matrix_name_current = matrix_name,
        matrix_index_current = matrix_index,
        matrix_df = matrix_df,
        feature_cols = feature_cols,
        k_current = k
      )
      
      metrics_list[[run_counter]] <- res$metrics
      centers_list[[run_counter]] <- res$centers_long
      sizes_list[[run_counter]] <- res$cluster_sizes
      distances_list[[run_counter]] <- res$distances
      composition_list[[run_counter]] <- res$composition
      
      if (SAVE_ASSIGNMENTS) {
        assignments_list[[run_counter]] <- res$assignments
      }
    }
  }
}

kmeans_metrics <- bind_rows(metrics_list)
kmeans_centers_long <- bind_rows(centers_list)
kmeans_cluster_sizes <- bind_rows(sizes_list)
kmeans_cluster_distances <- bind_rows(distances_list)
kmeans_cluster_composition <- bind_rows(composition_list)

if (SAVE_ASSIGNMENTS) {
  kmeans_assignments <- bind_rows(assignments_list)
} else {
  kmeans_assignments <- tibble()
}


# Resúmenes
kmeans_metrics_summary <- kmeans_metrics %>%
  group_by(matrix_name, k) %>%
  summarise(
    n_runs = n(),
    n_ok = sum(status == "ok"),
    n_error = sum(status != "ok"),
    mean_n_rows_used = mean(n_rows_used, na.rm = TRUE),
    mean_n_rows_dropped = mean(n_rows_dropped, na.rm = TRUE),
    mean_prop_rows_used = mean(prop_rows_used, na.rm = TRUE),
    mean_tot_withinss = mean(tot_withinss, na.rm = TRUE),
    sd_tot_withinss = sd(tot_withinss, na.rm = TRUE),
    mean_between_over_total = mean(between_over_total, na.rm = TRUE),
    sd_between_over_total = sd(between_over_total, na.rm = TRUE),
    mean_cluster_distance = mean(mean_cluster_distance, na.rm = TRUE),
    sd_cluster_distance = sd(mean_cluster_distance, na.rm = TRUE),
    mean_min_cluster_distance = mean(min_cluster_distance, na.rm = TRUE),
    mean_max_cluster_distance = mean(max_cluster_distance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(matrix_name, k)

cluster_size_summary <- kmeans_cluster_sizes %>%
  group_by(matrix_name, k, cluster_rank) %>%
  summarise(
    mean_n_cluster = mean(n_cluster, na.rm = TRUE),
    sd_n_cluster = sd(n_cluster, na.rm = TRUE),
    min_n_cluster = min(n_cluster, na.rm = TRUE),
    max_n_cluster = max(n_cluster, na.rm = TRUE),
    mean_prop_cluster = mean(prop_cluster, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(matrix_name, k, cluster_rank)

kmeans_centers_mean <- kmeans_centers_long %>%
  group_by(matrix_name, k, cluster_rank, determinant) %>%
  summarise(
    mean_center_value = mean(center_value, na.rm = TRUE),
    sd_center_value = sd(center_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(matrix_name, k, cluster_rank, determinant)

cluster_distance_summary <- kmeans_cluster_distances %>%
  group_by(matrix_name, k) %>%
  summarise(
    n_distances = n(),
    mean_distance = mean(euclidean_distance, na.rm = TRUE),
    sd_distance = sd(euclidean_distance, na.rm = TRUE),
    min_distance = min(euclidean_distance, na.rm = TRUE),
    q25_distance = quantile(euclidean_distance, 0.25, na.rm = TRUE),
    median_distance = median(euclidean_distance, na.rm = TRUE),
    q75_distance = quantile(euclidean_distance, 0.75, na.rm = TRUE),
    max_distance = max(euclidean_distance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(matrix_name, k)

cluster_composition_summary <- kmeans_cluster_composition %>%
  group_by(
    matrix_name,
    k,
    cluster_rank,
    target_sample_type,
    target_electoral_group
  ) %>%
  summarise(
    mean_n = mean(n, na.rm = TRUE),
    mean_prop_within_cluster = mean(prop_within_cluster, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(matrix_name, k, cluster_rank, desc(mean_prop_within_cluster))


#  Guardar outputs
write_csv(
  kmeans_metrics,
  file.path(out_dir, "kmeans_metrics_by_run.csv")
)

write_csv(
  kmeans_metrics_summary,
  file.path(out_dir, "kmeans_metrics_summary.csv")
)

write_csv(
  kmeans_cluster_sizes,
  file.path(out_dir, "kmeans_cluster_sizes_by_run.csv")
)

write_csv(
  cluster_size_summary,
  file.path(out_dir, "kmeans_cluster_size_summary.csv")
)

write_csv(
  kmeans_centers_long,
  file.path(out_dir, "kmeans_centers_long.csv")
)

write_csv(
  kmeans_centers_mean,
  file.path(out_dir, "kmeans_centers_mean_across_bootstraps.csv")
)

write_csv(
  kmeans_cluster_distances,
  file.path(out_dir, "kmeans_cluster_distances_by_run.csv")
)

write_csv(
  cluster_distance_summary,
  file.path(out_dir, "kmeans_cluster_distance_summary.csv")
)

write_csv(
  kmeans_cluster_composition,
  file.path(out_dir, "kmeans_cluster_composition_by_run.csv")
)

write_csv(
  cluster_composition_summary,
  file.path(out_dir, "kmeans_cluster_composition_summary.csv")
)

if (SAVE_ASSIGNMENTS) {
  write_csv(
    kmeans_assignments,
    file.path(out_dir, "kmeans_assignments_index.csv")
  )
}

parameters <- tibble(
  parameter = c(
    "bootstrap_scenario",
    "bootstrap_file",
    "matrix_dir",
    "matrices_to_run",
    "k_grid",
    "nstart",
    "iter_max",
    "max_bootstraps",
    "save_assignments",
    "save_heatmaps"
  ),
  value = c(
    BOOTSTRAP_SCENARIO,
    bootstrap_file,
    matrix_dir,
    paste(MATRICES_TO_RUN, collapse = ", "),
    paste(K_GRID, collapse = ", "),
    as.character(NSTART),
    as.character(ITER_MAX),
    as.character(MAX_BOOTSTRAPS),
    as.character(SAVE_ASSIGNMENTS),
    as.character(SAVE_HEATMAPS)
  )
)

write_csv(
  parameters,
  file.path(out_dir, "kmeans_bootstrap_parameters.csv")
)


# Heatmaps medios de centroides
if (SAVE_HEATMAPS && nrow(kmeans_centers_mean) > 0) {
  
  for (matrix_name_current in unique(kmeans_centers_mean$matrix_name)) {
    
    for (k_current in sort(unique(kmeans_centers_mean$k))) {
      
      plot_data <- kmeans_centers_mean %>%
        filter(
          matrix_name == matrix_name_current,
          k == k_current
        ) %>%
        mutate(
          cluster_rank = factor(cluster_rank),
          determinant = factor(
            determinant,
            levels = rev(sort(unique(determinant)))
          )
        )
      
      p <- ggplot(
        plot_data,
        aes(
          x = cluster_rank,
          y = determinant,
          fill = mean_center_value
        )
      ) +
        geom_tile() +
        theme_minimal(base_size = 10) +
        labs(
          title = paste0("K-means mean centers | ", matrix_name_current, " | K = ", k_current),
          subtitle = paste0("Averaged across ", length(boot_ids), " bootstrap samples"),
          x = "Cluster rank",
          y = "Determinant",
          fill = "Mean center"
        )
      
      ggsave(
        filename = file.path(
          fig_dir,
          paste0("heatmap_", matrix_name_current, "_K", k_current, ".png")
        ),
        plot = p,
        width = 9,
        height = 8,
        dpi = 300
      )
    }
  }
}


# Resumen consola
cat("\n============================================================\n")
cat("06. K-MEANS SOBRE BOOTSTRAPS COMPLETADO\n")
cat("============================================================\n")

cat("\nEscenario bootstrap usado:\n")
cat(BOOTSTRAP_SCENARIO, "\n")

cat("\nBootstraps usados:\n")
cat(length(boot_ids), "\n")

cat("\nMatrices usadas:\n")
print(MATRICES_TO_RUN)

cat("\nK explorados:\n")
print(K_GRID)

cat("\nResumen de métricas:\n")
print(kmeans_metrics_summary, n = Inf, width = Inf)

cat("\nResumen de distancias entre clusters:\n")
print(cluster_distance_summary, n = Inf, width = Inf)

cat("\nResumen de tamaño de clusters:\n")
print(cluster_size_summary, n = Inf, width = Inf)

cat("\nOutputs guardados en:\n")
cat(out_dir, "\n")

message("\nListo. K-means bootstrap guardado en: ", out_dir)