
# SCRIPT 10 - CLUSTERING K-MEANS GENERAL Y POR ETAPA TTM

# Este script realiza clustering mediante k-means sobre los 32 determinantes
# de decisión relacionados con renovaciones energéticas residenciales.
#
# El análisis combina dos fuentes:
#
# 1. General:
#    - Se obtiene desde df_analysis_ready.csv.
#    - Corresponde al bloque general "Determinants" analizado en el Script 06.
#
# 2. Etapas TTM:
#    - Se obtienen desde ttm_stage_determinant_vector_wide.csv.
#    - Incluyen:
#         * Implementada
#         * La conoce / la consideraría
#         * No la conoce, pero le genera curiosidad
#
# Para cada bloque se:
#    - construye una matriz participante x 32 determinantes,
#    - se ejecuta k-means,
#    - se selecciona k automáticamente según silhouette,
#    - se guardan asignaciones, centroides, tamaños de cluster,
#    - se identifica una persona representativa por cluster,
#    - se generan gráficos y PDFs,
#    - se compara con la autoclasificación de la pregunta 4.3 si está disponible.
#


### forzar con 8 !!!!!!!!!!!!!!!!!
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tibble)
library(cluster)
library(purrr)
library(gridExtra)


# 1. Cargar datasets
df_general <- read_csv(
  "initial_descriptive_analysis/output/df_analysis_ready.csv",
  show_col_types = FALSE
)

df_ttm <- read_csv(
  "initial_descriptive_analysis/output/ttm_stage_determinant_vector_wide.csv",
  show_col_types = FALSE
)

cat("Filas df_general:", nrow(df_general), "\n")
cat("Columnas df_general:", ncol(df_general), "\n")
cat("Filas df_ttm:", nrow(df_ttm), "\n")
cat("Columnas df_ttm:", ncol(df_ttm), "\n")


# Crear participant_id en df_general

df_general <- df_general %>%
  mutate(
    participant_id = coalesce(
      as.character(join_key),
      as.character(prolific_id),
      as.character(identification_code),
      as.character(row_number())
    )
  )



# 2. Definir determinantes
# Columnas originales del bloque "Determinants" en df_analysis_ready.csv
determinant_cols_general <- names(df_general)[8:39]

# IDs cortos de los 32 determinantes
determinant_ids <- c(
  "profits",
  "credit_score_access_to_funding",
  "risk_profile",
  "added_value",
  "frugality",
  "climate_protection",
  "legal",
  "trust",
  "safety",
  "cost_efficiency",
  "knowledge",
  "own_competence",
  "technical_fit",
  "environmental_concerns",
  "self_satisfaction",
  "commitment",
  "adherence",
  "autarky",
  "wellbeing",
  "coziness",
  "rights_and_duties",
  "peer_pressure",
  "support",
  "socialising",
  "agreement",
  "novelty",
  "fun",
  "recognition",
  "trends",
  "authority",
  "approval",
  "own_significance"
)

determinant_labels <- c(
  "Beneficio económico",
  "Acceso a financiación",
  "Perfil de riesgo",
  "Valor añadido",
  "Frugalidad",
  "Protección climática",
  "Cumplimiento legal",
  "Confianza",
  "Seguridad",
  "Eficiencia de costes",
  "Conocimiento",
  "Competencia propia",
  "Adecuación técnica",
  "Preocupación ambiental",
  "Satisfacción personal",
  "Compromiso",
  "Persistencia",
  "Autosuficiencia",
  "Bienestar",
  "Confort",
  "Derechos y deberes",
  "Presión social",
  "Apoyo social",
  "Sociabilidad",
  "Acuerdo",
  "Novedad",
  "Diversión",
  "Reconocimiento",
  "Tendencias",
  "Autoridad",
  "Aprobación",
  "Significado personal"
)

determinant_dictionary <- tibble(
  determinant_col = determinant_cols_general,
  determinant = determinant_ids,
  determinant_label = determinant_labels
)

cat("Número de determinantes General:", length(determinant_cols_general), "\n")


# Columnas de determinantes en la tabla TTM 
id_col <- "participant_id"
stage_col <- "stage"
technology_col <- "technology"
dimension_col <- "dimension"

non_determinant_cols <- c(
  id_col,
  stage_col,
  technology_col,
  dimension_col,
  "dimension_label",
  "stage_dimension_id"
)

determinant_cols_ttm <- setdiff(names(df_ttm), non_determinant_cols)

determinant_cols_ttm <- determinant_cols_ttm[
  sapply(df_ttm[determinant_cols_ttm], is.numeric)
]

cat("Número de determinantes TTM:", length(determinant_cols_ttm), "\n")
print(determinant_cols_ttm)

if (length(determinant_cols_general) != 32) {
  warning("El bloque General no tiene 32 columnas de determinantes. Revisar names(df_general)[8:39].")
}

if (length(determinant_cols_ttm) != 32) {
  warning("La tabla TTM no tiene 32 columnas de determinantes. Revisar columnas no determinantes.")
}



# 3. Crear carpetas de salida
output_dir <- "initial_descriptive_analysis/output/clustering_ttm_stages"
plots_dir <- file.path(output_dir, "plots")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

write_csv(
  determinant_dictionary,
  file.path(output_dir, "determinant_dictionary_clustering.csv")
)

# 4. Funciones auxiliares
clean_determinant_score <- function(x) {
  
  x <- suppressWarnings(as.numeric(x))
  
  case_when(
    is.na(x) ~ NA_real_,
    x >= 0 & x <= 100 ~ x,
    TRUE ~ NA_real_
  )
}

safe_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("_$", "")
}

save_plot <- function(plot, filename, width = 10, height = 7) {
  ggsave(
    filename = file.path(plots_dir, paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}

theme_clustering <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )


# 5. Preparar matriz General desde df_analysis_ready.csv
prepare_general_matrix <- function(data) {
  
  data_long <- data %>%
    select(
      participant_id,
      all_of(determinant_cols_general)
    ) %>%
    pivot_longer(
      cols = all_of(determinant_cols_general),
      names_to = "determinant_col",
      values_to = "response_raw"
    ) %>%
    mutate(
      response_raw = str_squish(as.character(response_raw)),
      response_raw = na_if(response_raw, ""),
      response_numeric = clean_determinant_score(response_raw)
    ) %>%
    left_join(
      determinant_dictionary,
      by = "determinant_col"
    )
  
  write_csv(
    data_long,
    file.path(output_dir, "general_determinants_long.csv")
  )
  
  data_wide <- data_long %>%
    select(
      participant_id,
      determinant,
      response_numeric
    ) %>%
    pivot_wider(
      names_from = determinant,
      values_from = response_numeric
    ) %>%
    select(
      participant_id,
      all_of(determinant_ids)
    )
  
  data_before_imputation <- data_wide %>%
    mutate(
      n_valid = rowSums(!is.na(across(all_of(determinant_ids)))),
      n_missing = rowSums(is.na(across(all_of(determinant_ids))))
    )
  
  write_csv(
    data_before_imputation,
    file.path(output_dir, "general_matrix_before_imputation.csv")
  )
  
  data_partial <- data_before_imputation %>%
    filter(n_valid > 0)
  
  imputation_values <- data_partial %>%
    summarise(
      across(
        all_of(determinant_ids),
        ~ median(.x, na.rm = TRUE)
      )
    )
  
  data_imputed <- data_partial %>%
    mutate(
      across(
        all_of(determinant_ids),
        ~ ifelse(
          is.na(.x),
          median(.x, na.rm = TRUE),
          .x
        )
      )
    ) %>%
    select(
      participant_id,
      all_of(determinant_ids)
    ) %>%
    mutate(
      etapa_cluster = "General",
      .after = participant_id
    )
  
  write_csv(
    data_imputed,
    file.path(output_dir, "general_matrix_imputed.csv")
  )
  
  cat("General - participantes para clustering:", nrow(data_imputed), "\n")
  
  data_imputed
}


# 6. Preparar matriz por etapa desde ttm_stage_determinant_vector_wide.csv
prepare_stage_matrix <- function(data, selected_stage) {
  
  matrix_data <- data %>%
    filter(.data[[stage_col]] == selected_stage) %>%
    mutate(etapa_cluster = selected_stage) %>%
    group_by(
      participant_id = .data[[id_col]],
      etapa_cluster
    ) %>%
    summarise(
      across(
        all_of(determinant_cols_ttm),
        ~ mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    mutate(
      across(
        all_of(determinant_cols_ttm),
        ~ ifelse(is.nan(.x), NA_real_, .x)
      )
    )
  
  # En los vectores TTM, NA significa que el determinante no está activado
  # por la dimensión seleccionada. Para clustering se codifica como 0.
  matrix_data <- matrix_data %>%
    mutate(
      across(
        all_of(determinant_cols_ttm),
        ~ replace_na(.x, 0)
      )
    ) %>%
    select(
      participant_id,
      etapa_cluster,
      all_of(determinant_ids)
    )
  
  write_csv(
    matrix_data,
    file.path(output_dir, paste0("matrix_", safe_name(selected_stage), ".csv"))
  )
  
  cat(selected_stage, "- participantes para clustering:", nrow(matrix_data), "\n")
  
  matrix_data
}


# 7. Función principal de clustering
run_kmeans_for_stage <- function(stage_name, data_matrix, k_max_input = 8) {
  
  cat("CLUSTERING:", stage_name, "\n")

  stage_file <- safe_name(stage_name)
  
  if (nrow(data_matrix) < 4) {
    warning(paste("Muy pocos participantes para clusterizar:", stage_name))
    return(NULL)
  }
  
  x_raw <- data_matrix %>%
    select(all_of(determinant_ids)) %>%
    as.matrix()
  
  # Eliminar columnas con varianza 0 para evitar problemas al escalar
  variable_sds <- apply(x_raw, 2, sd, na.rm = TRUE)
  valid_vars <- names(variable_sds)[variable_sds > 0 & !is.na(variable_sds)]
  
  if (length(valid_vars) < 2) {
    warning(paste("Muy pocas variables con varianza para clusterizar:", stage_name))
    return(NULL)
  }
  
  x_raw_valid <- x_raw[, valid_vars, drop = FALSE]
  x_scaled <- scale(x_raw_valid)
  
  k_max <- min(k_max_input, nrow(x_scaled) - 1)
  
  if (k_max < 2) {
    warning(paste("No hay suficientes participantes para probar k >= 2:", stage_name))
    return(NULL)
  }
  
  k_diagnostics <- tibble(
    etapa_cluster = stage_name,
    k = 1:k_max,
    total_withinss = NA_real_,
    avg_silhouette = NA_real_
  )
  
  set.seed(1234)
  
  for (k in 1:k_max) {
    
    km_tmp <- kmeans(
      x = x_scaled,
      centers = k,
      nstart = 50,
      iter.max = 100
    )
    
    k_diagnostics$total_withinss[k_diagnostics$k == k] <- km_tmp$tot.withinss
    
    if (k >= 2) {
      sil_tmp <- silhouette(
        km_tmp$cluster,
        dist(x_scaled)
      )
      
      k_diagnostics$avg_silhouette[k_diagnostics$k == k] <-
        mean(sil_tmp[, "sil_width"])
    }
  }
  
  write_csv(
    k_diagnostics,
    file.path(output_dir, paste0("kmeans_diagnostics_", stage_file, ".csv"))
  )
  
  # Forzar k = 8 para compararlo con los 8 arquetipos teóricos
  k_final <- 8
  
  if (nrow(x_scaled) <= k_final) {
    stop(paste("No hay suficientes participantes para k = 8 en:", stage_name))
  }
  
  cat("k forzado por arquetipos teóricos:", k_final, "\n")
  
  cat("k elegido automáticamente:", k_final, "\n")
  
  set.seed(1234)
  
  km_final <- kmeans(
    x = x_scaled,
    centers = k_final,
    nstart = 50,
    iter.max = 100
  )
  
  # Asignaciones 
  cluster_assignments <- data_matrix %>%
    select(participant_id, etapa_cluster) %>%
    mutate(
      cluster = factor(
        paste0("Cluster ", km_final$cluster),
        levels = paste0("Cluster ", 1:k_final)
      )
    )
  
  write_csv(
    cluster_assignments,
    file.path(output_dir, paste0("cluster_assignments_", stage_file, ".csv"))
  )
  
  # Tamaños 
  cluster_sizes <- cluster_assignments %>%
    count(etapa_cluster, cluster, name = "n_participants")
  
  write_csv(
    cluster_sizes,
    file.path(output_dir, paste0("cluster_sizes_", stage_file, ".csv"))
  )
  
  # Centroides en escala original
  matrix_with_cluster <- data_matrix %>%
    left_join(
      cluster_assignments,
      by = c("participant_id", "etapa_cluster")
    )
  
  centroids_raw <- matrix_with_cluster %>%
    group_by(etapa_cluster, cluster) %>%
    summarise(
      across(
        all_of(determinant_ids),
        mean,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  write_csv(
    centroids_raw,
    file.path(output_dir, paste0("centroids_raw_", stage_file, ".csv"))
  )
  
  centroids_long <- centroids_raw %>%
    pivot_longer(
      cols = all_of(determinant_ids),
      names_to = "determinant",
      values_to = "centroid"
    ) %>%
    left_join(
      determinant_dictionary %>%
        select(determinant, determinant_label),
      by = "determinant"
    )
  
  write_csv(
    centroids_long,
    file.path(output_dir, paste0("centroids_long_", stage_file, ".csv"))
  )
  
  # Diferencias respecto a la media
  overall_means <- data_matrix %>%
    summarise(
      across(
        all_of(determinant_ids),
        mean,
        na.rm = TRUE
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "determinant",
      values_to = "overall_mean"
    )
  
  centroid_differences <- centroids_long %>%
    left_join(
      overall_means,
      by = "determinant"
    ) %>%
    mutate(
      difference_from_overall = centroid - overall_mean
    )
  
  write_csv(
    centroid_differences,
    file.path(output_dir, paste0("centroid_differences_", stage_file, ".csv"))
  )
  
  top_positive <- centroid_differences %>%
    group_by(etapa_cluster, cluster) %>%
    slice_max(
      order_by = difference_from_overall,
      n = 5,
      with_ties = FALSE
    ) %>%
    ungroup()
  
  top_negative <- centroid_differences %>%
    group_by(etapa_cluster, cluster) %>%
    slice_min(
      order_by = difference_from_overall,
      n = 5,
      with_ties = FALSE
    ) %>%
    ungroup()
  
  write_csv(
    top_positive,
    file.path(output_dir, paste0("top_positive_determinants_", stage_file, ".csv"))
  )
  
  write_csv(
    top_negative,
    file.path(output_dir, paste0("top_negative_determinants_", stage_file, ".csv"))
  )
  
  # Persona representativa
  centers_scaled <- km_final$centers
  
  distances_to_centroid <- map_dfr(1:nrow(x_scaled), function(i) {
    
    cl <- km_final$cluster[i]
    
    tibble(
      participant_id = data_matrix$participant_id[i],
      etapa_cluster = stage_name,
      cluster = paste0("Cluster ", cl),
      distance_to_centroid = sqrt(
        sum((x_scaled[i, ] - centers_scaled[cl, ])^2)
      )
    )
  })
  
  representative_persons <- distances_to_centroid %>%
    group_by(etapa_cluster, cluster) %>%
    slice_min(
      order_by = distance_to_centroid,
      n = 1,
      with_ties = FALSE
    ) %>%
    ungroup()
  
  write_csv(
    representative_persons,
    file.path(output_dir, paste0("representative_persons_", stage_file, ".csv"))
  )
  
  # PCA 
  pca <- prcomp(
    x_scaled,
    center = FALSE,
    scale. = FALSE
  )
  
  pca_variance <- summary(pca)$importance[2, 1:2] * 100
  
  pca_scores <- as_tibble(pca$x[, 1:2]) %>%
    mutate(
      participant_id = data_matrix$participant_id,
      etapa_cluster = stage_name
    ) %>%
    left_join(
      cluster_assignments,
      by = c("participant_id", "etapa_cluster")
    )
  
  write_csv(
    pca_scores,
    file.path(output_dir, paste0("pca_scores_", stage_file, ".csv"))
  )
  
  # Gráficos
  plot_elbow <- ggplot(
    k_diagnostics,
    aes(x = k, y = total_withinss)
  ) +
    geom_line(color = "#2C3E50") +
    geom_point(size = 2, color = "#2C3E50") +
    geom_vline(
      xintercept = k_final,
      linetype = "dashed"
    ) +
    scale_x_continuous(breaks = 1:k_max) +
    labs(
      title = paste("Método del codo -", stage_name),
      subtitle = paste("k forzado:", k_final),
      x = "Número de clusters",
      y = "Total within-cluster sum of squares"
    ) +
    theme_clustering
  
  plot_silhouette <- ggplot(
    k_diagnostics %>%
      filter(k >= 2),
    aes(x = k, y = avg_silhouette)
  ) +
    geom_line(color = "#2C3E50") +
    geom_point(size = 2, color = "#2C3E50") +
    geom_vline(
      xintercept = k_final,
      linetype = "dashed"
    ) +
    scale_x_continuous(breaks = 2:k_max) +
    labs(
      title = paste("Silhouette medio -", stage_name),
      subtitle = paste("k forzado:", k_final),
      x = "Número de clusters",
      y = "Silhouette medio"
    ) +
    theme_clustering
  
  plot_sizes <- ggplot(
    cluster_sizes,
    aes(x = cluster, y = n_participants)
  ) +
    geom_col(fill = "#BDE3FF", color = "#2C3E50") +
    geom_text(
      aes(label = n_participants),
      vjust = -0.2
    ) +
    labs(
      title = paste("Tamaño de clusters -", stage_name),
      x = NULL,
      y = "Número de participantes"
    ) +
    theme_clustering
  
  plot_centroids <- ggplot(
    centroids_long,
    aes(x = cluster, y = determinant_label, fill = centroid)
  ) +
    geom_tile(color = "white", linewidth = 0.2) +
    scale_fill_gradient(
      low = "white",
      high = "#4DADE8",
      limits = c(0, 100)
    ) +
    labs(
      title = paste("Centroides de clusters -", stage_name),
      x = NULL,
      y = "Determinante",
      fill = "Media"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 7),
      panel.grid = element_blank()
    )
  
  plot_differences <- ggplot(
    centroid_differences,
    aes(x = cluster, y = determinant_label, fill = difference_from_overall)
  ) +
    geom_tile(color = "white", linewidth = 0.2) +
    scale_fill_gradient2(
      low = "#D55E00",
      mid = "white",
      high = "#0072B2",
      midpoint = 0
    ) +
    labs(
      title = paste("Diferencias frente a la media -", stage_name),
      x = NULL,
      y = "Determinante",
      fill = "Diferencia"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 7),
      panel.grid = element_blank()
    )
  
  plot_pca <- ggplot(
    pca_scores,
    aes(x = PC1, y = PC2, color = cluster)
  ) +
    geom_point(size = 2.5, alpha = 0.8) +
    labs(
      title = paste("Visualización PCA -", stage_name),
      x = paste0("PC1 (", round(pca_variance[1], 1), "%)"),
      y = paste0("PC2 (", round(pca_variance[2], 1), "%)"),
      color = "Cluster"
    ) +
    theme_clustering
  
  save_plot(plot_elbow, paste0("elbow_", stage_file), width = 8, height = 5)
  save_plot(plot_silhouette, paste0("silhouette_", stage_file), width = 8, height = 5)
  save_plot(plot_sizes, paste0("cluster_sizes_", stage_file), width = 8, height = 5)
  save_plot(plot_centroids, paste0("centroids_heatmap_", stage_file), width = 10, height = 9)
  save_plot(plot_differences, paste0("centroid_differences_", stage_file), width = 10, height = 9)
  save_plot(plot_pca, paste0("pca_", stage_file), width = 8, height = 6)
  
  pdf(
    file = file.path(
      plots_dir,
      paste0("clustering_", stage_file, "_all_plots.pdf")
    ),
    width = 12,
    height = 8
  )
  
  print(plot_elbow)
  print(plot_silhouette)
  print(plot_sizes)
  print(plot_centroids)
  print(plot_differences)
  print(plot_pca)
  
  dev.off()
  
  list(
    stage_name = stage_name,
    k_final = k_final,
    diagnostics = k_diagnostics,
    assignments = cluster_assignments,
    sizes = cluster_sizes,
    centroids_long = centroids_long,
    centroid_differences = centroid_differences,
    representatives = representative_persons,
    pca_scores = pca_scores,
    plots = list(
      elbow = plot_elbow,
      silhouette = plot_silhouette,
      sizes = plot_sizes,
      centroids = plot_centroids,
      differences = plot_differences,
      pca = plot_pca
    )
  )
}


# ==============================================================================
# 8. Construir matrices de clustering
# ==============================================================================

matrices <- list(
  "General" = prepare_general_matrix(df_general),
  "Implementada" = prepare_stage_matrix(df_ttm, "Implementada"),
  "La conoce / la consideraría" = prepare_stage_matrix(df_ttm, "La conoce / la consideraría"),
  "No la conoce, pero le genera curiosidad" = prepare_stage_matrix(df_ttm, "No la conoce, pero le genera curiosidad")
)

# 9. Ejecutar clustering
results <- imap(
  matrices,
  ~ run_kmeans_for_stage(
    stage_name = .y,
    data_matrix = .x,
    k_max_input = 8
  )
)

# 10. Unir resultados globales
all_assignments <- results %>%
  compact() %>%
  map_dfr("assignments")

all_sizes <- results %>%
  compact() %>%
  map_dfr("sizes")

all_centroids <- results %>%
  compact() %>%
  map_dfr("centroids_long")

all_representatives <- results %>%
  compact() %>%
  map_dfr("representatives")

all_k_selected <- results %>%
  compact() %>%
  map_dfr(~ tibble(
    etapa_cluster = .x$stage_name,
    k_selected = .x$k_final
  ))

write_csv(
  all_assignments,
  file.path(output_dir, "ALL_cluster_assignments.csv")
)

write_csv(
  all_sizes,
  file.path(output_dir, "ALL_cluster_sizes.csv")
)

write_csv(
  all_centroids,
  file.path(output_dir, "ALL_cluster_centroids_long.csv")
)

write_csv(
  all_representatives,
  file.path(output_dir, "ALL_representative_persons.csv")
)

write_csv(
  all_k_selected,
  file.path(output_dir, "ALL_k_selected.csv")
)


# 11. Comparar con autoclasificación 4.3 si existe
self_classification_path <- "initial_descriptive_analysis/output/self_classification_4_3_clean.csv"

if (file.exists(self_classification_path)) {
  
  self_classification <- read_csv(
    self_classification_path,
    show_col_types = FALSE
  ) %>%
    select(participant_id, self_response_raw, self_profile)
  
  assignments_with_self <- all_assignments %>%
    left_join(
      self_classification,
      by = "participant_id"
    )
  
  write_csv(
    assignments_with_self,
    file.path(output_dir, "ALL_cluster_assignments_with_self_classification.csv")
  )
  
  comparison_self_cluster <- assignments_with_self %>%
    count(
      etapa_cluster,
      self_profile,
      cluster,
      name = "n_participants"
    ) %>%
    arrange(etapa_cluster, self_profile, cluster)
  
  write_csv(
    comparison_self_cluster,
    file.path(output_dir, "comparison_self_profile_vs_kmeans_cluster.csv")
  )
  
  plot_self_cluster <- comparison_self_cluster %>%
    filter(!is.na(self_profile)) %>%
    ggplot(
      aes(
        x = cluster,
        y = n_participants,
        fill = self_profile
      )
    ) +
    geom_col(position = "stack") +
    facet_wrap(~ etapa_cluster, scales = "free_x") +
    labs(
      title = "Comparación entre autoclasificación y clusters K-means",
      x = "Cluster K-means",
      y = "Número de participantes",
      fill = "Perfil 4.3"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
  
  print(plot_self_cluster)
  
  save_plot(
    plot_self_cluster,
    "comparison_self_profile_vs_kmeans_cluster",
    width = 14,
    height = 9
  )
  
  ggsave(
    filename = file.path(
      plots_dir,
      "comparison_self_profile_vs_kmeans_cluster.pdf"
    ),
    plot = plot_self_cluster,
    width = 14,
    height = 9
  )
}

# 12. PDF final con todos los gráficos
pdf(
  file = file.path(
    plots_dir,
    "clustering_ttm_stages_TODO.pdf"
  ),
  width = 12,
  height = 8
)

for (res in compact(results)) {
  print(res$plots$elbow)
  print(res$plots$silhouette)
  print(res$plots$sizes)
  print(res$plots$centroids)
  print(res$plots$differences)
  print(res$plots$pca)
}

if (exists("plot_self_cluster")) {
  print(plot_self_cluster)
}

dev.off()

cat("Clustering generado en:", output_dir, "\n")