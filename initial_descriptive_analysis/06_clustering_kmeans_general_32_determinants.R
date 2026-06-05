# ==============================================================================
# SCRIPT 06 - CLUSTERING K-MEANS CON 32 DETERMINANTES
# ==============================================================================

# Librerías
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tibble)
library(cluster)


# Cargar datos
df <- read_csv(
  "initial_descriptive_analysis/output/clean_datasets/df_clean_general.csv",
  show_col_types = FALSE
)

base_output_dir <- "initial_descriptive_analysis/output/clustering_32_determinants"

csv_dir <- file.path(base_output_dir, "csv")
plots_dir <- file.path(base_output_dir, "plots")
pdf_dir <- file.path(base_output_dir, "pdf")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)

cat("Filas iniciales:", nrow(df), "\n")
cat("Columnas:", ncol(df), "\n")


# Crear participant_id
df <- df %>%
  mutate(
    participant_id = coalesce(
      as.character(join_key),
      as.character(prolific_id),
      as.character(identification_code),
      as.character(row_number())
    )
  )


# Definir columnas de determinantes
determinant_cols <- names(df)[9:40]

cat("Número de determinantes:", length(determinant_cols), "\n")

# Comprobar que hay una fila por participante

duplicate_participants <- df_cluster %>%
  count(participant_id, name = "n_rows") %>%
  filter(n_rows > 1)

write_csv(duplicate_participants, file.path(csv_dir, "duplicate_participants_clustering.csv"))


if (nrow(duplicate_participants) > 0) {
  warning("Hay participant_id duplicados. Revisa duplicate_participants_clustering.csv antes de interpretar el clustering.")
}

print(duplicate_participants, n = Inf)


# Diccionario de determinantes
determinant_dictionary <- tibble(
  determinant_col = determinant_cols,
  
  determinant_id = c(
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
  ),
  
  determinant_label = c(
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
)

write_csv(determinant_dictionary, file.path(csv_dir, "determinant_dictionary.csv"))


print(determinant_dictionary, n = Inf)


# Comprobar opciones originales de los determinantes
determinant_options_raw <- df_cluster %>%
  select(all_of(determinant_cols)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "determinant_col",
    values_to = "response_raw"
  ) %>%
  mutate(
    response_raw = str_squish(as.character(response_raw)),
    response_raw = na_if(response_raw, "")
  ) %>%
  filter(!is.na(response_raw)) %>%
  count(response_raw, sort = TRUE)

write_csv(determinant_options_raw, file.path(csv_dir, "determinant_options_raw.csv"))

View(determinant_options_raw)

print(determinant_options_raw, n = Inf)


determinant_options_by_variable <- df_cluster %>%
  select(all_of(determinant_cols)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "determinant_col",
    values_to = "response_raw"
  ) %>%
  mutate(
    response_raw = str_squish(as.character(response_raw)),
    response_raw = na_if(response_raw, "")
  ) %>%
  left_join(
    determinant_dictionary,
    by = "determinant_col"
  ) %>%
  filter(!is.na(response_raw)) %>%
  count(
    determinant_id,
    determinant_label,
    response_raw,
    sort = FALSE
  )

write_csv(determinant_options_by_variable, file.path(csv_dir, "determinant_options_by_variable.csv"))

View(determinant_options_by_variable)


# Función para limpiar la escala de determinantes

# La escala real de los determinantes es de 0 a 100.
clean_determinant_score <- function(x) {
  
  x <- suppressWarnings(as.numeric(x))
  
  case_when(
    is.na(x) ~ NA_real_,
    x >= 0 & x <= 100 ~ x,
    TRUE ~ NA_real_
  )
}


# Crear tabla larga de determinantes
determinants_long <- df_cluster %>%
  select(
    participant_id,
    all_of(determinant_cols)
  ) %>%
  pivot_longer(
    cols = all_of(determinant_cols),
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
  determinants_long,
  file.path(csv_dir, "determinants_long.csv")
  )

View(determinants_long)

glimpse(determinants_long)


# Revisar respuestas no clasificadas
determinants_unclassified <- determinants_long %>%
  filter(
    !is.na(response_raw),
    is.na(response_numeric)
  ) %>%
  count(
    determinant_id,
    determinant_label,
    response_raw,
    sort = TRUE
  )

write_csv(determinants_unclassified, file.path(csv_dir, "determinants_unclassified.csv"))

View(determinants_unclassified)

print(determinants_unclassified, n = Inf)

if (nrow(determinants_unclassified) > 0) {
  warning("Hay respuestas de determinantes sin recodificar. Revisa determinants_unclassified.csv.")
}


# Revisar valores faltantes
determinants_missing_by_variable <- determinants_long %>%
  group_by(
    determinant_id,
    determinant_label
  ) %>%
  summarise(
    n_missing = sum(is.na(response_numeric)),
    n_valid = sum(!is.na(response_numeric)),
    percentage_missing = n_missing / n() * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(n_missing))

write_csv(determinants_missing_by_variable, file.path(csv_dir, "determinants_missing_by_variable.csv"))


print(determinants_missing_by_variable, n = Inf)


determinants_missing_by_participant <- determinants_long %>%
  group_by(participant_id) %>%
  summarise(
    n_missing = sum(is.na(response_numeric)),
    n_valid = sum(!is.na(response_numeric)),
    .groups = "drop"
  ) %>%
  arrange(desc(n_missing))

write_csv(determinants_missing_by_participant, file.path(csv_dir, "determinants_missing_by_participant.csv"))


print(determinants_missing_by_participant, n = Inf)


# Crear matriz ancha de determinantes
determinant_ids <- determinant_dictionary$determinant_id

determinants_wide <- determinants_long %>%
  select(
    participant_id,
    determinant_id,
    response_numeric
  ) %>%
  pivot_wider(
    names_from = determinant_id,
    values_from = response_numeric
  ) %>%
  select(
    participant_id,
    all_of(determinant_ids)
  )

write_csv(determinants_wide, file.path(csv_dir, "determinants_wide.csv"))


View(determinants_wide)


# Preparar datos para clustering con imputación
# Primero calculamos cuántas respuestas válidas y faltantes tiene cada persona.
clustering_data_before_imputation <- determinants_wide %>%
  mutate(
    n_valid = rowSums(!is.na(across(all_of(determinant_ids)))),
    n_missing = rowSums(is.na(across(all_of(determinant_ids))))
  )

write_csv(clustering_data_before_imputation, file.path(csv_dir, "determinants_clustering_matrix_before_imputation.csv"))

print(
  clustering_data_before_imputation %>%
    select(participant_id, n_valid, n_missing) %>%
    arrange(desc(n_missing)),
  n = Inf
)

# Eliminamos solo a quienes no han respondido ningún determinante.
clustering_data_partial <- clustering_data_before_imputation %>%
  filter(n_valid > 0)

cat(
  "Participantes con al menos un determinante respondido:",
  nrow(clustering_data_partial),
  "\n"
)

cat(
  "Participantes excluidos por no responder ningún determinante:",
  nrow(clustering_data_before_imputation) - nrow(clustering_data_partial),
  "\n"
)

# Calcular mediana de cada determinante para imputar los valores faltantes.
imputation_values <- clustering_data_partial %>%
  summarise(
    across(
      all_of(determinant_ids),
      ~ median(.x, na.rm = TRUE)
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "determinant_id",
    values_to = "median_imputation_value"
  ) %>%
  left_join(
    determinant_dictionary %>%
      select(determinant_id, determinant_label),
    by = "determinant_id"
  ) %>%
  select(
    determinant_id,
    determinant_label,
    median_imputation_value
  )

write_csv(imputation_values, file.path(csv_dir, "determinants_imputation_values_median.csv"))


print(imputation_values, n = Inf)

# Matriz final para clustering:
# se mantienen los participantes con respuestas parciales
# y los NA se imputan con la mediana de cada determinante.
clustering_data <- clustering_data_partial %>%
  mutate(
    across(
      all_of(determinant_ids),
      ~ if_else(
        is.na(.x),
        median(.x, na.rm = TRUE),
        .x
      )
    )
  ) %>%
  select(
    participant_id,
    all_of(determinant_ids)
  )

write_csv(clustering_data, file.path(csv_dir, "determinants_clustering_matrix_imputed.csv"))


cat(
  "Participantes disponibles para clustering tras imputación:",
  nrow(clustering_data),
  "\n"
)

# Resumen de imputación por participante
imputation_summary_by_participant <- clustering_data_before_imputation %>%
  filter(n_valid > 0) %>%
  select(
    participant_id,
    n_valid_before_imputation = n_valid,
    n_imputed_values = n_missing
  ) %>%
  arrange(desc(n_imputed_values))

write_csv(imputation_summary_by_participant, file.path(csv_dir, "determinants_imputation_summary_by_participant.csv"))


print(imputation_summary_by_participant, n = Inf)


# Resumen de imputación por determinante
imputation_summary_by_determinant <- determinants_long %>%
  group_by(
    determinant_id,
    determinant_label
  ) %>%
  summarise(
    n_imputed_values = sum(is.na(response_numeric)),
    .groups = "drop"
  ) %>%
  arrange(desc(n_imputed_values))

write_csv(imputation_summary_by_determinant, file.path(csv_dir, "determinants_imputation_summary_by_determinant.csv"))


print(imputation_summary_by_determinant, n = Inf)


# Matriz numérica para k-means
kmeans_matrix <- clustering_data %>%
  select(all_of(determinant_ids)) %>%
  as.matrix()


# Diagnósticos de k
set.seed(1234)

k_max <- min(12, nrow(kmeans_matrix) - 1)

k_diagnostics <- tibble(
  k = 1:k_max,
  total_withinss = NA_real_,
  avg_silhouette = NA_real_
)

for (k in 1:k_max) {
  
  km_tmp <- kmeans(
    x = kmeans_matrix,
    centers = k,
    nstart = 50,
    iter.max = 100
  )
  
  k_diagnostics$total_withinss[k_diagnostics$k == k] <- km_tmp$tot.withinss
  
  if (k >= 2) {
    
    sil_tmp <- silhouette(
      km_tmp$cluster,
      dist(kmeans_matrix)
    )
    
    k_diagnostics$avg_silhouette[k_diagnostics$k == k] <-
      mean(sil_tmp[, "sil_width"])
  }
}

write_csv(k_diagnostics, file.path(csv_dir, "kmeans_diagnostics_32_determinants.csv"))


print(k_diagnostics, n = Inf)

# Ejecutar k-means definitivo con k = 8
k_final <- 8

if (nrow(kmeans_matrix) < k_final) {
  stop("No hay suficientes participantes para ejecutar k-means con k = 8.")
}

set.seed(1234)

kmeans_k8 <- kmeans(
  x = kmeans_matrix,
  centers = k_final,
  nstart = 50,
  iter.max = 100
)

print(kmeans_k8)


# Asignación de cluster por participante
cluster_assignments_k8 <- clustering_data %>%
  select(participant_id) %>%
  mutate(
    cluster = factor(
      paste0("Cluster ", kmeans_k8$cluster),
      levels = paste0("Cluster ", 1:k_final)
    )
  )

write_csv(cluster_assignments_k8, file.path(csv_dir, "kmeans_cluster_assignments_k8_32_determinants.csv"))


print(cluster_assignments_k8, n = Inf)


# Matriz de determinantes con cluster asignado
clustering_matrix_with_cluster_k8 <- clustering_data %>%
  left_join(
    cluster_assignments_k8,
    by = "participant_id"
  )

write_csv(clustering_matrix_with_cluster_k8, file.path(csv_dir, "determinants_clustering_matrix_with_cluster_k8.csv"))



# Tamaño de los clusters
cluster_sizes_k8 <- cluster_assignments_k8 %>%
  count(cluster, name = "n_participants")

write_csv(cluster_sizes_k8, file.path(csv_dir, "kmeans_cluster_sizes_k8_32_determinants.csv"))

print(cluster_sizes_k8, n = Inf)


# Centroides de los clusters
centroids_raw_k8 <- as_tibble(
  kmeans_k8$centers
) %>%
  mutate(
    cluster = factor(
      paste0("Cluster ", row_number()),
      levels = paste0("Cluster ", 1:k_final)
    )
  ) %>%
  select(
    cluster,
    all_of(determinant_ids)
  )

write_csv(centroids_raw_k8, file.path(csv_dir, "kmeans_centroids_raw_k8_32_determinants.csv"))


print(centroids_raw_k8, n = Inf)


centroids_long_k8 <- centroids_raw_k8 %>%
  pivot_longer(
    cols = all_of(determinant_ids),
    names_to = "determinant_id",
    values_to = "centroid"
  ) %>%
  left_join(
    determinant_dictionary %>%
      select(determinant_id, determinant_label),
    by = "determinant_id"
  )

write_csv(centroids_long_k8, file.path(csv_dir, "kmeans_centroids_long_k8_32_determinants.csv"))


View(centroids_long_k8)


# Diferencias respecto a la media global
overall_means <- clustering_data %>%
  select(all_of(determinant_ids)) %>%
  summarise(
    across(
      everything(),
      mean
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "determinant_id",
    values_to = "overall_mean"
  )

centroid_differences_k8 <- centroids_long_k8 %>%
  left_join(
    overall_means,
    by = "determinant_id"
  ) %>%
  mutate(
    difference_from_overall = centroid - overall_mean
  )

write_csv(centroid_differences_k8, file.path(csv_dir, "kmeans_centroid_differences_from_overall_k8_32_determinants.csv"))


View(centroid_differences_k8)


# Determinantes más por encima y más por debajo de la media en cada cluster
top_positive_differences_k8 <- centroid_differences_k8 %>%
  group_by(cluster) %>%
  slice_max(
    order_by = difference_from_overall,
    n = 5,
    with_ties = FALSE
  ) %>%
  ungroup()

top_negative_differences_k8 <- centroid_differences_k8 %>%
  group_by(cluster) %>%
  slice_min(
    order_by = difference_from_overall,
    n = 5,
    with_ties = FALSE
  ) %>%
  ungroup()

write_csv(top_positive_differences_k8, file.path(csv_dir, "kmeans_top_positive_differences_k8_32_determinants.csv"))


write_csv(top_negative_differences_k8, file.path(csv_dir, "kmeans_top_negative_differences_k8_32_determinants.csv"))


print(top_positive_differences_k8, n = Inf)
print(top_negative_differences_k8, n = Inf)


# PCA solo para visualizar los clusters
pca_k8 <- prcomp(
  kmeans_matrix,
  center = TRUE,
  scale. = TRUE
)

pca_variance <- summary(pca_k8)$importance[2, 1:2] * 100

pca_scores_k8 <- as_tibble(
  pca_k8$x[, 1:2]
) %>%
  mutate(
    participant_id = clustering_data$participant_id
  ) %>%
  left_join(
    cluster_assignments_k8,
    by = "participant_id"
  )

write_csv(pca_scores_k8, file.path(csv_dir, "kmeans_pca_scores_k8_32_determinants.csv"))


# Crear carpeta de gráficos
save_plot_png <- function(plot, filename, width = 9, height = 5) {
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
    axis.text = element_text(size = 10),
    legend.position = "right",
    plot.margin = margin(10, 10, 10, 10)
  )


# Gráfico de codo
plot_elbow_kmeans <- ggplot(
  k_diagnostics,
  aes(
    x = k,
    y = total_withinss
  )
) +
  geom_line() +
  geom_point(size = 2) +
  geom_vline(
    xintercept = k_final,
    linetype = "dashed"
  ) +
  scale_x_continuous(
    breaks = 1:k_max
  ) +
  labs(
    title = "Método del codo para k-means",
    subtitle = "Clustering con los 32 determinantes",
    x = "Número de clusters (k)",
    y = "Suma total de cuadrados intra-cluster"
  ) +
  theme_clustering

print(plot_elbow_kmeans)

save_plot_png(
  plot_elbow_kmeans,
  "kmeans_elbow_32_determinants",
  width = 8,
  height = 5
)


# Gráfico de silhouette
plot_silhouette_kmeans <- ggplot(
  k_diagnostics %>%
    filter(k >= 2),
  aes(
    x = k,
    y = avg_silhouette
  )
) +
  geom_line() +
  geom_point(size = 2) +
  geom_vline(
    xintercept = k_final,
    linetype = "dashed"
  ) +
  scale_x_continuous(
    breaks = 2:k_max
  ) +
  labs(
    title = "Silhouette medio para k-means",
    subtitle = "Clustering con los 32 determinantes",
    x = "Número de clusters (k)",
    y = "Silhouette medio"
  ) +
  theme_clustering

print(plot_silhouette_kmeans)

save_plot_png(
  plot_silhouette_kmeans,
  "kmeans_silhouette_32_determinants",
  width = 8,
  height = 5
)


# Gráfico de tamaño de clusters
plot_cluster_sizes_k8 <- ggplot(
  cluster_sizes_k8,
  aes(
    x = cluster,
    y = n_participants
  )
) +
  geom_col() +
  geom_text(
    aes(label = n_participants),
    vjust = -0.2
  ) +
  scale_y_continuous(
    limits = c(0, max(cluster_sizes_k8$n_participants) + 2)
  ) +
  labs(
    title = "Tamaño de los clusters",
    subtitle = "K-means con k = 8 sobre los 32 determinantes",
    x = NULL,
    y = "Número de participantes"
  ) +
  theme_clustering

print(plot_cluster_sizes_k8)

save_plot_png(
  plot_cluster_sizes_k8,
  "kmeans_cluster_sizes_k8_32_determinants",
  width = 8,
  height = 5
)


# Heatmap de centroides
centroids_heatmap_data <- centroids_long_k8 %>%
  mutate(
    determinant_label = factor(
      determinant_label,
      levels = rev(determinant_dictionary$determinant_label)
    )
  )

plot_centroids_heatmap_k8 <- ggplot(
  centroids_heatmap_data,
  aes(
    x = cluster,
    y = determinant_label,
    fill = centroid
  )
) +
  geom_tile(color = "black", linewidth = 0.2) +
  geom_text(
    aes(label = round(centroid, 1)),
    size = 2.8
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#0072B2",
    limits = c(0, 100)
  ) +
  labs(
    title = "Perfil medio de los clusters",
    subtitle = "Centroides en la escala original de los determinantes (0-100)",
    x = NULL,
    y = NULL,
    fill = "Media"
  ) +
  theme_clustering +
  theme(
    axis.text.y = element_text(size = 8)
  )

print(plot_centroids_heatmap_k8)

save_plot_png(
  plot_centroids_heatmap_k8,
  "kmeans_centroids_heatmap_k8_32_determinants",
  width = 10,
  height = 11
)


#Heatmap de diferencias frente a la media global
centroid_differences_heatmap_data <- centroid_differences_k8 %>%
  mutate(
    determinant_label = factor(
      determinant_label,
      levels = rev(determinant_dictionary$determinant_label)
    )
  )

plot_centroid_differences_heatmap_k8 <- ggplot(
  centroid_differences_heatmap_data,
  aes(
    x = cluster,
    y = determinant_label,
    fill = difference_from_overall
  )
) +
  geom_tile(color = "black", linewidth = 0.2) +
  geom_text(
    aes(label = round(difference_from_overall, 1)),
    size = 2.8
  ) +
  scale_fill_gradient2(
    low = "#D55E00",
    mid = "white",
    high = "#0072B2",
    midpoint = 0
  ) +
  labs(
    title = "Diferencias de cada cluster respecto a la media global",
    subtitle = "Valores positivos: por encima de la media; negativos: por debajo",
    x = NULL,
    y = NULL,
    fill = "Diferencia"
  ) +
  theme_clustering +
  theme(
    axis.text.y = element_text(size = 8)
  )

print(plot_centroid_differences_heatmap_k8)

save_plot_png(
  plot_centroid_differences_heatmap_k8,
  "kmeans_centroid_differences_heatmap_k8_32_determinants",
  width = 10,
  height = 11
)


# PCA de visualización
plot_pca_k8 <- ggplot(
  pca_scores_k8,
  aes(
    x = PC1,
    y = PC2,
    color = cluster
  )
) +
  geom_point(size = 2.5) +
  labs(
    title = "Visualización de los clusters mediante PCA",
    subtitle = "K-means con k = 8 sobre los 32 determinantes",
    x = paste0("PC1 (", round(pca_variance[1], 1), "%)"),
    y = paste0("PC2 (", round(pca_variance[2], 1), "%)"),
    color = "Cluster"
  ) +
  theme_clustering

print(plot_pca_k8)

save_plot_png(
  plot_pca_k8,
  "kmeans_pca_k8_32_determinants",
  width = 8,
  height = 6
)


# Guardar todos los gráficos en un único PDF
save_plots_pdf <- function(plot_list, filename, width = 10, height = 8) {
  
  pdf(
    file = file.path(pdf_dir, filename),
    width = width,
    height = height,
    onefile = TRUE
  )
  
  for (p in plot_list) {
    print(p)
  }
  
  dev.off()
}

all_clustering_plots <- list(
  plot_elbow_kmeans,
  plot_silhouette_kmeans,
  plot_cluster_sizes_k8,
  plot_centroids_heatmap_k8,
  plot_centroid_differences_heatmap_k8,
  plot_pca_k8
)

save_plots_pdf(
  plot_list = all_clustering_plots,
  filename = "kmeans_32_determinants_all_plots.pdf",
  width = 11,
  height = 8
)

