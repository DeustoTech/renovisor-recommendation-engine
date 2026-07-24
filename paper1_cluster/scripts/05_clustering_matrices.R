
# Objetivo:
# Preparar las matrices de 32 determinantes para clustering:
#
# 1. raw_0_1:
#    determinantes originales 0-100 pasados a 0-1.
#    NA -> 50 antes de transformar; por tanto NA -> 0.5.
#
# 2. pos_0_1:
#    mantiene valores positivos/altos.
#    corte como en script antiguo: MAX = 50.
#    valores < 50 se mandan a neutral 0.5.
#
# 3. ext_0_1:
#    mide extremidad respecto al punto neutro 50.
#    ext = abs(x - 50)
#    corte como en script antiguo: Q3 = 25.
#    valores con ext < 25 se mandan a 0.
#
# 4. z_abs:
#    z-score absoluto.
#    z = abs((x - media) / sd)
#    NA -> 0 después de calcular z.
#
# También guarda matrices auxiliares para Greedy:
# - pos_mask_for_greedy
# - ext_mask_for_greedy
#
# Este script NO usa todavía bootstrap político.
# Las matrices se guardan por integrated_row_id.
# Luego, si hay bootstrap_samples_index.csv, se usarán esos IDs
# para seleccionar filas de estas matrices.

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(stringr)
  library(ggplot2)
})

set.seed(123)

# Parámetros
processed_root <- "paper1_cluster/data/processed"

in_file <- file.path(
  processed_root,
  "03_component_quality",
  "matrix_32det_for_clustering.csv"
)

out_dir <- file.path(
  processed_root,
  "05_clustering_matrices"
)

fig_dir <- file.path(out_dir, "figures")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# Cortes heredados del script antiguo
K_DEFAULT <- 8
MAX_POS <- 50
Q3_EXT <- 25
NF_DEFAULT <- 15

# Rango que se explorará después, no se usa todavía aquí
K_GRID <- 4:8
D_GRID <- 4:15


# Lectura
if (!file.exists(in_file)) {
  stop("No encuentro el archivo de entrada: ", in_file)
}

df <- read_csv(
  in_file,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

det_cols <- names(df)[str_detect(names(df), "^det_\\d{2}_")]

if (length(det_cols) != 32) {
  stop(
    "Se esperaban 32 determinantes, pero se han encontrado ",
    length(det_cols),
    ". Revisa nombres de columnas ^det_\\d{2}_"
  )
}

id_cols <- c(
  "integrated_row_id",
  "global_participant_key",
  "dataset_source",
  "source_survey",
  "country_model",
  "country_model_grouped",
  "age_group_model",
  "gender_model",
  "employment_model",
  "row_quality_final",
  "usable_for_clustering",
  "usable_for_main_analysis",
  "n_det_valid"
)

id_cols <- id_cols[id_cols %in% names(df)]

# Funciones auxiliares
as_num <- function(x) {
  suppressWarnings(
    readr::parse_number(
      as.character(x),
      locale = locale(decimal_mark = ".", grouping_mark = ",")
    )
  )
}

save_plot <- function(plot, filename, width = 10, height = 6) {
  ggsave(
    filename = file.path(fig_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}

write_matrix <- function(mat, name) {
  out <- bind_cols(
    df %>% select(any_of(id_cols)),
    as_tibble(mat)
  )
  
  write_csv(
    out,
    file.path(out_dir, paste0(name, ".csv"))
  )
  
  invisible(out)
}

summarise_matrix <- function(mat, matrix_name) {
  values <- as.numeric(as.matrix(mat))
  
  tibble(
    matrix_name = matrix_name,
    n_rows = nrow(mat),
    n_determinants = ncol(mat),
    n_values = length(values),
    n_missing = sum(is.na(values)),
    prop_missing = n_missing / n_values,
    mean_value = mean(values, na.rm = TRUE),
    sd_value = sd(values, na.rm = TRUE),
    min_value = min(values, na.rm = TRUE),
    q25_value = quantile(values, 0.25, na.rm = TRUE),
    median_value = median(values, na.rm = TRUE),
    q75_value = quantile(values, 0.75, na.rm = TRUE),
    max_value = max(values, na.rm = TRUE)
  )
}

# Matriz base 0-100
det_0_100_original <- df %>%
  select(all_of(det_cols)) %>%
  mutate(across(everything(), as_num))

# Valores fuera de rango se tratan como NA por seguridad
det_0_100_clean <- det_0_100_original %>%
  mutate(
    across(
      everything(),
      ~ if_else(.x >= 0 & .x <= 100, .x, NA_real_)
    )
  )

# Imputación neutral para matrices 0-100
det_0_100_imputed50 <- det_0_100_clean %>%
  mutate(across(everything(), ~ replace_na(.x, 50)))

# Matriz 1: raw / 100

matrix_raw_0_1 <- det_0_100_imputed50 / 100
names(matrix_raw_0_1) <- det_cols

# Matriz 2: pos / 100
# Hereda el corte del script antiguo:
# pos[pos < MAX] <- NA
#
# Para clustering no dejamos NA, sino neutral 0.5.
# Es decir:
# - si x >= 50: x / 100
# - si x < 50: 0.5
#
# También guardamos pos_mask_for_greedy con NA, para Greedy posterior.

pos_mask_for_greedy <- det_0_100_imputed50 %>%
  mutate(
    across(
      everything(),
      ~ if_else(.x >= MAX_POS, .x, NA_real_)
    )
  )

matrix_pos_0_1 <- pos_mask_for_greedy %>%
  mutate(
    across(
      everything(),
      ~ replace_na(.x / 100, 0.5)
    )
  )

names(matrix_pos_0_1) <- det_cols
names(pos_mask_for_greedy) <- det_cols

# Matriz 3: ext
# Hereda la lógica del script antiguo:
# ext = abs(x - 50)
# ext[ext < Q3] <- NA
#
# Para clustering:
# - si ext >= 25: ext / 50
# - si ext < 25: 0
#
# Nota:
# Aquí el valor neutral es 0, no 0.5.
# Porque ext mide distancia respecto al centro.
#
# También guardamos ext_mask_for_greedy con NA, para Greedy posterior.

ext_distance_0_50 <- abs(det_0_100_imputed50 - 50)

ext_mask_for_greedy <- ext_distance_0_50 %>%
  mutate(
    across(
      everything(),
      ~ if_else(.x >= Q3_EXT, .x, NA_real_)
    )
  )

matrix_ext_0_1 <- ext_mask_for_greedy %>%
  mutate(
    across(
      everything(),
      ~ replace_na(.x / 50, 0)
    )
  )

names(matrix_ext_0_1) <- det_cols
names(ext_mask_for_greedy) <- det_cols


# Matriz 4: z-score absoluto
#
# Primero se calcula el z-score con los datos observados.
# Después:
# - valor absoluto
# - NA -> 0
#
# 0 significa "no destaca respecto a la media".

det_means <- map_dbl(det_0_100_clean, ~ mean(.x, na.rm = TRUE))
det_sds <- map_dbl(det_0_100_clean, ~ sd(.x, na.rm = TRUE))

# Evitar división por cero
det_sds[is.na(det_sds) | det_sds == 0] <- 1

matrix_z_abs <- map2_dfc(
  det_0_100_clean,
  seq_along(det_cols),
  function(x, j) {
    z <- (x - det_means[j]) / det_sds[j]
    z_abs <- abs(z)
    z_abs[is.na(z_abs)] <- 0
    tibble(!!det_cols[j] := z_abs)
  }
)


# Matrices 9 dimensiones

# Estas matrices son opcionales, pero útiles para comparar 32 determinantes
# frente a 9 dimensiones.
#
# Agregamos por MEDIA de determinantes dentro de cada dimensión,
# no por suma, para que las dimensiones con más determinantes no pesen más.

dimension_determinant_map <- tribble(
  ~dimension, ~det_col,
  
  "FINANCIAL", "det_01_profits",
  "FINANCIAL", "det_02_credit_score",
  "FINANCIAL", "det_03_risk_profile",
  "FINANCIAL", "det_04_added_value",
  "FINANCIAL", "det_05_frugality",
  
  "SECURITY", "det_07_legal",
  "SECURITY", "det_08_trust",
  "SECURITY", "det_09_safety",
  
  "COMPETENCE", "det_10_cost_efficiency",
  "COMPETENCE", "det_11_knowledge",
  "COMPETENCE", "det_12_own_competence",
  "COMPETENCE", "det_13_technical_fit",
  
  "AUTONOMY", "det_15_self_satisfaction",
  "AUTONOMY", "det_16_commitment",
  "AUTONOMY", "det_17_adherence",
  "AUTONOMY", "det_18_autonomy",
  
  "PHYSIOLOGICAL", "det_19_wellbeing",
  "PHYSIOLOGICAL", "det_20_coziness",
  
  "RELATEDNESS", "det_21_rights_and_duties",
  "RELATEDNESS", "det_22_peer_pressure",
  "RELATEDNESS", "det_23_support",
  "RELATEDNESS", "det_24_socialising",
  "RELATEDNESS", "det_25_agreement",
  
  "STIMULATION", "det_26_novelty",
  "STIMULATION", "det_27_fun",
  
  "POPULARITY", "det_28_recognition",
  "POPULARITY", "det_29_trends",
  "POPULARITY", "det_30_authority",
  "POPULARITY", "det_31_approval",
  
  "MEANING", "det_06_climate_protection",
  "MEANING", "det_14_environmental_concerns",
  "MEANING", "det_32_own_significance"
)

aggregate_to_dimensions <- function(mat, matrix_name) {
  map_dfc(
    unique(dimension_determinant_map$dimension),
    function(dim) {
      cols_dim <- dimension_determinant_map %>%
        filter(dimension == dim) %>%
        pull(det_col)
      
      cols_dim <- cols_dim[cols_dim %in% names(mat)]
      
      if (length(cols_dim) == 0) {
        value <- rep(NA_real_, nrow(mat))
      } else {
        value <- rowMeans(
          as.matrix(mat[, cols_dim, drop = FALSE]),
          na.rm = TRUE
        )
      }
      
      tibble(!!paste0("dim_", str_to_lower(dim)) := value)
    }
  )
}

matrix_dim9_raw_0_1 <- aggregate_to_dimensions(matrix_raw_0_1, "raw")
matrix_dim9_pos_0_1 <- aggregate_to_dimensions(matrix_pos_0_1, "pos")
matrix_dim9_ext_0_1 <- aggregate_to_dimensions(matrix_ext_0_1, "ext")
matrix_dim9_z_abs <- aggregate_to_dimensions(matrix_z_abs, "z_abs")


# Guardar matrices
write_matrix(det_0_100_imputed50, "matrix_32_raw_0_100_imputed50")
write_matrix(matrix_raw_0_1, "matrix_32_raw_0_1")
write_matrix(matrix_pos_0_1, "matrix_32_pos_0_1")
write_matrix(matrix_ext_0_1, "matrix_32_ext_0_1")
write_matrix(matrix_z_abs, "matrix_32_z_abs")

write_matrix(pos_mask_for_greedy, "matrix_32_pos_mask_for_greedy")
write_matrix(ext_mask_for_greedy, "matrix_32_ext_mask_for_greedy")

write_matrix(matrix_dim9_raw_0_1, "matrix_9dim_raw_0_1")
write_matrix(matrix_dim9_pos_0_1, "matrix_9dim_pos_0_1")
write_matrix(matrix_dim9_ext_0_1, "matrix_9dim_ext_0_1")
write_matrix(matrix_dim9_z_abs, "matrix_9dim_z_abs")

# Diagnósticos
diagnostics_parameters <- tibble(
  parameter = c(
    "K_DEFAULT",
    "MAX_POS",
    "Q3_EXT",
    "NF_DEFAULT",
    "K_GRID",
    "D_GRID",
    "input_file",
    "output_dir"
  ),
  value = c(
    as.character(K_DEFAULT),
    as.character(MAX_POS),
    as.character(Q3_EXT),
    as.character(NF_DEFAULT),
    paste(K_GRID, collapse = ","),
    paste(D_GRID, collapse = ","),
    in_file,
    out_dir
  )
)

diagnostics_input_missing <- det_0_100_clean %>%
  mutate(integrated_row_id = df$integrated_row_id) %>%
  pivot_longer(
    cols = all_of(det_cols),
    names_to = "determinant",
    values_to = "value"
  ) %>%
  mutate(
    is_missing = is.na(value)
  ) %>%
  group_by(determinant) %>%
  summarise(
    n_rows = n(),
    n_missing = sum(is_missing),
    prop_missing = n_missing / n_rows,
    mean_observed = mean(value, na.rm = TRUE),
    sd_observed = sd(value, na.rm = TRUE),
    min_observed = min(value, na.rm = TRUE),
    max_observed = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(prop_missing), determinant)

diagnostics_matrix_summary <- bind_rows(
  summarise_matrix(matrix_raw_0_1, "matrix_32_raw_0_1"),
  summarise_matrix(matrix_pos_0_1, "matrix_32_pos_0_1"),
  summarise_matrix(matrix_ext_0_1, "matrix_32_ext_0_1"),
  summarise_matrix(matrix_z_abs, "matrix_32_z_abs"),
  summarise_matrix(matrix_dim9_raw_0_1, "matrix_9dim_raw_0_1"),
  summarise_matrix(matrix_dim9_pos_0_1, "matrix_9dim_pos_0_1"),
  summarise_matrix(matrix_dim9_ext_0_1, "matrix_9dim_ext_0_1"),
  summarise_matrix(matrix_dim9_z_abs, "matrix_9dim_z_abs")
)

diagnostics_threshold_counts <- tibble(
  determinant = det_cols,
  n_pos_ge_MAX = map_int(
    det_0_100_imputed50[det_cols],
    ~ sum(.x >= MAX_POS, na.rm = TRUE)
  ),
  prop_pos_ge_MAX = n_pos_ge_MAX / nrow(det_0_100_imputed50),
  n_ext_ge_Q3 = map_int(
    ext_distance_0_50[det_cols],
    ~ sum(.x >= Q3_EXT, na.rm = TRUE)
  ),
  prop_ext_ge_Q3 = n_ext_ge_Q3 / nrow(ext_distance_0_50),
  mean_raw_0_100 = map_dbl(
    det_0_100_imputed50[det_cols],
    ~ mean(.x, na.rm = TRUE)
  ),
  sd_raw_0_100 = map_dbl(
    det_0_100_imputed50[det_cols],
    ~ sd(.x, na.rm = TRUE)
  )
)

matrix_registry <- tibble(
  matrix_name = c(
    "matrix_32_raw_0_100_imputed50",
    "matrix_32_raw_0_1",
    "matrix_32_pos_0_1",
    "matrix_32_ext_0_1",
    "matrix_32_z_abs",
    "matrix_32_pos_mask_for_greedy",
    "matrix_32_ext_mask_for_greedy",
    "matrix_9dim_raw_0_1",
    "matrix_9dim_pos_0_1",
    "matrix_9dim_ext_0_1",
    "matrix_9dim_z_abs"
  ),
  file = paste0(matrix_name, ".csv"),
  intended_use = c(
    "diagnostic_old_scale",
    "kmeans_efa_greedy",
    "kmeans_efa_greedy_positive_high_values",
    "kmeans_efa_greedy_extreme_values",
    "kmeans_efa_greedy_relative_deviation",
    "greedy_binarisation_only",
    "greedy_binarisation_only",
    "kmeans_efa_dimension_level",
    "kmeans_efa_dimension_level_positive",
    "kmeans_efa_dimension_level_extreme",
    "kmeans_efa_dimension_level_relative_deviation"
  ),
  description = c(
    "Original 0-100 values with NA imputed to 50.",
    "Original values divided by 100; NA becomes 0.5.",
    "Only values >= MAX_POS keep their value/100; the rest becomes neutral 0.5.",
    "Absolute distance from 50, thresholded at Q3_EXT and scaled by /50; non-extreme values become 0.",
    "Absolute z-score; NA becomes 0.",
    "Old-style positive mask: values < MAX_POS become NA. For later top-D binarisation.",
    "Old-style extreme mask: abs(x-50) < Q3_EXT becomes NA. For later top-D binarisation.",
    "Mean of raw_0_1 determinants by theoretical dimension.",
    "Mean of pos_0_1 determinants by theoretical dimension.",
    "Mean of ext_0_1 determinants by theoretical dimension.",
    "Mean of z_abs determinants by theoretical dimension."
  )
)

write_csv(
  diagnostics_parameters,
  file.path(out_dir, "diagnostics_parameters.csv")
)

write_csv(
  diagnostics_input_missing,
  file.path(out_dir, "diagnostics_input_missing_by_determinant.csv")
)

write_csv(
  diagnostics_matrix_summary,
  file.path(out_dir, "diagnostics_matrix_summary.csv")
)

write_csv(
  diagnostics_threshold_counts,
  file.path(out_dir, "diagnostics_threshold_counts.csv")
)

write_csv(
  matrix_registry,
  file.path(out_dir, "matrix_registry.csv")
)

write_csv(
  dimension_determinant_map,
  file.path(out_dir, "dimension_determinant_map_used.csv")
)

# Gráficos rápidos
plot_matrix_summary <- diagnostics_matrix_summary %>%
  mutate(matrix_name = factor(matrix_name, levels = matrix_name)) %>%
  ggplot(aes(x = matrix_name, y = mean_value)) +
  geom_col() +
  geom_errorbar(
    aes(
      ymin = mean_value - sd_value,
      ymax = mean_value + sd_value
    ),
    width = 0.2
  ) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  labs(
    title = "Mean and SD by clustering matrix",
    x = "Matrix",
    y = "Mean value ± SD"
  )

save_plot(plot_matrix_summary, "01_matrix_summary_mean_sd.png", width = 10, height = 6)

plot_thresholds <- diagnostics_threshold_counts %>%
  select(determinant, prop_pos_ge_MAX, prop_ext_ge_Q3) %>%
  pivot_longer(
    cols = c(prop_pos_ge_MAX, prop_ext_ge_Q3),
    names_to = "threshold_type",
    values_to = "prop"
  ) %>%
  mutate(
    threshold_type = recode(
      threshold_type,
      prop_pos_ge_MAX = paste0("Positive >= ", MAX_POS),
      prop_ext_ge_Q3 = paste0("Extreme |x-50| >= ", Q3_EXT)
    ),
    determinant = factor(determinant, levels = rev(det_cols))
  ) %>%
  ggplot(aes(x = determinant, y = prop, fill = threshold_type)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_minimal(base_size = 10) +
  labs(
    title = "Share of rows passing positive/extreme thresholds",
    x = "Determinant",
    y = "Share of rows",
    fill = "Threshold"
  )

save_plot(plot_thresholds, "02_threshold_counts_by_determinant.png", width = 12, height = 9)


# Gráfico compacto de distribución de valores
boxplot_matrix_values <- bind_rows(
  matrix_raw_0_1 %>%
    mutate(matrix_name = "matrix_32_raw_0_1"),
  
  matrix_pos_0_1 %>%
    mutate(matrix_name = "matrix_32_pos_0_1"),
  
  matrix_ext_0_1 %>%
    mutate(matrix_name = "matrix_32_ext_0_1"),
  
  matrix_z_abs %>%
    mutate(matrix_name = "matrix_32_z_abs")
) %>%
  pivot_longer(
    cols = all_of(det_cols),
    names_to = "determinant",
    values_to = "value"
  )

plot_matrix_boxplot <- ggplot(
  boxplot_matrix_values,
  aes(x = matrix_name, y = value, fill = matrix_name)
) +
  geom_boxplot(outlier.alpha = 0.15) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  guides(fill = "none") +
  labs(
    title = "Distribution of values by clustering matrix",
    x = "Matrix",
    y = "Value"
  )

save_plot(
  plot_matrix_boxplot,
  "03_matrix_value_boxplot_32det.png",
  width = 10,
  height = 6
)

# Consola
cat("\n============================================================\n")
cat("05. CLUSTERING MATRICES COMPLETADO\n")
cat("============================================================\n")

cat("\nArchivo de entrada:\n")
cat(in_file, "\n")

cat("\nMatrices guardadas en:\n")
cat(out_dir, "\n")

cat("\nFiguras guardadas en:\n")
cat(fig_dir, "\n")

cat("\nMatrices creadas:\n")
print(matrix_registry$matrix_name)

message("\nListo. Matrices de clustering guardadas.")