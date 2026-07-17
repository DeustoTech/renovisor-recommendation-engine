
# Objetivo:
# Crear un resumen completo del estado actual del dataset antes de
# pasar al propensity score.
#
# Incluye:
# - Datos por fuente
# - Dimensiones de archivos
# - Calidad final
# - Calidad de determinantes
# - Cobertura sociodemográfica
# - Preparación para propensity score
# - Bloque nuevo de dimensiones/fases
# - Gráficos resumen
#

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(stringr)
  library(ggplot2)
})


# Rutas
processed_root <- "paper1_cluster/data/processed"

file_integrated <- file.path(
  processed_root,
  "01_mergeData",
  "all_sources_integrated.csv"
)

file_clean <- file.path(
  processed_root,
  "01_1_harmonize_sociodemographics",
  "all_sources_integrated_clean.csv"
)

file_quality <- file.path(
  processed_root,
  "03_component_quality",
  "all_sources_integrated_component_quality.csv"
)

file_phase_scores <- file.path(
  processed_root,
  "03_2_phase_dimension_scores",
  "all_sources_integrated_component_quality_phase_scores.csv"
)

file_cluster_matrix <- file.path(
  processed_root,
  "03_component_quality",
  "matrix_32det_for_clustering.csv"
)

out_dir <- file.path(
  processed_root,
  "03_3_summary_dataset_status"
)

fig_dir <- file.path(out_dir, "figures")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

required_files <- c(
  file_integrated,
  file_clean,
  file_quality,
  file_phase_scores,
  file_cluster_matrix
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Faltan estos archivos. Revisa que hayas ejecutado los scripts anteriores:\n",
    paste(missing_files, collapse = "\n")
  )
}


# Lectura
integrated <- read_csv(
  file_integrated,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

clean <- read_csv(
  file_clean,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

quality <- read_csv(
  file_quality,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

phase_scores <- read_csv(
  file_phase_scores,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

cluster_matrix <- read_csv(
  file_cluster_matrix,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

# El dataset principal para el resumen final es el enriquecido
df <- phase_scores


# Funciones auxiliares
as_num <- function(x) {
  suppressWarnings(readr::parse_number(as.character(x)))
}

is_true <- function(x) {
  as.character(x) %in% c("TRUE", "True", "true", "1")
}

is_valid_for_model <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  
  !is.na(x) &
    x != "" &
    !str_to_lower(x) %in% c(
      "unknown",
      "conflict",
      "other",
      "invalid",
      "na",
      "nan",
      "none"
    )
}

print_section <- function(title) {
  cat("\n\n============================================================\n")
  cat(title, "\n")
  cat("============================================================\n")
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


# Dimensiones de archivos
summary_00_files_dimensions <- tibble(
  step = c(
    "01_integrated",
    "02_clean",
    "03_quality",
    "03_2_phase_scores",
    "matrix_32det_for_clustering"
  ),
  file = c(
    file_integrated,
    file_clean,
    file_quality,
    file_phase_scores,
    file_cluster_matrix
  ),
  n_rows = c(
    nrow(integrated),
    nrow(clean),
    nrow(quality),
    nrow(phase_scores),
    nrow(cluster_matrix)
  ),
  n_cols = c(
    ncol(integrated),
    ncol(clean),
    ncol(quality),
    ncol(phase_scores),
    ncol(cluster_matrix)
  )
)

# Datos por fuente
summary_01_sources <- df %>%
  count(dataset_source, name = "n_rows") %>%
  mutate(prop_total = n_rows / sum(n_rows)) %>%
  arrange(desc(n_rows)) %>%
  bind_rows(
    tibble(
      dataset_source = "TOTAL",
      n_rows = nrow(df),
      prop_total = 1
    )
  )

summary_01_sources_surveys <- df %>%
  count(dataset_source, source_survey, name = "n_rows") %>%
  group_by(dataset_source) %>%
  mutate(prop_within_source = n_rows / sum(n_rows)) %>%
  ungroup() %>%
  arrange(dataset_source, desc(n_rows))


# Calidad final por fuente
summary_02_quality_by_source <- df %>%
  group_by(dataset_source) %>%
  summarise(
    n_rows = n(),
    
    n_usable_for_clustering = sum(is_true(usable_for_clustering), na.rm = TRUE),
    prop_usable_for_clustering = n_usable_for_clustering / n_rows,
    
    n_usable_for_main_analysis = sum(is_true(usable_for_main_analysis), na.rm = TRUE),
    prop_usable_for_main_analysis = n_usable_for_main_analysis / n_rows,
    
    n_usable_complete = sum(row_quality_final == "usable_complete", na.rm = TRUE),
    n_usable_complete_limited_metadata = sum(row_quality_final == "usable_complete_limited_metadata", na.rm = TRUE),
    n_usable_partial = sum(row_quality_final == "usable_partial", na.rm = TRUE),
    n_usable_partial_limited_metadata = sum(row_quality_final == "usable_partial_limited_metadata", na.rm = TRUE),
    
    n_no_determinants = sum(row_quality_final == "no_determinants", na.rm = TRUE),
    n_too_many_missing_determinants = sum(row_quality_final == "too_many_missing_determinants", na.rm = TRUE),
    n_suspicious_determinants = sum(row_quality_final == "suspicious_determinants", na.rm = TRUE),
    n_invalid_determinants = sum(row_quality_final == "invalid_determinants", na.rm = TRUE),
    
    mean_n_det_valid = mean(as_num(n_det_valid), na.rm = TRUE),
    min_n_det_valid = min(as_num(n_det_valid), na.rm = TRUE),
    max_n_det_valid = max(as_num(n_det_valid), na.rm = TRUE),
    
    .groups = "drop"
  )

summary_03_quality_counts <- df %>%
  count(dataset_source, row_quality_final, name = "n") %>%
  group_by(dataset_source) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  arrange(dataset_source, desc(n))

summary_04_metadata_quality <- df %>%
  count(dataset_source, metadata_quality, name = "n") %>%
  group_by(dataset_source) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  arrange(dataset_source, desc(n))


# Determinantes 32
det_cols <- names(df)[str_detect(names(df), "^det_\\d{2}_")]

summary_05_determinants_by_source <- df %>%
  group_by(dataset_source) %>%
  summarise(
    n_rows = n(),
    n_with_any_det = sum(as_num(n_det_valid) > 0, na.rm = TRUE),
    prop_with_any_det = n_with_any_det / n_rows,
    n_complete_32det = sum(as_num(n_det_valid) == 32, na.rm = TRUE),
    prop_complete_32det = n_complete_32det / n_rows,
    n_usable_for_clustering = sum(is_true(usable_for_clustering), na.rm = TRUE),
    prop_usable_for_clustering = n_usable_for_clustering / n_rows,
    mean_n_det_valid = mean(as_num(n_det_valid), na.rm = TRUE),
    min_n_det_valid = min(as_num(n_det_valid), na.rm = TRUE),
    max_n_det_valid = max(as_num(n_det_valid), na.rm = TRUE),
    n_rows_out_of_range = sum(as_num(n_det_out_of_range) > 0, na.rm = TRUE),
    .groups = "drop"
  )

summary_06_missing_by_determinant <- df %>%
  select(dataset_source, all_of(det_cols)) %>%
  pivot_longer(
    cols = all_of(det_cols),
    names_to = "determinant",
    values_to = "value_raw"
  ) %>%
  mutate(
    value_num = as_num(value_raw),
    is_missing = is.na(value_num),
    is_out_of_range = !is.na(value_num) & (value_num < 0 | value_num > 100)
  ) %>%
  group_by(dataset_source, determinant) %>%
  summarise(
    n_rows = n(),
    n_missing = sum(is_missing, na.rm = TRUE),
    prop_missing = n_missing / n_rows,
    n_out_of_range = sum(is_out_of_range, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(dataset_source, desc(prop_missing), determinant)

# Cobertura sociodemográfica
sociodemographic_vars <- c(
  "age_model",
  "age_group_model",
  "gender_model",
  "education_model",
  "employment_model",
  "student_status_model",
  "city_size_model",
  "tenure_model",
  "income_model",
  "num_children_model",
  "country_model",
  "country_model_grouped",
  "country_birth_model",
  "nationality_model",
  "ethnicity_model",
  "language_model",
  "vote_status_declared",
  "voted_observed",
  "political_left_right_model",
  "political_block_model",
  "self_classification_model"
)

sociodemographic_vars <- sociodemographic_vars[
  sociodemographic_vars %in% names(df)
]

summary_07_sociodemographic_coverage <- df %>%
  group_by(dataset_source) %>%
  summarise(
    n_rows = n(),
    across(
      all_of(sociodemographic_vars),
      ~ sum(is_valid_for_model(.x), na.rm = TRUE),
      .names = "n_valid_{.col}"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("n_valid_"),
    names_to = "variable",
    values_to = "n_valid"
  ) %>%
  mutate(
    variable = str_remove(variable, "^n_valid_"),
    prop_valid = n_valid / n_rows
  ) %>%
  arrange(variable, dataset_source)

summary_08_sociodemographic_counts <- df %>%
  select(dataset_source, all_of(sociodemographic_vars)) %>%
  pivot_longer(
    cols = all_of(sociodemographic_vars),
    names_to = "variable",
    values_to = "value"
  ) %>%
  count(dataset_source, variable, value, name = "n") %>%
  group_by(dataset_source, variable) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  arrange(dataset_source, variable, desc(n))

# Propensity score readiness
propensity_vars <- c(
  "voted_observed",
  "vote_status_declared",
  "age_group_model",
  "gender_model",
  "country_model_grouped",
  "employment_model",
  "education_model",
  "income_model",
  "city_size_model",
  "student_status_model",
  "language_model"
)

propensity_vars <- propensity_vars[propensity_vars %in% names(df)]

summary_09_propensity_variable_coverage <- df %>%
  group_by(dataset_source) %>%
  summarise(
    n_rows = n(),
    across(
      all_of(propensity_vars),
      ~ sum(is_valid_for_model(.x), na.rm = TRUE),
      .names = "n_valid_{.col}"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("n_valid_"),
    names_to = "variable",
    values_to = "n_valid"
  ) %>%
  mutate(
    variable = str_remove(variable, "^n_valid_"),
    prop_valid = n_valid / n_rows
  ) %>%
  arrange(variable, dataset_source)

summary_10_propensity_readiness <- df %>%
  group_by(dataset_source) %>%
  summarise(
    n_rows = n(),
    
    n_has_binary_vote_outcome = sum(is_true(has_propensity_outcome_binary), na.rm = TRUE),
    prop_has_binary_vote_outcome = n_has_binary_vote_outcome / n_rows,
    
    n_has_age = sum(is_true(has_propensity_age), na.rm = TRUE),
    prop_has_age = n_has_age / n_rows,
    
    n_has_gender = sum(is_valid_for_model(gender_model), na.rm = TRUE),
    prop_has_gender = n_has_gender / n_rows,
    
    n_has_country = sum(is_true(has_propensity_country), na.rm = TRUE),
    prop_has_country = n_has_country / n_rows,
    
    n_has_employment = sum(is_true(has_propensity_employment), na.rm = TRUE),
    prop_has_employment = n_has_employment / n_rows,
    
    n_has_education = sum(is_true(has_propensity_education), na.rm = TRUE),
    prop_has_education = n_has_education / n_rows,
    
    n_has_income = sum(is_true(has_propensity_income), na.rm = TRUE),
    prop_has_income = n_has_income / n_rows,
    
    n_has_city_size = sum(is_true(has_propensity_city_size), na.rm = TRUE),
    prop_has_city_size = n_has_city_size / n_rows,
    
    n_model_ready_strict = sum(is_true(usable_for_propensity_model_strict), na.rm = TRUE),
    prop_model_ready_strict = n_model_ready_strict / n_rows,
    
    n_model_ready_without_income = sum(is_true(usable_for_propensity_model_without_income), na.rm = TRUE),
    prop_model_ready_without_income = n_model_ready_without_income / n_rows,
    
    n_model_ready_minimal = sum(is_true(usable_for_propensity_model_minimal), na.rm = TRUE),
    prop_model_ready_minimal = n_model_ready_minimal / n_rows,
    
    .groups = "drop"
  )

# Bloque de dimensiones/fases
phase_prefixes <- c(
  "phase_implemented_reasons",
  "phase_more_likely_1",
  "phase_more_likely_2"
)

phase_score_cols <- paste0(phase_prefixes, "_selected_dimension_score_mean")
phase_quality_cols <- paste0(phase_prefixes, "_quality")

phase_score_cols <- phase_score_cols[phase_score_cols %in% names(df)]
phase_quality_cols <- phase_quality_cols[phase_quality_cols %in% names(df)]

summary_11_phase_block_overview <- tibble(
  metric = c(
    "n_rows_phase_scores_dataset",
    "n_cols_phase_scores_dataset",
    "n_phase_prefixes",
    "n_phase_score_columns",
    "n_phase_quality_columns",
    "n_phase_blocks_with_any_response"
  ),
  value = c(
    nrow(df),
    ncol(df),
    length(phase_prefixes),
    length(phase_score_cols),
    length(phase_quality_cols),
    sum(as_num(df$n_phase_dimension_blocks_with_response) > 0, na.rm = TRUE)
  )
)

summary_12_phase_quality_counts <- df %>%
  select(dataset_source, source_survey, all_of(phase_quality_cols), quality_phase_dimension_block) %>%
  pivot_longer(
    cols = c(all_of(phase_quality_cols), quality_phase_dimension_block),
    names_to = "quality_variable",
    values_to = "quality_label"
  ) %>%
  count(dataset_source, source_survey, quality_variable, quality_label, name = "n") %>%
  group_by(dataset_source, source_survey, quality_variable) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  arrange(dataset_source, source_survey, quality_variable, desc(n))

selected_dim_cols <- names(df)[
  str_detect(
    names(df),
    "^phase_.*_selected_dim_"
  )
]

summary_13_phase_dimension_frequencies <- df %>%
  select(all_of(selected_dim_cols)) %>%
  summarise(
    across(
      everything(),
      ~ sum(is_true(.x), na.rm = TRUE)
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "phase_dimension_variable",
    values_to = "n_selected"
  ) %>%
  mutate(
    phase_prefix = str_remove(
      phase_dimension_variable,
      "_selected_dim_.*$"
    ),
    dimension = str_remove(
      phase_dimension_variable,
      "^phase_.*_selected_dim_"
    ),
    dimension = str_to_upper(dimension),
    prop_selected_total = n_selected / nrow(df)
  ) %>%
  select(phase_prefix, dimension, n_selected, prop_selected_total) %>%
  arrange(phase_prefix, desc(n_selected))

summary_14_phase_scores <- df %>%
  select(dataset_source, source_survey, all_of(phase_score_cols)) %>%
  pivot_longer(
    cols = all_of(phase_score_cols),
    names_to = "score_variable",
    values_to = "score_raw"
  ) %>%
  mutate(score = as_num(score_raw)) %>%
  group_by(dataset_source, source_survey, score_variable) %>%
  summarise(
    n_rows = n(),
    n_valid_score = sum(!is.na(score)),
    prop_valid_score = n_valid_score / n_rows,
    mean_score = mean(score, na.rm = TRUE),
    sd_score = sd(score, na.rm = TRUE),
    min_score = suppressWarnings(min(score, na.rm = TRUE)),
    max_score = suppressWarnings(max(score, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    min_score = if_else(is.infinite(min_score), NA_real_, min_score),
    max_score = if_else(is.infinite(max_score), NA_real_, max_score)
  ) %>%
  arrange(dataset_source, source_survey, score_variable)

summary_15_dimension_mapping <- read_csv(
  file.path(
    processed_root,
    "03_2_phase_dimension_scores",
    "dimension_determinant_mapping.csv"
  ),
  show_col_types = FALSE
)

summary_15_dimension_mapping_counts <- summary_15_dimension_mapping %>%
  count(dimension, name = "n_determinants") %>%
  arrange(dimension)

# Componentes generales
quality_cols <- names(df)[str_detect(names(df), "^quality_")]

summary_16_component_quality_counts <- df %>%
  select(dataset_source, all_of(quality_cols)) %>%
  pivot_longer(
    cols = all_of(quality_cols),
    names_to = "component",
    values_to = "quality_label"
  ) %>%
  count(dataset_source, component, quality_label, name = "n") %>%
  group_by(dataset_source, component) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  arrange(dataset_source, component, desc(n))

component_n_cols <- names(df)[str_detect(names(df), "^n_.*_non_missing$")]

summary_17_component_coverage <- df %>%
  select(dataset_source, all_of(component_n_cols)) %>%
  mutate(across(all_of(component_n_cols), as_num)) %>%
  pivot_longer(
    cols = all_of(component_n_cols),
    names_to = "component_n_variable",
    values_to = "n_non_missing"
  ) %>%
  group_by(dataset_source, component_n_variable) %>%
  summarise(
    n_rows = n(),
    n_rows_with_any = sum(n_non_missing > 0, na.rm = TRUE),
    prop_rows_with_any = n_rows_with_any / n_rows,
    mean_n_non_missing = mean(n_non_missing, na.rm = TRUE),
    min_n_non_missing = suppressWarnings(min(n_non_missing, na.rm = TRUE)),
    max_n_non_missing = suppressWarnings(max(n_non_missing, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    min_n_non_missing = if_else(is.infinite(min_n_non_missing), NA_real_, min_n_non_missing),
    max_n_non_missing = if_else(is.infinite(max_n_non_missing), NA_real_, max_n_non_missing)
  ) %>%
  arrange(dataset_source, component_n_variable)


# Resumen ejecutivo numérico
summary_99_final_overview <- tibble(
  metric = c(
    "total_rows_final_dataset",
    "total_columns_final_dataset",
    "total_sources",
    "n_rv_rows",
    "n_why_rows",
    "n_diego_rows",
    "n_usable_for_clustering_total",
    "prop_usable_for_clustering_total",
    "n_usable_for_main_analysis_total",
    "prop_usable_for_main_analysis_total",
    "n_matrix_32det_for_clustering",
    "n_rows_with_invalid_determinants",
    "n_rows_with_any_phase_dimension_response",
    "n_rv_rows_with_binary_vote_outcome",
    "n_rv_model_ready_minimal",
    "n_why_rows_to_predict_propensity",
    "n_diego_rows_to_predict_propensity"
  ),
  value = c(
    nrow(df),
    ncol(df),
    n_distinct(df$dataset_source),
    sum(df$dataset_source == "rv"),
    sum(df$dataset_source == "why"),
    sum(df$dataset_source == "diego"),
    sum(is_true(df$usable_for_clustering), na.rm = TRUE),
    mean(is_true(df$usable_for_clustering), na.rm = TRUE),
    sum(is_true(df$usable_for_main_analysis), na.rm = TRUE),
    mean(is_true(df$usable_for_main_analysis), na.rm = TRUE),
    nrow(cluster_matrix),
    sum(as_num(df$n_det_out_of_range) > 0, na.rm = TRUE),
    sum(as_num(df$n_phase_dimension_blocks_with_response) > 0, na.rm = TRUE),
    sum(df$dataset_source == "rv" & is_true(df$has_propensity_outcome_binary), na.rm = TRUE),
    sum(df$dataset_source == "rv" & is_true(df$usable_for_propensity_model_minimal), na.rm = TRUE),
    sum(df$dataset_source == "why"),
    sum(df$dataset_source == "diego")
  )
)


# Guardar tablas
write_csv(summary_99_final_overview, file.path(out_dir, "summary_99_final_overview.csv"))
write_csv(summary_00_files_dimensions, file.path(out_dir, "summary_00_files_dimensions.csv"))
write_csv(summary_01_sources, file.path(out_dir, "summary_01_sources.csv"))
write_csv(summary_01_sources_surveys, file.path(out_dir, "summary_01_sources_surveys.csv"))
write_csv(summary_02_quality_by_source, file.path(out_dir, "summary_02_quality_by_source.csv"))
write_csv(summary_03_quality_counts, file.path(out_dir, "summary_03_quality_counts.csv"))
write_csv(summary_04_metadata_quality, file.path(out_dir, "summary_04_metadata_quality.csv"))
write_csv(summary_05_determinants_by_source, file.path(out_dir, "summary_05_determinants_by_source.csv"))
write_csv(summary_06_missing_by_determinant, file.path(out_dir, "summary_06_missing_by_determinant.csv"))
write_csv(summary_07_sociodemographic_coverage, file.path(out_dir, "summary_07_sociodemographic_coverage.csv"))
write_csv(summary_08_sociodemographic_counts, file.path(out_dir, "summary_08_sociodemographic_counts.csv"))
write_csv(summary_09_propensity_variable_coverage, file.path(out_dir, "summary_09_propensity_variable_coverage.csv"))
write_csv(summary_10_propensity_readiness, file.path(out_dir, "summary_10_propensity_readiness.csv"))
write_csv(summary_11_phase_block_overview, file.path(out_dir, "summary_11_phase_block_overview.csv"))
write_csv(summary_12_phase_quality_counts, file.path(out_dir, "summary_12_phase_quality_counts.csv"))
write_csv(summary_13_phase_dimension_frequencies, file.path(out_dir, "summary_13_phase_dimension_frequencies.csv"))
write_csv(summary_14_phase_scores, file.path(out_dir, "summary_14_phase_scores.csv"))
write_csv(summary_15_dimension_mapping, file.path(out_dir, "summary_15_dimension_mapping.csv"))
write_csv(summary_15_dimension_mapping_counts, file.path(out_dir, "summary_15_dimension_mapping_counts.csv"))
write_csv(summary_16_component_quality_counts, file.path(out_dir, "summary_16_component_quality_counts.csv"))
write_csv(summary_17_component_coverage, file.path(out_dir, "summary_17_component_coverage.csv"))


# Gráficos
plot_01_sources <- summary_01_sources %>%
  filter(dataset_source != "TOTAL") %>%
  ggplot(aes(x = reorder(dataset_source, n_rows), y = n_rows)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Rows by data source",
    x = "Source",
    y = "Number of rows"
  ) +
  theme_minimal(base_size = 13)

save_plot(plot_01_sources, "plot_01_rows_by_source.png")

plot_02_usable_clustering <- summary_02_quality_by_source %>%
  ggplot(aes(x = dataset_source, y = prop_usable_for_clustering)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Usable rows for clustering by source",
    x = "Source",
    y = "Usable for clustering"
  ) +
  theme_minimal(base_size = 13)

save_plot(plot_02_usable_clustering, "plot_02_usable_for_clustering_by_source.png")

plot_03_quality_counts <- summary_03_quality_counts %>%
  ggplot(aes(x = dataset_source, y = n, fill = row_quality_final)) +
  geom_col(position = "stack") +
  labs(
    title = "Final row quality by source",
    x = "Source",
    y = "Number of rows",
    fill = "Row quality"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

save_plot(plot_03_quality_counts, "plot_03_row_quality_by_source.png", width = 11, height = 7)

plot_04_metadata <- summary_04_metadata_quality %>%
  ggplot(aes(x = dataset_source, y = n, fill = metadata_quality)) +
  geom_col(position = "stack") +
  labs(
    title = "Metadata quality by source",
    x = "Source",
    y = "Number of rows",
    fill = "Metadata quality"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

save_plot(plot_04_metadata, "plot_04_metadata_quality_by_source.png", width = 11, height = 7)

plot_05_propensity_vars <- summary_09_propensity_variable_coverage %>%
  ggplot(aes(x = reorder(variable, prop_valid), y = prop_valid)) +
  geom_col() +
  facet_wrap(~ dataset_source) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Coverage of candidate propensity-score variables",
    x = "Variable",
    y = "Valid proportion"
  ) +
  theme_minimal(base_size = 12)

save_plot(plot_05_propensity_vars, "plot_05_propensity_variable_coverage.png", width = 12, height = 8)

plot_06_phase_dimensions <- summary_13_phase_dimension_frequencies %>%
  ggplot(aes(x = reorder(dimension, n_selected), y = n_selected)) +
  geom_col() +
  facet_wrap(~ phase_prefix, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Selected dimensions by phase/block",
    x = "Dimension",
    y = "Number of selections"
  ) +
  theme_minimal(base_size = 12)

save_plot(plot_06_phase_dimensions, "plot_06_phase_dimension_frequencies.png", width = 12, height = 8)

plot_07_phase_scores <- summary_14_phase_scores %>%
  filter(dataset_source == "rv", str_detect(source_survey, "decision")) %>%
  ggplot(aes(x = score_variable, y = mean_score)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Mean selected-dimension score in RV Decision rows",
    x = "Phase score",
    y = "Mean score"
  ) +
  theme_minimal(base_size = 12)

save_plot(plot_07_phase_scores, "plot_07_phase_scores_rv_decision.png", width = 11, height = 6)

plot_08_mapping <- summary_15_dimension_mapping_counts %>%
  ggplot(aes(x = reorder(dimension, n_determinants), y = n_determinants)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Number of determinants mapped to each dimension",
    x = "Dimension",
    y = "Number of determinants"
  ) +
  theme_minimal(base_size = 13)

save_plot(plot_08_mapping, "plot_08_dimension_determinant_mapping.png", width = 10, height = 6)


# Imprimir resumen en consola
print_section("99. RESUMEN EJECUTIVO NUMÉRICO")
print(summary_99_final_overview, n = Inf, width = Inf)

print_section("00. DIMENSIONES DE ARCHIVOS")
print(summary_00_files_dimensions, n = Inf, width = Inf)

print_section("01. DATOS POR FUENTE")
print(summary_01_sources, n = Inf, width = Inf)

print_section("02. CALIDAD FINAL POR FUENTE")
print(summary_02_quality_by_source, n = Inf, width = Inf)

print_section("03. CATEGORÍAS DE CALIDAD")
print(summary_03_quality_counts, n = Inf, width = Inf)

print_section("04. CALIDAD DE METADATA")
print(summary_04_metadata_quality, n = Inf, width = Inf)

print_section("05. DETERMINANTES POR FUENTE")
print(summary_05_determinants_by_source, n = Inf, width = Inf)

print_section("07. COBERTURA SOCIODEMOGRÁFICA")
print(summary_07_sociodemographic_coverage, n = Inf, width = Inf)

print_section("09. COBERTURA VARIABLES PROPENSITY")
print(summary_09_propensity_variable_coverage, n = Inf, width = Inf)

print_section("10. PREPARACIÓN PROPENSITY SCORE")
print(summary_10_propensity_readiness, n = Inf, width = Inf)

print_section("11. BLOQUE DE DIMENSIONES/FASES")
print(summary_11_phase_block_overview, n = Inf, width = Inf)

print_section("13. FRECUENCIA DIMENSIONES POR FASE")
print(summary_13_phase_dimension_frequencies, n = Inf, width = Inf)

print_section("14. SCORES DE DIMENSIONES POR FASE")
print(
  summary_14_phase_scores %>%
    filter(dataset_source == "rv", str_detect(source_survey, "decision")),
  n = Inf,
  width = Inf
)

print_section("15. MAPEO DIMENSIÓN-DETERMINANTES")
print(summary_15_dimension_mapping_counts, n = Inf, width = Inf)

cat("\n\nListo.\n")
cat("Tablas guardadas en:\n", out_dir, "\n")
cat("Gráficos guardados en:\n", fig_dir, "\n")