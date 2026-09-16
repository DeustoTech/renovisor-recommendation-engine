
#
# Resume el estado del dataset después de la integración, armonización,
# control de calidad y creación de scores de fase.
#
# Muestras descriptivas:
#   POOLED_ALL
#   DIEGO
#   RENOVISOR
#   WHY_EUROPE
#   WHY_LATAM
#
# POOLED_ALL se construye únicamente como agregado de las cuatro
# submuestras. No existe como categoría fila a fila.
#
# Este script es PRE-BOOTSTRAP:
#   EUROPE   -> se construirá en 04_2b
#   LATAM    -> se construirá en 04_2e
#   COMPLETE -> EUROPE + LATAM en 04_2f
#
# Aquí se generan únicamente tablas resumen. Las figuras descriptivas
# se generan posteriormente en 03_4_figures.R.


suppressPackageStartupMessages({
  library(tidyverse)
})


# Configuración

processed_root <- "paper1_cluster/data/processed"

file_integrated <- file.path(
  processed_root,
  "01_mergeData",
  "all_sources_integrated.csv"
)

file_clean <- file.path(
  processed_root,
  "01_1_harmonize_sociodemographics",
  "all_sources_integrated_clean_traceability.csv"
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

file_dimension_mapping <- file.path(
  processed_root,
  "03_2_phase_dimension_scores",
  "dimension_determinant_mapping.csv"
)

out_dir <- file.path(
  processed_root,
  "03_3_summary_dataset_status"
)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

required_files <- c(
  file_integrated,
  file_clean,
  file_quality,
  file_phase_scores,
  file_cluster_matrix,
  file_dimension_mapping
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files)) {
  stop(
    "Faltan archivos. Ejecuta primero los scripts anteriores:\n",
    paste(
      missing_files,
      collapse = "\n"
    )
  )
}


# Lectura

read_chr_csv <- function(path) {
  read_csv(
    path,
    show_col_types = FALSE,
    col_types = cols(
      .default = col_character()
    )
  )
}

integrated <- read_chr_csv(
  file_integrated
)

clean <- read_chr_csv(
  file_clean
)

quality <- read_chr_csv(
  file_quality
)

phase_scores <- read_chr_csv(
  file_phase_scores
)

cluster_matrix <- read_chr_csv(
  file_cluster_matrix
)

dimension_mapping <- read_csv(
  file_dimension_mapping,
  show_col_types = FALSE
)

df <- quality
phase_df <- phase_scores


# Funciones auxiliares

clean_text <- function(x) {
  x <- str_squish(
    as.character(x)
  )
  
  invalid_values <- c(
    "",
    "NA",
    "NaN",
    "NULL",
    "null",
    "None",
    "none",
    "DATA_EXPIRED",
    "data_expired"
  )
  
  x[
    is.na(x) |
      x %in% invalid_values
  ] <- NA_character_
  
  x
}


as_num <- function(x) {
  suppressWarnings(
    parse_number(
      as.character(x)
    )
  )
}


is_true <- function(x) {
  as.character(x) %in%
    c(
      "TRUE",
      "True",
      "true",
      "1"
    )
}


is_valid_for_model <- function(x) {
  x <- clean_text(x)
  
  !is.na(x) &
    !str_to_lower(x) %in%
    c(
      "unknown",
      "unknown_model",
      "conflict",
      "other",
      "invalid",
      "na",
      "nan",
      "none"
    )
}


safe_true_col <- function(data, col) {
  if (col %in% names(data)) {
    is_true(
      data[[col]]
    )
  } else {
    rep(
      NA,
      nrow(data)
    )
  }
}


safe_valid_col <- function(data, col) {
  if (col %in% names(data)) {
    is_valid_for_model(
      data[[col]]
    )
  } else {
    rep(
      NA,
      nrow(data)
    )
  }
}


add_analysis_samples <- function(data) {
  bind_rows(
    data %>%
      mutate(
        analysis_sample = "POOLED_ALL",
        analysis_region = "ALL"
      ),
    
    data %>%
      mutate(
        analysis_sample = subsample,
        analysis_region = comparison_region
      )
  )
}


coverage_summary <- function(
    data,
    variables
) {
  data %>%
    group_by(
      analysis_sample,
      analysis_region
    ) %>%
    summarise(
      n_rows = n(),
      
      across(
        all_of(variables),
        ~ sum(
          is_valid_for_model(.x),
          na.rm = TRUE
        ),
        .names = "n_valid_{.col}"
      ),
      
      .groups = "drop"
    ) %>%
    pivot_longer(
      starts_with(
        "n_valid_"
      ),
      names_to = "variable",
      values_to = "n_valid"
    ) %>%
    mutate(
      variable = str_remove(
        variable,
        "^n_valid_"
      ),
      
      prop_valid =
        n_valid /
        n_rows
    ) %>%
    arrange(
      variable,
      analysis_sample
    )
}


# Comprobaciones

required_quality_cols <- c(
  "integrated_row_id",
  "subsample",
  "comparison_region",
  "dataset_source",
  "source_survey",
  "usable_for_clustering",
  "n_det_valid",
  "row_quality_final",
  "metadata_quality"
)

missing_quality_cols <- setdiff(
  required_quality_cols,
  names(df)
)

if (length(missing_quality_cols)) {
  stop(
    "Faltan columnas fundamentales en 03_1: ",
    paste(
      missing_quality_cols,
      collapse = ", "
    )
  )
}

expected_subsamples <- c(
  "DIEGO",
  "RENOVISOR",
  "WHY_EUROPE",
  "WHY_LATAM"
)

missing_subsamples <- setdiff(
  expected_subsamples,
  unique(df$subsample)
)

if (length(missing_subsamples)) {
  stop(
    "Faltan submuestras esperadas: ",
    paste(
      missing_subsamples,
      collapse = ", "
    )
  )
}

if (anyDuplicated(df$integrated_row_id)) {
  stop(
    "integrated_row_id contiene duplicados en el dataset de calidad."
  )
}

if (!"integrated_row_id" %in% names(phase_df)) {
  stop(
    "El archivo de phase scores no contiene integrated_row_id."
  )
}

if (anyDuplicated(phase_df$integrated_row_id)) {
  stop(
    "El archivo de phase scores contiene integrated_row_id duplicados."
  )
}

if (
  !identical(
    as.character(
      df$integrated_row_id
    ),
    as.character(
      phase_df$integrated_row_id
    )
  )
) {
  stop(
    "03_1 y 03_2 no contienen los mismos IDs en el mismo orden."
  )
}

if (anyDuplicated(cluster_matrix$integrated_row_id)) {
  stop(
    "matrix_32det_for_clustering contiene IDs duplicados."
  )
}

if (
  any(
    !cluster_matrix$integrated_row_id %in%
    df$integrated_row_id
  )
) {
  stop(
    "La matriz de clustering contiene IDs no presentes en el dataset de calidad."
  )
}

if (
  n_distinct(
    dimension_mapping$det_col
  ) != 32
) {
  stop(
    "El mapeo dimensión-determinantes no contiene 32 determinantes únicos."
  )
}

if (
  n_distinct(
    dimension_mapping$dimension
  ) != 9
) {
  stop(
    "El mapeo dimensión-determinantes no contiene 9 dimensiones."
  )
}

df_analysis <- add_analysis_samples(
  df
)

phase_analysis <- add_analysis_samples(
  phase_df
)


# 00. Dimensiones de archivos

summary_00_files_dimensions <- tibble(
  step = c(
    "01_integrated",
    "02_harmonized",
    "03_1_quality",
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


# 01. Tamaños de muestra

summary_01_subsamples <- df %>%
  count(
    comparison_region,
    subsample,
    dataset_source,
    name = "n_rows"
  ) %>%
  mutate(
    prop_total =
      n_rows /
      sum(n_rows)
  ) %>%
  arrange(
    desc(n_rows)
  )

summary_01_analysis_samples <- df_analysis %>%
  group_by(
    analysis_sample,
    analysis_region
  ) %>%
  summarise(
    n_rows = n(),
    
    n_unique_ids = n_distinct(
      integrated_row_id
    ),
    
    n_sources = n_distinct(
      dataset_source
    ),
    
    n_subsamples = n_distinct(
      subsample
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = c(
        "POOLED_ALL",
        "DIEGO",
        "RENOVISOR",
        "WHY_EUROPE",
        "WHY_LATAM"
      )
    )
  ) %>%
  arrange(
    analysis_sample
  ) %>%
  mutate(
    analysis_sample =
      as.character(
        analysis_sample
      )
  )

summary_01_sources <- df %>%
  count(
    dataset_source,
    name = "n_rows"
  ) %>%
  mutate(
    prop_total =
      n_rows /
      sum(n_rows)
  ) %>%
  arrange(
    desc(n_rows)
  ) %>%
  bind_rows(
    tibble(
      dataset_source = "TOTAL",
      n_rows = nrow(df),
      prop_total = 1
    )
  )

summary_01_subsamples_surveys <- df %>%
  count(
    subsample,
    dataset_source,
    source_survey,
    name = "n_rows"
  ) %>%
  group_by(
    subsample
  ) %>%
  mutate(
    prop_within_subsample =
      n_rows /
      sum(n_rows)
  ) %>%
  ungroup() %>%
  arrange(
    subsample,
    desc(n_rows)
  )


# 02. Calidad final

usable_main_bool <- safe_true_col(
  df_analysis,
  "usable_for_main_analysis"
)

summary_02_quality_by_sample <- df_analysis %>%
  mutate(
    .usable_main =
      usable_main_bool
  ) %>%
  group_by(
    analysis_sample,
    analysis_region
  ) %>%
  summarise(
    n_rows = n(),
    
    n_usable_for_clustering = sum(
      is_true(
        usable_for_clustering
      ),
      na.rm = TRUE
    ),
    
    prop_usable_for_clustering =
      n_usable_for_clustering /
      n_rows,
    
    n_usable_for_main_analysis =
      if (
        all(
          is.na(
            .usable_main
          )
        )
      ) {
        NA_integer_
      } else {
        sum(
          .usable_main,
          na.rm = TRUE
        )
      },
    
    prop_usable_for_main_analysis =
      if_else(
        !is.na(
          n_usable_for_main_analysis
        ),
        n_usable_for_main_analysis /
          n_rows,
        NA_real_
      ),
    
    n_usable_complete = sum(
      row_quality_final ==
        "usable_complete",
      na.rm = TRUE
    ),
    
    n_usable_complete_limited_metadata = sum(
      row_quality_final ==
        "usable_complete_limited_metadata",
      na.rm = TRUE
    ),
    
    n_usable_partial = sum(
      row_quality_final ==
        "usable_partial",
      na.rm = TRUE
    ),
    
    n_usable_partial_limited_metadata = sum(
      row_quality_final ==
        "usable_partial_limited_metadata",
      na.rm = TRUE
    ),
    
    n_no_determinants = sum(
      row_quality_final ==
        "no_determinants",
      na.rm = TRUE
    ),
    
    n_too_many_missing_determinants = sum(
      row_quality_final ==
        "too_many_missing_determinants",
      na.rm = TRUE
    ),
    
    n_suspicious_determinants = sum(
      row_quality_final ==
        "suspicious_determinants",
      na.rm = TRUE
    ),
    
    n_invalid_determinants = sum(
      row_quality_final ==
        "invalid_determinants",
      na.rm = TRUE
    ),
    
    mean_n_det_valid = mean(
      as_num(
        n_det_valid
      ),
      na.rm = TRUE
    ),
    
    min_n_det_valid = min(
      as_num(
        n_det_valid
      ),
      na.rm = TRUE
    ),
    
    max_n_det_valid = max(
      as_num(
        n_det_valid
      ),
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# 03. Categorías de calidad

summary_03_quality_counts <- df_analysis %>%
  count(
    analysis_sample,
    analysis_region,
    row_quality_final,
    name = "n"
  ) %>%
  group_by(
    analysis_sample
  ) %>%
  mutate(
    prop =
      n /
      sum(n)
  ) %>%
  ungroup() %>%
  arrange(
    analysis_sample,
    desc(n)
  )


# 04. Calidad de metadata

summary_04_metadata_quality <- df_analysis %>%
  count(
    analysis_sample,
    analysis_region,
    metadata_quality,
    name = "n"
  ) %>%
  group_by(
    analysis_sample
  ) %>%
  mutate(
    prop =
      n /
      sum(n)
  ) %>%
  ungroup() %>%
  arrange(
    analysis_sample,
    desc(n)
  )


# 05. Cobertura de determinantes

det_cols <- names(df)[
  str_detect(
    names(df),
    "^det_\\d{2}_"
  )
]

if (length(det_cols) != 32) {
  stop(
    "Se esperaban 32 determinantes y se han encontrado ",
    length(det_cols),
    "."
  )
}

summary_05_determinants_by_sample <- df_analysis %>%
  group_by(
    analysis_sample,
    analysis_region
  ) %>%
  summarise(
    n_rows = n(),
    
    n_with_any_det = sum(
      as_num(
        n_det_valid
      ) > 0,
      na.rm = TRUE
    ),
    
    prop_with_any_det =
      n_with_any_det /
      n_rows,
    
    n_complete_32det = sum(
      as_num(
        n_det_valid
      ) == 32,
      na.rm = TRUE
    ),
    
    prop_complete_32det =
      n_complete_32det /
      n_rows,
    
    n_usable_for_clustering = sum(
      is_true(
        usable_for_clustering
      ),
      na.rm = TRUE
    ),
    
    prop_usable_for_clustering =
      n_usable_for_clustering /
      n_rows,
    
    mean_n_det_valid = mean(
      as_num(
        n_det_valid
      ),
      na.rm = TRUE
    ),
    
    min_n_det_valid = min(
      as_num(
        n_det_valid
      ),
      na.rm = TRUE
    ),
    
    max_n_det_valid = max(
      as_num(
        n_det_valid
      ),
      na.rm = TRUE
    ),
    
    n_rows_out_of_range = sum(
      as_num(
        n_det_out_of_range
      ) > 0,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# 06. Missing por determinante

summary_06_missing_by_determinant <- df_analysis %>%
  select(
    analysis_sample,
    analysis_region,
    all_of(
      det_cols
    )
  ) %>%
  pivot_longer(
    all_of(
      det_cols
    ),
    names_to = "determinant",
    values_to = "value_raw"
  ) %>%
  mutate(
    value_num = as_num(
      value_raw
    ),
    
    is_missing =
      is.na(
        value_num
      ),
    
    is_out_of_range =
      !is.na(value_num) &
      (
        value_num < 0 |
          value_num > 100
      )
  ) %>%
  group_by(
    analysis_sample,
    analysis_region,
    determinant
  ) %>%
  summarise(
    n_rows = n(),
    
    n_missing = sum(
      is_missing,
      na.rm = TRUE
    ),
    
    prop_missing =
      n_missing /
      n_rows,
    
    n_out_of_range = sum(
      is_out_of_range,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  arrange(
    analysis_sample,
    desc(
      prop_missing
    ),
    determinant
  )


# 07. Cobertura sociodemográfica

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
  "country_region_model",
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

sociodemographic_vars <- intersect(
  sociodemographic_vars,
  names(df_analysis)
)

summary_07_sociodemographic_coverage <- coverage_summary(
  df_analysis,
  sociodemographic_vars
)


# 08. Distribuciones sociodemográficas

summary_08_sociodemographic_counts <- df_analysis %>%
  select(
    analysis_sample,
    analysis_region,
    all_of(
      sociodemographic_vars
    )
  ) %>%
  pivot_longer(
    all_of(
      sociodemographic_vars
    ),
    names_to = "variable",
    values_to = "value"
  ) %>%
  count(
    analysis_sample,
    analysis_region,
    variable,
    value,
    name = "n"
  ) %>%
  group_by(
    analysis_sample,
    variable
  ) %>%
  mutate(
    prop =
      n /
      sum(n)
  ) %>%
  ungroup() %>%
  arrange(
    analysis_sample,
    variable,
    desc(n)
  )


# 09. Cobertura de variables de propensity europeo

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

propensity_vars <- intersect(
  propensity_vars,
  names(df_analysis)
)

summary_09_propensity_variable_coverage <- coverage_summary(
  df_analysis,
  propensity_vars
)


# 10. Readiness del propensity europeo

df_analysis_propensity <- df_analysis

df_analysis_propensity$.eu_applicable <- if (
  "propensity_eu_applicable" %in%
  names(df_analysis_propensity)
) {
  is_true(
    df_analysis_propensity$propensity_eu_applicable
  )
} else {
  df_analysis_propensity$comparison_region ==
    "EUROPE"
}

df_analysis_propensity$.has_binary_vote <- safe_true_col(
  df_analysis_propensity,
  "has_propensity_outcome_binary"
)

df_analysis_propensity$.has_age <- safe_true_col(
  df_analysis_propensity,
  "has_propensity_age"
)

df_analysis_propensity$.has_country <- safe_true_col(
  df_analysis_propensity,
  "has_propensity_country"
)

df_analysis_propensity$.has_employment <- safe_true_col(
  df_analysis_propensity,
  "has_propensity_employment"
)

df_analysis_propensity$.has_education <- safe_true_col(
  df_analysis_propensity,
  "has_propensity_education"
)

df_analysis_propensity$.has_income <- safe_true_col(
  df_analysis_propensity,
  "has_propensity_income"
)

df_analysis_propensity$.has_city_size <- safe_true_col(
  df_analysis_propensity,
  "has_propensity_city_size"
)

df_analysis_propensity$.has_gender <- safe_valid_col(
  df_analysis_propensity,
  "gender_model"
)

df_analysis_propensity$.ready_strict <- safe_true_col(
  df_analysis_propensity,
  "usable_for_propensity_model_strict"
)

df_analysis_propensity$.ready_without_income <- safe_true_col(
  df_analysis_propensity,
  "usable_for_propensity_model_without_income"
)

df_analysis_propensity$.ready_minimal <- safe_true_col(
  df_analysis_propensity,
  "usable_for_propensity_model_minimal"
)

summary_10_propensity_readiness <- df_analysis_propensity %>%
  group_by(
    analysis_sample,
    analysis_region
  ) %>%
  summarise(
    n_rows = n(),
    
    n_eu_applicable = sum(
      .eu_applicable,
      na.rm = TRUE
    ),
    
    prop_eu_applicable =
      n_eu_applicable /
      n_rows,
    
    n_has_binary_vote_outcome = sum(
      .has_binary_vote,
      na.rm = TRUE
    ),
    
    prop_has_binary_vote_outcome =
      n_has_binary_vote_outcome /
      n_rows,
    
    n_has_age = sum(
      .has_age,
      na.rm = TRUE
    ),
    
    n_has_gender = sum(
      .has_gender,
      na.rm = TRUE
    ),
    
    n_has_country = sum(
      .has_country,
      na.rm = TRUE
    ),
    
    n_has_employment = sum(
      .has_employment,
      na.rm = TRUE
    ),
    
    n_has_education = sum(
      .has_education,
      na.rm = TRUE
    ),
    
    n_has_income = sum(
      .has_income,
      na.rm = TRUE
    ),
    
    n_has_city_size = sum(
      .has_city_size,
      na.rm = TRUE
    ),
    
    n_model_ready_strict = sum(
      .ready_strict,
      na.rm = TRUE
    ),
    
    n_model_ready_without_income = sum(
      .ready_without_income,
      na.rm = TRUE
    ),
    
    n_model_ready_minimal = sum(
      .ready_minimal,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    prop_model_ready_strict =
      n_model_ready_strict /
      n_rows,
    
    prop_model_ready_without_income =
      n_model_ready_without_income /
      n_rows,
    
    prop_model_ready_minimal =
      n_model_ready_minimal /
      n_rows
  )


# 11. Fases y dimensiones

phase_prefixes <- c(
  "phase_implemented_reasons",
  "phase_more_likely_1",
  "phase_more_likely_2"
)

phase_score_cols <- intersect(
  paste0(
    phase_prefixes,
    "_selected_dimension_score_mean"
  ),
  names(
    phase_analysis
  )
)

phase_quality_cols <- intersect(
  paste0(
    phase_prefixes,
    "_quality"
  ),
  names(
    phase_analysis
  )
)

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
    nrow(phase_df),
    ncol(phase_df),
    length(phase_prefixes),
    length(phase_score_cols),
    length(phase_quality_cols),
    
    if (
      "n_phase_dimension_blocks_with_response" %in%
      names(phase_df)
    ) {
      sum(
        as_num(
          phase_df$n_phase_dimension_blocks_with_response
        ) > 0,
        na.rm = TRUE
      )
    } else {
      NA_integer_
    }
  )
)

summary_11_phase_block_by_sample <- phase_analysis %>%
  group_by(
    analysis_sample,
    analysis_region
  ) %>%
  summarise(
    n_rows = n(),
    
    n_rows_with_any_phase_response =
      if (
        "n_phase_dimension_blocks_with_response" %in%
        names(phase_analysis)
      ) {
        sum(
          as_num(
            n_phase_dimension_blocks_with_response
          ) > 0,
          na.rm = TRUE
        )
      } else {
        NA_integer_
      },
    
    prop_rows_with_any_phase_response =
      n_rows_with_any_phase_response /
      n_rows,
    
    .groups = "drop"
  )


# 12. Calidad de fases

if (
  length(
    phase_quality_cols
  ) &&
  "quality_phase_dimension_block" %in%
  names(phase_analysis)
) {
  summary_12_phase_quality_counts <- phase_analysis %>%
    select(
      analysis_sample,
      analysis_region,
      dataset_source,
      source_survey,
      all_of(
        phase_quality_cols
      ),
      quality_phase_dimension_block
    ) %>%
    pivot_longer(
      c(
        all_of(
          phase_quality_cols
        ),
        quality_phase_dimension_block
      ),
      names_to = "quality_variable",
      values_to = "quality_label"
    ) %>%
    count(
      analysis_sample,
      analysis_region,
      dataset_source,
      source_survey,
      quality_variable,
      quality_label,
      name = "n"
    ) %>%
    group_by(
      analysis_sample,
      source_survey,
      quality_variable
    ) %>%
    mutate(
      prop =
        n /
        sum(n)
    ) %>%
    ungroup() %>%
    arrange(
      analysis_sample,
      source_survey,
      quality_variable,
      desc(n)
    )
} else {
  summary_12_phase_quality_counts <- tibble()
}


# 13. Frecuencia de dimensiones por fase

selected_dim_cols <- names(
  phase_analysis
)[
  str_detect(
    names(
      phase_analysis
    ),
    "^phase_.*_selected_dim_"
  )
]

if (length(selected_dim_cols)) {
  summary_13_phase_dimension_frequencies <- phase_analysis %>%
    select(
      analysis_sample,
      analysis_region,
      all_of(
        selected_dim_cols
      )
    ) %>%
    group_by(
      analysis_sample,
      analysis_region
    ) %>%
    summarise(
      across(
        all_of(
          selected_dim_cols
        ),
        ~ sum(
          is_true(.x),
          na.rm = TRUE
        )
      ),
      
      n_rows_sample = n(),
      .groups = "drop"
    ) %>%
    pivot_longer(
      all_of(
        selected_dim_cols
      ),
      names_to = "phase_dimension_variable",
      values_to = "n_selected"
    ) %>%
    mutate(
      phase_prefix = str_remove(
        phase_dimension_variable,
        "_selected_dim_.*$"
      ),
      
      dimension = phase_dimension_variable %>%
        str_remove(
          "^phase_.*_selected_dim_"
        ) %>%
        str_to_upper(),
      
      prop_selected_sample =
        n_selected /
        n_rows_sample
    ) %>%
    select(
      analysis_sample,
      analysis_region,
      phase_prefix,
      dimension,
      n_rows_sample,
      n_selected,
      prop_selected_sample
    ) %>%
    arrange(
      analysis_sample,
      phase_prefix,
      desc(n_selected)
    )
} else {
  summary_13_phase_dimension_frequencies <- tibble()
}


# 14. Scores por fase

if (length(phase_score_cols)) {
  summary_14_phase_scores <- phase_analysis %>%
    select(
      analysis_sample,
      analysis_region,
      dataset_source,
      source_survey,
      all_of(
        phase_score_cols
      )
    ) %>%
    pivot_longer(
      all_of(
        phase_score_cols
      ),
      names_to = "score_variable",
      values_to = "score_raw"
    ) %>%
    mutate(
      score = as_num(
        score_raw
      )
    ) %>%
    group_by(
      analysis_sample,
      analysis_region,
      dataset_source,
      source_survey,
      score_variable
    ) %>%
    summarise(
      n_rows = n(),
      
      n_valid_score = sum(
        !is.na(score)
      ),
      
      prop_valid_score =
        n_valid_score /
        n_rows,
      
      mean_score = mean(
        score,
        na.rm = TRUE
      ),
      
      sd_score = sd(
        score,
        na.rm = TRUE
      ),
      
      min_score = suppressWarnings(
        min(
          score,
          na.rm = TRUE
        )
      ),
      
      max_score = suppressWarnings(
        max(
          score,
          na.rm = TRUE
        )
      ),
      
      .groups = "drop"
    ) %>%
    mutate(
      mean_score = if_else(
        is.nan(
          mean_score
        ),
        NA_real_,
        mean_score
      ),
      
      min_score = if_else(
        is.infinite(
          min_score
        ),
        NA_real_,
        min_score
      ),
      
      max_score = if_else(
        is.infinite(
          max_score
        ),
        NA_real_,
        max_score
      )
    ) %>%
    arrange(
      analysis_sample,
      source_survey,
      score_variable
    )
} else {
  summary_14_phase_scores <- tibble()
}


# 15. Mapeo dimensión-determinantes

summary_15_dimension_mapping <-
  dimension_mapping

summary_15_dimension_mapping_counts <- dimension_mapping %>%
  count(
    dimension,
    name = "n_determinants"
  ) %>%
  arrange(
    dimension
  )


# 16. Calidad de componentes

quality_cols <- names(
  df_analysis
)[
  str_detect(
    names(
      df_analysis
    ),
    "^quality_"
  )
]

if (length(quality_cols)) {
  summary_16_component_quality_counts <- df_analysis %>%
    select(
      analysis_sample,
      analysis_region,
      all_of(
        quality_cols
      )
    ) %>%
    pivot_longer(
      all_of(
        quality_cols
      ),
      names_to = "component",
      values_to = "quality_label"
    ) %>%
    count(
      analysis_sample,
      analysis_region,
      component,
      quality_label,
      name = "n"
    ) %>%
    group_by(
      analysis_sample,
      component
    ) %>%
    mutate(
      prop =
        n /
        sum(n)
    ) %>%
    ungroup() %>%
    arrange(
      analysis_sample,
      component,
      desc(n)
    )
} else {
  summary_16_component_quality_counts <- tibble()
}


# 17. Cobertura de componentes

component_n_cols <- names(
  df_analysis
)[
  str_detect(
    names(
      df_analysis
    ),
    "^n_.*_non_missing$"
  )
]

if (length(component_n_cols)) {
  summary_17_component_coverage <- df_analysis %>%
    select(
      analysis_sample,
      analysis_region,
      all_of(
        component_n_cols
      )
    ) %>%
    mutate(
      across(
        all_of(
          component_n_cols
        ),
        as_num
      )
    ) %>%
    pivot_longer(
      all_of(
        component_n_cols
      ),
      names_to = "component_n_variable",
      values_to = "n_non_missing"
    ) %>%
    group_by(
      analysis_sample,
      analysis_region,
      component_n_variable
    ) %>%
    summarise(
      n_rows = n(),
      
      n_rows_with_any = sum(
        n_non_missing > 0,
        na.rm = TRUE
      ),
      
      prop_rows_with_any =
        n_rows_with_any /
        n_rows,
      
      mean_n_non_missing = mean(
        n_non_missing,
        na.rm = TRUE
      ),
      
      min_n_non_missing = suppressWarnings(
        min(
          n_non_missing,
          na.rm = TRUE
        )
      ),
      
      max_n_non_missing = suppressWarnings(
        max(
          n_non_missing,
          na.rm = TRUE
        )
      ),
      
      .groups = "drop"
    ) %>%
    mutate(
      mean_n_non_missing = if_else(
        is.nan(
          mean_n_non_missing
        ),
        NA_real_,
        mean_n_non_missing
      ),
      
      min_n_non_missing = if_else(
        is.infinite(
          min_n_non_missing
        ),
        NA_real_,
        min_n_non_missing
      ),
      
      max_n_non_missing = if_else(
        is.infinite(
          max_n_non_missing
        ),
        NA_real_,
        max_n_non_missing
      )
    ) %>%
    arrange(
      analysis_sample,
      component_n_variable
    )
} else {
  summary_17_component_coverage <- tibble()
}


# 18. Tamaños pre-bootstrap disponibles para clustering

summary_18_clustering_sample_sizes <- bind_rows(
  cluster_matrix %>%
    summarise(
      analysis_sample = "POOLED_ALL",
      comparison_region = "ALL",
      n_usable_for_clustering = n(),
      n_unique_ids = n_distinct(
        integrated_row_id
      )
    ),
  
  cluster_matrix %>%
    group_by(
      subsample,
      comparison_region
    ) %>%
    summarise(
      analysis_sample = first(
        subsample
      ),
      
      n_usable_for_clustering = n(),
      
      n_unique_ids = n_distinct(
        integrated_row_id
      ),
      
      .groups = "drop"
    ) %>%
    select(
      analysis_sample,
      comparison_region,
      n_usable_for_clustering,
      n_unique_ids
    )
) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = c(
        "POOLED_ALL",
        "DIEGO",
        "RENOVISOR",
        "WHY_EUROPE",
        "WHY_LATAM"
      )
    )
  ) %>%
  arrange(
    analysis_sample
  ) %>%
  mutate(
    analysis_sample =
      as.character(
        analysis_sample
      )
  )

n_pooled_clustering <- summary_18_clustering_sample_sizes %>%
  filter(
    analysis_sample ==
      "POOLED_ALL"
  ) %>%
  pull(
    n_usable_for_clustering
  )

n_subsamples_clustering <- summary_18_clustering_sample_sizes %>%
  filter(
    analysis_sample !=
      "POOLED_ALL"
  ) %>%
  summarise(
    n = sum(
      n_usable_for_clustering
    )
  ) %>%
  pull(n)

if (length(n_pooled_clustering) != 1) {
  stop(
    "No se ha obtenido exactamente un POOLED_ALL."
  )
}

if (
  n_pooled_clustering !=
  n_subsamples_clustering
) {
  stop(
    "POOLED_ALL no coincide con la suma de las cuatro submuestras."
  )
}


# 99. Resumen ejecutivo

summary_99_final_overview <- tibble(
  metric = c(
    "total_rows_final_dataset",
    "total_columns_quality_dataset",
    "total_sources",
    "total_subsamples",
    "n_diego_rows",
    "n_renovisor_rows",
    "n_why_europe_rows",
    "n_why_latam_rows",
    "n_europe_rows",
    "n_latam_rows",
    "n_usable_for_clustering_total",
    "prop_usable_for_clustering_total",
    "n_usable_for_main_analysis_total",
    "prop_usable_for_main_analysis_total",
    "n_matrix_32det_for_clustering",
    "n_clustering_diego",
    "n_clustering_renovisor",
    "n_clustering_why_europe",
    "n_clustering_why_latam",
    "n_rows_with_invalid_determinants",
    "n_rows_with_any_phase_dimension_response",
    "n_propensity_eu_applicable",
    "n_propensity_non_europe_not_applicable",
    "n_rv_rows_with_binary_vote_outcome",
    "n_rv_model_ready_minimal"
  ),
  
  value = c(
    nrow(df),
    ncol(df),
    n_distinct(
      df$dataset_source
    ),
    n_distinct(
      df$subsample
    ),
    
    sum(
      df$subsample == "DIEGO",
      na.rm = TRUE
    ),
    
    sum(
      df$subsample == "RENOVISOR",
      na.rm = TRUE
    ),
    
    sum(
      df$subsample == "WHY_EUROPE",
      na.rm = TRUE
    ),
    
    sum(
      df$subsample == "WHY_LATAM",
      na.rm = TRUE
    ),
    
    sum(
      df$comparison_region == "EUROPE",
      na.rm = TRUE
    ),
    
    sum(
      df$comparison_region == "LATAM",
      na.rm = TRUE
    ),
    
    sum(
      is_true(
        df$usable_for_clustering
      ),
      na.rm = TRUE
    ),
    
    mean(
      is_true(
        df$usable_for_clustering
      ),
      na.rm = TRUE
    ),
    
    if (
      "usable_for_main_analysis" %in%
      names(df)
    ) {
      sum(
        is_true(
          df$usable_for_main_analysis
        ),
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    
    if (
      "usable_for_main_analysis" %in%
      names(df)
    ) {
      mean(
        is_true(
          df$usable_for_main_analysis
        ),
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    
    nrow(
      cluster_matrix
    ),
    
    sum(
      cluster_matrix$subsample ==
        "DIEGO",
      na.rm = TRUE
    ),
    
    sum(
      cluster_matrix$subsample ==
        "RENOVISOR",
      na.rm = TRUE
    ),
    
    sum(
      cluster_matrix$subsample ==
        "WHY_EUROPE",
      na.rm = TRUE
    ),
    
    sum(
      cluster_matrix$subsample ==
        "WHY_LATAM",
      na.rm = TRUE
    ),
    
    sum(
      as_num(
        df$n_det_out_of_range
      ) > 0,
      na.rm = TRUE
    ),
    
    if (
      "n_phase_dimension_blocks_with_response" %in%
      names(phase_df)
    ) {
      sum(
        as_num(
          phase_df$n_phase_dimension_blocks_with_response
        ) > 0,
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    
    sum(
      df$comparison_region ==
        "EUROPE",
      na.rm = TRUE
    ),
    
    sum(
      df$comparison_region !=
        "EUROPE",
      na.rm = TRUE
    ),
    
    if (
      "has_propensity_outcome_binary" %in%
      names(df)
    ) {
      sum(
        df$subsample ==
          "RENOVISOR" &
          is_true(
            df$has_propensity_outcome_binary
          ),
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    
    if (
      "usable_for_propensity_model_minimal" %in%
      names(df)
    ) {
      sum(
        df$subsample ==
          "RENOVISOR" &
          is_true(
            df$usable_for_propensity_model_minimal
          ),
        na.rm = TRUE
      )
    } else {
      NA_real_
    }
  )
)


# Guardado

outputs <- list(
  "summary_99_final_overview.csv" =
    summary_99_final_overview,
  
  "summary_00_files_dimensions.csv" =
    summary_00_files_dimensions,
  
  "summary_01_subsamples.csv" =
    summary_01_subsamples,
  
  "summary_01_analysis_samples.csv" =
    summary_01_analysis_samples,
  
  "summary_01_sources.csv" =
    summary_01_sources,
  
  "summary_01_subsamples_surveys.csv" =
    summary_01_subsamples_surveys,
  
  "summary_02_quality_by_sample.csv" =
    summary_02_quality_by_sample,
  
  "summary_03_quality_counts_by_sample.csv" =
    summary_03_quality_counts,
  
  "summary_04_metadata_quality_by_sample.csv" =
    summary_04_metadata_quality,
  
  "summary_05_determinants_by_sample.csv" =
    summary_05_determinants_by_sample,
  
  "summary_06_missing_by_determinant_by_sample.csv" =
    summary_06_missing_by_determinant,
  
  "summary_07_sociodemographic_coverage_by_sample.csv" =
    summary_07_sociodemographic_coverage,
  
  "summary_08_sociodemographic_counts_by_sample.csv" =
    summary_08_sociodemographic_counts,
  
  "summary_09_propensity_variable_coverage_by_sample.csv" =
    summary_09_propensity_variable_coverage,
  
  "summary_10_propensity_readiness_by_sample.csv" =
    summary_10_propensity_readiness,
  
  "summary_11_phase_block_overview.csv" =
    summary_11_phase_block_overview,
  
  "summary_11_phase_block_by_sample.csv" =
    summary_11_phase_block_by_sample,
  
  "summary_12_phase_quality_counts_by_sample.csv" =
    summary_12_phase_quality_counts,
  
  "summary_13_phase_dimension_frequencies_by_sample.csv" =
    summary_13_phase_dimension_frequencies,
  
  "summary_14_phase_scores_by_sample.csv" =
    summary_14_phase_scores,
  
  "summary_15_dimension_mapping.csv" =
    summary_15_dimension_mapping,
  
  "summary_15_dimension_mapping_counts.csv" =
    summary_15_dimension_mapping_counts,
  
  "summary_16_component_quality_counts_by_sample.csv" =
    summary_16_component_quality_counts,
  
  "summary_17_component_coverage_by_sample.csv" =
    summary_17_component_coverage,
  
  "summary_18_clustering_sample_sizes.csv" =
    summary_18_clustering_sample_sizes
)

iwalk(
  outputs,
  ~ write_csv(
    .x,
    file.path(
      out_dir,
      .y
    )
  )
)


# Resumen

cat("\nRESUMEN EJECUTIVO\n")

print(
  summary_99_final_overview,
  n = Inf,
  width = Inf
)

cat("\nSUBMUESTRAS ORIGINALES\n")

print(
  summary_01_subsamples,
  n = Inf,
  width = Inf
)

cat("\nMUESTRAS PRE-BOOTSTRAP\n")

print(
  summary_01_analysis_samples,
  n = Inf,
  width = Inf
)

cat("\nTAMAÑOS USABLES PARA CLUSTERING\n")

print(
  summary_18_clustering_sample_sizes,
  n = Inf,
  width = Inf
)

cat(
  "\nCHECK POOLED_ALL\n",
  "POOLED_ALL clustering: ",
  n_pooled_clustering,
  "\nSuma de las cuatro submuestras: ",
  n_subsamples_clustering,
  "\n",
  sep = ""
)

message(
  "\nListo. Resúmenes guardados en: ",
  out_dir
)