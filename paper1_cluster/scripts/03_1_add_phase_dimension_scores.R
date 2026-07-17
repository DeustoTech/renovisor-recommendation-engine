

# Objetivo:
# Añadir al dataset final grande variables derivadas de:
# - las dimensiones seleccionadas como importantes en la encuesta RV Decision
# - los 32 determinantes armonizados
#
# Para cada bloque/fase:
# 1. Extrae qué dimensiones fueron seleccionadas.
# 2. Crea pesos 0/1 para los determinantes según su dimensión.
# 3. Crea determinantes ponderados.
# 4. Calcula una puntuación media de las dimensiones seleccionadas.
# 5. Añade calidad del bloque según cuántos determinantes estaban disponibles.
#


suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(stringr)
})


# Rutas
processed_root <- "paper1_cluster/data/processed"

in_file <- file.path(
  processed_root,
  "03_component_quality",
  "all_sources_integrated_component_quality.csv"
)

if (!file.exists(in_file)) {
  stop("No encuentro el archivo de entrada: ", in_file)
}

out_dir <- file.path(
  processed_root,
  "03_2_phase_dimension_scores"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_file <- file.path(
  out_dir,
  "all_sources_integrated_component_quality_phase_scores.csv"
)

# Lectura
df <- read_csv(
  in_file,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)


# Funciones auxiliares
clean_text <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x <- na_if(x, "")
  x <- na_if(x, "NA")
  x <- na_if(x, "NaN")
  x <- na_if(x, "NULL")
  x <- na_if(x, "null")
  x
}

parse_num_clean <- function(x) {
  suppressWarnings(
    readr::parse_number(
      as.character(x),
      locale = locale(decimal_mark = ".", grouping_mark = ",")
    )
  )
}

dim_to_colname <- function(x) {
  str_to_lower(x)
}

make_quality_label <- function(n_valid, n_expected, n_dims_selected) {
  prop_valid <- if_else(n_expected > 0, n_valid / n_expected, NA_real_)
  
  case_when(
    n_dims_selected == 0 ~ "no_dimension_selected",
    n_expected == 0 ~ "no_determinants_mapped",
    n_valid == n_expected & n_expected > 0 ~ "complete",
    prop_valid >= 0.75 ~ "usable_partial",
    prop_valid > 0 ~ "too_many_missing",
    TRUE ~ "no_valid_determinants"
  )
}


# Columnas de fases / bloques RV Decision
phase_specs <- tribble(
  ~phase_prefix, ~phase_label, ~source_column,
  
  "phase_implemented_reasons",
  "Reasons that led to implement or contract the selected technology",
  "rv_decision__what_were_the_reasons_that_led_you_to_implement_or_contract_the_selected_technology_or_energy_related_measure_please_select_the_3_most_important_for_you",
  
  "phase_more_likely_1",
  "What would make respondent more likely to implement or contract this technology - branch 1",
  "rv_decision__what_would_make_you_more_likely_to_implement_or_contract_this_technology_or_measure_please_select_the_3_most_important_for_you",
  
  "phase_more_likely_2",
  "What would make respondent more likely to implement or contract this technology - branch 2",
  "rv_decision__what_would_make_you_more_likely_to_implement_or_contract_this_technology_or_measure_please_select_the_3_most_important_for_you_1"
)

missing_phase_cols <- phase_specs$source_column[
  !phase_specs$source_column %in% names(df)
]

if (length(missing_phase_cols) > 0) {
  stop(
    "Faltan estas columnas de fase/dimensión en el dataset:\n",
    paste(missing_phase_cols, collapse = "\n")
  )
}


# Mapeo dimensión → determinantes
dimension_determinant_map <- tribble(
  ~dimension, ~det_col,
  
  # FINANCIAL
  # En la imagen: Profits, Credit Score, Added Value, Frugality y Risk Profile.
  # Risk Profile está entre Financial y Security, pero lo dejamos en Financial
  # para mantener un mapeo único 32→9.
  "FINANCIAL", "det_01_profits",
  "FINANCIAL", "det_02_credit_score",
  "FINANCIAL", "det_03_risk_profile",
  "FINANCIAL", "det_04_added_value",
  "FINANCIAL", "det_05_frugality",
  
  # SECURITY
  # En la imagen: Legal, Safety, Trust.
  "SECURITY", "det_07_legal",
  "SECURITY", "det_08_trust",
  "SECURITY", "det_09_safety",
  
  # COMPETENCE
  # En la imagen: Knowledge, Own competence, Technical fit y Cost-efficiency.
  # Por tanto, Cost-efficiency se mueve aquí.
  "COMPETENCE", "det_10_cost_efficiency",
  "COMPETENCE", "det_11_knowledge",
  "COMPETENCE", "det_12_own_competence",
  "COMPETENCE", "det_13_technical_fit",
  
  # AUTONOMY
  # En la imagen: Commitment, Adherence, Self-satisfaction y Autarky.
  # La columna det_18_autonomy corresponde al nodo de autonomía/autarky.
  "AUTONOMY", "det_15_self_satisfaction",
  "AUTONOMY", "det_16_commitment",
  "AUTONOMY", "det_17_adherence",
  "AUTONOMY", "det_18_autonomy",
  
  # PHYSIOLOGICAL / PHYSICALNESS
  # En la imagen aparece como Physicalness, pero en tu modelo lo mantenemos como PHYSIOLOGICAL.
  "PHYSIOLOGICAL", "det_19_wellbeing",
  "PHYSIOLOGICAL", "det_20_coziness",
  
  # RELATEDNESS
  # En la imagen: Support, Agreement, Rights & Duties, Socialising, Peer Pressure.
  "RELATEDNESS", "det_21_rights_and_duties",
  "RELATEDNESS", "det_22_peer_pressure",
  "RELATEDNESS", "det_23_support",
  "RELATEDNESS", "det_24_socialising",
  "RELATEDNESS", "det_25_agreement",
  
  # STIMULATION
  # En la imagen: Novelty y Fun.
  "STIMULATION", "det_26_novelty",
  "STIMULATION", "det_27_fun",
  
  # POPULARITY
  # En la imagen: Recognition/Poseur, Trends, Authority, Approval/Brag.
  "POPULARITY", "det_28_recognition",
  "POPULARITY", "det_29_trends",
  "POPULARITY", "det_30_authority",
  "POPULARITY", "det_31_approval",
  
  # MEANING
  # En la imagen: Climate Protection, Environmental Concern y Own Significance.
  "MEANING", "det_06_climate_protection",
  "MEANING", "det_14_environmental_concerns",
  "MEANING", "det_32_own_significance"
)

dimension_levels <- c(
  "FINANCIAL",
  "SECURITY",
  "COMPETENCE",
  "AUTONOMY",
  "PHYSIOLOGICAL",
  "RELATEDNESS",
  "STIMULATION",
  "POPULARITY",
  "MEANING"
)

det_cols <- dimension_determinant_map$det_col

missing_det_cols <- det_cols[!det_cols %in% names(df)]

if (length(missing_det_cols) > 0) {
  stop(
    "Faltan determinantes armonizados en el dataset:\n",
    paste(missing_det_cols, collapse = "\n")
  )
}

if (n_distinct(dimension_determinant_map$det_col) != 32) {
  stop("El mapeo dimensión-determinante no contiene exactamente 32 determinantes únicos.")
}

# Matriz numérica de determinantes
det_numeric <- df %>%
  select(all_of(det_cols)) %>%
  mutate(across(everything(), parse_num_clean))

det_mat <- as.matrix(det_numeric)
colnames(det_mat) <- det_cols

valid_det_mat <- !is.na(det_mat) & det_mat >= 0 & det_mat <= 100


# Función para crear variables por fase
add_phase_features <- function(data, phase_prefix, phase_label, phase_col) {
  
  raw_response <- clean_text(data[[phase_col]])
  response_upper <- str_to_upper(coalesce(raw_response, ""))
  
  # 1. Dimensiones seleccionadas
  selected_dim_df <- map_dfc(dimension_levels, function(dim) {
    tibble(
      !!dim := str_detect(
        response_upper,
        regex(paste0("\\b", dim, "\\b"), ignore_case = FALSE)
      )
    )
  })
  
  selected_dim_mat <- as.matrix(selected_dim_df)
  n_dims_selected <- rowSums(selected_dim_mat, na.rm = TRUE)
  
  selected_dims_collapsed <- apply(selected_dim_mat, 1, function(x) {
    dims <- dimension_levels[as.logical(x)]
    if (length(dims) == 0) {
      return(NA_character_)
    }
    paste(dims, collapse = "|")
  })
  
  has_phase_response <- n_dims_selected > 0
  
  # 2. Pesos por determinante según dimensiones seleccionadas
  det_dimension_lookup <- dimension_determinant_map %>%
    select(det_col, dimension)
  
  weight_mat <- matrix(
    NA_real_,
    nrow = nrow(data),
    ncol = length(det_cols),
    dimnames = list(NULL, det_cols)
  )
  
  for (j in seq_along(det_cols)) {
    det_j <- det_cols[j]
    dim_j <- det_dimension_lookup$dimension[
      match(det_j, det_dimension_lookup$det_col)
    ]
    
    selected_j <- selected_dim_df[[dim_j]]
    
    weight_mat[, j] <- ifelse(
      has_phase_response,
      as.numeric(selected_j),
      NA_real_
    )
  }
  
  # Si el determinante pertenece a una dimensión seleccionada:
  # weighted value = valor del determinante.
  # Si no pertenece: weighted value = 0.
  # Si no hay respuesta de dimensión: NA.
  weighted_mat <- ifelse(
    is.na(weight_mat),
    NA_real_,
    ifelse(weight_mat == 1, det_mat, 0)
  )
  
  weight_df <- as_tibble(weight_mat) %>%
    set_names(paste0(phase_prefix, "_det_weight_", det_cols))
  
  weighted_df <- as_tibble(weighted_mat) %>%
    set_names(paste0(phase_prefix, "_weighted_", det_cols))
  
  # 3. Puntuación por dimensión seleccionada
  dim_score_df <- map_dfc(dimension_levels, function(dim) {
    dim_cols <- dimension_determinant_map %>%
      filter(dimension == dim) %>%
      pull(det_col)
    
    dim_mat <- det_mat[, dim_cols, drop = FALSE]
    dim_valid_mat <- valid_det_mat[, dim_cols, drop = FALSE]
    
    n_valid_dim <- rowSums(dim_valid_mat, na.rm = TRUE)
    
    score_dim <- rowSums(
      ifelse(dim_valid_mat, dim_mat, 0),
      na.rm = TRUE
    ) / n_valid_dim
    
    score_dim[n_valid_dim == 0] <- NA_real_
    
    # Solo se informa la puntuación si esa dimensión fue seleccionada
    score_selected_dim <- ifelse(
      selected_dim_df[[dim]],
      score_dim,
      NA_real_
    )
    
    tibble(
      !!paste0(phase_prefix, "_dim_score_", dim_to_colname(dim)) :=
        score_selected_dim
    )
  })
  
  # 4. Puntuación agregada de todas las dimensiones seleccionadas
  selected_det_expected <- rowSums(weight_mat == 1, na.rm = TRUE)
  selected_det_valid <- rowSums(weight_mat == 1 & valid_det_mat, na.rm = TRUE)
  
  selected_det_score_sum <- rowSums(
    ifelse(weight_mat == 1 & valid_det_mat, det_mat, 0),
    na.rm = TRUE
  )
  
  selected_dimension_score_mean <- selected_det_score_sum / selected_det_valid
  selected_dimension_score_mean[selected_det_valid == 0] <- NA_real_
  
  selected_det_prop_valid <- ifelse(
    selected_det_expected > 0,
    selected_det_valid / selected_det_expected,
    NA_real_
  )
  
  phase_quality <- make_quality_label(
    n_valid = selected_det_valid,
    n_expected = selected_det_expected,
    n_dims_selected = n_dims_selected
  )
  
  # 5. Flags de dimensiones seleccionadas
  selected_dim_flags <- selected_dim_df %>%
    rename_with(
      ~ paste0(phase_prefix, "_selected_dim_", str_to_lower(.x))
    )
  
  # 6. Resultado de la fase
  phase_summary_df <- tibble(
    !!paste0(phase_prefix, "_source_column") := phase_col,
    !!paste0(phase_prefix, "_raw_response") := raw_response,
    !!paste0(phase_prefix, "_has_response") := has_phase_response,
    !!paste0(phase_prefix, "_selected_dimensions") := selected_dims_collapsed,
    !!paste0(phase_prefix, "_n_selected_dimensions") := n_dims_selected,
    !!paste0(phase_prefix, "_n_selected_determinants_expected") := selected_det_expected,
    !!paste0(phase_prefix, "_n_selected_determinants_valid") := selected_det_valid,
    !!paste0(phase_prefix, "_prop_selected_determinants_valid") := selected_det_prop_valid,
    !!paste0(phase_prefix, "_selected_dimension_score_mean") :=
      selected_dimension_score_mean,
    !!paste0(phase_prefix, "_quality") := phase_quality
  )
  
  bind_cols(
    phase_summary_df,
    selected_dim_flags,
    dim_score_df,
    weight_df,
    weighted_df
  )
}

# Crear columnas nuevas
phase_features <- pmap(
  phase_specs,
  function(phase_prefix, phase_label, source_column) {
    add_phase_features(
      data = df,
      phase_prefix = phase_prefix,
      phase_label = phase_label,
      phase_col = source_column
    )
  }
)

phase_features_df <- bind_cols(phase_features)

df_enriched <- bind_cols(
  df,
  phase_features_df
)


# Calidad global del bloque nuevo
phase_quality_cols <- paste0(phase_specs$phase_prefix, "_quality")
phase_has_response_cols <- paste0(phase_specs$phase_prefix, "_has_response")

df_enriched <- df_enriched %>%
  mutate(
    n_phase_dimension_blocks_with_response = rowSums(
      across(all_of(phase_has_response_cols), ~ .x == TRUE),
      na.rm = TRUE
    ),
    
    n_phase_dimension_blocks_complete = rowSums(
      across(all_of(phase_quality_cols), ~ .x == "complete"),
      na.rm = TRUE
    ),
    
    n_phase_dimension_blocks_usable = rowSums(
      across(
        all_of(phase_quality_cols),
        ~ .x %in% c("complete", "usable_partial")
      ),
      na.rm = TRUE
    ),
    
    quality_phase_dimension_block = case_when(
      n_phase_dimension_blocks_with_response == 0 ~ "no_phase_dimension_response",
      n_phase_dimension_blocks_complete == n_phase_dimension_blocks_with_response ~ "complete",
      n_phase_dimension_blocks_usable == n_phase_dimension_blocks_with_response ~ "usable_partial",
      n_phase_dimension_blocks_usable > 0 ~ "partially_usable",
      TRUE ~ "not_usable"
    )
  )

# Diagnósticos
diagnostics_phase_columns <- phase_specs %>%
  mutate(
    n_valid_raw = map_int(
      source_column,
      ~ sum(!is.na(clean_text(df[[.x]])), na.rm = TRUE)
    ),
    prop_valid_raw = n_valid_raw / nrow(df)
  )

diagnostics_phase_columns_by_source <- map_dfr(
  seq_len(nrow(phase_specs)),
  function(i) {
    phase_prefix_i <- phase_specs$phase_prefix[i]
    phase_label_i <- phase_specs$phase_label[i]
    source_column_i <- phase_specs$source_column[i]
    
    df %>%
      mutate(
        .has_response = !is.na(clean_text(.data[[source_column_i]]))
      ) %>%
      group_by(dataset_source, source_survey) %>%
      summarise(
        n_rows = n(),
        n_valid_raw = sum(.has_response, na.rm = TRUE),
        prop_valid_raw = n_valid_raw / n_rows,
        .groups = "drop"
      ) %>%
      mutate(
        phase_prefix = phase_prefix_i,
        phase_label = phase_label_i,
        source_column = source_column_i,
        .before = 1
      )
  }
)

diagnostics_phase_dimension_frequencies <- map_dfr(
  phase_specs$phase_prefix,
  function(phase_prefix) {
    map_dfr(dimension_levels, function(dim) {
      selected_col <- paste0(
        phase_prefix,
        "_selected_dim_",
        str_to_lower(dim)
      )
      
      tibble(
        phase_prefix = phase_prefix,
        dimension = dim,
        n_selected = sum(df_enriched[[selected_col]] == TRUE, na.rm = TRUE),
        prop_selected_total = n_selected / nrow(df_enriched)
      )
    })
  }
) %>%
  arrange(phase_prefix, desc(n_selected))

diagnostics_phase_quality_counts <- df_enriched %>%
  select(
    dataset_source,
    source_survey,
    all_of(phase_quality_cols),
    quality_phase_dimension_block
  ) %>%
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

diagnostics_phase_scores_summary <- df_enriched %>%
  select(
    dataset_source,
    source_survey,
    ends_with("_selected_dimension_score_mean")
  ) %>%
  pivot_longer(
    cols = ends_with("_selected_dimension_score_mean"),
    names_to = "score_variable",
    values_to = "score"
  ) %>%
  mutate(score = parse_num_clean(score)) %>%
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


# Guardado
write_csv(
  df_enriched,
  out_file
)

write_csv(
  dimension_determinant_map,
  file.path(out_dir, "dimension_determinant_mapping.csv")
)

write_csv(
  phase_specs,
  file.path(out_dir, "phase_dimension_source_columns.csv")
)

write_csv(
  diagnostics_phase_columns,
  file.path(out_dir, "diagnostics_phase_columns.csv")
)

write_csv(
  diagnostics_phase_columns_by_source,
  file.path(out_dir, "diagnostics_phase_columns_by_source.csv")
)

write_csv(
  diagnostics_phase_dimension_frequencies,
  file.path(out_dir, "diagnostics_phase_dimension_frequencies.csv")
)

write_csv(
  diagnostics_phase_quality_counts,
  file.path(out_dir, "diagnostics_phase_quality_counts.csv")
)

write_csv(
  diagnostics_phase_scores_summary,
  file.path(out_dir, "diagnostics_phase_scores_summary.csv")
)

# Resumen en consola
cat("\n============================================================\n")
cat("Columnas de fases usadas\n")
cat("============================================================\n")
print(diagnostics_phase_columns, n = Inf, width = Inf)

cat("\n============================================================\n")
cat("Cobertura de columnas de fases por fuente / encuesta\n")
cat("============================================================\n")
print(diagnostics_phase_columns_by_source, n = Inf, width = Inf)

cat("\n============================================================\n")
cat("Frecuencia de dimensiones seleccionadas por fase\n")
cat("============================================================\n")
print(diagnostics_phase_dimension_frequencies, n = Inf, width = Inf)

cat("\n============================================================\n")
cat("Calidad del nuevo bloque por fuente / encuesta\n")
cat("============================================================\n")
print(diagnostics_phase_quality_counts, n = Inf, width = Inf)

cat("\n============================================================\n")
cat("Resumen de puntuaciones de dimensiones seleccionadas\n")
cat("============================================================\n")
print(diagnostics_phase_scores_summary, n = Inf, width = Inf)

message("Listo.")
message("Dataset enriquecido: ", out_file)
message("Mapeo dimensiones-determinantes: ", file.path(out_dir, "dimension_determinant_mapping.csv"))
message("Columnas de fase usadas: ", file.path(out_dir, "phase_dimension_source_columns.csv"))
message("Diagnóstico frecuencia dimensiones: ", file.path(out_dir, "diagnostics_phase_dimension_frequencies.csv"))
message("Diagnóstico calidad bloque nuevo: ", file.path(out_dir, "diagnostics_phase_quality_counts.csv"))
message("Diagnóstico puntuaciones: ", file.path(out_dir, "diagnostics_phase_scores_summary.csv"))