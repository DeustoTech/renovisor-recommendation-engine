# 
# Objetivo
# Scores de dimensiones por fase
#
# Añade al dataset de calidad variables derivadas de los bloques de
# decisión de RV Decision y de los 32 determinantes armonizados
#
# Fases utilizadas:
#   phase_implemented_reasons -> Implementada
#   phase_more_likely_1       -> La conoce / la consideraría
#   phase_more_likely_2       -> No la conoce, pero le genera curiosidad
#
# Para cada fase:
#   1. Detecta las dimensiones seleccionadas.
#   2. Identifica los determinantes asociados.
#   3. Calcula scores por dimensión.
#   4. Calcula un score agregado de los determinantes seleccionados.
#   5. Evalúa la calidad según la disponibilidad de determinantes.
#
# Los bloques de fase proceden de RV Decision, por lo que se esperan
# principalmente en RENOVISOR. DIEGO, WHY_EUROPE y WHY_LATAM se
# mantienen en el dataset aunque no tengan respuestas de fase.
#

suppressPackageStartupMessages({
  library(tidyverse)
})


# Configuración
processed_root <- "paper1_cluster/data/processed"

in_file <- file.path(
  processed_root,
  "03_component_quality",
  "all_sources_integrated_component_quality.csv"
)

out_dir <- file.path(
  processed_root,
  "03_2_phase_dimension_scores"
)

out_file <- file.path(
  out_dir,
  "all_sources_integrated_component_quality_phase_scores.csv"
)

if (!file.exists(in_file)) {
  stop("No encuentro el archivo de entrada: ", in_file)
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

df <- read_csv(
  in_file,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)


# Comprobaciones
required_traceability_cols <- c(
  "integrated_row_id",
  "subsample",
  "comparison_region",
  "dataset_source",
  "source_survey"
)

missing_traceability_cols <- setdiff(
  required_traceability_cols,
  names(df)
)

if (length(missing_traceability_cols)) {
  stop(
    "Faltan columnas de trazabilidad: ",
    paste(missing_traceability_cols, collapse = ", ")
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
    paste(missing_subsamples, collapse = ", ")
  )
}

if (anyDuplicated(df$integrated_row_id)) {
  stop("integrated_row_id contiene duplicados en el input.")
}

input_traceability <- df %>%
  select(all_of(required_traceability_cols))


# Funciones auxiliares
clean_text <- function(x) {
  x <- str_squish(as.character(x))
  
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
  
  x[is.na(x) | x %in% invalid_values] <- NA_character_
  x
}


parse_num_clean <- function(x) {
  suppressWarnings(
    parse_number(
      as.character(x),
      locale = locale(
        decimal_mark = ".",
        grouping_mark = ","
      )
    )
  )
}


make_quality_label <- function(
    n_valid,
    n_expected,
    n_dims_selected
) {
  prop_valid <- if_else(
    n_expected > 0,
    n_valid / n_expected,
    NA_real_
  )
  
  case_when(
    n_dims_selected == 0 ~ "no_dimension_selected",
    n_expected == 0 ~ "no_determinants_mapped",
    n_valid == n_expected & n_expected > 0 ~ "complete",
    prop_valid >= 0.75 ~ "usable_partial",
    prop_valid > 0 ~ "too_many_missing",
    TRUE ~ "no_valid_determinants"
  )
}


# Fases
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

missing_phase_cols <- setdiff(
  phase_specs$source_column,
  names(df)
)

if (length(missing_phase_cols)) {
  stop(
    "Faltan columnas de fase/dimensión: ",
    paste(missing_phase_cols, collapse = ", ")
  )
}


# Dimensiones y determinantes
dimension_determinants <- list(
  FINANCIAL = c(
    "det_01_profits",
    "det_02_credit_score",
    "det_03_risk_profile",
    "det_04_added_value",
    "det_05_frugality"
  ),
  
  SECURITY = c(
    "det_07_legal",
    "det_08_trust",
    "det_09_safety"
  ),
  
  COMPETENCE = c(
    "det_10_cost_efficiency",
    "det_11_knowledge",
    "det_12_own_competence",
    "det_13_technical_fit"
  ),
  
  AUTONOMY = c(
    "det_15_self_satisfaction",
    "det_16_commitment",
    "det_17_adherence",
    "det_18_autonomy"
  ),
  
  PHYSIOLOGICAL = c(
    "det_19_wellbeing",
    "det_20_coziness"
  ),
  
  RELATEDNESS = c(
    "det_21_rights_and_duties",
    "det_22_peer_pressure",
    "det_23_support",
    "det_24_socialising",
    "det_25_agreement"
  ),
  
  STIMULATION = c(
    "det_26_novelty",
    "det_27_fun"
  ),
  
  POPULARITY = c(
    "det_28_recognition",
    "det_29_trends",
    "det_30_authority",
    "det_31_approval"
  ),
  
  MEANING = c(
    "det_06_climate_protection",
    "det_14_environmental_concerns",
    "det_32_own_significance"
  )
)

dimension_levels <- names(dimension_determinants)

dimension_determinant_map <- enframe(
  dimension_determinants,
  name = "dimension",
  value = "det_col"
) %>%
  unnest_longer(det_col)

det_cols <- dimension_determinant_map$det_col

missing_det_cols <- setdiff(
  det_cols,
  names(df)
)

if (length(missing_det_cols)) {
  stop(
    "Faltan determinantes armonizados: ",
    paste(missing_det_cols, collapse = ", ")
  )
}

if (n_distinct(det_cols) != 32) {
  stop("El mapeo no contiene exactamente 32 determinantes únicos.")
}

if (n_distinct(dimension_determinant_map$dimension) != 9) {
  stop("El mapeo no contiene exactamente 9 dimensiones.")
}

det_mat <- df %>%
  select(all_of(det_cols)) %>%
  mutate(across(everything(), parse_num_clean)) %>%
  as.matrix()

colnames(det_mat) <- det_cols

valid_det_mat <-
  !is.na(det_mat) &
  det_mat >= 0 &
  det_mat <= 100

det_dimensions <- dimension_determinant_map$dimension[
  match(
    det_cols,
    dimension_determinant_map$det_col
  )
]


# Features de fase
add_phase_features <- function(
    data,
    phase_prefix,
    phase_label,
    phase_col
) {
  raw_response <- clean_text(
    data[[phase_col]]
  )
  
  response_upper <- str_to_upper(
    coalesce(
      raw_response,
      ""
    )
  )
  
  selected_dim_df <- map_dfc(
    dimension_levels,
    function(dim_current) {
      tibble(
        !!dim_current := str_detect(
          response_upper,
          regex(
            paste0(
              "\\b",
              dim_current,
              "\\b"
            ),
            ignore_case = FALSE
          )
        )
      )
    }
  )
  
  selected_dim_mat <- as.matrix(
    selected_dim_df
  )
  
  n_dims_selected <- rowSums(
    selected_dim_mat,
    na.rm = TRUE
  )
  
  selected_dims_collapsed <- apply(
    selected_dim_mat,
    1,
    function(x) {
      selected <- dimension_levels[
        as.logical(x)
      ]
      
      if (!length(selected)) {
        return(NA_character_)
      }
      
      paste(
        selected,
        collapse = "|"
      )
    }
  )
  
  has_phase_response <-
    n_dims_selected > 0
  
  weight_mat <- vapply(
    det_dimensions,
    function(dim_current) {
      as.numeric(selected_dim_df[[dim_current]])
    },
    numeric(
      nrow(data)
    )
  )
  
  colnames(weight_mat) <- det_cols
  weight_mat[!has_phase_response, ] <- NA_real_
  
  weighted_mat <- ifelse(
    is.na(weight_mat),
    NA_real_,
    ifelse(
      weight_mat == 1,
      det_mat,
      0
    )
  )
  
  weight_df <- as_tibble(
    weight_mat
  )
  
  names(weight_df) <- paste0(
    phase_prefix,
    "_det_weight_",
    det_cols
  )
  
  weighted_df <- as_tibble(
    weighted_mat
  )
  
  names(weighted_df) <- paste0(
    phase_prefix,
    "_weighted_",
    det_cols
  )
  
  dim_score_df <- map_dfc(
    dimension_levels,
    function(dim_current) {
      dim_cols <- dimension_determinants[[dim_current]]
      
      dim_values <- det_mat[
        ,
        dim_cols,
        drop = FALSE
      ]
      
      dim_valid <- valid_det_mat[
        ,
        dim_cols,
        drop = FALSE
      ]
      
      n_valid_dim <- rowSums(
        dim_valid,
        na.rm = TRUE
      )
      
      dim_sum <- rowSums(
        ifelse(
          dim_valid,
          dim_values,
          0
        ),
        na.rm = TRUE
      )
      
      score_dim <- rep(
        NA_real_,
        nrow(data)
      )
      
      valid_rows <-
        n_valid_dim > 0
      
      score_dim[valid_rows] <-
        dim_sum[valid_rows] /
        n_valid_dim[valid_rows]
      
      score_selected <- ifelse(
        selected_dim_df[[dim_current]],
        score_dim,
        NA_real_
      )
      
      tibble(
        !!paste0(
          phase_prefix,
          "_dim_score_",
          str_to_lower(
            dim_current
          )
        ) := score_selected
      )
    }
  )
  
  selected_det_expected <- rowSums(
    weight_mat == 1,
    na.rm = TRUE
  )
  
  selected_det_valid <- rowSums(
    weight_mat == 1 &
      valid_det_mat,
    na.rm = TRUE
  )
  
  selected_det_score_sum <- rowSums(
    ifelse(
      weight_mat == 1 &
        valid_det_mat,
      det_mat,
      0
    ),
    na.rm = TRUE
  )
  
  selected_dimension_score_mean <- rep(
    NA_real_,
    nrow(data)
  )
  
  valid_score_rows <-
    selected_det_valid > 0
  
  selected_dimension_score_mean[
    valid_score_rows
  ] <-
    selected_det_score_sum[
      valid_score_rows
    ] /
    selected_det_valid[
      valid_score_rows
    ]
  
  selected_det_prop_valid <- ifelse(
    selected_det_expected > 0,
    selected_det_valid /
      selected_det_expected,
    NA_real_
  )
  
  phase_quality <- make_quality_label(
    selected_det_valid,
    selected_det_expected,
    n_dims_selected
  )
  
  selected_dim_flags <- selected_dim_df %>%
    rename_with(
      ~ paste0(
        phase_prefix,
        "_selected_dim_",
        str_to_lower(.x)
      )
    )
  
  phase_summary_df <- tibble(
    !!paste0(
      phase_prefix,
      "_source_column"
    ) := rep(
      phase_col,
      nrow(data)
    ),
    
    !!paste0(
      phase_prefix,
      "_phase_label"
    ) := rep(
      phase_label,
      nrow(data)
    ),
    
    !!paste0(
      phase_prefix,
      "_raw_response"
    ) := raw_response,
    
    !!paste0(
      phase_prefix,
      "_has_response"
    ) := has_phase_response,
    
    !!paste0(
      phase_prefix,
      "_selected_dimensions"
    ) := selected_dims_collapsed,
    
    !!paste0(
      phase_prefix,
      "_n_selected_dimensions"
    ) := n_dims_selected,
    
    !!paste0(
      phase_prefix,
      "_n_selected_determinants_expected"
    ) := selected_det_expected,
    
    !!paste0(
      phase_prefix,
      "_n_selected_determinants_valid"
    ) := selected_det_valid,
    
    !!paste0(
      phase_prefix,
      "_prop_selected_determinants_valid"
    ) := selected_det_prop_valid,
    
    !!paste0(
      phase_prefix,
      "_selected_dimension_score_mean"
    ) := selected_dimension_score_mean,
    
    !!paste0(
      phase_prefix,
      "_quality"
    ) := phase_quality
  )
  
  bind_cols(
    phase_summary_df,
    selected_dim_flags,
    dim_score_df,
    weight_df,
    weighted_df
  )
}


# Crear features
phase_features_df <- pmap(
  phase_specs,
  function(
    phase_prefix,
    phase_label,
    source_column
  ) {
    add_phase_features(
      df,
      phase_prefix,
      phase_label,
      source_column
    )
  }
) %>%
  bind_cols()

if (nrow(phase_features_df) != nrow(df)) {
  stop(
    "phase_features_df no tiene el mismo número de filas que el input."
  )
}

df_enriched <- bind_cols(
  df,
  phase_features_df
)

phase_quality_cols <- paste0(
  phase_specs$phase_prefix,
  "_quality"
)

phase_has_response_cols <- paste0(
  phase_specs$phase_prefix,
  "_has_response"
)

df_enriched <- df_enriched %>%
  mutate(
    n_phase_dimension_blocks_with_response = rowSums(
      across(
        all_of(
          phase_has_response_cols
        ),
        ~ .x == TRUE
      ),
      na.rm = TRUE
    ),
    
    n_phase_dimension_blocks_complete = rowSums(
      across(
        all_of(
          phase_quality_cols
        ),
        ~ .x == "complete"
      ),
      na.rm = TRUE
    ),
    
    n_phase_dimension_blocks_usable = rowSums(
      across(
        all_of(
          phase_quality_cols
        ),
        ~ .x %in%
          c(
            "complete",
            "usable_partial"
          )
      ),
      na.rm = TRUE
    ),
    
    quality_phase_dimension_block = case_when(
      n_phase_dimension_blocks_with_response == 0 ~
        "no_phase_dimension_response",
      
      n_phase_dimension_blocks_complete ==
        n_phase_dimension_blocks_with_response ~
        "complete",
      
      n_phase_dimension_blocks_usable ==
        n_phase_dimension_blocks_with_response ~
        "usable_partial",
      
      n_phase_dimension_blocks_usable > 0 ~
        "partially_usable",
      
      TRUE ~
        "not_usable"
    )
  )


# Trazabilidad
if (nrow(df_enriched) != nrow(df)) {
  stop(
    "El número de filas ha cambiado. Input = ",
    nrow(df),
    "; output = ",
    nrow(df_enriched)
  )
}

if (anyDuplicated(df_enriched$integrated_row_id)) {
  stop(
    "Se han creado integrated_row_id duplicados."
  )
}

if (
  !identical(
    as.character(
      df$integrated_row_id
    ),
    as.character(
      df_enriched$integrated_row_id
    )
  )
) {
  stop(
    "El orden o los valores de integrated_row_id han cambiado."
  )
}

output_traceability <- df_enriched %>%
  select(
    all_of(
      required_traceability_cols
    )
  )

if (
  !identical(
    input_traceability,
    output_traceability
  )
) {
  stop(
    "La trazabilidad de las filas ha cambiado."
  )
}


# Diagnósticos
diagnostics_dataset_integrity <- tibble(
  metric = c(
    "n_input_rows",
    "n_output_rows",
    "n_input_unique_ids",
    "n_output_unique_ids",
    "n_sources",
    "n_subsamples"
  ),
  
  value = c(
    nrow(df),
    nrow(df_enriched),
    n_distinct(
      df$integrated_row_id
    ),
    n_distinct(
      df_enriched$integrated_row_id
    ),
    n_distinct(
      df_enriched$dataset_source
    ),
    n_distinct(
      df_enriched$subsample
    )
  )
)

diagnostics_subsample_counts <- df_enriched %>%
  count(
    comparison_region,
    subsample,
    dataset_source,
    name = "n_rows"
  ) %>%
  arrange(
    comparison_region,
    subsample
  )

diagnostics_subsample_integrity <- full_join(
  input_traceability %>%
    count(
      comparison_region,
      subsample,
      name = "n_input"
    ),
  
  output_traceability %>%
    count(
      comparison_region,
      subsample,
      name = "n_output"
    ),
  
  by = c(
    "comparison_region",
    "subsample"
  )
) %>%
  mutate(
    n_input = replace_na(
      n_input,
      0L
    ),
    
    n_output = replace_na(
      n_output,
      0L
    ),
    
    same_n =
      n_input ==
      n_output
  ) %>%
  arrange(
    comparison_region,
    subsample
  )

if (
  any(
    !diagnostics_subsample_integrity$same_n
  )
) {
  stop(
    "El número de filas por submuestra ha cambiado."
  )
}

diagnostics_phase_columns <- phase_specs %>%
  mutate(
    n_valid_raw = map_int(
      source_column,
      ~ sum(
        !is.na(
          clean_text(
            df[[.x]])
        ),
        na.rm = TRUE
      )
    ),
    
    prop_valid_raw =
      n_valid_raw /
      nrow(df)
  )


phase_coverage <- function(
    data,
    grouping_vars
) {
  map_dfr(
    seq_len(
      nrow(
        phase_specs
      )
    ),
    function(i) {
      current <- phase_specs[
        i,
      ]
      
      data %>%
        mutate(
          .has_raw_response = !is.na(
            clean_text(.data[[current$source_column]]
            )
          )
        ) %>%
        group_by(
          across(
            all_of(
              grouping_vars
            )
          )
        ) %>%
        summarise(
          n_rows = n(),
          
          n_valid_raw = sum(
            .has_raw_response,
            na.rm = TRUE
          ),
          
          prop_valid_raw =
            n_valid_raw /
            n_rows,
          
          .groups = "drop"
        ) %>%
        mutate(
          phase_prefix =
            current$phase_prefix,
          
          phase_label =
            current$phase_label,
          
          source_column =
            current$source_column,
          
          .before = 1
        )
    }
  )
}


diagnostics_phase_columns_by_subsample <- phase_coverage(
  df,
  c(
    "comparison_region",
    "subsample",
    "dataset_source"
  )
)

diagnostics_phase_columns_by_source <- phase_coverage(
  df,
  c(
    "dataset_source",
    "source_survey"
  )
)


# POOLED_ALL se utiliza aquí únicamente como resumen pre-bootstrap
df_phase_analysis <- bind_rows(
  df_enriched %>%
    mutate(
      analysis_sample = "POOLED_ALL",
      analysis_region = "ALL"
    ),
  
  df_enriched %>%
    mutate(
      analysis_sample = subsample,
      analysis_region = comparison_region
    )
)

diagnostics_phase_response_by_sample <- df_phase_analysis %>%
  group_by(
    analysis_sample,
    analysis_region
  ) %>%
  summarise(
    n_rows = n(),
    
    n_with_any_phase_response = sum(
      n_phase_dimension_blocks_with_response > 0,
      na.rm = TRUE
    ),
    
    prop_with_any_phase_response =
      n_with_any_phase_response /
      n_rows,
    
    n_with_1_phase_response = sum(
      n_phase_dimension_blocks_with_response == 1,
      na.rm = TRUE
    ),
    
    n_with_2_phase_responses = sum(
      n_phase_dimension_blocks_with_response == 2,
      na.rm = TRUE
    ),
    
    n_with_3_phase_responses = sum(
      n_phase_dimension_blocks_with_response == 3,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


diagnostics_phase_dimension_frequencies <- expand_grid(
  phase_prefix =
    phase_specs$phase_prefix,
  dimension =
    dimension_levels
) %>%
  mutate(
    n_selected = map2_int(
      phase_prefix,
      dimension,
      ~ {
        selected_col <- paste0(
          .x,
          "_selected_dim_",
          str_to_lower(.y)
        )
        
        sum(
          df_enriched[[selected_col]] == TRUE,
          na.rm = TRUE
        )
      }
    ),
    
    prop_selected_total =
      n_selected /
      nrow(df_enriched)
  ) %>%
  arrange(
    phase_prefix,
    desc(n_selected)
  )


diagnostics_phase_dimension_frequencies_by_subsample <- map_dfr(
  phase_specs$phase_prefix,
  function(phase_prefix_current) {
    map_dfr(
      dimension_levels,
      function(dim_current) {
        selected_col <- paste0(
          phase_prefix_current,
          "_selected_dim_",
          str_to_lower(
            dim_current
          )
        )
        
        df_enriched %>%
          group_by(
            comparison_region,
            subsample
          ) %>%
          summarise(
            n_rows = n(),
            
            n_selected = sum(.data[[selected_col]] == TRUE,
              na.rm = TRUE
            ),
            
            prop_selected_total =
              n_selected /
              n_rows,
            
            .groups = "drop"
          ) %>%
          mutate(
            phase_prefix =
              phase_prefix_current,
            
            dimension =
              dim_current,
            
            .before = 1
          )
      }
    )
  }
) %>%
  arrange(
    subsample,
    phase_prefix,
    desc(n_selected)
  )


quality_count_table <- function(
    data,
    select_vars,
    count_vars,
    prop_group_vars,
    arrange_vars
) {
  data %>%
    select(
      all_of(select_vars),
      all_of(phase_quality_cols),
      quality_phase_dimension_block
    ) %>%
    pivot_longer(
      cols = c(
        all_of(
          phase_quality_cols
        ),
        quality_phase_dimension_block
      ),
      names_to = "quality_variable",
      values_to = "quality_label"
    ) %>%
    count(
      across(
        all_of(
          count_vars
        )
      ),
      quality_variable,
      quality_label,
      name = "n"
    ) %>%
    group_by(
      across(
        all_of(
          prop_group_vars
        )
      ),
      quality_variable
    ) %>%
    mutate(
      prop =
        n /
        sum(n)
    ) %>%
    ungroup() %>%
    arrange(
      across(
        all_of(
          arrange_vars
        )
      ),
      quality_variable,
      desc(n)
    )
}


diagnostics_phase_quality_counts_by_subsample <- quality_count_table(
  df_enriched,
  
  select_vars = c(
    "comparison_region",
    "subsample",
    "dataset_source",
    "source_survey"
  ),
  
  count_vars = c(
    "comparison_region",
    "subsample",
    "dataset_source",
    "source_survey"
  ),
  
  prop_group_vars = c(
    "subsample",
    "source_survey"
  ),
  
  arrange_vars = c(
    "subsample",
    "source_survey"
  )
)


diagnostics_phase_quality_counts_by_source <- quality_count_table(
  df_enriched,
  
  select_vars = c(
    "dataset_source",
    "source_survey"
  ),
  
  count_vars = c(
    "dataset_source",
    "source_survey"
  ),
  
  prop_group_vars = c(
    "dataset_source",
    "source_survey"
  ),
  
  arrange_vars = c(
    "dataset_source",
    "source_survey"
  )
)


summarise_score_table <- function(
    data,
    grouping_vars
) {
  score_cols <- names(data)[
    str_detect(
      names(data),
      "_selected_dimension_score_mean$"
    )
  ]
  
  data %>%
    select(
      all_of(grouping_vars),
      all_of(score_cols)
    ) %>%
    pivot_longer(
      cols = all_of(
        score_cols
      ),
      names_to = "score_variable",
      values_to = "score"
    ) %>%
    mutate(
      score = parse_num_clean(
        score
      )
    ) %>%
    group_by(
      across(
        all_of(
          grouping_vars
        )
      ),
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
        is.nan(mean_score),
        NA_real_,
        mean_score
      ),
      
      min_score = if_else(
        is.infinite(min_score),
        NA_real_,
        min_score
      ),
      
      max_score = if_else(
        is.infinite(max_score),
        NA_real_,
        max_score
      )
    )
}


diagnostics_phase_scores_summary_by_subsample <- summarise_score_table(
  df_enriched,
  c(
    "comparison_region",
    "subsample",
    "dataset_source",
    "source_survey"
  )
) %>%
  arrange(
    subsample,
    source_survey,
    score_variable
  )

diagnostics_phase_scores_summary_by_source <- summarise_score_table(
  df_enriched,
  c(
    "dataset_source",
    "source_survey"
  )
) %>%
  arrange(
    dataset_source,
    source_survey,
    score_variable
  )


# Guardado
outputs <- list(
  "all_sources_integrated_component_quality_phase_scores.csv" =
    df_enriched,
  
  "dimension_determinant_mapping.csv" =
    dimension_determinant_map,
  
  "phase_dimension_source_columns.csv" =
    phase_specs,
  
  "diagnostics_dataset_integrity.csv" =
    diagnostics_dataset_integrity,
  
  "diagnostics_subsample_counts.csv" =
    diagnostics_subsample_counts,
  
  "diagnostics_subsample_integrity.csv" =
    diagnostics_subsample_integrity,
  
  "diagnostics_phase_columns.csv" =
    diagnostics_phase_columns,
  
  "diagnostics_phase_columns_by_subsample.csv" =
    diagnostics_phase_columns_by_subsample,
  
  "diagnostics_phase_columns_by_source.csv" =
    diagnostics_phase_columns_by_source,
  
  "diagnostics_phase_response_by_sample.csv" =
    diagnostics_phase_response_by_sample,
  
  "diagnostics_phase_dimension_frequencies.csv" =
    diagnostics_phase_dimension_frequencies,
  
  "diagnostics_phase_dimension_frequencies_by_subsample.csv" =
    diagnostics_phase_dimension_frequencies_by_subsample,
  
  "diagnostics_phase_quality_counts_by_subsample.csv" =
    diagnostics_phase_quality_counts_by_subsample,
  
  "diagnostics_phase_quality_counts_by_source.csv" =
    diagnostics_phase_quality_counts_by_source,
  
  "diagnostics_phase_scores_summary_by_subsample.csv" =
    diagnostics_phase_scores_summary_by_subsample,
  
  "diagnostics_phase_scores_summary_by_source.csv" =
    diagnostics_phase_scores_summary_by_source
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
cat("\nINTEGRIDAD DEL DATASET\n")

print(
  diagnostics_dataset_integrity,
  n = Inf,
  width = Inf
)

cat("\nSUBMUESTRAS INPUT / OUTPUT\n")

print(
  diagnostics_subsample_integrity,
  n = Inf,
  width = Inf
)

cat("\nCOLUMNAS DE FASE USADAS\n")

print(
  diagnostics_phase_columns,
  n = Inf,
  width = Inf
)

cat("\nCOBERTURA DE FASE POR SUBMUESTRA\n")

print(
  diagnostics_phase_columns_by_subsample,
  n = Inf,
  width = Inf
)

cat("\nRESPUESTAS DE FASE POR MUESTRA\n")

print(
  diagnostics_phase_response_by_sample,
  n = Inf,
  width = Inf
)

cat("\nFRECUENCIA DE DIMENSIONES SELECCIONADAS\n")

print(
  diagnostics_phase_dimension_frequencies,
  n = Inf,
  width = Inf
)

cat("\nCALIDAD DE FASE POR SUBMUESTRA\n")

print(
  diagnostics_phase_quality_counts_by_subsample,
  n = Inf,
  width = Inf
)

cat("\nSCORES DE FASE POR SUBMUESTRA\n")

print(
  diagnostics_phase_scores_summary_by_subsample,
  n = Inf,
  width = Inf
)

cat(
  "\nFilas input: ",
  nrow(df),
  "\nFilas output: ",
  nrow(df_enriched),
  "\nIDs únicos input: ",
  n_distinct(
    df$integrated_row_id
  ),
  "\nIDs únicos output: ",
  n_distinct(
    df_enriched$integrated_row_id
  ),
  "\n",
  sep = ""
)

message(
  "\nListo. Dataset enriquecido guardado en: ",
  out_file
)