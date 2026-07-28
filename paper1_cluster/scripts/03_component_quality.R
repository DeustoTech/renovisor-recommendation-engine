
# Objetivo:
# Evaluar la calidad por componentes del dataset integrado y armonizado.
#
# Este script debe ejecutarse DESPUÉS de:
# 01_mergeData.R
# 02_harmonize_common_variables.R
#
# Input principal:
# paper1_cluster/data/processed/01_1_harmonize_sociodemographics/all_sources_integrated_clean_traceability.csv
#
# Outputs:
# paper1_cluster/data/processed/03_component_quality/all_sources_integrated_component_quality.csv
# paper1_cluster/data/processed/03_component_quality/matrix_32det_with_quality.csv
# paper1_cluster/data/processed/03_component_quality/matrix_32det_for_clustering.csv
# paper1_cluster/data/processed/03_component_quality/diagnostics_component_candidate_columns.csv
# paper1_cluster/data/processed/03_component_quality/diagnostics_component_quality_counts.csv
# paper1_cluster/data/processed/03_component_quality/diagnostics_component_coverage_by_source.csv
# paper1_cluster/data/processed/03_component_quality/diagnostics_row_quality_by_source.csv
# paper1_cluster/data/processed/03_component_quality/diagnostics_row_quality_counts.csv
# paper1_cluster/data/processed/03_component_quality/diagnostics_missing_by_determinant.csv
# paper1_cluster/data/processed/03_component_quality/diagnostics_metadata_quality_counts.csv

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(stringr)
})


# Configuración
processed_root <- "paper1_cluster/data/processed"

in_file <- file.path(
  processed_root,
  "01_1_harmonize_sociodemographics",
  "all_sources_integrated_clean_traceability.csv"
)

if (!file.exists(in_file)) {
  stop("No encuentro el archivo de entrada: ", in_file)
}

out_dir <- file.path(
  processed_root,
  "03_component_quality"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Umbrales de calidad para los 32 determinantes
N_DET_TOTAL <- 32
MIN_DET_VALID_FOR_CLUSTERING <- 24       # 75% de los determinantes
LOW_VARIABILITY_MAX_UNIQUE <- 2          # muy poca variación en la fila
HIGH_EXTREME_SHARE_THRESHOLD <- 0.80     # demasiados 0/100


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
  x <- na_if(x, "None")
  x <- na_if(x, "none")
  x <- na_if(x, "DATA_EXPIRED")
  x <- na_if(x, "data_expired")
  
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

is_non_missing_raw <- function(x) {
  x <- clean_text(x)
  !is.na(x)
}

is_valid_model_value <- function(x) {
  x <- clean_text(x)
  
  !is.na(x) &
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

safe_chr_col <- function(data, col) {
  if (col %in% names(data)) {
    as.character(data[[col]])
  } else {
    rep(NA_character_, nrow(data))
  }
}

safe_num_col <- function(data, col) {
  if (col %in% names(data)) {
    parse_num_clean(data[[col]])
  } else {
    rep(NA_real_, nrow(data))
  }
}

count_valid_model_cols <- function(data, cols) {
  cols <- cols[cols %in% names(data)]
  
  if (length(cols) == 0) {
    return(rep(0L, nrow(data)))
  }
  
  mat <- data %>%
    select(all_of(cols)) %>%
    mutate(across(everything(), is_valid_model_value)) %>%
    as.data.frame()
  
  rowSums(mat, na.rm = TRUE)
}

find_component_cols <- function(data, pattern, exclude = NULL) {
  out <- names(data)[
    str_detect(names(data), regex(pattern, ignore_case = TRUE))
  ]
  
  if (!is.null(exclude)) {
    out <- out[
      !str_detect(out, regex(exclude, ignore_case = TRUE))
    ]
  }
  
  unique(out)
}

component_stats <- function(data, component_name, cols) {
  cols <- unique(cols)
  cols <- cols[cols %in% names(data)]
  
  n_col_name <- paste0("n_", component_name, "_non_missing")
  prop_col_name <- paste0("prop_", component_name, "_non_missing")
  quality_col_name <- paste0("quality_", component_name)
  
  if (length(cols) == 0) {
    return(
      tibble(
        !!n_col_name := rep(0L, nrow(data)),
        !!prop_col_name := rep(NA_real_, nrow(data)),
        !!quality_col_name := rep("not_found_in_dataset", nrow(data))
      )
    )
  }
  
  tmp <- data %>%
    select(all_of(cols)) %>%
    mutate(across(everything(), is_non_missing_raw)) %>%
    as.data.frame()
  
  n_non_missing <- rowSums(tmp, na.rm = TRUE)
  prop_non_missing <- n_non_missing / length(cols)
  
  source_expected <- data %>%
    mutate(.n_component = n_non_missing) %>%
    group_by(dataset_source) %>%
    summarise(
      .source_has_component = any(.n_component > 0, na.rm = TRUE),
      .groups = "drop"
    )
  
  source_expected_vec <- data$dataset_source %in%
    source_expected$dataset_source[source_expected$.source_has_component]
  
  quality <- case_when(
    !source_expected_vec ~ "not_available_for_source",
    n_non_missing == 0 ~ "missing_component",
    prop_non_missing >= 0.80 ~ "component_complete",
    prop_non_missing > 0 ~ "component_partial",
    TRUE ~ "missing_component"
  )
  
  tibble(
    !!n_col_name := n_non_missing,
    !!prop_col_name := prop_non_missing,
    !!quality_col_name := quality
  )
}


# Identificar columnas por componente
generic_exclude <- paste(
  c(
    "^det_",
    "^diego_det_",
    "_model$",
    "_raw_clean$",
    "_conflict$",
    "^n_det",
    "^prop_det",
    "quality",
    "diagnostic",
    "integrated_row_id",
    "global_participant_key",
    "participant_key",
    "source_file",
    "source_row"
  ),
  collapse = "|"
)

det_cols <- names(df)[
  str_detect(names(df), "^det_\\d{2}_")
]

sociodemographic_model_cols <- c(
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
  "language_model"
)

vote_politics_cols <- c(
  "vote_status_declared",
  "voted_observed",
  "political_left_right_model",
  "political_block_model"
)

technology_adoption_cols <- find_component_cols(
  df,
  paste(
    c(
      "stage",
      "adoption",
      "technology",
      "heat_pump",
      "heat pump",
      "solar",
      "photovoltaic",
      "\\bpv\\b",
      "insulation",
      "efficient_appliances",
      "efficient appliances",
      "smart_home",
      "smart home",
      "tariff_change",
      "tariff change",
      "energy_community",
      "energy community",
      "battery_storage",
      "battery storage",
      "ventilation_recovery",
      "ventilation recovery",
      "implemented",
      "consider"
    ),
    collapse = "|"
  ),
  exclude = generic_exclude
)

tariffs_costs_payback_cols <- find_component_cols(
  df,
  paste(
    c(
      "tariff",
      "bill",
      "cost",
      "price",
      "payback",
      "subsid",
      "grant",
      "investment",
      "saving",
      "electricity",
      "gas",
      "heating",
      "energy_cost",
      "monthly"
    ),
    collapse = "|"
  ),
  exclude = generic_exclude
)

concerns_barriers_cols <- unique(c(
  names(df)[str_starts(names(df), "rv_concerns2__")],
  find_component_cols(
    df,
    paste(
      c(
        "concern",
        "barrier",
        "worry",
        "fear",
        "privacy",
        "control",
        "dependence",
        "supplier",
        "corporation",
        "comfort",
        "installation",
        "risk"
      ),
      collapse = "|"
    ),
    exclude = generic_exclude
  )
))

trust_information_cols <- find_component_cols(
  df,
  paste(
    c(
      "trust",
      "information",
      "source",
      "advisor",
      "expert",
      "professional",
      "installer",
      "neighbour",
      "neighbor",
      "government",
      "public",
      "company",
      "authority"
    ),
    collapse = "|"
  ),
  exclude = generic_exclude
)

energy_crisis_cols <- unique(c(
  names(df)[str_starts(names(df), "rv_energy_crisis__")],
  find_component_cols(
    df,
    "energy_crisis|crisis|inflation|electricity|gas|heating|fuel",
    exclude = generic_exclude
  )
))

poverty_cols <- unique(c(
  names(df)[str_starts(names(df), "rv_poverty__")],
  find_component_cols(
    df,
    "poverty|vulnerab|depriv|difficulty|arrears|afford|unable|inability",
    exclude = generic_exclude
  )
))

# ============================================================
# Attention checks reales
# ============================================================

find_first_attention_col <- function(data, patterns, check_id) {
  
  cols <- names(data)
  
  hits <- unique(unlist(
    map(
      patterns,
      ~ cols[str_detect(cols, regex(.x, ignore_case = TRUE))]
    )
  ))
  
  hits <- hits[hits %in% cols]
  
  if (length(hits) == 0) {
    warning("No se encontró columna para attention check: ", check_id)
    return(NA_character_)
  }
  
  if (length(hits) > 1) {
    warning(
      "Más de una columna encontrada para attention check: ",
      check_id,
      "\nUsando la primera:\n",
      hits[1],
      "\nCandidatas:\n",
      paste(hits, collapse = "\n")
    )
  }
  
  hits[1]
}

attention_check_specs <- tribble(
  ~check_id, ~expected_type, ~patterns,
  
  # RV Decision: select 42
  "rv_decision_select_42",
  "select_42",
  list(c(
    "^rv_decision__.*select.*42",
    "^rv_decision__.*choose.*42",
    "^rv_decision__.*answer.*42",
    "^rv_decision__.*\\b42\\b"
  )),
  
  # RV Decision: línea más larga
  "rv_decision_longest_line",
  "longest_line",
  list(c(
    "^rv_decision__.*longest.*line",
    "^rv_decision__.*line.*longest",
    "^rv_decision__.*largest.*line",
    "^rv_decision__.*line.*largest",
    "^rv_decision__.*linea.*larga",
    "^rv_decision__.*línea.*larga"
  )),
  
  # RV Concerns2: option 4
  "rv_concerns2_option_4",
  "option_4",
  list(c(
    "^rv_concerns2__.*select.*option.*4",
    "^rv_concerns2__.*option_4",
    "^rv_concerns2__.*select_option_4"
  )),
  
  # RV Concerns2: strongly disagree
  "rv_concerns2_strongly_disagree",
  "strongly_disagree",
  list(c(
    "^rv_concerns2__.*please_select_strongly_disagree",
    "^rv_concerns2__.*strongly_disagree",
    "^rv_concerns2__.*strongly.*disagree"
  )),
  
  # RV Energy crisis: option 4
  "rv_energy_crisis_option_4",
  "option_4",
  list(c(
    "^rv_energy_crisis__.*select.*option.*4",
    "^rv_energy_crisis__.*option_4",
    "^rv_energy_crisis__.*select_option_4"
  )),
  
  # RV Energy crisis: strongly disagree
  "rv_energy_crisis_strongly_disagree",
  "strongly_disagree",
  list(c(
    "^rv_energy_crisis__.*please_select_strongly_disagree",
    "^rv_energy_crisis__.*strongly_disagree",
    "^rv_energy_crisis__.*strongly.*disagree"
  )),
  
  # RV Poverty: disagree
  "rv_poverty_disagree",
  "disagree",
  list(c(
    "^rv_poverty__.*please_select_disagree",
    "^rv_poverty__.*select.*disagree"
  )),
  
  # WHY: zero
  "why_select_zero",
  "zero",
  list(c(
    "^why__.*please_select_zero",
    "^why__.*select.*zero",
    "^why__.*select.*0"
  ))
) %>%
  mutate(
    column = pmap_chr(
      list(patterns, check_id),
      ~ find_first_attention_col(
        data = df,
        patterns = ..1,
        check_id = ..2
      )
    )
  ) %>%
  filter(!is.na(column))

attention_quality_cols <- attention_check_specs$column

component_columns <- list(
  determinants_32 = det_cols,
  sociodemographics_model = sociodemographic_model_cols,
  vote_politics = vote_politics_cols,
  technology_adoption = technology_adoption_cols,
  tariffs_costs_payback = tariffs_costs_payback_cols,
  concerns_barriers = concerns_barriers_cols,
  trust_information = trust_information_cols,
  energy_crisis = energy_crisis_cols,
  poverty = poverty_cols,
  attention_quality = attention_quality_cols
)

diagnostics_component_candidate_columns <- enframe(
  component_columns,
  name = "component",
  value = "column"
) %>%
  unnest_longer(column, values_to = "column") %>%
  mutate(column = as.character(column)) %>%
  filter(!is.na(column), column != "") %>%
  arrange(component, column)


# Calidad de los 32 determinantes
if (length(det_cols) != N_DET_TOTAL) {
  warning(
    "Se esperaban 32 determinantes armonizados, pero se han encontrado ",
    length(det_cols),
    ". Revisa nombres de columnas ^det_\\d{2}_"
  )
}

det_numeric <- df %>%
  select(all_of(det_cols)) %>%
  mutate(across(everything(), parse_num_clean))

det_mat <- as.data.frame(det_numeric)

n_det_numeric <- rowSums(!is.na(det_mat))
n_det_valid <- rowSums(!is.na(det_mat) & det_mat >= 0 & det_mat <= 100)
n_det_missing <- N_DET_TOTAL - n_det_valid
n_det_below_0 <- rowSums(!is.na(det_mat) & det_mat < 0)
n_det_above_100 <- rowSums(!is.na(det_mat) & det_mat > 100)
n_det_out_of_range <- n_det_below_0 + n_det_above_100

det_row_mean <- apply(det_mat, 1, function(x) {
  x <- x[!is.na(x) & x >= 0 & x <= 100]
  if (length(x) == 0) return(NA_real_)
  mean(x)
})

det_row_sd <- apply(det_mat, 1, function(x) {
  x <- x[!is.na(x) & x >= 0 & x <= 100]
  if (length(x) < 2) return(NA_real_)
  sd(x)
})

det_row_min <- apply(det_mat, 1, function(x) {
  x <- x[!is.na(x) & x >= 0 & x <= 100]
  if (length(x) == 0) return(NA_real_)
  min(x)
})

det_row_max <- apply(det_mat, 1, function(x) {
  x <- x[!is.na(x) & x >= 0 & x <= 100]
  if (length(x) == 0) return(NA_real_)
  max(x)
})

det_n_unique_values <- apply(det_mat, 1, function(x) {
  x <- x[!is.na(x) & x >= 0 & x <= 100]
  length(unique(round(x, 6)))
})

det_prop_extreme_values <- apply(det_mat, 1, function(x) {
  x <- x[!is.na(x) & x >= 0 & x <= 100]
  if (length(x) == 0) return(NA_real_)
  mean(x <= 0 | x >= 100)
})

flag_no_determinants <- n_det_valid == 0
flag_too_many_missing <- n_det_valid > 0 & n_det_valid < MIN_DET_VALID_FOR_CLUSTERING
flag_incomplete_but_usable <- n_det_valid >= MIN_DET_VALID_FOR_CLUSTERING & n_det_valid < N_DET_TOTAL
flag_complete_32det <- n_det_valid == N_DET_TOTAL
flag_out_of_range <- n_det_out_of_range > 0
flag_low_variability <- n_det_valid >= MIN_DET_VALID_FOR_CLUSTERING &
  det_n_unique_values <= LOW_VARIABILITY_MAX_UNIQUE
flag_high_extreme_share <- n_det_valid >= MIN_DET_VALID_FOR_CLUSTERING &
  !is.na(det_prop_extreme_values) &
  det_prop_extreme_values >= HIGH_EXTREME_SHARE_THRESHOLD

det_quality <- case_when(
  flag_out_of_range ~ "invalid_values",
  flag_no_determinants ~ "no_determinants",
  flag_too_many_missing ~ "too_many_missing",
  flag_low_variability ~ "low_variability",
  flag_high_extreme_share ~ "high_extreme_share",
  flag_complete_32det ~ "usable_complete",
  flag_incomplete_but_usable ~ "usable_partial",
  TRUE ~ "review"
)

usable_for_clustering <- det_quality %in% c(
  "usable_complete",
  "usable_partial"
)

det_quality_df <- tibble(
  n_det_valid = n_det_valid,
  n_det_missing = n_det_missing,
  prop_det_valid = n_det_valid / N_DET_TOTAL,
  n_det_below_0 = n_det_below_0,
  n_det_above_100 = n_det_above_100,
  n_det_out_of_range = n_det_out_of_range,
  det_row_mean = det_row_mean,
  det_row_sd = det_row_sd,
  det_row_min = det_row_min,
  det_row_max = det_row_max,
  det_n_unique_values = det_n_unique_values,
  det_prop_extreme_values = det_prop_extreme_values,
  flag_no_determinants = flag_no_determinants,
  flag_too_many_missing = flag_too_many_missing,
  flag_incomplete_but_usable = flag_incomplete_but_usable,
  flag_complete_32det = flag_complete_32det,
  flag_out_of_range = flag_out_of_range,
  flag_low_variability = flag_low_variability,
  flag_high_extreme_share = flag_high_extreme_share,
  quality_determinants_32 = det_quality,
  usable_for_clustering = usable_for_clustering
)


# Calidad sociodemográfica y metadatos
has_age_info <- is_valid_model_value(safe_chr_col(df, "age_group_model")) |
  !is.na(safe_num_col(df, "age_model"))

has_gender_info <- is_valid_model_value(safe_chr_col(df, "gender_model"))

has_country_info <- is_valid_model_value(safe_chr_col(df, "country_model")) |
  is_valid_model_value(safe_chr_col(df, "country_model_grouped"))

has_identifier <- is_non_missing_raw(safe_chr_col(df, "global_participant_key")) |
  is_non_missing_raw(safe_chr_col(df, "participant_key")) |
  is_non_missing_raw(safe_chr_col(df, "prolific_id")) |
  is_non_missing_raw(safe_chr_col(df, "identification_code"))

core_sociodemographic_count <- rowSums(
  cbind(
    has_age_info,
    has_gender_info,
    has_country_info
  ),
  na.rm = TRUE
)

extended_sociodemographic_count <- count_valid_model_cols(
  df,
  c(
    "education_model",
    "employment_model",
    "student_status_model",
    "language_model",
    "income_model",
    "num_children_model",
    "country_birth_model",
    "nationality_model",
    "ethnicity_model",
    "city_size_model",
    "tenure_model"
  )
)

quality_sociodemographics <- case_when(
  core_sociodemographic_count == 3 ~ "core_complete",
  core_sociodemographic_count == 2 ~ "core_partial",
  core_sociodemographic_count == 1 ~ "core_poor",
  TRUE ~ "core_missing"
)

metadata_quality <- case_when(
  has_identifier & core_sociodemographic_count == 3 ~ "metadata_complete",
  has_identifier & core_sociodemographic_count >= 1 ~ "metadata_partial",
  has_identifier & core_sociodemographic_count == 0 ~ "metadata_poor",
  TRUE ~ "metadata_no_identifier"
)

sociodemographic_quality_df <- tibble(
  has_identifier = has_identifier,
  has_age_info = has_age_info,
  has_gender_info = has_gender_info,
  has_country_info = has_country_info,
  core_sociodemographic_count = core_sociodemographic_count,
  extended_sociodemographic_count = extended_sociodemographic_count,
  quality_sociodemographics = quality_sociodemographics,
  metadata_quality = metadata_quality
)


# Preparación específica para propensity score

# Modelo previsto:
# Probabilidad de abstención ~ Edad + Educación + Sueldo/Income +
# País + Tamaño de ciudad + Situación laboral
#
# Nota:
# income_model probablemente solo existe bien en Diego.
# Por eso se crean tres niveles:
# - strict: todos los predictores, incluido income
# - without_income: todos menos income
# - minimal: edad + país + empleo

has_propensity_age <- is_valid_model_value(safe_chr_col(df, "age_group_model")) |
  !is.na(safe_num_col(df, "age_model"))

has_propensity_education <- is_valid_model_value(
  safe_chr_col(df, "education_model")
)

has_propensity_income <- is_valid_model_value(
  safe_chr_col(df, "income_model")
)

has_propensity_country <- is_valid_model_value(
  safe_chr_col(df, "country_model_grouped")
) |
  is_valid_model_value(
    safe_chr_col(df, "country_model")
  )

has_propensity_city_size <- is_valid_model_value(
  safe_chr_col(df, "city_size_model")
)

has_propensity_employment <- is_valid_model_value(
  safe_chr_col(df, "employment_model")
)

has_propensity_outcome_binary <- safe_chr_col(df, "vote_status_declared") %in% c(
  "voter",
  "abstainer"
) |
  !is.na(safe_num_col(df, "voted_observed"))

n_propensity_predictors_available <- rowSums(
  cbind(
    has_propensity_age,
    has_propensity_education,
    has_propensity_income,
    has_propensity_country,
    has_propensity_city_size,
    has_propensity_employment
  ),
  na.rm = TRUE
)

prop_propensity_predictors_available <- n_propensity_predictors_available / 6

usable_for_propensity_score_strict <- has_propensity_age &
  has_propensity_education &
  has_propensity_income &
  has_propensity_country &
  has_propensity_city_size &
  has_propensity_employment

usable_for_propensity_score_without_income <- has_propensity_age &
  has_propensity_education &
  has_propensity_country &
  has_propensity_city_size &
  has_propensity_employment

usable_for_propensity_score_minimal <- has_propensity_age &
  has_propensity_country &
  has_propensity_employment

usable_for_propensity_model_strict <- usable_for_propensity_score_strict &
  has_propensity_outcome_binary

usable_for_propensity_model_without_income <- usable_for_propensity_score_without_income &
  has_propensity_outcome_binary

usable_for_propensity_model_minimal <- usable_for_propensity_score_minimal &
  has_propensity_outcome_binary

quality_propensity_score_predictors <- case_when(
  usable_for_propensity_score_strict ~ "complete_strict_formula",
  usable_for_propensity_score_without_income ~ "complete_without_income",
  usable_for_propensity_score_minimal ~ "minimal_predictors_available",
  n_propensity_predictors_available > 0 ~ "partial_predictors",
  TRUE ~ "missing_predictors"
)

quality_propensity_score_model <- case_when(
  !has_propensity_outcome_binary ~ "missing_binary_vote_outcome",
  usable_for_propensity_model_strict ~ "model_ready_strict",
  usable_for_propensity_model_without_income ~ "model_ready_without_income",
  usable_for_propensity_model_minimal ~ "model_ready_minimal",
  n_propensity_predictors_available > 0 ~ "outcome_available_but_predictors_partial",
  TRUE ~ "not_ready"
)

propensity_quality_df <- tibble(
  has_propensity_age = has_propensity_age,
  has_propensity_education = has_propensity_education,
  has_propensity_income = has_propensity_income,
  has_propensity_country = has_propensity_country,
  has_propensity_city_size = has_propensity_city_size,
  has_propensity_employment = has_propensity_employment,
  has_propensity_outcome_binary = has_propensity_outcome_binary,
  
  n_propensity_predictors_available = n_propensity_predictors_available,
  prop_propensity_predictors_available = prop_propensity_predictors_available,
  
  usable_for_propensity_score_strict = usable_for_propensity_score_strict,
  usable_for_propensity_score_without_income = usable_for_propensity_score_without_income,
  usable_for_propensity_score_minimal = usable_for_propensity_score_minimal,
  
  usable_for_propensity_model_strict = usable_for_propensity_model_strict,
  usable_for_propensity_model_without_income = usable_for_propensity_model_without_income,
  usable_for_propensity_model_minimal = usable_for_propensity_model_minimal,
  
  quality_propensity_score_predictors = quality_propensity_score_predictors,
  quality_propensity_score_model = quality_propensity_score_model
)

# Calidad voto / política
has_vote_status <- is_valid_model_value(safe_chr_col(df, "vote_status_declared"))
has_voted_observed <- !is.na(safe_num_col(df, "voted_observed"))

political_left_right_num <- safe_num_col(df, "political_left_right_model")

has_political_left_right <- !is.na(political_left_right_num) &
  political_left_right_num >= 0 &
  political_left_right_num <= 100

has_political_block <- is_valid_model_value(safe_chr_col(df, "political_block_model"))

quality_vote_politics <- case_when(
  has_vote_status & has_political_left_right ~ "vote_and_politics_complete",
  has_vote_status & !has_political_left_right ~ "vote_only",
  !has_vote_status & has_political_left_right ~ "politics_only",
  has_political_block ~ "politics_block_only",
  TRUE ~ "missing_or_not_collected"
)

vote_politics_quality_df <- tibble(
  has_vote_status = has_vote_status,
  has_voted_observed = has_voted_observed,
  has_political_left_right = has_political_left_right,
  has_political_block = has_political_block,
  quality_vote_politics = quality_vote_politics
)


# Calidad genérica por componentes originales
technology_quality_df <- component_stats(
  df,
  "technology_adoption",
  technology_adoption_cols
)

tariffs_quality_df <- component_stats(
  df,
  "tariffs_costs_payback",
  tariffs_costs_payback_cols
)

concerns_quality_df <- component_stats(
  df,
  "concerns_barriers",
  concerns_barriers_cols
)

trust_quality_df <- component_stats(
  df,
  "trust_information",
  trust_information_cols
)

energy_crisis_quality_df <- component_stats(
  df,
  "energy_crisis",
  energy_crisis_cols
)

poverty_quality_df <- component_stats(
  df,
  "poverty",
  poverty_cols
)

attention_quality_df <- component_stats(
  df,
  "attention_quality",
  attention_quality_cols
)

# ============================================================
# Attention checks: acierto / fallo explícito
# ============================================================

normalise_attention_response <- function(x) {
  x <- clean_text(x)
  x <- str_to_lower(x)
  x <- str_replace_all(x, "_", " ")
  x <- str_replace_all(x, "\\s+", " ")
  str_squish(x)
}

attention_passes_expected <- function(x, expected_type) {
  
  x_clean <- clean_text(x)
  x_low <- normalise_attention_response(x)
  x_num <- parse_num_clean(x)
  
  if (is.na(x_clean)) {
    return(NA)
  }
  
  case_when(
    expected_type == "select_42" ~
      (!is.na(x_num) & x_num == 42) |
      str_detect(x_low, "^42$") |
      str_detect(x_low, "\\b42\\b"),
    
    expected_type == "longest_line" ~
      str_detect(x_low, "longest") |
      str_detect(x_low, "largest") |
      str_detect(x_low, "linea mas larga") |
      str_detect(x_low, "línea más larga"),
    
    expected_type == "option_4" ~
      (!is.na(x_num) & x_num == 4) |
      str_detect(x_low, "^4$") |
      str_detect(x_low, "\\boption\\s*4\\b") |
      str_detect(x_low, "\\bopcion\\s*4\\b") |
      str_detect(x_low, "\\bopción\\s*4\\b"),
    
    expected_type == "strongly_disagree" ~
      str_detect(x_low, "strongly disagree") |
      str_detect(x_low, "totalmente en desacuerdo") |
      str_detect(x_low, "muy en desacuerdo"),
    
    expected_type == "disagree" ~
      (
        str_detect(x_low, "^disagree$") |
          str_detect(x_low, "\\bdisagree\\b") |
          str_detect(x_low, "\\ben desacuerdo\\b")
      ) &
      !str_detect(x_low, "strongly") &
      !str_detect(x_low, "totalmente") &
      !str_detect(x_low, "muy"),
    
    expected_type == "zero" ~
      (!is.na(x_num) & x_num == 0) |
      str_detect(x_low, "^0$") |
      str_detect(x_low, "^zero$") |
      str_detect(x_low, "^cero$"),
    
    TRUE ~ NA
  )
}

attention_check_long <- pmap_dfr(
  attention_check_specs,
  function(check_id, expected_type, patterns, column) {
    
    response_raw <- df[[column]]
    
    passed <- map_lgl(
      response_raw,
      ~ {
        res <- attention_passes_expected(.x, expected_type)
        ifelse(is.na(res), FALSE, res)
      }
    )
    
    available <- !is.na(clean_text(response_raw))
    failed <- available & !passed
    
    tibble(
      row_index_attention = seq_len(nrow(df)),
      check_id = check_id,
      attention_check_column = column,
      expected_type = expected_type,
      response_raw = as.character(response_raw),
      attention_check_available = available,
      attention_check_passed = if_else(available, passed, NA),
      attention_check_failed = if_else(available, failed, NA)
    )
  }
)

attention_check_df <- attention_check_long %>%
  group_by(row_index_attention) %>%
  summarise(
    n_attention_checks_available_explicit = sum(attention_check_available, na.rm = TRUE),
    n_attention_checks_passed_explicit = sum(attention_check_passed == TRUE, na.rm = TRUE),
    n_attention_checks_failed_explicit = sum(attention_check_failed == TRUE, na.rm = TRUE),
    
    attention_check_failed_any =
      n_attention_checks_failed_explicit > 0,
    
    attention_check_failed_all_available =
      n_attention_checks_available_explicit > 0 &
      n_attention_checks_failed_explicit == n_attention_checks_available_explicit,
    
    attention_check_failed_all_4 =
      n_attention_checks_available_explicit == 4 &
      n_attention_checks_failed_explicit == 4,
    
    attention_check_failed_4_or_more =
      n_attention_checks_failed_explicit >= 4,
    
    failed_attention_check_ids = paste(
      check_id[attention_check_failed == TRUE],
      collapse = "; "
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    failed_attention_check_ids = na_if(failed_attention_check_ids, ""),
    
    attention_check_status_explicit = case_when(
      n_attention_checks_available_explicit == 0 ~ "not_available",
      n_attention_checks_failed_explicit == 0 ~ "passed_all_available",
      attention_check_failed_all_available ~ "failed_all_available",
      n_attention_checks_failed_explicit > 0 ~ "failed_some",
      TRUE ~ "review"
    )
  ) %>%
  right_join(
    tibble(row_index_attention = seq_len(nrow(df))),
    by = "row_index_attention"
  ) %>%
  arrange(row_index_attention) %>%
  mutate(
    n_attention_checks_available_explicit = replace_na(n_attention_checks_available_explicit, 0L),
    n_attention_checks_passed_explicit = replace_na(n_attention_checks_passed_explicit, 0L),
    n_attention_checks_failed_explicit = replace_na(n_attention_checks_failed_explicit, 0L),
    attention_check_failed_any = replace_na(attention_check_failed_any, FALSE),
    attention_check_failed_all_available = replace_na(attention_check_failed_all_available, FALSE),
    attention_check_failed_all_4 = replace_na(attention_check_failed_all_4, FALSE),
    attention_check_failed_4_or_more = replace_na(attention_check_failed_4_or_more, FALSE),
    attention_check_status_explicit = replace_na(attention_check_status_explicit, "not_available")
  ) %>%
  select(-row_index_attention)


# Dataset final con calidad
df_quality <- bind_cols(
  df,
  det_quality_df,
  sociodemographic_quality_df,
  propensity_quality_df,
  vote_politics_quality_df,
  technology_quality_df,
  tariffs_quality_df,
  concerns_quality_df,
  trust_quality_df,
  energy_crisis_quality_df,
  poverty_quality_df,
  attention_quality_df,
  attention_check_df
) %>%
  mutate(
    row_quality_final = case_when(
      quality_determinants_32 == "invalid_values" ~ "invalid_determinants",
      quality_determinants_32 == "no_determinants" ~ "no_determinants",
      quality_determinants_32 == "too_many_missing" ~ "too_many_missing_determinants",
      quality_determinants_32 %in% c("low_variability", "high_extreme_share") ~ "suspicious_determinants",
      quality_determinants_32 == "usable_complete" &
        quality_sociodemographics == "core_complete" ~ "usable_complete",
      quality_determinants_32 == "usable_complete" &
        quality_sociodemographics != "core_complete" ~ "usable_complete_limited_metadata",
      quality_determinants_32 == "usable_partial" &
        quality_sociodemographics %in% c("core_complete", "core_partial") ~ "usable_partial",
      quality_determinants_32 == "usable_partial" ~ "usable_partial_limited_metadata",
      TRUE ~ "review"
    ),
    
    usable_for_main_analysis = row_quality_final %in% c(
      "usable_complete",
      "usable_complete_limited_metadata",
      "usable_partial",
      "usable_partial_limited_metadata"
    )
  )

# Matrices para clustering
id_cols <- c(
  "integrated_row_id",
  "global_participant_key",
  "dataset_source",
  "source_survey",
  "source_file",
  "participant_key",
  "prolific_id",
  "identification_code"
)

metadata_cols <- c(
  "country_model",
  "country_model_grouped",
  "country_birth_model",
  "nationality_model",
  "ethnicity_model",
  "language_model",
  "age_model",
  "age_group_model",
  "gender_model",
  "education_model",
  "employment_model",
  "student_status_model",
  "income_model",
  "num_children_model"
)

quality_cols <- names(df_quality)[
  str_detect(
    names(df_quality),
    "^n_det_|^prop_det_|^det_row_|^det_n_|^det_prop_|^flag_|^quality_|^usable_|^has_|metadata_quality|row_quality_final|core_sociodemographic_count|extended_sociodemographic_count|propensity"
  )
]

matrix_32det_with_quality <- df_quality %>%
  select(
    any_of(id_cols),
    any_of(metadata_cols),
    all_of(det_cols),
    any_of(quality_cols)
  )

matrix_32det_for_clustering <- matrix_32det_with_quality %>%
  filter(usable_for_clustering == TRUE) %>%
  filter(n_det_valid >= MIN_DET_VALID_FOR_CLUSTERING)

# Diagnósticos
diagnostics_missing_by_determinant <- df_quality %>%
  select(dataset_source, all_of(det_cols)) %>%
  pivot_longer(
    cols = all_of(det_cols),
    names_to = "det_col",
    values_to = "value_raw"
  ) %>%
  mutate(
    value_num = parse_num_clean(value_raw),
    is_missing = is.na(value_num),
    is_out_of_range = !is.na(value_num) & (value_num < 0 | value_num > 100)
  ) %>%
  group_by(dataset_source, det_col) %>%
  summarise(
    n_rows = n(),
    n_missing = sum(is_missing),
    prop_missing = n_missing / n_rows,
    n_out_of_range = sum(is_out_of_range),
    .groups = "drop"
  ) %>%
  arrange(dataset_source, det_col)

diagnostics_row_quality_counts <- df_quality %>%
  count(dataset_source, row_quality_final, name = "n") %>%
  group_by(dataset_source) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  arrange(dataset_source, desc(n))

diagnostics_metadata_quality_counts <- df_quality %>%
  count(dataset_source, metadata_quality, name = "n") %>%
  group_by(dataset_source) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  arrange(dataset_source, desc(n))


diagnostics_propensity_score_readiness <- df_quality %>%
  group_by(dataset_source) %>%
  summarise(
    n_rows = n(),
    
    n_has_binary_vote_outcome = sum(has_propensity_outcome_binary, na.rm = TRUE),
    prop_has_binary_vote_outcome = n_has_binary_vote_outcome / n_rows,
    
    n_has_age = sum(has_propensity_age, na.rm = TRUE),
    prop_has_age = n_has_age / n_rows,
    
    n_has_education = sum(has_propensity_education, na.rm = TRUE),
    prop_has_education = n_has_education / n_rows,
    
    n_has_income = sum(has_propensity_income, na.rm = TRUE),
    prop_has_income = n_has_income / n_rows,
    
    n_has_country = sum(has_propensity_country, na.rm = TRUE),
    prop_has_country = n_has_country / n_rows,
    
    n_has_city_size = sum(has_propensity_city_size, na.rm = TRUE),
    prop_has_city_size = n_has_city_size / n_rows,
    
    n_has_employment = sum(has_propensity_employment, na.rm = TRUE),
    prop_has_employment = n_has_employment / n_rows,
    
    mean_n_propensity_predictors_available = mean(
      n_propensity_predictors_available,
      na.rm = TRUE
    ),
    
    n_predictors_strict_complete = sum(
      usable_for_propensity_score_strict,
      na.rm = TRUE
    ),
    prop_predictors_strict_complete = n_predictors_strict_complete / n_rows,
    
    n_predictors_complete_without_income = sum(
      usable_for_propensity_score_without_income,
      na.rm = TRUE
    ),
    prop_predictors_complete_without_income = n_predictors_complete_without_income / n_rows,
    
    n_predictors_minimal = sum(
      usable_for_propensity_score_minimal,
      na.rm = TRUE
    ),
    prop_predictors_minimal = n_predictors_minimal / n_rows,
    
    n_model_ready_strict = sum(
      usable_for_propensity_model_strict,
      na.rm = TRUE
    ),
    prop_model_ready_strict = n_model_ready_strict / n_rows,
    
    n_model_ready_without_income = sum(
      usable_for_propensity_model_without_income,
      na.rm = TRUE
    ),
    prop_model_ready_without_income = n_model_ready_without_income / n_rows,
    
    n_model_ready_minimal = sum(
      usable_for_propensity_model_minimal,
      na.rm = TRUE
    ),
    prop_model_ready_minimal = n_model_ready_minimal / n_rows,
    
    .groups = "drop"
  )
diagnostics_row_quality_by_source <- df_quality %>%
  group_by(dataset_source) %>%
  summarise(
    n_rows = n(),
    n_usable_for_clustering = sum(usable_for_clustering, na.rm = TRUE),
    prop_usable_for_clustering = n_usable_for_clustering / n_rows,
    n_usable_for_main_analysis = sum(usable_for_main_analysis, na.rm = TRUE),
    prop_usable_for_main_analysis = n_usable_for_main_analysis / n_rows,
    
    n_usable_complete = sum(row_quality_final == "usable_complete", na.rm = TRUE),
    n_usable_complete_limited_metadata = sum(row_quality_final == "usable_complete_limited_metadata", na.rm = TRUE),
    n_usable_partial = sum(row_quality_final == "usable_partial", na.rm = TRUE),
    n_usable_partial_limited_metadata = sum(row_quality_final == "usable_partial_limited_metadata", na.rm = TRUE),
    
    n_no_determinants = sum(row_quality_final == "no_determinants", na.rm = TRUE),
    n_too_many_missing_determinants = sum(row_quality_final == "too_many_missing_determinants", na.rm = TRUE),
    n_suspicious_determinants = sum(row_quality_final == "suspicious_determinants", na.rm = TRUE),
    n_invalid_determinants = sum(row_quality_final == "invalid_determinants", na.rm = TRUE),
    
    mean_n_det_valid = mean(n_det_valid, na.rm = TRUE),
    min_n_det_valid = min(n_det_valid, na.rm = TRUE),
    max_n_det_valid = max(n_det_valid, na.rm = TRUE),
    mean_det_row_sd = mean(det_row_sd, na.rm = TRUE),
    
    n_metadata_complete = sum(metadata_quality == "metadata_complete", na.rm = TRUE),
    n_metadata_partial = sum(metadata_quality == "metadata_partial", na.rm = TRUE),
    n_metadata_poor = sum(metadata_quality %in% c("metadata_poor", "metadata_no_identifier"), na.rm = TRUE),
    
    .groups = "drop"
  )

component_quality_cols <- names(df_quality)[
  str_detect(names(df_quality), "^quality_")
]

diagnostics_component_quality_counts <- df_quality %>%
  select(dataset_source, all_of(component_quality_cols)) %>%
  pivot_longer(
    cols = all_of(component_quality_cols),
    names_to = "component",
    values_to = "quality"
  ) %>%
  count(dataset_source, component, quality, name = "n") %>%
  group_by(dataset_source, component) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  arrange(dataset_source, component, desc(n))

component_n_cols <- names(df_quality)[
  str_detect(names(df_quality), "^n_.*_non_missing$")
]

diagnostics_component_coverage_by_source <- df_quality %>%
  select(dataset_source, all_of(component_n_cols)) %>%
  mutate(
    across(
      all_of(component_n_cols),
      ~ suppressWarnings(as.numeric(.x))
    )
  ) %>%
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


# Guardado
write_csv(
  df_quality,
  file.path(out_dir, "all_sources_integrated_component_quality.csv")
)

write_csv(
  matrix_32det_with_quality,
  file.path(out_dir, "matrix_32det_with_quality.csv")
)

write_csv(
  matrix_32det_for_clustering,
  file.path(out_dir, "matrix_32det_for_clustering.csv")
)

write_csv(
  diagnostics_component_candidate_columns,
  file.path(out_dir, "diagnostics_component_candidate_columns.csv")
)

write_csv(
  diagnostics_component_quality_counts,
  file.path(out_dir, "diagnostics_component_quality_counts.csv")
)

write_csv(
  diagnostics_component_coverage_by_source,
  file.path(out_dir, "diagnostics_component_coverage_by_source.csv")
)

write_csv(
  diagnostics_row_quality_by_source,
  file.path(out_dir, "diagnostics_row_quality_by_source.csv")
)

write_csv(
  diagnostics_row_quality_counts,
  file.path(out_dir, "diagnostics_row_quality_counts.csv")
)

write_csv(
  diagnostics_missing_by_determinant,
  file.path(out_dir, "diagnostics_missing_by_determinant.csv")
)

write_csv(
  diagnostics_metadata_quality_counts,
  file.path(out_dir, "diagnostics_metadata_quality_counts.csv")
)

write_csv(
  diagnostics_propensity_score_readiness,
  file.path(out_dir, "diagnostics_propensity_score_readiness.csv")
)


# Resumen en consola
print(diagnostics_row_quality_by_source)
print(diagnostics_row_quality_counts)
print(diagnostics_metadata_quality_counts)
print(diagnostics_propensity_score_readiness)

message("Listo.")
message("Dataset con calidad por componentes: ", file.path(out_dir, "all_sources_integrated_component_quality.csv"))
message("Matriz 32 determinantes con calidad: ", file.path(out_dir, "matrix_32det_with_quality.csv"))
message("Matriz lista para clustering: ", file.path(out_dir, "matrix_32det_for_clustering.csv"))
message("Diagnóstico columnas por componente: ", file.path(out_dir, "diagnostics_component_candidate_columns.csv"))
message("Diagnóstico calidad por componente: ", file.path(out_dir, "diagnostics_component_quality_counts.csv"))
message("Diagnóstico cobertura por componente y fuente: ", file.path(out_dir, "diagnostics_component_coverage_by_source.csv"))
message("Diagnóstico calidad final por fuente: ", file.path(out_dir, "diagnostics_row_quality_by_source.csv"))
message("Diagnóstico calidad final por categorías: ", file.path(out_dir, "diagnostics_row_quality_counts.csv"))
message("Diagnóstico missing por determinante: ", file.path(out_dir, "diagnostics_missing_by_determinant.csv"))
message("Diagnóstico metadata: ", file.path(out_dir, "diagnostics_metadata_quality_counts.csv"))
message("Diagnóstico preparación propensity score: ", file.path(out_dir, "diagnostics_propensity_score_readiness.csv"))