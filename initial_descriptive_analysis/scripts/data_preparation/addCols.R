
# Este script parte del dataset consolidado df_complete y añade columnas de control
# de calidad por respuesta.
#
# La calidad se calcula a partir de:
#   - Completitud de la fila original
#   - Tipo de identificador
#   - Identificadores sospechosos
#   - Duplicados por join_key
#   - Problemas de merge
#   - Datos sociodemográficos básicos
#   - Attention checks
#
# Además, crea variables sociodemográficas limpias para evitar categorías duplicadas
# como "Male." / "Male" o distintas versiones de nivel educativo.
#
# Resultado principal:
#   df_all_surveys_complete_with_quality.csv


library(dplyr)
library(stringr)
library(tidyr)
library(readr)


# ==============================================================================
# 0. DIRECTORIOS DE SALIDA
# ==============================================================================

base_output_dir <- "initial_descriptive_analysis/output/data_preparation"

csv_dir  <- file.path(base_output_dir, "csv")
logs_dir <- file.path(base_output_dir, "logs")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)


# Protección por si esta columna no existe en df_complete
if (!"problematic_merge" %in% names(df_complete)) {
  df_complete$problematic_merge <- FALSE
}

# Guardamos las columnas originales para calcular completitud real
original_cols <- names(df_complete)


# ==============================================================================
# 1. FUNCIONES AUXILIARES
# ==============================================================================

clean_text_basic <- function(x) {
  x <- str_squish(as.character(x))
  x <- na_if(x, "")
  x <- na_if(x, "NA")
  x <- na_if(x, "NaN")
  x
}

get_optional_col <- function(data, col_name) {
  if (col_name %in% names(data)) {
    clean_text_basic(data[[col_name]])
  } else {
    rep(NA_character_, nrow(data))
  }
}

coalesce_optional_cols <- function(data, candidates) {
  existing <- candidates[candidates %in% names(data)]
  
  if (length(existing) == 0) {
    return(rep(NA_character_, nrow(data)))
  }
  
  values <- lapply(existing, function(col) clean_text_basic(data[[col]]))
  do.call(coalesce, values)
}

classify_attention_check <- function(answer, check_exists, expected_regex) {
  answer <- clean_text_basic(answer)
  
  if (!check_exists) {
    return(rep("no_aplica_columna_no_existe", length(answer)))
  }
  
  status <- rep(NA_character_, length(answer))
  status[is.na(answer)] <- "no_responde"
  
  idx <- !is.na(answer)
  status[idx & str_detect(answer, regex(expected_regex, ignore_case = TRUE))] <- "pasa"
  status[idx & is.na(status)] <- "falla"
  
  status
}

count_non_missing_safe <- function(data) {
  tmp <- data %>%
    mutate(across(everything(), ~ !is.na(.x) & as.character(.x) != ""))
  
  rowSums(as.data.frame(tmp))
}


# ==============================================================================
# 2. FUNCIONES DE LIMPIEZA SOCIODEMOGRÁFICA
# ==============================================================================

clean_gender_quality <- function(x) {
  x <- clean_text_basic(x)
  x <- str_remove(x, "\\s*\\(ID[0-9]+\\)$")
  x <- str_remove(x, "\\.$")
  x <- str_squish(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("^male$", ignore_case = TRUE)) ~ "Male",
    str_detect(x, regex("^female$", ignore_case = TRUE)) ~ "Female",
    str_detect(x, regex("other|prefer", ignore_case = TRUE)) ~ "Other / Prefer not to say",
    TRUE ~ "Other / Prefer not to say"
  )
}

clean_education_quality <- function(x) {
  x <- clean_text_basic(x)
  x <- str_remove(x, "\\s*\\(ID[0-9]+\\)$")
  x <- str_remove(x, "\\.$")
  x <- str_squish(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("university|bachelor|master|phd", ignore_case = TRUE)) ~ 
      "University degree or above",
    str_detect(x, regex("primary|secondary|vocational|high school|elementary|certificate", ignore_case = TRUE)) ~ 
      "Below university degree",
    TRUE ~ NA_character_
  )
}

clean_country_quality <- function(x) {
  x <- clean_text_basic(x)
  x <- str_remove(x, "\\s*\\(ID[0-9]+\\)$")
  x <- str_remove(x, "^[A-Z]{2}\\s*[–-]\\s*")
  x <- str_squish(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    x %in% c("United Kingdom *", "UK", "Great Britain") ~ "United Kingdom",
    x %in% c("Czechia") ~ "Czech Republic",
    x %in% c("Moldova (Republic of Moldova)") ~ "Moldova",
    x %in% c("Russian Federation") ~ "Russia",
    TRUE ~ x
  )
}

clean_year_of_birth_quality <- function(x, project_year = 2026) {
  x_num <- suppressWarnings(as.numeric(clean_text_basic(x)))
  
  case_when(
    !is.na(x_num) & x_num >= 1900 & x_num <= 2008 ~ x_num,
    !is.na(x_num) & x_num >= 18 & x_num <= 100 ~ project_year - x_num,
    TRUE ~ NA_real_
  )
}


# ==============================================================================
# 3. AUDITORÍA DE DUPLICADOS POR JOIN_KEY
# ==============================================================================

duplicate_key_audit <- df_complete %>%
  filter(!is.na(join_key)) %>%
  group_by(join_key) %>%
  summarise(
    n_rows_join_key = n(),
    n_surveys_join_key = n_distinct(source_survey),
    surveys_join_key = paste(unique(source_survey), collapse = "; "),
    n_decision_join_key = sum(source_survey == "decision"),
    .groups = "drop"
  ) %>%
  mutate(
    duplicate_type = case_when(
      n_rows_join_key == 1 ~ "not_duplicated",
      n_decision_join_key > 1 ~ "problematic_duplicate_in_decision",
      n_surveys_join_key > 1 ~ "expected_cross_survey_link",
      TRUE ~ "duplicate_within_same_survey"
    )
  )


# ==============================================================================
# 4. CREAR DATASET CON COLUMNAS DE IDENTIFICADOR Y DUPLICADOS
# ==============================================================================

df_complete_quality <- df_complete %>%
  left_join(
    duplicate_key_audit,
    by = "join_key"
  ) %>%
  mutate(
    n_rows_join_key = replace_na(n_rows_join_key, 0L),
    n_surveys_join_key = replace_na(n_surveys_join_key, 0L),
    surveys_join_key = replace_na(surveys_join_key, ""),
    duplicate_type = replace_na(duplicate_type, "not_duplicated"),
    
    identifier_type = case_when(
      str_detect(coalesce(join_key, ""), "^PROLIFIC_") ~ "prolific",
      str_detect(coalesce(join_key, ""), "^CODE_") ~ "code",
      TRUE ~ "no_identifier"
    ),
    
    # Solo se considera sospechoso un identificador Prolific demasiado corto.
    # Los CODE_XXXXX pueden ser códigos reales de 5 caracteres y no se marcan
    # como sospechosos por defecto.
    suspicious_join_key = case_when(
      str_detect(coalesce(join_key, ""), "^PROLIFIC_") &
        str_length(str_remove(join_key, "^PROLIFIC_")) < 20 ~ TRUE,
      TRUE ~ FALSE
    )
  )


# ==============================================================================
# 5. EXTRAER Y LIMPIAR SOCIODEMOGRÁFICO BÁSICO
# ==============================================================================

year_candidates <- c(
  "please_enter_your_year_of_birth_final",
  "please_enter_your_year_of_birth",
  "please_complete_one_row_for_each_member_of_your_household_starting_with_yourself_select_the_option_in_each_column_that_corresponds_to_each_household_member_respondent_year_of_birth"
)

gender_candidates <- c(
  "what_is_your_gender_final",
  "what_is_your_gender",
  "please_complete_one_row_for_each_member_of_your_household_starting_with_yourself_select_the_option_in_each_column_that_corresponds_to_each_household_member_respondent_gender"
)

country_candidates <- c(
  "in_which_country_do_you_currently_live_final",
  "in_which_country_do_you_currently_live"
)

education_candidates <- c(
  "what_is_the_highest_level_of_education_that_you_have_completed_if_you_are_currently_studying_and_have_not_yet_completed_a_level_please_select_the_last_level_you_have_finished_final",
  "what_is_the_highest_level_of_education_that_you_have_completed_if_you_are_currently_studying_and_have_not_yet_completed_a_level_please_select_the_last_level_you_have_finished",
  "please_complete_one_row_for_each_member_of_your_household_starting_with_yourself_select_the_option_in_each_column_that_corresponds_to_each_household_member_respondent_educational_level"
)

df_complete_quality <- df_complete_quality %>%
  mutate(
    year_of_birth_quality_raw = coalesce_optional_cols(., year_candidates),
    gender_quality_raw = coalesce_optional_cols(., gender_candidates),
    country_quality_raw = coalesce_optional_cols(., country_candidates),
    education_quality_raw = coalesce_optional_cols(., education_candidates),
    
    year_of_birth_quality = clean_year_of_birth_quality(year_of_birth_quality_raw),
    age_quality = 2026 - year_of_birth_quality,
    
    age_group_quality = case_when(
      is.na(age_quality) ~ NA_character_,
      age_quality < 25 ~ "18-24",
      age_quality < 35 ~ "25-34",
      age_quality < 50 ~ "35-49",
      age_quality < 65 ~ "50-64",
      age_quality >= 65 ~ "65+"
    ),
    
    gender_quality = clean_gender_quality(gender_quality_raw),
    country_quality = clean_country_quality(country_quality_raw),
    education_quality = clean_education_quality(education_quality_raw),
    
    has_country = !is.na(country_quality),
    
    has_any_sociodemographic =
      !is.na(year_of_birth_quality) |
      !is.na(gender_quality) |
      !is.na(country_quality) |
      !is.na(education_quality)
  )


# ==============================================================================
# 6. ATTENTION CHECKS
# ==============================================================================

line_check_candidates <- names(df_complete_quality)[
  str_detect(names(df_complete_quality), "horizontal_lines")
]

check_42_candidates <- names(df_complete_quality)[
  str_detect(names(df_complete_quality), "select_42")
]

check_4_candidates <- names(df_complete_quality)[
  str_detect(names(df_complete_quality), "select_option_4")
]

check_car_candidates <- names(df_complete_quality)[
  str_detect(names(df_complete_quality), "select_car")
]

check_0_candidates <- names(df_complete_quality)[
  str_detect(names(df_complete_quality), "select_0")
]

strongly_disagree_candidates <- names(df_complete_quality)[
  str_detect(names(df_complete_quality), "strongly_disagree")
]

disagree_candidates <- names(df_complete_quality)[
  str_detect(names(df_complete_quality), "please_select_disagree")
]

df_complete_quality <- df_complete_quality %>%
  mutate(
    line_check_answer = coalesce_optional_cols(., line_check_candidates),
    check_42_answer = coalesce_optional_cols(., check_42_candidates),
    check_4_answer = coalesce_optional_cols(., check_4_candidates),
    check_car_answer = coalesce_optional_cols(., check_car_candidates),
    check_0_answer = coalesce_optional_cols(., check_0_candidates),
    strongly_disagree_answer = coalesce_optional_cols(., strongly_disagree_candidates),
    disagree_answer = coalesce_optional_cols(., disagree_candidates),
    
    line_check_status = classify_attention_check(
      line_check_answer,
      length(line_check_candidates) > 0,
      "^All lines are the same len.*gth\\.?$"
    ),
    
    check_42_status = classify_attention_check(
      check_42_answer,
      length(check_42_candidates) > 0,
      "^42\\.?$"
    ),
    
    check_4_status = classify_attention_check(
      check_4_answer,
      length(check_4_candidates) > 0,
      "^Option 4\\.?$|^4\\.?$"
    ),
    
    check_car_status = classify_attention_check(
      check_car_answer,
      length(check_car_candidates) > 0,
      "^Car\\.?$"
    ),
    
    check_0_status = classify_attention_check(
      check_0_answer,
      length(check_0_candidates) > 0,
      "^0\\.?$"
    ),
    
    strongly_disagree_status = classify_attention_check(
      strongly_disagree_answer,
      length(strongly_disagree_candidates) > 0,
      "Strongly disagree"
    ),
    
    disagree_status = classify_attention_check(
      disagree_answer,
      length(disagree_candidates) > 0,
      "^Disagree\\.?$"
    ),
    
    failed_attention_check_any =
      line_check_status == "falla" |
      check_42_status == "falla" |
      check_4_status == "falla" |
      check_car_status == "falla" |
      check_0_status == "falla" |
      strongly_disagree_status == "falla" |
      disagree_status == "falla"
  )


# ==============================================================================
# 7. COMPLETITUD DE LA FILA ORIGINAL
# ==============================================================================

df_complete_quality$n_non_missing_original <- count_non_missing_safe(
  df_complete_quality %>%
    select(all_of(original_cols))
)

df_complete_quality <- df_complete_quality %>%
  mutate(
    completeness_percentage_original = round(
      n_non_missing_original / length(original_cols) * 100,
      1
    )
  )


# ==============================================================================
# 8. COLUMNA FINAL DE CALIDAD
# ==============================================================================

df_complete_quality <- df_complete_quality %>%
  mutate(
    response_quality = case_when(
      n_non_missing_original <= 5 ~ "casi_vacia_0_5",
      n_non_missing_original <= 10 ~ "muy_incompleta_6_10",
      suspicious_join_key ~ "suspicious_identifier",
      problematic_merge == TRUE ~ "problematic_merge",
      duplicate_type %in% c(
        "problematic_duplicate_in_decision",
        "duplicate_within_same_survey"
      ) ~ "duplicado_problematico",
      failed_attention_check_any ~ "attention_check_fallido",
      !has_any_sociodemographic ~ "sin_sociodemografico",
      !has_country ~ "sin_pais",
      TRUE ~ "usable"
    )
  )


# ==============================================================================
# 9. RESÚMENES DE CONTROL
# ==============================================================================

quality_summary <- df_complete_quality %>%
  count(response_quality, name = "n") %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1)
  ) %>%
  arrange(desc(n))

sociodemographic_quality_summary <- df_complete_quality %>%
  summarise(
    n_rows = n(),
    n_with_year_of_birth = sum(!is.na(year_of_birth_quality)),
    n_with_age = sum(!is.na(age_quality)),
    n_with_gender = sum(!is.na(gender_quality)),
    n_with_country = sum(!is.na(country_quality)),
    n_with_education = sum(!is.na(education_quality)),
    n_with_any_sociodemographic = sum(has_any_sociodemographic),
    n_without_sociodemographic = sum(!has_any_sociodemographic)
  )

gender_quality_summary <- df_complete_quality %>%
  count(gender_quality, name = "n") %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1)
  ) %>%
  arrange(desc(n))

education_quality_summary <- df_complete_quality %>%
  count(education_quality, name = "n") %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1)
  ) %>%
  arrange(desc(n))

country_quality_summary <- df_complete_quality %>%
  count(country_quality, name = "n") %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1)
  ) %>%
  arrange(desc(n))

age_group_quality_summary <- df_complete_quality %>%
  count(age_group_quality, name = "n") %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1)
  ) %>%
  arrange(age_group_quality)

identifier_quality_summary <- df_complete_quality %>%
  count(source_survey, identifier_type, name = "n") %>%
  group_by(source_survey) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1)
  ) %>%
  ungroup() %>%
  arrange(source_survey, desc(n))

suspicious_identifiers_detail <- df_complete_quality %>%
  filter(suspicious_join_key == TRUE) %>%
  select(
    row_id_global,
    source_survey,
    merge_status,
    idioma,
    fecha,
    join_key,
    identifier_type,
    suspicious_join_key,
    duplicate_type,
    response_quality
  ) %>%
  arrange(join_key, source_survey, fecha)

duplicate_key_audit_detail <- df_complete_quality %>%
  filter(!is.na(join_key)) %>%
  select(
    row_id_global,
    source_survey,
    merge_status,
    idioma,
    fecha,
    join_key,
    identifier_type,
    duplicate_type,
    n_rows_join_key,
    n_surveys_join_key,
    surveys_join_key,
    response_quality
  ) %>%
  filter(duplicate_type != "not_duplicated") %>%
  arrange(join_key, source_survey, fecha)


# ==============================================================================
# 10. MODELO DE PROPENSIÓN DE VOTO / ABSTENCIÓN
# ==============================================================================
#
# Este bloque estima, para cada respuesta, la probabilidad de abstención y la
# probabilidad de voto a partir de variables sociodemográficas.
#
# No calcula pesos, no hace postestratificación y no hace bootstrap.
# Solo añade:
#   - vote_response_raw
#   - vote_status_declared
#   - abstained_observed
#   - p_abstain_model
#   - p_vote_model
#
# ==============================================================================


# ------------------------------------------------------------------------------
# 10.1. Funciones auxiliares para voto y predictores
# ------------------------------------------------------------------------------

clean_vote_status_declared <- function(x) {
  x <- clean_text_basic(x)
  x <- str_remove(x, "\\s*\\(ID[0-9]+\\)$")
  x <- str_remove(x, "\\.$")
  x <- str_squish(x)
  x_clean <- str_to_lower(x)
  
  case_when(
    is.na(x_clean) ~ NA_character_,
    
    str_detect(
      x_clean,
      "do not vote|don't vote|abstain|abstention|blank|null|invalid"
    ) ~ "abstainer",
    
    str_detect(
      x_clean,
      "national parties|regionalist|pro-independence|candidate|program|programme|type of election|i vote|always|usually|regularly|most elections"
    ) ~ "voter",
    
    str_detect(
      x_clean,
      "other options|other|prefer not|rather not|depends|sometimes|occasionally"
    ) ~ "uncertain",
    
    TRUE ~ "uncertain"
  )
}

clean_city_size_quality <- function(x) {
  x <- clean_text_basic(x)
  x <- str_remove(x, "\\s*\\(ID[0-9]+\\)$")
  x <- str_remove(x, "\\.$")
  x <- str_squish(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("^Village", ignore_case = TRUE)) ~ "Village / rural area",
    str_detect(x, regex("^Small town", ignore_case = TRUE)) ~ "Small town",
    str_detect(x, regex("^Town", ignore_case = TRUE)) ~ "Town",
    str_detect(x, regex("^Small city", ignore_case = TRUE)) ~ "Small city",
    str_detect(x, regex("^Medium city", ignore_case = TRUE)) ~ "Medium city",
    str_detect(x, regex("^Large city", ignore_case = TRUE)) ~ "Large city",
    str_detect(x, regex("^Global city|Metropolis", ignore_case = TRUE)) ~ "Metropolis",
    TRUE ~ "Other / missing"
  )
}

clean_employment_quality <- function(x) {
  x <- clean_text_basic(x)
  x <- str_remove(x, "\\s*\\(ID[0-9]+\\)$")
  x <- str_remove(x, "\\.$")
  x <- str_squish(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("full.?time|part.?time|self.?employed|employed", ignore_case = TRUE)) ~ "Employed",
    str_detect(x, regex("student", ignore_case = TRUE)) ~ "Student",
    str_detect(x, regex("unemployed", ignore_case = TRUE)) ~ "Unemployed",
    str_detect(x, regex("retired", ignore_case = TRUE)) ~ "Retired",
    str_detect(x, regex("stay.?at.?home|homemaker|care", ignore_case = TRUE)) ~ "Other inactive",
    TRUE ~ "Other"
  )
}

clean_income_quality <- function(x) {
  x <- clean_text_basic(x)
  x <- str_remove(x, "\\s*\\(ID[0-9]+\\)$")
  x <- str_remove(x, "\\.$")
  x <- str_squish(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    TRUE ~ x
  )
}

collapse_small_categories <- function(x, min_n = 10, missing_label = "Missing") {
  x <- clean_text_basic(x)
  x <- replace_na(x, missing_label)
  
  freq <- table(x)
  keep_levels <- names(freq[freq >= min_n])
  
  ifelse(x %in% keep_levels, x, "Other / small n")
}


# ------------------------------------------------------------------------------
# 10.2. Extraer respuesta declarada sobre comportamiento electoral
# ------------------------------------------------------------------------------

vote_candidates <- c(
  "which_of_the_following_best_describes_your_general_approach_to_voting_in_elections_final",
  "which_of_the_following_best_describes_your_general_approach_to_voting_in_elections",
  "which_of_the_following_best_describes_your_usual_voting_behaviour"
)

df_complete_quality <- df_complete_quality %>%
  mutate(
    vote_response_raw = coalesce_optional_cols(., vote_candidates),
    vote_status_declared = clean_vote_status_declared(vote_response_raw),
    
    abstained_observed = case_when(
      vote_status_declared == "abstainer" ~ 1,
      vote_status_declared == "voter" ~ 0,
      TRUE ~ NA_real_
    )
  )

vote_response_mapping <- df_complete_quality %>%
  count(vote_response_raw, vote_status_declared, name = "n") %>%
  arrange(vote_status_declared, desc(n))

vote_status_summary <- df_complete_quality %>%
  count(vote_status_declared, name = "n") %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1)
  ) %>%
  arrange(desc(n))


# ------------------------------------------------------------------------------
# 10.3. Crear predictores sociodemográficos para el modelo
# ------------------------------------------------------------------------------

city_size_candidates <- c(
  "what_is_the_approximate_population_size_of_the_city_where_you_live_final",
  "what_is_the_approximate_population_size_of_the_city_where_you_live"
)

employment_candidates <- c(
  "what_is_your_current_employment_status_please_indicate_your_main_contractual_status_if_you_are_temporarily_on_leave_such_as_sick_leave_parental_leave_or_a_temporary_reduction_in_working_hours_please_report_your_usual_employment_status_final",
  "what_is_your_current_employment_status_please_indicate_your_main_contractual_status_if_you_are_temporarily_on_leave_such_as_sick_leave_parental_leave_or_a_temporary_reduction_in_working_hours_please_report_your_usual_employment_status",
  "please_complete_one_row_for_each_member_of_your_household_starting_with_yourself_select_the_option_in_each_column_that_corresponds_to_each_household_member_respondent_employment_status"
)

income_candidates <- c(
  "what_is_approximately_the_current_total_net_monthly_income_of_your_household_please_include_from_all_sources_selected_above_final",
  "what_is_approximately_the_current_total_net_monthly_income_of_your_household_please_include_from_all_sources_selected_above"
)

df_complete_quality <- df_complete_quality %>%
  mutate(
    city_size_quality_raw = coalesce_optional_cols(., city_size_candidates),
    employment_quality_raw = coalesce_optional_cols(., employment_candidates),
    income_quality_raw = coalesce_optional_cols(., income_candidates),
    
    city_size_quality = clean_city_size_quality(city_size_quality_raw),
    employment_quality = clean_employment_quality(employment_quality_raw),
    income_quality = clean_income_quality(income_quality_raw),
    
    age_group_model = replace_na(age_group_quality, "Missing"),
    gender_model = replace_na(gender_quality, "Missing"),
    education_model = replace_na(education_quality, "Missing"),
    country_model = replace_na(country_quality, "Missing"),
    city_size_model = replace_na(city_size_quality, "Missing"),
    employment_model = replace_na(employment_quality, "Missing"),
    income_model = replace_na(income_quality, "Missing")
  )

df_complete_quality <- df_complete_quality %>%
  mutate(
    country_model_grouped = collapse_small_categories(country_model, min_n = 10),
    city_size_model_grouped = collapse_small_categories(city_size_model, min_n = 10),
    employment_model_grouped = collapse_small_categories(employment_model, min_n = 10),
    income_model_grouped = collapse_small_categories(income_model, min_n = 10)
  )


# ------------------------------------------------------------------------------
# 10.4. Ajustar modelo logístico de propensión a la abstención
# ------------------------------------------------------------------------------

candidate_predictors <- c(
  "age_group_model",
  "gender_model",
  "education_model",
  "country_model_grouped",
  "city_size_model_grouped",
  "employment_model_grouped",
  "income_model_grouped"
)

df_complete_quality <- df_complete_quality %>%
  mutate(
    across(all_of(candidate_predictors), as.factor)
  )

vote_model_data <- df_complete_quality %>%
  filter(
    response_quality == "usable",
    !is.na(abstained_observed)
  ) %>%
  select(abstained_observed, all_of(candidate_predictors))

if (nrow(vote_model_data) == 0 || length(unique(vote_model_data$abstained_observed)) < 2) {
  vote_model_data <- df_complete_quality %>%
    filter(!is.na(abstained_observed)) %>%
    select(abstained_observed, all_of(candidate_predictors))
}

if (nrow(vote_model_data) == 0 || length(unique(vote_model_data$abstained_observed)) < 2) {
  stop("No hay suficientes casos de votantes y abstencionistas para ajustar el modelo de propensión.")
}

predictors_used <- candidate_predictors[
  sapply(candidate_predictors, function(v) {
    n_distinct(as.character(vote_model_data[[v]]), na.rm = TRUE) >= 2
  })
]

if (length(predictors_used) == 0) {
  stop("No hay predictores con variación suficiente para ajustar el modelo.")
}

vote_model_formula <- as.formula(
  paste(
    "abstained_observed ~",
    paste(predictors_used, collapse = " + ")
  )
)

vote_propensity_model <- glm(
  formula = vote_model_formula,
  data = vote_model_data,
  family = binomial()
)

# ------------------------------------------------------------------------------
# Predicción segura: evitar niveles nuevos no vistos en entrenamiento
# ------------------------------------------------------------------------------

make_prediction_data_safe <- function(newdata, model, predictors) {
  
  prediction_data <- newdata
  
  for (v in predictors) {
    
    allowed_levels <- model$xlevels[[v]]
    
    if (!is.null(allowed_levels)) {
      
      fallback_level <- if ("Other / small n" %in% allowed_levels) {
        "Other / small n"
      } else if ("Missing" %in% allowed_levels) {
        "Missing"
      } else {
        allowed_levels[1]
      }
      
      prediction_data[[v]] <- as.character(prediction_data[[v]])
      prediction_data[[v]][!prediction_data[[v]] %in% allowed_levels] <- fallback_level
      prediction_data[[v]] <- factor(prediction_data[[v]], levels = allowed_levels)
    }
  }
  
  prediction_data
}

prediction_data <- make_prediction_data_safe(
  newdata = df_complete_quality,
  model = vote_propensity_model,
  predictors = predictors_used
)

df_complete_quality$p_abstain_model <- predict(
  vote_propensity_model,
  newdata = prediction_data,
  type = "response"
)

df_complete_quality <- df_complete_quality %>%
  mutate(
    p_abstain_model = pmin(pmax(p_abstain_model, 0.01), 0.99),
    p_vote_model = 1 - p_abstain_model
  )


# ------------------------------------------------------------------------------
# 10.5. Resúmenes del modelo de propensión
# ------------------------------------------------------------------------------

vote_propensity_summary <- df_complete_quality %>%
  summarise(
    n_rows = n(),
    n_with_vote_response_raw = sum(!is.na(vote_response_raw)),
    n_classified_as_voter = sum(vote_status_declared == "voter", na.rm = TRUE),
    n_classified_as_abstainer = sum(vote_status_declared == "abstainer", na.rm = TRUE),
    n_uncertain_or_missing = sum(is.na(abstained_observed)),
    observed_abstention_rate = round(mean(abstained_observed, na.rm = TRUE) * 100, 1),
    mean_p_abstain_model = round(mean(p_abstain_model, na.rm = TRUE) * 100, 1),
    mean_p_vote_model = round(mean(p_vote_model, na.rm = TRUE) * 100, 1),
    n_training_rows = nrow(vote_model_data),
    n_predictors_used = length(predictors_used)
  )

vote_predictors_used <- tibble(
  predictor = predictors_used
)

vote_model_coefficients <- as.data.frame(summary(vote_propensity_model)$coefficients) %>%
  tibble::rownames_to_column("term") %>%
  rename(
    estimate = Estimate,
    std_error = `Std. Error`,
    z_value = `z value`,
    p_value = `Pr(>|z|)`
  )
# ==============================================================================
# 10. GUARDAR ARCHIVOS
# ==============================================================================

write_csv(
  df_complete_quality,
  file.path(csv_dir, "df_all_surveys_complete_with_quality.csv")
)

write_csv(
  quality_summary,
  file.path(logs_dir, "quality_summary_complete_dataset.csv")
)

write_csv(
  sociodemographic_quality_summary,
  file.path(logs_dir, "sociodemographic_quality_summary.csv")
)

write_csv(
  gender_quality_summary,
  file.path(logs_dir, "gender_quality_summary.csv")
)

write_csv(
  education_quality_summary,
  file.path(logs_dir, "education_quality_summary.csv")
)

write_csv(
  country_quality_summary,
  file.path(logs_dir, "country_quality_summary.csv")
)

write_csv(
  age_group_quality_summary,
  file.path(logs_dir, "age_group_quality_summary.csv")
)

write_csv(
  identifier_quality_summary,
  file.path(logs_dir, "identifier_quality_summary.csv")
)

write_csv(
  suspicious_identifiers_detail,
  file.path(logs_dir, "suspicious_identifiers_detail.csv")
)

write_csv(
  duplicate_key_audit_detail,
  file.path(logs_dir, "duplicate_key_audit_detail.csv")
)

write_csv(
  df_complete_quality,
  file.path(csv_dir, "df_all_surveys_complete_with_quality_vote_probability.csv")
)

write_csv(
  vote_response_mapping,
  file.path(logs_dir, "vote_response_mapping.csv")
)

write_csv(
  vote_status_summary,
  file.path(logs_dir, "vote_status_summary.csv")
)

write_csv(
  vote_propensity_summary,
  file.path(logs_dir, "vote_propensity_summary.csv")
)

write_csv(
  vote_predictors_used,
  file.path(logs_dir, "vote_predictors_used.csv")
)

write_csv(
  vote_model_coefficients,
  file.path(logs_dir, "vote_model_coefficients.csv")
)
# ==============================================================================
# 11. COMPROBACIONES FINALES EN CONSOLA
# ==============================================================================

cat("\nDataset con calidad generado:\n")
cat("Filas:", nrow(df_complete_quality), "\n")
cat("Columnas:", ncol(df_complete_quality), "\n")

cat("\nResumen de calidad:\n")
print(quality_summary, n = Inf)

cat("\nResumen sociodemográfico:\n")
print(sociodemographic_quality_summary)

cat("\nGénero limpio:\n")
print(gender_quality_summary, n = Inf)

cat("\nEducación limpia:\n")
print(education_quality_summary, n = Inf)

cat("\nIdentificadores sospechosos:\n")
print(suspicious_identifiers_detail, n = Inf)
  
cat("\nArchivos guardados:\n")
cat("- ", file.path(csv_dir, "df_all_surveys_complete_with_quality.csv"), "\n")
cat("- ", file.path(logs_dir, "quality_summary_complete_dataset.csv"), "\n")
cat("- ", file.path(logs_dir, "sociodemographic_quality_summary.csv"), "\n")
cat("- ", file.path(logs_dir, "gender_quality_summary.csv"), "\n")
cat("- ", file.path(logs_dir, "education_quality_summary.csv"), "\n")
cat("- ", file.path(logs_dir, "country_quality_summary.csv"), "\n")
cat("- ", file.path(logs_dir, "age_group_quality_summary.csv"), "\n")
cat("- ", file.path(logs_dir, "identifier_quality_summary.csv"), "\n")
cat("- ", file.path(logs_dir, "suspicious_identifiers_detail.csv"), "\n")
cat("- ", file.path(logs_dir, "duplicate_key_audit_detail.csv"), "\n")


cat("\nResumen voto / abstención:\n")
print(vote_status_summary, n = Inf)

cat("\nResumen del modelo de propensión:\n")
print(vote_propensity_summary)

cat("\nPredictores usados en el modelo:\n")
print(vote_predictors_used, n = Inf)

cat("\nArchivos de propensión guardados:\n")
cat("- ", file.path(csv_dir, "df_all_surveys_complete_with_quality_vote_probability.csv"), "\n")
cat("- ", file.path(logs_dir, "vote_response_mapping.csv"), "\n")
cat("- ", file.path(logs_dir, "vote_status_summary.csv"), "\n")
cat("- ", file.path(logs_dir, "vote_propensity_summary.csv"), "\n")
cat("- ", file.path(logs_dir, "vote_predictors_used.csv"), "\n")
cat("- ", file.path(logs_dir, "vote_model_coefficients.csv"), "\n")