
# SCRIPT 03 - PREPARACIÓN FINAL DEL DATASET PARA ANÁLISIS

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(tibble)

# RUTAS
base_output_dir <- "initial_descriptive_analysis/output/data_preparation"

csv_dir <- file.path(base_output_dir, "csv")
logs_dir <- file.path(base_output_dir, "logs")
clean_output_dir <- "initial_descriptive_analysis/output/clean_datasets"

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(clean_output_dir, recursive = TRUE, showWarnings = FALSE)


# FUNCIONES AUXILIARES
clean_text_basic <- function(x) {
  x <- str_squish(as.character(x))
  x <- na_if(x, "")
  x <- na_if(x, "NA")
  x <- na_if(x, "NaN")
  x
}

clean_response_ids <- function(x) {
  x <- clean_text_basic(x)
  
  x <- str_replace_all(
    x,
    "\\s*\\(ID[0-9]+\\)",
    ""
  )
  
  x <- str_squish(x)
  x
}
get_optional_col <- function(data, col_name) {
  if (col_name %in% names(data)) {
    as.character(data[[col_name]])
  } else {
    rep(NA_character_, nrow(data))
  }
}

coalesce_optional_cols <- function(data, candidates) {
  existing <- candidates[candidates %in% names(data)]
  
  if (length(existing) == 0) {
    return(rep(NA_character_, nrow(data)))
  }
  
  values <- lapply(existing, function(col) as.character(data[[col]]))
  do.call(coalesce, values)
}

clean_id_value <- function(x) {
  x <- str_squish(as.character(x))
  x <- na_if(x, "")
  x <- na_if(x, "NA")
  x <- na_if(x, "NaN")
  x
}

clean_country_final <- function(x) {
  x <- clean_text_basic(x)
  x <- str_remove(x, "\\s*\\(ID[0-9]+\\)$")
  x <- str_remove(x, "^[A-Z]{2}\\s*[–-]\\s*")
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    x == "United Kingdom *" ~ "United Kingdom",
    x == "UK" ~ "United Kingdom",
    x == "Great Britain" ~ "United Kingdom",
    x == "Czechia" ~ "Czech Republic",
    TRUE ~ x
  )
}

clean_residence_region <- function(country) {
  case_when(
    country %in% c(
      "Denmark", "Estonia", "Finland", "Ireland", "Iceland",
      "Latvia", "Lithuania", "Norway", "United Kingdom", "Sweden"
    ) ~ "Europa del Norte",
    
    country %in% c(
      "Germany", "Austria", "Belgium", "France", "Liechtenstein",
      "Luxembourg", "Monaco", "Netherlands", "Switzerland"
    ) ~ "Europa Occidental",
    
    country %in% c(
      "Albania", "Andorra", "Bosnia and Herzegovina", "Croatia",
      "Slovenia", "Spain", "Greece", "Italy", "Malta",
      "Montenegro", "Portugal", "North Macedonia", "San Marino",
      "Serbia", "Cyprus"
    ) ~ "Europa del Sur",
    
    country %in% c(
      "Belarus", "Bulgaria", "Slovakia", "Hungary",
      "Moldova", "Moldova (Republic of Moldova)",
      "Poland", "Czech Republic", "Czechia",
      "Romania", "Russia", "Russian Federation", "Ukraine"
    ) ~ "Europa del Este",
    
    is.na(country) ~ NA_character_,
    TRUE ~ "Otra región"
  )
}

as_logical_safe <- function(x) {
  x <- str_to_lower(str_squish(as.character(x)))
  
  case_when(
    x %in% c("true", "t", "1", "yes") ~ TRUE,
    x %in% c("false", "f", "0", "no") ~ FALSE,
    TRUE ~ FALSE
  )
}

count_non_missing <- function(data, cols) {
  if (length(cols) == 0) {
    return(rep(0, nrow(data)))
  }
  
  rowSums(!is.na(data[, cols, drop = FALSE]))
}

classify_attention_check <- function(answer, check_exists, expected_regex = NULL, expected_exact = NULL) {
  
  answer <- clean_text_basic(answer)
  
  if (!check_exists) {
    return(rep("no_aplica_columna_no_existe", length(answer)))
  }
  
  status <- rep(NA_character_, length(answer))
  status[is.na(answer)] <- "no_responde"
  
  idx_answered <- !is.na(answer)
  
  if (!is.null(expected_exact)) {
    status[idx_answered & answer == expected_exact] <- "pasa"
  }
  
  if (!is.null(expected_regex)) {
    status[idx_answered & str_detect(answer, regex(expected_regex, ignore_case = TRUE))] <- "pasa"
  }
  
  status[idx_answered & is.na(status)] <- "falla"
  
  status
}

# 3. CARGAR DATASET EXTENDIDO
df_extended <- read_csv(
  file.path(csv_dir, "df_decision_extended.csv"),
  show_col_types = FALSE
)

cat("Dataset extendido cargado\n")
cat("Filas:", nrow(df_extended), "\n")
cat("Columnas:", ncol(df_extended), "\n")


# CREAR df_analysis_ready
cols_final <- names(df_extended)[str_detect(names(df_extended), "_final$")]
cols_original_with_final <- str_remove(cols_final, "_final$")

cols_remove <- names(df_extended)[
  str_detect(names(df_extended), "_external$|_conflict$") |
    names(df_extended) %in% cols_original_with_final
]

df_analysis <- df_extended %>%
  select(-all_of(cols_remove)) %>%
  mutate(
    across(
      where(is.character),
      clean_response_ids
    )
  )

cat("df_analysis_ready creado\n")
cat("Filas:", nrow(df_analysis), "\n")
cat("Columnas:", ncol(df_analysis), "\n")



# VERSIÓN AUXILIAR PARA CONTROL DE CALIDAD
df_check <- df_analysis %>%
  mutate(across(everything(), clean_text_basic))

df_check$n_non_missing <- rowSums(!is.na(df_check))

df_check <- df_check %>%
  mutate(
    row_id = row_number(),
    participant_id = coalesce(
      as.character(get_optional_col(., "join_key")),
      as.character(get_optional_col(., "prolific_id")),
      as.character(get_optional_col(., "identification_code")),
      as.character(row_number())
    ),
    participant_id_check = participant_id
  )

if (!"source_survey" %in% names(df_check)) {
  df_check <- df_check %>%
    mutate(source_survey = NA_character_)
}

# IDENTIFICADORES
df_id_quality <- df_check %>%
  mutate(
    prolific_value = coalesce(
      clean_id_value(get_optional_col(., "prolific_id")),
      clean_id_value(get_optional_col(., "please_provide_your_prolific_id"))
    ),
    
    code_value = coalesce(
      clean_id_value(get_optional_col(., "identification_code")),
      clean_id_value(get_optional_col(., "please_provide_your_identification_code"))
    ),
    
    join_key_clean = clean_id_value(get_optional_col(., "join_key")),
    
    has_prolific = !is.na(prolific_value) |
      str_detect(coalesce(join_key_clean, ""), "^PROLIFIC_"),
    
    has_code = !is.na(code_value) |
      str_detect(coalesce(join_key_clean, ""), "^CODE_"),
    
    identifier_type = case_when(
      has_prolific & has_code ~ "prolific_y_codigo",
      has_prolific ~ "prolific",
      has_code ~ "codigo",
      TRUE ~ "sin_identificador"
    )
  )

identifier_summary_rows <- df_id_quality %>%
  count(identifier_type, name = "n_rows") %>%
  mutate(
    percentage_rows = round(n_rows / sum(n_rows) * 100, 1)
  ) %>%
  arrange(desc(n_rows))

identifier_summary_by_source <- df_id_quality %>%
  count(source_survey, identifier_type, name = "n_rows") %>%
  group_by(source_survey) %>%
  mutate(
    percentage_within_source = round(n_rows / sum(n_rows) * 100, 1)
  ) %>%
  ungroup() %>%
  arrange(source_survey, desc(n_rows))

rows_without_identifier <- df_id_quality %>%
  filter(identifier_type == "sin_identificador") %>%
  select(
    row_id,
    participant_id_check,
    source_survey,
    identifier_type,
    any_of(c(
      "join_key",
      "prolific_id",
      "identification_code",
      "please_provide_your_prolific_id",
      "please_provide_your_identification_code"
    ))
  )


# VARIABLES SOCIODEMOGRÁFICAS
# VARIABLES SOCIODEMOGRÁFICAS

year_of_birth_col <- "please_enter_your_year_of_birth_final"
year_of_birth_alt_col <- "year_of_birth"
age_col <- "age"

gender_col <- "what_is_your_gender_final"

country_col <- "in_which_country_do_you_currently_live_final"

size_city_col <- "what_is_the_approximate_population_size_of_the_city_where_you_live_final"

climate_zone_col <- "in_which_climate_zone_do_you_live_please_select_the_option_that_corresponds_to_your_location_if_you_re_not_sure_which_climate_zone_you_live_in_please_refer_to_the_map_below_or_select_the_option_that_best_matches_your_region_final"

employment_col <- "what_is_your_current_employment_status_please_indicate_your_main_contractual_status_if_you_are_temporarily_on_leave_such_as_sick_leave_parental_leave_or_a_temporary_reduction_in_working_hours_please_report_your_usual_employment_status_final"

education_level_col <- "what_is_the_highest_level_of_education_that_you_have_completed_if_you_are_currently_studying_and_have_not_yet_completed_a_level_please_select_the_last_level_you_have_finished_final"

work_home_col <- "do_you_currently_work_or_study_from_home_final"

type_house_col <- "what_type_of_household_do_you_live_in_please_select_the_option_that_best_describes_your_household_final"

tenure_col <- "what_is_the_current_tenure_status_of_your_home_final"

political_col <- "on_a_scale_from_0_to_100_where_0_means_most_left_and_100_means_most_right_where_would_you_place_yourself_politically_final"

vote_col <- "which_of_the_following_best_describes_your_general_approach_to_voting_in_elections_final"


# Todas las sociodemográficas disponibles para decir si una fila tiene perfil sociodemográfico
sociodemographic_cols <- c(
  year_of_birth_col,
  year_of_birth_alt_col,
  age_col,
  gender_col,
  country_col,
  size_city_col,
  climate_zone_col,
  employment_col,
  education_level_col,
  work_home_col,
  type_house_col,
  tenure_col,
  political_col,
  vote_col
)

# Sociodemográficas clave mínimas
# Estas sirven para saber si la fila tiene perfil básico, no todo el perfil completo
key_sociodemographic_cols <- c(
  year_of_birth_col,
  year_of_birth_alt_col,
  age_col,
  gender_col,
  country_col
)

sociodemographic_cols_present <- intersect(sociodemographic_cols, names(df_id_quality))
key_sociodemographic_cols_present <- intersect(key_sociodemographic_cols, names(df_id_quality))

df_sociodemographic_quality <- df_id_quality %>%
  mutate(
    n_sociodemographic_non_missing =
      count_non_missing(., sociodemographic_cols_present),
    
    n_key_sociodemographic_non_missing =
      count_non_missing(., key_sociodemographic_cols_present),
    
    has_any_sociodemographic =
      n_sociodemographic_non_missing > 0,
    
    has_no_sociodemographic =
      n_sociodemographic_non_missing == 0,
    
    has_no_key_sociodemographic =
      n_key_sociodemographic_non_missing == 0
  )

sociodemographic_summary <- df_sociodemographic_quality %>%
  summarise(
    total_rows = n(),
    rows_without_any_sociodemographic = sum(has_no_sociodemographic),
    rows_with_any_sociodemographic = sum(has_any_sociodemographic),
    percentage_without_any_sociodemographic =
      round(rows_without_any_sociodemographic / total_rows * 100, 1),
    rows_without_age_gender_country = sum(has_no_key_sociodemographic),
    percentage_without_age_gender_country =
      round(rows_without_age_gender_country / total_rows * 100, 1)
  )

rows_without_sociodemographic <- df_sociodemographic_quality %>%
  filter(has_no_sociodemographic) %>%
  select(
    row_id,
    participant_id_check,
    source_survey,
    identifier_type,
    has_prolific,
    has_code,
    n_sociodemographic_non_missing,
    n_key_sociodemographic_non_missing,
    any_of(c(
      "join_key",
      "prolific_id",
      "identification_code",
      "please_provide_your_prolific_id",
      "please_provide_your_identification_code"
    ))
  )

rows_without_sociodemographic_by_source <- rows_without_sociodemographic %>%
  count(source_survey, name = "n") %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

rows_without_sociodemographic_full <- df_sociodemographic_quality %>%
  filter(has_no_sociodemographic) %>%
  select(
    row_id,
    participant_id_check,
    source_survey,
    identifier_type,
    has_prolific,
    has_code,
    n_sociodemographic_non_missing,
    n_key_sociodemographic_non_missing,
    any_of(c(
      "join_key",
      "prolific_id",
      "identification_code",
      "please_provide_your_prolific_id",
      "please_provide_your_identification_code",
      "has_previous_survey",
      "needs_external_data",
      "problematic_merge"
    )),
    everything()
  )


# PAÍS LIMPIO Y REGIÓN
country_col <- "in_which_country_do_you_currently_live_final"

df_country_quality <- df_sociodemographic_quality %>%
  mutate(
    country_clean = if (country_col %in% names(.)) {
      clean_country_final(.data[[country_col]])
    } else {
      NA_character_
    },
    
    residence_region = clean_residence_region(country_clean),
    
    has_country = !is.na(country_clean)
  )


# ATTENTION CHECKS
line_check_candidates <- c(
  "look_at_the_three_horizontal_lines_in_the_image_which_line_is_longest",
  "look_at_the_three_horizontal_lines_in_the_image_which_line_is_the_longest"
)

check_42_candidates <- c(
  "please_follow_the_instruction_below_when_selecting_your_answer_select_42"
)

check_4_candidates <- c(
  "please_follow_the_instruction_below_when_selecting_your_answer_select_option_4"
)

strongly_disagree_candidates <- c(
  "please_select_strongly_disagree"
)

line_check_exists <- any(line_check_candidates %in% names(df_country_quality))
check_42_exists <- any(check_42_candidates %in% names(df_country_quality))
check_4_exists <- any(check_4_candidates %in% names(df_country_quality))
strongly_disagree_exists <- any(strongly_disagree_candidates %in% names(df_country_quality))

df_attention_quality <- df_country_quality %>%
  mutate(
    line_check_answer = clean_text_basic(
      coalesce_optional_cols(., line_check_candidates)
    ),
    
    check_42_answer = clean_text_basic(
      coalesce_optional_cols(., check_42_candidates)
    ),
    
    check_4_answer = clean_text_basic(
      coalesce_optional_cols(., check_4_candidates)
    ),
    
    strongly_disagree_answer = clean_text_basic(
      coalesce_optional_cols(., strongly_disagree_candidates)
    ),
    
    line_check_status = classify_attention_check(
      line_check_answer,
      check_exists = line_check_exists,
      expected_regex = "^All lines are the same len.*gth\\.?$"
    ),
    
    check_42_status = classify_attention_check(
      check_42_answer,
      check_exists = check_42_exists,
      expected_regex = "^42\\.?$"
    ),
    
    check_4_status = classify_attention_check(
      check_4_answer,
      check_exists = check_4_exists,
      expected_regex = "^Option 4\\.?$|^4\\.?$"
    ),
    
    strongly_disagree_status = classify_attention_check(
      strongly_disagree_answer,
      check_exists = strongly_disagree_exists,
      expected_regex = "Strongly disagree"
    ),
    
    pass_line_check = line_check_status == "pasa",
    pass_42_check = check_42_status == "pasa",
    pass_4_check = check_4_status == "pasa",
    pass_strongly_disagree_check = strongly_disagree_status == "pasa",
    
    fail_line_check = line_check_status == "falla",
    fail_42_check = check_42_status == "falla",
    fail_4_check = check_4_status == "falla",
    fail_strongly_disagree_check = strongly_disagree_status == "falla",
    
    no_response_line_check = line_check_status == "no_responde",
    no_response_42_check = check_42_status == "no_responde",
    no_response_4_check = check_4_status == "no_responde",
    no_response_strongly_disagree_check = strongly_disagree_status == "no_responde",
    
    failed_attention_check_decision =
      fail_line_check | fail_42_check,
    
    failed_attention_check_other =
      fail_4_check | fail_strongly_disagree_check,
    
    failed_attention_check_any =
      failed_attention_check_decision | failed_attention_check_other,
    
    # Para row_quality usamos solo los checks principales de RV-Decision.
    # Se marcan, pero NO se eliminan por ahora en los datasets limpios.
    failed_attention_check =
      failed_attention_check_decision,
    
    no_response_attention_check =
      no_response_line_check |
      no_response_42_check |
      no_response_4_check |
      no_response_strongly_disagree_check
  )


# PROBLEMATIC MERGE
df_merge_quality <- df_attention_quality %>%
  mutate(
    problematic_merge_flag = if ("problematic_merge" %in% names(.)) {
      as_logical_safe(.data[["problematic_merge"]])
    } else {
      FALSE
    }
  )

# DUPLICADOS
df_duplicate_quality <- df_merge_quality %>%
  group_by(participant_id) %>%
  arrange(desc(n_non_missing), .by_group = TRUE) %>%
  mutate(
    duplicate_n = n(),
    duplicate_rank = row_number(),
    duplicated_participant = duplicate_n > 1 & duplicate_rank > 1
  ) %>%
  ungroup()


# POSIBLE STRAIGHTLINING EN DETERMINANTES
det_start <- match(
  "profits_profits_are_what_guide_my_decision_making_i_always_prefer_to_earn_or_save_money_with_every_decision_i_take",
  names(df_duplicate_quality)
)

det_end <- match(
  "own_significance_i_only_make_a_decision_if_the_action_has_a_personal_inner_meaning_for_me_beyond_any_economic_gain",
  names(df_duplicate_quality)
)

determinant_cols <- if (!is.na(det_start) & !is.na(det_end)) {
  names(df_duplicate_quality)[det_start:det_end]
} else {
  character(0)
}

if (length(determinant_cols) > 0) {
  
  det_numeric <- df_duplicate_quality %>%
    select(all_of(determinant_cols)) %>%
    mutate(across(everything(), ~ suppressWarnings(as.numeric(.x))))
  
  n_determinants_valid <- rowSums(!is.na(det_numeric))
  
  determinant_sd <- apply(det_numeric, 1, function(x) {
    x <- x[!is.na(x)]
    if (length(x) <= 1) {
      NA_real_
    } else {
      sd(x)
    }
  })
  
  df_straightlining_quality <- df_duplicate_quality %>%
    mutate(
      n_determinants_valid = n_determinants_valid,
      determinant_sd = determinant_sd,
      possible_straightlining =
        n_determinants_valid >= 20 &
        !is.na(determinant_sd) &
        determinant_sd == 0
    )
  
} else {
  
  df_straightlining_quality <- df_duplicate_quality %>%
    mutate(
      n_determinants_valid = NA_integer_,
      determinant_sd = NA_real_,
      possible_straightlining = FALSE
    )
}

# CLASIFICACIÓN FINAL DE CALIDAD
df_quality <- df_straightlining_quality %>%
  mutate(
    row_quality = case_when(
      n_non_missing <= 5 ~ "casi_vacia_0_5",
      n_non_missing <= 10 ~ "muy_incompleta_6_10",
      problematic_merge_flag ~ "problematic_merge",
      duplicated_participant ~ "duplicado",
      failed_attention_check ~ "attention_check_fallido",
      !has_any_sociodemographic ~ "sin_sociodemografico",
      !has_country ~ "sin_pais",
      TRUE ~ "usable"
    )
  )

quality_summary <- df_quality %>%
  count(row_quality, name = "n") %>%
  mutate(percentage = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

problematic_rows <- df_quality %>%
  filter(row_quality != "usable" | possible_straightlining) %>%
  select(
    row_id,
    participant_id,
    source_survey,
    identifier_type,
    n_non_missing,
    country_clean,
    residence_region,
    has_country,
    n_sociodemographic_non_missing,
    n_key_sociodemographic_non_missing,
    
    line_check_answer,
    check_42_answer,
    check_4_answer,
    strongly_disagree_answer,
    
    line_check_status,
    check_42_status,
    check_4_status,
    strongly_disagree_status,
    
    pass_line_check,
    pass_42_check,
    pass_4_check,
    pass_strongly_disagree_check,
    
    fail_line_check,
    fail_42_check,
    fail_4_check,
    fail_strongly_disagree_check,
    
    no_response_line_check,
    no_response_42_check,
    no_response_4_check,
    no_response_strongly_disagree_check,
    no_response_attention_check,
    
    failed_attention_check,
    failed_attention_check_decision,
    failed_attention_check_other,
    failed_attention_check_any,
    
    problematic_merge_flag,
    duplicate_n,
    duplicate_rank,
    duplicated_participant,
    n_determinants_valid,
    determinant_sd,
    possible_straightlining,
    row_quality
  )


# RESÚMENES DE ATTENTION CHECKS
attention_check_status_summary <- df_quality %>%
  select(
    row_id,
    participant_id,
    line_check_status,
    check_42_status,
    check_4_status,
    strongly_disagree_status
  ) %>%
  pivot_longer(
    cols = ends_with("_status"),
    names_to = "attention_check",
    values_to = "status"
  ) %>%
  count(attention_check, status, name = "n") %>%
  group_by(attention_check) %>%
  mutate(percentage_within_check = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  arrange(attention_check, desc(n))

attention_check_failure_summary_decision <- df_quality %>%
  filter(failed_attention_check_decision) %>%
  count(
    line_check_status,
    check_42_status,
    check_4_status,
    strongly_disagree_status,
    name = "n"
  ) %>%
  arrange(desc(n))

attention_check_failure_summary_any <- df_quality %>%
  filter(failed_attention_check_any) %>%
  count(
    line_check_status,
    check_42_status,
    check_4_status,
    strongly_disagree_status,
    name = "n"
  ) %>%
  arrange(desc(n))

attention_check_person_summary <- df_quality %>%
  rowwise() %>%
  mutate(
    n_attention_checks_pasa = sum(c_across(c(
      line_check_status,
      check_42_status,
      check_4_status,
      strongly_disagree_status
    )) == "pasa", na.rm = TRUE),
    
    n_attention_checks_falla = sum(c_across(c(
      line_check_status,
      check_42_status,
      check_4_status,
      strongly_disagree_status
    )) == "falla", na.rm = TRUE),
    
    n_attention_checks_no_responde = sum(c_across(c(
      line_check_status,
      check_42_status,
      check_4_status,
      strongly_disagree_status
    )) == "no_responde", na.rm = TRUE),
    
    n_attention_checks_no_aplica = sum(c_across(c(
      line_check_status,
      check_42_status,
      check_4_status,
      strongly_disagree_status
    )) == "no_aplica_columna_no_existe", na.rm = TRUE),
    
    n_attention_checks_respondidos =
      n_attention_checks_pasa + n_attention_checks_falla,
    
    falla_todos_los_respondidos =
      n_attention_checks_respondidos > 0 &
      n_attention_checks_falla == n_attention_checks_respondidos,
    
    falla_los_4_checks =
      line_check_status == "falla" &
      check_42_status == "falla" &
      check_4_status == "falla" &
      strongly_disagree_status == "falla",
    
    falla_los_2_checks_decision =
      line_check_status == "falla" &
      check_42_status == "falla",
    
    n_attention_checks_decision_pasa =
      sum(c_across(c(line_check_status, check_42_status)) == "pasa", na.rm = TRUE),
    
    n_attention_checks_decision_falla =
      sum(c_across(c(line_check_status, check_42_status)) == "falla", na.rm = TRUE),
    
    n_attention_checks_decision_respondidos =
      n_attention_checks_decision_pasa + n_attention_checks_decision_falla,
    
    falla_todos_checks_decision_respondidos =
      n_attention_checks_decision_respondidos > 0 &
      n_attention_checks_decision_falla == n_attention_checks_decision_respondidos
  ) %>%
  ungroup() %>%
  select(
    row_id,
    participant_id,
    source_survey,
    
    line_check_status,
    check_42_status,
    check_4_status,
    strongly_disagree_status,
    
    n_attention_checks_pasa,
    n_attention_checks_falla,
    n_attention_checks_no_responde,
    n_attention_checks_no_aplica,
    n_attention_checks_respondidos,
    
    n_attention_checks_decision_pasa,
    n_attention_checks_decision_falla,
    n_attention_checks_decision_respondidos,
    
    failed_attention_check,
    failed_attention_check_decision,
    failed_attention_check_other,
    failed_attention_check_any,
    
    falla_todos_los_respondidos,
    falla_los_4_checks,
    falla_los_2_checks_decision,
    falla_todos_checks_decision_respondidos,
    
    row_quality
  )

attention_check_all_fail_summary <- attention_check_person_summary %>%
  summarise(
    total_rows = n(),
    
    personas_que_fallan_al_menos_un_check_decision =
      sum(failed_attention_check_decision, na.rm = TRUE),
    
    personas_que_fallan_al_menos_un_check_any =
      sum(failed_attention_check_any, na.rm = TRUE),
    
    personas_que_fallan_todos_los_checks_respondidos =
      sum(falla_todos_los_respondidos, na.rm = TRUE),
    
    personas_que_fallan_los_4_checks =
      sum(falla_los_4_checks, na.rm = TRUE),
    
    personas_que_fallan_los_2_checks_decision =
      sum(falla_los_2_checks_decision, na.rm = TRUE),
    
    personas_que_fallan_todos_checks_decision_respondidos =
      sum(falla_todos_checks_decision_respondidos, na.rm = TRUE),
    
    total_fallos_attention_checks =
      sum(n_attention_checks_falla, na.rm = TRUE),
    
    total_fallos_attention_checks_decision =
      sum(n_attention_checks_decision_falla, na.rm = TRUE)
  )



# CREAR DATASETS FINALES
quality_flags <- df_quality %>%
  select(
    row_id,
    participant_id,
    identifier_type,
    has_prolific,
    has_code,
    prolific_value,
    code_value,
    country_clean,
    residence_region,
    has_country,
    n_sociodemographic_non_missing,
    n_key_sociodemographic_non_missing,
    has_any_sociodemographic,
    has_no_sociodemographic,
    has_no_key_sociodemographic,
    
    line_check_answer,
    check_42_answer,
    check_4_answer,
    strongly_disagree_answer,
    
    line_check_status,
    check_42_status,
    check_4_status,
    strongly_disagree_status,
    
    pass_line_check,
    pass_42_check,
    pass_4_check,
    pass_strongly_disagree_check,
    
    fail_line_check,
    fail_42_check,
    fail_4_check,
    fail_strongly_disagree_check,
    
    no_response_line_check,
    no_response_42_check,
    no_response_4_check,
    no_response_strongly_disagree_check,
    no_response_attention_check,
    
    failed_attention_check,
    failed_attention_check_decision,
    failed_attention_check_other,
    failed_attention_check_any,
    
    problematic_merge_flag,
    duplicate_n,
    duplicate_rank,
    duplicated_participant,
    n_determinants_valid,
    determinant_sd,
    possible_straightlining,
    row_quality
  )

df_analysis_base <- df_analysis %>%
  select(-any_of(setdiff(names(quality_flags), "row_id")))

df_analysis_enriched <- df_analysis_base %>%
  mutate(row_id = row_number()) %>%
  left_join(quality_flags, by = "row_id")

# Exclusiones duras actuales
# IMPORTANTE:
# Los fallos de attention checks NO se eliminan por ahora.
# Quedan marcados mediante:
# - row_quality == "attention_check_fallido"
# - failed_attention_check_decision
# - failed_attention_check_any

hard_exclusions <- c(
  "casi_vacia_0_5",
  "muy_incompleta_6_10",
  # "attention_check_fallido",
  "problematic_merge",
  "duplicado"
)

df_clean_general <- df_analysis_enriched %>%
  filter(!row_quality %in% hard_exclusions)

df_clean_sociodemographic <- df_analysis_enriched %>%
  filter(
    !row_quality %in% hard_exclusions,
    has_any_sociodemographic
  )

df_excluded_final <- df_analysis_enriched %>%
  filter(row_quality %in% hard_exclusions)

cleaning_summary <- tibble(
  dataset = c(
    "df_analysis_ready_original",
    "df_analysis_enriched",
    "df_clean_general",
    "df_clean_sociodemographic",
    "df_excluded_final"
  ),
  n_rows = c(
    nrow(df_analysis),
    nrow(df_analysis_enriched),
    nrow(df_clean_general),
    nrow(df_clean_sociodemographic),
    nrow(df_excluded_final)
  )
)

# GUARDAR DATASETS
write_csv(df_analysis, file.path(csv_dir, "df_analysis_ready.csv"))
write_csv(df_analysis_enriched, file.path(clean_output_dir, "df_analysis_enriched.csv"))
write_csv(df_clean_general, file.path(clean_output_dir, "df_clean_general.csv"))
write_csv(df_clean_sociodemographic, file.path(clean_output_dir, "df_clean_sociodemographic.csv"))
write_csv(df_excluded_final, file.path(clean_output_dir, "df_excluded_final.csv"))
write_csv(cleaning_summary, file.path(clean_output_dir, "cleaning_summary.csv"))


# GUARDAR LOGS
write_csv(
  tibble(column = names(df_analysis)),
  file.path(logs_dir, "column_names_df_analysis_ready.csv")
)

write_csv(
  tibble(column_removed = cols_remove),
  file.path(logs_dir, "columns_removed_df_analysis_ready.csv")
)

write_csv(identifier_summary_rows, file.path(logs_dir, "identifier_summary_rows.csv"))
write_csv(identifier_summary_by_source, file.path(logs_dir, "identifier_summary_by_source.csv"))
write_csv(rows_without_identifier, file.path(logs_dir, "rows_without_identifier.csv"))

write_csv(sociodemographic_summary, file.path(logs_dir, "sociodemographic_missing_summary.csv"))
write_csv(rows_without_sociodemographic, file.path(logs_dir, "rows_without_any_sociodemographic.csv"))
write_csv(rows_without_sociodemographic_by_source, file.path(logs_dir, "rows_without_sociodemographic_by_source.csv"))
write_csv(rows_without_sociodemographic_full, file.path(logs_dir, "rows_without_sociodemographic_full.csv"))

write_csv(quality_summary, file.path(logs_dir, "quality_summary.csv"))
write_csv(problematic_rows, file.path(logs_dir, "problematic_rows.csv"))

write_csv(attention_check_status_summary, file.path(logs_dir, "attention_check_status_summary.csv"))
write_csv(attention_check_failure_summary_decision, file.path(logs_dir, "attention_check_failure_summary_decision.csv"))
write_csv(attention_check_failure_summary_any, file.path(logs_dir, "attention_check_failure_summary_any.csv"))
write_csv(attention_check_person_summary, file.path(logs_dir, "attention_check_person_summary.csv"))
write_csv(attention_check_all_fail_summary, file.path(logs_dir, "attention_check_all_fail_summary.csv"))


# COMPROBACIONES FINALES

cat("\nDimensiones df_analysis_ready:\n")
print(dim(df_analysis))

cat("\nDimensiones df_analysis_enriched:\n")
print(dim(df_analysis_enriched))

cat("\nDistribución por source_survey:\n")
print(table(df_analysis$source_survey, useNA = "ifany"))

cat("\nResumen identificadores:\n")
print(identifier_summary_rows)

cat("\nResumen sociodemográfico:\n")
print(sociodemographic_summary)

cat("\nResumen calidad:\n")
print(quality_summary)

cat("\nResumen estados attention checks:\n")
print(attention_check_status_summary, n = Inf)

cat("\nResumen fallos attention checks Decision:\n")
print(attention_check_failure_summary_decision, n = Inf)

cat("\nResumen fallos attention checks todos:\n")
print(attention_check_failure_summary_any, n = Inf)

cat("\nResumen personas que fallan attention checks:\n")
print(attention_check_all_fail_summary)

cat("\nResumen datasets limpios:\n")
print(cleaning_summary)

cat("\nArchivos guardados en:\n")
cat("- Dataset base:", file.path(csv_dir, "df_analysis_ready.csv"), "\n")
cat("- Datasets limpios:", clean_output_dir, "\n")
cat("- Logs:", logs_dir, "\n")