
# Objetivo:
# Crear una versión limpia de all_sources_integrated.csv.
#
# Mantiene TODAS las columnas originales en una versión de trazabilidad
# y crea otra versión limpia solo con identificadores + columnas unificadas.
#
# Añade:
# 1. Variables sociodemográficas armonizadas.
# 2. Variables políticas/voto armonizadas.
# 3. Variables de contexto armonizadas.
# 4. 32 determinantes armonizados entre RV Decision, WHY y Diego.
# 5. Diagnósticos técnicos de cobertura, conflictos y mapeo.
#
# Información específica por fuente:
#
# Diego:
# Q1 = grupo de edad
# Q2 = género
# Q3 = país
# Q4 = educación
# Q5 = estado laboral
# Q6 = número de hijos
# Q7 = income
#
# WHY:
# WHY ya viene unido con Prolific desde 01_mergeData.R.
# Se usan columnas Prolific disponibles en all_sources_integrated.csv:
# Age, Sex, Ethnicity simplified, Country of birth,
# Country of residence, Nationality, Language,
# Student status, Employment status.
#
# Input:
# paper1_cluster/data/processed/01_mergeData/all_sources_integrated.csv
#
# Outputs:
# paper1_cluster/data/processed/01_1_harmonize_sociodemographics/all_sources_integrated_clean.csv
#   -> dataset limpio para análisis, sin columnas originales repetidas.
#
# paper1_cluster/data/processed/01_1_harmonize_sociodemographics/all_sources_integrated_clean_traceability.csv
#   -> dataset completo para trazabilidad, con columnas originales + columnas limpias.
#
# paper1_cluster/data/processed/01_1_harmonize_sociodemographics/sociodemographics_clean.csv
# paper1_cluster/data/processed/01_1_harmonize_sociodemographics/determinants_harmonized.csv
# paper1_cluster/data/processed/01_1_harmonize_sociodemographics/sociodemographic_source_columns.csv
# paper1_cluster/data/processed/01_1_harmonize_sociodemographics/determinant_dictionary_32.csv
# paper1_cluster/data/processed/01_1_harmonize_sociodemographics/diagnostics_sociodemographics_coverage.csv
# paper1_cluster/data/processed/01_1_harmonize_sociodemographics/diagnostics_sociodemographics_counts.csv
# paper1_cluster/data/processed/01_1_harmonize_sociodemographics/diagnostics_sociodemographics_conflicts.csv
# paper1_cluster/data/processed/01_1_harmonize_sociodemographics/diagnostics_32det_by_source.csv
# paper1_cluster/data/processed/01_1_harmonize_sociodemographics/diagnostics_32det_ranges.csv
# paper1_cluster/data/processed/01_1_harmonize_sociodemographics/sociodemographic_dictionary.csv

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(stringr)
})


# Configuración
REFERENCE_YEAR_DEFAULT <- 2026
MIN_COUNTRY_N <- 10

processed_root <- "paper1_cluster/data/processed"

in_file_new <- file.path(
  processed_root,
  "01_mergeData",
  "all_sources_integrated.csv"
)

in_file_old <- file.path(
  processed_root,
  "all_sources_integrated.csv"
)

in_file <- if (file.exists(in_file_new)) {
  in_file_new
} else if (file.exists(in_file_old)) {
  in_file_old
} else {
  stop(
    "No encuentro all_sources_integrated.csv ni en:\n",
    in_file_new, "\nni en:\n", in_file_old
  )
}

out_dir <- file.path(
  processed_root,
  "01_1_harmonize_sociodemographics"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# Lectura
all_sources_integrated <- read_csv(
  in_file,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

df <- all_sources_integrated

reference_year_model_vec <- case_when(
  df$dataset_source == "rv" ~ 2026L,
  df$dataset_source == "why" ~ 2022L,
  df$dataset_source == "diego" ~ 2023L,
  TRUE ~ REFERENCE_YEAR_DEFAULT
)


# Funciones auxiliares generales
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
  x <- na_if(x, "Prefer not to say")
  x <- na_if(x, "Prefer not to answer")
  
  x
}

normalise_text <- function(x) {
  x <- clean_text(x)
  x <- str_to_lower(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- str_replace_all(x, "[[:punct:]]+", " ")
  x <- str_squish(x)
  x
}

parse_num_clean <- function(x) {
  x <- clean_text(x)
  
  suppressWarnings(
    readr::parse_number(
      x,
      locale = locale(decimal_mark = ".", grouping_mark = ",")
    )
  )
}

flag_conflict <- function(x) {
  x <- clean_text(x)
  !is.na(x) & str_detect(x, "\\s\\|\\s")
}

parse_num_no_conflict <- function(x) {
  out <- parse_num_clean(x)
  out[flag_conflict(x)] <- NA_real_
  out
}

find_cols <- function(df, pattern, exclude = NULL) {
  out <- names(df)[str_detect(names(df), regex(pattern, ignore_case = TRUE))]
  
  if (!is.null(exclude)) {
    out <- out[!str_detect(out, regex(exclude, ignore_case = TRUE))]
  }
  
  unique(out)
}

collapse_many_unique <- function(df, cols) {
  cols <- unique(cols)
  cols <- cols[cols %in% names(df)]
  
  if (length(cols) == 0) {
    return(rep(NA_character_, nrow(df)))
  }
  
  tmp <- df[, cols, drop = FALSE] %>%
    mutate(across(everything(), clean_text))
  
  apply(tmp, 1, function(row_values) {
    values <- as.character(row_values)
    values <- values[!is.na(values) & values != "" & values != "NA" & values != "NaN"]
    values <- unique(values)
    
    if (length(values) == 0) {
      return(NA_character_)
    }
    
    if (length(values) == 1) {
      return(values[[1]])
    }
    
    paste(values, collapse = " | ")
  })
}

is_valid_model_value <- function(x) {
  if (is.numeric(x) || is.integer(x)) {
    return(!is.na(x))
  }
  
  !is.na(x) &
    x != "" &
    !x %in% c("unknown", "UNKNOWN", "conflict", "other", "OTHER")
}

parse_det <- function(x) {
  suppressWarnings(
    readr::parse_number(
      as.character(x),
      locale = locale(decimal_mark = ".", grouping_mark = ",")
    )
  )
}

safe_numeric_col <- function(df, col) {
  if (is.na(col) || !col %in% names(df)) {
    rep(NA_real_, nrow(df))
  } else {
    parse_det(df[[col]])
  }
}

resolve_unique_prefix <- function(df, prefix) {
  matches <- names(df)[str_starts(names(df), prefix)]
  
  matches <- matches[
    !str_detect(matches, "which_country_do_you_reside|languages|prolific_id")
  ]
  
  if (length(matches) == 0) {
    return(NA_character_)
  }
  
  if (length(matches) > 1) {
    stop(
      "Prefijo ambiguo: ", prefix, "\n",
      "Coincidencias:\n",
      paste(matches, collapse = "\n")
    )
  }
  
  matches
}

# Países
country_name_to_iso2 <- c(
  "spain" = "ES", "espana" = "ES",
  "germany" = "DE", "alemania" = "DE",
  "italy" = "IT", "italia" = "IT",
  "greece" = "GR", "grecia" = "GR",
  "the netherlands" = "NL", "netherlands" = "NL", "nederland" = "NL",
  "belgium" = "BE", "belgica" = "BE",
  "france" = "FR",
  "portugal" = "PT",
  "bulgaria" = "BG",
  "lithuania" = "LT",
  "czechia" = "CZ", "czech republic" = "CZ",
  "denmark" = "DK",
  "sweden" = "SE",
  "finland" = "FI",
  "poland" = "PL",
  "romania" = "RO",
  "hungary" = "HU",
  "ireland" = "IE",
  "austria" = "AT",
  "croatia" = "HR",
  "slovenia" = "SI",
  "slovakia" = "SK",
  "estonia" = "EE",
  "latvia" = "LV",
  "luxembourg" = "LU",
  "malta" = "MT",
  "cyprus" = "CY",
  "united kingdom" = "GB",
  "uk" = "GB",
  "great britain" = "GB",
  "ukraine" = "UA",
  "switzerland" = "CH",
  "serbia" = "RS",
  "albania" = "AL",
  "moldova" = "MD",
  "other eu country" = "OTHER_EU",
  "other eu countries" = "OTHER_EU",
  "other european country" = "OTHER_EU",
  "other european countries" = "OTHER_EU",
  "mexico" = "MX",
  "chile" = "CL",
  "colombia" = "CO",
  "argentina" = "AR",
  "peru" = "PE",
  "venezuela bolivarian republic of" = "VE",
  "brazil" = "BR",
  "bolivia" = "BO",
  "panama" = "PA",
  "cuba" = "CU",
  "united states" = "US",
  "united states of america" = "US",
  "canada" = "CA",
  "india" = "IN",
  "turkey" = "TR",
  "russian federation" = "RU",
  "russia" = "RU",
  "nigeria" = "NG",
  "china" = "CN",
  "indonesia" = "ID",
  "south africa" = "ZA",
  "australia" = "AU",
  "bangladesh" = "BD",
  "ghana" = "GH",
  "iran" = "IR",
  "kazakhstan" = "KZ",
  "lebanon" = "LB",
  "malaysia" = "MY",
  "morocco" = "MA",
  "pakistan" = "PK",
  "armenia" = "AM",
  "cameroon" = "CM",
  "egypt" = "EG",
  "haiti" = "HT",
  "hong kong" = "HK",
  "kyrgyzstan" = "KG",
  "philippines" = "PH",
  "saudi arabia" = "SA",
  "tanzania" = "TZ",
  "tunisia" = "TN"
)

extract_country_code_one <- function(x) {
  x_chr <- clean_text(x)
  x_low <- normalise_text(x_chr)
  
  code_exact <- ifelse(
    str_detect(x_chr, "^[A-Za-z]{2}$"),
    str_to_upper(x_chr),
    NA_character_
  )
  
  code_parentheses <- str_match(x_chr, "\\(([A-Za-z]{2})\\)")[, 2]
  code_prefix <- str_match(x_chr, "^\\s*([A-Za-z]{2})\\s*[-–]")[, 2]
  code_name <- unname(country_name_to_iso2[x_low])
  
  code <- coalesce(code_exact, code_parentheses, code_prefix, code_name)
  str_to_upper(code)
}

extract_country_code_multi <- function(x) {
  map_chr(clean_text(x), function(value) {
    if (is.na(value)) {
      return(NA_character_)
    }
    
    parts <- str_split(value, "\\s\\|\\s")[[1]]
    codes <- extract_country_code_one(parts)
    codes <- unique(codes[!is.na(codes) & codes != ""])
    
    if (length(codes) == 0) {
      return(NA_character_)
    }
    
    if (length(codes) == 1) {
      return(codes[[1]])
    }
    
    paste(codes, collapse = " | ")
  })
}


# Localizar columnas candidatas sociodemográficas
id_cols <- c(
  "integrated_row_id",
  "global_participant_key",
  "participant_key",
  "dataset_source",
  "n_source_rows",
  "n_distinct_surveys",
  "source_survey",
  "source_file",
  "prolific_id",
  "identification_code"
)

# Diego:
# Q1 edad grupo; Q2 género; Q3 país; Q4 educación; Q5 estado laboral; Q6 hijos; Q7 income.

year_birth_cols <- unique(c(
  "year_birth_raw",
  find_cols(df, "please_enter_your_year_of_birth|year_of_birth|birth_year")
))

age_cols <- unique(c(
  "age_raw",
  "prolific_age",
  "diego__q1",
  find_cols(
    df,
    "^diego__q1$|^prolific_age$|^age$|(^|__)age($|_)|age_group|age_bracket",
    exclude = "stage|message|image|language|languages"
  )
))

gender_cols <- unique(c(
  "gender_raw",
  "prolific_sex",
  "prolific_gender",
  "diego__q2",
  find_cols(
    df,
    "what_is_your_gender|gender|sex|^diego__q2$|prolific.*sex|prolific.*gender"
  )
))

country_raw_cols <- unique(c(
  "country_raw",
  "country_raw_survey",
  "prolific_country_raw",
  "prolific_country_of_residence",
  "prolific_country_residence",
  "diego__q3",
  find_cols(
    df,
    "in_which_country_do_you_currently_live|which_country_do_you_reside|country_of_residence|country_residence|^diego__q3$|prolific.*country.*residence",
    exclude = "code|source|is_europe"
  )
))

country_code_cols <- unique(c(
  "country_code",
  "country_code_survey",
  "prolific_country_code",
  "prolific_country_residence_code"
))

country_birth_cols <- unique(c(
  "prolific_country_of_birth",
  "prolific_country_birth",
  find_cols(
    df,
    "country_of_birth|country_birth|birth_country|prolific.*country.*birth",
    exclude = "code|source"
  )
))

nationality_cols <- unique(c(
  "prolific_nationality",
  find_cols(
    df,
    "nationality|prolific.*nationality",
    exclude = "code|source"
  )
))

ethnicity_cols <- unique(c(
  "prolific_ethnicity_simplified",
  "prolific_ethnicity",
  find_cols(
    df,
    "ethnicity_simplified|ethnicity|prolific.*ethnicity",
    exclude = "code|source"
  )
))

language_cols <- unique(c(
  "language_raw",
  "prolific_language",
  find_cols(
    df,
    "^why__languages$|^languages$|language|prolific.*language"
  )
))

student_status_cols <- unique(c(
  "prolific_student_status",
  find_cols(
    df,
    "student_status|student status|prolific.*student",
    exclude = "source|date|time"
  )
))

city_size_cols <- find_cols(
  df,
  "approximate_population_size|population_size_of_the_city|population_size|city_size|size_of_the_city"
)

climate_zone_cols <- find_cols(
  df,
  "climate_zone"
)

household_type_cols <- find_cols(
  df,
  "type_of_household|household_type|household_do_you_live"
)

tenure_cols <- find_cols(
  df,
  "tenure_status|current_tenure|housing_tenure"
)

education_cols <- unique(c(
  "diego__q4",
  find_cols(
    df,
    "highest_level_of_education|education|prolific.*education",
    exclude = "energy|efficiency|goal|det_|support|oppose|concern|barrier"
  )
))

employment_cols <- unique(c(
  "diego__q5",
  "prolific_employment_status",
  find_cols(
    df,
    "employment_status|current_employment|contractual_status|labour|labor|prolific.*employment",
    exclude = "source|sources|income|capital|property|pension|joy|risk|concern|barrier|energy|support|oppose|det_"
  )
))

num_children_cols <- unique(c(
  "diego__q6",
  find_cols(
    df,
    "number_of_children|num_children|children|hijos|^diego__q6$",
    exclude = "energy|concern|barrier|support|oppose|det_"
  )
))

health_cols <- find_cols(
  df,
  "health_condition|functional_limitation|requires_assistance"
)

income_cols <- unique(c(
  "diego__q7",
  find_cols(
    df,
    "household_income|monthly_income|annual_income|net_income|gross_income|monthly_net|net_monthly|^diego__q7$",
    exclude = "source|sources|type|capital|property|pension|employment|self_employment|joy|risk|concern|barrier|poverty|energy|support|oppose|feel|feeling|doing|thinking|actions|saved|saving|savings|salary_is_saved|typical_month|det_"
  )
))

travel_distance_cols <- find_cols(
  df,
  "total_distance_you_travel|distance_you_travel"
)

travel_time_cols <- find_cols(
  df,
  "total_time_do_you_spend_travelling|time_do_you_spend_travelling"
)

work_from_home_cols <- find_cols(
  df,
  "work_or_study_from_home|working_from_home"
)

travel_role_cols <- find_cols(
  df,
  "usual_travel_role|travel_role"
)

vote_cols <- find_cols(
  df,
  "general_approach_to_voting|approach_to_voting|voting_in_elections|abstain|abstention|turnout"
)

political_lr_cols <- find_cols(
  df,
  "most_left_and_100_means_most_right|place_yourself_politically|left.*right|political"
)

self_classification_cols <- find_cols(
  df,
  "which_statement_best_describes_you_when_making_an_investment_decision"
)

energy_efficiency_goal_cols <- find_cols(
  df,
  "energy_efficiency_goal"
)

climate_awareness_cols <- find_cols(
  df,
  "climate_change_does_not_exist|awareness_of_climate_change"
)

energy_transition_awareness_cols <- find_cols(
  df,
  "awareness_about_the_energy_transition|energy_transition"
)

renovation_role_cols <- find_cols(
  df,
  "role_or_situation_regarding_household_renovation_decisions|renovation_decisions"
)

household_decision_cols <- find_cols(
  df,
  "in_your_household_how_are_decisions_usually_made|decisions_usually_made"
)

sociodemographic_source_columns <- tibble(
  harmonised_variable = c(
    "year_birth", "age", "gender", "country_raw", "country_code",
    "country_birth", "nationality", "ethnicity",
    "language", "student_status", "city_size", "climate_zone",
    "household_type", "tenure", "education", "employment",
    "num_children", "health_condition", "income",
    "travel_distance", "travel_time", "work_from_home",
    "travel_role", "vote_status", "political_left_right",
    "self_classification", "energy_efficiency_goal",
    "climate_awareness", "energy_transition_awareness",
    "renovation_role", "household_decision"
  ),
  source_columns = c(
    paste(year_birth_cols, collapse = " | "),
    paste(age_cols, collapse = " | "),
    paste(gender_cols, collapse = " | "),
    paste(country_raw_cols, collapse = " | "),
    paste(country_code_cols, collapse = " | "),
    paste(country_birth_cols, collapse = " | "),
    paste(nationality_cols, collapse = " | "),
    paste(ethnicity_cols, collapse = " | "),
    paste(language_cols, collapse = " | "),
    paste(student_status_cols, collapse = " | "),
    paste(city_size_cols, collapse = " | "),
    paste(climate_zone_cols, collapse = " | "),
    paste(household_type_cols, collapse = " | "),
    paste(tenure_cols, collapse = " | "),
    paste(education_cols, collapse = " | "),
    paste(employment_cols, collapse = " | "),
    paste(num_children_cols, collapse = " | "),
    paste(health_cols, collapse = " | "),
    paste(income_cols, collapse = " | "),
    paste(travel_distance_cols, collapse = " | "),
    paste(travel_time_cols, collapse = " | "),
    paste(work_from_home_cols, collapse = " | "),
    paste(travel_role_cols, collapse = " | "),
    paste(vote_cols, collapse = " | "),
    paste(political_lr_cols, collapse = " | "),
    paste(self_classification_cols, collapse = " | "),
    paste(energy_efficiency_goal_cols, collapse = " | "),
    paste(climate_awareness_cols, collapse = " | "),
    paste(energy_transition_awareness_cols, collapse = " | "),
    paste(renovation_role_cols, collapse = " | "),
    paste(household_decision_cols, collapse = " | ")
  )
) %>%
  mutate(
    n_source_columns = if_else(
      source_columns == "",
      0L,
      str_count(source_columns, "\\|") + 1L
    )
  )


# Crear variables raw unificadas
year_birth_raw_clean <- collapse_many_unique(df, year_birth_cols)
age_raw_clean <- collapse_many_unique(df, age_cols)
gender_raw_clean <- collapse_many_unique(df, gender_cols)
country_raw_clean <- collapse_many_unique(df, country_raw_cols)
country_code_raw_clean <- collapse_many_unique(df, country_code_cols)
country_birth_raw_clean <- collapse_many_unique(df, country_birth_cols)
nationality_raw_clean <- collapse_many_unique(df, nationality_cols)
ethnicity_raw_clean <- collapse_many_unique(df, ethnicity_cols)
language_raw_clean <- collapse_many_unique(df, language_cols)
student_status_raw_clean <- collapse_many_unique(df, student_status_cols)

city_size_raw_clean <- collapse_many_unique(df, city_size_cols)
climate_zone_raw_clean <- collapse_many_unique(df, climate_zone_cols)
household_type_raw_clean <- collapse_many_unique(df, household_type_cols)
tenure_raw_clean <- collapse_many_unique(df, tenure_cols)
education_raw_clean <- collapse_many_unique(df, education_cols)
employment_raw_clean <- collapse_many_unique(df, employment_cols)
num_children_raw_clean <- collapse_many_unique(df, num_children_cols)
health_raw_clean <- collapse_many_unique(df, health_cols)
income_raw_clean <- collapse_many_unique(df, income_cols)

travel_distance_raw_clean <- collapse_many_unique(df, travel_distance_cols)
travel_time_raw_clean <- collapse_many_unique(df, travel_time_cols)
work_from_home_raw_clean <- collapse_many_unique(df, work_from_home_cols)
travel_role_raw_clean <- collapse_many_unique(df, travel_role_cols)

vote_raw_clean <- collapse_many_unique(df, vote_cols)
political_left_right_raw_clean <- collapse_many_unique(df, political_lr_cols)
self_classification_raw_clean <- collapse_many_unique(df, self_classification_cols)

energy_efficiency_goal_raw_clean <- collapse_many_unique(df, energy_efficiency_goal_cols)
climate_awareness_raw_clean <- collapse_many_unique(df, climate_awareness_cols)
energy_transition_awareness_raw_clean <- collapse_many_unique(df, energy_transition_awareness_cols)

renovation_role_raw_clean <- collapse_many_unique(df, renovation_role_cols)
household_decision_raw_clean <- collapse_many_unique(df, household_decision_cols)


# Funciones de recodificación
recode_age_group_from_raw <- function(x, numeric_age = NULL) {
  x_low <- normalise_text(x)
  
  case_when(
    is.na(x_low) & is.null(numeric_age) ~ "unknown",
    is.na(x_low) & !is.null(numeric_age) & is.na(numeric_age) ~ "unknown",
    
    str_detect(x_low, "18\\s*25|18 25|18-25") ~ "18_24",
    str_detect(x_low, "26\\s*39|26 39|26-39") ~ "25_34",
    str_detect(x_low, "40\\s*59|40 59|40-59") ~ "45_54",
    str_detect(x_low, "60 or older|60\\+|60 and older|older than 60") ~ "65_plus",
    
    !is.null(numeric_age) & !is.na(numeric_age) & numeric_age < 25 ~ "18_24",
    !is.null(numeric_age) & !is.na(numeric_age) & numeric_age < 35 ~ "25_34",
    !is.null(numeric_age) & !is.na(numeric_age) & numeric_age < 45 ~ "35_44",
    !is.null(numeric_age) & !is.na(numeric_age) & numeric_age < 55 ~ "45_54",
    !is.null(numeric_age) & !is.na(numeric_age) & numeric_age < 65 ~ "55_64",
    !is.null(numeric_age) & !is.na(numeric_age) & numeric_age >= 65 ~ "65_plus",
    
    TRUE ~ "unknown"
  )
}

age_midpoint_from_raw <- function(x) {
  x_low <- normalise_text(x)
  
  case_when(
    str_detect(x_low, "18\\s*25|18 25|18-25") ~ 21.5,
    str_detect(x_low, "26\\s*39|26 39|26-39") ~ 32.5,
    str_detect(x_low, "40\\s*59|40 59|40-59") ~ 49.5,
    str_detect(x_low, "60 or older|60\\+|60 and older|older than 60") ~ 65,
    TRUE ~ NA_real_
  )
}

recode_gender <- function(x) {
  x_raw <- clean_text(x)
  x_low <- normalise_text(x_raw)
  
  case_when(
    is.na(x_low) ~ "unknown",
    str_detect(x_raw, "\\s\\|\\s") ~ "conflict",
    str_detect(x_low, "\\bfemale\\b|\\bwoman\\b|\\bwomen\\b|\\bmujer\\b|\\bfemen") ~ "female",
    str_detect(x_low, "\\bmale\\b|\\bman\\b|\\bmen\\b|\\bhombre\\b|\\bmascul") ~ "male",
    str_detect(x_low, "non binary|nonbinary|non binar|other|otro|otra|diverse|no binar") ~ "other",
    str_detect(x_low, "prefer not|no answer|dont know|do not know|ns nc") ~ "unknown",
    TRUE ~ "other"
  )
}

recode_language <- function(x) {
  x_low <- normalise_text(x)
  
  case_when(
    is.na(x_low) ~ "unknown",
    str_detect(x_low, "\\benglish\\b|\\ben\\b|ingles") ~ "EN",
    str_detect(x_low, "\\bspanish\\b|\\bes\\b|espanol|castellano") ~ "ES",
    str_detect(x_low, "\\bbasque\\b|euskera|euskara|\\beu\\b") ~ "EU",
    str_detect(x_low, "\\bdutch\\b|nederlands|\\bnl\\b") ~ "NL",
    str_detect(x_low, "\\bfrench\\b|francais|frances|\\bfr\\b") ~ "FR",
    str_detect(x_low, "\\bgerman\\b|deutsch|aleman|\\bde\\b") ~ "DE",
    str_detect(x_low, "\\bitalian\\b|italiano|\\bit\\b") ~ "IT",
    str_detect(x_low, "\\bpolish\\b|polski|\\bpl\\b") ~ "PL",
    str_detect(x_low, "\\bgreek\\b|ellenika|\\bgr\\b") ~ "GR",
    TRUE ~ "OTHER"
  )
}

recode_education <- function(x) {
  x_raw <- clean_text(x)
  x_low <- normalise_text(x_raw)
  
  case_when(
    is.na(x_low) ~ "unknown",
    str_detect(x_raw, "\\s\\|\\s") ~ "conflict",
    str_detect(x_low, "phd|doctor|doctoral") ~ "high",
    str_detect(x_low, "master|msc|postgraduate|bachelor|undergraduate|degree|university|universit|tertiary|higher education|grado|licenciatura|diploma universitario") ~ "high",
    str_detect(x_low, "vocational|professional training|upper secondary|secondary|high school|bachiller|formacion profesional|\\bfp\\b|college") ~ "medium",
    str_detect(x_low, "primary|basic|lower secondary|no formal|less than|sin estudios|primaria") ~ "low",
    str_detect(x_low, "prefer not|no answer|dont know|do not know") ~ "unknown",
    TRUE ~ "other"
  )
}

recode_employment <- function(x) {
  x_raw <- clean_text(x)
  x_low <- normalise_text(x_raw)
  
  case_when(
    is.na(x_low) ~ "unknown",
    str_detect(x_raw, "\\s\\|\\s") ~ "conflict",
    
    # Primero desempleo, porque "unemployed" contiene "employed"
    str_detect(x_low, "unemployed|desemple|paro|job seeking|not working unemployed") ~ "unemployed",
    
    # Nueva incorporación laboral próxima
    str_detect(x_low, "due to start a new job|start a new job|new job within the next month") ~ "employed",
    
    # Empleo
    str_detect(x_low, "full time|full-time|part time|part-time|paid employee|\\bemployee\\b|\\bemployed\\b|\\bworking\\b|trabaj|contrato") ~ "employed",
    
    # Autónomos
    str_detect(x_low, "self employed|self employment|autonom|freelance") ~ "self_employed",
    
    # Estudiantes
    str_detect(x_low, "student|estudiante") ~ "student",
    
    # Inactividad no diferenciada
    str_detect(x_low, "not in paid work|homemaker|disabled|unable to work|care|inactive") ~ "inactive_other",
    
    # Jubilación explícita
    str_detect(x_low, "retired|jubil|pension") ~ "retired",
    
    str_detect(x_low, "prefer not|no answer|dont know|do not know") ~ "unknown",
    TRUE ~ "other"
  )
}

recode_student_status <- function(x) {
  x_raw <- clean_text(x)
  x_low <- normalise_text(x_raw)
  
  case_when(
    is.na(x_low) ~ "unknown",
    str_detect(x_raw, "\\s\\|\\s") ~ "conflict",
    str_detect(x_low, "\\byes\\b|student|si") ~ "student",
    str_detect(x_low, "\\bno\\b|not student") ~ "not_student",
    TRUE ~ "other"
  )
}

recode_city_size <- function(x) {
  x_raw <- clean_text(x)
  x_low <- normalise_text(x_raw)
  n <- parse_num_no_conflict(x_raw)
  
  case_when(
    is.na(x_low) ~ "unknown",
    str_detect(x_raw, "\\s\\|\\s") ~ "conflict",
    str_detect(x_low, "rural|village|small town") ~ "rural_small",
    str_detect(x_low, "less than|under|below|<") & !is.na(n) & n <= 10000 ~ "rural_small",
    str_detect(x_low, "less than|under|below|<") & !is.na(n) & n <= 50000 ~ "small_city",
    !is.na(n) & n < 10000 ~ "rural_small",
    !is.na(n) & n < 50000 ~ "small_city",
    !is.na(n) & n < 250000 ~ "medium_city",
    !is.na(n) & n < 1000000 ~ "large_city",
    !is.na(n) & n >= 1000000 ~ "metropolitan",
    str_detect(x_low, "more than|over|above|>") ~ "large_city",
    TRUE ~ "other"
  )
}

recode_tenure <- function(x) {
  x_raw <- clean_text(x)
  x_low <- normalise_text(x_raw)
  
  case_when(
    is.na(x_low) ~ "unknown",
    str_detect(x_raw, "\\s\\|\\s") ~ "conflict",
    str_detect(x_low, "rent|tenant|alquil") ~ "rent",
    str_detect(x_low, "mortgage|hipoteca") ~ "owner_with_mortgage",
    str_detect(x_low, "without mortgage|sin hipoteca") ~ "owner_without_mortgage",
    str_detect(x_low, "own|owner|propiedad") ~ "owner",
    str_detect(x_low, "family|relative|parents") ~ "family_home",
    TRUE ~ "other"
  )
}

recode_yes_no <- function(x) {
  x_raw <- clean_text(x)
  x_low <- normalise_text(x_raw)
  
  case_when(
    is.na(x_low) ~ "unknown",
    str_detect(x_raw, "\\s\\|\\s") ~ "conflict",
    str_detect(x_low, "\\byes\\b|\\bsi\\b|true") ~ "yes",
    str_detect(x_low, "\\bno\\b|false|none") ~ "no",
    TRUE ~ "other"
  )
}

recode_vote_status <- function(x) {
  x_raw <- clean_text(x)
  x_low <- normalise_text(x_raw)
  
  case_when(
    is.na(x_low) ~ "unknown",
    str_detect(x_raw, "\\s\\|\\s") ~ "conflict",
    str_detect(x_low, "abstain|abstention|do not vote|dont vote|did not vote|not vote|never vote|usually do not vote|no voto|abstengo") ~ "abstainer",
    str_detect(x_low, "always vote|usually vote|i vote|voted|voter|participate|turn out|suelo votar|siempre voto") ~ "voter",
    str_detect(x_low, "sometimes|depends|not sure|uncertain|undecided|prefer not|no answer") ~ "uncertain",
    TRUE ~ "uncertain"
  )
}

recode_political_block <- function(x) {
  n <- parse_num_no_conflict(x)
  
  case_when(
    is.na(n) ~ "unknown",
    n < 0 | n > 100 ~ "invalid",
    n < 20 ~ "extreme_left",
    n < 40 ~ "left",
    n <= 60 ~ "centre",
    n <= 80 ~ "right",
    n <= 100 ~ "extreme_right",
    TRUE ~ "invalid"
  )
}

recode_income <- function(x) {
  x_raw <- clean_text(x)
  x_low <- normalise_text(x_raw)
  n <- parse_num_no_conflict(x_raw)
  
  case_when(
    is.na(x_low) ~ "unknown",
    str_detect(x_raw, "\\s\\|\\s") ~ "conflict",
    str_detect(x_low, "prefer not|no answer|dont know|do not know") ~ "unknown",
    
    str_detect(x_low, "less than 20 000|less than 20000|under 20 000|under 20000") ~ "low",
    str_detect(x_low, "20 000.*49 999|20000.*49999") ~ "medium",
    str_detect(x_low, "50 000.*100 000|50000.*100000|more than 100 000|more than 100000|over 100 000|over 100000") ~ "high",
    
    !is.na(n) & n >= 10000 & n < 20000 ~ "low",
    !is.na(n) & n >= 20000 & n < 50000 ~ "medium",
    !is.na(n) & n >= 50000 ~ "high",
    
    !is.na(n) & n < 1000 ~ "low",
    !is.na(n) & n < 2500 ~ "medium",
    !is.na(n) & n >= 2500 ~ "high",
    
    str_detect(x_low, "low|bajo") ~ "low",
    str_detect(x_low, "middle|medium|medio") ~ "medium",
    str_detect(x_low, "high|alto") ~ "high",
    TRUE ~ "other"
  )
}

parse_num_children <- function(x) {
  x_low <- normalise_text(x)
  n <- parse_num_no_conflict(x)
  
  case_when(
    is.na(x_low) ~ NA_real_,
    str_detect(x_low, "none|no children|sin hijos|ninguno") ~ 0,
    !is.na(n) ~ n,
    TRUE ~ NA_real_
  )
}

recode_self_classification <- function(x) {
  x_low <- normalise_text(x)
  
  case_when(
    is.na(x_low) ~ "unknown",
    str_detect(x_low, "environment|climate|planet") ~ "activist",
    str_detect(x_low, "safety|safe|risk") ~ "fearful",
    str_detect(x_low, "status|recognition|show others") ~ "influencer",
    str_detect(x_low, "comfort|cozy|cosy|well being|wellbeing") ~ "careful",
    str_detect(x_low, "not interested|not very interested") ~ "uninterested",
    str_detect(x_low, "new|innovation|technology|early adopter") ~ "pioneer",
    str_detect(x_low, "ethical|meaning|values") ~ "sentient",
    str_detect(x_low, "cost|money|save|saving|economic") ~ "homo_economicus",
    str_detect(x_low, "none") ~ "none",
    TRUE ~ "other"
  )
}


# Construir variables sociodemográficas model-ready
year_birth_num <- parse_num_no_conflict(year_birth_raw_clean)
age_num_raw <- parse_num_no_conflict(age_raw_clean)
age_midpoint_vec <- age_midpoint_from_raw(age_raw_clean)

year_birth_model_vec <- case_when(
  !is.na(year_birth_num) &
    year_birth_num >= 1900 &
    year_birth_num <= reference_year_model_vec ~ as.integer(year_birth_num),
  
  !is.na(age_num_raw) &
    age_num_raw >= 1900 &
    age_num_raw <= reference_year_model_vec ~ as.integer(age_num_raw),
  
  TRUE ~ NA_integer_
)

age_direct_vec <- case_when(
  !is.na(age_num_raw) &
    age_num_raw >= 15 &
    age_num_raw <= 110 &
    is.na(age_midpoint_vec) ~ as.numeric(age_num_raw),
  
  !is.na(year_birth_num) &
    year_birth_num >= 15 &
    year_birth_num <= 110 ~ as.numeric(year_birth_num),
  
  TRUE ~ NA_real_
)

age_model_vec <- coalesce(
  age_direct_vec,
  age_midpoint_vec,
  ifelse(
    !is.na(year_birth_model_vec),
    reference_year_model_vec - year_birth_model_vec,
    NA_real_
  )
)

age_model_is_approximate_vec <- !is.na(age_midpoint_vec) & is.na(age_direct_vec)

country_code_from_raw <- extract_country_code_multi(country_raw_clean)
country_birth_model_vec <- extract_country_code_multi(country_birth_raw_clean)

country_model_vec <- coalesce(
  extract_country_code_multi(country_code_raw_clean),
  country_code_from_raw
)

political_left_right_vec <- parse_num_no_conflict(political_left_right_raw_clean)

energy_efficiency_goal_vec <- parse_num_no_conflict(energy_efficiency_goal_raw_clean)
climate_awareness_vec <- parse_num_no_conflict(climate_awareness_raw_clean)
energy_transition_awareness_vec <- parse_num_no_conflict(energy_transition_awareness_raw_clean)

travel_distance_vec <- parse_num_no_conflict(travel_distance_raw_clean)
travel_time_vec <- parse_num_no_conflict(travel_time_raw_clean)
num_children_vec <- parse_num_children(num_children_raw_clean)

# Construir all_sources_integrated_clean
all_sources_integrated_clean <- all_sources_integrated %>%
  mutate(
    reference_year_model = reference_year_model_vec,
    
    year_birth_raw_clean = year_birth_raw_clean,
    age_raw_clean = age_raw_clean,
    gender_raw_clean = gender_raw_clean,
    country_raw_clean = country_raw_clean,
    country_code_raw_clean = country_code_raw_clean,
    country_birth_raw_clean = country_birth_raw_clean,
    nationality_raw_clean = nationality_raw_clean,
    ethnicity_raw_clean = ethnicity_raw_clean,
    language_raw_clean = language_raw_clean,
    student_status_raw_clean = student_status_raw_clean,
    
    city_size_raw_clean = city_size_raw_clean,
    climate_zone_raw_clean = climate_zone_raw_clean,
    household_type_raw_clean = household_type_raw_clean,
    tenure_raw_clean = tenure_raw_clean,
    education_raw_clean = education_raw_clean,
    employment_raw_clean = employment_raw_clean,
    num_children_raw_clean = num_children_raw_clean,
    health_raw_clean = health_raw_clean,
    income_raw_clean = income_raw_clean,
    
    travel_distance_raw_clean = travel_distance_raw_clean,
    travel_time_raw_clean = travel_time_raw_clean,
    work_from_home_raw_clean = work_from_home_raw_clean,
    travel_role_raw_clean = travel_role_raw_clean,
    
    vote_raw_clean = vote_raw_clean,
    political_left_right_raw_clean = political_left_right_raw_clean,
    self_classification_raw_clean = self_classification_raw_clean,
    
    energy_efficiency_goal_raw_clean = energy_efficiency_goal_raw_clean,
    climate_awareness_raw_clean = climate_awareness_raw_clean,
    energy_transition_awareness_raw_clean = energy_transition_awareness_raw_clean,
    
    renovation_role_raw_clean = renovation_role_raw_clean,
    household_decision_raw_clean = household_decision_raw_clean,
    
    year_birth_model = year_birth_model_vec,
    age_model = age_model_vec,
    age_model_is_approximate = age_model_is_approximate_vec,
    age_group_model = recode_age_group_from_raw(age_raw_clean, age_model_vec),
    
    gender_model = recode_gender(gender_raw_clean),
    education_model = recode_education(education_raw_clean),
    employment_model = recode_employment(employment_raw_clean),
    student_status_model = recode_student_status(student_status_raw_clean),
    city_size_model = recode_city_size(city_size_raw_clean),
    tenure_model = recode_tenure(tenure_raw_clean),
    health_condition_model = recode_yes_no(health_raw_clean),
    work_from_home_model = recode_yes_no(work_from_home_raw_clean),
    income_model = recode_income(income_raw_clean),
    num_children_model = num_children_vec,
    
    country_model = country_model_vec,
    country_birth_model = country_birth_model_vec,
    nationality_model = case_when(
      is.na(nationality_raw_clean) ~ "unknown",
      flag_conflict(nationality_raw_clean) ~ "conflict",
      TRUE ~ normalise_text(nationality_raw_clean)
    ),
    ethnicity_model = case_when(
      is.na(ethnicity_raw_clean) ~ "unknown",
      flag_conflict(ethnicity_raw_clean) ~ "conflict",
      TRUE ~ normalise_text(ethnicity_raw_clean)
    ),
    language_model = recode_language(language_raw_clean),
    
    climate_zone_model = case_when(
      is.na(climate_zone_raw_clean) ~ "unknown",
      flag_conflict(climate_zone_raw_clean) ~ "conflict",
      TRUE ~ normalise_text(climate_zone_raw_clean)
    ),
    
    household_type_model = case_when(
      is.na(household_type_raw_clean) ~ "unknown",
      flag_conflict(household_type_raw_clean) ~ "conflict",
      TRUE ~ normalise_text(household_type_raw_clean)
    ),
    
    travel_role_model = case_when(
      is.na(travel_role_raw_clean) ~ "unknown",
      flag_conflict(travel_role_raw_clean) ~ "conflict",
      TRUE ~ normalise_text(travel_role_raw_clean)
    ),
    
    renovation_role_model = case_when(
      is.na(renovation_role_raw_clean) ~ "unknown",
      flag_conflict(renovation_role_raw_clean) ~ "conflict",
      TRUE ~ normalise_text(renovation_role_raw_clean)
    ),
    
    household_decision_model = case_when(
      is.na(household_decision_raw_clean) ~ "unknown",
      flag_conflict(household_decision_raw_clean) ~ "conflict",
      TRUE ~ normalise_text(household_decision_raw_clean)
    ),
    
    vote_status_declared = recode_vote_status(vote_raw_clean),
    voted_observed = case_when(
      vote_status_declared == "voter" ~ 1L,
      vote_status_declared == "abstainer" ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    political_left_right_model = political_left_right_vec,
    political_block_model = recode_political_block(political_left_right_raw_clean),
    
    self_classification_model = recode_self_classification(self_classification_raw_clean),
    
    energy_efficiency_goal_0_100 = energy_efficiency_goal_vec,
    climate_awareness_0_100 = climate_awareness_vec,
    energy_transition_awareness_0_100 = energy_transition_awareness_vec,
    
    travel_distance_model = travel_distance_vec,
    travel_time_model = travel_time_vec,
    
    year_birth_conflict = flag_conflict(year_birth_raw_clean),
    age_conflict = flag_conflict(age_raw_clean),
    gender_conflict = flag_conflict(gender_raw_clean),
    education_conflict = flag_conflict(education_raw_clean),
    employment_conflict = flag_conflict(employment_raw_clean),
    country_conflict = flag_conflict(country_raw_clean) | flag_conflict(country_model),
    country_birth_conflict = flag_conflict(country_birth_raw_clean) | flag_conflict(country_birth_model),
    nationality_conflict = flag_conflict(nationality_raw_clean),
    ethnicity_conflict = flag_conflict(ethnicity_raw_clean),
    student_status_conflict = flag_conflict(student_status_raw_clean),
    income_conflict = flag_conflict(income_raw_clean),
    vote_conflict = flag_conflict(vote_raw_clean)
  )


# Agrupar países pequeños
country_counts <- all_sources_integrated_clean %>%
  count(country_model, name = "n_country_model")

all_sources_integrated_clean <- all_sources_integrated_clean %>%
  left_join(country_counts, by = "country_model") %>%
  mutate(
    country_model_grouped = case_when(
      is.na(country_model) | country_model == "" ~ "UNKNOWN",
      str_detect(country_model, "\\s\\|\\s") ~ "CONFLICT",
      n_country_model >= MIN_COUNTRY_N ~ country_model,
      TRUE ~ "OTHER_COUNTRIES"
    )
  ) %>%
  select(-n_country_model)


# Diccionario de 32 determinantes
determinant_dictionary <- tribble(
  ~det_id, ~det_name, ~rv_prefix, ~why_prefix, ~diego_col,
  1,  "profits",                 "rv_decision__profits_",                 "why__a_",   "diego_det_01_0_100",
  2,  "credit_score",            "rv_decision__credit_score_",            "why__b_",   "diego_det_02_0_100",
  3,  "risk_profile",            "rv_decision__risk_profile_",            "why__c_",   "diego_det_03_0_100",
  4,  "added_value",             "rv_decision__added_value_",             "why__d_",   "diego_det_04_0_100",
  5,  "frugality",               "rv_decision__frugality_",               "why__e_",   "diego_det_05_0_100",
  6,  "climate_protection",      "rv_decision__climate_protection_",      "why__f_",   "diego_det_06_0_100",
  7,  "legal",                   "rv_decision__legal_",                   "why__g_",   "diego_det_07_0_100",
  8,  "trust",                   "rv_decision__trust_",                   "why__h_",   "diego_det_08_0_100",
  9,  "safety",                  "rv_decision__safety_",                  "why__i_i",  "diego_det_09_0_100",
  10, "cost_efficiency",         "rv_decision__cost_efficiency_",         "why__j_",   "diego_det_10_0_100",
  11, "knowledge",               "rv_decision__knowledge_",               "why__k_",   "diego_det_11_0_100",
  12, "own_competence",          "rv_decision__own_competence_",          "why__l_",   "diego_det_12_0_100",
  13, "technical_fit",           "rv_decision__technical_fit_",           "why__m_",   "diego_det_13_0_100",
  14, "environmental_concerns",  "rv_decision__environmental_concerns_",  "why__n_",   "diego_det_14_0_100",
  15, "self_satisfaction",       "rv_decision__self_satisfaction_",       "why__o_",   "diego_det_15_0_100",
  16, "commitment",              "rv_decision__commitment_",              "why__p_",   "diego_det_16_0_100",
  17, "adherence",               "rv_decision__adherence_",               "why__q_",   "diego_det_17_0_100",
  18, "autonomy",                "rv_decision__autonomy_",                "why__r_",   "diego_det_18_0_100",
  19, "wellbeing",               "rv_decision__wellbeing_",               "why__s_",   "diego_det_19_0_100",
  20, "coziness",                "rv_decision__coziness_",                "why__t_",   "diego_det_20_0_100",
  21, "rights_and_duties",       "rv_decision__rights_and_duties_",       "why__u_",   "diego_det_21_0_100",
  22, "peer_pressure",           "rv_decision__peer_pressure_",           "why__v_",   "diego_det_22_0_100",
  23, "support",                 "rv_decision__support_",                 "why__w_",   "diego_det_23_0_100",
  24, "socialising",             "rv_decision__socialising_",             "why__x_",   "diego_det_24_0_100",
  25, "agreement",               "rv_decision__agreement_",               "why__y_",   "diego_det_25_0_100",
  26, "novelty",                 "rv_decision__novelty_",                 "why__z_",   "diego_det_26_0_100",
  27, "fun",                     "rv_decision__fun_",                     "why__aa_",  "diego_det_27_0_100",
  28, "recognition",             "rv_decision__recognition_",             "why__ab_",  "diego_det_28_0_100",
  29, "trends",                  "rv_decision__trends_",                  "why__ac_",  "diego_det_29_0_100",
  30, "authority",               "rv_decision__authority_",               "why__ad_",  "diego_det_30_0_100",
  31, "approval",                "rv_decision__approval_",                "why__ae_",  "diego_det_31_0_100",
  32, "own_significance",        "rv_decision__own_significance_",        "why__af_",  "diego_det_32_0_100"
) %>%
  mutate(
    det_col = paste0("det_", sprintf("%02d", det_id), "_", det_name)
  )

determinant_dictionary <- determinant_dictionary %>%
  mutate(
    rv_col = map_chr(rv_prefix, ~ resolve_unique_prefix(df, .x)),
    why_col = map_chr(why_prefix, ~ resolve_unique_prefix(df, .x)),
    diego_col_resolved = if_else(
      diego_col %in% names(df),
      diego_col,
      NA_character_
    )
  )

missing_mapping <- determinant_dictionary %>%
  filter(is.na(rv_col) | is.na(why_col) | is.na(diego_col_resolved))

if (nrow(missing_mapping) > 0) {
  print(missing_mapping)
  stop("Faltan columnas para uno o más determinantes. Revisa determinant_dictionary.")
}


# Crear 32 determinantes armonizados
for (i in seq_len(nrow(determinant_dictionary))) {
  det_col <- determinant_dictionary$det_col[i]
  
  rv_values <- safe_numeric_col(
    df,
    determinant_dictionary$rv_col[i]
  )
  
  why_values <- safe_numeric_col(
    df,
    determinant_dictionary$why_col[i]
  )
  
  diego_values <- safe_numeric_col(
    df,
    determinant_dictionary$diego_col_resolved[i]
  )
  
  all_sources_integrated_clean[[det_col]] <- case_when(
    all_sources_integrated_clean$dataset_source == "rv" ~ rv_values,
    all_sources_integrated_clean$dataset_source == "why" ~ why_values,
    all_sources_integrated_clean$dataset_source == "diego" ~ diego_values,
    TRUE ~ NA_real_
  )
}

det_cols <- determinant_dictionary$det_col

all_sources_integrated_clean <- all_sources_integrated_clean %>%
  mutate(
    n_det_non_missing = rowSums(!is.na(across(all_of(det_cols)))),
    prop_det_non_missing = n_det_non_missing / length(det_cols)
  )


# Relocalizar columnas limpias al principio
sociodemographic_clean_cols <- c(
  "reference_year_model",
  
  "year_birth_raw_clean",
  "age_raw_clean",
  "gender_raw_clean",
  "country_raw_clean",
  "country_code_raw_clean",
  "country_birth_raw_clean",
  "nationality_raw_clean",
  "ethnicity_raw_clean",
  "language_raw_clean",
  "student_status_raw_clean",
  
  "city_size_raw_clean",
  "climate_zone_raw_clean",
  "household_type_raw_clean",
  "tenure_raw_clean",
  "education_raw_clean",
  "employment_raw_clean",
  "num_children_raw_clean",
  "health_raw_clean",
  "income_raw_clean",
  
  "travel_distance_raw_clean",
  "travel_time_raw_clean",
  "work_from_home_raw_clean",
  "travel_role_raw_clean",
  
  "vote_raw_clean",
  "political_left_right_raw_clean",
  "self_classification_raw_clean",
  
  "energy_efficiency_goal_raw_clean",
  "climate_awareness_raw_clean",
  "energy_transition_awareness_raw_clean",
  
  "renovation_role_raw_clean",
  "household_decision_raw_clean",
  
  "year_birth_model",
  "age_model",
  "age_model_is_approximate",
  "age_group_model",
  "gender_model",
  "education_model",
  "employment_model",
  "student_status_model",
  "city_size_model",
  "tenure_model",
  "health_condition_model",
  "work_from_home_model",
  "income_model",
  "num_children_model",
  "country_model",
  "country_model_grouped",
  "country_birth_model",
  "nationality_model",
  "ethnicity_model",
  "language_model",
  
  "climate_zone_model",
  "household_type_model",
  "travel_role_model",
  "renovation_role_model",
  "household_decision_model",
  
  "vote_status_declared",
  "voted_observed",
  "political_left_right_model",
  "political_block_model",
  "self_classification_model",
  
  "energy_efficiency_goal_0_100",
  "climate_awareness_0_100",
  "energy_transition_awareness_0_100",
  "travel_distance_model",
  "travel_time_model",
  
  "year_birth_conflict",
  "age_conflict",
  "gender_conflict",
  "education_conflict",
  "employment_conflict",
  "student_status_conflict",
  "country_conflict",
  "country_birth_conflict",
  "nationality_conflict",
  "ethnicity_conflict",
  "income_conflict",
  "vote_conflict"
)

det_clean_cols <- c(
  "n_det_non_missing",
  "prop_det_non_missing",
  det_cols
)

all_sources_integrated_clean <- all_sources_integrated_clean %>%
  relocate(
    any_of(c(sociodemographic_clean_cols, det_clean_cols)),
    .after = any_of("identification_code")
  )


# Crear dos versiones del dataset limpio
all_sources_integrated_clean_traceability <- all_sources_integrated_clean

analysis_clean_cols <- unique(c(
  id_cols,
  sociodemographic_clean_cols,
  det_clean_cols
))

all_sources_integrated_clean <- all_sources_integrated_clean_traceability %>%
  select(any_of(analysis_clean_cols))


# Datasets compactos extra
sociodemographics_clean <- all_sources_integrated_clean %>%
  select(
    any_of(id_cols),
    any_of(sociodemographic_clean_cols)
  )

determinants_harmonized <- all_sources_integrated_clean %>%
  select(
    any_of(id_cols),
    any_of(det_clean_cols)
  )

# Diagnósticos sociodemográficos
coverage_vars <- c(
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
  "climate_zone_model",
  "vote_status_declared",
  "voted_observed",
  "political_left_right_model",
  "political_block_model",
  "self_classification_model",
  "energy_efficiency_goal_0_100",
  "climate_awareness_0_100",
  "energy_transition_awareness_0_100"
)

diagnostics_sociodemographics_coverage <- sociodemographics_clean %>%
  group_by(dataset_source) %>%
  summarise(
    n_rows = n(),
    across(
      all_of(coverage_vars),
      ~ sum(is_valid_model_value(.x), na.rm = TRUE),
      .names = "n_non_missing_{.col}"
    ),
    .groups = "drop"
  ) %>%
  bind_rows(
    sociodemographics_clean %>%
      summarise(
        dataset_source = "TOTAL",
        n_rows = n(),
        across(
          all_of(coverage_vars),
          ~ sum(is_valid_model_value(.x), na.rm = TRUE),
          .names = "n_non_missing_{.col}"
        )
      )
  )

diagnostics_sociodemographics_counts <- bind_rows(
  sociodemographics_clean %>%
    count(dataset_source, variable = "age_group_model", value = age_group_model, name = "n"),
  
  sociodemographics_clean %>%
    count(dataset_source, variable = "gender_model", value = gender_model, name = "n"),
  
  sociodemographics_clean %>%
    count(dataset_source, variable = "education_model", value = education_model, name = "n"),
  
  sociodemographics_clean %>%
    count(dataset_source, variable = "employment_model", value = employment_model, name = "n"),
  
  sociodemographics_clean %>%
    count(dataset_source, variable = "student_status_model", value = student_status_model, name = "n"),
  
  sociodemographics_clean %>%
    count(dataset_source, variable = "income_model", value = income_model, name = "n"),
  
  sociodemographics_clean %>%
    count(dataset_source, variable = "country_model_grouped", value = country_model_grouped, name = "n"),
  
  sociodemographics_clean %>%
    count(dataset_source, variable = "country_birth_model", value = country_birth_model, name = "n"),
  
  sociodemographics_clean %>%
    count(dataset_source, variable = "nationality_model", value = nationality_model, name = "n"),
  
  sociodemographics_clean %>%
    count(dataset_source, variable = "language_model", value = language_model, name = "n"),
  
  sociodemographics_clean %>%
    count(dataset_source, variable = "vote_status_declared", value = vote_status_declared, name = "n"),
  
  sociodemographics_clean %>%
    count(dataset_source, variable = "political_block_model", value = political_block_model, name = "n"),
  
  sociodemographics_clean %>%
    count(dataset_source, variable = "self_classification_model", value = self_classification_model, name = "n")
) %>%
  group_by(dataset_source, variable) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  arrange(dataset_source, variable, desc(n))

diagnostics_conflicts <- sociodemographics_clean %>%
  summarise(
    n_rows = n(),
    n_year_birth_conflict = sum(year_birth_conflict, na.rm = TRUE),
    n_age_conflict = sum(age_conflict, na.rm = TRUE),
    n_gender_conflict = sum(gender_conflict, na.rm = TRUE),
    n_education_conflict = sum(education_conflict, na.rm = TRUE),
    n_employment_conflict = sum(employment_conflict, na.rm = TRUE),
    n_student_status_conflict = sum(student_status_conflict, na.rm = TRUE),
    n_country_conflict = sum(country_conflict, na.rm = TRUE),
    n_country_birth_conflict = sum(country_birth_conflict, na.rm = TRUE),
    n_nationality_conflict = sum(nationality_conflict, na.rm = TRUE),
    n_ethnicity_conflict = sum(ethnicity_conflict, na.rm = TRUE),
    n_income_conflict = sum(income_conflict, na.rm = TRUE),
    n_vote_conflict = sum(vote_conflict, na.rm = TRUE)
  )

# Diagnósticos de 32 determinantes
diagnostics_32det_by_source <- all_sources_integrated_clean %>%
  group_by(dataset_source) %>%
  summarise(
    n_rows = n(),
    n_with_any_det = sum(n_det_non_missing > 0, na.rm = TRUE),
    n_complete_32_det = sum(n_det_non_missing == 32, na.rm = TRUE),
    mean_n_det_non_missing = mean(n_det_non_missing, na.rm = TRUE),
    min_n_det_non_missing = min(n_det_non_missing, na.rm = TRUE),
    max_n_det_non_missing = max(n_det_non_missing, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  bind_rows(
    all_sources_integrated_clean %>%
      summarise(
        dataset_source = "TOTAL",
        n_rows = n(),
        n_with_any_det = sum(n_det_non_missing > 0, na.rm = TRUE),
        n_complete_32_det = sum(n_det_non_missing == 32, na.rm = TRUE),
        mean_n_det_non_missing = mean(n_det_non_missing, na.rm = TRUE),
        min_n_det_non_missing = min(n_det_non_missing, na.rm = TRUE),
        max_n_det_non_missing = max(n_det_non_missing, na.rm = TRUE)
      )
  )

det_long <- all_sources_integrated_clean %>%
  select(dataset_source, all_of(det_cols)) %>%
  pivot_longer(
    cols = all_of(det_cols),
    names_to = "det_col",
    values_to = "value"
  )

diagnostics_32det_ranges <- det_long %>%
  group_by(dataset_source, det_col) %>%
  summarise(
    n_non_missing = sum(!is.na(value)),
    min_value = suppressWarnings(min(value, na.rm = TRUE)),
    max_value = suppressWarnings(max(value, na.rm = TRUE)),
    n_below_0 = sum(value < 0, na.rm = TRUE),
    n_above_100 = sum(value > 100, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    min_value = if_else(is.infinite(min_value), NA_real_, min_value),
    max_value = if_else(is.infinite(max_value), NA_real_, max_value)
  )

# Diccionario sociodemográfico
sociodemographic_dictionary <- tribble(
  ~variable, ~description,
  "reference_year_model", "Reference year used to compute age when only year of birth is available. RV = 2026, WHY = 2022, Diego = 2023.",
  "year_birth_model", "Year of birth harmonised from survey and Prolific fields.",
  "age_model", "Age harmonised from direct age fields, computed from year of birth, or approximated from Diego age group.",
  "age_model_is_approximate", "TRUE when age_model was approximated from an age group rather than observed as exact age.",
  "age_group_model", "Age grouped for modelling.",
  "gender_model", "Gender harmonised into female, male, other, unknown or conflict.",
  "education_model", "Education harmonised from RV, Diego Q4 and any Prolific education field found.",
  "employment_model", "Employment harmonised from RV, Diego Q5 and Prolific employment status.",
  "student_status_model", "Student status harmonised mainly from Prolific student status.",
  "num_children_model", "Number of children harmonised mainly from Diego Q6 where available.",
  "income_model", "Income harmonised mainly from Diego Q7 and any clear income field found.",
  "country_model", "Country of residence harmonised as ISO2 code where possible.",
  "country_birth_model", "Country of birth harmonised as ISO2 code where possible.",
  "nationality_model", "Nationality harmonised from Prolific when available.",
  "ethnicity_model", "Ethnicity simplified harmonised from Prolific when available.",
  "country_model_grouped", "Country grouped; countries with fewer than MIN_COUNTRY_N rows assigned to OTHER_COUNTRIES.",
  "language_model", "Language harmonised into EN, ES, EU, NL, FR, DE, IT, PL, GR, OTHER or unknown.",
  "vote_status_declared", "Declared voting status harmonised into voter, abstainer, uncertain, unknown or conflict.",
  "voted_observed", "Binary voting indicator: voter = 1, abstainer = 0, otherwise NA.",
  "political_left_right_model", "Numeric left-right self-placement, expected 0-100.",
  "political_block_model", "Political self-placement grouped into extreme_left, left, centre, right, extreme_right, unknown or invalid.",
  "self_classification_model", "Household investment self-classification mapped to archetype-like categories.",
  "energy_efficiency_goal_0_100", "Energy efficiency goal parsed as numeric 0-100 where available.",
  "climate_awareness_0_100", "Climate change awareness parsed as numeric 0-100 where available.",
  "energy_transition_awareness_0_100", "Energy transition awareness parsed as numeric 0-100 where available.",
  "travel_distance_model", "Daily travel distance parsed as numeric where available.",
  "travel_time_model", "Daily travel time parsed as numeric where available.",
  "n_det_non_missing", "Number of available harmonised determinants among the 32 determinant variables.",
  "prop_det_non_missing", "Proportion of available harmonised determinants among the 32 determinant variables."
)

# Guardado
write_csv(
  all_sources_integrated_clean,
  file.path(out_dir, "all_sources_integrated_clean.csv")
)

write_csv(
  all_sources_integrated_clean_traceability,
  file.path(out_dir, "all_sources_integrated_clean_traceability.csv")
)

write_csv(
  sociodemographics_clean,
  file.path(out_dir, "sociodemographics_clean.csv")
)

write_csv(
  determinants_harmonized,
  file.path(out_dir, "determinants_harmonized.csv")
)

write_csv(
  sociodemographic_source_columns,
  file.path(out_dir, "sociodemographic_source_columns.csv")
)

write_csv(
  determinant_dictionary,
  file.path(out_dir, "determinant_dictionary_32.csv")
)

write_csv(
  diagnostics_sociodemographics_coverage,
  file.path(out_dir, "diagnostics_sociodemographics_coverage.csv")
)

write_csv(
  diagnostics_sociodemographics_counts,
  file.path(out_dir, "diagnostics_sociodemographics_counts.csv")
)

write_csv(
  diagnostics_conflicts,
  file.path(out_dir, "diagnostics_sociodemographics_conflicts.csv")
)

write_csv(
  diagnostics_32det_by_source,
  file.path(out_dir, "diagnostics_32det_by_source.csv")
)

write_csv(
  diagnostics_32det_ranges,
  file.path(out_dir, "diagnostics_32det_ranges.csv")
)

write_csv(
  sociodemographic_dictionary,
  file.path(out_dir, "sociodemographic_dictionary.csv")
)


# Mensajes finales
print(diagnostics_sociodemographics_coverage)
print(diagnostics_32det_by_source)

message("Listo.")
message("Dataset limpio para análisis: ", file.path(out_dir, "all_sources_integrated_clean.csv"))
message("Dataset completo para trazabilidad: ", file.path(out_dir, "all_sources_integrated_clean_traceability.csv"))
message("Sociodemográficos limpios: ", file.path(out_dir, "sociodemographics_clean.csv"))
message("Determinantes armonizados: ", file.path(out_dir, "determinants_harmonized.csv"))
message("Columnas fuente sociodemográficas: ", file.path(out_dir, "sociodemographic_source_columns.csv"))
message("Diccionario determinantes: ", file.path(out_dir, "determinant_dictionary_32.csv"))
message("Diagnóstico cobertura sociodemográfica: ", file.path(out_dir, "diagnostics_sociodemographics_coverage.csv"))
message("Diagnóstico conteos sociodemográficos: ", file.path(out_dir, "diagnostics_sociodemographics_counts.csv"))
message("Diagnóstico conflictos sociodemográficos: ", file.path(out_dir, "diagnostics_sociodemographics_conflicts.csv"))
message("Diagnóstico 32 determinantes por fuente: ", file.path(out_dir, "diagnostics_32det_by_source.csv"))
message("Diagnóstico rangos 32 determinantes: ", file.path(out_dir, "diagnostics_32det_ranges.csv"))
message("Diccionario sociodemográfico: ", file.path(out_dir, "sociodemographic_dictionary.csv"))