

# Objetivo:
# 1. Leer y unir las 4 encuestas RV completas.
# 2. Leer WHY completo y separar Europa y Latinoamérica.
# 3. Leer datos de Diego y transformar escala a 0-100.
# 4. Construir la base integrada completa:
#       RENOVISOR + WHY_EUROPE + WHY_LATAM + DIEGO
# 5. Añadir la etiqueta "subsample":
#       - DIEGO
#       - RENOVISOR
#       - WHY_EUROPE
#       - WHY_LATAM
#
# POOLED_ALL se utiliza únicamente como resumen descriptivo.

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(janitor)
})

set.seed(123)

# Configuración
paths <- list(
  rv_decision = "paper1_cluster/data/raw/Content_Export_RV-Decision_956.xlsx",
  rv_concerns = "paper1_cluster/data/raw/Content_Export_RV-Concerns_2__166.xlsx",
  rv_energy = "paper1_cluster/data/raw/Content_Export_RV-Energy_Crisis_157.xlsx",
  rv_poverty = "paper1_cluster/data/raw/Content_Export_RV-Poverty__96.xlsx",
  why = "paper1_cluster/data/raw/Content_Export_Investment_Arquetypes_2022_full-latin-final.csv",
  diego = "paper1_cluster/data/raw/Survey_September2023_cleaned.xlsx",
  prolific_dir = "paper1_cluster/data/raw/prolific"
)

out_dir <- "paper1_cluster/data/processed/01_mergeData"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

DIEGO_SCALE <- "1_5"
ADD_DIEGO_NOISE <- TRUE
DIEGO_NOISE_SD <- 1
USE_DIEGO_NOISY_INTEGRATED <- TRUE


# Funciones auxiliares
quiet_unique_names <- function(x) {
  x[is.na(x) | x == ""] <- paste0("unnamed_", which(is.na(x) | x == ""))
  make.unique(x, sep = "...")
}

normalise_id <- function(x) {
  x <- str_trim(str_to_lower(as.character(x)))
  x <- na_if(x, "")
  x <- na_if(x, "na")
  x <- na_if(x, "nan")
  x <- str_replace_all(x, "[^a-z0-9]", "")
  na_if(x, "")
}

parse_num <- function(x) {
  suppressWarnings(
    parse_number(as.character(x), locale = locale(decimal_mark = "."))
  )
}

first_non_missing <- function(x) {
  x <- x[!is.na(x) & x != "" & x != "NA" & x != "NaN"]
  if (!length(x)) return(NA_character_)
  as.character(x[[1]])
}

collapse_unique_non_missing <- function(x) {
  x <- unique(str_squish(as.character(x)))
  x <- x[!is.na(x) & x != "" & x != "NA" & x != "NaN"]
  
  if (!length(x)) return(NA_character_)
  if (length(x) == 1) return(x)
  
  paste(x, collapse = " | ")
}

get_first_matching_col <- function(df, pattern) {
  out <- names(df)[str_detect(names(df), regex(pattern, ignore_case = TRUE))][1]
  ifelse(is.na(out), NA_character_, out)
}

safe_pull <- function(df, col) {
  if (is.na(col) || !col %in% names(df)) {
    rep(NA_character_, nrow(df))
  } else {
    df[[col]]
  }
}

as_prefixed_character_df <- function(df, prefix) {
  df %>%
    clean_names() %>%
    rename_with(~ paste0(prefix, "__", .x)) %>%
    mutate(across(everything(), as.character))
}


# Países
country_name_to_iso2 <- c(
  "spain" = "ES", "espana" = "ES", "españa" = "ES",
  "germany" = "DE", "alemania" = "DE",
  "italy" = "IT", "italia" = "IT",
  "greece" = "GR", "grecia" = "GR",
  "the netherlands" = "NL", "netherlands" = "NL", "nederland" = "NL",
  "belgium" = "BE", "belgica" = "BE", "bélgica" = "BE",
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
  
  "argentina" = "AR",
  "bolivia" = "BO",
  "bolivia, plurinational state of" = "BO",
  "brazil" = "BR", "brasil" = "BR",
  "chile" = "CL",
  "colombia" = "CO",
  "costa rica" = "CR",
  "cuba" = "CU",
  "dominican republic" = "DO",
  "república dominicana" = "DO",
  "ecuador" = "EC",
  "el salvador" = "SV",
  "guatemala" = "GT",
  "honduras" = "HN",
  "mexico" = "MX", "méxico" = "MX",
  "nicaragua" = "NI",
  "panama" = "PA", "panamá" = "PA",
  "paraguay" = "PY",
  "peru" = "PE", "perú" = "PE",
  "uruguay" = "UY",
  "venezuela" = "VE",
  "venezuela, bolivarian republic of" = "VE",
  
  "united states" = "US",
  "canada" = "CA",
  "india" = "IN",
  "turkey" = "TR",
  "russian federation" = "RU",
  "nigeria" = "NG",
  "china" = "CN",
  "indonesia" = "ID",
  "south africa" = "ZA",
  "syrian arab republic" = "SY",
  "vietnam" = "VN",
  "zimbabwe" = "ZW",
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
  "tunisia" = "TN",
  
  "consent_revoked" = NA_character_,
  "data_expired" = NA_character_
)

europe_iso2 <- c(
  "AL", "AD", "AT", "BE", "BA", "BG", "HR", "CY", "CZ", "DK",
  "EE", "FI", "FR", "DE", "GR", "HU", "IS", "IE", "IT", "XK",
  "LV", "LI", "LT", "LU", "MT", "MD", "MC", "ME", "NL", "MK",
  "NO", "PL", "PT", "RO", "SM", "RS", "SK", "SI", "ES", "SE",
  "CH", "UA", "GB", "UK", "VA"
)

latam_iso2 <- c(
  "AR", "BO", "BR", "CL", "CO", "CR", "CU", "DO",
  "EC", "SV", "GT", "HN", "MX", "NI", "PA", "PY",
  "PE", "UY", "VE"
)

extract_country_code <- function(x) {
  x_chr <- str_squish(as.character(x))
  x_low <- str_to_lower(x_chr)
  
  code_exact <- ifelse(
    str_detect(x_chr, "^[A-Za-z]{2}$"),
    str_to_upper(x_chr),
    NA_character_
  )
  
  code_parentheses <- str_match(x_chr, "\\(([A-Za-z]{2})\\)")[, 2]
  code_prefix <- str_match(x_chr, "^\\s*([A-Za-z]{2})\\s*[-–]")[, 2]
  code_name <- unname(country_name_to_iso2[x_low])
  
  str_to_upper(
    coalesce(code_exact, code_parentheses, code_prefix, code_name)
  )
}

is_europe_country <- function(country_code) {
  ifelse(
    is.na(country_code),
    NA,
    country_code %in% europe_iso2
  )
}

read_excel_content <- function(path, skip = 3) {
  sheets <- excel_sheets(path)
  
  read_excel(
    path,
    sheet = if ("Content" %in% sheets) "Content" else sheets[1],
    skip = skip,
    .name_repair = quiet_unique_names
  ) %>%
    as_tibble()
}


# RENOVISOR
read_rv_survey_full <- function(path, survey_name) {
  raw <- read_excel_content(path)
  raw_payload <- as_prefixed_character_df(raw, paste0("rv_", survey_name))
  
  meta <- tibble(
    dataset_source = "rv",
    source_survey = survey_name,
    source_file = basename(path),
    source_row = seq_len(nrow(raw)),
    prolific_id = normalise_id(raw[[2]]),
    identification_code = normalise_id(raw[[4]]),
    participant_key = coalesce(
      prolific_id,
      identification_code,
      paste0("rv_noid_", survey_name, "_", seq_len(nrow(raw)))
    ),
    year_birth_raw = as.character(raw[[5]]),
    gender_raw = as.character(raw[[6]]),
    country_raw = as.character(raw[[7]]),
    country_code = extract_country_code(country_raw)
  )
  
  bind_cols(meta, raw_payload)
}

rv_files <- c(
  decision = paths$rv_decision,
  concerns2 = paths$rv_concerns,
  energy_crisis = paths$rv_energy,
  poverty = paths$rv_poverty
)

rv_all_rows <- imap_dfr(rv_files, read_rv_survey_full)

rv_by_participant <- rv_all_rows %>%
  group_by(participant_key) %>%
  summarise(
    dataset_source = "rv",
    n_source_rows = n(),
    n_distinct_surveys = n_distinct(source_survey),
    source_survey = paste(sort(unique(source_survey)), collapse = ";"),
    source_file = paste(sort(unique(source_file)), collapse = ";"),
    prolific_id = first_non_missing(prolific_id),
    identification_code = first_non_missing(identification_code),
    year_birth_raw = first_non_missing(year_birth_raw),
    gender_raw = first_non_missing(gender_raw),
    country_raw = first_non_missing(country_raw),
    country_code = first_non_missing(country_code),
    
    across(
      -any_of(c(
        "dataset_source",
        "source_survey",
        "source_file",
        "source_row",
        "prolific_id",
        "identification_code",
        "year_birth_raw",
        "gender_raw",
        "country_raw",
        "country_code"
      )),
      collapse_unique_non_missing
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    global_participant_key = paste(
      dataset_source,
      participant_key,
      sep = "__"
    )
  ) %>%
  relocate(global_participant_key, .before = 1)


# Prolific
read_prolific_file <- function(path) {
  raw <- read_csv(
    path,
    show_col_types = FALSE,
    name_repair = quiet_unique_names,
    col_types = cols(.default = col_character())
  ) %>%
    clean_names()
  
  participant_col <- get_first_matching_col(raw, "^participant_id$|prolific")
  status_col <- get_first_matching_col(raw, "^status$")
  age_col <- get_first_matching_col(raw, "^age$")
  sex_col <- get_first_matching_col(raw, "^sex$|gender")
  country_residence_col <- get_first_matching_col(
    raw,
    "^country_of_residence$|country_residence|residence"
  )
  country_birth_col <- get_first_matching_col(
    raw,
    "^country_of_birth$|country_birth|birth_country"
  )
  nationality_col <- get_first_matching_col(raw, "^nationality$")
  ethnicity_col <- get_first_matching_col(
    raw,
    "^ethnicity_simplified$|ethnicity"
  )
  language_col <- get_first_matching_col(raw, "^language$")
  student_status_col <- get_first_matching_col(
    raw,
    "^student_status$|student"
  )
  employment_status_col <- get_first_matching_col(
    raw,
    "^employment_status$|employment"
  )
  
  country_residence_raw <- as.character(
    safe_pull(raw, country_residence_col)
  )
  
  country_birth_raw <- as.character(
    safe_pull(raw, country_birth_col)
  )
  
  tibble(
    prolific_id = normalise_id(safe_pull(raw, participant_col)),
    prolific_status = as.character(safe_pull(raw, status_col)),
    prolific_country_raw = country_residence_raw,
    prolific_country_code = extract_country_code(country_residence_raw),
    prolific_country_of_birth = country_birth_raw,
    prolific_country_of_birth_code = extract_country_code(country_birth_raw),
    prolific_age = suppressWarnings(
      as.integer(parse_num(safe_pull(raw, age_col)))
    ),
    prolific_sex = as.character(safe_pull(raw, sex_col)),
    prolific_ethnicity_simplified = as.character(
      safe_pull(raw, ethnicity_col)
    ),
    prolific_nationality = as.character(
      safe_pull(raw, nationality_col)
    ),
    prolific_language = as.character(
      safe_pull(raw, language_col)
    ),
    prolific_student_status = as.character(
      safe_pull(raw, student_status_col)
    ),
    prolific_employment_status = as.character(
      safe_pull(raw, employment_status_col)
    ),
    prolific_file = basename(path)
  )
}

prolific_files <- if (dir.exists(paths$prolific_dir)) {
  list.files(
    paths$prolific_dir,
    pattern = "^prolific_export_.*\\.csv$",
    full.names = TRUE
  )
} else {
  character(0)
}

prolific_pool <- if (length(prolific_files)) {
  map_dfr(prolific_files, read_prolific_file) %>%
    filter(!is.na(prolific_id)) %>%
    group_by(prolific_id) %>%
    summarise(
      prolific_status = first_non_missing(prolific_status),
      prolific_country_raw = first_non_missing(prolific_country_raw),
      prolific_country_code = first_non_missing(prolific_country_code),
      prolific_country_of_birth = first_non_missing(
        prolific_country_of_birth
      ),
      prolific_country_of_birth_code = first_non_missing(
        prolific_country_of_birth_code
      ),
      prolific_age = first_non_missing(prolific_age),
      prolific_sex = first_non_missing(prolific_sex),
      prolific_ethnicity_simplified = first_non_missing(
        prolific_ethnicity_simplified
      ),
      prolific_nationality = first_non_missing(prolific_nationality),
      prolific_language = first_non_missing(prolific_language),
      prolific_student_status = first_non_missing(
        prolific_student_status
      ),
      prolific_employment_status = first_non_missing(
        prolific_employment_status
      ),
      prolific_files = paste(
        sort(unique(prolific_file)),
        collapse = ";"
      ),
      .groups = "drop"
    )
} else {
  tibble(
    prolific_id = character(),
    prolific_status = character(),
    prolific_country_raw = character(),
    prolific_country_code = character(),
    prolific_country_of_birth = character(),
    prolific_country_of_birth_code = character(),
    prolific_age = character(),
    prolific_sex = character(),
    prolific_ethnicity_simplified = character(),
    prolific_nationality = character(),
    prolific_language = character(),
    prolific_student_status = character(),
    prolific_employment_status = character(),
    prolific_files = character()
  )
}


# WHY
read_why_full <- function(path, prolific_pool) {
  raw_clean <- read_csv(
    path,
    skip = 2,
    show_col_types = FALSE,
    name_repair = quiet_unique_names,
    col_types = cols(.default = col_character())
  ) %>%
    as_tibble() %>%
    clean_names()
  
  prolific_col <- "please_provide_your_prolific_id_id131"
  country_col <- "i_which_country_do_you_reside_id300"
  language_col <- "languages"
  
  if (!prolific_col %in% names(raw_clean)) {
    stop(
      "No encuentro la columna de Prolific ID en WHY: ",
      prolific_col
    )
  }
  
  if (!country_col %in% names(raw_clean)) {
    warning(
      "No encuentro la columna de país en WHY: ",
      country_col
    )
  }
  
  raw_payload <- raw_clean %>%
    mutate(across(everything(), as.character)) %>%
    rename_with(~ paste0("why__", .x))
  
  meta <- tibble(
    dataset_source = "why",
    source_survey = "Investment_Arquetypes_2022",
    source_file = basename(path),
    source_row = seq_len(nrow(raw_clean)),
    prolific_id = normalise_id(raw_clean[[prolific_col]]),
    participant_key = coalesce(
      prolific_id,
      paste0("why_noid_", seq_len(nrow(raw_clean)))
    ),
    country_raw_survey = if (country_col %in% names(raw_clean)) {
      as.character(raw_clean[[country_col]])
    } else {
      NA_character_
    },
    country_code_survey = extract_country_code(country_raw_survey),
    language_raw = if (language_col %in% names(raw_clean)) {
      as.character(raw_clean[[language_col]])
    } else {
      NA_character_
    }
  )
  
  bind_cols(meta, raw_payload) %>%
    left_join(prolific_pool, by = "prolific_id") %>%
    mutate(
      # Se prioriza el país de residencia de Prolific.
      country_raw = coalesce(
        prolific_country_raw,
        country_raw_survey
      ),
      country_code = coalesce(
        prolific_country_code,
        country_code_survey
      ),
      country_source = case_when(
        !is.na(prolific_country_code) ~ "prolific",
        !is.na(country_code_survey) ~ "survey",
        TRUE ~ NA_character_
      ),
      is_europe = is_europe_country(country_code)
    )
}

why_all_rows <- read_why_full(paths$why, prolific_pool)

why_europe_rows <- why_all_rows %>%
  filter(is_europe == TRUE)

why_latam_rows <- why_all_rows %>%
  filter(country_code %in% latam_iso2)

collapse_why_participants <- function(data) {
  data %>%
    group_by(participant_key) %>%
    summarise(
      dataset_source = "why",
      n_source_rows = n(),
      n_distinct_surveys = n_distinct(source_survey),
      source_survey = paste(
        sort(unique(source_survey)),
        collapse = ";"
      ),
      source_file = paste(
        sort(unique(source_file)),
        collapse = ";"
      ),
      prolific_id = first_non_missing(prolific_id),
      country_raw = first_non_missing(country_raw),
      country_code = first_non_missing(country_code),
      country_source = first_non_missing(country_source),
      language_raw = first_non_missing(language_raw),
      
      across(
        -any_of(c(
          "dataset_source",
          "source_survey",
          "source_file",
          "source_row",
          "prolific_id",
          "country_raw",
          "country_code_survey",
          "country_code",
          "country_source",
          "language_raw",
          "is_europe"
        )),
        collapse_unique_non_missing
      ),
      
      .groups = "drop"
    ) %>%
    mutate(
      global_participant_key = paste(
        dataset_source,
        participant_key,
        sep = "__"
      )
    ) %>%
    relocate(global_participant_key, .before = 1)
}

why_europe_by_participant <- collapse_why_participants(
  why_europe_rows
)

why_latam_by_participant <- collapse_why_participants(
  why_latam_rows
)


# Diego
get_diego_det_cols <- function(df_clean) {
  expected <- unlist(
    map(21:24, ~ paste0("q", .x, "_", 1:8))
  )
  
  found <- expected[expected %in% names(df_clean)]
  
  if (length(found) != 32) {
    warning(
      paste0(
        "No se han encontrado exactamente los 32 determinantes ",
        "de Diego Q21_1:Q24_8. Encontrados: ",
        length(found)
      )
    )
  }
  
  found
}

add_truncated_gaussian_noise <- function(
    x,
    sd = 1,
    min_value = 0,
    max_value = 5,
    max_iter = 100
) {
  x <- as.numeric(x)
  noise <- rnorm(length(x), mean = 0, sd = sd)
  
  bad <- !is.na(x) &
    ((x + noise) < min_value | (x + noise) > max_value)
  
  iter <- 1
  
  while (any(bad) && iter <= max_iter) {
    noise[bad] <- rnorm(
      sum(bad),
      mean = 0,
      sd = sd
    )
    
    bad <- !is.na(x) &
      ((x + noise) < min_value | (x + noise) > max_value)
    
    iter <- iter + 1
  }
  
  pmin(
    max_value,
    pmax(min_value, x + noise)
  )
}

transform_to_0_100 <- function(x, scale_type = "1_5") {
  x <- as.numeric(x)
  
  if (scale_type == "0_5") {
    out <- x * 20
  } else if (scale_type == "1_5") {
    out <- ((x - 1) / 4) * 100
  } else {
    stop(
      "Escala de Diego no reconocida. Usa '0_5' o '1_5'."
    )
  }
  
  pmin(
    100,
    pmax(0, out)
  )
}

read_diego_transformed <- function(
    path,
    diego_scale = "0_5",
    add_noise = FALSE,
    noise_sd = 1
) {
  raw_clean <- read_excel(
    path,
    .name_repair = quiet_unique_names
  ) %>%
    as_tibble() %>%
    clean_names()
  
  det_cols <- get_diego_det_cols(raw_clean)
  
  raw_payload <- raw_clean %>%
    mutate(across(everything(), as.character)) %>%
    rename_with(~ paste0("diego__", .x))
  
  country_col <- get_first_matching_col(
    raw_clean,
    "^q3$|country"
  )
  
  gender_col <- get_first_matching_col(
    raw_clean,
    "^q2$|gender"
  )
  
  age_col <- get_first_matching_col(
    raw_clean,
    "^q1$|age"
  )
  
  meta <- tibble(
    dataset_source = "diego",
    source_survey = "Survey_September2023_cleaned",
    source_file = basename(path),
    source_row = seq_len(nrow(raw_clean)),
    participant_key = paste0(
      "diego_",
      seq_len(nrow(raw_clean))
    ),
    age_raw = as.character(
      safe_pull(raw_clean, age_col)
    ),
    gender_raw = as.character(
      safe_pull(raw_clean, gender_col)
    ),
    country_raw = as.character(
      safe_pull(raw_clean, country_col)
    ),
    country_code = extract_country_code(country_raw),
    diego_scale_assumption = diego_scale,
    diego_noise_added = add_noise,
    diego_noise_sd = ifelse(
      add_noise,
      noise_sd,
      NA_real_
    )
  )
  
  det_original <- raw_clean %>%
    select(any_of(det_cols)) %>%
    mutate(across(everything(), parse_num))
  
  if (length(det_cols)) {
    min_original <- ifelse(
      diego_scale == "1_5",
      1,
      0
    )
    
    det_0_100 <- det_original %>%
      mutate(
        across(
          everything(),
          ~ {
            x <- .x
            
            if (add_noise) {
              x <- add_truncated_gaussian_noise(
                x,
                sd = noise_sd,
                min_value = min_original,
                max_value = 5
              )
            }
            
            transform_to_0_100(
              x,
              scale_type = diego_scale
            )
          }
        )
      )
    
    names(det_0_100) <- paste0(
      "diego_det_",
      sprintf("%02d", seq_along(det_cols)),
      "_0_100"
    )
  } else {
    det_0_100 <- tibble()
  }
  
  bind_cols(
    meta,
    raw_payload,
    det_0_100
  ) %>%
    mutate(
      global_participant_key = paste(
        dataset_source,
        participant_key,
        sep = "__"
      )
    ) %>%
    relocate(global_participant_key, .before = 1)
}

diego_transformed <- read_diego_transformed(
  paths$diego,
  diego_scale = DIEGO_SCALE,
  add_noise = FALSE,
  noise_sd = DIEGO_NOISE_SD
)

diego_transformed_noisy <- read_diego_transformed(
  paths$diego,
  diego_scale = DIEGO_SCALE,
  add_noise = ADD_DIEGO_NOISE,
  noise_sd = DIEGO_NOISE_SD
)


# Integración
diego_for_integrated <- if (USE_DIEGO_NOISY_INTEGRATED) {
  diego_transformed_noisy
} else {
  diego_transformed
}

rv_for_integrated <- rv_by_participant %>%
  mutate(
    subsample = "RENOVISOR",
    comparison_region = "EUROPE"
  )

why_europe_for_integrated <- why_europe_by_participant %>%
  mutate(
    subsample = "WHY_EUROPE",
    comparison_region = "EUROPE"
  )

why_latam_for_integrated <- why_latam_by_participant %>%
  mutate(
    subsample = "WHY_LATAM",
    comparison_region = "LATAM"
  )

diego_for_integrated <- diego_for_integrated %>%
  mutate(
    subsample = "DIEGO",
    comparison_region = "EUROPE"
  )

all_sources_integrated <- bind_rows(
  rv_for_integrated,
  why_europe_for_integrated,
  why_latam_for_integrated,
  diego_for_integrated
) %>%
  mutate(
    integrated_row_id = row_number()
  ) %>%
  group_by(subsample) %>%
  mutate(
    subsample_row_id = row_number()
  ) %>%
  ungroup() %>%
  relocate(
    integrated_row_id,
    subsample,
    comparison_region,
    subsample_row_id,
    .before = 1
  )

duplicate_global_keys <- all_sources_integrated %>%
  count(
    global_participant_key,
    name = "n"
  ) %>%
  filter(n > 1)

if (nrow(duplicate_global_keys)) {
  warning(
    "Hay ",
    nrow(duplicate_global_keys),
    " global_participant_key duplicadas en la base integrada."
  )
}


# Diagnósticos
summary_sources <- all_sources_integrated %>%
  count(
    dataset_source,
    name = "n_rows"
  ) %>%
  bind_rows(
    tibble(
      dataset_source = "TOTAL",
      n_rows = nrow(all_sources_integrated)
    )
  )

summary_subsamples <- all_sources_integrated %>%
  count(
    comparison_region,
    subsample,
    name = "n_participants"
  ) %>%
  arrange(
    comparison_region,
    subsample
  ) %>%
  bind_rows(
    tibble(
      comparison_region = "ALL",
      subsample = "POOLED_ALL",
      n_participants = nrow(all_sources_integrated)
    )
  )

rv_diagnostics <- rv_all_rows %>%
  count(
    source_survey,
    name = "n_rows"
  ) %>%
  arrange(source_survey)

why_country_diagnostics <- why_all_rows %>%
  count(
    country_code,
    is_europe,
    country_source,
    name = "n_rows"
  ) %>%
  arrange(desc(n_rows))

why_region_diagnostics <- bind_rows(
  why_europe_by_participant %>%
    mutate(region = "WHY_EUROPE"),
  
  why_latam_by_participant %>%
    mutate(region = "WHY_LATAM")
) %>%
  count(
    region,
    country_code,
    name = "n"
  ) %>%
  arrange(
    region,
    desc(n)
  )

diego_diagnostics <- tibble(
  diego_scale_assumption = DIEGO_SCALE,
  diego_noise_version_saved = TRUE,
  diego_noise_sd = DIEGO_NOISE_SD,
  diego_noisy_used_in_integrated = USE_DIEGO_NOISY_INTEGRATED,
  n_rows_clean = nrow(diego_transformed),
  n_rows_noisy = nrow(diego_transformed_noisy),
  n_diego_det_cols_clean_0_100 = sum(
    str_detect(
      names(diego_transformed),
      "^diego_det_\\d{2}_0_100$"
    )
  ),
  n_diego_det_cols_noisy_0_100 = sum(
    str_detect(
      names(diego_transformed_noisy),
      "^diego_det_\\d{2}_0_100$"
    )
  )
)


# Guardado
outputs <- list(
  "rv_all_rows.csv" = rv_all_rows,
  "rv_by_participant.csv" = rv_by_participant,
  "prolific_pool_clean.csv" = prolific_pool,
  "why_all_rows.csv" = why_all_rows,
  "why_europe_rows.csv" = why_europe_rows,
  "why_europe_by_participant.csv" = why_europe_by_participant,
  "why_latam_rows.csv" = why_latam_rows,
  "why_latam_by_participant.csv" = why_latam_by_participant,
  "diego_transformed.csv" = diego_transformed,
  "diego_transformed_noisy.csv" = diego_transformed_noisy,
  "all_sources_integrated.csv" = all_sources_integrated,
  "summary_merge_all_sources.csv" = summary_sources,
  "summary_merge_subsamples.csv" = summary_subsamples,
  "diagnostics_rv_rows.csv" = rv_diagnostics,
  "diagnostics_why_countries.csv" = why_country_diagnostics,
  "diagnostics_why_europe_latam.csv" = why_region_diagnostics,
  "diagnostics_diego.csv" = diego_diagnostics
)

iwalk(
  outputs,
  ~ write_csv(.x, file.path(out_dir, .y))
)

# Resumen
cat(
  "\nWHY EUROPE PARTICIPANTS:",
  nrow(why_europe_by_participant),
  "\n"
)

cat(
  "WHY LATAM PARTICIPANTS:",
  nrow(why_latam_by_participant),
  "\n\n"
)

print(
  why_region_diagnostics,
  n = Inf
)

cat("\nRESUMEN INTEGRACIÓN COMPLETA\n")

print(
  summary_sources,
  n = Inf
)

cat("\nRESUMEN POR SUBMUESTRA\n")

print(
  summary_subsamples,
  n = Inf
)

cat(
  "\nGlobal participant keys duplicadas:",
  nrow(duplicate_global_keys),
  "\n"
)

message(
  "Listo. Base integrada: ",
  file.path(out_dir, "all_sources_integrated.csv")
)