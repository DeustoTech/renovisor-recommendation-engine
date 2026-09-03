

# mirar all_sources_integrated.csv
#
# Objetivo:
# 1. Leer y unir las 4 encuestas RV completas.
# 2. Leer WHY y filtrar solo participantes europeos.
# 3. Leer datos de Diego y transformar escala a 0-100.
# 4. Juntar RV + WHY + Diego en una base integrada.
#
# Importante:
# Este script NO construye todavía la matriz de 32 determinantes.
# Eso irá en el script 02_build_32det_matrix.R.

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(readr)
  library(stringr)
  library(janitor)
})

set.seed(123)

# Configuración
paths <- list(
  rv_decision = "paper1_cluster/data/raw/Content_Export_RV-Decision_956.xlsx",
  rv_concerns = "paper1_cluster/data/raw/Content_Export_RV-Concerns_2__166.xlsx",
  rv_energy   = "paper1_cluster/data/raw/Content_Export_RV-Energy_Crisis_157.xlsx",
  rv_poverty  = "paper1_cluster/data/raw/Content_Export_RV-Poverty__96.xlsx",
  
  why = "paper1_cluster/data/raw/Content_Export_Investment_Arquetypes_2022_full-latin-final.csv",
  
  diego = "paper1_cluster/data/raw/Survey_September2023_cleaned.xlsx",
  
  prolific_dir = "paper1_cluster/data/raw/prolific"
)

out_dir <- "paper1_cluster/data/processed/01_mergeData"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Cambiar a "1_5" si Diego está realmente en escala 1-5.
# Si está en escala 0-5, dejar "0_5".
DIEGO_SCALE <- "1_5"

# Ruido opcional
# Para este primer merge, lo dejo en FALSE porque esto es transformación, no aumento.
ADD_DIEGO_NOISE <- TRUE
DIEGO_NOISE_SD <- 1

USE_DIEGO_NOISY_INTEGRATED <- TRUE


# Funciones auxiliares
quiet_unique_names <- function(x) {
  x[is.na(x) | x == ""] <- paste0("unnamed_", which(is.na(x) | x == ""))
  make.unique(x, sep = "...")
}

normalise_id <- function(x) {
  x <- as.character(x)
  x <- str_trim(str_to_lower(x))
  x <- na_if(x, "")
  x <- na_if(x, "na")
  x <- na_if(x, "nan")
  x <- str_replace_all(x, "[^a-z0-9]", "")
  na_if(x, "")
}

parse_num <- function(x) {
  readr::parse_number(as.character(x), locale = locale(decimal_mark = "."))
}

first_non_missing <- function(x) {
  x <- x[!is.na(x) & x != "" & x != "NA" & x != "NaN"]
  if (length(x) == 0) return(NA_character_)
  as.character(x[[1]])
}

collapse_unique_non_missing <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x <- x[!is.na(x) & x != "" & x != "NA" & x != "NaN"]
  x <- unique(x)
  
  if (length(x) == 0) return(NA_character_)
  if (length(x) == 1) return(x)
  
  paste(x, collapse = " | ")
}

get_first_matching_col <- function(df, pattern) {
  nm <- names(df)
  out <- nm[str_detect(nm, regex(pattern, ignore_case = TRUE))][1]
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


# Países y filtro Europa
country_name_to_iso2 <- c(
  # Europa
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
  
  # Latinoamérica / no Europa
  "mexico" = "MX",
  "chile" = "CL",
  "colombia" = "CO",
  "argentina" = "AR",
  "peru" = "PE",
  "venezuela, bolivarian republic of" = "VE",
  "brazil" = "BR",
  "bolivia" = "BO",
  "panama" = "PA",
  "cuba" = "CU",
  
  # Otros no Europa
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
  
  # Valores no válidos
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

extract_country_code <- function(x) {
  x_chr <- as.character(x)
  x_chr <- str_squish(x_chr)
  x_low <- str_to_lower(x_chr)
  
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

is_europe_country <- function(country_code) {
  ifelse(
    is.na(country_code),
    NA,
    country_code %in% europe_iso2
  )
}

# Lectura de Excel tipo EUSurvey
read_excel_content <- function(path, skip = 3) {
  sheets <- readxl::excel_sheets(path)
  
  sheet_to_read <- if ("Content" %in% sheets) {
    "Content"
  } else {
    sheets[1]
  }
  
  readxl::read_excel(
    path,
    sheet = sheet_to_read,
    skip = skip,
    .name_repair = quiet_unique_names
  ) %>%
    as_tibble()
}

# Leer y unir las 4 encuestas RV completas
read_rv_survey_full <- function(path, survey_name) {
  raw <- read_excel_content(path, skip = 3)
  
  raw_payload <- as_prefixed_character_df(raw, paste0("rv_", survey_name))
  
  meta <- tibble(
    dataset_source = "rv",
    source_survey = survey_name,
    source_file = basename(path),
    source_row = seq_len(nrow(raw)),
    
    # En los exports RV, estas posiciones suelen corresponder a:
    # columna 2 = Prolific ID
    # columna 4 = identification code
    prolific_id = normalise_id(raw[[2]]),
    identification_code = normalise_id(raw[[4]]),
    
    participant_key = coalesce(
      prolific_id,
      identification_code,
      paste0("rv_noid_", survey_name, "_", seq_len(nrow(raw)))
    ),
    
    # Metadatos aproximados por posición.
    # Si alguna encuesta tiene estructura distinta, queda igualmente guardada en raw_payload.
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
      .cols = -any_of(c(
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
      .fns = collapse_unique_non_missing
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    global_participant_key = paste(dataset_source, participant_key, sep = "__")
  ) %>%
  relocate(global_participant_key, .before = 1)


# Leer exports de Prolific
# Leer exports de Prolific
read_prolific_file <- function(path) {
  raw <- read_csv(
    path,
    show_col_types = FALSE,
    name_repair = quiet_unique_names
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
  
  nationality_col <- get_first_matching_col(
    raw,
    "^nationality$"
  )
  
  ethnicity_col <- get_first_matching_col(
    raw,
    "^ethnicity_simplified$|ethnicity"
  )
  
  language_col <- get_first_matching_col(
    raw,
    "^language$"
  )
  
  student_status_col <- get_first_matching_col(
    raw,
    "^student_status$|student"
  )
  
  employment_status_col <- get_first_matching_col(
    raw,
    "^employment_status$|employment"
  )
  
  country_residence_raw <- as.character(safe_pull(raw, country_residence_col))
  country_birth_raw <- as.character(safe_pull(raw, country_birth_col))
  
  tibble(
    prolific_id = normalise_id(safe_pull(raw, participant_col)),
    
    prolific_status = as.character(safe_pull(raw, status_col)),
    
    prolific_country_raw = country_residence_raw,
    prolific_country_code = extract_country_code(country_residence_raw),
    
    prolific_country_of_birth = country_birth_raw,
    prolific_country_of_birth_code = extract_country_code(country_birth_raw),
    
    prolific_age = suppressWarnings(as.integer(parse_num(safe_pull(raw, age_col)))),
    prolific_sex = as.character(safe_pull(raw, sex_col)),
    
    prolific_ethnicity_simplified = as.character(safe_pull(raw, ethnicity_col)),
    prolific_nationality = as.character(safe_pull(raw, nationality_col)),
    prolific_language = as.character(safe_pull(raw, language_col)),
    prolific_student_status = as.character(safe_pull(raw, student_status_col)),
    prolific_employment_status = as.character(safe_pull(raw, employment_status_col)),
    
    prolific_file = basename(path)
  )
}

prolific_files <- if (dir.exists(paths$prolific_dir)) {
  list.files(
    path = paths$prolific_dir,
    pattern = "^prolific_export_.*\\.csv$",
    full.names = TRUE
  )
} else {
  character(0)
}

prolific_pool <- if (length(prolific_files) > 0) {
  map_dfr(prolific_files, read_prolific_file) %>%
    filter(!is.na(prolific_id)) %>%
    group_by(prolific_id) %>%
    summarise(
      prolific_status = first_non_missing(prolific_status),
      
      prolific_country_raw = first_non_missing(prolific_country_raw),
      prolific_country_code = first_non_missing(prolific_country_code),
      
      prolific_country_of_birth = first_non_missing(prolific_country_of_birth),
      prolific_country_of_birth_code = first_non_missing(prolific_country_of_birth_code),
      
      prolific_age = first_non_missing(prolific_age),
      prolific_sex = first_non_missing(prolific_sex),
      
      prolific_ethnicity_simplified = first_non_missing(prolific_ethnicity_simplified),
      prolific_nationality = first_non_missing(prolific_nationality),
      prolific_language = first_non_missing(prolific_language),
      prolific_student_status = first_non_missing(prolific_student_status),
      prolific_employment_status = first_non_missing(prolific_employment_status),
      
      prolific_files = paste(sort(unique(prolific_file)), collapse = ";"),
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
# Leer WHY y filtrar Europa
read_why_full <- function(path, prolific_pool) {
  raw <- read_csv(
    path,
    skip = 2,
    show_col_types = FALSE,
    name_repair = quiet_unique_names
  ) %>%
    as_tibble()
  
  raw_clean <- raw %>%
    clean_names()
  
  # Columnas específicas de WHY
  prolific_col <- "please_provide_your_prolific_id_id131"
  country_col  <- "i_which_country_do_you_reside_id300"
  language_col <- "languages"
  
  if (!prolific_col %in% names(raw_clean)) {
    stop("No encuentro la columna de Prolific ID en WHY: ", prolific_col)
  }
  
  if (!country_col %in% names(raw_clean)) {
    warning("No encuentro la columna de país en WHY: ", country_col)
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
      # Prioridad: país de Prolific. Si no existe, país declarado en WHY.
      country_raw = coalesce(prolific_country_raw, country_raw_survey),
      country_code = coalesce(prolific_country_code, country_code_survey),
      
      country_source = case_when(
        !is.na(prolific_country_code) ~ "prolific",
        is.na(prolific_country_code) & !is.na(country_code_survey) ~ "survey",
        TRUE ~ NA_character_
      ),
      
      is_europe = is_europe_country(country_code)
    )
}

why_all_rows <- read_why_full(paths$why, prolific_pool)

why_europe_rows <- why_all_rows %>%
  filter(is_europe == TRUE)

why_europe_by_participant <- why_europe_rows %>%
  group_by(participant_key) %>%
  summarise(
    dataset_source = "why",
    n_source_rows = n(),
    n_distinct_surveys = n_distinct(source_survey),
    source_survey = paste(sort(unique(source_survey)), collapse = ";"),
    source_file = paste(sort(unique(source_file)), collapse = ";"),
    
    prolific_id = first_non_missing(prolific_id),
    country_raw = first_non_missing(country_raw),
    country_code = first_non_missing(country_code),
    country_source = first_non_missing(country_source),
    language_raw = first_non_missing(language_raw),
    
    across(
      .cols = -any_of(c(
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
      .fns = collapse_unique_non_missing
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    global_participant_key = paste(dataset_source, participant_key, sep = "__")
  ) %>%
  relocate(global_participant_key, .before = 1)


# Leer Diego y transformar escala a 0-100
get_diego_det_cols <- function(df_clean) {
  expected <- unlist(map(21:24, ~ paste0("q", .x, "_", 1:8)))
  found <- expected[expected %in% names(df_clean)]
  
  if (length(found) != 32) {
    warning(
      "No se han encontrado exactamente los 32 determinantes de Diego Q21_1:Q24_8. ",
      "Encontrados: ", length(found)
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
  
  bad <- !is.na(x) & ((x + noise) < min_value | (x + noise) > max_value)
  iter <- 1
  
  while (any(bad) && iter <= max_iter) {
    noise[bad] <- rnorm(sum(bad), mean = 0, sd = sd)
    bad <- !is.na(x) & ((x + noise) < min_value | (x + noise) > max_value)
    iter <- iter + 1
  }
  
  x_noisy <- x + noise
  pmin(max_value, pmax(min_value, x_noisy))
}

transform_to_0_100 <- function(x, scale_type = "1_5") {
  x <- as.numeric(x)
  
  if (scale_type == "0_5") {
    out <- x * 20
  } else if (scale_type == "1_5") {
    out <- ((x - 1) / 4) * 100
  } else {
    stop("Escala de Diego no reconocida. Usa '0_5' o '1_5'.")
  }
  
  pmin(100, pmax(0, out))
}

read_diego_transformed <- function(
    path,
    diego_scale = "0_5",
    add_noise = FALSE,
    noise_sd = 1
) {
  raw <- readxl::read_excel(
    path,
    .name_repair = quiet_unique_names
  ) %>%
    as_tibble()
  
  raw_clean <- raw %>%
    clean_names()
  
  det_cols <- get_diego_det_cols(raw_clean)
  
  raw_payload <- raw_clean %>%
    mutate(across(everything(), as.character)) %>%
    rename_with(~ paste0("diego__", .x))
  
  country_col <- get_first_matching_col(raw_clean, "^q3$|country")
  gender_col <- get_first_matching_col(raw_clean, "^q2$|gender")
  age_col <- get_first_matching_col(raw_clean, "^q1$|age")
  
  meta <- tibble(
    dataset_source = "diego",
    source_survey = "Survey_September2023_cleaned",
    source_file = basename(path),
    source_row = seq_len(nrow(raw_clean)),
    
    participant_key = paste0("diego_", seq_len(nrow(raw_clean))),
    
    age_raw = as.character(safe_pull(raw_clean, age_col)),
    gender_raw = as.character(safe_pull(raw_clean, gender_col)),
    country_raw = as.character(safe_pull(raw_clean, country_col)),
    country_code = extract_country_code(country_raw),
    
    diego_scale_assumption = diego_scale,
    diego_noise_added = add_noise,
    diego_noise_sd = ifelse(add_noise, noise_sd, NA_real_)
  )
  
  det_original <- raw_clean %>%
    select(any_of(det_cols)) %>%
    mutate(across(everything(), parse_num))
  
  if (length(det_cols) > 0) {
    min_original <- ifelse(diego_scale == "1_5", 1, 0)
    max_original <- 5
    
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
                max_value = max_original
              )
            }
            
            transform_to_0_100(x, scale_type = diego_scale)
          }
        )
      )
    
    names(det_0_100) <- paste0("diego_det_", sprintf("%02d", seq_along(det_cols)), "_0_100")
  } else {
    det_0_100 <- tibble()
  }
  
  bind_cols(meta, raw_payload, det_0_100) %>%
    mutate(
      global_participant_key = paste(dataset_source, participant_key, sep = "__")
    ) %>%
    relocate(global_participant_key, .before = 1)
}

diego_transformed <- read_diego_transformed(
  path = paths$diego,
  diego_scale = DIEGO_SCALE,
  add_noise = FALSE,
  noise_sd = DIEGO_NOISE_SD
)

diego_transformed_noisy <- read_diego_transformed(
  path = paths$diego,
  diego_scale = DIEGO_SCALE,
  add_noise = ADD_DIEGO_NOISE,
  noise_sd = DIEGO_NOISE_SD
)

# Integración final de todas las fuentes
diego_for_integrated <- if (USE_DIEGO_NOISY_INTEGRATED) {
  diego_transformed_noisy
} else {
  diego_transformed
}

all_sources_integrated <- bind_rows(
  rv_by_participant,
  why_europe_by_participant,
  diego_for_integrated
) %>%
  mutate(
    integrated_row_id = row_number()
  ) %>%
  relocate(integrated_row_id, .before = 1)

# Diagnósticos
summary_sources <- all_sources_integrated %>%
  count(dataset_source, name = "n_rows") %>%
  bind_rows(
    tibble(dataset_source = "TOTAL", n_rows = nrow(all_sources_integrated))
  )

rv_diagnostics <- rv_all_rows %>%
  count(source_survey, name = "n_rows") %>%
  arrange(source_survey)

why_country_diagnostics <- why_all_rows %>%
  count(country_code, is_europe, country_source, name = "n_rows") %>%
  arrange(desc(n_rows))

diego_diagnostics <- tibble(
  diego_scale_assumption = DIEGO_SCALE,
  diego_noise_version_saved = TRUE,
  diego_noise_sd = DIEGO_NOISE_SD,
  diego_noisy_used_in_integrated = USE_DIEGO_NOISY_INTEGRATED,
  n_rows_clean = nrow(diego_transformed),
  n_rows_noisy = nrow(diego_transformed_noisy),
  n_diego_det_cols_clean_0_100 = sum(str_detect(names(diego_transformed), "^diego_det_\\d{2}_0_100$")),
  n_diego_det_cols_noisy_0_100 = sum(str_detect(names(diego_transformed_noisy), "^diego_det_\\d{2}_0_100$"))
)


# Guardado
write_csv(rv_all_rows, file.path(out_dir, "rv_all_rows.csv"))
write_csv(rv_by_participant, file.path(out_dir, "rv_by_participant.csv"))

write_csv(prolific_pool, file.path(out_dir, "prolific_pool_clean.csv"))

write_csv(why_all_rows, file.path(out_dir, "why_all_rows.csv"))
write_csv(why_europe_rows, file.path(out_dir, "why_europe_rows.csv"))
write_csv(why_europe_by_participant, file.path(out_dir, "why_europe_by_participant.csv"))

write_csv(diego_transformed, file.path(out_dir, "diego_transformed.csv"))

write_csv( diego_transformed_noisy,file.path(out_dir, "diego_transformed_noisy.csv"))

write_csv(all_sources_integrated, file.path(out_dir, "all_sources_integrated.csv"))

write_csv(summary_sources, file.path(out_dir, "summary_merge_all_sources.csv"))
write_csv(rv_diagnostics, file.path(out_dir, "diagnostics_rv_rows.csv"))
write_csv(why_country_diagnostics, file.path(out_dir, "diagnostics_why_countries.csv"))
write_csv(diego_diagnostics, file.path(out_dir, "diagnostics_diego.csv"))

print(summary_sources)

message("Listo.")
message("Base integrada creada en: ", file.path(out_dir, "all_sources_integrated.csv"))
message("RV por participante: ", file.path(out_dir, "rv_by_participant.csv"))
message("WHY Europa: ", file.path(out_dir, "why_europe_by_participant.csv"))
message("Diego transformado: ", file.path(out_dir, "diego_transformed.csv"))