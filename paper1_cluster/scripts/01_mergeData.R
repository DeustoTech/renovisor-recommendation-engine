# 01_mergeData.R
#
# OBJETIVO
# Integrar los datos originales de RENOVISOR, WHY y DIEGO en una base
# común, conservando las respuestas de origen y su trazabilidad.
#
# ENTRADAS
# Directorio: paper1_cluster/data/raw/
#
# RENOVISOR:
# - Content_Export_RV-Decision_956.xlsx
# - Content_Export_RV-Concerns_2__166.xlsx
# - Content_Export_RV-Energy_Crisis_157.xlsx
# - Content_Export_RV-Poverty__96.xlsx
#
# WHY:
# - Content_Export_Investment_Arquetypes_2022_full-latin-final.csv
#
# DIEGO:
# - Survey_September2023_cleaned.xlsx
#
# PROLIFIC:
# - prolific/prolific_export_*.csv, si existen.
#
# PROCESAMIENTO
# 1. Leer las cuatro encuestas RENOVISOR y agrupar sus registros por
#    participante, conservando las columnas originales.
# 2. Leer los archivos Prolific y consolidar la información por ID.
# 3. Leer WHY, incorporar la información de Prolific y separar los
#    participantes de Europa y Latinoamérica.
# 4. Transformar los 32 determinantes DIEGO a una escala 0-100 y
#    generar una versión sin ruido y otra con ruido gaussiano truncado.
# 5. Unir las cuatro submuestras en una base integrada.
# 6. Generar resúmenes y diagnósticos de integración.
#
# SALIDAS
# Directorio: paper1_cluster/data/processed/01_mergeData/
#
# Datos RENOVISOR:
# - rv_all_rows.csv: todas las filas de las cuatro encuestas antes
#   de agrupar por participante.
# - rv_by_participant.csv: respuestas RENOVISOR agrupadas por participante.
#
# Datos Prolific:
# - prolific_pool_clean.csv: registros Prolific consolidados por ID.
#
# Datos WHY:
# - why_all_rows.csv: todas las filas WHY con datos de Prolific y país.
# - why_europe_rows.csv: filas WHY clasificadas como Europa.
# - why_europe_by_participant.csv: participantes WHY de Europa agrupados.
# - why_latam_rows.csv: filas WHY clasificadas como Latinoamérica.
# - why_latam_by_participant.csv: participantes WHY de Latinoamérica agrupados.
#
# Datos DIEGO:
# - diego_transformed.csv: determinantes transformados a 0-100 sin ruido.
# - diego_transformed_noisy.csv: determinantes transformados con ruido.
#
# Integración:
# - all_sources_integrated.csv: base integrada con las cuatro submuestras.
#
# Diagnósticos:
# - summary_merge_all_sources.csv: número de registros por fuente.
# - summary_merge_subsamples.csv: tamaño de las cuatro submuestras.
# - diagnostics_rv_rows.csv: número de filas por encuesta RENOVISOR.
# - diagnostics_why_countries.csv: distribución WHY por país y fuente
#   de identificación del país.
# - diagnostics_why_europe_latam.csv: distribución WHY por región y país.
# - diagnostics_diego.csv: configuración y resultados de la transformación
#   de los determinantes DIEGO.
#
# COLUMNAS PRINCIPALES CREADAS
#
# integrated_row_id: identificador de fila en la base integrada.
#
# subsample_row_id: identificador de fila dentro de cada submuestra.
#
# subsample: DIEGO, RENOVISOR, WHY_EUROPE o WHY_LATAM.
#
# comparison_region: EUROPE o LATAM.
#
# dataset_source: fuente de origen: diego, rv o why.
#
# participant_key: identificador utilizado para agrupar registros de una
#   misma persona dentro de su fuente de datos.
#
# global_participant_key: identificador formado por dataset_source
#   y participant_key.
#
# n_source_rows: número de filas originales agrupadas por participante.
#
# n_distinct_surveys: número de encuestas de origen distintas.
#
# source_survey: encuesta o encuestas de origen.
#
# source_file: archivo o archivos de origen.
#
# source_row: número de fila de origen en las tablas no agrupadas.
#
# country_code: código de país de dos letras, cuando puede determinarse.
#
# country_source: fuente utilizada para determinar el país en WHY:
#   prolific o survey.
#
# diego_scale_assumption: escala original asumida para transformar DIEGO.
#
# diego_noise_added: indica si se añadió ruido a los determinantes DIEGO.
#
# diego_noise_sd: desviación estándar del ruido aplicado.
#
# diego_det_01_0_100 ... diego_det_32_0_100: determinantes DIEGO
#   transformados a la escala 0-100.
#
# rv_*, why__* y diego__*: columnas originales conservadas con su prefijo.
#
# DEPENDENCIAS
# - 00_common.R: librerías, rutas y constantes comunes del proyecto.

################################################################################

# CARGA DE 00_common.R
script_sources <- vapply(
  sys.frames(),
  function(frame) {
    if (!is.null(frame$ofile)) {
      as.character(frame$ofile)[1]
    } else {
      NA_character_
    }
  },
  character(1)
)

script_sources <- script_sources[!is.na(script_sources)]

common_candidates <- c(
  file.path("paper1_cluster", "scripts", "00_common.R"),
  file.path("scripts", "00_common.R"),
  "00_common.R",
  file.path(dirname(script_sources), "00_common.R")
)

common_path <- common_candidates[
  file.exists(common_candidates)
][1]

if (is.na(common_path)) {
  stop(
    "No se encuentra 00_common.R junto al script ",
    "o desde el directorio de trabajo."
  )
}

source(common_path)

# library(tidyverse)  # Cargada desde 00_common.R.
# library(readxl)     # Cargada desde 00_common.R.
# library(janitor)    # Cargada desde 00_common.R.


# CONFIGURACIÓN
# RANDOM_SEED está definido en 00_common.R.
set.seed(RANDOM_SEED)

# data_root y processed_root están definidos en 00_common.R.
raw_root <- file.path(data_root, "raw")

paths <- list(
  rv_decision = file.path(raw_root,"Content_Export_RV-Decision_956.xlsx"),
  rv_concerns = file.path(raw_root,"Content_Export_RV-Concerns_2__166.xlsx"),
  rv_energy = file.path(raw_root,"Content_Export_RV-Energy_Crisis_157.xlsx"),
  rv_poverty = file.path(raw_root,"Content_Export_RV-Poverty__96.xlsx"),
  why = file.path(raw_root,"Content_Export_Investment_Arquetypes_2022_full-latin-final.csv"),
  diego = file.path(raw_root,"Survey_September2023_cleaned.xlsx"),
  prolific_dir = file.path(raw_root, "prolific")
)

out_dir <- file.path(processed_root, "01_mergeData")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Parámetros específicos de la transformación de DIEGO
DIEGO_SCALE <- "1_5"
ADD_DIEGO_NOISE <- TRUE
DIEGO_NOISE_SD <- 1
USE_DIEGO_NOISY_INTEGRATED <- TRUE


# FUNCIONES AUXILIARES


# Sustituye los nombres de columna vacios o NA por nombres identificables
# y hace únicos los nombres repetidos para evitar perder columnas al importar.
# x: vector de nombres de columnas.
# Devuelve: vector de nombres únicos.
quiet_unique_names <- function(x) {
  x[is.na(x) | x == ""] <- paste0(
    "unnamed_",
    which(is.na(x) | x == "")
  )
  make.unique(x, sep = "...")
}


# Normaliza identificadores antes de agrupar registros o cruzar encuestas:
# convierte a minúsculas, elimina espacios y caracteres no alfanuméricos,
# y representa los identificadores ausentes como NA.
# x: vector de identificadores originales.
# Devuelve: vector de caracteres con los identificadores normalizados.
normalise_id <- function(x) {
  x <- str_trim(
    str_to_lower(as.character(x))
  )
  
  x <- na_if(x, "")
  x <- na_if(x, "na")
  x <- na_if(x, "nan")
  x <- str_replace_all(x, "[^a-z0-9]", "")
  
  na_if(x, "")
}


# Extrae el primer número de valores que pueden llegar como texto.
# Interpreta el punto como separador decimal y suprime avisos de conversión.
# x: vector de valores originales.
# Devuelve: vector numérico, con NA cuando no puede extraer un número.
# parse_num <- function(x) {
#   suppressWarnings(
#     parse_number(
#       as.character(x),
#       locale = locale(decimal_mark = ".")
#     )
#   )
# }


# Selecciona el primer valor informado de varios registros de un mismo
# participante, omitiendo NA, textos vacíos y las cadenas "NA" y "NaN".
# x: vector de valores correspondientes a un grupo.
# Devuelve: primer valor disponible como carácter o NA_character_.
first_non_missing <- function(x) {
  x <- x[
    !is.na(x) &
      x != "" &
      x != "NA" &
      x != "NaN"
  ]
  
  if (!length(x)) {
    return(NA_character_)
  }
  
  as.character(x[[1]])
}


# Reúne las respuestas distintas no vacías de un mismo participante.
# Elimina duplicados y espacios adicionales y separa las respuestas
# diferentes mediante " | " para conservar la información de origen.
# x: vector de respuestas correspondientes a un grupo.
# Devuelve: una cadena con las respuestas únicas o NA_character_.
# collapse_unique_non_missing <- function(x) {
#   x <- unique(
#     str_squish(as.character(x))
#   )
#   
#   x <- x[
#     !is.na(x) &
#       x != "" &
#       x != "NA" &
#       x != "NaN"
#   ]
#   
#   if (!length(x)) {
#     return(NA_character_)
#   }
#   
#   if (length(x) == 1) {
#     return(x)
#   }
#   
#   paste(x, collapse = " | ")
# }


# Recupera el nombre de la primera columna que coincide con un patrón.
# Utiliza find_cols(), definida en 00_common.R.
#
# df: tabla de entrada.
# pattern: expresión regular de búsqueda.
#
# Devuelve: nombre de la primera columna coincidente o NA_character_.
get_first_matching_col <- function(df, pattern) {
  out <- find_cols(df, pattern)[1]
  
  if (is.na(out)) {
    return(NA_character_)
  }
  
  out
}


# Recupera una columna de una tabla sin producir un error cuando no existe.
# Si falta la columna, genera un NA por cada fila de la tabla.
# df: tabla de datos.
# col: nombre de la columna que se desea recuperar.
# Devuelve: vector con los valores de la columna o un vector de NA.
# safe_pull <- function(df, col) {
#   if (is.na(col) || !col %in% names(df)) {
#     rep(NA_character_, nrow(df))
#   } else {
#     df[[col]]
#   }
# }


# Conserva todas las respuestas originales de una encuesta.
# Normaliza los nombres de columna, añade un prefijo que identifica
# su procedencia y transforma los valores a carácter para facilitar
# la integración de fuentes con tipos de datos diferentes.
# df: tabla original de la encuesta.
# prefix: prefijo que identifica la fuente o encuesta.
# Devuelve: tibble con las columnas originales renombradas.
as_prefixed_character_df <- function(df, prefix) {
  df %>%
    clean_names() %>%
    rename_with(
      ~ paste0(prefix, "__", .x)
    ) %>%
    mutate(
      across(everything(), as.character)
    )
}


# IDENTIFICACIÓN DE PAÍSES

# Diccionario de nombres de países y sus códigos de dos letras.
# Se utiliza para convertir respuestas de texto a códigos de país.

# Diccionario común de países definido en 00_common.R.
# country_name_to_iso2 <- c(
#   "spain" = "ES",
#   "espana" = "ES",
#   "españa" = "ES",
#   "germany" = "DE",
#   "alemania" = "DE",
#   "italy" = "IT",
#   "italia" = "IT",
#   "greece" = "GR",
#   "grecia" = "GR",
#   "the netherlands" = "NL",
#   "netherlands" = "NL",
#   "nederland" = "NL",
#   "belgium" = "BE",
#   "belgica" = "BE",
#   "bélgica" = "BE",
#   "france" = "FR",
#   "portugal" = "PT",
#   "bulgaria" = "BG",
#   "lithuania" = "LT",
#   "czechia" = "CZ",
#   "czech republic" = "CZ",
#   "denmark" = "DK",
#   "sweden" = "SE",
#   "finland" = "FI",
#   "poland" = "PL",
#   "romania" = "RO",
#   "hungary" = "HU",
#   "ireland" = "IE",
#   "austria" = "AT",
#   "croatia" = "HR",
#   "slovenia" = "SI",
#   "slovakia" = "SK",
#   "estonia" = "EE",
#   "latvia" = "LV",
#   "luxembourg" = "LU",
#   "malta" = "MT",
#   "cyprus" = "CY",
#   "united kingdom" = "GB",
#   "uk" = "GB",
#   "great britain" = "GB",
#   "ukraine" = "UA",
#   "switzerland" = "CH",
#   "serbia" = "RS",
#   "albania" = "AL",
#   "moldova" = "MD",
#   
#   "argentina" = "AR",
#   "bolivia" = "BO",
#   "bolivia, plurinational state of" = "BO",
#   "brazil" = "BR",
#   "brasil" = "BR",
#   "chile" = "CL",
#   "colombia" = "CO",
#   "costa rica" = "CR",
#   "cuba" = "CU",
#   "dominican republic" = "DO",
#   "república dominicana" = "DO",
#   "ecuador" = "EC",
#   "el salvador" = "SV",
#   "guatemala" = "GT",
#   "honduras" = "HN",
#   "mexico" = "MX",
#   "méxico" = "MX",
#   "nicaragua" = "NI",
#   "panama" = "PA",
#   "panamá" = "PA",
#   "paraguay" = "PY",
#   "peru" = "PE",
#   "perú" = "PE",
#   "uruguay" = "UY",
#   "venezuela" = "VE",
#   "venezuela, bolivarian republic of" = "VE",
#   
#   "united states" = "US",
#   "canada" = "CA",
#   "india" = "IN",
#   "turkey" = "TR",
#   "russian federation" = "RU",
#   "nigeria" = "NG",
#   "china" = "CN",
#   "indonesia" = "ID",
#   "south africa" = "ZA",
#   "syrian arab republic" = "SY",
#   "vietnam" = "VN",
#   "zimbabwe" = "ZW",
#   "australia" = "AU",
#   "bangladesh" = "BD",
#   "ghana" = "GH",
#   "iran" = "IR",
#   "kazakhstan" = "KZ",
#   "lebanon" = "LB",
#   "malaysia" = "MY",
#   "morocco" = "MA",
#   "pakistan" = "PK",
#   "armenia" = "AM",
#   "cameroon" = "CM",
#   "egypt" = "EG",
#   "haiti" = "HT",
#   "hong kong" = "HK",
#   "kyrgyzstan" = "KG",
#   "philippines" = "PH",
#   "saudi arabia" = "SA",
#   "tanzania" = "TZ",
#   "tunisia" = "TN",
#   
#   "consent_revoked" = NA_character_,
#   "data_expired" = NA_character_
# )


# Códigos de países considerados europeos en la separación de WHY.
europe_iso2 <- c(
  "AL", "AD", "AT", "BE", "BA", "BG", "HR", "CY", "CZ", "DK",
  "EE", "FI", "FR", "DE", "GR", "HU", "IS", "IE", "IT", "XK",
  "LV", "LI", "LT", "LU", "MT", "MD", "MC", "ME", "NL", "MK",
  "NO", "PL", "PT", "RO", "SM", "RS", "SK", "SI", "ES", "SE",
  "CH", "UA", "GB", "UK", "VA"
)


# # Códigos de países considerados latinoamericanos en la separación de WHY.
# latam_iso2 <- c(
#   "AR", "BO", "BR", "CL", "CO", "CR", "CU", "DO",
#   "EC", "SV", "GT", "HN", "MX", "NI", "PA", "PY",
#   "PE", "UY", "VE"
# )


# Convierte respuestas de país en sus códigos correspondientes.
# Prepara el texto y utiliza extract_country_code_base() de 00_common.R.
#
# x: vector de respuestas originales de país.
# Devuelve: vector de códigos de país o NA_character_.
extract_country_code <- function(x) {
  x_chr <- str_squish(as.character(x))
  x_low <- str_to_lower(x_chr)
  
  extract_country_code_base(x_chr, x_low)
}


# Comprueba si cada código de país pertenece al conjunto europe_iso2.
# Conserva NA cuando no se dispone del código de país.
# country_code: vector de códigos de país.
# Devuelve: TRUE para países incluidos en europe_iso2, FALSE para los
# demás países y NA cuando falta el código.
is_europe_country <- function(country_code) {
  ifelse(
    is.na(country_code),
    NA,
    country_code %in% europe_iso2
  )
}


# LECTURA DE ARCHIVOS EXCEL RENOVISOR

# Lee una exportación Excel de RENOVISOR
# Utiliza la hoja "Content" cuando existe; en caso contrario, la primera.
# Omite las filas iniciales indicadas y repara los nombres de las columnas.
# path: ruta del archivo Excel.
# skip: número de filas iniciales que se omiten; por defecto, tres.
# Devuelve: tibble con los datos importados.
read_excel_content <- function(path, skip = 3) {
  sheets <- excel_sheets(path)
  
  read_excel(
    path,
    sheet = if ("Content" %in% sheets) {
      "Content"
    } else {
      sheets[1]
    },
    skip = skip,
    .name_repair = quiet_unique_names
  ) %>%
    as_tibble()
}


# RENOVISOR: LECTURA Y AGRUPACIÓN POR PARTICIPANTE

# Importa una encuesta RENOVISOR completa sin descartar sus columnas.
# Añade metadatos de procedencia, número de fila, identificadores
# normalizados y un prefijo a las columnas de respuestas originales.
# Utiliza Prolific ID como identificador preferente; si falta, utiliza
# identification_code y, si también falta, genera un ID por fila.
# path: ruta del archivo Excel.
# survey_name: nombre utilizado para identificar la encuesta de origen.
# Devuelve: tibble con una fila por registro original, sin agrupar todavía.
read_rv_survey_full <- function(path, survey_name) {
  raw <- read_excel_content(path)
  
  raw_payload <- as_prefixed_character_df(
    raw,
    paste0("rv_", survey_name)
  )
  
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
      paste0(
        "rv_noid_",
        survey_name,
        "_",
        seq_len(nrow(raw))
      )
    ),
    
    year_birth_raw = as.character(raw[[5]]),
    gender_raw = as.character(raw[[6]]),
    country_raw = as.character(raw[[7]]),
    country_code = extract_country_code(country_raw)
  )
  
  bind_cols(
    meta,
    raw_payload
  )
}


# Archivos RENOVISOR y etiquetas utilizadas para identificar cada encuesta.
rv_files <- c(
  decision = paths$rv_decision,
  concerns2 = paths$rv_concerns,
  energy_crisis = paths$rv_energy,
  poverty = paths$rv_poverty
)


# Importar todas las filas de las cuatro encuestas RENOVISOR.
rv_all_rows <- imap_dfr(
  rv_files,
  read_rv_survey_full
)


# Agrupar los registros RENOVISOR por participante.
# Se conservan el primer valor informado de los metadatos y todas las
# respuestas distintas de las columnas originales.
rv_by_participant <- rv_all_rows %>%
  group_by(participant_key) %>%
  summarise(
    dataset_source = "rv",
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
    
    identification_code = first_non_missing(
      identification_code
    ),
    
    year_birth_raw = first_non_missing(year_birth_raw),
    gender_raw = first_non_missing(gender_raw),
    country_raw = first_non_missing(country_raw),
    country_code = first_non_missing(country_code),
    
    across(
      -any_of(
        c(
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
        )
      ),
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
  relocate(
    global_participant_key,
    .before = 1
  )


# PROLIFIC: LECTURA Y AGRUPACIÓN POR PARTICIPANTE

# Lee un archivo de exportación de Prolific.
# Localiza las columnas relevantes aunque sus nombres varíen, normaliza
# los identificadores y extrae datos de residencia y perfil demográfico.
# Conserva el nombre del archivo para mantener la trazabilidad.
# path: ruta de un archivo prolific_export_*.csv.
# Devuelve: tibble con las variables seleccionadas por registro.
read_prolific_file <- function(path) {
  raw <- read_csv(
    path,
    show_col_types = FALSE,
    name_repair = quiet_unique_names,
    col_types = cols(.default = col_character())
  ) %>%
    clean_names()
  
  participant_col <- get_first_matching_col(
    raw,
    "^participant_id$|prolific"
  )
  
  status_col <- get_first_matching_col(
    raw,
    "^status$"
  )
  
  age_col <- get_first_matching_col(
    raw,
    "^age$"
  )
  
  sex_col <- get_first_matching_col(
    raw,
    "^sex$|gender"
  )
  
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
  
  country_residence_raw <- as.character(
    safe_pull(raw, country_residence_col)
  )
  
  country_birth_raw <- as.character(
    safe_pull(raw, country_birth_col)
  )
  
  tibble(
    prolific_id = normalise_id(
      safe_pull(raw, participant_col)
    ),
    
    prolific_status = as.character(
      safe_pull(raw, status_col)
    ),
    
    prolific_country_raw = country_residence_raw,
    
    prolific_country_code = extract_country_code(
      country_residence_raw
    ),
    
    prolific_country_of_birth = country_birth_raw,
    
    prolific_country_of_birth_code = extract_country_code(
      country_birth_raw
    ),
    
    prolific_age = suppressWarnings(
      as.integer(
        parse_num(
          safe_pull(raw, age_col)
        )
      )
    ),
    
    prolific_sex = as.character(
      safe_pull(raw, sex_col)
    ),
    
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


# Identificar los archivos Prolific disponibles.
# Si no existe el directorio, se continúa con un conjunto vacío.
prolific_files <- if (dir.exists(paths$prolific_dir)) {
  list.files(
    paths$prolific_dir,
    pattern = "^prolific_export_.*\\.csv$",
    full.names = TRUE
  )
} else {
  character(0)
}


# Consolidar los registros Prolific por participante.
# Cuando hay varios registros con el mismo Prolific ID, se conserva
# el primer valor informado de cada variable y se registran los archivos
# en los que aparece ese participante.
prolific_pool <- if (length(prolific_files)) {
  map_dfr(
    prolific_files,
    read_prolific_file
  ) %>%
    filter(!is.na(prolific_id)) %>%
    group_by(prolific_id) %>%
    summarise(
      prolific_status = first_non_missing(
        prolific_status
      ),
      
      prolific_country_raw = first_non_missing(
        prolific_country_raw
      ),
      
      prolific_country_code = first_non_missing(
        prolific_country_code
      ),
      
      prolific_country_of_birth = first_non_missing(
        prolific_country_of_birth
      ),
      
      prolific_country_of_birth_code = first_non_missing(
        prolific_country_of_birth_code
      ),
      
      prolific_age = first_non_missing(
        prolific_age
      ),
      
      prolific_sex = first_non_missing(
        prolific_sex
      ),
      
      prolific_ethnicity_simplified = first_non_missing(
        prolific_ethnicity_simplified
      ),
      
      prolific_nationality = first_non_missing(
        prolific_nationality
      ),
      
      prolific_language = first_non_missing(
        prolific_language
      ),
      
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


# WHY: LECTURA Y ASIGNACIÓN DE REGIÓN

# Importa la encuesta WHY completa y conserva sus columnas originales.
# Incorpora los metadatos Prolific mediante prolific_id y determina el
# país de residencia y la pertenencia a Europa.
# Para determinar el país, se prioriza la residencia registrada en
# Prolific frente a la declarada en la encuesta cuando está disponible.
# path: ruta del CSV original de WHY.
# prolific_pool: tabla consolidada con los datos de Prolific.
# Devuelve: tibble con las filas originales WHY y los metadatos añadidos.
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
    mutate(
      across(everything(), as.character)
    ) %>%
    rename_with(
      ~ paste0("why__", .x)
    )
  
  meta <- tibble(
    dataset_source = "why",
    source_survey = "Investment_Arquetypes_2022",
    source_file = basename(path),
    source_row = seq_len(nrow(raw_clean)),
    
    prolific_id = normalise_id(
      raw_clean[[prolific_col]]
    ),
    
    participant_key = coalesce(
      prolific_id,
      paste0(
        "why_noid_",
        seq_len(nrow(raw_clean))
      )
    ),
    
    country_raw_survey = if (
      country_col %in% names(raw_clean)
    ) {
      as.character(raw_clean[[country_col]])
    } else {
      NA_character_
    },
    
    country_code_survey = extract_country_code(
      country_raw_survey
    ),
    
    language_raw = if (
      language_col %in% names(raw_clean)
    ) {
      as.character(raw_clean[[language_col]])
    } else {
      NA_character_
    }
  )
  
  bind_cols(
    meta,
    raw_payload
  ) %>%
    left_join(
      prolific_pool,
      by = "prolific_id"
    ) %>%
    mutate(
      # La residencia de Prolific tiene prioridad sobre el país
      # declarado en la encuesta WHY cuando está disponible.
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
      
      is_europe = is_europe_country(
        country_code
      )
    )
}


# Leer WHY y separar las filas según la región asignada.
why_all_rows <- read_why_full(
  paths$why,
  prolific_pool
)

why_europe_rows <- why_all_rows %>%
  filter(is_europe == TRUE)

why_latam_rows <- why_all_rows %>%
  filter(country_code %in% latam_iso2)


# WHY: AGRUPACIÓN POR PARTICIPANTE

# Consolida las filas WHY correspondientes a un mismo participante.
# Conserva los metadatos principales y combina las respuestas distintas
# de las columnas originales, manteniendo la información de procedencia.
# data: tabla WHY previamente filtrada por región.
# Devuelve: tibble con una fila por participant_key y un identificador
# global compuesto por fuente y participante.
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
        -any_of(
          c(
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
          )
        ),
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
    relocate(
      global_participant_key,
      .before = 1
    )
}


# Agrupar por separado los participantes WHY de Europa y Latinoamérica.
why_europe_by_participant <- collapse_why_participants(
  why_europe_rows
)

why_latam_by_participant <- collapse_why_participants(
  why_latam_rows
)


# DIEGO: IDENTIFICACIÓN DE DETERMINANTES

# Identifica las 32 columnas correspondientes a los determinantes DIEGO:
# ocho preguntas de cada uno de los bloques Q21, Q22, Q23 y Q24.
# Comprueba cuántas columnas están disponibles y muestra una advertencia
# si no encuentra las 32 esperadas.
# df_clean: tabla DIEGO con los nombres de columna normalizados.
# Devuelve: vector con los nombres de las columnas encontradas,
# respetando el orden esperado de los determinantes.
get_diego_det_cols <- function(df_clean) {
  expected <- unlist(
    map(
      21:24,
      ~ paste0("q", .x, "_", 1:8)
    )
  )
  
  found <- expected[
    expected %in% names(df_clean)
  ]
  
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


# DIEGO: RUIDO GAUSSIANO TRUNCADO

# Añade ruido gaussiano independiente a cada valor DIEGO.
# Si el valor perturbado queda fuera del intervalo permitido, vuelve
# a generar su ruido hasta que entre en el intervalo o se alcance
# max_iter. Al terminar, limita cualquier valor restante a los bordes.
# x: vector de valores originales.
# sd: desviación estándar del ruido gaussiano.
# min_value y max_value: límites permitidos para los valores perturbados.
# max_iter: número máximo de intentos adicionales de remuestreo.
# Devuelve: vector de valores perturbados dentro del intervalo,
# conservando los NA de entrada.
add_truncated_gaussian_noise <- function(
    x,
    sd = 1,
    min_value = 0,
    max_value = 5,
    max_iter = 100
) {
  x <- as.numeric(x)
  
  noise <- rnorm(
    length(x),
    mean = 0,
    sd = sd
  )
  
  bad <- !is.na(x) &
    (
      (x + noise) < min_value |
        (x + noise) > max_value
    )
  
  iter <- 1
  
  while (any(bad) && iter <= max_iter) {
    noise[bad] <- rnorm(
      sum(bad),
      mean = 0,
      sd = sd
    )
    
    bad <- !is.na(x) &
      (
        (x + noise) < min_value |
          (x + noise) > max_value
      )
    
    iter <- iter + 1
  }
  
  pmin(
    max_value,
    pmax(min_value, x + noise)
  )
}


# DIEGO: TRANSFORMACIÓN DE ESCALA

# Transforma los determinantes DIEGO a una escala común de 0 a 100.
# Si la escala original es 0-5, multiplica los valores por 20.
# Si la escala original es 1-5, aplica (x - 1) / 4 * 100.
# x: vector numérico con los valores originales.
# scale_type: escala de entrada, "0_5" o "1_5".
# Devuelve: vector numérico acotado entre 0 y 100, conservando los NA.
transform_to_0_100 <- function(
    x,
    scale_type = "1_5"
) {
  x <- as.numeric(x)
  
  if (scale_type == "0_5") {
    out <- x * 20
  } else if (scale_type == "1_5") {
    out <- ((x - 1) / 4) * 100
  } else {
    stop(
      "Escala de Diego no reconocida. ",
      "Usa '0_5' o '1_5'."
    )
  }
  
  pmin(
    100,
    pmax(0, out)
  )
}


# DIEGO: LECTURA Y GENERACIÓN DE LAS DOS VERSIONES

# Importa el Excel DIEGO y conserva sus columnas originales.
# Identifica los determinantes Q21-Q24, los transforma a la escala 0-100
# y, cuando se solicita, añade ruido gaussiano antes de transformar.
# También crea los metadatos de procedencia e identifica la escala
# y los parámetros de ruido utilizados en cada versión.
# path: ruta del archivo Excel DIEGO.
# diego_scale: escala original de los determinantes, "0_5" o "1_5".
# add_noise: TRUE para aplicar ruido; FALSE para conservar los valores.
# noise_sd: desviación estándar del ruido cuando add_noise es TRUE.
# Devuelve: tibble con las columnas originales, los metadatos
# y los determinantes transformados a 0-100.
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
  
  det_cols <- get_diego_det_cols(
    raw_clean
  )
  
  raw_payload <- raw_clean %>%
    mutate(
      across(everything(), as.character)
    ) %>%
    rename_with(
      ~ paste0("diego__", .x)
    )
  
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
    
    country_code = extract_country_code(
      country_raw
    ),
    
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
    mutate(
      across(everything(), parse_num)
    )
  
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
      sprintf(
        "%02d",
        seq_along(det_cols)
      ),
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
    relocate(
      global_participant_key,
      .before = 1
    )
}


# Generar y guardar las dos versiones de DIEGO.
# La primera conserva los determinantes sin ruido.
diego_transformed <- read_diego_transformed(
  paths$diego,
  diego_scale = DIEGO_SCALE,
  add_noise = FALSE,
  noise_sd = DIEGO_NOISE_SD
)

# La segunda permite añadir el ruido definido en la configuración.
diego_transformed_noisy <- read_diego_transformed(
  paths$diego,
  diego_scale = DIEGO_SCALE,
  add_noise = ADD_DIEGO_NOISE,
  noise_sd = DIEGO_NOISE_SD
)


# INTEGRACIÓN DE LAS CUATRO SUBMUESTRAS

# Elegir la versión de DIEGO que se incorporará a la base integrada.
# Ambas versiones se guardan por separado, pero solo una se integra.
diego_for_integrated <- if (
  USE_DIEGO_NOISY_INTEGRATED
) {
  diego_transformed_noisy
} else {
  diego_transformed
}


# Asignar a cada fuente su etiqueta de submuestra y región.
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


# Unir las cuatro submuestras conservando sus columnas originales.
# Crear un identificador de fila global y otro dentro de cada submuestra.
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


# COMPROBACIÓN DE IDENTIFICADORES DUPLICADOS

# Comprobar si un identificador global aparece en más de una fila
# de la base integrada. Mostrar una advertencia si se detectan duplicados.
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


# DIAGNÓSTICOS DE INTEGRACIÓN

# Contar las filas integradas por fuente de datos.
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


# Contar los participantes por submuestra y región.
# Añadir POOLED_ALL como total descriptivo de todas las filas.
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


# Contar el número de filas originales de cada encuesta RENOVISOR.
rv_diagnostics <- rv_all_rows %>%
  count(
    source_survey,
    name = "n_rows"
  ) %>%
  arrange(source_survey)


# Resumir las filas WHY según el país identificado, su pertenencia
# a Europa y la fuente utilizada para determinar el país.
why_country_diagnostics <- why_all_rows %>%
  count(
    country_code,
    is_europe,
    country_source,
    name = "n_rows"
  ) %>%
  arrange(
    desc(n_rows)
  )


# Resumir los participantes WHY por región y país.
why_region_diagnostics <- bind_rows(
  why_europe_by_participant %>%
    mutate(
      region = "WHY_EUROPE"
    ),
  
  why_latam_by_participant %>%
    mutate(
      region = "WHY_LATAM"
    )
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


# Registrar la configuración aplicada a DIEGO y comprobar cuántas
# columnas de determinantes transformados contiene cada versión.
diego_diagnostics <- tibble(
  diego_scale_assumption = DIEGO_SCALE,
  diego_noise_version_saved = TRUE,
  diego_noise_sd = DIEGO_NOISE_SD,
  
  diego_noisy_used_in_integrated = USE_DIEGO_NOISY_INTEGRATED,
  
  n_rows_clean = nrow(
    diego_transformed
  ),
  
  n_rows_noisy = nrow(
    diego_transformed_noisy
  ),
  
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


# GUARDADO DE ARCHIVOS

# Asociar cada tabla generada con el nombre de su archivo de salida.
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


# Escribir los CSV en el directorio de salida definido para 01_mergeData.
iwalk(
  outputs,
  ~ write_csv(
    .x,
    file.path(out_dir, .y)
  )
)


# RESUMEN EN CONSOLA
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
  file.path(
    out_dir,
    "all_sources_integrated.csv"
  )
)