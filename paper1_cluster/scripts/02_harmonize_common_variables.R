# 01_1_harmonize_sociodemographics.R
#
# OBJETIVO
# Armonizar las variables sociodemográficas, de contexto, participación
# electoral y los 32 determinantes de RENOVISOR, WHY y DIEGO.
#
# El script genera una versión limpia para los análisis y otra versión
# completa que conserva las columnas originales para trazabilidad.
#
# ENTRADA
# paper1_cluster/data/processed/01_mergeData/all_sources_integrated.csv
#
# Contiene las cuatro submuestras:
# - DIEGO
# - RENOVISOR
# - WHY_EUROPE
# - WHY_LATAM
#
# PROCESAMIENTO
# 1. Identificar las columnas equivalentes de las distintas encuestas.
# 2. Consolidar las respuestas correspondientes a una misma variable.
# 3. Armonizar las variables sociodemográficas y de contexto.
# 4. Armonizar las variables de participación electoral y autoubicación
#    política declaradas en las encuestas.
# 5. Transformar los determinantes de RV, WHY y DIEGO en 32 columnas
#    comunes, conservando sus valores en la escala 0-100.
# 6. Calcular la cobertura, los conflictos y los rangos de los valores.
# 7. Crear los archivos de análisis y los diagnósticos por fuente y
#    por submuestra.
#
# IMPORTANTE
# WHY_EUROPE y WHY_LATAM utilizan el mismo mapeo de determinantes porque
# ambas proceden de WHY. La columna subsample permite analizarlas
# posteriormente por separado.
#
# En este script NO se imputan los determinantes ausentes.
# La imputación de NA a 50 se realiza posteriormente en 03_1.
#
# SALIDAS
# Directorio:
# paper1_cluster/data/processed/02_harmonize_sociodemographics/
#
# - all_sources_integrated_clean.csv:
#   Base integrada con identificadores y variables armonizadas.
#
# - all_sources_integrated_clean_traceability.csv:
#   Base integrada completa, con las columnas originales y las nuevas
#   variables armonizadas.
#
# - sociodemographics_clean.csv:
#   Identificadores y variables sociodemográficas, políticas y de contexto.
#
# - determinants_harmonized.csv:
#   Identificadores, 32 determinantes y medidas de cobertura.
#
# - sociodemographic_source_columns.csv:
#   Columnas originales consideradas para construir cada variable común.
#
# - determinant_dictionary_32.csv:
#   Correspondencia de los 32 determinantes entre RV, WHY y DIEGO.
#
# - diagnostics_sociodemographics_coverage.csv:
#   Disponibilidad de las variables armonizadas por fuente y total.
#
# - diagnostics_sociodemographics_counts.csv:
#   Frecuencias y proporciones de las categorías por fuente.
#
# - diagnostics_sociodemographics_coverage_by_subsample.csv:
#   Disponibilidad de las variables armonizadas por submuestra.
#
# - diagnostics_sociodemographics_counts_by_subsample.csv:
#   Frecuencias y proporciones de las categorías por submuestra.
#
# - diagnostics_sociodemographics_conflicts.csv:
#   Número total de conflictos detectados por variable.
#
# - diagnostics_sociodemographics_conflicts_by_subsample.csv:
#   Número de conflictos por variable y submuestra.
#
# - diagnostics_32det_by_source.csv:
#   Cobertura de los 32 determinantes por fuente de datos.
#
# - diagnostics_32det_by_subsample.csv:
#   Cobertura de los 32 determinantes por submuestra.
#
# - diagnostics_32det_ranges.csv:
#   Valores mínimos, máximos y fuera de rango por determinante y fuente.
#
# - diagnostics_32det_ranges_by_subsample.csv:
#   Valores mínimos, máximos y fuera de rango por determinante y submuestra.
#
# - sociodemographic_dictionary.csv:
#   Diccionario descriptivo de las principales variables armonizadas.
#
# COLUMNAS PRINCIPALES GENERADAS
#
# *_raw_clean:
#   Respuestas de origen consolidadas, antes de aplicar su recodificación.
#
# reference_year_model:
#   Año de referencia utilizado para calcular la edad a partir del año
#   de nacimiento: RENOVISOR = 2026, WHY = 2022 y DIEGO = 2023.
#
# year_birth_model:
#   Año de nacimiento armonizado.
#
# age_model:
#   Edad numérica declarada directamente o calculada a partir del año
#   de nacimiento y del año de referencia de la encuesta.
#   Cuando únicamente se conoce un intervalo de edad, se conserva NA.
#
# age_model_is_approximate:
#   TRUE cuando age_model se calcula a partir del año de nacimiento
#   y del año de referencia de la encuesta.
#   FALSE cuando se dispone de una edad numérica directa o age_model es NA.
#
# age_group_model:
#   Grupo de edad armonizado: 18_39, 40_59, 60_plus o unknown.
#   Los intervalos originales de DIEGO se agrupan sin atribuir
#   una edad numérica individual a los participantes.
#
# gender_model, education_model, employment_model, income_model, etc.:
#   Versiones armonizadas de las variables sociodemográficas.
#
# country_model:
#   País de residencia armonizado mediante su código de dos letras.
#
# country_model_grouped:
#   Agrupación de países; los países con menos de MIN_COUNTRY_N registros
#   se agrupan en OTHER_COUNTRIES.
#
# country_region_model:
#   Región geográfica amplia del país de residencia.
#
# vote_status_declared:
#   Participación electoral declarada: voter, abstainer, uncertain,
#   unknown o conflict.
#
# voted_observed:
#   Indicador binario derivado de la participación declarada:
#   1 = voter, 0 = abstainer y NA para las demás respuestas.
#
# political_left_right_model:
#   Autoubicación política declarada, expresada numéricamente.
#
# political_block_model:
#   Categoría obtenida a partir de la autoubicación declarada.
#
# self_classification_model:
#   Autoclasificación declarada en relación con decisiones de inversión.
#
# *_conflict:
#   TRUE cuando se han encontrado respuestas distintas para una variable
#   y se han conservado juntas mediante el separador " | ".
#
# det_01_profits ... det_32_own_significance:
#   Los 32 determinantes armonizados entre las tres fuentes originales.
#
# n_det_non_missing:
#   Número de determinantes no ausentes antes de la imputación.
#
# prop_det_non_missing:
#   Proporción de determinantes no ausentes sobre los 32 disponibles.
#
# DEPENDENCIAS
# - 00_common.R: librerías, rutas y funciones comunes del proyecto.
# - 01_mergeData.R: crea el archivo de entrada.
# - 03_1_component_quality.R: utiliza posteriormente los datos armonizados.

################################################################################

# CARGA DE 00_common.R
# Buscar el archivo común junto a este script o desde el directorio
# de trabajo, sin utilizar rutas absolutas específicas de un ordenador.

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
  file.path(dirname(script_sources), "00_common.R"),
  file.path("paper1_cluster", "scripts", "00_common.R"),
  file.path("scripts", "00_common.R"),
  "00_common.R"
)

common_path <- common_candidates[
  file.exists(common_candidates)
][1]

if (is.na(common_path)) {
  stop("No se encuentra 00_common.R.")
}

source(common_path)


# CONFIGURACIÓN

# Año utilizado cuando no se reconoce la fuente de datos.
REFERENCE_YEAR_DEFAULT <- 2026L

# Mínimo de registros exigido para mantener un país como categoría
# independiente en country_model_grouped.
MIN_COUNTRY_N <- 10L

# processed_root se define en 00_common.R.
# processed_root <- "paper1_cluster/data/processed"

# Se prioriza la salida actual de 01_mergeData.R.
# Se conserva la ruta antigua como alternativa para compatibilidad.

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
    in_file_new,
    "\nni en:\n",
    in_file_old
  )
}

out_dir <- file.path(
  processed_root,
  "02_harmonize_sociodemographics"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# LECTURA Y COMPROBACIÓN DEL ARCHIVO DE ENTRADA

# Todas las columnas se leen inicialmente como caracteres para evitar
# que diferentes formatos de las encuestas alteren la importación.

all_sources_integrated <- read_csv(
  in_file,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

df <- all_sources_integrated

# Comprobar que se conservan los identificadores creados en 01.
required_id_cols <- c(
  "integrated_row_id",
  "subsample",
  "comparison_region",
  "subsample_row_id",
  "dataset_source"
)

missing_required_id_cols <- setdiff(
  required_id_cols,
  names(df)
)

if (length(missing_required_id_cols)) {
  stop(
    "Faltan columnas creadas en 01_mergeData.R: ",
    paste(missing_required_id_cols, collapse = ", ")
  )
}

# Comprobar que se han recibido las cuatro submuestras esperadas.
# Las submuestras esperadas se definen en 00_common.R.
# expected_subsamples <- c(
#   "DIEGO",
#   "RENOVISOR",
#   "WHY_EUROPE",
#   "WHY_LATAM"
# )

# missing_subsamples <- setdiff(
#   expected_subsamples,
#   unique(df$subsample)
# )

# if (length(missing_subsamples)) {
#   stop(
#     "Faltan submuestras en all_sources_integrated.csv: ",
#     paste(missing_subsamples, collapse = ", ")
#   )
# }

check_expected_subsamples(df)

if (anyDuplicated(df$integrated_row_id)) {
  stop("integrated_row_id contiene duplicados.")
}

print(
  df %>%
    count(comparison_region, subsample, name = "n")
)


# AÑO DE REFERENCIA POR FUENTE

# Se utiliza el año correspondiente a cada encuesta para calcular
# la edad cuando únicamente se conoce el año de nacimiento.

reference_year_model_vec <- case_when(
  df$dataset_source == "rv" ~ 2026L,
  df$dataset_source == "why" ~ 2022L,
  df$dataset_source == "diego" ~ 2023L,
  TRUE ~ REFERENCE_YEAR_DEFAULT
)


# FUNCIONES AUXILIARES

# Limpia espacios y convierte respuestas vacías o códigos de ausencia en NA.
# x: vector de respuestas originales.
# Devuelve: vector de caracteres depurado.
clean_text <- function(x) {
  x <- str_squish(as.character(x))
  
  invalid <- c(
    "",
    "NA",
    "NaN",
    "NULL",
    "null",
    "None",
    "none",
    "DATA_EXPIRED",
    "data_expired",
    "Prefer not to say",
    "Prefer not to answer"
  )
  
  x[x %in% invalid] <- NA_character_
  x
}


# Normaliza respuestas de texto para comparar categorías procedentes
# de encuestas e idiomas diferentes.
# Convierte a minúsculas, elimina tildes y puntuación y ajusta espacios.
# x: vector de respuestas originales.
# Devuelve: vector de caracteres normalizados.
normalise_text <- function(x) {
  x <- str_to_lower(clean_text(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- str_replace_all(x, "[[:punct:]]+", " ")
  str_squish(x)
}


# Extrae números de las respuestas después de limpiar sus valores ausentes.
# Utiliza el punto como separador decimal y la coma como separador de millares.
# x: vector de respuestas.
# Devuelve: vector numérico, con NA cuando no puede extraerse un número.
# Limpia las respuestas originales y extrae sus valores numéricos.
# Utiliza parse_num(), definida en 00_common.R.
#
# x: vector de respuestas originales.
# Devuelve: vector numérico o NA_real_ si no puede extraerse un número.

parse_num_clean <- function(x) {
  parse_num(clean_text(x))
}


# Identifica respuestas distintas que se han conservado juntas mediante
# el separador " | " durante la integración de las encuestas.
# x: vector de respuestas.
# Devuelve: vector lógico; TRUE indica un conflicto entre respuestas.
flag_conflict <- function(x) {
  x <- clean_text(x)
  !is.na(x) & str_detect(x, "\\s\\|\\s")
}


# Convierte respuestas a números, pero asigna NA_real_ cuando detecta
# varias respuestas diferentes para una misma variable.
# x: vector de respuestas originales.
# Devuelve: vector numérico sin valores procedentes de conflictos.
parse_num_no_conflict <- function(x) {
  out <- parse_num_clean(x)
  out[flag_conflict(x)] <- NA_real_
  out
}


# # find_cols() está definida en 00_common.R.
# Busca las columnas cuyos nombres coincidan con una expresión regular.
# Admite excluir columnas que coincidan con un segundo patrón.
# df: tabla de entrada.
# pattern: expresión regular de búsqueda.
# exclude: expresión regular opcional para excluir nombres.
# Devuelve: vector de nombres de columnas únicos.
# find_cols <- function(df, pattern, exclude = NULL) {
#   out <- names(df)[
#     str_detect(
#       names(df),
#       regex(pattern, ignore_case = TRUE)
#     )
#   ]
#   
#   if (!is.null(exclude)) {
#     out <- out[
#       !str_detect(
#         out,
#         regex(exclude, ignore_case = TRUE)
#       )
#     ]
#   }
#   
#   unique(out)
# }


# Consolida, fila por fila, las columnas que representan una misma variable.
# Primero limpia las respuestas originales y después utiliza la función
# compartida collapse_unique_non_missing() definida en 00_common.R.
#
# df: tabla con las columnas originales.
# cols: nombres de las columnas que deben consolidarse.
# Devuelve: vector de caracteres; utiliza " | " cuando existen
# respuestas distintas y NA cuando no se dispone de ninguna respuesta.
collapse_many_unique <- function(df, cols) {
  cols <- unique(cols[cols %in% names(df)])
  
  if (!length(cols)) {
    return(rep(NA_character_, nrow(df)))
  }
  
  tmp <- df[, cols, drop = FALSE] %>%
    mutate(across(everything(), clean_text))
  
  # collapse_unique_non_missing() está definida en 00_common.R.
  apply(tmp, 1, collapse_unique_non_missing)
}


# Comprueba si una respuesta armonizada puede considerarse informada.
# Para valores numéricos exige que no sean NA.
# Para caracteres excluye NA, vacío y categorías de ausencia o conflicto.
# x: vector de valores armonizados.
# Devuelve: vector lógico que identifica los valores disponibles.
is_valid_model_value <- function(x) {
  if (is.numeric(x) || is.integer(x)) {
    return(!is.na(x))
  }
  
  !is.na(x) &
    x != "" &
    !x %in% c(
      "unknown",
      "UNKNOWN",
      "conflict",
      "other",
      "OTHER"
    )
}


# La conversión numérica utilizada para los determinantes era parse_det().
# Su lógica está centralizada en parse_num(), definida en 00_common.R.

# parse_det <- function(x) {
#   suppressWarnings(
#     parse_number(
#       as.character(x),
#       locale = locale(decimal_mark = ".", grouping_mark = ",")
#     )
#   )
# }


# Recupera una columna numérica sin producir un error cuando falta.
# Utiliza safe_pull() y parse_num(), ambas definidas en 00_common.R.
# df: tabla de datos.
# col: nombre de la columna o NA si no se encontró.
# Devuelve: vector numérico, con NA_real_ cuando falta la columna.
safe_numeric_col <- function(df, col) {
  parse_num(safe_pull(df, col))
}


# Localiza la columna de un determinante a partir de su prefijo.
# Excluye columnas de país, idioma e identificador que podrían coincidir
# accidentalmente con el prefijo.
# df: tabla de datos.
# prefix: prefijo esperado.
# Devuelve: nombre de la columna, NA si falta o un error si existen
# varias coincidencias y no es posible identificar una columna única.
resolve_unique_prefix <- function(df, prefix) {
  matches <- names(df)[str_starts(names(df), prefix)]
  
  matches <- matches[
    !str_detect(
      matches,
      "which_country_do_you_reside|languages|prolific_id"
    )
  ]
  
  if (!length(matches)) {
    return(NA_character_)
  }
  
  if (length(matches) > 1) {
    stop(
      "Prefijo ambiguo: ",
      prefix,
      "\nCoincidencias:\n",
      paste(matches, collapse = "\n")
    )
  }
  
  matches
}


# Normaliza una respuesta de texto y registra si está ausente
# o si contiene varias respuestas distintas.
# x: vector de respuestas.
# Devuelve: texto normalizado, "unknown" para ausencias
# o "conflict" cuando se detectan respuestas incompatibles.
normalised_or_status <- function(x) {
  case_when(
    is.na(x) ~ "unknown",
    flag_conflict(x) ~ "conflict",
    TRUE ~ normalise_text(x)
  )
}


# IDENTIFICACIÓN DE PAÍSES

# El diccionario unificado se define en 00_common.R.
# Se conserva aquí la definición original de este script, comentada,
# para documentar qué equivalencias utilizaba antes de centralizarlo
# country_name_to_iso2 <- c(
#   "spain" = "ES", "espana" = "ES",
#   "germany" = "DE", "alemania" = "DE",
#   "italy" = "IT", "italia" = "IT",
#   "greece" = "GR", "grecia" = "GR",
#   "the netherlands" = "NL",
#   "netherlands" = "NL",
#   "nederland" = "NL",
#   "belgium" = "BE",
#   "belgica" = "BE",
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
#   "rumania" = "RO",
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
#   "moldavia" = "MD",
#   "norway" = "NO",
#   "noruega" = "NO",
#   "iceland" = "IS",
#   "islandia" = "IS",
#   "liechtenstein" = "LI",
#   "monaco" = "MC",
#   "andorra" = "AD",
#   "bosnia and herzegovina" = "BA",
#   "bosnia y herzegovina" = "BA",
#   "bosnia" = "BA",
#   "montenegro" = "ME",
#   "north macedonia" = "MK",
#   "macedonia del norte" = "MK",
#   "san marino" = "SM",
#   "belarus" = "BY",
#   "bielorrusia" = "BY",
#   "republica checa" = "CZ",
#   "other eu country" = "OTHER_EU",
#   "other eu countries" = "OTHER_EU",
#   "other european country" = "OTHER_EU",
#   "other european countries" = "OTHER_EU",
#
#   "argentina" = "AR",
#   "bolivia" = "BO",
#   "bolivia plurinational state of" = "BO",
#   "brazil" = "BR",
#   "brasil" = "BR",
#   "chile" = "CL",
#   "colombia" = "CO",
#   "costa rica" = "CR",
#   "cuba" = "CU",
#   "dominican republic" = "DO",
#   "republica dominicana" = "DO",
#   "ecuador" = "EC",
#   "el salvador" = "SV",
#   "guatemala" = "GT",
#   "honduras" = "HN",
#   "mexico" = "MX",
#   "nicaragua" = "NI",
#   "panama" = "PA",
#   "paraguay" = "PY",
#   "peru" = "PE",
#   "uruguay" = "UY",
#   "venezuela" = "VE",
#   "venezuela bolivarian republic of" = "VE",
#
#   "united states" = "US",
#   "united states of america" = "US",
#   "canada" = "CA",
#   "india" = "IN",
#   "turkey" = "TR",
#   "russian federation" = "RU",
#   "russia" = "RU",
#   "nigeria" = "NG",
#   "china" = "CN",
#   "indonesia" = "ID",
#   "south africa" = "ZA",
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
#   "tunisia" = "TN"
# )


# Convierte una respuesta individual de país en su código.
# Aplica la limpieza específica de la armonización y utiliza
# extract_country_code_base() de 00_common.R.
#
# x: vector de respuestas originales de país.
# Devuelve: vector de códigos de país o NA_character_.
extract_country_code_one <- function(x) {
  x_chr <- clean_text(x)
  x_low <- normalise_text(x_chr)
  
  extract_country_code_base(x_chr, x_low)
}


# Resuelve respuestas de país que pueden contener varios valores
# separados por " | ".
# Convierte cada respuesta a un código y conserva los códigos diferentes.
# x: vector de respuestas de país.
# Devuelve: un código, varios códigos separados por " | " o NA.
extract_country_code_multi <- function(x) {
  map_chr(clean_text(x), function(value) {
    if (is.na(value)) {
      return(NA_character_)
    }
    
    parts <- str_split(
      value,
      "\\s\\|\\s"
    )[[1]]
    
    codes <- unique(
      extract_country_code_one(parts)
    )
    
    codes <- codes[
      !is.na(codes) &
        codes != ""
    ]
    
    if (!length(codes)) {
      return(NA_character_)
    }
    
    if (length(codes) == 1) {
      return(codes[[1]])
    }
    
    paste(codes, collapse = " | ")
  })
}


# El conjunto de países latinoamericanos está definido en 00_common.R.
# Se conserva comentada la definición original de este script.
# latam_iso2 <- c(
#   "AR", "BO", "BR", "CL", "CO", "CR", "CU", "DO",
#   "EC", "SV", "GT", "HN", "MX", "NI", "PA", "PY",
#   "PE", "UY", "VE"
# )


# IDENTIFICADORES Y COLUMNAS FUENTE

# Identificadores y metadatos que se conservarán en las salidas limpias.
id_cols <- c(
  "integrated_row_id",
  "subsample",
  "comparison_region",
  "subsample_row_id",
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


# Especificar qué columnas de las diferentes encuestas pueden representar
# una misma variable armonizada.
# find_cols() permite identificar las columnas aunque sus nombres varíen.
# Las columnas se consolidarán posteriormente con collapse_many_unique().
source_cols <- list(
  year_birth = unique(c(
    "year_birth_raw",
    find_cols(
      df,
      "please_enter_your_year_of_birth|year_of_birth|birth_year"
    )
  )),
  
  age = unique(c(
    "age_raw",
    "prolific_age",
    "diego__q1",
    find_cols(
      df,
      "^diego__q1$|^prolific_age$|^age$|(^|__)age($|_)|age_group|age_bracket",
      exclude = "stage|message|image|language|languages"
    )
  )),
  
  gender = unique(c(
    "gender_raw",
    "prolific_sex",
    "prolific_gender",
    "diego__q2",
    find_cols(
      df,
      "what_is_your_gender|gender|sex|^diego__q2$|prolific.*sex|prolific.*gender"
    )
  )),
  
  country_raw = unique(c(
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
  )),
  
  country_code = unique(c(
    "country_code",
    "country_code_survey",
    "prolific_country_code",
    "prolific_country_residence_code"
  )),
  
  country_birth = unique(c(
    "prolific_country_of_birth",
    "prolific_country_birth",
    find_cols(
      df,
      "country_of_birth|country_birth|birth_country|prolific.*country.*birth",
      exclude = "code|source"
    )
  )),
  
  nationality = unique(c(
    "prolific_nationality",
    find_cols(
      df,
      "nationality|prolific.*nationality",
      exclude = "code|source"
    )
  )),
  
  ethnicity = unique(c(
    "prolific_ethnicity_simplified",
    "prolific_ethnicity",
    find_cols(
      df,
      "ethnicity_simplified|ethnicity|prolific.*ethnicity",
      exclude = "code|source"
    )
  )),
  
  language = unique(c(
    "language_raw",
    "prolific_language",
    find_cols(
      df,
      "^why__languages$|^languages$|language|prolific.*language"
    )
  )),
  
  student_status = unique(c(
    "prolific_student_status",
    find_cols(
      df,
      "student_status|student status|prolific.*student",
      exclude = "source|date|time"
    )
  )),
  
  city_size = find_cols(
    df,
    "approximate_population_size|population_size_of_the_city|population_size|city_size|size_of_the_city"
  ),
  
  climate_zone = find_cols(
    df,
    "climate_zone"
  ),
  
  household_type = find_cols(
    df,
    "type_of_household|household_type|household_do_you_live"
  ),
  
  tenure = find_cols(
    df,
    "tenure_status|current_tenure|housing_tenure"
  ),
  
  education = unique(c(
    "diego__q4",
    find_cols(
      df,
      "highest_level_of_education|education|prolific.*education",
      exclude = "energy|efficiency|goal|det_|support|oppose|concern|barrier"
    )
  )),
  
  employment = unique(c(
    "diego__q5",
    "prolific_employment_status",
    find_cols(
      df,
      "employment_status|current_employment|contractual_status|labour|labor|prolific.*employment",
      exclude = "source|sources|income|capital|property|pension|joy|risk|concern|barrier|energy|support|oppose|det_"
    )
  )),
  
  num_children = unique(c(
    "diego__q6",
    find_cols(
      df,
      "number_of_children|num_children|children|hijos|^diego__q6$",
      exclude = "energy|concern|barrier|support|oppose|det_"
    )
  )),
  
  health_condition = find_cols(
    df,
    "health_condition|functional_limitation|requires_assistance"
  ),
  
  income = unique(c(
    "diego__q7",
    find_cols(
      df,
      "household_income|monthly_income|annual_income|net_income|gross_income|monthly_net|net_monthly|^diego__q7$",
      exclude = "source|sources|type|capital|property|pension|employment|self_employment|joy|risk|concern|barrier|poverty|energy|support|oppose|feel|feeling|doing|thinking|actions|saved|saving|savings|salary_is_saved|typical_month|det_"
    )
  )),
  
  travel_distance = find_cols(
    df,
    "total_distance_you_travel|distance_you_travel"
  ),
  
  travel_time = find_cols(
    df,
    "total_time_do_you_spend_travelling|time_do_you_spend_travelling"
  ),
  
  work_from_home = find_cols(
    df,
    "work_or_study_from_home|working_from_home"
  ),
  
  travel_role = find_cols(
    df,
    "usual_travel_role|travel_role"
  ),
  
  vote_status = find_cols(
    df,
    "general_approach_to_voting|approach_to_voting|voting_in_elections|abstain|abstention|turnout"
  ),
  
  political_left_right = find_cols(
    df,
    "most_left_and_100_means_most_right|place_yourself_politically|left.*right|political"
  ),
  
  self_classification = find_cols(
    df,
    "which_statement_best_describes_you_when_making_an_investment_decision"
  ),
  
  energy_efficiency_goal = find_cols(
    df,
    "energy_efficiency_goal"
  ),
  
  climate_awareness = find_cols(
    df,
    "climate_change_does_not_exist|awareness_of_climate_change"
  ),
  
  energy_transition_awareness = find_cols(
    df,
    "awareness_about_the_energy_transition|energy_transition"
  ),
  
  renovation_role = find_cols(
    df,
    "role_or_situation_regarding_household_renovation_decisions|renovation_decisions"
  ),
  
  household_decision = find_cols(
    df,
    "in_your_household_how_are_decisions_usually_made|decisions_usually_made"
  )
)


# Registrar qué columnas originales se han seleccionado para cada variable.
# El archivo resultante permite revisar el mapeo de forma independiente.
sociodemographic_source_columns <- tibble(
  harmonised_variable = names(source_cols),
  
  source_columns = map_chr(
    source_cols,
    ~ paste(.x, collapse = " | ")
  ),
  
  n_source_columns = map_int(
    source_cols,
    length
  )
)


# CONSOLIDACIÓN DE LAS VARIABLES ORIGINALES

# Cada variable de source_cols se procesa de forma independiente.
# Si esta operación supone un coste elevado, sus iteraciones pueden
# estudiarse para ejecutarlas en paralelo.

## PARALIZE
raw_clean <- map(
  source_cols,
  ~ collapse_many_unique(df, .x)
)


# Nombres que recibirán las variables originales una vez consolidadas.
# El sufijo raw_clean indica que todavía no se han recodificado.
raw_output_names <- c(
  year_birth = "year_birth_raw_clean",
  age = "age_raw_clean",
  gender = "gender_raw_clean",
  country_raw = "country_raw_clean",
  country_code = "country_code_raw_clean",
  country_birth = "country_birth_raw_clean",
  nationality = "nationality_raw_clean",
  ethnicity = "ethnicity_raw_clean",
  language = "language_raw_clean",
  student_status = "student_status_raw_clean",
  city_size = "city_size_raw_clean",
  climate_zone = "climate_zone_raw_clean",
  household_type = "household_type_raw_clean",
  tenure = "tenure_raw_clean",
  education = "education_raw_clean",
  employment = "employment_raw_clean",
  num_children = "num_children_raw_clean",
  health_condition = "health_raw_clean",
  income = "income_raw_clean",
  travel_distance = "travel_distance_raw_clean",
  travel_time = "travel_time_raw_clean",
  work_from_home = "work_from_home_raw_clean",
  travel_role = "travel_role_raw_clean",
  vote_status = "vote_raw_clean",
  political_left_right = "political_left_right_raw_clean",
  self_classification = "self_classification_raw_clean",
  energy_efficiency_goal = "energy_efficiency_goal_raw_clean",
  climate_awareness = "climate_awareness_raw_clean",
  energy_transition_awareness = "energy_transition_awareness_raw_clean",
  renovation_role = "renovation_role_raw_clean",
  household_decision = "household_decision_raw_clean"
)

raw_df <- as_tibble(
  setNames(
    raw_clean,
    unname(raw_output_names)
  )
)


# FUNCIONES DE RECODIFICACIÓN

# Armoniza la edad en tres grupos compatibles con los intervalos originales
# de DIEGO y con las edades numéricas de las otras encuestas.
#
# x: respuesta original de edad, que puede ser un intervalo o una edad exacta.
# numeric_age: edad numérica armonizada, cuando está disponible.
#
# Devuelve: "18_39", "40_59", "60_plus" o "unknown".
# Las respuestas con varios valores incompatibles no se clasifican.
recode_age_group_from_raw <- function(x, numeric_age = NULL) {
  x_low <- normalise_text(x)
  
  n <- if (is.null(numeric_age)) {
    rep(NA_real_, length(x_low))
  } else {
    as.numeric(numeric_age)
  }
  
  case_when(
    flag_conflict(x) ~ "unknown",
    
    str_detect(
      x_low,
      "^(18\\s+25|26\\s+39)$"
    ) ~ "18_39",
    
    str_detect(
      x_low,
      "^40\\s+59$"
    ) ~ "40_59",
    
    str_detect(
      x_low,
      "^60\\s+or\\s+older$"
    ) |
      str_detect(
        clean_text(x),
        "^60\\s*\\+$"
      ) ~ "60_plus",
    
    !is.na(n) &
      n >= 18 &
      n < 40 ~ "18_39",
    
    !is.na(n) &
      n >= 40 &
      n < 60 ~ "40_59",
    
    !is.na(n) &
      n >= 60 ~ "60_plus",
    
    TRUE ~ "unknown"
  )
}


# Identifica las respuestas de edad expresadas mediante los intervalos
# originales de DIEGO, sin atribuirles una edad numérica.
#
# x: vector de respuestas originales de edad.
# Devuelve: TRUE cuando la respuesta es un intervalo y FALSE en otro caso.
is_age_interval <- function(x) {
  x_low <- normalise_text(x)
  
  !is.na(x_low) &
    (
      str_detect(
        x_low,
        "^(18\\s+25|26\\s+39|40\\s+59|60\\s+or\\s+older)$"
      ) |
        str_detect(
          clean_text(x),
          "^60\\s*\\+$"
        )
    )
}


# Armoniza las respuestas de género procedentes de las encuestas.
# Identifica respuestas femeninas, masculinas, otras categorías,
# respuestas ausentes y conflictos entre fuentes.
# x: vector de respuestas originales.
# Devuelve: female, male, other, unknown o conflict.
recode_gender <- function(x) {
  x_raw <- clean_text(x)
  x_low <- normalise_text(x_raw)
  
  case_when(
    is.na(x_low) ~ "unknown",
    
    str_detect(
      x_raw,
      "\\s\\|\\s"
    ) ~ "conflict",
    
    str_detect(
      x_low,
      "\\bfemale\\b|\\bwoman\\b|\\bwomen\\b|\\bmujer\\b|\\bfemen"
    ) ~ "female",
    
    str_detect(
      x_low,
      "\\bmale\\b|\\bman\\b|\\bmen\\b|\\bhombre\\b|\\bmascul"
    ) ~ "male",
    
    str_detect(
      x_low,
      "non binary|nonbinary|non binar|other|otro|otra|diverse|no binar"
    ) ~ "other",
    
    str_detect(
      x_low,
      "prefer not|no answer|dont know|do not know|ns nc"
    ) ~ "unknown",
    
    TRUE ~ "other"
  )
}


# Convierte las respuestas sobre idioma a códigos comunes.
# x: vector de idiomas declarados.
# Devuelve: EN, ES, EU, NL, FR, DE, IT, PL, GR, OTHER o unknown.
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


# Armoniza el nivel educativo mediante tres categorías generales.
# x: respuestas originales de educación.
# Devuelve: low, medium, high, other, unknown o conflict.
recode_education <- function(x) {
  x_raw <- clean_text(x)
  x_low <- normalise_text(x_raw)
  
  case_when(
    is.na(x_low) ~ "unknown",
    
    str_detect(x_raw, "\\s\\|\\s") ~ "conflict",
    
    str_detect(
      x_low,
      "phd|doctor|doctoral"
    ) ~ "high",
    
    str_detect(
      x_low,
      "master|msc|postgraduate|bachelor|undergraduate|degree|university|universit|tertiary|higher education|grado|licenciatura|diploma universitario"
    ) ~ "high",
    
    str_detect(
      x_low,
      "vocational|professional training|upper secondary|secondary|high school|bachiller|formacion profesional|\\bfp\\b|college"
    ) ~ "medium",
    
    str_detect(
      x_low,
      "primary|basic|lower secondary|no formal|less than|sin estudios|primaria"
    ) ~ "low",
    
    str_detect(
      x_low,
      "prefer not|no answer|dont know|do not know"
    ) ~ "unknown",
    
    TRUE ~ "other"
  )
}


# Armoniza la educación para los análisis del TFM.
# Agrupa los niveles universitarios y no universitarios por separado.
# x: respuestas originales de educación.
# Devuelve: university, non_university, other, unknown o conflict.
recode_education_tfm <- function(x) {
  x_raw <- clean_text(x)
  x_low <- normalise_text(x_raw)
  
  case_when(
    is.na(x_low) ~ "unknown",
    
    str_detect(x_raw, "\\s\\|\\s") ~ "conflict",
    
    str_detect(
      x_low,
      "phd|doctor|doctoral|master|msc|postgraduate|bachelor|undergraduate|degree|university|universit|tertiary|higher education|grado|licenciatura|diploma universitario"
    ) ~ "university",
    
    str_detect(
      x_low,
      "vocational|professional training|upper secondary|secondary|high school|bachiller|formacion profesional|\\bfp\\b|college|primary|basic|lower secondary|no formal|less than|sin estudios|primaria"
    ) ~ "non_university",
    
    str_detect(
      x_low,
      "prefer not|no answer|dont know|do not know"
    ) ~ "unknown",
    
    TRUE ~ "other"
  )
}


# Agrupa los países de residencia en regiones geográficas amplias.
# Utiliza latam_iso2, definido en 00_common.R, para Latinoamérica.
# country_code: vector de códigos de país.
# Devuelve: región geográfica, unknown, conflict u other_region.
recode_residence_region <- function(country_code) {
  country_code <- str_to_upper(
    clean_text(country_code)
  )
  
  case_when(
    is.na(country_code) |
      country_code == "" ~ "unknown",
    
    str_detect(
      country_code,
      "\\s\\|\\s"
    ) ~ "conflict",
    
    country_code %in% c(
      "DK", "EE", "FI", "IE", "IS",
      "LV", "LT", "NO", "GB", "SE"
    ) ~ "northern_europe",
    
    country_code %in% c(
      "DE", "AT", "BE", "FR", "LI",
      "LU", "MC", "NL", "CH"
    ) ~ "western_europe",
    
    country_code %in% c(
      "AL", "AD", "BA", "HR", "SI",
      "ES", "GR", "IT", "MT", "ME",
      "PT", "MK", "SM", "RS", "CY"
    ) ~ "southern_europe",
    
    country_code %in% c(
      "BY", "BG", "SK", "HU", "MD",
      "PL", "CZ", "RO", "RU", "UA"
    ) ~ "eastern_europe",
    
    country_code %in% latam_iso2 ~ "latin_america",
    
    TRUE ~ "other_region"
  )
}


# Armoniza las respuestas sobre situación laboral.
# x: respuestas originales de empleo.
# Devuelve: unemployed, self_employed, employed, student,
# inactive_other, retired, other, unknown o conflict.
recode_employment <- function(x) {
  x_raw <- clean_text(x)
  x_low <- normalise_text(x_raw)
  
  case_when(
    is.na(x_low) ~ "unknown",
    
    str_detect(x_raw, "\\s\\|\\s") ~ "conflict",
    
    str_detect(
      x_low,
      "unemployed|desemple|paro|job seeking|not working unemployed"
    ) ~ "unemployed",
    
    str_detect(
      x_low,
      "self employed|self employment|autonom|freelance"
    ) ~ "self_employed",
    
    str_detect(
      x_low,
      "due to start a new job|start a new job|new job within the next month"
    ) ~ "employed",
    
    str_detect(
      x_low,
      "full time|full-time|part time|part-time|paid employee|\\bemployee\\b|\\bemployed\\b|\\bworking\\b|trabaj|contrato"
    ) ~ "employed",
    
    str_detect(
      x_low,
      "student|estudiante"
    ) ~ "student",
    
    str_detect(
      x_low,
      "not in paid work|homemaker|disabled|unable to work|care|inactive"
    ) ~ "inactive_other",
    
    str_detect(
      x_low,
      "retired|jubil|pension"
    ) ~ "retired",
    
    str_detect(
      x_low,
      "prefer not|no answer|dont know|do not know"
    ) ~ "unknown",
    
    TRUE ~ "other"
  )
}


# Armoniza la condición de estudiante.
# x: respuestas originales.
# Devuelve: student, not_student, other, unknown o conflict.
recode_student_status <- function(x) {
  x_raw <- clean_text(x)
  x_low <- normalise_text(x_raw)
  
  case_when(
    is.na(x_low) ~ "unknown",
    
    str_detect(x_raw, "\\s\\|\\s") ~ "conflict",
    
    str_detect(
      x_low,
      "not student|not a student|\\bno\\b"
    ) ~ "not_student",
    
    str_detect(
      x_low,
      "\\byes\\b|student|estudiante|\\bsi\\b"
    ) ~ "student",
    
    TRUE ~ "other"
  )
}


# Armoniza el tamaño de la ciudad de residencia.
# Interpreta respuestas categóricas o valores numéricos de población.
# x: respuestas originales de tamaño de ciudad.
# Devuelve: rural_small, small_city, medium_city, large_city,
# metropolitan, other, unknown o conflict.
recode_city_size <- function(x) {
  x_raw <- clean_text(x)
  x_low <- normalise_text(x_raw)
  n <- parse_num_no_conflict(x_raw)
  
  case_when(
    is.na(x_low) ~ "unknown",
    
    str_detect(x_raw, "\\s\\|\\s") ~ "conflict",
    
    str_detect(
      x_low,
      "rural|village|small town"
    ) ~ "rural_small",
    
    str_detect(
      x_low,
      "less than|under|below|<"
    ) &
      !is.na(n) &
      n <= 10000 ~ "rural_small",
    
    str_detect(
      x_low,
      "less than|under|below|<"
    ) &
      !is.na(n) &
      n <= 50000 ~ "small_city",
    
    !is.na(n) &
      n < 10000 ~ "rural_small",
    
    !is.na(n) &
      n < 50000 ~ "small_city",
    
    !is.na(n) &
      n < 250000 ~ "medium_city",
    
    !is.na(n) &
      n < 1000000 ~ "large_city",
    
    !is.na(n) &
      n >= 1000000 ~ "metropolitan",
    
    str_detect(
      x_low,
      "more than|over|above|>"
    ) ~ "large_city",
    
    TRUE ~ "other"
  )
}


# Armoniza el régimen de tenencia de la vivienda.
# x: respuestas originales sobre propiedad, alquiler o convivencia.
# Devuelve: rent, owner_without_mortgage, owner_with_mortgage,
# owner, family_home, other, unknown o conflict.
recode_tenure <- function(x) {
  x_raw <- clean_text(x)
  x_low <- normalise_text(x_raw)
  
  case_when(
    is.na(x_low) ~ "unknown",
    
    str_detect(x_raw, "\\s\\|\\s") ~ "conflict",
    
    str_detect(
      x_low,
      "rent|tenant|alquil"
    ) ~ "rent",
    
    str_detect(
      x_low,
      "without mortgage|sin hipoteca|outright|fully paid"
    ) ~ "owner_without_mortgage",
    
    str_detect(
      x_low,
      "mortgage|hipoteca"
    ) ~ "owner_with_mortgage",
    
    str_detect(
      x_low,
      "own|owner|propiedad"
    ) ~ "owner",
    
    str_detect(
      x_low,
      "family|relative|parents"
    ) ~ "family_home",
    
    TRUE ~ "other"
  )
}


# Recodifica la tenencia de la vivienda en las tres categorías
# utilizadas específicamente en los análisis del TFM.
# x: respuestas originales de tenencia.
# Devuelve: Homeowner without mortgage, Homeowner with mortgage,
# Non-homeowner, unknown o conflict.
recode_tenure_tfm <- function(x) {
  x_raw <- clean_text(x)
  x_low <- normalise_text(x_raw)
  
  case_when(
    is.na(x_low) ~ "unknown",
    
    str_detect(x_raw, "\\s\\|\\s") ~ "conflict",
    
    str_detect(
      x_low,
      "own the home outright|fully paid-off|without mortgage|sin hipoteca"
    ) ~ "Homeowner without mortgage",
    
    str_detect(
      x_low,
      "mortgage|outstanding payments|hipoteca"
    ) ~ "Homeowner with mortgage",
    
    str_detect(
      x_low,
      "rent|rental|tenant|alquil|family|relative|parents"
    ) ~ "Non-homeowner",
    
    str_detect(
      x_low,
      "own|owner|propiedad"
    ) ~ "Homeowner without mortgage",
    
    TRUE ~ "Non-homeowner"
  )
}


# Armoniza respuestas afirmativas y negativas de distintos cuestionarios.
# x: respuestas originales.
# Devuelve: yes, no, other, unknown o conflict.
recode_yes_no <- function(x) {
  x_raw <- clean_text(x)
  x_low <- normalise_text(x_raw)
  
  case_when(
    is.na(x_low) ~ "unknown",
    
    str_detect(x_raw, "\\s\\|\\s") ~ "conflict",
    
    str_detect(
      x_low,
      "\\byes\\b|\\bsi\\b|true"
    ) ~ "yes",
    
    str_detect(
      x_low,
      "\\bno\\b|false|none"
    ) ~ "no",
    
    TRUE ~ "other"
  )
}


# Armoniza las respuestas sobre participación electoral declarada.
# x: respuestas originales.
# Devuelve: voter, abstainer, uncertain, unknown o conflict.
recode_vote_status <- function(x) {
  x_raw <- clean_text(x)
  x_low <- normalise_text(x_raw)
  
  case_when(
    is.na(x_low) ~ "unknown",
    
    str_detect(x_raw, "\\s\\|\\s") ~ "conflict",
    
    str_detect(
      x_low,
      "abstain|abstention|do not vote|dont vote|did not vote|not vote|never vote|usually do not vote|no voto|abstengo"
    ) ~ "abstainer",
    
    str_detect(
      x_low,
      "always vote|usually vote|i vote|voted|voter|participate|turn out|suelo votar|siempre voto"
    ) ~ "voter",
    
    str_detect(
      x_low,
      "sometimes|depends|not sure|uncertain|undecided|prefer not|no answer"
    ) ~ "uncertain",
    
    TRUE ~ "uncertain"
  )
}


# Agrupa la autoubicación política declarada en intervalos numéricos
# de la escala de 0 a 100 utilizada por este análisis.
# x: respuesta original de autoubicación.
# Devuelve: extreme_left, left, centre, right, extreme_right,
# unknown o invalid.
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


# Armoniza los ingresos declarados en las diferentes encuestas.
# Interpreta intervalos, cantidades numéricas y categorías textuales.
# x: respuestas originales de ingresos.
# Devuelve: low, medium, high, other, unknown o conflict.
recode_income <- function(x) {
  x_raw <- clean_text(x)
  x_low <- normalise_text(x_raw)
  n <- parse_num_no_conflict(x_raw)
  
  case_when(
    is.na(x_low) ~ "unknown",
    
    str_detect(x_raw, "\\s\\|\\s") ~ "conflict",
    
    str_detect(
      x_low,
      "prefer not|no answer|dont know|do not know"
    ) ~ "unknown",
    
    str_detect(
      x_low,
      "less than 20 000|less than 20000|under 20 000|under 20000"
    ) ~ "low",
    
    str_detect(
      x_low,
      "20 000.*49 999|20000.*49999"
    ) ~ "medium",
    
    str_detect(
      x_low,
      "50 000.*100 000|50000.*100000|more than 100 000|more than 100000|over 100 000|over 100000"
    ) ~ "high",
    
    !is.na(n) &
      n >= 10000 &
      n < 20000 ~ "low",
    
    !is.na(n) &
      n >= 20000 &
      n < 50000 ~ "medium",
    
    !is.na(n) &
      n >= 50000 ~ "high",
    
    !is.na(n) &
      n < 1000 ~ "low",
    
    !is.na(n) &
      n < 2500 ~ "medium",
    
    !is.na(n) &
      n >= 2500 ~ "high",
    
    str_detect(
      x_low,
      "low|bajo"
    ) ~ "low",
    
    str_detect(
      x_low,
      "middle|medium|medio"
    ) ~ "medium",
    
    str_detect(
      x_low,
      "high|alto"
    ) ~ "high",
    
    TRUE ~ "other"
  )
}


# Convierte a número la cantidad de hijos declarada.
# Reconoce también respuestas textuales que indican que no hay hijos.
# x: respuestas originales.
# Devuelve: número de hijos o NA_real_ cuando no puede determinarse.
parse_num_children <- function(x) {
  x_low <- normalise_text(x)
  n <- parse_num_no_conflict(x)
  
  case_when(
    is.na(x_low) ~ NA_real_,
    
    str_detect(
      x_low,
      "none|no children|sin hijos|ninguno"
    ) ~ 0,
    
    !is.na(n) ~ n,
    
    TRUE ~ NA_real_
  )
}


# Armoniza la autoclasificación declarada sobre decisiones de inversión.
# Identifica las categorías de arquetipo utilizadas en el análisis.
# x: respuestas originales de autoclasificación.
# Devuelve: activist, fearful, influencer, careful, uninterested,
# pioneer, sentient, homo_economicus, none, other o unknown.
recode_self_classification <- function(x) {
  x_low <- normalise_text(x)
  
  case_when(
    is.na(x_low) ~ "unknown",
    
    str_detect(
      x_low,
      "environment|climate|planet"
    ) ~ "activist",
    
    str_detect(
      x_low,
      "safety|safe|risk"
    ) ~ "fearful",
    
    str_detect(
      x_low,
      "status|recognition|show others"
    ) ~ "influencer",
    
    str_detect(
      x_low,
      "comfort|cozy|cosy|well being|wellbeing"
    ) ~ "careful",
    
    str_detect(
      x_low,
      "not interested|not very interested"
    ) ~ "uninterested",
    
    str_detect(
      x_low,
      "new|innovation|technology|early adopter"
    ) ~ "pioneer",
    
    str_detect(
      x_low,
      "ethical|meaning|values"
    ) ~ "sentient",
    
    str_detect(
      x_low,
      "cost|money|save|saving|economic"
    ) ~ "homo_economicus",
    
    str_detect(
      x_low,
      "none"
    ) ~ "none",
    
    TRUE ~ "other"
  )
}


# CONSTRUCCIÓN DE LAS VARIABLES ARMONIZADAS

# Convertir las respuestas originales de edad y año de nacimiento a números.
year_birth_num <- parse_num_no_conflict(
  raw_clean$year_birth
)

age_num_raw <- parse_num_no_conflict(
  raw_clean$age
)

# Identificar las respuestas que expresan intervalos y que, por tanto,
# no pueden utilizarse directamente como edades numéricas exactas.
age_is_interval_vec <- is_age_interval(
  raw_clean$age
)


# Identificar el año de nacimiento, utilizando un año válido declarado
# directamente o detectado dentro del campo de edad.
year_birth_model_vec <- case_when(
  !is.na(year_birth_num) &
    year_birth_num >= 1900 &
    year_birth_num <= reference_year_model_vec ~
    as.integer(year_birth_num),
  
  !is.na(age_num_raw) &
    age_num_raw >= 1900 &
    age_num_raw <= reference_year_model_vec ~
    as.integer(age_num_raw),
  
  TRUE ~ NA_integer_
)


# Recuperar únicamente edades numéricas válidas.
# Se excluyen las respuestas expresadas mediante intervalos.

age_direct_vec <- case_when(
  !is.na(age_num_raw) &
    age_num_raw >= 15 &
    age_num_raw <= 110 &
    !age_is_interval_vec &
    !flag_conflict(raw_clean$age) ~
    as.numeric(age_num_raw),
  
  !is.na(year_birth_num) &
    year_birth_num >= 15 &
    year_birth_num <= 110 ~
    as.numeric(year_birth_num),
  
  TRUE ~ NA_real_
)

# Utilizar la edad declarada directamente o calcularla a partir
# del año de nacimiento y del año de referencia de la encuesta.
#
# No se utilizan puntos medios de intervalos para generar edades
# numéricas individuales.
age_model_vec <- coalesce(
  age_direct_vec,
  
  ifelse(
    !is.na(year_birth_model_vec),
    reference_year_model_vec - year_birth_model_vec,
    NA_real_
  )
)

# TRUE cuando la edad numérica se ha estimado a partir del año
# de nacimiento y del año de referencia de la encuesta.
#
# FALSE cuando se dispone de una edad numérica directa o cuando
# age_model es NA. Los intervalos de DIEGO no generan edades
# numéricas aproximadas.

age_model_is_approximate_vec <-
  !is.na(age_model_vec) &
  is.na(age_direct_vec) &
  !is.na(year_birth_model_vec)

# Armonizar país de residencia y país de nacimiento.
country_code_from_raw <- extract_country_code_multi(
  raw_clean$country_raw
)

country_birth_model_vec <- extract_country_code_multi(
  raw_clean$country_birth
)

country_model_vec <- coalesce(
  extract_country_code_multi(
    raw_clean$country_code
  ),
  country_code_from_raw
)


# Convertir a valores numéricos las variables de contexto que lo permiten.
political_left_right_vec <- parse_num_no_conflict(
  raw_clean$political_left_right
)

energy_efficiency_goal_vec <- parse_num_no_conflict(
  raw_clean$energy_efficiency_goal
)

climate_awareness_vec <- parse_num_no_conflict(
  raw_clean$climate_awareness
)

energy_transition_awareness_vec <- parse_num_no_conflict(
  raw_clean$energy_transition_awareness
)

travel_distance_vec <- parse_num_no_conflict(
  raw_clean$travel_distance
)

travel_time_vec <- parse_num_no_conflict(
  raw_clean$travel_time
)

num_children_vec <- parse_num_children(
  raw_clean$num_children
)


# Incorporar las respuestas consolidadas y las variables armonizadas.
# Se conservan las columnas originales de all_sources_integrated.
all_sources_integrated_clean <- bind_cols(
  all_sources_integrated,
  raw_df
) %>%
  mutate(
    reference_year_model = reference_year_model_vec,
    
    year_birth_model = year_birth_model_vec,
    
    age_model = age_model_vec,
    
    age_model_is_approximate = age_model_is_approximate_vec,
    
    age_group_model = recode_age_group_from_raw(
      raw_clean$age,
      age_model_vec
    ),
    
    gender_model = recode_gender(
      raw_clean$gender
    ),
    
    education_model = recode_education(
      raw_clean$education
    ),
    
    education_tfm_model = recode_education_tfm(
      raw_clean$education
    ),
    
    employment_model = recode_employment(
      raw_clean$employment
    ),
    
    student_status_model = recode_student_status(
      raw_clean$student_status
    ),
    
    city_size_model = recode_city_size(
      raw_clean$city_size
    ),
    
    tenure_model = recode_tenure(
      raw_clean$tenure
    ),
    
    tenure_tfm_model = recode_tenure_tfm(
      raw_clean$tenure
    ),
    
    health_condition_model = recode_yes_no(
      raw_clean$health_condition
    ),
    
    work_from_home_model = recode_yes_no(
      raw_clean$work_from_home
    ),
    
    income_model = recode_income(
      raw_clean$income
    ),
    
    num_children_model = num_children_vec,
    
    country_model = country_model_vec,
    
    country_region_model = recode_residence_region(
      country_model_vec
    ),
    
    country_birth_model = country_birth_model_vec,
    
    nationality_model = normalised_or_status(
      raw_clean$nationality
    ),
    
    ethnicity_model = normalised_or_status(
      raw_clean$ethnicity
    ),
    
    language_model = recode_language(
      raw_clean$language
    ),
    
    climate_zone_model = normalised_or_status(
      raw_clean$climate_zone
    ),
    
    household_type_model = normalised_or_status(
      raw_clean$household_type
    ),
    
    travel_role_model = normalised_or_status(
      raw_clean$travel_role
    ),
    
    renovation_role_model = normalised_or_status(
      raw_clean$renovation_role
    ),
    
    household_decision_model = normalised_or_status(
      raw_clean$household_decision
    ),
    
    vote_status_declared = recode_vote_status(
      raw_clean$vote_status
    ),
    
    voted_observed = case_when(
      vote_status_declared == "voter" ~ 1L,
      vote_status_declared == "abstainer" ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    political_left_right_model = political_left_right_vec,
    
    political_block_model = recode_political_block(
      raw_clean$political_left_right
    ),
    
    self_classification_model = recode_self_classification(
      raw_clean$self_classification
    ),
    
    energy_efficiency_goal_0_100 = energy_efficiency_goal_vec,
    
    climate_awareness_0_100 = climate_awareness_vec,
    
    energy_transition_awareness_0_100 =
      energy_transition_awareness_vec,
    
    travel_distance_model = travel_distance_vec,
    
    travel_time_model = travel_time_vec,
    
    # Registrar por separado los conflictos encontrados en las
    # respuestas originales que se han utilizado para armonizar.
    year_birth_conflict = flag_conflict(
      raw_clean$year_birth
    ),
    
    age_conflict = flag_conflict(
      raw_clean$age
    ),
    
    gender_conflict = flag_conflict(
      raw_clean$gender
    ),
    
    education_conflict = flag_conflict(
      raw_clean$education
    ),
    
    employment_conflict = flag_conflict(
      raw_clean$employment
    ),
    
    country_conflict =
      flag_conflict(raw_clean$country_raw) |
      flag_conflict(country_model),
    
    country_birth_conflict =
      flag_conflict(raw_clean$country_birth) |
      flag_conflict(country_birth_model),
    
    nationality_conflict = flag_conflict(
      raw_clean$nationality
    ),
    
    ethnicity_conflict = flag_conflict(
      raw_clean$ethnicity
    ),
    
    student_status_conflict = flag_conflict(
      raw_clean$student_status
    ),
    
    income_conflict = flag_conflict(
      raw_clean$income
    ),
    
    vote_conflict = flag_conflict(
      raw_clean$vote_status
    )
  )


# AGRUPACIÓN DE PAÍSES SEGÚN SU FRECUENCIA

# Contar los registros correspondientes a cada país armonizado.
country_counts <- all_sources_integrated_clean %>%
  count(
    country_model,
    name = "n_country_model"
  )

# Los países que aparecen al menos MIN_COUNTRY_N veces conservan
# su categoría; los demás se agrupan como OTHER_COUNTRIES.
all_sources_integrated_clean <- all_sources_integrated_clean %>%
  left_join(
    country_counts,
    by = "country_model"
  ) %>%
  mutate(
    country_model_grouped = case_when(
      is.na(country_model) |
        country_model == "" ~ "UNKNOWN",
      
      str_detect(
        country_model,
        "\\s\\|\\s"
      ) ~ "CONFLICT",
      
      n_country_model >= MIN_COUNTRY_N ~ country_model,
      
      TRUE ~ "OTHER_COUNTRIES"
    )
  ) %>%
  select(-n_country_model)


# ARMONIZACIÓN DE LOS 32 DETERMINANTES

# Cada fila del diccionario indica:
# det_id: número del determinante.
# det_name: nombre común del determinante.
# rv_prefix: prefijo de la columna correspondiente en RENOVISOR.
# why_prefix: prefijo de la columna correspondiente en WHY.
# diego_col: columna correspondiente en DIEGO.
#
# Se mantienen exactamente las correspondencias definidas en el script
# original para evitar modificar la interpretación de los determinantes.
determinant_dictionary <- tribble(
  ~det_id, ~det_name, ~rv_prefix, ~why_prefix, ~diego_col,
  
  1, "profits",
  "rv_decision__profits_",
  "why__a_",
  "diego_det_01_0_100",
  
  2, "credit_score",
  "rv_decision__credit_score_",
  "why__b_",
  "diego_det_02_0_100",
  
  3, "risk_profile",
  "rv_decision__risk_profile_",
  "why__c_",
  "diego_det_03_0_100",
  
  4, "added_value",
  "rv_decision__added_value_",
  "why__d_",
  "diego_det_04_0_100",
  
  5, "frugality",
  "rv_decision__frugality_",
  "why__e_",
  "diego_det_05_0_100",
  
  6, "climate_protection",
  "rv_decision__climate_protection_",
  "why__f_",
  "diego_det_06_0_100",
  
  7, "legal",
  "rv_decision__legal_",
  "why__g_",
  "diego_det_07_0_100",
  
  8, "trust",
  "rv_decision__trust_",
  "why__h_",
  "diego_det_08_0_100",
  
  9, "safety",
  "rv_decision__safety_",
  "why__i_i",
  "diego_det_09_0_100",
  
  10, "cost_efficiency",
  "rv_decision__cost_efficiency_",
  "why__j_",
  "diego_det_10_0_100",
  
  11, "knowledge",
  "rv_decision__knowledge_",
  "why__k_",
  "diego_det_11_0_100",
  
  12, "own_competence",
  "rv_decision__own_competence_",
  "why__l_",
  "diego_det_12_0_100",
  
  13, "technical_fit",
  "rv_decision__technical_fit_",
  "why__m_",
  "diego_det_13_0_100",
  
  14, "environmental_concerns",
  "rv_decision__environmental_concerns_",
  "why__n_",
  "diego_det_14_0_100",
  
  15, "self_satisfaction",
  "rv_decision__self_satisfaction_",
  "why__o_",
  "diego_det_15_0_100",
  
  16, "commitment",
  "rv_decision__commitment_",
  "why__p_",
  "diego_det_16_0_100",
  
  17, "adherence",
  "rv_decision__adherence_",
  "why__q_",
  "diego_det_17_0_100",
  
  18, "autonomy",
  "rv_decision__autonomy_",
  "why__r_",
  "diego_det_18_0_100",
  
  19, "wellbeing",
  "rv_decision__wellbeing_",
  "why__s_",
  "diego_det_19_0_100",
  
  20, "coziness",
  "rv_decision__coziness_",
  "why__t_",
  "diego_det_20_0_100",
  
  21, "rights_and_duties",
  "rv_decision__rights_and_duties_",
  "why__u_",
  "diego_det_21_0_100",
  
  22, "peer_pressure",
  "rv_decision__peer_pressure_",
  "why__v_",
  "diego_det_22_0_100",
  
  23, "support",
  "rv_decision__support_",
  "why__w_",
  "diego_det_23_0_100",
  
  24, "socialising",
  "rv_decision__socialising_",
  "why__x_",
  "diego_det_24_0_100",
  
  25, "agreement",
  "rv_decision__agreement_",
  "why__y_",
  "diego_det_25_0_100",
  
  26, "novelty",
  "rv_decision__novelty_",
  "why__z_",
  "diego_det_26_0_100",
  
  27, "fun",
  "rv_decision__fun_",
  "why__aa_",
  "diego_det_27_0_100",
  
  28, "recognition",
  "rv_decision__recognition_",
  "why__ab_",
  "diego_det_28_0_100",
  
  29, "trends",
  "rv_decision__trends_",
  "why__ac_",
  "diego_det_29_0_100",
  
  30, "authority",
  "rv_decision__authority_",
  "why__ad_",
  "diego_det_30_0_100",
  
  31, "approval",
  "rv_decision__approval_",
  "why__ae_",
  "diego_det_31_0_100",
  
  32, "own_significance",
  "rv_decision__own_significance_",
  "why__af_",
  "diego_det_32_0_100"
) %>%
  mutate(
    # Nombre común de la columna armonizada.
    det_col = paste0(
      "det_",
      sprintf("%02d", det_id),
      "_",
      det_name
    ),
    
    # Localizar la columna real correspondiente en RENOVISOR.
    rv_col = map_chr(
      rv_prefix,
      ~ resolve_unique_prefix(df, .x)
    ),
    
    # Localizar la columna real correspondiente en WHY.
    why_col = map_chr(
      why_prefix,
      ~ resolve_unique_prefix(df, .x)
    ),
    
    # Comprobar que la columna de DIEGO existe.
    diego_col_resolved = if_else(
      diego_col %in% names(df),
      diego_col,
      NA_character_
    )
  )


# DETECCIÓN DE COLUMNAS DE DETERMINANTES AUSENTES
missing_mapping <- determinant_dictionary %>%
  filter(
    is.na(rv_col) |
      is.na(why_col) |
      is.na(diego_col_resolved)
  )

if (nrow(missing_mapping)) {
  print(
    missing_mapping,
    n = Inf,
    width = Inf
  )
  
  stop(
    "Faltan columnas para uno o más determinantes. ",
    "Revisa determinant_dictionary."
  )
}


# CREACIÓN DE LOS 32 DETERMINANTES COMUNES

# Para cada determinante, recuperar las columnas originales de las
# tres fuentes y seleccionar el valor correspondiente a dataset_source.
#
# La operación recorre únicamente 32 determinantes y utiliza funciones
# vectorizadas. No se marca para paralelizar sin comprobar previamente
# que representa un coste significativo de ejecución.
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
    all_sources_integrated_clean$dataset_source == "rv" ~
      rv_values,
    
    all_sources_integrated_clean$dataset_source == "why" ~
      why_values,
    
    all_sources_integrated_clean$dataset_source == "diego" ~
      diego_values,
    
    TRUE ~ NA_real_
  )
}

det_cols <- determinant_dictionary$det_col

if (length(det_cols) != N_DET_TOTAL) {
  stop(
    "El diccionario no contiene exactamente ",
    N_DET_TOTAL,
    " determinantes."
  )
}


# Calcular cuántos determinantes no ausentes tiene cada participante.
# Estos recuentos corresponden a los valores previos a la imputación.
all_sources_integrated_clean <- all_sources_integrated_clean %>%
  mutate(
    n_det_non_missing = rowSums(
      !is.na(
        across(all_of(det_cols))
      )
    ),
    
    prop_det_non_missing =
      n_det_non_missing / N_DET_TOTAL
  )


# CREACIÓN DE LOS DATASETS LIMPIOS Y DE TRAZABILIDAD

# Seleccionar las columnas sociodemográficas, políticas y de contexto
# que se conservarán en la base limpia.
sociodemographic_clean_cols <- c(
  "reference_year_model",
  
  unname(raw_output_names),
  
  "year_birth_model",
  "age_model",
  "age_model_is_approximate",
  "age_group_model",
  "gender_model",
  "education_model",
  "education_tfm_model",
  "employment_model",
  "student_status_model",
  "city_size_model",
  "tenure_model",
  "tenure_tfm_model",
  "health_condition_model",
  "work_from_home_model",
  "income_model",
  "num_children_model",
  "country_model",
  "country_model_grouped",
  "country_region_model",
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


# Colocar las variables armonizadas junto a los identificadores,
# conservando el resto de las columnas originales.
all_sources_integrated_clean <- all_sources_integrated_clean %>%
  relocate(
    any_of(
      c(
        sociodemographic_clean_cols,
        det_clean_cols
      )
    ),
    .after = any_of("identification_code")
  )


# Guardar la versión completa antes de seleccionar las columnas
# que formarán la versión reducida para análisis.
all_sources_integrated_clean_traceability <-
  all_sources_integrated_clean

analysis_clean_cols <- unique(
  c(
    id_cols,
    sociodemographic_clean_cols,
    det_clean_cols
  )
)

all_sources_integrated_clean <-
  all_sources_integrated_clean_traceability %>%
  select(
    any_of(analysis_clean_cols)
  )


# Crear dos tablas específicas para facilitar su uso posterior.
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


# DIAGNÓSTICOS DE COBERTURA SOCIODEMOGRÁFICA

# Variables cuya disponibilidad se comprobará por fuente y submuestra.
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


# Calcula el número de filas y de valores disponibles para cada variable,
# agrupando los registros según las columnas indicadas.
# data: tabla con las variables armonizadas.
# group_vars: nombres de las columnas por las que se agrupará.
# Devuelve: tibble con n_rows y una columna n_non_missing_* por variable.
coverage_summary <- function(data, group_vars) {
  data %>%
    group_by(
      across(all_of(group_vars))
    ) %>%
    summarise(
      n_rows = n(),
      
      across(
        all_of(coverage_vars),
        ~ sum(
          is_valid_model_value(.x),
          na.rm = TRUE
        ),
        .names = "n_non_missing_{.col}"
      ),
      
      .groups = "drop"
    )
}


# Cobertura sociodemográfica por fuente y total.
diagnostics_sociodemographics_coverage <- bind_rows(
  coverage_summary(
    sociodemographics_clean,
    "dataset_source"
  ),
  
  sociodemographics_clean %>%
    summarise(
      dataset_source = "TOTAL",
      n_rows = n(),
      
      across(
        all_of(coverage_vars),
        ~ sum(
          is_valid_model_value(.x),
          na.rm = TRUE
        ),
        .names = "n_non_missing_{.col}"
      )
    )
)


# Cobertura sociodemográfica por submuestra.
diagnostics_sociodemographics_coverage_by_subsample <-
  coverage_summary(
    sociodemographics_clean,
    c(
      "comparison_region",
      "subsample",
      "dataset_source"
    )
  ) %>%
  arrange(
    comparison_region,
    subsample
  )


# DIAGNÓSTICOS DE FRECUENCIAS SOCIODEMOGRÁFICAS

# Variables categóricas para las que se contarán las respuestas.
count_vars <- c(
  "age_group_model",
  "gender_model",
  "education_model",
  "employment_model",
  "student_status_model",
  "income_model",
  "country_model_grouped",
  "country_birth_model",
  "nationality_model",
  "language_model",
  "vote_status_declared",
  "political_block_model",
  "self_classification_model"
)


# Frecuencias y proporciones por fuente de datos.
diagnostics_sociodemographics_counts <- sociodemographics_clean %>%
  select(
    dataset_source,
    all_of(count_vars)
  ) %>%
  pivot_longer(
    cols = all_of(count_vars),
    names_to = "variable",
    values_to = "value"
  ) %>%
  count(
    dataset_source,
    variable,
    value,
    name = "n"
  ) %>%
  group_by(
    dataset_source,
    variable
  ) %>%
  mutate(
    prop = n / sum(n)
  ) %>%
  ungroup() %>%
  arrange(
    dataset_source,
    variable,
    desc(n)
  )


# Frecuencias y proporciones por submuestra.
diagnostics_sociodemographics_counts_by_subsample <-
  sociodemographics_clean %>%
  select(
    subsample,
    all_of(count_vars)
  ) %>%
  pivot_longer(
    cols = all_of(count_vars),
    names_to = "variable",
    values_to = "value"
  ) %>%
  count(
    subsample,
    variable,
    value,
    name = "n"
  ) %>%
  group_by(
    subsample,
    variable
  ) %>%
  mutate(
    prop = n / sum(n)
  ) %>%
  ungroup() %>%
  arrange(
    subsample,
    variable,
    desc(n)
  )


# DIAGNÓSTICOS DE CONFLICTOS

# Columnas que registran las respuestas incompatibles encontradas
# durante la consolidación de los datos originales.
conflict_cols <- c(
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


# Número total de conflictos por variable.
diagnostics_conflicts <- sociodemographics_clean %>%
  summarise(
    n_rows = n(),
    
    across(
      all_of(conflict_cols),
      ~ sum(.x, na.rm = TRUE),
      .names = "n_{.col}"
    )
  )


# Número de conflictos por región y submuestra.
diagnostics_conflicts_by_subsample <-
  sociodemographics_clean %>%
  group_by(
    comparison_region,
    subsample
  ) %>%
  summarise(
    n_rows = n(),
    
    across(
      all_of(conflict_cols),
      ~ sum(.x, na.rm = TRUE),
      .names = "n_{.col}"
    ),
    
    .groups = "drop"
  ) %>%
  arrange(
    comparison_region,
    subsample
  )


# DIAGNÓSTICOS DE COBERTURA DE LOS 32 DETERMINANTES

# Calcula cuántos participantes tienen al menos un determinante disponible,
# cuántos tienen los 32 completos y cuántos determinantes están informados
# por término medio.
# data: tabla que contiene n_det_non_missing.
# group_vars: nombres de las columnas por las que se agrupará.
# Devuelve: tibble con el tamaño de cada grupo y sus medidas de cobertura.
det_summary <- function(data, group_vars) {
  data %>%
    group_by(
      across(all_of(group_vars))
    ) %>%
    summarise(
      n_rows = n(),
      
      n_with_any_det = sum(
        n_det_non_missing > 0,
        na.rm = TRUE
      ),
      
      n_complete_32_det = sum(
        n_det_non_missing == N_DET_TOTAL,
        na.rm = TRUE
      ),
      
      mean_n_det_non_missing = mean(
        n_det_non_missing,
        na.rm = TRUE
      ),
      
      min_n_det_non_missing = min(
        n_det_non_missing,
        na.rm = TRUE
      ),
      
      max_n_det_non_missing = max(
        n_det_non_missing,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    )
}


# Cobertura de los determinantes por fuente y total.
diagnostics_32det_by_source <- bind_rows(
  det_summary(
    all_sources_integrated_clean,
    "dataset_source"
  ),
  
  all_sources_integrated_clean %>%
    summarise(
      dataset_source = "TOTAL",
      
      n_rows = n(),
      
      n_with_any_det = sum(
        n_det_non_missing > 0,
        na.rm = TRUE
      ),
      
      n_complete_32_det = sum(
        n_det_non_missing == N_DET_TOTAL,
        na.rm = TRUE
      ),
      
      mean_n_det_non_missing = mean(
        n_det_non_missing,
        na.rm = TRUE
      ),
      
      min_n_det_non_missing = min(
        n_det_non_missing,
        na.rm = TRUE
      ),
      
      max_n_det_non_missing = max(
        n_det_non_missing,
        na.rm = TRUE
      )
    )
)


# Cobertura de los determinantes por región y submuestra.
diagnostics_32det_by_subsample <- det_summary(
  all_sources_integrated_clean,
  c(
    "comparison_region",
    "subsample",
    "dataset_source"
  )
) %>%
  mutate(
    pct_complete_32_det =
      100 * n_complete_32_det / n_rows,
    .after = n_complete_32_det
  ) %>%
  arrange(
    comparison_region,
    subsample
  )


# DIAGNÓSTICOS DE RANGOS DE LOS DETERMINANTES

# Transformar los 32 determinantes a formato largo.
# Cada fila representa el valor de un determinante de un participante.
det_long <- all_sources_integrated_clean %>%
  select(
    dataset_source,
    subsample,
    comparison_region,
    all_of(det_cols)
  ) %>%
  pivot_longer(
    cols = all_of(det_cols),
    names_to = "det_col",
    values_to = "value"
  )


# Calcular mínimos, máximos y valores fuera de la escala 0-100
# para cada determinante y fuente.
diagnostics_32det_ranges <- det_long %>%
  group_by(
    dataset_source,
    det_col
  ) %>%
  summarise(
    n_non_missing = sum(!is.na(value)),
    
    min_value = suppressWarnings(
      min(value, na.rm = TRUE)
    ),
    
    max_value = suppressWarnings(
      max(value, na.rm = TRUE)
    ),
    
    n_below_0 = sum(
      value < 0,
      na.rm = TRUE
    ),
    
    n_above_100 = sum(
      value > 100,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    min_value = if_else(
      is.infinite(min_value),
      NA_real_,
      min_value
    ),
    
    max_value = if_else(
      is.infinite(max_value),
      NA_real_,
      max_value
    )
  )


# Calcular los mismos indicadores por región y submuestra.
diagnostics_32det_ranges_by_subsample <- det_long %>%
  group_by(
    comparison_region,
    subsample,
    det_col
  ) %>%
  summarise(
    n_rows = n(),
    
    n_non_missing = sum(!is.na(value)),
    
    pct_non_missing =
      100 * n_non_missing / n_rows,
    
    min_value = suppressWarnings(
      min(value, na.rm = TRUE)
    ),
    
    max_value = suppressWarnings(
      max(value, na.rm = TRUE)
    ),
    
    n_below_0 = sum(
      value < 0,
      na.rm = TRUE
    ),
    
    n_above_100 = sum(
      value > 100,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    min_value = if_else(
      is.infinite(min_value),
      NA_real_,
      min_value
    ),
    
    max_value = if_else(
      is.infinite(max_value),
      NA_real_,
      max_value
    )
  ) %>%
  arrange(
    comparison_region,
    subsample,
    det_col
  )


# DICCIONARIO SOCIODEMOGRÁFICO

# Describir las principales variables armonizadas para facilitar
# la interpretación de los archivos de salida.
sociodemographic_dictionary <- tribble(
  ~variable, ~description,
  
  "reference_year_model",
  "Reference year used to compute age when only year of birth is available. RV = 2026, WHY = 2022, Diego = 2023.",
  
  "year_birth_model",
  "Year of birth harmonised from survey and Prolific fields.",
  
  "age_model",
  "Numeric age declared directly or calculated from year of birth and the survey reference year. Participants with only an age interval have NA.",
  
  "age_model_is_approximate",
  "TRUE when numeric age is calculated from year of birth and the survey reference year; FALSE for directly reported numeric ages or missing age_model.",
  
  "age_group_model",
  "Age harmonised into 18_39, 40_59, 60_plus or unknown. Original Diego age intervals are grouped without assigning individual numeric ages.",
  
  "gender_model",
  "Gender harmonised into female, male, other, unknown or conflict.",
  
  "education_model",
  "Education harmonised from RV, Diego Q4 and any Prolific education field found.",
  
  "employment_model",
  "Employment harmonised from RV, Diego Q5 and Prolific employment status.",
  
  "student_status_model",
  "Student status harmonised mainly from Prolific student status.",
  
  "num_children_model",
  "Number of children harmonised mainly from Diego Q6 where available.",
  
  "income_model",
  "Income harmonised mainly from Diego Q7 and any clear income field found.",
  
  "country_model",
  "Country of residence harmonised as ISO2 code where possible.",
  
  "country_region_model",
  "Broad residence region. European countries are grouped by European subregion and Latin American countries are labelled latin_america.",
  
  "country_birth_model",
  "Country of birth harmonised as ISO2 code where possible.",
  
  "nationality_model",
  "Nationality harmonised from Prolific when available.",
  
  "ethnicity_model",
  "Ethnicity simplified harmonised from Prolific when available.",
  
  "country_model_grouped",
  "Country grouped; countries with fewer than MIN_COUNTRY_N rows assigned to OTHER_COUNTRIES.",
  
  "language_model",
  "Language harmonised into EN, ES, EU, NL, FR, DE, IT, PL, GR, OTHER or unknown.",
  
  "vote_status_declared",
  "Declared voting status harmonised into voter, abstainer, uncertain, unknown or conflict.",
  
  "voted_observed",
  "Binary voting indicator: voter = 1, abstainer = 0, otherwise NA.",
  
  "political_left_right_model",
  "Numeric left-right self-placement, expected 0-100.",
  
  "political_block_model",
  "Political self-placement grouped into extreme_left, left, centre, right, extreme_right, unknown or invalid.",
  
  "self_classification_model",
  "Household investment self-classification mapped to archetype-like categories.",
  
  "energy_efficiency_goal_0_100",
  "Energy efficiency goal parsed as numeric 0-100 where available.",
  
  "climate_awareness_0_100",
  "Climate change awareness parsed as numeric 0-100 where available.",
  
  "energy_transition_awareness_0_100",
  "Energy transition awareness parsed as numeric 0-100 where available.",
  
  "travel_distance_model",
  "Daily travel distance parsed as numeric where available.",
  
  "travel_time_model",
  "Daily travel time parsed as numeric where available.",
  
  "n_det_non_missing",
  "Number of available harmonised determinants among the 32 determinant variables.",
  
  "prop_det_non_missing",
  "Proportion of available harmonised determinants among the 32 determinant variables."
)


# GUARDADO DE ARCHIVOS

# Asociar cada tabla creada con su correspondiente archivo CSV.
outputs <- list(
  "all_sources_integrated_clean.csv" =
    all_sources_integrated_clean,
  
  "all_sources_integrated_clean_traceability.csv" =
    all_sources_integrated_clean_traceability,
  
  "sociodemographics_clean.csv" =
    sociodemographics_clean,
  
  "determinants_harmonized.csv" =
    determinants_harmonized,
  
  "sociodemographic_source_columns.csv" =
    sociodemographic_source_columns,
  
  "determinant_dictionary_32.csv" =
    determinant_dictionary,
  
  "diagnostics_sociodemographics_coverage.csv" =
    diagnostics_sociodemographics_coverage,
  
  "diagnostics_sociodemographics_counts.csv" =
    diagnostics_sociodemographics_counts,
  
  "diagnostics_sociodemographics_coverage_by_subsample.csv" =
    diagnostics_sociodemographics_coverage_by_subsample,
  
  "diagnostics_sociodemographics_counts_by_subsample.csv" =
    diagnostics_sociodemographics_counts_by_subsample,
  
  "diagnostics_sociodemographics_conflicts.csv" =
    diagnostics_conflicts,
  
  "diagnostics_sociodemographics_conflicts_by_subsample.csv" =
    diagnostics_conflicts_by_subsample,
  
  "diagnostics_32det_by_source.csv" =
    diagnostics_32det_by_source,
  
  "diagnostics_32det_by_subsample.csv" =
    diagnostics_32det_by_subsample,
  
  "diagnostics_32det_ranges.csv" =
    diagnostics_32det_ranges,
  
  "diagnostics_32det_ranges_by_subsample.csv" =
    diagnostics_32det_ranges_by_subsample,
  
  "sociodemographic_dictionary.csv" =
    sociodemographic_dictionary
)


# Escribir los archivos de salida en el directorio de armonización.
iwalk(
  outputs,
  ~ write_csv(
    .x,
    file.path(out_dir, .y)
  )
)


# RESUMEN EN CONSOLA
cat("\nCOBERTURA SOCIODEMOGRÁFICA POR FUENTE\n")

print(
  diagnostics_sociodemographics_coverage
)

cat("\n32 DETERMINANTES POR FUENTE\n")

print(
  diagnostics_32det_by_source
)

cat("\n32 DETERMINANTES POR SUBMUESTRA\n")

print(
  diagnostics_32det_by_subsample,
  n = Inf
)

message(
  "Listo. Resultados guardados en: ",
  out_dir
)