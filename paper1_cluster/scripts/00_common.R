

# Librerías, configuración y funciones comunes del proyecto


# PAQUETES

PAQUETES <- c(
  "tidyverse",
  "readxl",
  "janitor",
  "scales",
  "psych",
  "GPArotation",
  "cluster",
  "clue",
  "future.apply",
  "data.table",
  "bigstatsr",
  "R.utils",
  "stringr",
)

PAQUETES_FALTANTES <- PAQUETES[
  !vapply(
    PAQUETES,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(PAQUETES_FALTANTES) > 0L) {
  install.packages(
    PAQUETES_FALTANTES,
    repos = "https://cloud.r-project.org",
    dependencies = c("Depends", "Imports", "LinkingTo")
  )
}

invisible(
  lapply(
    PAQUETES,
    function(paquete) {
      suppressPackageStartupMessages(
        library(paquete, character.only = TRUE)
      )
    }
  )
)


# RUTAS COMUNES

# 00_common.R debe encontrarse en paper1_cluster/scripts/.
# Las rutas se calculan a partir de la ubicación del archivo.
source_files <- vapply(
  sys.frames(),
  function(frame) {
    if (is.null(frame$ofile)) {
      ""
    } else {
      as.character(frame$ofile)
    }
  },
  character(1)
)

common_files <- source_files[
  basename(source_files) == "00_common.R"
]

if (length(common_files) == 0L) {
  stop(
    "No se ha podido localizar 00_common.R. ",
    "Cárgalo mediante source()."
  )
}

common_file <- normalizePath(
  tail(common_files, 1L),
  mustWork = TRUE
)

scripts_root <- dirname(common_file)

paper_root <- normalizePath(
  file.path(scripts_root, ".."),
  mustWork = TRUE
)

project_root <- normalizePath(
  file.path(paper_root, ".."),
  mustWork = TRUE
)

data_root <- file.path(
  paper_root,
  "data"
)

processed_root <- file.path(
  data_root,
  "processed"
)


# VARIABLES Y CONSTANTES COMUNES

RANDOM_SEED <- 123L

# Muestras utilizadas en los análisis
ANALYSIS_SAMPLES <- c(
  "COMPLETE",
  "EUROPE",
  "LATAM",
  "DIEGO",
  "RENOVISOR",
  "WHY_EUROPE",
  "WHY_LATAM"
)


# Métodos de análisis
METHODS <- c(
  "KMEANS",
  "EFA"
)


# Matrices de análisis
MATRICES_TO_RUN <- c(
  "matrix_32_raw_0_1",
  "matrix_32_pos_0_1",
  "matrix_32_ext_0_1",
  "matrix_32_z_abs"
)

MATRICES <- c(
  "RAW",
  "POS",
  "EXT",
  "Z_ABS"
)


# Determinantes
N_DET_TOTAL <- 32L

DET_NEUTRAL_VALUE <- 50


# Número de réplicas del índice bootstrap definitivo
N_BOOTSTRAPS_INDEX <- 1000L


# Parámetros de construcción de firmas D-pooled
D_DET_GRID <- 8:15


# Configuración general de Greedy D-pooled
MAX_GREEDY_PROTOTYPES <- 12L

PLOT_PROTOTYPES <- 1:8

COVERAGE_THRESHOLDS <- c(
  80,
  85,
  90,
  95
)


# Ponderación de las firmas
POOL_WEIGHTINGS <- c(
  "equal_candidate",
  "equal_element"
)

PRIMARY_WEIGHTING <- "equal_candidate"


# Umbral utilizado para diagnosticar las coincidencias Jaccard
MATCH_THRESHOLD <- 50


# Diccionario común de nombres de países y códigos de dos letras.
# Incluye las denominaciones utilizadas en 01_mergeData.R y en
# 01_1_harmonize_sociodemographics.R.
# Se conservan variantes con y sin tildes o signos de puntuación,
# porque las funciones de ambos scripts normalizan el texto de
# entrada de forma diferente.
#
# OTHER_EU identifica respuestas que indican otro país europeo sin
# especificarlo; no es un código ISO2 de un país concreto.
# Los valores NA corresponden a respuestas sin información válida.

country_name_to_iso2 <- c(
  "spain" = "ES",
  "espana" = "ES",
  "españa" = "ES",
  "germany" = "DE",
  "alemania" = "DE",
  "italy" = "IT",
  "italia" = "IT",
  "greece" = "GR",
  "grecia" = "GR",
  "the netherlands" = "NL",
  "netherlands" = "NL",
  "nederland" = "NL",
  "belgium" = "BE",
  "belgica" = "BE",
  "bélgica" = "BE",
  "france" = "FR",
  "portugal" = "PT",
  "bulgaria" = "BG",
  "lithuania" = "LT",
  "czechia" = "CZ",
  "czech republic" = "CZ",
  "republica checa" = "CZ",
  "denmark" = "DK",
  "sweden" = "SE",
  "finland" = "FI",
  "poland" = "PL",
  "romania" = "RO",
  "rumania" = "RO",
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
  "moldavia" = "MD",
  "norway" = "NO",
  "noruega" = "NO",
  "iceland" = "IS",
  "islandia" = "IS",
  "liechtenstein" = "LI",
  "monaco" = "MC",
  "andorra" = "AD",
  "bosnia and herzegovina" = "BA",
  "bosnia y herzegovina" = "BA",
  "bosnia" = "BA",
  "montenegro" = "ME",
  "north macedonia" = "MK",
  "macedonia del norte" = "MK",
  "san marino" = "SM",
  "belarus" = "BY",
  "bielorrusia" = "BY",
  
  "other eu country" = "OTHER_EU",
  "other eu countries" = "OTHER_EU",
  "other european country" = "OTHER_EU",
  "other european countries" = "OTHER_EU",
  
  "argentina" = "AR",
  "bolivia" = "BO",
  "bolivia, plurinational state of" = "BO",
  "bolivia plurinational state of" = "BO",
  "brazil" = "BR",
  "brasil" = "BR",
  "chile" = "CL",
  "colombia" = "CO",
  "costa rica" = "CR",
  "cuba" = "CU",
  "dominican republic" = "DO",
  "república dominicana" = "DO",
  "republica dominicana" = "DO",
  "ecuador" = "EC",
  "el salvador" = "SV",
  "guatemala" = "GT",
  "honduras" = "HN",
  "mexico" = "MX",
  "méxico" = "MX",
  "nicaragua" = "NI",
  "panama" = "PA",
  "panamá" = "PA",
  "paraguay" = "PY",
  "peru" = "PE",
  "perú" = "PE",
  "uruguay" = "UY",
  "venezuela" = "VE",
  "venezuela, bolivarian republic of" = "VE",
  "venezuela bolivarian republic of" = "VE",
  
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



# Códigos de países considerados latinoamericanos en la separación de WHY.
latam_iso2 <- c(
  "AR", "BO", "BR", "CL", "CO", "CR", "CU", "DO",
  "EC", "SV", "GT", "HN", "MX", "NI", "PA", "PY",
  "PE", "UY", "VE"
)


# Identifica el código de país a partir de un texto original y
# su correspondiente versión normalizada.
#
# Reconoce códigos directos, códigos entre paréntesis, códigos al inicio
# de una respuesta y nombres incluidos en country_name_to_iso2.
#
# x_chr: vector de respuestas originales de país.
# x_low: vector de respuestas normalizadas para buscar en el diccionario.
#
# Devuelve: vector de códigos de país o NA_character_ cuando
# no se puede identificar el país.

extract_country_code_base <- function(x_chr, x_low) {
  code_exact <- ifelse(
    str_detect(x_chr, "^[A-Za-z]{2}$"),
    str_to_upper(x_chr),
    NA_character_
  )
  
  code_parentheses <- str_match(
    x_chr,
    "\\(([A-Za-z]{2})\\)"
  )[, 2]
  
  code_prefix <- str_match(
    x_chr,
    "^\\s*([A-Za-z]{2})\\s*[-–]"
  )[, 2]
  
  code_name <- unname(
    country_name_to_iso2[x_low]
  )
  
  str_to_upper(
    coalesce(
      code_exact,
      code_parentheses,
      code_prefix,
      code_name
    )
  )
}

##################### FUNCIONES COMUNES


# Busca columnas cuyos nombres coincidan con una expresión regular.
# Permite excluir columnas mediante un segundo patrón de búsqueda.
#
# df: tabla en la que se buscarán las columnas.
# pattern: expresión regular que deben cumplir los nombres.
# exclude: expresión regular opcional para excluir columnas.
#
# Devuelve: vector de nombres de columnas coincidentes, sin duplicados.

find_cols <- function(df, pattern, exclude = NULL) {
  out <- names(df)[
    str_detect(
      names(df),
      regex(pattern, ignore_case = TRUE)
    )
  ]
  
  if (!is.null(exclude)) {
    out <- out[
      !str_detect(
        out,
        regex(exclude, ignore_case = TRUE)
      )
    ]
  }
  
  unique(out)
}
# Reúne los valores distintos y no ausentes de un vector.
# Elimina espacios adicionales y duplicados. Cuando existen varias
# respuestas diferentes, las conserva separadas mediante " | ".
#
# x: vector de respuestas de un mismo participante o una misma fila.
# Devuelve: una cadena con los valores únicos o NA_character_
# cuando no existe ningún valor informado.
collapse_unique_non_missing <- function(x) {
  x <- unique(
    str_squish(as.character(x))
  )
  
  x <- x[
    !is.na(x) &
      x != "" &
      x != "NA" &
      x != "NaN"
  ]
  
  if (!length(x)) {
    return(NA_character_)
  }
  
  if (length(x) == 1) {
    return(x)
  }
  
  paste(x, collapse = " | ")
}

# Recupera una columna de una tabla sin producir un error cuando no existe.
# Si la columna no está disponible, devuelve un NA por cada fila.
#
# df: tabla de datos.
# col: nombre de la columna que se desea recuperar.
# Devuelve: valores de la columna o un vector de NA_character_.
safe_pull <- function(df, col) {
  if (is.na(col) || !col %in% names(df)) {
    rep(NA_character_, nrow(df))
  } else {
    df[[col]]
  }
}
# Extrae valores numéricos de respuestas que pueden contener texto.
# Utiliza el punto como separador decimal y la coma como separador
# de millares; suprime los avisos de conversión.
# x: vector de valores originales.
# Devuelve: vector numérico, con NA si no puede extraer un número.
parse_num <- function(x) {
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

# FUNCIONES COMUNES: ARCHIVOS Y RUTAS
check_file <- function(path) {
  
  if (!file.exists(path)) {
    stop(
      "No encuentro el archivo:\n",
      path
    )
  }
  
  invisible(path)
}


read_csv_safe <- function(path) {
  
  check_file(path)
  
  readr::read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE
  )
}


# FUNCIONES COMUNES: MATRICES
matrix_label <- function(x) {
  
  dplyr::recode(
    x,
    "matrix_32_raw_0_1" = "RAW",
    "matrix_32_pos_0_1" = "POS",
    "matrix_32_ext_0_1" = "EXT",
    "matrix_32_z_abs" = "Z_ABS",
    .default = x
  )
}


# FUNCIONES COMUNES: ESTADÍSTICAS
mean_or_na <- function(x) {
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  mean(
    x,
    na.rm = TRUE
  )
}


median_or_na <- function(x) {
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  median(
    x,
    na.rm = TRUE
  )
}


# FUNCIONES COMUNES: COBERTURA GREEDY
coverage_at_n <- function(
    prototype,
    cumulative_covered_pct,
    n_target
) {
  
  valid <- prototype <= n_target
  
  if (!any(valid)) {
    return(NA_real_)
  }
  
  p_available <- prototype[valid]
  
  cumulative_covered_pct[valid][
    which.max(p_available)
  ]
}


first_prototype_reaching <- function(
    prototype,
    cumulative_covered_pct,
    threshold
) {
  
  valid <-
    !is.na(cumulative_covered_pct) &
    cumulative_covered_pct >= threshold
  
  if (!any(valid)) {
    return(NA_integer_)
  }
  
  min(prototype[valid])
}


# FUNCIONES COMUNES: FIRMAS BINARIAS
patterns_to_matrix <- function(
    keys,
    determinant_names
) {
  
  mat <- do.call(
    rbind,
    lapply(
      strsplit(keys, ""),
      as.integer
    )
  )
  
  colnames(mat) <- determinant_names
  
  mat
}


hamming_distance <- function(
    center,
    mat
) {
  
  rowSums(
    sweep(
      mat,
      2,
      center,
      FUN = "!="
    )
  )
}


# FUNCIONES COMUNES: DETERMINANTES
normalize_determinant_key <- function(x) {
  
  x %>%
    as.character() %>%
    stringr::str_to_lower() %>%
    stringr::str_trim() %>%
    stringr::str_remove(
      "^det[_\\.-]*\\d+[_\\.-]*"
    ) %>%
    stringr::str_replace_all(
      "[^a-z0-9]+",
      "_"
    ) %>%
    stringr::str_remove("^_+") %>%
    stringr::str_remove("_+$")
}


parse_determinants <- function(x) {
  
  if (
    !length(x) ||
    is.na(x) ||
    stringr::str_trim(x) == ""
  ) {
    return(character())
  }
  
  stringr::str_split(
    x,
    ";\\s*"
  )[[1]] %>%
    stringr::str_trim() %>%
    unique() %>%
    sort()
}


collapse_determinants <- function(x) {
  
  paste(
    sort(unique(x)),
    collapse = "; "
  )
}


coerce_binary_numeric <- function(x) {
  
  if (is.logical(x)) {
    return(as.numeric(x))
  }
  
  x_chr <- as.character(x) %>%
    stringr::str_trim() %>%
    stringr::str_to_lower()
  
  out <- suppressWarnings(
    as.numeric(x_chr)
  )
  
  out[
    is.na(out) &
      x_chr %in% c(
        "true",
        "yes",
        "y",
        "x"
      )
  ] <- 1
  
  out[
    is.na(out) &
      x_chr %in% c(
        "false",
        "no",
        "n",
        "",
        "-"
      )
  ] <- 0
  
  out
}


# FUNCIONES COMUNES: COMPARACIÓN DE FIRMAS
compare_sets <- function(
    left_set,
    right_set
) {
  
  left_set <- unique(left_set)
  right_set <- unique(right_set)
  
  common_set <- intersect(
    left_set,
    right_set
  )
  
  union_set <- union(
    left_set,
    right_set
  )
  
  n_common <- length(common_set)
  
  tibble(
    n_left = length(left_set),
    n_right = length(right_set),
    
    n_common = n_common,
    
    overlap_left_pct =
      if (length(left_set)) {
        100 * n_common / length(left_set)
      } else {
        NA_real_
      },
    
    overlap_right_pct =
      if (length(right_set)) {
        100 * n_common / length(right_set)
      } else {
        NA_real_
      },
    
    jaccard_pct =
      if (length(union_set)) {
        100 * n_common / length(union_set)
      } else {
        NA_real_
      },
    
    common_determinants =
      collapse_determinants(common_set)
  )
}


make_pairwise_comparison <- function(
    left,
    right
) {
  
  tidyr::crossing(
    left_id = left$id,
    right_id = right$id
  ) %>%
    rowwise() %>%
    mutate(
      comparison = list(
        compare_sets(
          left$signature[
            left$id == left_id
          ][[1]],
          
          right$signature[
            right$id == right_id
          ][[1]]
        )
      )
    ) %>%
    tidyr::unnest(comparison) %>%
    ungroup()
}


# FUNCIONES COMUNES: VISUALIZACIÓN
save_plot <- function(
    p,
    filename,
    width = 10,
    height = 7
) {
  
  ggplot2::ggsave(
    filename = filename,
    plot = p,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}


theme_paper <- function(
    base_size = 11
) {
  
  ggplot2::theme_minimal(
    base_size = base_size
  ) +
    ggplot2::theme(
      panel.grid.minor = element_blank(),
      
      legend.position = "bottom",
      
      strip.text = element_text(
        face = "bold"
      ),
      
      plot.title = element_text(
        face = "bold"
      )
    )
}


# CONFIRMACIÓN
cat(
  "\n00_common.R cargado correctamente.\n",
  "Ruta del proyecto: ", project_root, "\n",
  "Ruta de paper1_cluster: ", paper_root, "\n",
  "Muestras: ", length(ANALYSIS_SAMPLES), "\n",
  "Matrices: ", length(MATRICES_TO_RUN), "\n",
  sep = ""
)
