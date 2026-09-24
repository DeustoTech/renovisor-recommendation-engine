# 03_1_component_quality.R
#
# OBJETIVO
# Evaluar la calidad y la disponibilidad de los datos integrados de
# RENOVISOR, WHY y DIEGO, aplicar la imputación definida para los
# determinantes y preparar las matrices para los análisis posteriores.
#
# ENTRADA
# paper1_cluster/data/processed/02_harmonize_sociodemographics/
# all_sources_integrated_clean_traceability.csv
#
# PROCESAMIENTO
# 1. Comprobar la estructura de la base integrada, la presencia de las
#    cuatro submuestras y la ausencia de identificadores duplicados.
#
# 2. Identificar las columnas correspondientes a los componentes de
#    las encuestas: determinantes, sociodemografía, voto y política,
#    adopción tecnológica, tarifas y costes, preocupaciones y barreras,
#    confianza e información, crisis energética, pobreza y atención.
#
# 3. Evaluar los 32 determinantes originales por participante:
#    disponibilidad, valores válidos en la escala 0-100, ausencias
#    y valores fuera de rango.
#
# 4. Imputar con 50 los determinantes originalmente ausentes de los
#    participantes que cumplen los requisitos de imputación.
#    Registrar cuántos valores se imputan y qué determinantes son.
#
# 5. Evaluar los determinantes después de la imputación:
#    número de valores válidos, media, desviación típica, mínimo,
#    máximo, valores distintos y proporción de valores extremos.
#
# 6. Clasificar la calidad de los determinantes e identificar a los
#    participantes que cumplen los criterios para clustering.
#
# 7. Evaluar la disponibilidad de edad, género, país, identificadores
#    y otras variables sociodemográficas. Clasificar la calidad
#    sociodemográfica y de los metadatos.
#
# 8. Evaluar la disponibilidad de las variables de voto y autoubicación
#    política. Calcular, para la muestra europea, qué participantes
#    disponen de los predictores requeridos por las distintas
#    especificaciones del modelo de propensity y cuáles disponen,
#    además, de una variable de voto binaria.
#
# 9. Localizar y evaluar los controles de atención disponibles.
#    Registrar controles respondidos, superados y fallados, así como
#    los controles concretos fallados por cada participante.
#
# 10. Evaluar la cobertura de los componentes de las encuestas
#     y clasificar la calidad global de cada fila según sus
#     determinantes y su información sociodemográfica.
#
# 11. Crear la base completa de calidad, la matriz con indicadores
#     de calidad y la matriz filtrada para clustering.
#
# 12. Generar diagnósticos por fuente y por submuestra sobre calidad,
#     cobertura, ausencias, imputación, metadatos, disponibilidad
#     para propensity y tamaños finales para clustering.
#
# REGLAS DE IMPUTACIÓN
# - Se aplica únicamente a RENOVISOR, WHY_EUROPE y WHY_LATAM.
# - DIEGO no se imputa.
# - Se exige un mínimo de 24 determinantes originales válidos
#   en la escala 0-100.
# - Solo se imputan las celdas originalmente ausentes.
# - No se imputan automáticamente las respuestas no numéricas ni
#   los valores fuera de rango.
# - Se asigna el valor 50 a cada celda elegible.
# - Se conservan indicadores de la disponibilidad original y un
#   registro de los determinantes imputados por participante.
#
# CRITERIOS DE CLUSTERING
# - Al menos 24 de los 32 determinantes válidos después de imputar.
# - Ningún determinante fuera del intervalo 0-100.
# - Más de dos valores distintos entre los determinantes válidos.
# - Se calcula un indicador cuando el 80 % o más de los determinantes
#   válidos tienen valor 0 o 100, pero este indicador NO constituye
#   un criterio automático de exclusión.
#
# IMPORTANTE
# - usable_for_clustering se determina a partir de la calidad
#   de los 32 determinantes.
# - Los controles de atención, la disponibilidad sociodemográfica
#   y la disponibilidad de variables para propensity se evalúan
#   por separado; no se utilizan como filtros automáticos
#   adicionales de usable_for_clustering.
# - La imputación con 50 no representa una respuesta observada
#   del participante, sino el valor asignado a una ausencia.
# - La matriz para clustering puede admitir filas con al menos
#   24 determinantes válidos; la ausencia de NA debe comprobarse
#   antes de utilizarla en algoritmos que exijan datos completos.
#
# SALIDAS
# Directorio:
# paper1_cluster/data/processed/03_1_component_quality/
#
# BASES PRINCIPALES
# - all_sources_integrated_component_quality.csv:
#   Base completa con determinantes posteriores a la imputación
#   y todos los indicadores de calidad generados.
#
# - matrix_32det_with_quality.csv:
#   Identificadores, metadatos, 32 determinantes e indicadores de
#   calidad de todas las filas.
#
# - matrix_32det_for_clustering.csv:
#   Filas que cumplen los criterios definidos para clustering.
#
# DIAGNÓSTICOS
# - diagnostics_component_candidate_columns.csv
# - diagnostics_component_quality_counts.csv
# - diagnostics_component_coverage_by_source.csv
# - diagnostics_component_quality_counts_by_subsample.csv
# - diagnostics_component_coverage_by_subsample.csv
# - diagnostics_row_quality_by_source.csv
# - diagnostics_row_quality_by_subsample.csv
# - diagnostics_row_quality_counts.csv
# - diagnostics_row_quality_counts_by_subsample.csv
# - diagnostics_missing_by_determinant.csv
# - diagnostics_missing_by_determinant_by_subsample.csv
# - diagnostics_missing_after_imputation_by_determinant.csv
# - diagnostics_missing_after_imputation_by_subsample.csv
# - diagnostics_imputation_50_by_determinant.csv
# - diagnostics_imputation_50_by_subsample.csv
# - diagnostics_imputation_50_by_row.csv
# - diagnostics_imputation_50_summary.csv
# - diagnostics_metadata_quality_counts.csv
# - diagnostics_metadata_quality_counts_by_subsample.csv
# - diagnostics_propensity_score_readiness.csv
# - diagnostics_propensity_score_readiness_by_subsample.csv
# - diagnostics_clustering_sample_sizes.csv
#
# VARIABLES PRINCIPALES GENERADAS
#
# n_det_valid_original:
#   Número de determinantes originales válidos en la escala 0-100.
#
# n_det_missing_original:
#   Número de determinantes ausentes o no convertibles a número
#   antes de imputar.
#
# n_det_imputed_50:
#   Número de determinantes imputados con 50 por participante.
#
# prop_det_imputed_50:
#   Proporción de los 32 determinantes imputados con 50.
#
# det_cols_imputed_50:
#   Nombres de los determinantes imputados por participante.
#
# n_det_valid:
#   Número de determinantes válidos después de imputar.
#
# n_det_missing:
#   Número de determinantes sin un valor válido en la escala 0-100
#   después de imputar.
#
# det_row_mean, det_row_sd, det_row_min, det_row_max:
#   Estadísticos calculados sobre los determinantes válidos.
#
# det_n_unique_values:
#   Número de valores distintos entre los determinantes válidos.
#
# det_prop_extreme_values:
#   Proporción de determinantes válidos con valor 0 o 100.
#
# flag_*:
#   Indicadores de ausencia, falta de valores válidos, valores fuera
#   de rango, baja variabilidad y alta proporción de extremos.
#
# quality_determinants_32:
#   Categoría de calidad asignada a los 32 determinantes.
#
# usable_for_clustering:
#   TRUE cuando la fila cumple los criterios de calidad definidos
#   para entrar en la matriz de clustering.
#
# quality_sociodemographics, metadata_quality:
#   Clasificaciones de disponibilidad sociodemográfica y metadatos.
#
# quality_vote_politics:
#   Clasificación de disponibilidad de las variables de voto
#   y autoubicación política.
#
# has_propensity_*, usable_for_propensity_*,
# quality_propensity_score_*:
#   Indicadores de disponibilidad de predictores y de la variable
#   de voto para las distintas especificaciones del modelo europeo.
#
# quality_technology_adoption, quality_tariffs_costs_payback,
# quality_concerns_barriers, quality_trust_information,
# quality_energy_crisis, quality_poverty, quality_attention_quality:
#   Disponibilidad y calidad de los componentes de las encuestas.
#
# attention_check_*:
#   Disponibilidad, resultados y fallos de los controles de atención.
#
# row_quality_final:
#   Clasificación global de calidad de la fila a partir de los
#   determinantes y la disponibilidad sociodemográfica.
#
# usable_for_main_analysis:
#   Indicador derivado de las categorías admitidas
#   en row_quality_final.
#
# DEPENDENCIAS
# - 00_common.R: paquetes, rutas, constantes y funciones compartidas.
# - 01_mergeData.R: genera la base integrada original.
# - 02_harmonize_common_variables.R: genera el archivo de entrada
#   con las variables y los 32 determinantes armonizados.

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

# processed_root se define en 00_common.R.
# processed_root <- "paper1_cluster/data/processed"
in_file <- file.path(
  processed_root,
  "02_harmonize_sociodemographics",
  "all_sources_integrated_clean_traceability.csv"
)

out_dir <- file.path(
  processed_root,
  "03_1_component_quality"
)

# Número mínimo de determinantes válidos para entrar en clustering.
MIN_DET_VALID_FOR_CLUSTERING <- 24L

# Una fila con dos valores distintos o menos se considera de baja variabilidad.
LOW_VARIABILITY_MAX_UNIQUE <- 2L

# Una proporción de extremos igual o superior a 0,80 se marca como sospechosa.
HIGH_EXTREME_SHARE_THRESHOLD <- 0.80

# Mínimo de determinantes originales válidos para permitir imputación a 50.
MIN_DET_VALID_ORIGINAL_FOR_IMPUTATION <- 24L

# DIEGO no se incluye porque sus 32 determinantes están completos
# en la base armonizada y no se ha definido imputación para esa fuente.
IMPUTE_50_SUBSAMPLES <- c(
  "RENOVISOR",
  "WHY_EUROPE",
  "WHY_LATAM"
)

check_file(in_file)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# Leer inicialmente todas las columnas como caracteres.
# Las conversiones numéricas se realizan posteriormente de forma explícita.
df <- read_csv(
  in_file,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)


# COMPROBACIONES INICIALES
required_structure_cols <- c(
  "integrated_row_id",
  "subsample",
  "comparison_region",
  "subsample_row_id",
  "dataset_source"
)

missing_structure_cols <- setdiff(
  required_structure_cols,
  names(df)
)

if (length(missing_structure_cols)) {
  stop(
    "Faltan columnas necesarias creadas en 01/02: ",
    paste(missing_structure_cols, collapse = ", ")
  )
}

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
#     "Faltan submuestras esperadas: ",
#     paste(missing_subsamples, collapse = ", ")
#   )
# }

check_expected_subsamples(df)

if (anyDuplicated(df$integrated_row_id)) {
  stop("integrated_row_id contiene duplicados.")
}

cat("\nSUBMUESTRAS RECIBIDAS DESDE 02\n")

print(
  df %>%
    count(
      comparison_region,
      subsample,
      dataset_source,
      name = "n"
    ),
  n = Inf
)


# FUNCIONES AUXILIARES

# Limpia espacios y transforma los códigos de ausencia en NA.
#
# x: vector de respuestas originales.
# Devuelve: vector de caracteres depurado.
#
# Se mantiene local porque la versión de clean_text() utilizada en
# 02_harmonize_common_variables.R también elimina determinadas respuestas
# del tipo "Prefer not to say". No son exactamente la misma función.
# clean_text <- function(x) {
#   x <- str_squish(as.character(x))
#   
#   x[
#     is.na(x) |
#       x %in% c(
#         "",
#         "NA",
#         "NaN",
#         "NULL",
#         "null",
#         "None",
#         "none",
#         "DATA_EXPIRED",
#         "data_expired"
#       )
#   ] <- NA_character_
#   
#   x
# }


# Convierte respuestas a números usando parse_num() de 00_common.R.
#
# x: vector de respuestas.
# Devuelve: vector numérico, con NA si no puede extraerse un número.
#
# Se conserva el nombre parse_num_clean() para no modificar todas
# las llamadas existentes en este script.
#
# Definición anterior, ahora centralizada en parse_num():
#
# parse_num_clean <- function(x) {
#   suppressWarnings(
#     parse_number(
#       as.character(x),
#       locale = locale(
#         decimal_mark = ".",
#         grouping_mark = ","
#       )
#     )
#   )
# }

# parse_num_clean <- function(x) {
#   parse_num(x)
# }


# Comprueba si existe una respuesta original distinta de los códigos
# de ausencia reconocidos por clean_text().
#
# x: vector de respuestas originales.
# Devuelve: vector lógico; TRUE si la respuesta está informada.
is_non_missing_raw <- function(x) {
  !is.na(clean_text_quality(x))
}


# Comprueba si una variable armonizada contiene información utilizable.
# Excluye las categorías de ausencia, conflicto, otros e invalidación.
#
# x: vector de valores armonizados.
# Devuelve: vector lógico; TRUE si el valor se considera informado.
#
# Se mantiene local: is_valid_model_value() de 02 utiliza criterios
# ligeramente diferentes, especialmente para datos numéricos.
is_valid_model_value <- function(x) {
  x <- clean_text_quality(x)
  
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


# Recupera una columna como caracteres. Si no existe, devuelve
# un NA por cada fila. Utiliza safe_pull() de 00_common.R.
#
# data: tabla de entrada.
# col: nombre de la columna que se desea recuperar.
# Devuelve: vector de caracteres.
#
# Definición anterior:
#
# safe_chr_col <- function(data, col) {
#   if (col %in% names(data)) {
#     as.character(data[[col]])
#   } else {
#     rep(NA_character_, nrow(data))
#   }
# }

safe_chr_col <- function(data, col) {
  as.character(safe_pull(data, col))
}


# Recupera una columna como números. Si no existe, devuelve un
# NA_real_ por cada fila.
# Utiliza safe_pull() y parse_num(), definidas en 00_common.R.
#
# data: tabla de entrada.
# col: nombre de la columna.
# Devuelve: vector numérico.
#
# Definición anterior:
#
# safe_num_col <- function(data, col) {
#   if (col %in% names(data)) {
#     parse_num_clean(data[[col]])
#   } else {
#     rep(NA_real_, nrow(data))
#   }
# }

safe_num_col <- function(data, col) {
  parse_num(safe_pull(data, col))
}


# Cuenta, fila por fila, cuántas columnas de un conjunto contienen
# información armonizada válida.
#
# data: tabla de entrada.
# cols: nombres de las columnas que se desean evaluar.
# Devuelve: vector numérico de recuentos por fila.
count_valid_model_cols <- function(data, cols) {
  cols <- intersect(cols, names(data))
  
  if (!length(cols)) {
    return(rep(0L, nrow(data)))
  }
  
  data %>%
    select(all_of(cols)) %>%
    mutate(across(everything(), is_valid_model_value)) %>%
    as.data.frame() %>%
    rowSums(na.rm = TRUE)
}


# find_component_cols() realiza la misma búsqueda que find_cols(),
# definida en 00_common.R. Se conserva comentada su definición original.
#
# find_component_cols <- function(
    #     data,
#     pattern,
#     exclude = NULL
# ) {
#   out <- names(data)[
#     str_detect(
#       names(data),
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


# Calcula la disponibilidad de un componente de encuesta por participante.
# La disponibilidad esperada se determina dentro de cada fuente de datos.
#
# data: tabla que contiene las respuestas y dataset_source.
# component_name: nombre utilizado para crear las columnas del resultado.
# cols: columnas originales pertenecientes al componente.
#
# Devuelve: tibble con número y proporción de respuestas informadas
# y una categoría de calidad para el componente.
component_stats <- function(
    data,
    component_name,
    cols
) {
  cols <- intersect(unique(cols), names(data))
  
  n_col_name <- paste0(
    "n_", component_name, "_non_missing"
  )
  
  prop_col_name <- paste0(
    "prop_", component_name, "_non_missing"
  )
  
  quality_col_name <- paste0(
    "quality_", component_name
  )
  
  if (!length(cols)) {
    return(
      tibble(
        !!n_col_name := rep(0L, nrow(data)),
        !!prop_col_name := rep(NA_real_, nrow(data)),
        !!quality_col_name := rep(
          "not_found_in_dataset",
          nrow(data)
        )
      )
    )
  }
  
  n_non_missing <- data %>%
    select(all_of(cols)) %>%
    mutate(across(everything(), is_non_missing_raw)) %>%
    as.data.frame() %>%
    rowSums(na.rm = TRUE)
  
  prop_non_missing <- n_non_missing / length(cols)
  
  source_expected <- data %>%
    mutate(.n_component = n_non_missing) %>%
    group_by(dataset_source) %>%
    summarise(
      .source_has_component = any(
        .n_component > 0,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  source_expected_vec <-
    data$dataset_source %in%
    source_expected$dataset_source[
      source_expected$.source_has_component
    ]
  
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


# Conserva únicamente los determinantes numéricos válidos en 0-100.
#
# x: vector numérico de determinantes.
# Devuelve: vector con los valores válidos.
valid_det_values <- function(x) {
  x[
    !is.na(x) &
      x >= 0 &
      x <= 100
  ]
}


# Calcula un estadístico por participante sobre sus determinantes válidos.
#
# mat: matriz numérica de determinantes.
# fun: función estadística que se aplica a cada fila.
# min_n: mínimo de valores válidos exigido.
# empty_value: resultado cuando no se alcanza min_n.
#
# Devuelve: vector con un resultado por fila de mat.
det_row_stat <- function(
    mat,
    fun,
    min_n = 1,
    empty_value = NA_real_
) {
  apply(
    mat,
    1,
    function(x) {
      x <- valid_det_values(x)
      
      if (length(x) < min_n) {
        return(empty_value)
      }
      
      fun(x)
    }
  )
}


# Calcula la ausencia y los valores fuera de rango de cada determinante
# dentro de los grupos indicados.
#
# data: tabla original o tabla posterior a la imputación.
# group_vars: columnas que definen los grupos del diagnóstico.
# det_cols: nombres de los 32 determinantes.
#
# Devuelve: tibble con número de filas, ausencias, proporción de ausencias
# y número de valores fuera del intervalo 0-100 por determinante y grupo.
missing_by_determinant <- function(
    data,
    group_vars,
    det_cols
) {
  data %>%
    select(
      all_of(group_vars),
      all_of(det_cols)
    ) %>%
    pivot_longer(
      all_of(det_cols),
      names_to = "det_col",
      values_to = "value_raw"
    ) %>%
    mutate(
      value_num = parse_num(value_raw),
      
      is_missing = is.na(value_num),
      
      is_out_of_range =
        !is.na(value_num) &
        (value_num < 0 | value_num > 100)
    ) %>%
    group_by(
      across(all_of(group_vars)),
      det_col
    ) %>%
    summarise(
      n_rows = n(),
      
      n_missing = sum(is_missing),
      
      prop_missing = n_missing / n_rows,
      
      n_out_of_range = sum(is_out_of_range),
      
      .groups = "drop"
    )
}


# Resume la calidad final de las filas por fuente o submuestra.
#
# data: tabla completa con indicadores de calidad.
# group_vars: columnas de agrupación.
#
# Devuelve: tibble con tamaños, categorías de calidad, disponibilidad
# para clustering, estadísticos de determinantes y metadatos.
row_quality_summary <- function(
    data,
    group_vars
) {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      n_rows = n(),
      
      n_usable_for_clustering = sum(
        usable_for_clustering,
        na.rm = TRUE
      ),
      
      prop_usable_for_clustering =
        n_usable_for_clustering / n_rows,
      
      n_usable_for_main_analysis = sum(
        usable_for_main_analysis,
        na.rm = TRUE
      ),
      
      prop_usable_for_main_analysis =
        n_usable_for_main_analysis / n_rows,
      
      n_usable_complete = sum(
        row_quality_final == "usable_complete",
        na.rm = TRUE
      ),
      
      n_usable_complete_limited_metadata = sum(
        row_quality_final ==
          "usable_complete_limited_metadata",
        na.rm = TRUE
      ),
      
      n_usable_partial = sum(
        row_quality_final == "usable_partial",
        na.rm = TRUE
      ),
      
      n_usable_partial_limited_metadata = sum(
        row_quality_final ==
          "usable_partial_limited_metadata",
        na.rm = TRUE
      ),
      
      n_no_determinants = sum(
        row_quality_final == "no_determinants",
        na.rm = TRUE
      ),
      
      n_too_many_missing_determinants = sum(
        row_quality_final ==
          "too_many_missing_determinants",
        na.rm = TRUE
      ),
      
      n_suspicious_determinants = sum(
        row_quality_final ==
          "suspicious_determinants",
        na.rm = TRUE
      ),
      
      n_invalid_determinants = sum(
        row_quality_final == "invalid_determinants",
        na.rm = TRUE
      ),
      
      mean_n_det_valid = mean(
        n_det_valid,
        na.rm = TRUE
      ),
      
      min_n_det_valid = min(
        n_det_valid,
        na.rm = TRUE
      ),
      
      max_n_det_valid = max(
        n_det_valid,
        na.rm = TRUE
      ),
      
      mean_det_row_sd = mean(
        det_row_sd,
        na.rm = TRUE
      ),
      
      n_metadata_complete = sum(
        metadata_quality == "metadata_complete",
        na.rm = TRUE
      ),
      
      n_metadata_partial = sum(
        metadata_quality == "metadata_partial",
        na.rm = TRUE
      ),
      
      n_metadata_poor = sum(
        metadata_quality %in% c(
          "metadata_poor",
          "metadata_no_identifier"
        ),
        na.rm = TRUE
      ),
      
      .groups = "drop"
    )
}


# Calcula las frecuencias y proporciones de las categorías de calidad
# de los componentes.
#
# data: tabla con las categorías de calidad.
# count_vars: columnas utilizadas para contar las observaciones.
# prop_vars: columnas utilizadas como denominador de las proporciones.
# quality_cols: columnas de calidad que se quieren resumir.
#
# Devuelve: tibble con frecuencias y proporciones por componente.
component_quality_counts <- function(
    data,
    count_vars,
    prop_vars,
    quality_cols
) {
  data %>%
    select(
      all_of(count_vars),
      all_of(quality_cols)
    ) %>%
    pivot_longer(
      all_of(quality_cols),
      names_to = "component",
      values_to = "quality"
    ) %>%
    group_by(
      across(all_of(count_vars)),
      component,
      quality
    ) %>%
    summarise(
      n = n(),
      .groups = "drop"
    ) %>%
    group_by(
      across(all_of(prop_vars)),
      component
    ) %>%
    mutate(
      prop = n / sum(n)
    ) %>%
    ungroup()
}


# Resume la cobertura de los componentes de encuesta.
#
# data: tabla con los recuentos de disponibilidad por componente.
# group_vars: columnas de agrupación.
# component_n_cols: columnas con recuentos n_*_non_missing.
#
# Devuelve: tibble con cobertura, media, mínimo y máximo por componente.
component_coverage <- function(
    data,
    group_vars,
    component_n_cols
) {
  data %>%
    select(
      all_of(group_vars),
      all_of(component_n_cols)
    ) %>%
    mutate(
      across(
        all_of(component_n_cols),
        ~ suppressWarnings(as.numeric(.x))
      )
    ) %>%
    pivot_longer(
      all_of(component_n_cols),
      names_to = "component_n_variable",
      values_to = "n_non_missing"
    ) %>%
    group_by(
      across(all_of(group_vars)),
      component_n_variable
    ) %>%
    summarise(
      n_rows = n(),
      
      n_rows_with_any = sum(
        n_non_missing > 0,
        na.rm = TRUE
      ),
      
      prop_rows_with_any =
        n_rows_with_any / n_rows,
      
      mean_n_non_missing = mean(
        n_non_missing,
        na.rm = TRUE
      ),
      
      min_n_non_missing = suppressWarnings(
        min(n_non_missing, na.rm = TRUE)
      ),
      
      max_n_non_missing = suppressWarnings(
        max(n_non_missing, na.rm = TRUE)
      ),
      
      .groups = "drop"
    ) %>%
    mutate(
      min_n_non_missing = if_else(
        is.infinite(min_n_non_missing),
        NA_real_,
        min_n_non_missing
      ),
      
      max_n_non_missing = if_else(
        is.infinite(max_n_non_missing),
        NA_real_,
        max_n_non_missing
      )
    )
}


# COLUMNAS POR COMPONENTE

# Excluir columnas armonizadas, identificadores y diagnósticos.
# Los componentes genéricos se construyen a partir de columnas originales.
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
    "subsample",
    "comparison_region",
    "subsample_row_id",
    "global_participant_key",
    "participant_key",
    "source_file",
    "source_row"
  ),
  collapse = "|"
)

# Detectar los 32 determinantes armonizados.
det_cols <- names(df)[
  str_detect(
    names(df),
    "^det_\\d{2}_"
  )
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

technology_adoption_cols <- find_cols(
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

tariffs_costs_payback_cols <- find_cols(
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

concerns_barriers_cols <- unique(
  c(
    names(df)[
      str_starts(
        names(df),
        "rv_concerns2__"
      )
    ],
    
    find_cols(
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
  )
)

trust_information_cols <- find_cols(
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

energy_crisis_cols <- unique(
  c(
    names(df)[
      str_starts(
        names(df),
        "rv_energy_crisis__"
      )
    ],
    
    find_cols(
      df,
      "energy_crisis|crisis|inflation|electricity|gas|heating|fuel",
      exclude = generic_exclude
    )
  )
)

poverty_cols <- unique(
  c(
    names(df)[
      str_starts(
        names(df),
        "rv_poverty__"
      )
    ],
    
    find_cols(
      df,
      "poverty|vulnerab|depriv|difficulty|arrears|afford|unable|inability",
      exclude = generic_exclude
    )
  )
)


# ATTENTION CHECKS: IDENTIFICACIÓN DE COLUMNAS

# Busca la primera columna que coincide con alguno de los patrones
# definidos para un control de atención.
#
# data: tabla de datos.
# patterns: vector o lista de expresiones regulares.
# check_id: identificador del control de atención.
#
# Devuelve: nombre de la primera columna encontrada o NA_character_.
# Emite un aviso si no hay patrones, no encuentra columnas o encuentra varias.
find_first_attention_col <- function(
    data,
    patterns,
    check_id
) {
  cols <- names(data)
  
  patterns <- unlist(
    patterns,
    recursive = TRUE,
    use.names = FALSE
  )
  
  patterns <- as.character(patterns)
  
  patterns <- patterns[
    !is.na(patterns) &
      patterns != ""
  ]
  
  if (!length(patterns)) {
    warning(
      "No hay patrones definidos para attention check: ",
      check_id
    )
    
    return(NA_character_)
  }
  
  hits <- unique(
    unlist(
      map(
        patterns,
        ~ cols[
          str_detect(
            cols,
            regex(.x, ignore_case = TRUE)
          )
        ]
      ),
      use.names = FALSE
    )
  )
  
  hits <- hits[hits %in% cols]
  
  if (!length(hits)) {
    warning(
      "No se encontró columna para attention check: ",
      check_id
    )
    
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


# Especificaciones de los controles de atención.
# Cada fila define su identificador, el tipo de respuesta esperada
# y los patrones utilizados para localizar su columna original.
attention_check_specs <- tribble(
  ~check_id, ~expected_type, ~patterns,
  
  "rv_decision_select_42",
  "select_42",
  list(
    c(
      "^rv_decision__.*select.*42",
      "^rv_decision__.*choose.*42",
      "^rv_decision__.*answer.*42",
      "^rv_decision__.*\\b42\\b"
    )
  ),
  
  "rv_decision_longest_line",
  "longest_line",
  list(
    c(
      "^rv_decision__.*longest.*line",
      "^rv_decision__.*line.*longest",
      "^rv_decision__.*largest.*line",
      "^rv_decision__.*line.*largest",
      "^rv_decision__.*linea.*larga",
      "^rv_decision__.*línea.*larga"
    )
  ),
  
  "rv_concerns2_option_4",
  "option_4",
  list(
    c(
      "^rv_concerns2__.*select.*option.*4",
      "^rv_concerns2__.*option_4",
      "^rv_concerns2__.*select_option_4"
    )
  ),
  
  "rv_concerns2_strongly_disagree",
  "strongly_disagree",
  list(
    c(
      "^rv_concerns2__.*please_select_strongly_disagree",
      "^rv_concerns2__.*strongly_disagree",
      "^rv_concerns2__.*strongly.*disagree"
    )
  ),
  
  "rv_energy_crisis_option_4",
  "option_4",
  list(
    c(
      "^rv_energy_crisis__.*select.*option.*4",
      "^rv_energy_crisis__.*option_4",
      "^rv_energy_crisis__.*select_option_4"
    )
  ),
  
  "rv_energy_crisis_strongly_disagree",
  "strongly_disagree",
  list(
    c(
      "^rv_energy_crisis__.*please_select_strongly_disagree",
      "^rv_energy_crisis__.*strongly_disagree",
      "^rv_energy_crisis__.*strongly.*disagree"
    )
  ),
  
  "rv_poverty_disagree",
  "disagree",
  list(
    c(
      "^rv_poverty__.*please_select_disagree",
      "^rv_poverty__.*select.*disagree"
    )
  ),
  
  "why_select_zero",
  "zero",
  list(
    c(
      "^why__.*please_select_zero",
      "^why__.*select.*zero",
      "^why__.*select.*0"
    )
  )
) %>%
  mutate(
    column = map2_chr(
      patterns,
      check_id,
      ~ find_first_attention_col(
        data = df,
        patterns = .x,
        check_id = .y
      )
    )
  ) %>%
  filter(!is.na(column))

attention_quality_cols <- attention_check_specs$column


# COMPONENTES DE CALIDAD
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

# Registrar qué columnas se han identificado para cada componente.
diagnostics_component_candidate_columns <- enframe(
  component_columns,
  name = "component",
  value = "column"
) %>%
  unnest_longer(
    column,
    values_to = "column"
  ) %>%
  mutate(
    column = as.character(column)
  ) %>%
  filter(
    !is.na(column),
    column != ""
  ) %>%
  arrange(
    component,
    column
  )


# CALIDAD DE LOS 32 DETERMINANTES
if (length(det_cols) != N_DET_TOTAL) {
  stop(
    "Se esperaban 32 determinantes armonizados, pero se han encontrado ",
    length(det_cols),
    ". Revisa nombres de columnas ^det_\\d{2}_"
  )
}


# VALORES ORIGINALES ANTES DE IMPUTAR

# Convertir los 32 determinantes a números sin modificar todavía sus NA.
det_numeric_original <- df %>%
  select(all_of(det_cols)) %>%
  mutate(
    across(
      everything(),
      parse_num
    )
  )

det_mat_original <- as.matrix(det_numeric_original)


# MÁSCARA DE AUSENCIAS ORIGINALES

# Identificar solamente los NA originales.
# Una respuesta no numérica que produzca NA durante la conversión
# no se imputa automáticamente si no estaba ausente en el dato original.
raw_missing_mat <- df %>%
  select(all_of(det_cols)) %>%
  mutate(
    across(
      everything(),
      ~ is.na(clean_text_quality(.x))
    )
  ) %>%
  as.matrix()


# DIAGNÓSTICOS ORIGINALES

# Número de determinantes con un valor numérico dentro de 0-100.
n_det_valid_original <- rowSums(
  !is.na(det_mat_original) &
    det_mat_original >= 0 &
    det_mat_original <= 100
)

# Incluye valores ausentes y valores que no pudieron convertirse a número.
n_det_missing_original <- rowSums(
  is.na(det_mat_original)
)


# FILAS ELEGIBLES PARA IMPUTACIÓN

# Se exige pertenecer a una submuestra autorizada y tener al menos
# 24 determinantes originales válidos.
rows_eligible_for_imputation <-
  df$subsample %in% IMPUTE_50_SUBSAMPLES &
  n_det_valid_original >= MIN_DET_VALID_ORIGINAL_FOR_IMPUTATION


# MÁSCARA DE IMPUTACIÓN

# TRUE identifica cada celda originalmente ausente que recibirá el valor 50.
# La máscara tiene las mismas dimensiones que la matriz de determinantes.
imputation_mask <- raw_missing_mat & matrix(
  rows_eligible_for_imputation,
  nrow = nrow(df),
  ncol = length(det_cols)
)

colnames(imputation_mask) <- det_cols


# RECUENTO DE IMPUTACIONES POR PARTICIPANTE

n_det_imputed_50 <- rowSums(
  imputation_mask
)

prop_det_imputed_50 <-
  n_det_imputed_50 / N_DET_TOTAL

has_det_imputed_50 <-
  n_det_imputed_50 > 0L


# DETERMINANTES IMPUTADOS EN CADA FILA

# Guardar sus nombres separados por "; " para permitir su auditoría.
det_cols_imputed_50 <- apply(
  imputation_mask,
  1,
  function(flags) {
    chosen <- det_cols[which(flags)]
    
    if (length(chosen)) {
      paste(chosen, collapse = "; ")
    } else {
      NA_character_
    }
  }
)


# APLICACIÓN DE LA IMPUTACIÓN

# Trabajar sobre una copia numérica.
# El archivo de entrada y det_mat_original permanecen sin modificar.
det_mat <- det_mat_original

det_mat[imputation_mask] <- DET_NEUTRAL_VALUE

colnames(det_mat) <- det_cols

det_numeric <- as_tibble(
  det_mat,
  .name_repair = "minimal"
)


# ESTADÍSTICOS DESPUÉS DE IMPUTAR

n_det_numeric <- rowSums(
  !is.na(det_mat)
)

n_det_valid <- rowSums(
  !is.na(det_mat) &
    det_mat >= 0 &
    det_mat <= 100
)

# Aquí "missing" significa que falta un valor válido en 0-100.
# Incluye ausencias y valores fuera de rango.
n_det_missing <-
  N_DET_TOTAL - n_det_valid

n_det_below_0 <- rowSums(
  !is.na(det_mat) &
    det_mat < 0
)

n_det_above_100 <- rowSums(
  !is.na(det_mat) &
    det_mat > 100
)

n_det_out_of_range <-
  n_det_below_0 + n_det_above_100

det_row_mean <- det_row_stat(
  det_mat,
  mean
)

det_row_sd <- det_row_stat(
  det_mat,
  sd,
  min_n = 2
)

det_row_min <- det_row_stat(
  det_mat,
  min
)

det_row_max <- det_row_stat(
  det_mat,
  max
)

# Número de valores distintos entre los determinantes válidos.
# El redondeo evita que diferencias numéricas insignificantes
# se cuenten como valores distintos.
det_n_unique_values <- apply(
  det_mat,
  1,
  function(x) {
    x <- valid_det_values(x)
    
    length(
      unique(
        round(x, 6)
      )
    )
  }
)

# Proporción de determinantes válidos con valores exactamente 0 o 100.
det_prop_extreme_values <- apply(
  det_mat,
  1,
  function(x) {
    x <- valid_det_values(x)
    
    if (!length(x)) {
      return(NA_real_)
    }
    
    mean(
      x <= 0 |
        x >= 100
    )
  }
)


# FLAGS DE CALIDAD SOBRE LOS DATOS DESPUÉS DE IMPUTAR

flag_no_determinants <-
  n_det_valid == 0

flag_too_many_missing <-
  n_det_valid > 0 &
  n_det_valid < MIN_DET_VALID_FOR_CLUSTERING

flag_incomplete_but_usable <-
  n_det_valid >= MIN_DET_VALID_FOR_CLUSTERING &
  n_det_valid < N_DET_TOTAL

flag_complete_32det <-
  n_det_valid == N_DET_TOTAL

flag_out_of_range <-
  n_det_out_of_range > 0

flag_low_variability <-
  n_det_valid >= MIN_DET_VALID_FOR_CLUSTERING &
  det_n_unique_values <= LOW_VARIABILITY_MAX_UNIQUE

flag_high_extreme_share <-
  n_det_valid >= MIN_DET_VALID_FOR_CLUSTERING &
  !is.na(det_prop_extreme_values) &
  det_prop_extreme_values >= HIGH_EXTREME_SHARE_THRESHOLD


# CLASIFICACIÓN DE CALIDAD DE LOS DETERMINANTES

# El orden de las condiciones establece qué categoría prevalece
# si una misma fila presenta varios problemas.
det_quality <- case_when(
  flag_out_of_range ~ "invalid_values",
  flag_no_determinants ~ "no_determinants",
  flag_too_many_missing ~ "too_many_missing",
  flag_low_variability ~ "low_variability",
  #flag_high_extreme_share ~ "high_extreme_share",
  flag_complete_32det ~ "usable_complete",
  flag_incomplete_but_usable ~ "usable_partial",
  TRUE ~ "review"
)

usable_for_clustering <- det_quality %in% c(
  "usable_complete",
  "usable_partial"
)


# TABLA DE CALIDAD E IMPUTACIÓN POR PARTICIPANTE

det_quality_df <- tibble(
  n_det_valid_original = n_det_valid_original,
  n_det_missing_original = n_det_missing_original,
  
  n_det_imputed_50 = n_det_imputed_50,
  prop_det_imputed_50 = prop_det_imputed_50,
  has_det_imputed_50 = has_det_imputed_50,
  det_cols_imputed_50 = det_cols_imputed_50,
  
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


# CALIDAD SOCIODEMOGRÁFICA Y DE METADATOS

# La edad se considera disponible si existe age_group_model
# o una edad numérica válida. Esto permite que DIEGO tenga edad
# informada aunque no disponga de edades individuales exactas.
has_age_info <-
  is_valid_model_value(
    safe_chr_col(df, "age_group_model")
  ) |
  !is.na(
    safe_num_col(df, "age_model")
  )

has_gender_info <- is_valid_model_value(
  safe_chr_col(df, "gender_model")
)

has_country_info <-
  is_valid_model_value(
    safe_chr_col(df, "country_model")
  ) |
  is_valid_model_value(
    safe_chr_col(df, "country_model_grouped")
  )

has_identifier <-
  is_non_missing_raw(
    safe_chr_col(df, "global_participant_key")
  ) |
  is_non_missing_raw(
    safe_chr_col(df, "participant_key")
  ) |
  is_non_missing_raw(
    safe_chr_col(df, "prolific_id")
  ) |
  is_non_missing_raw(
    safe_chr_col(df, "identification_code")
  )

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
  has_identifier &
    core_sociodemographic_count == 3 ~
    "metadata_complete",
  
  has_identifier &
    core_sociodemographic_count >= 1 ~
    "metadata_partial",
  
  has_identifier &
    core_sociodemographic_count == 0 ~
    "metadata_poor",
  
  TRUE ~ "metadata_no_identifier"
)

sociodemographic_quality_df <- tibble(
  has_identifier = has_identifier,
  has_age_info = has_age_info,
  has_gender_info = has_gender_info,
  has_country_info = has_country_info,
  
  core_sociodemographic_count =
    core_sociodemographic_count,
  
  extended_sociodemographic_count =
    extended_sociodemographic_count,
  
  quality_sociodemographics =
    quality_sociodemographics,
  
  metadata_quality = metadata_quality
)


# DISPONIBILIDAD PARA EL MODELO POLÍTICO EUROPEO

# Estas variables diagnostican la disponibilidad de sus predictores.
# No construyen el modelo ni realizan el bootstrap.
# WHY_LATAM no entra en este diagnóstico como muestra europea.
propensity_eu_applicable <-
  safe_chr_col(df, "comparison_region") == "EUROPE"

has_propensity_age <-
  is_valid_model_value(
    safe_chr_col(df, "age_group_model")
  ) |
  !is.na(
    safe_num_col(df, "age_model")
  )

has_propensity_education <- is_valid_model_value(
  safe_chr_col(df, "education_model")
)

has_propensity_income <- is_valid_model_value(
  safe_chr_col(df, "income_model")
)

has_propensity_country <-
  is_valid_model_value(
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

has_propensity_outcome_binary <-
  safe_chr_col(df, "vote_status_declared") %in%
  c("voter", "abstainer") |
  !is.na(
    safe_num_col(df, "voted_observed")
  )

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

prop_propensity_predictors_available <-
  n_propensity_predictors_available / 6

usable_for_propensity_score_strict <-
  propensity_eu_applicable &
  has_propensity_age &
  has_propensity_education &
  has_propensity_income &
  has_propensity_country &
  has_propensity_city_size &
  has_propensity_employment

usable_for_propensity_score_without_income <-
  propensity_eu_applicable &
  has_propensity_age &
  has_propensity_education &
  has_propensity_country &
  has_propensity_city_size &
  has_propensity_employment

usable_for_propensity_score_minimal <-
  propensity_eu_applicable &
  has_propensity_age &
  has_propensity_country &
  has_propensity_employment

usable_for_propensity_model_strict <-
  usable_for_propensity_score_strict &
  has_propensity_outcome_binary

usable_for_propensity_model_without_income <-
  usable_for_propensity_score_without_income &
  has_propensity_outcome_binary

usable_for_propensity_model_minimal <-
  usable_for_propensity_score_minimal &
  has_propensity_outcome_binary

quality_propensity_score_predictors <- case_when(
  !propensity_eu_applicable ~
    "not_applicable_non_europe",
  
  usable_for_propensity_score_strict ~
    "complete_strict_formula",
  
  usable_for_propensity_score_without_income ~
    "complete_without_income",
  
  usable_for_propensity_score_minimal ~
    "minimal_predictors_available",
  
  n_propensity_predictors_available > 0 ~
    "partial_predictors",
  
  TRUE ~ "missing_predictors"
)

quality_propensity_score_model <- case_when(
  !propensity_eu_applicable ~
    "not_applicable_non_europe",
  
  !has_propensity_outcome_binary ~
    "missing_binary_vote_outcome",
  
  usable_for_propensity_model_strict ~
    "model_ready_strict",
  
  usable_for_propensity_model_without_income ~
    "model_ready_without_income",
  
  usable_for_propensity_model_minimal ~
    "model_ready_minimal",
  
  n_propensity_predictors_available > 0 ~
    "outcome_available_but_predictors_partial",
  
  TRUE ~ "not_ready"
)

propensity_quality_df <- tibble(
  propensity_eu_applicable = propensity_eu_applicable,
  
  has_propensity_age = has_propensity_age,
  has_propensity_education = has_propensity_education,
  has_propensity_income = has_propensity_income,
  has_propensity_country = has_propensity_country,
  has_propensity_city_size = has_propensity_city_size,
  has_propensity_employment = has_propensity_employment,
  
  has_propensity_outcome_binary =
    has_propensity_outcome_binary,
  
  n_propensity_predictors_available =
    n_propensity_predictors_available,
  
  prop_propensity_predictors_available =
    prop_propensity_predictors_available,
  
  usable_for_propensity_score_strict =
    usable_for_propensity_score_strict,
  
  usable_for_propensity_score_without_income =
    usable_for_propensity_score_without_income,
  
  usable_for_propensity_score_minimal =
    usable_for_propensity_score_minimal,
  
  usable_for_propensity_model_strict =
    usable_for_propensity_model_strict,
  
  usable_for_propensity_model_without_income =
    usable_for_propensity_model_without_income,
  
  usable_for_propensity_model_minimal =
    usable_for_propensity_model_minimal,
  
  quality_propensity_score_predictors =
    quality_propensity_score_predictors,
  
  quality_propensity_score_model =
    quality_propensity_score_model
)


# CALIDAD DE VOTO Y VARIABLES POLÍTICAS

has_vote_status <- is_valid_model_value(
  safe_chr_col(df, "vote_status_declared")
)

has_voted_observed <- !is.na(
  safe_num_col(df, "voted_observed")
)

political_left_right_num <- safe_num_col(
  df,
  "political_left_right_model"
)

has_political_left_right <-
  !is.na(political_left_right_num) &
  political_left_right_num >= 0 &
  political_left_right_num <= 100

has_political_block <- is_valid_model_value(
  safe_chr_col(df, "political_block_model")
)

quality_vote_politics <- case_when(
  has_vote_status &
    has_political_left_right ~
    "vote_and_politics_complete",
  
  has_vote_status &
    !has_political_left_right ~
    "vote_only",
  
  !has_vote_status &
    has_political_left_right ~
    "politics_only",
  
  has_political_block ~
    "politics_block_only",
  
  TRUE ~ "missing_or_not_collected"
)

vote_politics_quality_df <- tibble(
  has_vote_status = has_vote_status,
  has_voted_observed = has_voted_observed,
  has_political_left_right = has_political_left_right,
  has_political_block = has_political_block,
  quality_vote_politics = quality_vote_politics
)


# CALIDAD GENÉRICA POR COMPONENTES

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


# ATTENTION CHECKS EXPLÍCITOS

# Normaliza las respuestas a controles de atención para compararlas
# con las respuestas esperadas.
#
# x: respuesta original o vector de respuestas.
# Devuelve: texto en minúsculas, sin guiones bajos y con espacios ajustados.
normalise_attention_response <- function(x) {
  clean_text_quality(x) %>%
    str_to_lower() %>%
    str_replace_all("_", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_squish()
}


# Comprueba si una respuesta cumple el control de atención indicado.
#
# x: respuesta individual original.
# expected_type: tipo de control, según attention_check_specs.
#
# Devuelve: TRUE si pasa, FALSE si falla y NA si no hay respuesta
# o el tipo de control no está reconocido.
attention_passes_expected <- function(
    x,
    expected_type
) {
  x_clean <- clean_text_quality(x)
  x_low <- normalise_attention_response(x)
  x_num <- parse_num(x)
  
  if (is.na(x_clean)) {
    return(NA)
  }
  
  case_when(
    expected_type == "select_42" ~
      (
        !is.na(x_num) &
          x_num == 42
      ) |
      str_detect(x_low, "^42$") |
      str_detect(x_low, "\\b42\\b"),
    
    expected_type == "longest_line" ~
      str_detect(x_low, "longest") |
      str_detect(x_low, "largest") |
      str_detect(x_low, "linea mas larga") |
      str_detect(x_low, "línea más larga"),
    
    expected_type == "option_4" ~
      (
        !is.na(x_num) &
          x_num == 4
      ) |
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
      (
        !is.na(x_num) &
          x_num == 0
      ) |
      str_detect(x_low, "^0$") |
      str_detect(x_low, "^zero$") |
      str_detect(x_low, "^cero$"),
    
    TRUE ~ NA
  )
}


# Evaluar cada control de atención identificado en attention_check_specs.
# La tabla larga conserva un registro por participante y control.
attention_check_long <- pmap_dfr(
  attention_check_specs,
  function(
    check_id,
    expected_type,
    patterns,
    column
  ) {
    response_raw <- df[[column]]
    
    passed <- map_lgl(
      response_raw,
      ~ {
        res <- attention_passes_expected(
          .x,
          expected_type
        )
        
        ifelse(
          is.na(res),
          FALSE,
          res
        )
      }
    )
    
    available <- !is.na(
      clean_text_quality(response_raw)
    )
    
    failed <- available & !passed
    
    tibble(
      row_index_attention = seq_len(nrow(df)),
      check_id = check_id,
      attention_check_column = column,
      expected_type = expected_type,
      response_raw = as.character(response_raw),
      
      attention_check_available = available,
      
      attention_check_passed = if_else(
        available,
        passed,
        NA
      ),
      
      attention_check_failed = if_else(
        available,
        failed,
        NA
      )
    )
  }
)


# RESUMEN DE ATTENTION CHECKS POR PARTICIPANTE

# Si una encuesta no tiene controles reconocidos, mantener una fila
# de diagnóstico por participante con estado "not_available".
if (nrow(attention_check_long) > 0L) {
  
  attention_check_df <- attention_check_long %>%
    group_by(row_index_attention) %>%
    summarise(
      n_attention_checks_available_explicit = sum(
        attention_check_available,
        na.rm = TRUE
      ),
      
      n_attention_checks_passed_explicit = sum(
        attention_check_passed == TRUE,
        na.rm = TRUE
      ),
      
      n_attention_checks_failed_explicit = sum(
        attention_check_failed == TRUE,
        na.rm = TRUE
      ),
      
      attention_check_failed_any =
        n_attention_checks_failed_explicit > 0,
      
      attention_check_failed_all_available =
        n_attention_checks_available_explicit > 0 &
        n_attention_checks_failed_explicit ==
        n_attention_checks_available_explicit,
      
      attention_check_failed_all_4 =
        n_attention_checks_available_explicit == 4 &
        n_attention_checks_failed_explicit == 4,
      
      attention_check_failed_4_or_more =
        n_attention_checks_failed_explicit >= 4,
      
      failed_attention_check_ids = paste(
        check_id[
          attention_check_failed == TRUE
        ],
        collapse = "; "
      ),
      
      .groups = "drop"
    ) %>%
    mutate(
      failed_attention_check_ids = na_if(
        failed_attention_check_ids,
        ""
      ),
      
      attention_check_status_explicit = case_when(
        n_attention_checks_available_explicit == 0 ~
          "not_available",
        
        n_attention_checks_failed_explicit == 0 ~
          "passed_all_available",
        
        attention_check_failed_all_available ~
          "failed_all_available",
        
        n_attention_checks_failed_explicit > 0 ~
          "failed_some",
        
        TRUE ~ "review"
      )
    ) %>%
    right_join(
      tibble(
        row_index_attention = seq_len(nrow(df))
      ),
      by = "row_index_attention"
    ) %>%
    arrange(row_index_attention) %>%
    mutate(
      n_attention_checks_available_explicit =
        replace_na(
          n_attention_checks_available_explicit,
          0L
        ),
      
      n_attention_checks_passed_explicit =
        replace_na(
          n_attention_checks_passed_explicit,
          0L
        ),
      
      n_attention_checks_failed_explicit =
        replace_na(
          n_attention_checks_failed_explicit,
          0L
        ),
      
      attention_check_failed_any =
        replace_na(
          attention_check_failed_any,
          FALSE
        ),
      
      attention_check_failed_all_available =
        replace_na(
          attention_check_failed_all_available,
          FALSE
        ),
      
      attention_check_failed_all_4 =
        replace_na(
          attention_check_failed_all_4,
          FALSE
        ),
      
      attention_check_failed_4_or_more =
        replace_na(
          attention_check_failed_4_or_more,
          FALSE
        ),
      
      attention_check_status_explicit =
        replace_na(
          attention_check_status_explicit,
          "not_available"
        )
    ) %>%
    select(-row_index_attention)
  
} else {
  
  attention_check_df <- tibble(
    n_attention_checks_available_explicit =
      rep(0L, nrow(df)),
    
    n_attention_checks_passed_explicit =
      rep(0L, nrow(df)),
    
    n_attention_checks_failed_explicit =
      rep(0L, nrow(df)),
    
    attention_check_failed_any =
      rep(FALSE, nrow(df)),
    
    attention_check_failed_all_available =
      rep(FALSE, nrow(df)),
    
    attention_check_failed_all_4 =
      rep(FALSE, nrow(df)),
    
    attention_check_failed_4_or_more =
      rep(FALSE, nrow(df)),
    
    failed_attention_check_ids =
      rep(NA_character_, nrow(df)),
    
    attention_check_status_explicit =
      rep("not_available", nrow(df))
  )
}


# DATASET FINAL DE CALIDAD

# Unir los datos originales con todos los diagnósticos por participante.
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
)


# Sustituir las 32 columnas originales por los determinantes numéricos
# posteriores a la imputación. Los diagnósticos originales permanecen
# en las columnas n_det_*_original y en los archivos específicos.
df_quality[det_cols] <- det_numeric


# CLASIFICACIÓN GLOBAL DE CALIDAD

df_quality <- df_quality %>%
  mutate(
    row_quality_final = case_when(
      quality_determinants_32 == "invalid_values" ~
        "invalid_determinants",
      
      quality_determinants_32 == "no_determinants" ~
        "no_determinants",
      
      quality_determinants_32 == "too_many_missing" ~
        "too_many_missing_determinants",
      
      quality_determinants_32 %in% c(
        "low_variability",
        "high_extreme_share"
      ) ~ "suspicious_determinants",
      
      quality_determinants_32 == "usable_complete" &
        quality_sociodemographics == "core_complete" ~
        "usable_complete",
      
      quality_determinants_32 == "usable_complete" &
        quality_sociodemographics != "core_complete" ~
        "usable_complete_limited_metadata",
      
      quality_determinants_32 == "usable_partial" &
        quality_sociodemographics %in% c(
          "core_complete",
          "core_partial"
        ) ~ "usable_partial",
      
      quality_determinants_32 == "usable_partial" ~
        "usable_partial_limited_metadata",
      
      TRUE ~ "review"
    ),
    
    usable_for_main_analysis =
      row_quality_final %in% c(
        "usable_complete",
        "usable_complete_limited_metadata",
        "usable_partial",
        "usable_partial_limited_metadata"
      )
  )


# MATRICES PARA CLUSTERING

id_cols <- c(
  "integrated_row_id",
  "subsample",
  "comparison_region",
  "subsample_row_id",
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
  "country_region_model",
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

# Seleccionar las columnas de calidad generadas en esta etapa.
quality_cols <- names(df_quality)[
  str_detect(
    names(df_quality),
    paste(
      c(
        "^n_det_",
        "^prop_det_",
        "^has_det_imputed_50$",
        "^det_cols_imputed_50$",
        "^det_row_",
        "^det_n_",
        "^det_prop_",
        "^flag_",
        "^quality_",
        "^usable_",
        "^has_",
        "metadata_quality",
        "row_quality_final",
        "core_sociodemographic_count",
        "extended_sociodemographic_count",
        "propensity"
      ),
      collapse = "|"
    )
  )
]

# Matriz completa, incluyendo filas que no cumplen los filtros.
matrix_32det_with_quality <- df_quality %>%
  select(
    any_of(id_cols),
    any_of(metadata_cols),
    all_of(det_cols),
    any_of(quality_cols)
  )

# Matriz filtrada que utilizarán los análisis de clustering.
matrix_32det_for_clustering <- matrix_32det_with_quality %>%
  filter(
    usable_for_clustering == TRUE,
    n_det_valid >= MIN_DET_VALID_FOR_CLUSTERING
  )


# DIAGNÓSTICOS DE MISSING ORIGINAL

# Utilizar df, no df_quality, para conservar las ausencias originales.
diagnostics_missing_by_determinant <- missing_by_determinant(
  df,
  "dataset_source",
  det_cols
) %>%
  arrange(
    dataset_source,
    det_col
  )

diagnostics_missing_by_determinant_by_subsample <-
  missing_by_determinant(
    df,
    c(
      "comparison_region",
      "subsample",
      "dataset_source"
    ),
    det_cols
  ) %>%
  arrange(
    comparison_region,
    subsample,
    det_col
  )


# DIAGNÓSTICOS DE MISSING DESPUÉS DE IMPUTAR

diagnostics_missing_after_imputation_by_determinant <-
  missing_by_determinant(
    df_quality,
    "dataset_source",
    det_cols
  ) %>%
  arrange(
    dataset_source,
    det_col
  )

diagnostics_missing_after_imputation_by_subsample <-
  missing_by_determinant(
    df_quality,
    c(
      "comparison_region",
      "subsample",
      "dataset_source"
    ),
    det_cols
  ) %>%
  arrange(
    comparison_region,
    subsample,
    det_col
  )


# DIAGNÓSTICOS DE IMPUTACIÓN A 50

# Crear un registro largo con una fila por participante y determinante.
# La columna imputed_50 indica si esa celda fue imputada.
imputation_long <- tibble(
  integrated_row_id = df$integrated_row_id,
  comparison_region = df$comparison_region,
  subsample = df$subsample,
  dataset_source = df$dataset_source,
  row_index = seq_len(nrow(df))
) %>%
  bind_cols(
    as_tibble(
      imputation_mask,
      .name_repair = "minimal"
    )
  ) %>%
  pivot_longer(
    cols = all_of(det_cols),
    names_to = "det_col",
    values_to = "imputed_50"
  )


# IMPUTACIONES POR DETERMINANTE Y FUENTE

diagnostics_imputation_50_by_determinant <-
  imputation_long %>%
  group_by(
    dataset_source,
    det_col
  ) %>%
  summarise(
    n_rows = n(),
    
    n_imputed_50 = sum(imputed_50),
    
    prop_rows_imputed_50 = n_imputed_50 / n_rows,
    
    .groups = "drop"
  ) %>%
  arrange(
    dataset_source,
    det_col
  )


# IMPUTACIONES POR DETERMINANTE Y SUBMUESTRA

diagnostics_imputation_50_by_subsample <-
  imputation_long %>%
  group_by(
    comparison_region,
    subsample,
    dataset_source,
    det_col
  ) %>%
  summarise(
    n_rows = n(),
    
    n_imputed_50 = sum(imputed_50),
    
    prop_rows_imputed_50 = n_imputed_50 / n_rows,
    
    .groups = "drop"
  ) %>%
  arrange(
    comparison_region,
    subsample,
    det_col
  )


# IMPUTACIONES POR PARTICIPANTE

diagnostics_imputation_50_by_row <- df_quality %>%
  select(
    integrated_row_id,
    comparison_region,
    subsample,
    dataset_source,
    
    n_det_valid_original,
    n_det_missing_original,
    
    n_det_imputed_50,
    prop_det_imputed_50,
    has_det_imputed_50,
    det_cols_imputed_50,
    
    n_det_valid,
    n_det_missing,
    usable_for_clustering
  )


# RESUMEN DE IMPUTACIONES POR SUBMUESTRA

diagnostics_imputation_50_summary <- df_quality %>%
  group_by(
    comparison_region,
    subsample,
    dataset_source
  ) %>%
  summarise(
    n_rows = n(),
    
    n_rows_with_imputation = sum(
      has_det_imputed_50
    ),
    
    n_cells_imputed_50 = sum(
      n_det_imputed_50
    ),
    
    mean_imputations_per_row = mean(
      n_det_imputed_50
    ),
    
    n_rows_all_missing_original = sum(
      n_det_valid_original == 0L
    ),
    
    .groups = "drop"
  ) %>%
  arrange(
    comparison_region,
    subsample
  )


# DIAGNÓSTICOS DE CALIDAD FINAL POR FUENTE

diagnostics_row_quality_counts <- df_quality %>%
  count(
    dataset_source,
    row_quality_final,
    name = "n"
  ) %>%
  group_by(dataset_source) %>%
  mutate(
    prop = n / sum(n)
  ) %>%
  ungroup() %>%
  arrange(
    dataset_source,
    desc(n)
  )


# DIAGNÓSTICOS DE CALIDAD FINAL POR SUBMUESTRA

diagnostics_row_quality_counts_by_subsample <-
  df_quality %>%
  count(
    comparison_region,
    subsample,
    dataset_source,
    row_quality_final,
    name = "n"
  ) %>%
  group_by(
    comparison_region,
    subsample
  ) %>%
  mutate(
    prop = n / sum(n)
  ) %>%
  ungroup() %>%
  arrange(
    comparison_region,
    subsample,
    desc(n)
  )


# DIAGNÓSTICOS DE CALIDAD DE METADATOS POR FUENTE

diagnostics_metadata_quality_counts <- df_quality %>%
  count(
    dataset_source,
    metadata_quality,
    name = "n"
  ) %>%
  group_by(dataset_source) %>%
  mutate(
    prop = n / sum(n)
  ) %>%
  ungroup() %>%
  arrange(
    dataset_source,
    desc(n)
  )


# DIAGNÓSTICOS DE CALIDAD DE METADATOS POR SUBMUESTRA

diagnostics_metadata_quality_counts_by_subsample <-
  df_quality %>%
  count(
    comparison_region,
    subsample,
    dataset_source,
    metadata_quality,
    name = "n"
  ) %>%
  group_by(
    comparison_region,
    subsample
  ) %>%
  mutate(
    prop = n / sum(n)
  ) %>%
  ungroup() %>%
  arrange(
    comparison_region,
    subsample,
    desc(n)
  )


# DISPONIBILIDAD PARA PROPENSITY POR FUENTE

diagnostics_propensity_score_readiness <- df_quality %>%
  group_by(dataset_source) %>%
  summarise(
    n_rows = n(),
    
    n_has_binary_vote_outcome = sum(
      has_propensity_outcome_binary,
      na.rm = TRUE
    ),
    
    prop_has_binary_vote_outcome =
      n_has_binary_vote_outcome / n_rows,
    
    n_has_age = sum(
      has_propensity_age,
      na.rm = TRUE
    ),
    
    prop_has_age = n_has_age / n_rows,
    
    n_has_education = sum(
      has_propensity_education,
      na.rm = TRUE
    ),
    
    prop_has_education =
      n_has_education / n_rows,
    
    n_has_income = sum(
      has_propensity_income,
      na.rm = TRUE
    ),
    
    prop_has_income = n_has_income / n_rows,
    
    n_has_country = sum(
      has_propensity_country,
      na.rm = TRUE
    ),
    
    prop_has_country = n_has_country / n_rows,
    
    n_has_city_size = sum(
      has_propensity_city_size,
      na.rm = TRUE
    ),
    
    prop_has_city_size =
      n_has_city_size / n_rows,
    
    n_has_employment = sum(
      has_propensity_employment,
      na.rm = TRUE
    ),
    
    prop_has_employment = n_has_employment / n_rows,
    
    mean_n_propensity_predictors_available = mean(
      n_propensity_predictors_available,
      na.rm = TRUE
    ),
    
    n_predictors_strict_complete = sum(
      usable_for_propensity_score_strict,
      na.rm = TRUE
    ),
    
    prop_predictors_strict_complete =
      n_predictors_strict_complete / n_rows,
    
    n_predictors_complete_without_income = sum(
      usable_for_propensity_score_without_income,
      na.rm = TRUE
    ),
    
    prop_predictors_complete_without_income =
      n_predictors_complete_without_income / n_rows,
    
    n_predictors_minimal = sum(
      usable_for_propensity_score_minimal,
      na.rm = TRUE
    ),
    
    prop_predictors_minimal =
      n_predictors_minimal / n_rows,
    
    n_model_ready_strict = sum(
      usable_for_propensity_model_strict,
      na.rm = TRUE
    ),
    
    prop_model_ready_strict =
      n_model_ready_strict / n_rows,
    
    n_model_ready_without_income = sum(
      usable_for_propensity_model_without_income,
      na.rm = TRUE
    ),
    
    prop_model_ready_without_income =
      n_model_ready_without_income / n_rows,
    
    n_model_ready_minimal = sum(
      usable_for_propensity_model_minimal,
      na.rm = TRUE
    ),
    
    prop_model_ready_minimal =
      n_model_ready_minimal / n_rows,
    
    .groups = "drop"
  )


# DISPONIBILIDAD PARA PROPENSITY POR SUBMUESTRA

diagnostics_propensity_score_readiness_by_subsample <-
  df_quality %>%
  group_by(
    comparison_region,
    subsample,
    dataset_source
  ) %>%
  summarise(
    n_rows = n(),
    
    n_eu_applicable = sum(
      propensity_eu_applicable,
      na.rm = TRUE
    ),
    
    prop_eu_applicable =
      n_eu_applicable / n_rows,
    
    n_has_binary_vote_outcome = sum(
      has_propensity_outcome_binary,
      na.rm = TRUE
    ),
    
    n_predictors_strict_complete = sum(
      usable_for_propensity_score_strict,
      na.rm = TRUE
    ),
    
    n_predictors_complete_without_income = sum(
      usable_for_propensity_score_without_income,
      na.rm = TRUE
    ),
    
    n_predictors_minimal = sum(
      usable_for_propensity_score_minimal,
      na.rm = TRUE
    ),
    
    n_model_ready_strict = sum(
      usable_for_propensity_model_strict,
      na.rm = TRUE
    ),
    
    n_model_ready_without_income = sum(
      usable_for_propensity_model_without_income,
      na.rm = TRUE
    ),
    
    n_model_ready_minimal = sum(
      usable_for_propensity_model_minimal,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  arrange(
    comparison_region,
    subsample
  )


# CALIDAD DE FILAS POR FUENTE

diagnostics_row_quality_by_source <- row_quality_summary(
  df_quality,
  "dataset_source"
)


# CALIDAD DE FILAS POR SUBMUESTRA

diagnostics_row_quality_by_subsample <- row_quality_summary(
  df_quality,
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


# TAMAÑOS DISPONIBLES PARA CLUSTERING

diagnostics_clustering_sample_sizes <-
  diagnostics_row_quality_by_subsample %>%
  transmute(
    analysis_sample = subsample,
    comparison_region = comparison_region,
    n_total = n_rows,
    
    n_usable_for_clustering =
      n_usable_for_clustering,
    
    prop_usable_for_clustering =
      prop_usable_for_clustering
  ) %>%
  bind_rows(
    df_quality %>%
      summarise(
        analysis_sample = "POOLED_ALL",
        comparison_region = "ALL",
        n_total = n(),
        
        n_usable_for_clustering = sum(
          usable_for_clustering,
          na.rm = TRUE
        ),
        
        prop_usable_for_clustering =
          n_usable_for_clustering / n_total
      )
  ) %>%
  arrange(
    desc(analysis_sample == "POOLED_ALL"),
    analysis_sample
  )


# CALIDAD POR COMPONENTE

component_quality_cols <- names(df_quality)[
  str_detect(
    names(df_quality),
    "^quality_"
  )
]

diagnostics_component_quality_counts <-
  component_quality_counts(
    df_quality,
    count_vars = "dataset_source",
    prop_vars = "dataset_source",
    quality_cols = component_quality_cols
  ) %>%
  arrange(
    dataset_source,
    component,
    desc(n)
  )


# CALIDAD POR COMPONENTE Y SUBMUESTRA

diagnostics_component_quality_counts_by_subsample <-
  component_quality_counts(
    df_quality,
    count_vars = c(
      "comparison_region",
      "subsample",
      "dataset_source"
    ),
    prop_vars = c(
      "comparison_region",
      "subsample"
    ),
    quality_cols = component_quality_cols
  ) %>%
  arrange(
    comparison_region,
    subsample,
    component,
    desc(n)
  )


# COBERTURA POR COMPONENTE

component_n_cols <- names(df_quality)[
  str_detect(
    names(df_quality),
    "^n_.*_non_missing$"
  )
]

diagnostics_component_coverage_by_source <-
  component_coverage(
    df_quality,
    "dataset_source",
    component_n_cols
  ) %>%
  arrange(
    dataset_source,
    component_n_variable
  )


# COBERTURA POR COMPONENTE Y SUBMUESTRA

diagnostics_component_coverage_by_subsample <-
  component_coverage(
    df_quality,
    c(
      "comparison_region",
      "subsample",
      "dataset_source"
    ),
    component_n_cols
  ) %>%
  arrange(
    comparison_region,
    subsample,
    component_n_variable
  )


# GUARDADO DE ARCHIVOS

outputs <- list(
  "all_sources_integrated_component_quality.csv" =
    df_quality,
  
  "matrix_32det_with_quality.csv" =
    matrix_32det_with_quality,
  
  "matrix_32det_for_clustering.csv" =
    matrix_32det_for_clustering,
  
  "diagnostics_component_candidate_columns.csv" =
    diagnostics_component_candidate_columns,
  
  "diagnostics_component_quality_counts.csv" =
    diagnostics_component_quality_counts,
  
  "diagnostics_component_coverage_by_source.csv" =
    diagnostics_component_coverage_by_source,
  
  "diagnostics_row_quality_by_source.csv" =
    diagnostics_row_quality_by_source,
  
  "diagnostics_row_quality_counts.csv" =
    diagnostics_row_quality_counts,
  
  "diagnostics_missing_by_determinant.csv" =
    diagnostics_missing_by_determinant,
  
  "diagnostics_missing_by_determinant_by_subsample.csv" =
    diagnostics_missing_by_determinant_by_subsample,
  
  "diagnostics_missing_after_imputation_by_determinant.csv" =
    diagnostics_missing_after_imputation_by_determinant,
  
  "diagnostics_missing_after_imputation_by_subsample.csv" =
    diagnostics_missing_after_imputation_by_subsample,
  
  "diagnostics_imputation_50_by_determinant.csv" =
    diagnostics_imputation_50_by_determinant,
  
  "diagnostics_imputation_50_by_subsample.csv" =
    diagnostics_imputation_50_by_subsample,
  
  "diagnostics_imputation_50_by_row.csv" =
    diagnostics_imputation_50_by_row,
  
  "diagnostics_imputation_50_summary.csv" =
    diagnostics_imputation_50_summary,
  
  "diagnostics_metadata_quality_counts.csv" =
    diagnostics_metadata_quality_counts,
  
  "diagnostics_propensity_score_readiness.csv" =
    diagnostics_propensity_score_readiness,
  
  "diagnostics_row_quality_by_subsample.csv" =
    diagnostics_row_quality_by_subsample,
  
  "diagnostics_row_quality_counts_by_subsample.csv" =
    diagnostics_row_quality_counts_by_subsample,
  
  "diagnostics_metadata_quality_counts_by_subsample.csv" =
    diagnostics_metadata_quality_counts_by_subsample,
  
  "diagnostics_component_quality_counts_by_subsample.csv" =
    diagnostics_component_quality_counts_by_subsample,
  
  "diagnostics_component_coverage_by_subsample.csv" =
    diagnostics_component_coverage_by_subsample,
  
  "diagnostics_propensity_score_readiness_by_subsample.csv" =
    diagnostics_propensity_score_readiness_by_subsample,
  
  "diagnostics_clustering_sample_sizes.csv" =
    diagnostics_clustering_sample_sizes
)

# Guardar todos los CSV en el directorio de esta etapa.
iwalk(
  outputs,
  ~ write_csv(
    .x,
    file.path(out_dir, .y)
  )
)


# RESUMEN EN CONSOLA

cat("\nIMPUTACIÓN DE DETERMINANTES CON 50 POR SUBMUESTRA\n")

print(
  diagnostics_imputation_50_summary,
  n = Inf
)

cat("\nCALIDAD PARA CLUSTERING POR SUBMUESTRA\n")

print(
  diagnostics_row_quality_by_subsample,
  n = Inf
)

cat("\nTAMAÑOS DISPONIBLES PARA CLUSTERING\n")

print(
  diagnostics_clustering_sample_sizes,
  n = Inf
)

cat("\nPROPENSITY POLÍTICO: APLICABILIDAD\n")

print(
  diagnostics_propensity_score_readiness_by_subsample,
  n = Inf
)

message(
  "\nListo. Resultados guardados en: ",
  out_dir
)