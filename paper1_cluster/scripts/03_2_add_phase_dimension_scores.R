# 03_2_phase_dimension_scores.R
#
# OBJETIVO
# Relacionar las razones que los participantes de RENOVISOR seleccionan
# en cada fase de decisión con las nueve dimensiones motivacionales
# definidas en el proyecto.
#
# Para cada participante y fase, el script identifica las dimensiones
# seleccionadas y calcula un score para cada una de ellas. Este score
# representa la valoración media que el participante ha asignado a
# los determinantes asociados a esa dimensión, en una escala de 0 a 100.
#
# También calcula un score agregado de fase: la media de todos los
# determinantes válidos asociados a las dimensiones seleccionadas.
# Este agregado resume la valoración de los determinantes vinculados
# a las razones elegidas por el participante en esa fase.
#
# Los scores se incorporan a la base integrada de calidad como variables
# derivadas para los análisis posteriores. Se conservan las cuatro
# submuestras, aunque las preguntas de fase proceden de RV Decision
# y, por tanto, se esperan principalmente en RENOVISOR.
#
# ENTRADA
# paper1_cluster/data/processed/03_1_component_quality/
# all_sources_integrated_component_quality.csv
#
# Contiene las cuatro submuestras:
# - DIEGO
# - RENOVISOR
# - WHY_EUROPE
# - WHY_LATAM
#
# FASES ANALIZADAS
# - phase_implemented_reasons:
#   Razones que llevaron a implementar o contratar la tecnología.
#
# - phase_more_likely_1: --> 
#   Razones que aumentarían la disposición a implementar o contratar
#   la tecnología, correspondientes a la primera rama del cuestionario.
#
# - phase_more_likely_2:
#   Razones que aumentarían la disposición a implementar o contratar
#   la tecnología, correspondientes a la segunda rama del cuestionario.
#
# Las tres fases se identifican mediante sus columnas originales de
# RV Decision. Las respuestas de fase se esperan principalmente en
# RENOVISOR, pero se conservan todas las filas de DIEGO y WHY.
#
# DIMENSIONES ANALIZADAS
# - FINANCIAL
# - SECURITY
# - COMPETENCE
# - AUTONOMY
# - PHYSIOLOGICAL
# - RELATEDNESS
# - STIMULATION
# - POPULARITY
# - MEANING
#
# # Los 32 determinantes se asignan a estas nueve dimensiones mediante
# el diccionario dimension_determinants definido en 00_common.R.
# Este script utiliza ese mapeo para calcular los scores de fase.
#
# PROCESAMIENTO
# 1. Leer la base integrada de calidad y comprobar su estructura,
#    sus identificadores y la presencia de las cuatro submuestras.
#
# 2. Identificar las tres columnas originales de RV Decision que
#    contienen las dimensiones seleccionadas en cada fase.
#
# 3. Definir la correspondencia entre las nueve dimensiones y los
#    32 determinantes armonizados. Comprobar que las columnas existen
#    y que el mapeo contiene nueve dimensiones y 32 determinantes únicos.
#
# 4. Convertir los determinantes a números e identificar los valores
#    válidos dentro de la escala 0-100.
#
# 5. Para cada fase y participante, detectar las dimensiones mencionadas
#    en la respuesta original y registrar cuáles se han seleccionado.
#
# 6. Identificar los determinantes asociados a las dimensiones
#    seleccionadas y construir sus indicadores de selección y sus
#    valores ponderados.
#
# 7. Calcular el score de cada dimensión seleccionada como la media
#    de sus determinantes válidos.
#
# 8. Calcular un score agregado de fase como la media de todos los
#    determinantes válidos asociados a las dimensiones seleccionadas.
#    En este agregado, cada determinante tiene el mismo peso.
#
# 9. Evaluar la calidad de cada fase a partir del número de
#    determinantes esperados y válidos entre los seleccionados.
#
# 10. Resumir, por participante, cuántos bloques de fase tienen
#     dimensiones reconocidas y cuántos presentan calidad completa
#     o parcialmente utilizable.
#
# 11. Añadir las nuevas variables a la base de entrada sin eliminar
#     participantes ni modificar los identificadores o su orden.
#
# 12. Generar diagnósticos de integridad, cobertura, frecuencia de
#     dimensiones, calidad y distribución de los scores por fuente
#     y por submuestra.
#
# CRITERIOS DE CÁLCULO
# - Una dimensión se identifica buscando su nombre en la respuesta
#   original del bloque de fase.
#
# - Una dimensión no seleccionada recibe NA en su columna de score.
#
# - El score de una dimensión seleccionada es la media de sus
#   determinantes válidos, siempre que tenga al menos uno disponible.
#
# - El score agregado de fase es la media de todos los determinantes
#   válidos pertenecientes a las dimensiones seleccionadas.
#
# - El score agregado no es la media de los nueve scores de dimensión:
#   cada determinante seleccionado contribuye individualmente.
#
# - Los scores se calculan sobre los determinantes recibidos desde
#   03_1_component_quality.R. Por tanto, pueden incluir valores
#   imputados a 50 en aquella etapa.
#
# CALIDAD DE CADA FASE
# - no_dimension_selected:
#   No se ha identificado ninguna dimensión seleccionada.
#
# - no_determinants_mapped:
#   Se han seleccionado dimensiones, pero no se han identificado
#   determinantes asociados.
#
# - complete:
#   Todos los determinantes esperados están disponibles y son válidos.
#
# - usable_partial:
#   Está disponible al menos el 75 % de los determinantes esperados.
#
# - too_many_missing:
#   Hay determinantes válidos, pero su proporción es inferior al 75 %.
#
# - no_valid_determinants:
#   No hay determinantes válidos entre los seleccionados.
#
# IMPORTANTE
# - Este script genera variables derivadas y diagnósticos; no realiza
#   clustering ni modifica usable_for_clustering.
#
# - No elimina filas por ausencia de respuesta de fase.
#
# - La variable *_has_response indica que se ha reconocido al menos
#   una dimensión. Una respuesta original informada que no contiene
#   ningún nombre de dimensión reconocido no activa ese indicador.
#
# - Las columnas *_weighted_* contienen el valor del determinante si
#   está seleccionado, 0 si no está seleccionado y NA cuando no se ha
#   reconocido ninguna dimensión en la fase.
#
# - El valor 0 de una columna *_weighted_* puede representar ausencia
#   de selección. Para distinguirlo de un determinante seleccionado
#   cuyo valor real es 0, debe consultarse *_det_weight_*.
#
# SALIDAS
# Directorio:
# paper1_cluster/data/processed/03_2_phase_dimension_scores/
#
# BASE PRINCIPAL
# - all_sources_integrated_component_quality_phase_scores.csv:
#   Base completa de calidad enriquecida con variables de las
#   tres fases, dimensiones seleccionadas y scores.
#
# DICCIONARIOS
# - dimension_determinant_mapping.csv:
#   Correspondencia entre las nueve dimensiones y los 32 determinantes.
#
# - phase_dimension_source_columns.csv:
#   Identificadores de las fases y columnas originales utilizadas.
#
# DIAGNÓSTICOS
# - diagnostics_dataset_integrity.csv
# - diagnostics_subsample_counts.csv
# - diagnostics_subsample_integrity.csv
# - diagnostics_phase_columns.csv
# - diagnostics_phase_columns_by_subsample.csv
# - diagnostics_phase_columns_by_source.csv
# - diagnostics_phase_response_by_sample.csv
# - diagnostics_phase_dimension_frequencies.csv
# - diagnostics_phase_dimension_frequencies_by_subsample.csv
# - diagnostics_phase_quality_counts_by_subsample.csv
# - diagnostics_phase_quality_counts_by_source.csv
# - diagnostics_phase_scores_summary_by_subsample.csv
# - diagnostics_phase_scores_summary_by_source.csv
#
# VARIABLES PRINCIPALES GENERADAS PARA CADA FASE
#
# *_source_column:
#   Nombre de la columna original utilizada para esa fase.
#
# *_phase_label:
#   Descripción del bloque de decisión.
#
# *_raw_response:
#   Respuesta original del participante, después de limpiar los
#   códigos de ausencia definidos en este script.
#
# *_has_response:
#   TRUE si se reconoce al menos una dimensión en la respuesta.
#
# *_selected_dimensions:
#   Nombres de las dimensiones reconocidas, separados por "|".
#
# *_n_selected_dimensions:
#   Número de dimensiones reconocidas.
#
# *_selected_dim_*:
#   Indicador TRUE/FALSE de selección de cada una de las nueve dimensiones.
#
# *_det_weight_*:
#   Indicador 1/0 de pertenencia de cada determinante a una de las
#   dimensiones seleccionadas. Es NA si no hay dimensiones reconocidas.
#
# *_weighted_*:
#   Valor del determinante cuando está seleccionado, 0 cuando no lo está
#   y NA si no hay dimensiones reconocidas en el bloque.
#
# *_dim_score_*:
#   Media de los determinantes válidos de cada dimensión seleccionada.
#   Es NA si la dimensión no está seleccionada o no tiene valores válidos.
#
# *_n_selected_determinants_expected:
#   Número total de determinantes asociados a las dimensiones seleccionadas.
#
# *_n_selected_determinants_valid:
#   Número de esos determinantes con valor válido en la escala 0-100.
#
# *_prop_selected_determinants_valid:
#   Proporción de determinantes seleccionados que tienen valor válido.
#
# *_selected_dimension_score_mean:
#   Media de todos los determinantes válidos asociados a las
#   dimensiones seleccionadas.
#
# *_quality:
#   Clasificación de calidad del bloque de fase.
#
# VARIABLES GLOBALES GENERADAS
#
# n_phase_dimension_blocks_with_response:
#   Número de bloques de fase con al menos una dimensión reconocida.
#
# n_phase_dimension_blocks_complete:
#   Número de bloques clasificados como complete.
#
# n_phase_dimension_blocks_usable:
#   Número de bloques clasificados como complete o usable_partial.
#
# quality_phase_dimension_block:
#   Clasificación conjunta de la disponibilidad y calidad de
#   los bloques de fase de cada participante.
#
# DEPENDENCIAS
# - 00_common.R: paquetes, rutas y funciones comunes del proyecto.
# - 01_mergeData.R: genera la base integrada original.
# - 02_harmonize_common_variables.R: genera las variables armonizadas.
# - 03_1_component_quality.R: genera el archivo de entrada.

################################################################################

# CARGA DE 00_common.R

# Buscar el archivo común junto al script o desde el directorio de trabajo,
# evitando rutas absolutas específicas de un ordenador.

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

# suppressPackageStartupMessages({
#   library(tidyverse)
# })
# Las librerías se cargan desde 00_common.R.


# CONFIGURACIÓN

# processed_root se define en 00_common.R.
# processed_root <- "paper1_cluster/data/processed"

in_file <- file.path(
  processed_root,
  "03_1_component_quality",
  "all_sources_integrated_component_quality.csv"
)

out_dir <- file.path(
  processed_root,
  "03_2_phase_dimension_scores"
)

out_file <- file.path(
  out_dir,
  "all_sources_integrated_component_quality_phase_scores.csv"
)

check_file(in_file)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# Leer inicialmente todas las columnas como caracteres para conservar
# los formatos originales y convertir explícitamente las variables
# numéricas necesarias para el cálculo de los scores.

df <- read_csv(
  in_file,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)


# COMPROBACIONES INICIALES

# Comprobar que están disponibles los identificadores y las variables
# necesarias para conservar la trazabilidad entre la entrada y la salida.

required_traceability_cols <- c(
  "integrated_row_id",
  "subsample",
  "comparison_region",
  "dataset_source",
  "source_survey"
)

missing_traceability_cols <- setdiff(
  required_traceability_cols,
  names(df)
)

if (length(missing_traceability_cols)) {
  stop(
    "Faltan columnas de trazabilidad: ",
    paste(missing_traceability_cols, collapse = ", ")
  )
}

# Comprobar que la base contiene las cuatro submuestras esperadas.
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

# Cada integrated_row_id debe identificar una única fila.

if (anyDuplicated(df$integrated_row_id)) {
  stop("integrated_row_id contiene duplicados en el input.")
}

# Conservar los identificadores originales para verificar al final
# que el enriquecimiento no modifica las filas ni su orden.

input_traceability <- df %>%
  select(all_of(required_traceability_cols))


# FUNCIONES AUXILIARES

# Limpia espacios y transforma los códigos de ausencia en NA.
#
# x: vector de respuestas originales.
# Devuelve: vector de caracteres depurado.
#
# Se conserva localmente la misma definición utilizada en 03_1,
# para mantener sus reglas de tratamiento de respuestas ausentes.

# clean_text <- function(x) {
#   x <- str_squish(as.character(x))
#   
#   invalid_values <- c(
#     "",
#     "NA",
#     "NaN",
#     "NULL",
#     "null",
#     "None",
#     "none",
#     "DATA_EXPIRED",
#     "data_expired"
#   )
#   
#   x[is.na(x) | x %in% invalid_values] <- NA_character_
#   x
# }


# Convierte respuestas numéricas utilizando parse_num() de 00_common.R.
#
# x: vector de respuestas.
# Devuelve: vector numérico, con NA para valores no convertibles.
#
# Se mantiene el nombre parse_num_clean() para conservar las llamadas
# existentes en este script.
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


# Clasifica la calidad de una fase según el número de dimensiones
# seleccionadas y la disponibilidad de sus determinantes.
#
# n_valid: número de determinantes seleccionados con valor válido.
# n_expected: número total de determinantes seleccionados esperados.
# n_dims_selected: número de dimensiones seleccionadas.
#
# Devuelve: vector de categorías de calidad.
#
# Un bloque es completo si todos sus determinantes son válidos.
# Es parcialmente utilizable si dispone de al menos el 75 %.

make_quality_label <- function(
    n_valid,
    n_expected,
    n_dims_selected
) {
  prop_valid <- if_else(
    n_expected > 0,
    n_valid / n_expected,
    NA_real_
  )
  
  case_when(
    n_dims_selected == 0 ~ "no_dimension_selected",
    n_expected == 0 ~ "no_determinants_mapped",
    n_valid == n_expected & n_expected > 0 ~ "complete",
    prop_valid >= 0.75 ~ "usable_partial",
    prop_valid > 0 ~ "too_many_missing",
    TRUE ~ "no_valid_determinants"
  )
}


# IDENTIFICACIÓN DE LAS TRES FASES

# Cada fila indica:
# phase_prefix: prefijo utilizado para las nuevas variables.
# phase_label: descripción de la pregunta.
# source_column: columna original de RV Decision.

phase_specs <- tribble(
  ~phase_prefix, ~phase_label, ~source_column,
  
  "phase_implemented_reasons",
  "Reasons that led to implement or contract the selected technology",
  "rv_decision__what_were_the_reasons_that_led_you_to_implement_or_contract_the_selected_technology_or_energy_related_measure_please_select_the_3_most_important_for_you",
  
  "phase_more_likely_1",
  "What would make respondent more likely to implement or contract this technology - branch 1",
  "rv_decision__what_would_make_you_more_likely_to_implement_or_contract_this_technology_or_measure_please_select_the_3_most_important_for_you",
  
  "phase_more_likely_2",
  "What would make respondent more likely to implement or contract this technology - branch 2",
  "rv_decision__what_would_make_you_more_likely_to_implement_or_contract_this_technology_or_measure_please_select_the_3_most_important_for_you_1"
)

# No calcular scores si falta alguna de las columnas originales
# necesarias para identificar las fases.

missing_phase_cols <- setdiff(
  phase_specs$source_column,
  names(df)
)

if (length(missing_phase_cols)) {
  stop(
    "Faltan columnas de fase/dimensión: ",
    paste(missing_phase_cols, collapse = ", ")
  )
}


# CORRESPONDENCIA ENTRE DIMENSIONES Y DETERMINANTES

# El diccionario conserva el mapeo de las nueve dimensiones
# y los 32 determinantes del script original.
#
# Cada determinante aparece en una única dimensión.
# dimension_determinants <- list(
#   FINANCIAL = c(
#     "det_01_profits",
#     "det_02_credit_score",
#     "det_03_risk_profile",
#     "det_04_added_value",
#     "det_05_frugality"
#   ),
#   
#   SECURITY = c(
#     "det_07_legal",
#     "det_08_trust",
#     "det_09_safety"
#   ),
#   
#   COMPETENCE = c(
#     "det_10_cost_efficiency",
#     "det_11_knowledge",
#     "det_12_own_competence",
#     "det_13_technical_fit"
#   ),
#   
#   AUTONOMY = c(
#     "det_15_self_satisfaction",
#     "det_16_commitment",
#     "det_17_adherence",
#     "det_18_autonomy"
#   ),
#   
#   PHYSIOLOGICAL = c(
#     "det_19_wellbeing",
#     "det_20_coziness"
#   ),
#   
#   RELATEDNESS = c(
#     "det_21_rights_and_duties",
#     "det_22_peer_pressure",
#     "det_23_support",
#     "det_24_socialising",
#     "det_25_agreement"
#   ),
#   
#   STIMULATION = c(
#     "det_26_novelty",
#     "det_27_fun"
#   ),
#   
#   POPULARITY = c(
#     "det_28_recognition",
#     "det_29_trends",
#     "det_30_authority",
#     "det_31_approval"
#   ),
#   
#   MEANING = c(
#     "det_06_climate_protection",
#     "det_14_environmental_concerns",
#     "det_32_own_significance"
#   )
# )

# Conservar el orden definido en el diccionario.

dimension_levels <- names(dimension_determinants)

# Convertir la lista en una tabla con una fila por pareja
# dimensión-determinante para facilitar la trazabilidad del mapeo.

dimension_determinant_map <- enframe(
  dimension_determinants,
  name = "dimension",
  value = "det_col"
) %>%
  unnest_longer(det_col)

det_cols <- dimension_determinant_map$det_col

# Comprobar que existen todos los determinantes requeridos.

missing_det_cols <- setdiff(
  det_cols,
  names(df)
)

if (length(missing_det_cols)) {
  stop(
    "Faltan determinantes armonizados: ",
    paste(missing_det_cols, collapse = ", ")
  )
}

# Comprobar que el diccionario contiene 32 determinantes únicos
# distribuidos entre nueve dimensiones.

if (n_distinct(det_cols) != N_DET_TOTAL) {
  stop(
    "El mapeo no contiene exactamente ",
    N_DET_TOTAL,
    " determinantes únicos."
  )
}

if (n_distinct(dimension_determinant_map$dimension) != 9) {
  stop("El mapeo no contiene exactamente 9 dimensiones.")
}


# MATRIZ NUMÉRICA DE LOS 32 DETERMINANTES

# Recuperar los determinantes posteriores a la etapa de calidad.
# Los valores imputados con 50 en 03_1 ya están incorporados en el input.

det_mat <- df %>%
  select(all_of(det_cols)) %>%
  mutate(across(everything(), parse_num)) %>%
  as.matrix()

colnames(det_mat) <- det_cols

# TRUE identifica los determinantes con valor numérico válido en 0-100.

valid_det_mat <-
  !is.na(det_mat) &
  det_mat >= 0 &
  det_mat <= 100

# Registrar a qué dimensión pertenece cada columna de determinante,
# respetando el orden de las columnas de det_mat.

det_dimensions <- dimension_determinant_map$dimension[
  match(
    det_cols,
    dimension_determinant_map$det_col
  )
]


# FUNCIÓN PARA GENERAR LAS VARIABLES DE UNA FASE

# Genera las variables derivadas de una fase para todos los participantes.
#
# data: tabla de entrada, con las respuestas originales de la fase.
# phase_prefix: prefijo que recibirán las columnas generadas.
# phase_label: descripción de la fase.
# phase_col: nombre de la columna original de RV Decision.
#
# Utiliza también:
# - dimension_levels: nombres de las nueve dimensiones.
# - dimension_determinants: determinantes de cada dimensión.
# - det_dimensions: dimensión de cada columna de det_mat.
# - det_mat: matriz numérica de determinantes.
# - valid_det_mat: matriz lógica de validez de los determinantes.
#
# Devuelve: tibble con indicadores de selección, scores por dimensión,
# pesos, determinantes ponderados y diagnóstico de calidad de la fase.

add_phase_features <- function(
    data,
    phase_prefix,
    phase_label,
    phase_col
) {
  # Recuperar la respuesta original y preparar una versión en mayúsculas
  # para detectar los nombres de las dimensiones.
  
  raw_response <- clean_text_quality(
    data[[phase_col]]
  )
  
  response_upper <- str_to_upper(
    coalesce(
      raw_response,
      ""
    )
  )
  
  # Detectar las dimensiones seleccionadas.
  # Cada columna del resultado corresponde a una dimensión.
  
  selected_dim_df <- map_dfc(
    dimension_levels,
    function(dim_current) {
      tibble(
        !!dim_current := str_detect(
          response_upper,
          regex(
            paste0(
              "\\b",
              dim_current,
              "\\b"
            ),
            ignore_case = FALSE
          )
        )
      )
    }
  )
  
  selected_dim_mat <- as.matrix(
    selected_dim_df
  )
  
  # Número de dimensiones reconocidas por participante.
  
  n_dims_selected <- rowSums(
    selected_dim_mat,
    na.rm = TRUE
  )
  
  # Guardar sus nombres en una única columna de texto.
  
  selected_dims_collapsed <- apply(
    selected_dim_mat,
    1,
    function(x) {
      selected <- dimension_levels[
        as.logical(x)
      ]
      
      if (!length(selected)) {
        return(NA_character_)
      }
      
      paste(
        selected,
        collapse = "|"
      )
    }
  )
  
  # Una fase se considera respondida cuando se reconoce
  # al menos una de las nueve dimensiones.
  
  has_phase_response <- n_dims_selected > 0
  
  # MATRIZ DE PESOS DE LOS DETERMINANTES
  
  # Asignar 1 a los determinantes cuyas dimensiones se han seleccionado
  # y 0 a los demás.
  #
  # Cuando no se reconoce ninguna dimensión, toda la fila recibe NA.
  
  weight_mat <- vapply(
    det_dimensions,
    function(dim_current) {
      as.numeric(selected_dim_df[[dim_current]])
    },
    numeric(nrow(data))
  )
  
  colnames(weight_mat) <- det_cols
  
  weight_mat[!has_phase_response, ] <- NA_real_
  
  # MATRIZ DE VALORES PONDERADOS
  
  # Conservar el valor del determinante cuando su peso es 1.
  # Asignar 0 cuando el peso es 0 y NA cuando el peso es NA.
  
  weighted_mat <- ifelse(
    is.na(weight_mat),
    NA_real_,
    ifelse(
      weight_mat == 1,
      det_mat,
      0
    )
  )
  
  # Convertir las matrices anteriores en tablas.
  # Los nombres incluyen el prefijo de fase y el determinante
  # para evitar colisiones entre las tres fases.
  
  weight_df <- as_tibble(
    weight_mat
  )
  
  names(weight_df) <- paste0(
    phase_prefix,
    "_det_weight_",
    det_cols
  )
  
  weighted_df <- as_tibble(
    weighted_mat
  )
  
  names(weighted_df) <- paste0(
    phase_prefix,
    "_weighted_",
    det_cols
  )
  
  # SCORES DE LAS NUEVE DIMENSIONES
  
  # Para cada dimensión:
  # - Recuperar sus determinantes.
  # - Contar los que tienen un valor válido.
  # - Calcular la media de los valores válidos.
  # - Conservar el score únicamente si la dimensión fue seleccionada.
  
  dim_score_df <- map_dfc(
    dimension_levels,
    function(dim_current) {
      dim_cols <- dimension_determinants[[dim_current]]
      
      dim_values <- det_mat[
        ,
        dim_cols,
        drop = FALSE
      ]
      
      dim_valid <- valid_det_mat[
        ,
        dim_cols,
        drop = FALSE
      ]
      
      n_valid_dim <- rowSums(
        dim_valid,
        na.rm = TRUE
      )
      
      dim_sum <- rowSums(
        ifelse(
          dim_valid,
          dim_values,
          0
        ),
        na.rm = TRUE
      )
      
      score_dim <- rep(
        NA_real_,
        nrow(data)
      )
      
      valid_rows <- n_valid_dim > 0
      
      score_dim[valid_rows] <-
        dim_sum[valid_rows] /
        n_valid_dim[valid_rows]
      
      score_selected <- ifelse(
        selected_dim_df[[dim_current]],
        score_dim,
        NA_real_
      )
      
      tibble(
        !!paste0(
          phase_prefix,
          "_dim_score_",
          str_to_lower(dim_current)
        ) := score_selected
      )
    }
  )
  
  # COBERTURA DE LOS DETERMINANTES SELECCIONADOS
  
  # Número de determinantes esperados según las dimensiones
  # seleccionadas, independientemente de si tienen valor disponible.
  
  selected_det_expected <- rowSums(
    weight_mat == 1,
    na.rm = TRUE
  )
  
  # Número de determinantes seleccionados cuyo valor es válido en 0-100.
  
  selected_det_valid <- rowSums(
    weight_mat == 1 &
      valid_det_mat,
    na.rm = TRUE
  )
  
  # Suma de los valores válidos de todos los determinantes seleccionados.
  
  selected_det_score_sum <- rowSums(
    ifelse(
      weight_mat == 1 &
        valid_det_mat,
      det_mat,
      0
    ),
    na.rm = TRUE
  )
  
  # SCORE AGREGADO DE LA FASE
  
  # Calcular la media de todos los determinantes válidos seleccionados.
  # Las dimensiones con más determinantes aportan más valores al agregado,
  # porque la media se calcula por determinante y no por dimensión.
  
  selected_dimension_score_mean <- rep(
    NA_real_,
    nrow(data)
  )
  
  valid_score_rows <- selected_det_valid > 0
  
  selected_dimension_score_mean[
    valid_score_rows
  ] <-
    selected_det_score_sum[
      valid_score_rows
    ] /
    selected_det_valid[
      valid_score_rows
    ]
  
  # Proporción de determinantes válidos respecto al total esperado
  # según las dimensiones seleccionadas.
  
  selected_det_prop_valid <- ifelse(
    selected_det_expected > 0,
    selected_det_valid /
      selected_det_expected,
    NA_real_
  )
  
  # CLASIFICACIÓN DE CALIDAD DE LA FASE
  
  phase_quality <- make_quality_label(
    selected_det_valid,
    selected_det_expected,
    n_dims_selected
  )
  
  # INDICADORES INDIVIDUALES DE DIMENSIONES SELECCIONADAS
  
  selected_dim_flags <- selected_dim_df %>%
    rename_with(
      ~ paste0(
        phase_prefix,
        "_selected_dim_",
        str_to_lower(.x)
      )
    )
  
  # TABLA RESUMEN DE LA FASE
  
  phase_summary_df <- tibble(
    !!paste0(
      phase_prefix,
      "_source_column"
    ) := rep(
      phase_col,
      nrow(data)
    ),
    
    !!paste0(
      phase_prefix,
      "_phase_label"
    ) := rep(
      phase_label,
      nrow(data)
    ),
    
    !!paste0(
      phase_prefix,
      "_raw_response"
    ) := raw_response,
    
    !!paste0(
      phase_prefix,
      "_has_response"
    ) := has_phase_response,
    
    !!paste0(
      phase_prefix,
      "_selected_dimensions"
    ) := selected_dims_collapsed,
    
    !!paste0(
      phase_prefix,
      "_n_selected_dimensions"
    ) := n_dims_selected,
    
    !!paste0(
      phase_prefix,
      "_n_selected_determinants_expected"
    ) := selected_det_expected,
    
    !!paste0(
      phase_prefix,
      "_n_selected_determinants_valid"
    ) := selected_det_valid,
    
    !!paste0(
      phase_prefix,
      "_prop_selected_determinants_valid"
    ) := selected_det_prop_valid,
    
    !!paste0(
      phase_prefix,
      "_selected_dimension_score_mean"
    ) := selected_dimension_score_mean,
    
    !!paste0(
      phase_prefix,
      "_quality"
    ) := phase_quality
  )
  
  # Devolver todas las variables generadas para esta fase.
  
  bind_cols(
    phase_summary_df,
    selected_dim_flags,
    dim_score_df,
    weight_df,
    weighted_df
  )
}


# GENERACIÓN DE VARIABLES PARA LAS TRES FASES

# Ejecutar add_phase_features() una vez por fase.
# Cada iteración utiliza las especificaciones de phase_specs.
# Los resultados se unen horizontalmente, manteniendo una fila
# por cada participante de la base de entrada.

phase_features_df <- pmap(
  phase_specs,
  function(
    phase_prefix,
    phase_label,
    source_column
  ) {
    add_phase_features(
      df,
      phase_prefix,
      phase_label,
      source_column
    )
  }
) %>%
  bind_cols()

# Comprobar que la tabla de nuevas variables tiene el mismo
# número de filas que la base original.

if (nrow(phase_features_df) != nrow(df)) {
  stop(
    "phase_features_df no tiene el mismo número de filas que el input."
  )
}

# Añadir todas las variables de fase a la base completa de calidad.
# No se eliminan filas ni se filtra por usable_for_clustering.

df_enriched <- bind_cols(
  df,
  phase_features_df
)

# Identificar las columnas de calidad y disponibilidad de cada fase.

phase_quality_cols <- paste0(
  phase_specs$phase_prefix,
  "_quality"
)

phase_has_response_cols <- paste0(
  phase_specs$phase_prefix,
  "_has_response"
)


# RESUMEN GLOBAL DE LAS TRES FASES POR PARTICIPANTE

df_enriched <- df_enriched %>%
  mutate(
    # Número de fases con al menos una dimensión reconocida.
    
    n_phase_dimension_blocks_with_response = rowSums(
      across(
        all_of(phase_has_response_cols),
        ~ .x == TRUE
      ),
      na.rm = TRUE
    ),
    
    # Número de fases cuya calidad es completa.
    
    n_phase_dimension_blocks_complete = rowSums(
      across(
        all_of(phase_quality_cols),
        ~ .x == "complete"
      ),
      na.rm = TRUE
    ),
    
    # Número de fases completas o parcialmente utilizables.
    
    n_phase_dimension_blocks_usable = rowSums(
      across(
        all_of(phase_quality_cols),
        ~ .x %in% c(
          "complete",
          "usable_partial"
        )
      ),
      na.rm = TRUE
    ),
    
    # Clasificación conjunta según la disponibilidad y calidad
    # de los tres bloques de fase.
    
    quality_phase_dimension_block = case_when(
      n_phase_dimension_blocks_with_response == 0 ~
        "no_phase_dimension_response",
      
      n_phase_dimension_blocks_complete ==
        n_phase_dimension_blocks_with_response ~
        "complete",
      
      n_phase_dimension_blocks_usable ==
        n_phase_dimension_blocks_with_response ~
        "usable_partial",
      
      n_phase_dimension_blocks_usable > 0 ~
        "partially_usable",
      
      TRUE ~ "not_usable"
    )
  )


# COMPROBACIONES DE TRAZABILIDAD

# Verificar que la nueva base conserva exactamente el número
# de filas de la base de entrada.

if (nrow(df_enriched) != nrow(df)) {
  stop(
    "El número de filas ha cambiado. Input = ",
    nrow(df),
    "; output = ",
    nrow(df_enriched)
  )
}

# Comprobar que no se han generado identificadores duplicados.

if (anyDuplicated(df_enriched$integrated_row_id)) {
  stop(
    "Se han creado integrated_row_id duplicados."
  )
}

# Verificar que los identificadores mantienen exactamente
# los mismos valores y el mismo orden.

if (
  !identical(
    as.character(df$integrated_row_id),
    as.character(df_enriched$integrated_row_id)
  )
) {
  stop(
    "El orden o los valores de integrated_row_id han cambiado."
  )
}

# Recuperar los identificadores y las variables de trazabilidad
# para compararlos con los guardados al principio del script.

output_traceability <- df_enriched %>%
  select(all_of(required_traceability_cols))

if (
  !identical(
    input_traceability,
    output_traceability
  )
) {
  stop(
    "La trazabilidad de las filas ha cambiado."
  )
}


# DIAGNÓSTICOS DE INTEGRIDAD DEL DATASET

# Registrar el número de filas, identificadores únicos,
# fuentes de datos y submuestras antes y después del enriquecimiento.

diagnostics_dataset_integrity <- tibble(
  metric = c(
    "n_input_rows",
    "n_output_rows",
    "n_input_unique_ids",
    "n_output_unique_ids",
    "n_sources",
    "n_subsamples"
  ),
  
  value = c(
    nrow(df),
    nrow(df_enriched),
    n_distinct(df$integrated_row_id),
    n_distinct(df_enriched$integrated_row_id),
    n_distinct(df_enriched$dataset_source),
    n_distinct(df_enriched$subsample)
  )
)


# RECUENTO DE FILAS POR SUBMUESTRA

diagnostics_subsample_counts <- df_enriched %>%
  count(
    comparison_region,
    subsample,
    dataset_source,
    name = "n_rows"
  ) %>%
  arrange(
    comparison_region,
    subsample
  )


# COMPARACIÓN DE FILAS POR SUBMUESTRA ENTRE ENTRADA Y SALIDA

# Comparar el número de filas de cada submuestra.
# El resultado debe indicar TRUE en same_n para todas ellas.

diagnostics_subsample_integrity <- full_join(
  input_traceability %>%
    count(
      comparison_region,
      subsample,
      name = "n_input"
    ),
  
  output_traceability %>%
    count(
      comparison_region,
      subsample,
      name = "n_output"
    ),
  
  by = c(
    "comparison_region",
    "subsample"
  )
) %>%
  mutate(
    n_input = replace_na(n_input, 0L),
    n_output = replace_na(n_output, 0L),
    same_n = n_input == n_output
  ) %>%
  arrange(
    comparison_region,
    subsample
  )

if (
  any(!diagnostics_subsample_integrity$same_n)
) {
  stop(
    "El número de filas por submuestra ha cambiado."
  )
}


# DIAGNÓSTICO DE LAS COLUMNAS ORIGINALES DE FASE

# Registrar para cada fase su columna original, el número de respuestas
# informadas y su proporción sobre la base completa.

diagnostics_phase_columns <- phase_specs %>%
  mutate(
    n_valid_raw = map_int(
      source_column,
      ~ sum(
        !is.na(clean_text_quality(df[[.x]])),
        na.rm = TRUE
      )
    ),
    
    prop_valid_raw =
      n_valid_raw / nrow(df)
  )


# FUNCIÓN DE COBERTURA DE RESPUESTAS ORIGINALES POR FASE

# Calcula la disponibilidad de cada columna original de fase
# dentro de los grupos indicados.
#
# data: tabla con las respuestas originales.
# grouping_vars: nombres de las columnas de agrupación.
#
# Devuelve: tibble con el número de filas, respuestas informadas,
# proporción de respuestas informadas y especificación de la fase.

phase_coverage <- function(
    data,
    grouping_vars
) {
  map_dfr(
    seq_len(nrow(phase_specs)),
    function(i) {
      current <- phase_specs[i, ]
      
      data %>%
        mutate(
          .has_raw_response = !is.na(
            clean_text_quality(
              .data[[current$source_column]]
            )
          )
        ) %>%
        group_by(
          across(all_of(grouping_vars))
        ) %>%
        summarise(
          n_rows = n(),
          
          n_valid_raw = sum(
            .has_raw_response,
            na.rm = TRUE
          ),
          
          prop_valid_raw =
            n_valid_raw / n_rows,
          
          .groups = "drop"
        ) %>%
        mutate(
          phase_prefix = current$phase_prefix,
          phase_label = current$phase_label,
          source_column = current$source_column,
          .before = 1
        )
    }
  )
}


# COBERTURA DE LAS FASES POR SUBMUESTRA

diagnostics_phase_columns_by_subsample <- phase_coverage(
  df,
  c(
    "comparison_region",
    "subsample",
    "dataset_source"
  )
)


# COBERTURA DE LAS FASES POR FUENTE Y ENCUESTA ORIGINAL

diagnostics_phase_columns_by_source <- phase_coverage(
  df,
  c(
    "dataset_source",
    "source_survey"
  )
)


# PREPARACIÓN DEL RESUMEN POOLED Y POR SUBMUESTRA

# La función add_analysis_samples() está definida en 00_common.R.
# Se conserva comentada la construcción anterior.
#
# df_phase_analysis <- bind_rows(
#   df_enriched %>%
#     mutate(
#       analysis_sample = "POOLED_ALL",
#       analysis_region = "ALL"
#     ),
#
#   df_enriched %>%
#     mutate(
#       analysis_sample = subsample,
#       analysis_region = comparison_region
#     )
# )

# Crear POOLED_ALL y las cuatro submuestras para los diagnósticos.
# Esta operación no modifica df_enriched ni el CSV principal.

df_phase_analysis <- add_analysis_samples(df_enriched)

# RESPUESTAS DE FASE POR MUESTRA

# Contar participantes sin respuestas de fase reconocidas
# y con una, dos o tres fases reconocidas.

diagnostics_phase_response_by_sample <- df_phase_analysis %>%
  group_by(
    analysis_sample,
    analysis_region
  ) %>%
  summarise(
    n_rows = n(),
    
    n_with_any_phase_response = sum(
      n_phase_dimension_blocks_with_response > 0,
      na.rm = TRUE
    ),
    
    prop_with_any_phase_response =
      n_with_any_phase_response / n_rows,
    
    n_with_1_phase_response = sum(
      n_phase_dimension_blocks_with_response == 1,
      na.rm = TRUE
    ),
    
    n_with_2_phase_responses = sum(
      n_phase_dimension_blocks_with_response == 2,
      na.rm = TRUE
    ),
    
    n_with_3_phase_responses = sum(
      n_phase_dimension_blocks_with_response == 3,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# FRECUENCIA DE SELECCIÓN DE LAS NUEVE DIMENSIONES

# Para cada combinación de fase y dimensión, contar cuántos
# participantes la seleccionaron.
#
# prop_selected_total utiliza como denominador todas las filas
# de df_enriched, no únicamente las personas que respondieron esa fase.

diagnostics_phase_dimension_frequencies <- expand_grid(
  phase_prefix = phase_specs$phase_prefix,
  dimension = dimension_levels
) %>%
  mutate(
    n_selected = map2_int(
      phase_prefix,
      dimension,
      ~ {
        selected_col <- paste0(
          .x,
          "_selected_dim_",
          str_to_lower(.y)
        )
        
        sum(
          df_enriched[[selected_col]] == TRUE,
          na.rm = TRUE
        )
      }
    ),
    
    prop_selected_total =
      n_selected / nrow(df_enriched)
  ) %>%
  arrange(
    phase_prefix,
    desc(n_selected)
  )


# FRECUENCIA DE DIMENSIONES POR SUBMUESTRA

# Repetir los recuentos de selección para cada submuestra.
# La proporción se calcula sobre todas las filas de esa submuestra.

diagnostics_phase_dimension_frequencies_by_subsample <- map_dfr(
  phase_specs$phase_prefix,
  function(phase_prefix_current) {
    map_dfr(
      dimension_levels,
      function(dim_current) {
        selected_col <- paste0(
          phase_prefix_current,
          "_selected_dim_",
          str_to_lower(dim_current)
        )
        
        df_enriched %>%
          group_by(
            comparison_region,
            subsample
          ) %>%
          summarise(
            n_rows = n(),
            
            n_selected = sum(
              .data[[selected_col]] == TRUE,
              na.rm = TRUE
            ),
            
            prop_selected_total =
              n_selected / n_rows,
            
            .groups = "drop"
          ) %>%
          mutate(
            phase_prefix = phase_prefix_current,
            dimension = dim_current,
            .before = 1
          )
      }
    )
  }
) %>%
  arrange(
    subsample,
    phase_prefix,
    desc(n_selected)
  )


# FUNCIÓN DE RECUENTO DE CATEGORÍAS DE CALIDAD

# Calcula frecuencias y proporciones de las categorías de calidad
# de cada fase y del resumen global de los bloques.
#
# data: tabla enriquecida con las variables de calidad.
# select_vars: columnas que deben conservarse antes de pivot_longer().
# count_vars: columnas utilizadas para contar cada categoría.
# prop_group_vars: columnas que definen el denominador de la proporción.
# arrange_vars: columnas utilizadas para ordenar el resultado.
#
# Devuelve: tibble con categoría de calidad, frecuencia y proporción.

quality_count_table <- function(
    data,
    select_vars,
    count_vars,
    prop_group_vars,
    arrange_vars
) {
  data %>%
    select(
      all_of(select_vars),
      all_of(phase_quality_cols),
      quality_phase_dimension_block
    ) %>%
    pivot_longer(
      cols = c(
        all_of(phase_quality_cols),
        quality_phase_dimension_block
      ),
      names_to = "quality_variable",
      values_to = "quality_label"
    ) %>%
    count(
      across(all_of(count_vars)),
      quality_variable,
      quality_label,
      name = "n"
    ) %>%
    group_by(
      across(all_of(prop_group_vars)),
      quality_variable
    ) %>%
    mutate(
      prop = n / sum(n)
    ) %>%
    ungroup() %>%
    arrange(
      across(all_of(arrange_vars)),
      quality_variable,
      desc(n)
    )
}


# CALIDAD DE LAS FASES POR SUBMUESTRA Y ENCUESTA ORIGINAL

diagnostics_phase_quality_counts_by_subsample <- quality_count_table(
  df_enriched,
  
  select_vars = c(
    "comparison_region",
    "subsample",
    "dataset_source",
    "source_survey"
  ),
  
  count_vars = c(
    "comparison_region",
    "subsample",
    "dataset_source",
    "source_survey"
  ),
  
  prop_group_vars = c(
    "subsample",
    "source_survey"
  ),
  
  arrange_vars = c(
    "subsample",
    "source_survey"
  )
)


# CALIDAD DE LAS FASES POR FUENTE Y ENCUESTA ORIGINAL

diagnostics_phase_quality_counts_by_source <- quality_count_table(
  df_enriched,
  
  select_vars = c(
    "dataset_source",
    "source_survey"
  ),
  
  count_vars = c(
    "dataset_source",
    "source_survey"
  ),
  
  prop_group_vars = c(
    "dataset_source",
    "source_survey"
  ),
  
  arrange_vars = c(
    "dataset_source",
    "source_survey"
  )
)


# FUNCIÓN DE RESUMEN DE SCORES POR GRUPO

# Calcula los estadísticos descriptivos de los scores agregados
# de las tres fases.
#
# data: tabla enriquecida.
# grouping_vars: columnas utilizadas para agrupar los participantes.
#
# Devuelve: tibble con cobertura, media, desviación típica, mínimo
# y máximo de cada score de fase dentro de cada grupo.
#
# Los participantes sin score válido no contribuyen a la media,
# desviación típica, mínimo ni máximo de ese score.

summarise_score_table <- function(
    data,
    grouping_vars
) {
  score_cols <- names(data)[
    str_detect(
      names(data),
      "_selected_dimension_score_mean$"
    )
  ]
  
  data %>%
    select(
      all_of(grouping_vars),
      all_of(score_cols)
    ) %>%
    pivot_longer(
      cols = all_of(score_cols),
      names_to = "score_variable",
      values_to = "score"
    ) %>%
    mutate(
      score = parse_num(score)
    ) %>%
    group_by(
      across(all_of(grouping_vars)),
      score_variable
    ) %>%
    summarise(
      n_rows = n(),
      
      n_valid_score = sum(
        !is.na(score)
      ),
      
      prop_valid_score =
        n_valid_score / n_rows,
      
      mean_score = mean(
        score,
        na.rm = TRUE
      ),
      
      sd_score = sd(
        score,
        na.rm = TRUE
      ),
      
      min_score = suppressWarnings(
        min(
          score,
          na.rm = TRUE
        )
      ),
      
      max_score = suppressWarnings(
        max(
          score,
          na.rm = TRUE
        )
      ),
      
      .groups = "drop"
    ) %>%
    mutate(
      mean_score = if_else(
        is.nan(mean_score),
        NA_real_,
        mean_score
      ),
      
      min_score = if_else(
        is.infinite(min_score),
        NA_real_,
        min_score
      ),
      
      max_score = if_else(
        is.infinite(max_score),
        NA_real_,
        max_score
      )
    )
}


# RESUMEN DE SCORES POR SUBMUESTRA Y ENCUESTA ORIGINAL

diagnostics_phase_scores_summary_by_subsample <- summarise_score_table(
  df_enriched,
  c(
    "comparison_region",
    "subsample",
    "dataset_source",
    "source_survey"
  )
) %>%
  arrange(
    subsample,
    source_survey,
    score_variable
  )


# RESUMEN DE SCORES POR FUENTE Y ENCUESTA ORIGINAL

diagnostics_phase_scores_summary_by_source <- summarise_score_table(
  df_enriched,
  c(
    "dataset_source",
    "source_survey"
  )
) %>%
  arrange(
    dataset_source,
    source_survey,
    score_variable
  )


# GUARDADO DE ARCHIVOS

# Asociar cada tabla generada con el nombre de su correspondiente CSV.

outputs <- list(
  "all_sources_integrated_component_quality_phase_scores.csv" =
    df_enriched,
  
  "dimension_determinant_mapping.csv" =
    dimension_determinant_map,
  
  "phase_dimension_source_columns.csv" =
    phase_specs,
  
  "diagnostics_dataset_integrity.csv" =
    diagnostics_dataset_integrity,
  
  "diagnostics_subsample_counts.csv" =
    diagnostics_subsample_counts,
  
  "diagnostics_subsample_integrity.csv" =
    diagnostics_subsample_integrity,
  
  "diagnostics_phase_columns.csv" =
    diagnostics_phase_columns,
  
  "diagnostics_phase_columns_by_subsample.csv" =
    diagnostics_phase_columns_by_subsample,
  
  "diagnostics_phase_columns_by_source.csv" =
    diagnostics_phase_columns_by_source,
  
  "diagnostics_phase_response_by_sample.csv" =
    diagnostics_phase_response_by_sample,
  
  "diagnostics_phase_dimension_frequencies.csv" =
    diagnostics_phase_dimension_frequencies,
  
  "diagnostics_phase_dimension_frequencies_by_subsample.csv" =
    diagnostics_phase_dimension_frequencies_by_subsample,
  
  "diagnostics_phase_quality_counts_by_subsample.csv" =
    diagnostics_phase_quality_counts_by_subsample,
  
  "diagnostics_phase_quality_counts_by_source.csv" =
    diagnostics_phase_quality_counts_by_source,
  
  "diagnostics_phase_scores_summary_by_subsample.csv" =
    diagnostics_phase_scores_summary_by_subsample,
  
  "diagnostics_phase_scores_summary_by_source.csv" =
    diagnostics_phase_scores_summary_by_source
)

# Guardar los CSV en el directorio de resultados de esta etapa.

iwalk(
  outputs,
  ~ write_csv(
    .x,
    file.path(out_dir, .y)
  )
)


# RESUMEN EN CONSOLA

cat("\nINTEGRIDAD DEL DATASET\n")

print(
  diagnostics_dataset_integrity,
  n = Inf,
  width = Inf
)

cat("\nSUBMUESTRAS INPUT / OUTPUT\n")

print(
  diagnostics_subsample_integrity,
  n = Inf,
  width = Inf
)

cat("\nCOLUMNAS DE FASE USADAS\n")

print(
  diagnostics_phase_columns,
  n = Inf,
  width = Inf
)

cat("\nCOBERTURA DE FASE POR SUBMUESTRA\n")

print(
  diagnostics_phase_columns_by_subsample,
  n = Inf,
  width = Inf
)

cat("\nRESPUESTAS DE FASE POR MUESTRA\n")

print(
  diagnostics_phase_response_by_sample,
  n = Inf,
  width = Inf
)

cat("\nFRECUENCIA DE DIMENSIONES SELECCIONADAS\n")

print(
  diagnostics_phase_dimension_frequencies,
  n = Inf,
  width = Inf
)

cat("\nCALIDAD DE FASE POR SUBMUESTRA\n")

print(
  diagnostics_phase_quality_counts_by_subsample,
  n = Inf,
  width = Inf
)

cat("\nSCORES DE FASE POR SUBMUESTRA\n")

print(
  diagnostics_phase_scores_summary_by_subsample,
  n = Inf,
  width = Inf
)

cat(
  "\nFilas input: ",
  nrow(df),
  "\nFilas output: ",
  nrow(df_enriched),
  "\nIDs únicos input: ",
  n_distinct(df$integrated_row_id),
  "\nIDs únicos output: ",
  n_distinct(df_enriched$integrated_row_id),
  "\n",
  sep = ""
)

message(
  "\nListo. Dataset enriquecido guardado en: ",
  out_file
)