# 03_4_figures_current_dataset_status.R
#
# OBJETIVO
# Generar las figuras descriptivas del estado de la base integrada
# después de la armonización, el control de calidad y el cálculo
# de los scores de fase, y antes de los bootstraps finales.
#
# Las figuras permiten visualizar el tamaño y la composición de las
# submuestras, la disponibilidad de los 32 determinantes, la calidad
# de los datos, la cobertura sociodemográfica, la disponibilidad de
# variables para propensity y los resultados de las fases de decisión.
# También muestran el número de participantes que cumplen los
# criterios de calidad para clustering antes del remuestreo.
#
# Este script únicamente representa los resultados calculados en 03_3.
#
# MUESTRAS DESCRIPTIVAS
# - POOLED_ALL: unión sin ponderación de las cuatro submuestras.
# - DIEGO
# - RENOVISOR
# - WHY_EUROPE
# - WHY_LATAM
#
# POOLED_ALL se utiliza como agregado descriptivo anterior al bootstrap.
# No equivale a la muestra COMPLETE construida en etapas posteriores.
#
# ARQUITECTURA POSTERIOR
# - EUROPE: bootstrap político/electoral en 04_2b.
# - LATAM: bootstrap económico por ingresos en 04_2e.
# - COMPLETE: combinación de EUROPE y LATAM en 04_2f.
#
# ENTRADA
# Directorio:
# paper1_cluster/data/processed/03_3_summary_dataset_status/
#
# Tablas utilizadas:
# - summary_00_files_dimensions.csv
# - summary_01_subsamples.csv
# - summary_02_quality_by_sample.csv
# - summary_03_quality_counts_by_sample.csv
# - summary_04_metadata_quality_by_sample.csv
# - summary_05_determinants_by_sample.csv
# - summary_06_missing_by_determinant_by_sample.csv
# - summary_07_sociodemographic_coverage_by_sample.csv
# - summary_09_propensity_variable_coverage_by_sample.csv
# - summary_10_propensity_readiness_by_sample.csv
# - summary_11_phase_block_by_sample.csv
# - summary_13_phase_dimension_frequencies_by_sample.csv
# - summary_14_phase_scores_by_sample.csv
# - summary_15_dimension_mapping_counts.csv
# - summary_18_clustering_sample_sizes.csv
#
# PROCESAMIENTO
# 1. Comprobar que existen las tablas de entrada generadas en 03_3.
#
# 2. Leer los resúmenes y verificar que la tabla de tamaños para
#    clustering contiene POOLED_ALL y las cuatro submuestras.
#
# 3. Recuperar el número total de participantes de la base integrada
#    y el número total disponible para clustering.
#
# 4. Representar las dimensiones de los archivos y los tamaños
#    originales de las submuestras.
#
# 5. Comparar el número de participantes originales y utilizables
#    para clustering, así como su proporción por muestra.
#
# 6. Representar las categorías de calidad global, la calidad
#    de los metadatos y la cobertura de los 32 determinantes.
#
# 7. Visualizar las ausencias de los determinantes, la disponibilidad
#    sociodemográfica y la cobertura de las variables de propensity.
#
# 8. Representar la aplicabilidad del propensity europeo y el
#    número de participantes con variables disponibles para sus
#    diferentes especificaciones.
#
# 9. Visualizar la cobertura de las preguntas de fase, la frecuencia
#    de las dimensiones seleccionadas y los scores agregados de fase.
#
# 10. Representar el número de determinantes de cada dimensión.
#
# 11. Mostrar los tamaños y la composición de las muestras disponibles
#     para clustering antes del bootstrap.
#
# 12. Guardar las figuras en PNG y generar figure_index.csv con
#     los nombres y las descripciones de las figuras.
#
# CRITERIOS DE INTERPRETACIÓN
# - Los tamaños originales se obtienen de la base completa resumida
#   en 03_3.
#
# - Los tamaños utilizables para clustering se obtienen de la matriz
#   filtrada en 03_1, según los criterios de calidad de determinantes.
#
# - Los perfiles con 32 determinantes completos pueden incluir valores
#   imputados con 50 durante 03_1. No equivalen necesariamente a
#   participantes que respondieron originalmente los 32 determinantes.
#
# - Las ausencias representadas corresponden a los determinantes
#   posteriores a la imputación.
#
# - La disponibilidad de variables para propensity no equivale
#   a la elegibilidad para clustering.
#
# - Las frecuencias de dimensiones utilizan los recuentos generados
#   en 03_3, cuyo denominador incluye todas las filas de cada muestra.
#
# - Los scores de fase representados son medias descriptivas de los
#   scores agregados calculados en 03_2. Se calculan únicamente entre
#   participantes con un score válido para la fase correspondiente.
#
# - Las figuras de POOLED_ALL son descriptivas: no aplican pesos
#   de representatividad ni los índices bootstrap posteriores.
#
# SALIDAS
# Directorio:
# paper1_cluster/data/processed/03_4_figures_current_dataset_status/
#
# FIGURAS
# 01_columns_by_processing_step.png
# 02_original_rows_by_subsample.png
# 03_original_sample_composition.png
# 04_total_vs_clustering_usable_by_subsample.png
# 05_prop_usable_for_clustering.png
# 06_quality_categories_by_subsample.png
# 07_metadata_quality_by_subsample.png
# 08_determinant_coverage_by_subsample.png
# 09_complete_32det_by_sample.png
# 10_missing_determinants_by_subsample.png
# 11_sociodemographic_coverage_by_subsample.png
# 12_propensity_variable_coverage_by_subsample.png
# 13_european_propensity_applicability.png
# 14_european_propensity_readiness.png
# 15_phase_response_coverage.png
# 16_selected_dimensions_by_phase_renovisor.png
# 17_phase_dimension_scores_renovisor.png
# 18_dimension_determinant_mapping.png
# 19_final_clustering_sample_sizes.png
# 20_pooled_clustering_composition.png
# 21_original_vs_final_analysis_sizes.png
#
# ÍNDICE
# - figure_index.csv:
#   Nombre, descripción y disponibilidad de cada figura.
#
# DEPENDENCIAS
# - 00_common.R: paquetes, rutas, constantes, mapeo de dimensiones
#   y funciones comunes de lectura y guardado.
# - 03_3_summary_dataset_status.R: genera las tablas de entrada.
#
# Las figuras de este script describen el dataset pre-bootstrap.

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

common_path <- common_candidates[file.exists(common_candidates)][1]

if (is.na(common_path)) {
  stop("No se encuentra 00_common.R.")
}

source(common_path)

# Definiciones anteriores, ahora centralizadas en 00_common.R:
#
# suppressPackageStartupMessages({
#   library(tidyverse)
# })
#
# processed_root <- "paper1_cluster/data/processed"


# CONFIGURACIÓN

summary_dir <- file.path(
  processed_root,
  "03_3_summary_dataset_status"
)

out_dir <- file.path(
  processed_root,
  "03_4_figures_current_dataset_status"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Los nombres de las submuestras proceden de expected_subsamples,
# definido en 00_common.R.
#
# ANALYSIS_SAMPLE_LEVELS <- c(
#   "POOLED_ALL",
#   "DIEGO",
#   "RENOVISOR",
#   "WHY_EUROPE",
#   "WHY_LATAM"
# )
#
# SUBSAMPLE_LEVELS <- c(
#   "DIEGO",
#   "RENOVISOR",
#   "WHY_EUROPE",
#   "WHY_LATAM"
# )

analysis_sample_levels <- c(
  "POOLED_ALL",
  expected_subsamples
)


# FUNCIONES AUXILIARES

# Guarda una figura en el directorio de esta etapa.
#
# p: objeto ggplot.
# filename: nombre del PNG.
# width, height: dimensiones de la imagen en pulgadas.
#
# Utiliza save_plot() de 00_common.R, que establece la resolución
# y las opciones comunes de guardado.
#
# No sustituye ni redefine la función save_plot() del proyecto.

save_figure <- function(
    p,
    filename,
    width = 10,
    height = 6
) {
  save_plot(
    p,
    file.path(out_dir, filename),
    width = width,
    height = height
  )
}


# Lee una tabla de 03_3 y detiene la ejecución si no existe.
#
# filename: nombre del CSV de resumen.
# Devuelve: tabla leída mediante read_csv_safe() de 00_common.R.

read_required <- function(filename) {
  path <- file.path(summary_dir, filename)
  
  if (!file.exists(path)) {
    stop(
      "No existe el archivo requerido:\n",
      path,
      "\n\nEjecuta primero 03_3_summary_dataset_status.R"
    )
  }
  
  read_csv_safe(path)
}


# Sustituye los guiones bajos por espacios y limpia espacios adicionales.
#
# x: vector de etiquetas.
# Devuelve: etiquetas legibles para los gráficos.

clean_label <- function(x) {
  x %>%
    str_replace_all("_", " ") %>%
    str_squish()
}


# Convierte los nombres de las muestras descriptivas en un factor
# con POOLED_ALL en primer lugar y las cuatro submuestras después.
#
# x: vector de nombres de muestras.

sample_factor <- function(x) {
  factor(
    x,
    levels = analysis_sample_levels
  )
}


# Ordena las cuatro submuestras según expected_subsamples.
#
# x: vector de nombres de submuestras.

subsample_factor <- function(x) {
  factor(
    x,
    levels = expected_subsamples
  )
}


# LECTURA DE LOS RESÚMENES

summary_00 <- read_required(
  "summary_00_files_dimensions.csv"
)

summary_01_subsamples <- read_required(
  "summary_01_subsamples.csv"
)

summary_02 <- read_required(
  "summary_02_quality_by_sample.csv"
)

summary_03 <- read_required(
  "summary_03_quality_counts_by_sample.csv"
)

summary_04 <- read_required(
  "summary_04_metadata_quality_by_sample.csv"
)

summary_05 <- read_required(
  "summary_05_determinants_by_sample.csv"
)

summary_06 <- read_required(
  "summary_06_missing_by_determinant_by_sample.csv"
)

summary_07 <- read_required(
  "summary_07_sociodemographic_coverage_by_sample.csv"
)

summary_09 <- read_required(
  "summary_09_propensity_variable_coverage_by_sample.csv"
)

summary_10 <- read_required(
  "summary_10_propensity_readiness_by_sample.csv"
)

summary_11_phase <- read_required(
  "summary_11_phase_block_by_sample.csv"
)

summary_13 <- read_required(
  "summary_13_phase_dimension_frequencies_by_sample.csv"
)

summary_14 <- read_required(
  "summary_14_phase_scores_by_sample.csv"
)

summary_15 <- read_required(
  "summary_15_dimension_mapping_counts.csv"
)

summary_18 <- read_required(
  "summary_18_clustering_sample_sizes.csv"
)


# COMPROBACIONES INICIALES

# Comprobar que la tabla de tamaños para clustering contiene
# POOLED_ALL y las cuatro submuestras esperadas.

missing_analysis_samples <- setdiff(
  analysis_sample_levels,
  unique(summary_18$analysis_sample)
)

if (length(missing_analysis_samples)) {
  stop(
    "Faltan muestras de análisis en summary_18: ",
    paste(missing_analysis_samples, collapse = ", ")
  )
}


# Número original de participantes en la base integrada.

original_total_n <- sum(
  summary_01_subsamples$n_rows,
  na.rm = TRUE
)


# Número total de participantes disponibles para clustering
# antes del bootstrap.

pooled_usable_n <- summary_18 %>%
  filter(analysis_sample == "POOLED_ALL") %>%
  pull(n_usable_for_clustering)

if (length(pooled_usable_n) != 1L) {
  stop(
    "summary_18 debe contener exactamente una fila para POOLED_ALL."
  )
}


# 01. NÚMERO DE COLUMNAS POR ETAPA DEL PIPELINE

p01 <- summary_00 %>%
  mutate(
    step = factor(step, levels = step)
  ) %>%
  ggplot(aes(x = step, y = n_cols)) +
  geom_col() +
  geom_text(
    aes(label = n_cols),
    vjust = -0.3,
    size = 3.5
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    )
  ) +
  labs(
    title = "Dataset construction: number of columns by processing step",
    x = "Processing step",
    y = "Number of columns"
  )

save_figure(
  p01,
  "01_columns_by_processing_step.png",
  width = 11
)


# 02. NÚMERO ORIGINAL DE PARTICIPANTES POR SUBMUESTRA

p02 <- summary_01_subsamples %>%
  mutate(
    subsample = subsample_factor(subsample)
  ) %>%
  ggplot(aes(x = subsample, y = n_rows)) +
  geom_col() +
  geom_text(
    aes(label = n_rows),
    vjust = -0.3,
    size = 4
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Original sample size by subsample",
    subtitle = paste0(
      "Total integrated dataset: ",
      format(original_total_n, big.mark = ","),
      " respondents"
    ),
    x = "Subsample",
    y = "Number of respondents"
  )

save_figure(
  p02,
  "02_original_rows_by_subsample.png"
)


# 03. COMPOSICIÓN ORIGINAL DE LA BASE INTEGRADA

p03 <- summary_01_subsamples %>%
  mutate(
    subsample = subsample_factor(subsample),
    prop_pct = prop_total * 100
  ) %>%
  ggplot(aes(x = subsample, y = prop_pct)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(prop_pct, 1), "%")),
    vjust = -0.3,
    size = 4
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Composition of the integrated dataset",
    x = "Subsample",
    y = "Share of total sample (%)"
  )

save_figure(
  p03,
  "03_original_sample_composition.png"
)


# 04. PARTICIPANTES ORIGINALES Y UTILIZABLES PARA CLUSTERING

p04_data <- summary_02 %>%
  filter(analysis_sample != "POOLED_ALL") %>%
  select(
    analysis_sample,
    n_rows,
    n_usable_for_clustering
  ) %>%
  pivot_longer(
    c(n_rows, n_usable_for_clustering),
    names_to = "metric",
    values_to = "n"
  ) %>%
  mutate(
    analysis_sample = subsample_factor(analysis_sample),
    
    metric = recode(
      metric,
      n_rows = "Total rows",
      n_usable_for_clustering = "Usable for clustering"
    )
  )

p04 <- ggplot(
  p04_data,
  aes(
    x = analysis_sample,
    y = n,
    fill = metric
  )
) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3.5
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Total and clustering-usable respondents by subsample",
    x = "Subsample",
    y = "Number of respondents",
    fill = NULL
  )

save_figure(
  p04,
  "04_total_vs_clustering_usable_by_subsample.png",
  width = 11
)


# 05. PORCENTAJE DE PARTICIPANTES UTILIZABLES PARA CLUSTERING

p05 <- summary_02 %>%
  mutate(
    analysis_sample = sample_factor(analysis_sample),
    prop_pct = prop_usable_for_clustering * 100
  ) %>%
  ggplot(aes(x = analysis_sample, y = prop_pct)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(prop_pct, 1), "%")),
    vjust = -0.3,
    size = 4
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Share of respondents usable for clustering",
    x = "Analysis sample",
    y = "Usable respondents (%)"
  )

save_figure(
  p05,
  "05_prop_usable_for_clustering.png",
  width = 11
)


# 06. CATEGORÍAS DE CALIDAD GLOBAL POR SUBMUESTRA

p06 <- summary_03 %>%
  filter(analysis_sample != "POOLED_ALL") %>%
  mutate(
    analysis_sample = subsample_factor(analysis_sample),
    row_quality_final = clean_label(row_quality_final)
  ) %>%
  ggplot(
    aes(
      x = analysis_sample,
      y = n,
      fill = row_quality_final
    )
  ) +
  geom_col() +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom") +
  labs(
    title = "Final row-quality categories by subsample",
    x = "Subsample",
    y = "Number of respondents",
    fill = "Row quality"
  )

save_figure(
  p06,
  "06_quality_categories_by_subsample.png",
  width = 12,
  height = 7
)


# 07. CALIDAD DE METADATOS POR SUBMUESTRA

p07 <- summary_04 %>%
  filter(analysis_sample != "POOLED_ALL") %>%
  mutate(
    analysis_sample = subsample_factor(analysis_sample),
    metadata_quality = clean_label(metadata_quality)
  ) %>%
  ggplot(
    aes(
      x = analysis_sample,
      y = n,
      fill = metadata_quality
    )
  ) +
  geom_col() +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom") +
  labs(
    title = "Metadata quality by subsample",
    x = "Subsample",
    y = "Number of respondents",
    fill = "Metadata quality"
  )

save_figure(
  p07,
  "07_metadata_quality_by_subsample.png",
  width = 12,
  height = 7
)


# 08. COBERTURA DE LOS 32 DETERMINANTES POR SUBMUESTRA

# Los perfiles completos se definen según la disponibilidad
# posterior a la imputación de 03_1.

p08_data <- summary_05 %>%
  filter(analysis_sample != "POOLED_ALL") %>%
  select(
    analysis_sample,
    n_rows,
    n_with_any_det,
    n_complete_32det,
    n_usable_for_clustering
  ) %>%
  pivot_longer(
    -analysis_sample,
    names_to = "metric",
    values_to = "n"
  ) %>%
  mutate(
    analysis_sample = subsample_factor(analysis_sample),
    
    metric = recode(
      metric,
      n_rows = "Total rows",
      n_with_any_det = "At least one determinant",
      n_complete_32det = "Complete 32 determinants",
      n_usable_for_clustering = "Usable for clustering"
    )
  )

p08 <- ggplot(
  p08_data,
  aes(
    x = analysis_sample,
    y = n,
    fill = metric
  )
) +
  geom_col(position = "dodge") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Coverage of the 32 determinants by subsample",
    x = "Subsample",
    y = "Number of respondents",
    fill = NULL
  )

save_figure(
  p08,
  "08_determinant_coverage_by_subsample.png",
  width = 13,
  height = 7
)


# 09. PORCENTAJE DE PERFILES CON 32 DETERMINANTES DISPONIBLES

p09 <- summary_05 %>%
  mutate(
    analysis_sample = sample_factor(analysis_sample),
    prop_complete_pct = prop_complete_32det * 100
  ) %>%
  ggplot(
    aes(
      x = analysis_sample,
      y = prop_complete_pct
    )
  ) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(prop_complete_pct, 1), "%")),
    vjust = -0.3,
    size = 3.7
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Respondents with all 32 determinants available",
    x = "Analysis sample",
    y = "Complete 32-determinant profiles (%)"
  )

save_figure(
  p09,
  "09_complete_32det_by_sample.png",
  width = 11
)


# 10. AUSENCIAS POR DETERMINANTE Y SUBMUESTRA

# Las ausencias corresponden a los valores posteriores
# a la imputación realizada en 03_1.

p10_data <- summary_06 %>%
  filter(analysis_sample != "POOLED_ALL") %>%
  mutate(
    analysis_sample = subsample_factor(analysis_sample),
    
    determinant = determinant %>%
      str_remove("^det_\\d{2}_") %>%
      clean_label(),
    
    prop_missing_pct = prop_missing * 100
  )

p10 <- ggplot(
  p10_data,
  aes(
    x = determinant,
    y = prop_missing_pct
  )
) +
  geom_col() +
  facet_wrap(
    ~ analysis_sample,
    ncol = 2
  ) +
  coord_flip() +
  theme_minimal(base_size = 10) +
  labs(
    title = "Missingness of the 32 determinants by subsample",
    x = "Determinant",
    y = "Missing values (%)"
  )

save_figure(
  p10,
  "10_missing_determinants_by_subsample.png",
  width = 14,
  height = 12
)


# 11. COBERTURA SOCIODEMOGRÁFICA POR SUBMUESTRA

p11_data <- summary_07 %>%
  filter(analysis_sample != "POOLED_ALL") %>%
  mutate(
    analysis_sample = subsample_factor(analysis_sample),
    variable = clean_label(variable),
    prop_valid_pct = prop_valid * 100
  )

p11 <- ggplot(
  p11_data,
  aes(
    x = variable,
    y = prop_valid_pct,
    fill = analysis_sample
  )
) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_minimal(base_size = 10) +
  labs(
    title = "Sociodemographic coverage by subsample",
    x = "Variable",
    y = "Valid values (%)",
    fill = "Subsample"
  )

save_figure(
  p11,
  "11_sociodemographic_coverage_by_subsample.png",
  width = 14,
  height = 10
)


# 12. COBERTURA DE VARIABLES CANDIDATAS PARA PROPENSITY

# Se describe la disponibilidad de las variables por submuestra.
# WHY_LATAM aparece con fines descriptivos; no forma parte
# del modelo de propensity europeo.

p12_data <- summary_09 %>%
  filter(analysis_sample != "POOLED_ALL") %>%
  mutate(
    analysis_sample = subsample_factor(analysis_sample),
    variable = clean_label(variable),
    prop_valid_pct = prop_valid * 100
  )

p12 <- ggplot(
  p12_data,
  aes(
    x = variable,
    y = prop_valid_pct,
    fill = analysis_sample
  )
) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_minimal(base_size = 10) +
  labs(
    title = "Coverage of candidate European propensity variables",
    subtitle = paste(
      "WHY_LATAM is descriptive only;",
      "the European electoral propensity model does not apply to it"
    ),
    x = "Variable",
    y = "Valid values (%)",
    fill = "Subsample"
  )

save_figure(
  p12,
  "12_propensity_variable_coverage_by_subsample.png",
  width = 14,
  height = 9
)


# 13. APLICABILIDAD DEL MODELO DE PROPENSITY EUROPEO

p13_data <- summary_10 %>%
  filter(analysis_sample != "POOLED_ALL") %>%
  select(
    analysis_sample,
    n_rows,
    n_eu_applicable
  ) %>%
  pivot_longer(
    c(n_rows, n_eu_applicable),
    names_to = "metric",
    values_to = "n"
  ) %>%
  mutate(
    analysis_sample = subsample_factor(analysis_sample),
    
    metric = recode(
      metric,
      n_rows = "Total respondents",
      n_eu_applicable = "European propensity applicable"
    )
  )

p13 <- ggplot(
  p13_data,
  aes(
    x = analysis_sample,
    y = n,
    fill = metric
  )
) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3.5
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Applicability of the European propensity model",
    subtitle = paste(
      "WHY_LATAM is intentionally outside",
      "the European electoral propensity framework"
    ),
    x = "Subsample",
    y = "Number of respondents",
    fill = NULL
  )

save_figure(
  p13,
  "13_european_propensity_applicability.png",
  width = 12
)


# 14. DISPONIBILIDAD PARA LAS ESPECIFICACIONES DE PROPENSITY

# Este gráfico describe la disponibilidad de los datos requerida
# por las especificaciones indicadas en 03_1.
# No representa elegibilidad para clustering.

p14_data <- summary_10 %>%
  filter(
    analysis_sample %in% c(
      "DIEGO",
      "RENOVISOR",
      "WHY_EUROPE"
    )
  ) %>%
  select(
    analysis_sample,
    n_has_binary_vote_outcome,
    n_model_ready_without_income,
    n_model_ready_minimal
  ) %>%
  pivot_longer(
    -analysis_sample,
    names_to = "metric",
    values_to = "n"
  ) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = expected_subsamples[
        expected_subsamples != "WHY_LATAM"
      ]
    ),
    
    metric = recode(
      metric,
      
      n_has_binary_vote_outcome =
        "Observed binary vote outcome",
      
      n_model_ready_without_income =
        "Model-ready without income",
      
      n_model_ready_minimal =
        "Model-ready minimal"
    )
  )

p14 <- ggplot(
  p14_data,
  aes(
    x = analysis_sample,
    y = n,
    fill = metric
  )
) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3.2
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "European propensity-score readiness",
    subtitle = paste(
      "Readiness refers to the political propensity model,",
      "not to clustering eligibility"
    ),
    x = "European subsample",
    y = "Number of respondents",
    fill = NULL
  )

save_figure(
  p14,
  "14_european_propensity_readiness.png",
  width = 12
)


# 15. COBERTURA DE LAS RESPUESTAS A LOS BLOQUES DE FASE

# Las preguntas de fase proceden de RV Decision.
# Se representa el porcentaje de participantes con al menos
# una dimensión reconocida en alguna de las tres fases.

p15 <- summary_11_phase %>%
  mutate(
    analysis_sample = sample_factor(analysis_sample),
    
    prop_pct =
      prop_rows_with_any_phase_response * 100
  ) %>%
  ggplot(
    aes(
      x = analysis_sample,
      y = prop_pct
    )
  ) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(prop_pct, 1), "%")),
    vjust = -0.3,
    size = 3.7
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Respondents with any phase-dimension response",
    subtitle = paste(
      "These phase questions originate from RV Decision",
      "and therefore occur in RENOVISOR"
    ),
    x = "Analysis sample",
    y = "Respondents with any phase response (%)"
  )

save_figure(
  p15,
  "15_phase_response_coverage.png",
  width = 11
)


# 16. DIMENSIONES SELECCIONADAS EN LAS FASES DE RENOVISOR

# Para cada dimensión se representa el número de veces que
# ha sido reconocida en las respuestas de cada fase.

p16_data <- summary_13 %>%
  filter(analysis_sample == "RENOVISOR") %>%
  mutate(
    phase_prefix = recode(
      phase_prefix,
      phase_implemented_reasons = "Implemented reasons",
      phase_more_likely_1 = "More likely 1",
      phase_more_likely_2 = "More likely 2"
    ),
    
    dimension = factor(
      dimension,
      levels = rev(names(dimension_determinants))
    )
  )

p16 <- ggplot(
  p16_data,
  aes(
    x = dimension,
    y = n_selected,
    fill = phase_prefix
  )
) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_minimal(base_size = 12) +
  labs(
    title = "Selected dimensions by decision phase in RENOVISOR",
    subtitle = "Phase variables originate from RV Decision",
    x = "Dimension",
    y = "Number of selections",
    fill = "Phase"
  )

save_figure(
  p16,
  "16_selected_dimensions_by_phase_renovisor.png",
  width = 11,
  height = 7
)


# 17. SCORES AGREGADOS DE FASE EN RENOVISOR

# Cada score agregado es la media de los determinantes válidos
# asociados a las dimensiones seleccionadas por un participante
# en una fase concreta, calculada previamente en 03_2.
#
# Los resúmenes de 03_3 contienen medias por fuente y encuesta.
# Para obtener una media conjunta de RENOVISOR, se combinan
# esas medias utilizando como peso el número de scores válidos
# de cada grupo.
#
# Este cálculo es descriptivo y no introduce pesos muestrales
# ni modifica los scores individuales.

phase_score_plot_data <- summary_14 %>%
  filter(
    analysis_sample == "RENOVISOR",
    n_valid_score > 0
  ) %>%
  group_by(score_variable) %>%
  summarise(
    n_valid = sum(n_valid_score, na.rm = TRUE),
    
    mean_score = weighted.mean(
      mean_score,
      w = n_valid_score,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    score_variable = recode(
      score_variable,
      
      phase_implemented_reasons_selected_dimension_score_mean =
        "Implemented reasons",
      
      phase_more_likely_1_selected_dimension_score_mean =
        "More likely 1",
      
      phase_more_likely_2_selected_dimension_score_mean =
        "More likely 2"
    )
  )

# El gráfico solo se genera si existen scores válidos
# de alguna de las fases de decisión.

if (nrow(phase_score_plot_data)) {
  p17 <- ggplot(
    phase_score_plot_data,
    aes(
      x = score_variable,
      y = mean_score
    )
  ) +
    geom_col() +
    geom_text(
      aes(label = round(mean_score, 1)),
      vjust = -0.4,
      size = 4
    ) +
    theme_minimal(base_size = 13) +
    labs(
      title = "Mean selected-dimension scores in RENOVISOR",
      subtitle = "Weighted across RV records with a valid phase score",
      x = "Decision phase",
      y = "Mean score (0-100)"
    )
  
  save_figure(
    p17,
    "17_phase_dimension_scores_renovisor.png"
  )
}


# 18. NÚMERO DE DETERMINANTES ASOCIADOS A CADA DIMENSIÓN

# El mapeo de las nueve dimensiones se define en 00_common.R.
# Este gráfico representa el número de determinantes por dimensión,
# calculado en 03_3.

p18 <- summary_15 %>%
  mutate(
    dimension = factor(
      dimension,
      levels = dimension[order(n_determinants)]
    )
  ) %>%
  ggplot(
    aes(
      x = dimension,
      y = n_determinants
    )
  ) +
  geom_col() +
  geom_text(
    aes(label = n_determinants),
    hjust = -0.2,
    size = 4
  ) +
  coord_flip() +
  theme_minimal(base_size = 13) +
  labs(
    title = "Number of determinants mapped to each dimension",
    x = "Dimension",
    y = "Number of determinants"
  )

save_figure(
  p18,
  "18_dimension_determinant_mapping.png"
)


# 19. TAMAÑOS DISPONIBLES PARA CLUSTERING ANTES DEL BOOTSTRAP

# Se representan POOLED_ALL y las cuatro submuestras.
# Los tamaños proceden de la matriz filtrada en 03_1.

p19 <- summary_18 %>%
  mutate(
    analysis_sample = sample_factor(analysis_sample)
  ) %>%
  ggplot(
    aes(
      x = analysis_sample,
      y = n_usable_for_clustering
    )
  ) +
  geom_col() +
  geom_text(
    aes(label = n_usable_for_clustering),
    vjust = -0.3,
    size = 4
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Pre-bootstrap sample sizes available for clustering",
    subtitle = "Rows passing the determinant-quality criteria",
    x = "Analysis sample",
    y = "Usable respondents"
  )

save_figure(
  p19,
  "19_final_clustering_sample_sizes.png",
  width = 11
)


# 20. COMPOSICIÓN DE POOLED_ALL UTILIZABLE PARA CLUSTERING

# Se calcula qué proporción de la muestra filtrada para clustering
# corresponde a cada una de las cuatro submuestras.

pooled_clustering_composition <- summary_18 %>%
  filter(analysis_sample != "POOLED_ALL") %>%
  mutate(
    analysis_sample = subsample_factor(analysis_sample),
    
    prop_pooled =
      n_usable_for_clustering /
      sum(n_usable_for_clustering),
    
    prop_pooled_pct = prop_pooled * 100
  )

p20 <- ggplot(
  pooled_clustering_composition,
  aes(
    x = analysis_sample,
    y = prop_pooled_pct
  )
) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(prop_pooled_pct, 1), "%")),
    vjust = -0.3,
    size = 4
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Composition of POOLED_ALL after clustering-quality filtering",
    subtitle = paste0(
      "Total usable pooled sample: ",
      format(pooled_usable_n, big.mark = ","),
      " respondents"
    ),
    x = "Subsample",
    y = "Share of usable pooled sample (%)"
  )

save_figure(
  p20,
  "20_pooled_clustering_composition.png"
)


# 21. COMPARACIÓN DE TAMAÑOS ORIGINALES Y UTILIZABLES

# La comparación se realiza para POOLED_ALL y las cuatro submuestras.
# Los tamaños originales y los utilizables para clustering
# proceden de las tablas descriptivas de 03_3.

p21_data <- summary_02 %>%
  select(
    analysis_sample,
    n_rows,
    n_usable_for_clustering
  ) %>%
  pivot_longer(
    c(n_rows, n_usable_for_clustering),
    names_to = "metric",
    values_to = "n"
  ) %>%
  mutate(
    analysis_sample = sample_factor(analysis_sample),
    
    metric = recode(
      metric,
      n_rows = "Original rows",
      n_usable_for_clustering = "Usable for clustering"
    )
  )

p21 <- ggplot(
  p21_data,
  aes(
    x = analysis_sample,
    y = n,
    fill = metric
  )
) +
  geom_col(position = "dodge") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Original and clustering-usable sample sizes",
    x = "Analysis sample",
    y = "Number of respondents",
    fill = NULL
  )

save_figure(
  p21,
  "21_original_vs_final_analysis_sizes.png",
  width = 12
)


# ÍNDICE DE FIGURAS

# El índice registra los nombres y las descripciones de las figuras.
# La columna generated comprueba si cada archivo existe en out_dir.
#
# La figura 17 puede no generarse si no existen scores de fase válidos.

figure_index <- tribble(
  ~figure, ~description,
  
  "01_columns_by_processing_step.png",
  "Number of columns at each processing step.",
  
  "02_original_rows_by_subsample.png",
  "Original N of the four subsamples.",
  
  "03_original_sample_composition.png",
  "Relative composition of the integrated dataset.",
  
  "04_total_vs_clustering_usable_by_subsample.png",
  "Original versus clustering-usable N by subsample.",
  
  "05_prop_usable_for_clustering.png",
  "Percentage of rows usable for clustering.",
  
  "06_quality_categories_by_subsample.png",
  "Final row-quality categories by subsample.",
  
  "07_metadata_quality_by_subsample.png",
  "Metadata-quality categories by subsample.",
  
  "08_determinant_coverage_by_subsample.png",
  "Availability and completeness of the 32 determinants.",
  
  "09_complete_32det_by_sample.png",
  "Percentage with all 32 determinants available.",
  
  "10_missing_determinants_by_subsample.png",
  "Missingness of each determinant by subsample.",
  
  "11_sociodemographic_coverage_by_subsample.png",
  "Coverage of sociodemographic variables.",
  
  "12_propensity_variable_coverage_by_subsample.png",
  "Coverage of candidate variables for European propensity.",
  
  "13_european_propensity_applicability.png",
  "Applicability of the European propensity model.",
  
  "14_european_propensity_readiness.png",
  "Readiness of European samples for the political propensity model.",
  
  "15_phase_response_coverage.png",
  "Coverage of RV Decision phase questions.",
  
  "16_selected_dimensions_by_phase_renovisor.png",
  "Dimensions selected in the RV Decision phase questions.",
  
  "17_phase_dimension_scores_renovisor.png",
  "Mean determinant scores associated with selected dimensions.",
  
  "18_dimension_determinant_mapping.png",
  "Mapping of the 32 determinants to the 9 dimensions.",
  
  "19_final_clustering_sample_sizes.png",
  "Pre-bootstrap usable N for POOLED_ALL and each subsample.",
  
  "20_pooled_clustering_composition.png",
  "Composition of the usable POOLED_ALL sample.",
  
  "21_original_vs_final_analysis_sizes.png",
  "Original versus clustering-usable N for all five pre-bootstrap analysis samples."
) %>%
  mutate(
    generated = file.exists(file.path(out_dir, figure))
  )

write_csv(
  figure_index,
  file.path(out_dir, "figure_index.csv")
)


# RESUMEN EN CONSOLA

cat("\nSUBMUESTRAS ORIGINALES\n\n")

print(
  summary_01_subsamples %>%
    select(
      comparison_region,
      subsample,
      n_rows,
      prop_total
    ),
  n = Inf,
  width = Inf
)


cat("\nMUESTRAS PRE-BOOTSTRAP USABLES PARA CLUSTERING\n\n")

print(
  summary_18,
  n = Inf,
  width = Inf
)


cat("\nAPLICABILIDAD DEL PROPENSITY EUROPEO\n\n")

print(
  summary_10 %>%
    select(
      analysis_sample,
      n_rows,
      n_eu_applicable,
      prop_eu_applicable,
      n_model_ready_minimal
    ),
  n = Inf,
  width = Inf
)


cat("\nRESPUESTAS DE FASE\n\n")

print(
  summary_11_phase,
  n = Inf,
  width = Inf
)


cat(
  "\nFiguras disponibles: ",
  sum(figure_index$generated),
  " de ",
  nrow(figure_index),
  "\n",
  sep = ""
)

cat(
  "Directorio de salida: ",
  out_dir,
  "\n",
  sep = ""
)

message(
  "\n03_4. FIGURAS DESCRIPTIVAS PRE-BOOTSTRAP COMPLETADAS"
)