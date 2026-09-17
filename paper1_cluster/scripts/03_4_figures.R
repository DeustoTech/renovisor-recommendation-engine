# Objetivo
#
# Genera figuras del estado del dataset antes de los bootstraps finales.
#
# Describe:
#   POOLED_ALL
#   DIEGO
#   RENOVISOR
#   WHY_EUROPE
#   WHY_LATAM
#
# POOLED_ALL es la unión de las cuatro submuestras antes del bootstrap.
#
# La arquitectura posterior de modelado es:
#   EUROPE   -> bootstrap político/electoral (04_2b)
#   LATAM    -> bootstrap económico por ingreso (04_2e)
#   COMPLETE -> EUROPE + LATAM (04_2f)
#
# Este script es exclusivamente descriptivo y no remuestrea datos.


suppressPackageStartupMessages({
  library(tidyverse)
})


# Configuración
processed_root <- "paper1_cluster/data/processed"

summary_dir <- file.path(
  processed_root,
  "03_3_summary_dataset_status"
)

out_dir <- file.path(
  processed_root,
  "03_4_figures_current_dataset_status"
)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

ANALYSIS_SAMPLE_LEVELS <- c(
  "POOLED_ALL",
  "DIEGO",
  "RENOVISOR",
  "WHY_EUROPE",
  "WHY_LATAM"
)

SUBSAMPLE_LEVELS <- c(
  "DIEGO",
  "RENOVISOR",
  "WHY_EUROPE",
  "WHY_LATAM"
)


# Funciones auxiliares
save_plot <- function(
    p,
    filename,
    width = 10,
    height = 6
) {
  ggsave(
    file.path(
      out_dir,
      filename
    ),
    plot = p,
    width = width,
    height = height,
    dpi = 300
  )
}


read_required <- function(filename) {
  path <- file.path(
    summary_dir,
    filename
  )
  
  if (!file.exists(path)) {
    stop(
      "No existe el archivo requerido:\n",
      path,
      "\n\nEjecuta primero 03_3_summary_dataset_status.R"
    )
  }
  
  read_csv(
    path,
    show_col_types = FALSE
  )
}


clean_label <- function(x) {
  x %>%
    str_replace_all(
      "_",
      " "
    ) %>%
    str_squish()
}


sample_factor <- function(x) {
  factor(
    x,
    levels = ANALYSIS_SAMPLE_LEVELS
  )
}


subsample_factor <- function(x) {
  factor(
    x,
    levels = SUBSAMPLE_LEVELS
  )
}


# Leer resúmenes

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


# Comprobaciones
missing_analysis_samples <- setdiff(
  ANALYSIS_SAMPLE_LEVELS,
  unique(
    summary_18$analysis_sample
  )
)

if (length(missing_analysis_samples)) {
  stop(
    "Faltan muestras de análisis en summary_18: ",
    paste(
      missing_analysis_samples,
      collapse = ", "
    )
  )
}

original_total_n <- sum(
  summary_01_subsamples$n_rows,
  na.rm = TRUE
)

pooled_usable_n <- summary_18 %>%
  filter(
    analysis_sample == "POOLED_ALL"
  ) %>%
  pull(
    n_usable_for_clustering
  )

if (!length(pooled_usable_n)) {
  pooled_usable_n <- NA_real_
} else {
  pooled_usable_n <- pooled_usable_n[1]
}


# Columnas por paso del pipeline
p01 <- summary_00 %>%
  mutate(
    step = factor(
      step,
      levels = step
    )
  ) %>%
  ggplot(
    aes(
      x = step,
      y = n_cols
    )
  ) +
  geom_col() +
  geom_text(
    aes(
      label = n_cols
    ),
    vjust = -0.3,
    size = 3.5
  ) +
  theme_minimal(
    base_size = 13
  ) +
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

save_plot(
  p01,
  "01_columns_by_processing_step.png",
  width = 11
)


# Filas originales por submuestra
p02 <- summary_01_subsamples %>%
  mutate(
    subsample = subsample_factor(
      subsample
    )
  ) %>%
  ggplot(
    aes(
      x = subsample,
      y = n_rows
    )
  ) +
  geom_col() +
  geom_text(
    aes(
      label = n_rows
    ),
    vjust = -0.3,
    size = 4
  ) +
  theme_minimal(
    base_size = 13
  ) +
  labs(
    title = "Original sample size by subsample",
    subtitle = paste0(
      "Total integrated dataset: ",
      format(
        original_total_n,
        big.mark = ","
      ),
      " respondents"
    ),
    x = "Subsample",
    y = "Number of respondents"
  )

save_plot(
  p02,
  "02_original_rows_by_subsample.png"
)


# Composición del dataset original
p03 <- summary_01_subsamples %>%
  mutate(
    subsample = subsample_factor(
      subsample
    ),
    prop_pct = prop_total * 100
  ) %>%
  ggplot(
    aes(
      x = subsample,
      y = prop_pct
    )
  ) +
  geom_col() +
  geom_text(
    aes(
      label = paste0(
        round(
          prop_pct,
          1
        ),
        "%"
      )
    ),
    vjust = -0.3,
    size = 4
  ) +
  theme_minimal(
    base_size = 13
  ) +
  labs(
    title = "Composition of the integrated dataset",
    x = "Subsample",
    y = "Share of total sample (%)"
  )

save_plot(
  p03,
  "03_original_sample_composition.png"
)


# Total vs usable para clustering
p04_data <- summary_02 %>%
  filter(
    analysis_sample != "POOLED_ALL"
  ) %>%
  select(
    analysis_sample,
    n_rows,
    n_usable_for_clustering
  ) %>%
  pivot_longer(
    c(
      n_rows,
      n_usable_for_clustering
    ),
    names_to = "metric",
    values_to = "n"
  ) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = SUBSAMPLE_LEVELS
    ),
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
  geom_col(
    position = "dodge"
  ) +
  geom_text(
    aes(
      label = n
    ),
    position = position_dodge(
      width = 0.9
    ),
    vjust = -0.3,
    size = 3.5
  ) +
  theme_minimal(
    base_size = 13
  ) +
  labs(
    title = "Total and clustering-usable respondents by subsample",
    x = "Subsample",
    y = "Number of respondents",
    fill = NULL
  )

save_plot(
  p04,
  "04_total_vs_clustering_usable_by_subsample.png",
  width = 11
)


# Porcentaje usable para clustering
p05 <- summary_02 %>%
  mutate(
    analysis_sample = sample_factor(
      analysis_sample
    ),
    prop_pct =
      prop_usable_for_clustering *
      100
  ) %>%
  ggplot(
    aes(
      x = analysis_sample,
      y = prop_pct
    )
  ) +
  geom_col() +
  geom_text(
    aes(
      label = paste0(
        round(
          prop_pct,
          1
        ),
        "%"
      )
    ),
    vjust = -0.3,
    size = 4
  ) +
  theme_minimal(
    base_size = 13
  ) +
  labs(
    title = "Share of respondents usable for clustering",
    x = "Analysis sample",
    y = "Usable respondents (%)"
  )

save_plot(
  p05,
  "05_prop_usable_for_clustering.png",
  width = 11
)


# Categorías de calidad final
p06 <- summary_03 %>%
  filter(
    analysis_sample != "POOLED_ALL"
  ) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = SUBSAMPLE_LEVELS
    ),
    row_quality_final = clean_label(
      row_quality_final
    )
  ) %>%
  ggplot(
    aes(
      x = analysis_sample,
      y = n,
      fill = row_quality_final
    )
  ) +
  geom_col() +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    title = "Final row-quality categories by subsample",
    x = "Subsample",
    y = "Number of respondents",
    fill = "Row quality"
  )

save_plot(
  p06,
  "06_quality_categories_by_subsample.png",
  width = 12,
  height = 7
)


# Calidad de metadata
p07 <- summary_04 %>%
  filter(
    analysis_sample != "POOLED_ALL"
  ) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = SUBSAMPLE_LEVELS
    ),
    metadata_quality = clean_label(
      metadata_quality
    )
  ) %>%
  ggplot(
    aes(
      x = analysis_sample,
      y = n,
      fill = metadata_quality
    )
  ) +
  geom_col() +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    title = "Metadata quality by subsample",
    x = "Subsample",
    y = "Number of respondents",
    fill = "Metadata quality"
  )

save_plot(
  p07,
  "07_metadata_quality_by_subsample.png",
  width = 12,
  height = 7
)


# Cobertura de los 32 determinantes
p08_data <- summary_05 %>%
  filter(
    analysis_sample != "POOLED_ALL"
  ) %>%
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
    analysis_sample = factor(
      analysis_sample,
      levels = SUBSAMPLE_LEVELS
    ),
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
  geom_col(
    position = "dodge"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  labs(
    title = "Coverage of the 32 determinants by subsample",
    x = "Subsample",
    y = "Number of respondents",
    fill = NULL
  )

save_plot(
  p08,
  "08_determinant_coverage_by_subsample.png",
  width = 13,
  height = 7
)


# Perfiles completos de 32 determinantes
p09 <- summary_05 %>%
  mutate(
    analysis_sample = sample_factor(
      analysis_sample
    ),
    prop_complete_pct =
      prop_complete_32det *
      100
  ) %>%
  ggplot(
    aes(
      x = analysis_sample,
      y = prop_complete_pct
    )
  ) +
  geom_col() +
  geom_text(
    aes(
      label = paste0(
        round(
          prop_complete_pct,
          1
        ),
        "%"
      )
    ),
    vjust = -0.3,
    size = 3.7
  ) +
  theme_minimal(
    base_size = 13
  ) +
  labs(
    title = "Respondents with all 32 determinants available",
    x = "Analysis sample",
    y = "Complete 32-determinant profiles (%)"
  )

save_plot(
  p09,
  "09_complete_32det_by_sample.png",
  width = 11
)


# Missing por determinante
p10_data <- summary_06 %>%
  filter(
    analysis_sample != "POOLED_ALL"
  ) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = SUBSAMPLE_LEVELS
    ),
    determinant = determinant %>%
      str_remove(
        "^det_\\d{2}_"
      ) %>%
      clean_label(),
    prop_missing_pct =
      prop_missing *
      100
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
  theme_minimal(
    base_size = 10
  ) +
  labs(
    title = "Missingness of the 32 determinants by subsample",
    x = "Determinant",
    y = "Missing values (%)"
  )

save_plot(
  p10,
  "10_missing_determinants_by_subsample.png",
  width = 14,
  height = 12
)


# Cobertura sociodemográfica
p11_data <- summary_07 %>%
  filter(
    analysis_sample != "POOLED_ALL"
  ) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = SUBSAMPLE_LEVELS
    ),
    variable = clean_label(
      variable
    ),
    prop_valid_pct =
      prop_valid *
      100
  )

p11 <- ggplot(
  p11_data,
  aes(
    x = variable,
    y = prop_valid_pct,
    fill = analysis_sample
  )
) +
  geom_col(
    position = "dodge"
  ) +
  coord_flip() +
  theme_minimal(
    base_size = 10
  ) +
  labs(
    title = "Sociodemographic coverage by subsample",
    x = "Variable",
    y = "Valid values (%)",
    fill = "Subsample"
  )

save_plot(
  p11,
  "11_sociodemographic_coverage_by_subsample.png",
  width = 14,
  height = 10
)


# Cobertura de variables candidatas para propensity
p12_data <- summary_09 %>%
  filter(
    analysis_sample != "POOLED_ALL"
  ) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = SUBSAMPLE_LEVELS
    ),
    variable = clean_label(
      variable
    ),
    prop_valid_pct =
      prop_valid *
      100
  )

p12 <- ggplot(
  p12_data,
  aes(
    x = variable,
    y = prop_valid_pct,
    fill = analysis_sample
  )
) +
  geom_col(
    position = "dodge"
  ) +
  coord_flip() +
  theme_minimal(
    base_size = 10
  ) +
  labs(
    title = "Coverage of candidate European propensity variables",
    subtitle = "WHY_LATAM is descriptive only; the European electoral propensity model does not apply to it",
    x = "Variable",
    y = "Valid values (%)",
    fill = "Subsample"
  )

save_plot(
  p12,
  "12_propensity_variable_coverage_by_subsample.png",
  width = 14,
  height = 9
)


# Aplicabilidad del propensity europeo
p13_data <- summary_10 %>%
  filter(
    analysis_sample != "POOLED_ALL"
  ) %>%
  select(
    analysis_sample,
    n_rows,
    n_eu_applicable
  ) %>%
  pivot_longer(
    c(
      n_rows,
      n_eu_applicable
    ),
    names_to = "metric",
    values_to = "n"
  ) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = SUBSAMPLE_LEVELS
    ),
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
  geom_col(
    position = "dodge"
  ) +
  geom_text(
    aes(
      label = n
    ),
    position = position_dodge(
      width = 0.9
    ),
    vjust = -0.3,
    size = 3.5
  ) +
  theme_minimal(
    base_size = 13
  ) +
  labs(
    title = "Applicability of the European propensity model",
    subtitle = "WHY_LATAM is intentionally outside the European electoral propensity framework",
    x = "Subsample",
    y = "Number of respondents",
    fill = NULL
  )

save_plot(
  p13,
  "13_european_propensity_applicability.png",
  width = 12
)


# Readiness del propensity europeo
p14_data <- summary_10 %>%
  filter(
    analysis_sample %in%
      c(
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
      levels = c(
        "DIEGO",
        "RENOVISOR",
        "WHY_EUROPE"
      )
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
  geom_col(
    position = "dodge"
  ) +
  geom_text(
    aes(
      label = n
    ),
    position = position_dodge(
      width = 0.9
    ),
    vjust = -0.3,
    size = 3.2
  ) +
  theme_minimal(
    base_size = 12
  ) +
  labs(
    title = "European propensity-score readiness",
    subtitle = "Readiness refers to the political propensity model, not to clustering eligibility",
    x = "European subsample",
    y = "Number of respondents",
    fill = NULL
  )

save_plot(
  p14,
  "14_european_propensity_readiness.png",
  width = 12
)


# Respuestas a bloques de fase
p15 <- summary_11_phase %>%
  mutate(
    analysis_sample = sample_factor(
      analysis_sample
    ),
    prop_pct =
      prop_rows_with_any_phase_response *
      100
  ) %>%
  ggplot(
    aes(
      x = analysis_sample,
      y = prop_pct
    )
  ) +
  geom_col() +
  geom_text(
    aes(
      label = paste0(
        round(
          prop_pct,
          1
        ),
        "%"
      )
    ),
    vjust = -0.3,
    size = 3.7
  ) +
  theme_minimal(
    base_size = 13
  ) +
  labs(
    title = "Respondents with any phase-dimension response",
    subtitle = "These phase questions originate from RV Decision and therefore occur in RENOVISOR",
    x = "Analysis sample",
    y = "Respondents with any phase response (%)"
  )

save_plot(
  p15,
  "15_phase_response_coverage.png",
  width = 11
)


# Dimensiones seleccionadas en RENOVISOR
p16_data <- summary_13 %>%
  filter(
    analysis_sample == "RENOVISOR"
  ) %>%
  mutate(
    phase_prefix = recode(
      phase_prefix,
      phase_implemented_reasons = "Implemented reasons",
      phase_more_likely_1 = "More likely 1",
      phase_more_likely_2 = "More likely 2"
    ),
    dimension = factor(
      dimension,
      levels = rev(
        c(
          "FINANCIAL",
          "SECURITY",
          "COMPETENCE",
          "AUTONOMY",
          "PHYSIOLOGICAL",
          "RELATEDNESS",
          "STIMULATION",
          "POPULARITY",
          "MEANING"
        )
      )
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
  geom_col(
    position = "dodge"
  ) +
  coord_flip() +
  theme_minimal(
    base_size = 12
  ) +
  labs(
    title = "Selected dimensions by decision phase in RENOVISOR",
    subtitle = "Phase variables originate from RV Decision",
    x = "Dimension",
    y = "Number of selections",
    fill = "Phase"
  )

save_plot(
  p16,
  "16_selected_dimensions_by_phase_renovisor.png",
  width = 11,
  height = 7
)


# Scores de dimensiones seleccionadas
phase_score_plot_data <- summary_14 %>%
  filter(
    analysis_sample == "RENOVISOR",
    n_valid_score > 0
  ) %>%
  group_by(
    score_variable
  ) %>%
  summarise(
    n_valid = sum(
      n_valid_score,
      na.rm = TRUE
    ),
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
      aes(
        label = round(
          mean_score,
          1
        )
      ),
      vjust = -0.4,
      size = 4
    ) +
    theme_minimal(
      base_size = 13
    ) +
    labs(
      title = "Mean selected-dimension scores in RENOVISOR",
      subtitle = "Weighted across RV records with a valid phase score",
      x = "Decision phase",
      y = "Mean score (0-100)"
    )
  
  save_plot(
    p17,
    "17_phase_dimension_scores_renovisor.png"
  )
}


# Mapeo dimensiones-determinante
p18 <- summary_15 %>%
  mutate(
    dimension = factor(
      dimension,
      levels = dimension[
        order(
          n_determinants
        )
      ]
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
    aes(
      label = n_determinants
    ),
    hjust = -0.2,
    size = 4
  ) +
  coord_flip() +
  theme_minimal(
    base_size = 13
  ) +
  labs(
    title = "Number of determinants mapped to each dimension",
    x = "Dimension",
    y = "Number of determinants"
  )

save_plot(
  p18,
  "18_dimension_determinant_mapping.png"
)


# Tamaños finales disponibles para clustering
p19 <- summary_18 %>%
  mutate(
    analysis_sample = sample_factor(
      analysis_sample
    )
  ) %>%
  ggplot(
    aes(
      x = analysis_sample,
      y = n_usable_for_clustering
    )
  ) +
  geom_col() +
  geom_text(
    aes(
      label = n_usable_for_clustering
    ),
    vjust = -0.3,
    size = 4
  ) +
  theme_minimal(
    base_size = 13
  ) +
  labs(
    title = "Pre-bootstrap sample sizes available for clustering",
    subtitle = "Rows passing the determinant-quality criteria",
    x = "Analysis sample",
    y = "Usable respondents"
  )

save_plot(
  p19,
  "19_final_clustering_sample_sizes.png",
  width = 11
)


# Composición del POOLED_ALL usable
pooled_clustering_composition <- summary_18 %>%
  filter(
    analysis_sample != "POOLED_ALL"
  ) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = SUBSAMPLE_LEVELS
    ),
    prop_pooled =
      n_usable_for_clustering /
      sum(
        n_usable_for_clustering
      ),
    prop_pooled_pct =
      prop_pooled *
      100
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
    aes(
      label = paste0(
        round(
          prop_pooled_pct,
          1
        ),
        "%"
      )
    ),
    vjust = -0.3,
    size = 4
  ) +
  theme_minimal(
    base_size = 13
  ) +
  labs(
    title = "Composition of POOLED_ALL after clustering-quality filtering",
    subtitle = paste0(
      "Total usable pooled sample: ",
      format(
        pooled_usable_n,
        big.mark = ","
      ),
      " respondents"
    ),
    x = "Subsample",
    y = "Share of usable pooled sample (%)"
  )

save_plot(
  p20,
  "20_pooled_clustering_composition.png"
)


# Total original vs usable final
p21_data <- summary_02 %>%
  select(
    analysis_sample,
    n_rows,
    n_usable_for_clustering
  ) %>%
  pivot_longer(
    c(
      n_rows,
      n_usable_for_clustering
    ),
    names_to = "metric",
    values_to = "n"
  ) %>%
  mutate(
    analysis_sample = sample_factor(
      analysis_sample
    ),
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
  geom_col(
    position = "dodge"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  labs(
    title = "Original and clustering-usable sample sizes",
    x = "Analysis sample",
    y = "Number of respondents",
    fill = NULL
  )

save_plot(
  p21,
  "21_original_vs_final_analysis_sizes.png",
  width = 12
)


# Índice de figuras
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
)

write_csv(
  figure_index,
  file.path(
    out_dir,
    "figure_index.csv"
  )
)


# Resumen
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
  "\nFiguras generadas: ",
  nrow(
    figure_index
  ),
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
  "\nListo. Figuras descriptivas pre-bootstrap generadas para POOLED_ALL y las cuatro submuestras."
)