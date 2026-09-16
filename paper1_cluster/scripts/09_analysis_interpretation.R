
# Resumir y visualizar conjuntamente los resultados de:
# - 06 K-means
# - 07 EFA
# - 08 Greedy
#
# Este script:
# - compara COMPLETE, EUROPE, LATAM y cada submuestra;
# - resume K-means para K = 2:8;
# - resume EFA para F = 2:8;
# - analiza Greedy para las cuatro matrices y ambas ponderaciones;
# - estudia sensibilidad a D y Hamming;
# - compara cobertura entre muestras;
# - compara K-means y EFA como generadores de patrones.
#
# IMPORTANTE:
# Este bloque todavía no fija K, número de factores, D, Hamming
# ni número final de prototipos.
#
# COMPLETE + RAW + D=8 + H=4 se utiliza únicamente como referencia
# visual para facilitar la interpretación de algunos resultados.


suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

# Configuración
project_root <- path.expand(
  "~/Desktop/MASTER/recommendation-engine/TFM"
)

processed_root <- file.path(
  project_root,
  "paper1_cluster/data/processed"
)

kmeans_dir <- file.path(
  processed_root,
  "06_kmeans_bootstrap"
)

efa_dir <- file.path(
  processed_root,
  "07_efa_bootstrap"
)

greedy_dir <- file.path(
  processed_root,
  "08_greedy_kmeans_efa"
)

out_dir <- file.path(
  processed_root,
  "09_analysis_plots"
)

fig_dir <- file.path(
  out_dir,
  "figures"
)

fig_kmeans_dir <- file.path(
  fig_dir,
  "kmeans"
)

fig_efa_dir <- file.path(
  fig_dir,
  "efa"
)

fig_greedy_dir <- file.path(
  fig_dir,
  "greedy"
)

fig_profiles_dir <- file.path(
  fig_dir,
  "profiles"
)

walk(
  c(
    out_dir,
    fig_dir,
    fig_kmeans_dir,
    fig_efa_dir,
    fig_greedy_dir,
    fig_profiles_dir
  ),
  ~ dir.create(
    .x,
    recursive = TRUE,
    showWarnings = FALSE
  )
)

ANALYSIS_SAMPLES <- c(
  "COMPLETE",
  "EUROPE",
  "LATAM",
  "DIEGO",
  "RENOVISOR",
  "WHY_EUROPE",
  "WHY_LATAM"
)

MATRICES_TO_RUN <- c(
  "matrix_32_raw_0_1",
  "matrix_32_pos_0_1",
  "matrix_32_ext_0_1",
  "matrix_32_z_abs"
)

# Configuración de referencia utilizada solo para algunos diagnósticos
# y figuras de interpretación.
REFERENCE_SAMPLE <- "COMPLETE"
REFERENCE_MATRIX <- "matrix_32_raw_0_1"
REFERENCE_WEIGHTING <- "equal_candidate"
REFERENCE_METHOD <- "KMEANS"
REFERENCE_D_DET <- 8
REFERENCE_D_HAMMING <- 4

D_DET_GRID <- 8:15

HAMMING_GRID_PLOT <- seq(
  0,
  10,
  by = 2
)

COVERAGE_THRESHOLDS_PLOT <- c(
  80,
  85,
  90
)

PROTOTYPES_OF_INTEREST <- c(
  6,
  7,
  8
)

REFERENCE_COMMON_PCT <- 75

SIMILARITY_MIN <- 75
SIMILARITY_MAX <- 85


# Funciones auxiliares

matrix_label <- function(x) {
  recode(
    x,
    "matrix_32_raw_0_1" = "RAW",
    "matrix_32_pos_0_1" = "POS",
    "matrix_32_ext_0_1" = "EXT",
    "matrix_32_z_abs" = "Z_ABS",
    .default = x
  )
}

clean_determinant_label <- function(x) {
  x %>%
    str_remove(
      "^det_\\d+_"
    ) %>%
    str_replace_all(
      "_",
      " "
    ) %>%
    str_to_sentence()
}

save_plot <- function(
    plot,
    filename,
    width = 10,
    height = 7
) {
  ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

write_output <- function(
    data,
    filename
) {
  write_csv(
    data,
    file.path(
      out_dir,
      filename
    )
  )
}

theme_paper <- function(
    base_size = 12
) {
  theme_minimal(
    base_size = base_size
  ) +
    theme(
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


# Inputs

kmeans_metrics_file <- file.path(
  kmeans_dir,
  "kmeans_metrics_summary_all_samples.csv"
)

kmeans_sizes_file <- file.path(
  kmeans_dir,
  "kmeans_cluster_size_summary_all_samples.csv"
)

efa_summary_file <- file.path(
  efa_dir,
  "02_efa_metrics_summary.csv"
)

greedy_steps_file <- file.path(
  greedy_dir,
  "03_greedy_prototype_steps_pooled.csv"
)

greedy_prevalence_file <- file.path(
  greedy_dir,
  "05_greedy_ball_determinant_prevalence_pooled.csv.gz"
)

greedy_hamming_file <- file.path(
  greedy_dir,
  "07_hamming_interpretation_pooled.csv"
)

greedy_thresholds_file <- file.path(
  greedy_dir,
  "08_coverage_threshold_diagnostic_pooled.csv"
)

required_files <- c(
  kmeans_metrics_file,
  kmeans_sizes_file,
  efa_summary_file,
  greedy_steps_file,
  greedy_prevalence_file,
  greedy_hamming_file,
  greedy_thresholds_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files)) {
  stop(
    "Faltan archivos necesarios:\n",
    paste(
      missing_files,
      collapse = "\n"
    )
  )
}


# K-means

kmeans_summary <- read_csv(
  kmeans_metrics_file,
  show_col_types = FALSE
) %>%
  filter(
    analysis_sample %in% ANALYSIS_SAMPLES,
    matrix_name %in% MATRICES_TO_RUN
  ) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = ANALYSIS_SAMPLES
    ),
    
    matrix = matrix_label(
      matrix_name
    )
  )

required_kmeans_cols <- c(
  "analysis_sample",
  "matrix_name",
  "k",
  "n_runs",
  "n_ok",
  "n_error",
  "mean_tot_withinss",
  "mean_between_over_total",
  "mean_calinski_harabasz",
  "mean_silhouette",
  "mean_min_cluster_distance"
)

missing_kmeans_cols <- setdiff(
  required_kmeans_cols,
  names(kmeans_summary)
)

if (length(missing_kmeans_cols)) {
  stop(
    "Faltan columnas K-means: ",
    paste(
      missing_kmeans_cols,
      collapse = ", "
    )
  )
}


# Ganancia marginal al aumentar K.

kmeans_delta <- kmeans_summary %>%
  group_by(
    analysis_sample,
    matrix_name
  ) %>%
  arrange(
    k,
    .by_group = TRUE
  ) %>%
  mutate(
    delta_between_over_total =
      mean_between_over_total -
      lag(
        mean_between_over_total
      ),
    
    withinss_reduction_pct =
      100 *
      (
        lag(
          mean_tot_withinss
        ) -
          mean_tot_withinss
      ) /
      lag(
        mean_tot_withinss
      )
  ) %>%
  ungroup()


# Diagnóstico del tamaño de los clusters.

cluster_size_summary <- read_csv(
  kmeans_sizes_file,
  show_col_types = FALSE
) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = ANALYSIS_SAMPLES
    ),
    
    matrix = matrix_label(
      matrix_name
    )
  )

cluster_size_diagnostic <- cluster_size_summary %>%
  group_by(
    analysis_sample,
    matrix_name,
    matrix,
    k
  ) %>%
  summarise(
    mean_smallest_cluster_share = min(
      mean_prop_cluster,
      na.rm = TRUE
    ),
    
    min_observed_cluster_n = min(
      min_n_cluster,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

write_output(
  kmeans_summary,
  "01_kmeans_summary.csv"
)

write_output(
  kmeans_delta,
  "02_kmeans_marginal_gain.csv"
)

write_output(
  cluster_size_diagnostic,
  "03_kmeans_cluster_size_diagnostic.csv"
)


# Figuras K-means para COMPLETE.

complete_kmeans <- kmeans_summary %>%
  filter(
    analysis_sample ==
      REFERENCE_SAMPLE
  )

p_kmeans_between <- ggplot(
  complete_kmeans,
  aes(
    x = k,
    y = mean_between_over_total
  )
) +
  geom_line() +
  geom_point(
    size = 2.5
  ) +
  facet_wrap(
    ~ matrix,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = 2:8
  ) +
  labs(
    title = "K-means - COMPLETE - Between / Total",
    
    subtitle = paste(
      "Diagnostic only; increasing K normally increases",
      "explained between-cluster variation"
    ),
    
    x = "Number of clusters (K)",
    y = "Between SS / Total SS"
  ) +
  theme_paper()

save_plot(
  p_kmeans_between,
  file.path(
    fig_kmeans_dir,
    "01_kmeans_between_over_total_COMPLETE.png"
  ),
  width = 11,
  height = 7
)

p_kmeans_ch <- ggplot(
  complete_kmeans,
  aes(
    x = k,
    y = mean_calinski_harabasz
  )
) +
  geom_line() +
  geom_point(
    size = 2.5
  ) +
  facet_wrap(
    ~ matrix,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = 2:8
  ) +
  labs(
    title = "K-means - COMPLETE - Calinski-Harabasz",
    x = "Number of clusters (K)",
    y = "Calinski-Harabasz"
  ) +
  theme_paper()

save_plot(
  p_kmeans_ch,
  file.path(
    fig_kmeans_dir,
    "02_kmeans_calinski_harabasz_COMPLETE.png"
  ),
  width = 11,
  height = 7
)

p_kmeans_silhouette <- ggplot(
  complete_kmeans,
  aes(
    x = k,
    y = mean_silhouette
  )
) +
  geom_line() +
  geom_point(
    size = 2.5
  ) +
  facet_wrap(
    ~ matrix
  ) +
  scale_x_continuous(
    breaks = 2:8
  ) +
  labs(
    title = "K-means - COMPLETE - Silhouette",
    
    subtitle =
      "Higher values indicate clearer separation between clusters",
    
    x = "Number of clusters (K)",
    y = "Mean silhouette"
  ) +
  theme_paper()

save_plot(
  p_kmeans_silhouette,
  file.path(
    fig_kmeans_dir,
    "03_kmeans_silhouette_COMPLETE.png"
  ),
  width = 11,
  height = 7
)


# Comparación de silhouette entre muestras y matrices.

p_kmeans_samples <- ggplot(
  kmeans_summary,
  aes(
    x = k,
    y = mean_silhouette
  )
) +
  geom_line() +
  geom_point(
    size = 1.3
  ) +
  facet_grid(
    analysis_sample ~ matrix,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = 2:8
  ) +
  labs(
    title = "K-means silhouette by sample and matrix",
    x = "K",
    y = "Mean silhouette"
  ) +
  theme_paper(
    base_size = 9
  )

save_plot(
  p_kmeans_samples,
  file.path(
    fig_kmeans_dir,
    "04_kmeans_silhouette_all_samples.png"
  ),
  width = 15,
  height = 18
)


# EFA

efa_summary <- read_csv(
  efa_summary_file,
  show_col_types = FALSE
) %>%
  filter(
    analysis_sample %in% ANALYSIS_SAMPLES,
    matrix_name %in% MATRICES_TO_RUN
  ) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = ANALYSIS_SAMPLES
    ),
    
    matrix = matrix_label(
      matrix_name
    )
  )

write_output(
  efa_summary,
  "04_efa_summary.csv"
)

complete_efa <- efa_summary %>%
  filter(
    analysis_sample ==
      REFERENCE_SAMPLE
  )


# BIC por número de factores.

p_efa_bic <- ggplot(
  complete_efa,
  aes(
    x = n_factors,
    y = mean_BIC
  )
) +
  geom_line() +
  geom_point(
    size = 2.5
  ) +
  facet_wrap(
    ~ matrix,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = 2:8
  ) +
  labs(
    title = "EFA - COMPLETE - BIC",
    
    subtitle =
      "Lower BIC is preferred within the same matrix transformation",
    
    x = "Number of factors",
    y = "Mean BIC"
  ) +
  theme_paper()

save_plot(
  p_efa_bic,
  file.path(
    fig_efa_dir,
    "05_efa_bic_COMPLETE.png"
  ),
  width = 11,
  height = 7
)


# Métricas de ajuste EFA.

efa_fit_long <- complete_efa %>%
  select(
    matrix,
    n_factors,
    mean_RMSR,
    mean_TLI,
    mean_RMSEA
  ) %>%
  pivot_longer(
    cols = c(
      mean_RMSR,
      mean_TLI,
      mean_RMSEA
    ),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      "mean_RMSR" = "RMSR",
      "mean_TLI" = "TLI",
      "mean_RMSEA" = "RMSEA"
    )
  )

p_efa_fit <- ggplot(
  efa_fit_long,
  aes(
    x = n_factors,
    y = value,
    color = matrix,
    group = matrix
  )
) +
  geom_line() +
  geom_point(
    size = 2.2
  ) +
  facet_wrap(
    ~ metric,
    scales = "free_y",
    ncol = 1
  ) +
  scale_x_continuous(
    breaks = 2:8
  ) +
  labs(
    title = "EFA - COMPLETE - Fit metrics",
    x = "Number of factors",
    y = "Metric",
    color = "Matrix"
  ) +
  theme_paper()

save_plot(
  p_efa_fit,
  file.path(
    fig_efa_dir,
    "06_efa_fit_COMPLETE.png"
  ),
  width = 10,
  height = 10
)


# Comparación del BIC entre muestras.

p_efa_samples <- ggplot(
  efa_summary,
  aes(
    x = n_factors,
    y = mean_BIC
  )
) +
  geom_line() +
  geom_point(
    size = 1.3
  ) +
  facet_grid(
    analysis_sample ~ matrix,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = 2:8
  ) +
  labs(
    title = "EFA BIC by sample and matrix",
    x = "Number of factors",
    y = "Mean BIC"
  ) +
  theme_paper(
    base_size = 9
  )

save_plot(
  p_efa_samples,
  file.path(
    fig_efa_dir,
    "07_efa_bic_all_samples.png"
  ),
  width = 15,
  height = 18
)


# Greedy

greedy_steps <- read_csv(
  greedy_steps_file,
  show_col_types = FALSE
) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = ANALYSIS_SAMPLES
    ),
    
    matrix = matrix_label(
      matrix_name
    )
  )

greedy_prevalence <- read_csv(
  greedy_prevalence_file,
  show_col_types = FALSE
) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = ANALYSIS_SAMPLES
    ),
    
    matrix = matrix_label(
      matrix_name
    )
  )

hamming_interpretation <- read_csv(
  greedy_hamming_file,
  show_col_types = FALSE
)

greedy_thresholds <- read_csv(
  greedy_thresholds_file,
  show_col_types = FALSE
) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = ANALYSIS_SAMPLES
    ),
    
    matrix = matrix_label(
      matrix_name
    )
  )


# Referencia provisional para estudiar sensibilidad D × Hamming.
# No representa una selección definitiva de parámetros.

greedy_reference_steps <- greedy_steps %>%
  filter(
    analysis_sample ==
      REFERENCE_SAMPLE,
    
    weighting ==
      REFERENCE_WEIGHTING,
    
    method ==
      REFERENCE_METHOD,
    
    matrix_name ==
      REFERENCE_MATRIX,
    
    d_det %in%
      D_DET_GRID,
    
    d_hamming %in%
      HAMMING_GRID_PLOT
  )

greedy_reference_thresholds <- greedy_thresholds %>%
  filter(
    analysis_sample ==
      REFERENCE_SAMPLE,
    
    weighting ==
      REFERENCE_WEIGHTING,
    
    method ==
      REFERENCE_METHOD,
    
    matrix_name ==
      REFERENCE_MATRIX,
    
    coverage_threshold %in%
      COVERAGE_THRESHOLDS_PLOT,
    
    d_det %in%
      D_DET_GRID,
    
    d_hamming %in%
      HAMMING_GRID_PLOT
  )


# Sensibilidad D × Hamming:
# número de prototipos necesarios para alcanzar cada nivel de cobertura.

threshold_wide <- greedy_reference_thresholds %>%
  select(
    d_det,
    d_hamming,
    min_common_determinants,
    min_common_pct,
    coverage_threshold,
    first_prototype_reaching_threshold
  ) %>%
  pivot_wider(
    names_from =
      coverage_threshold,
    
    values_from =
      first_prototype_reaching_threshold,
    
    names_prefix =
      "prototypes_for_"
  )


# Cobertura obtenida con 6, 7 y 8 prototipos.

coverage_fixed <- greedy_reference_steps %>%
  filter(
    prototype %in%
      PROTOTYPES_OF_INTEREST
  ) %>%
  select(
    d_det,
    d_hamming,
    prototype,
    cumulative_covered_pct
  ) %>%
  pivot_wider(
    names_from =
      prototype,
    
    values_from =
      cumulative_covered_pct,
    
    names_prefix =
      "coverage_with_"
  )

greedy_sensitivity_table <- threshold_wide %>%
  left_join(
    coverage_fixed,
    by = c(
      "d_det",
      "d_hamming"
    )
  ) %>%
  arrange(
    d_det,
    d_hamming
  )

write_output(
  greedy_sensitivity_table,
  "05_GREEDY_COMPLETE_sensitivity_D_H.csv"
)


# Heatmap: número de prototipos necesarios para cada cobertura.

max_prototypes_available <- max(
  greedy_reference_steps$prototype,
  na.rm = TRUE
)

threshold_heatmap_data <- greedy_reference_thresholds %>%
  mutate(
    threshold_label = paste0(
      coverage_threshold,
      "% coverage"
    ),
    
    prototype_label = if_else(
      is.na(
        first_prototype_reaching_threshold
      ),
      
      paste0(
        ">",
        max_prototypes_available
      ),
      
      as.character(
        first_prototype_reaching_threshold
      )
    )
  )

p_threshold_heatmap <- ggplot(
  threshold_heatmap_data,
  aes(
    x = factor(
      d_hamming
    ),
    
    y = factor(
      d_det
    ),
    
    fill =
      first_prototype_reaching_threshold
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(
      label = prototype_label
    ),
    size = 3.2
  ) +
  facet_wrap(
    ~ threshold_label,
    nrow = 1
  ) +
  labs(
    title =
      "Greedy - COMPLETE - prototypes needed",
    
    subtitle = paste0(
      "K-means RAW | weighting = ",
      REFERENCE_WEIGHTING
    ),
    
    x = "Hamming radius",
    y = "Selected determinants (D)",
    fill = "Prototypes"
  ) +
  theme_paper(
    base_size = 11
  )

save_plot(
  p_threshold_heatmap,
  file.path(
    fig_greedy_dir,
    "08_greedy_COMPLETE_prototypes_needed_D_H.png"
  ),
  width = 15,
  height = 6
)


# Heatmap de cobertura con 6, 7 y 8 prototipos.

fixed_profiles_plot_data <- greedy_reference_steps %>%
  filter(
    prototype %in%
      PROTOTYPES_OF_INTEREST
  ) %>%
  mutate(
    prototype_label = paste0(
      prototype,
      " prototypes"
    )
  )

p_fixed_profiles <- ggplot(
  fixed_profiles_plot_data,
  aes(
    x = factor(
      d_hamming
    ),
    
    y = factor(
      d_det
    ),
    
    fill =
      cumulative_covered_pct
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(
      label = paste0(
        round(
          cumulative_covered_pct,
          1
        ),
        "%"
      )
    ),
    size = 3
  ) +
  facet_wrap(
    ~ prototype_label,
    nrow = 1
  ) +
  labs(
    title =
      "Greedy - COMPLETE - coverage with 6, 7 and 8 prototypes",
    
    subtitle = paste0(
      "K-means RAW | weighting = ",
      REFERENCE_WEIGHTING
    ),
    
    x = "Hamming radius",
    y = "Selected determinants (D)",
    fill = "Coverage (%)"
  ) +
  theme_paper(
    base_size = 11
  )

save_plot(
  p_fixed_profiles,
  file.path(
    fig_greedy_dir,
    "09_greedy_COMPLETE_coverage_6_7_8.png"
  ),
  width = 15,
  height = 6
)


# Comparación entre muestras con la referencia provisional D=8, H=4.

sample_comparison <- greedy_thresholds %>%
  filter(
    weighting ==
      REFERENCE_WEIGHTING,
    
    method ==
      REFERENCE_METHOD,
    
    matrix_name ==
      REFERENCE_MATRIX,
    
    d_det ==
      REFERENCE_D_DET,
    
    d_hamming ==
      REFERENCE_D_HAMMING,
    
    coverage_threshold %in%
      COVERAGE_THRESHOLDS_PLOT
  ) %>%
  select(
    analysis_sample,
    coverage_threshold,
    first_prototype_reaching_threshold,
    max_coverage_available
  )

write_output(
  sample_comparison,
  "06_GREEDY_sample_comparison_D8_H4.csv"
)

p_sample_comparison <- ggplot(
  sample_comparison,
  aes(
    x = analysis_sample,
    
    y =
      first_prototype_reaching_threshold,
    
    group = factor(
      coverage_threshold
    ),
    
    color = factor(
      coverage_threshold
    )
  )
) +
  geom_line() +
  geom_point(
    size = 2.5
  ) +
  labs(
    title =
      "Greedy comparison across samples",
    
    subtitle =
      "K-means RAW | D=8 | Hamming=4 | equal_candidate",
    
    x = "Analysis sample",
    y = "Prototypes required",
    color = "Coverage target"
  ) +
  theme_paper() +
  theme(
    axis.text.x = element_text(
      angle = 35,
      hjust = 1
    )
  )

save_plot(
  p_sample_comparison,
  file.path(
    fig_greedy_dir,
    "10_greedy_sample_comparison_D8_H4.png"
  ),
  width = 11,
  height = 7
)


# Comparación entre K-means y EFA como generadores de patrones.

method_comparison <- greedy_thresholds %>%
  filter(
    analysis_sample ==
      REFERENCE_SAMPLE,
    
    weighting ==
      REFERENCE_WEIGHTING,
    
    d_det ==
      REFERENCE_D_DET,
    
    d_hamming ==
      REFERENCE_D_HAMMING,
    
    coverage_threshold %in%
      COVERAGE_THRESHOLDS_PLOT
  ) %>%
  select(
    method,
    matrix,
    coverage_threshold,
    first_prototype_reaching_threshold
  )

write_output(
  method_comparison,
  "07_GREEDY_KMEANS_vs_EFA_COMPLETE_D8_H4.csv"
)

p_method_comparison <- ggplot(
  method_comparison,
  aes(
    x = factor(
      coverage_threshold
    ),
    
    y =
      first_prototype_reaching_threshold,
    
    group = method,
    color = method
  )
) +
  geom_line() +
  geom_point(
    size = 2.5
  ) +
  facet_wrap(
    ~ matrix
  ) +
  labs(
    title =
      "Greedy - K-means vs EFA",
    
    subtitle =
      "COMPLETE | D=8 | Hamming=4 | equal_candidate",
    
    x = "Coverage target (%)",
    y = "Prototypes required",
    color = "Generator"
  ) +
  theme_paper()

save_plot(
  p_method_comparison,
  file.path(
    fig_greedy_dir,
    "11_greedy_KMEANS_vs_EFA_COMPLETE.png"
  ),
  width = 11,
  height = 7
)


# Sensibilidad a la forma de ponderar el pool Greedy.

weighting_comparison <- greedy_thresholds %>%
  filter(
    analysis_sample ==
      REFERENCE_SAMPLE,
    
    method ==
      REFERENCE_METHOD,
    
    matrix_name ==
      REFERENCE_MATRIX,
    
    d_det %in%
      D_DET_GRID,
    
    d_hamming %in%
      HAMMING_GRID_PLOT,
    
    coverage_threshold %in%
      COVERAGE_THRESHOLDS_PLOT
  ) %>%
  select(
    weighting,
    d_det,
    d_hamming,
    coverage_threshold,
    first_prototype_reaching_threshold
  ) %>%
  pivot_wider(
    names_from =
      weighting,
    
    values_from =
      first_prototype_reaching_threshold
  ) %>%
  mutate(
    both_not_reached =
      is.na(
        equal_element
      ) &
      is.na(
        equal_candidate
      ),
    
    same_result = case_when(
      both_not_reached ~
        TRUE,
      
      !is.na(
        equal_element
      ) &
        !is.na(
          equal_candidate
        ) ~
        equal_element ==
        equal_candidate,
      
      TRUE ~
        FALSE
    ),
    
    difference_candidate_minus_element =
      equal_candidate -
      equal_element,
    
    abs_difference = abs(
      difference_candidate_minus_element
    )
  )

write_output(
  weighting_comparison,
  "08_GREEDY_weighting_comparison.csv"
)

weighting_robustness_summary <- weighting_comparison %>%
  group_by(
    coverage_threshold
  ) %>%
  summarise(
    n_combinations = n(),
    
    n_same = sum(
      same_result,
      na.rm = TRUE
    ),
    
    pct_same =
      100 *
      mean(
        same_result,
        na.rm = TRUE
      ),
    
    mean_abs_difference = mean(
      abs_difference,
      na.rm = TRUE
    ),
    
    max_abs_difference = max(
      abs_difference,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

write_output(
  weighting_robustness_summary,
  "09_GREEDY_weighting_robustness_summary.csv"
)


# Figura de comparación entre ponderaciones para D=8 y H=4.

weighting_reference_plot_data <- greedy_thresholds %>%
  filter(
    analysis_sample ==
      REFERENCE_SAMPLE,
    
    method ==
      REFERENCE_METHOD,
    
    matrix_name ==
      REFERENCE_MATRIX,
    
    d_det ==
      REFERENCE_D_DET,
    
    d_hamming ==
      REFERENCE_D_HAMMING,
    
    coverage_threshold %in%
      COVERAGE_THRESHOLDS_PLOT
  )

p_weighting <- ggplot(
  weighting_reference_plot_data,
  aes(
    x = factor(
      coverage_threshold
    ),
    
    y =
      first_prototype_reaching_threshold,
    
    fill = weighting
  )
) +
  geom_col(
    position = position_dodge(
      width = 0.8
    ),
    width = 0.7
  ) +
  labs(
    title =
      "Greedy sensitivity to pool weighting",
    
    subtitle =
      "COMPLETE | K-means RAW | D=8 | Hamming=4",
    
    x = "Coverage target (%)",
    y = "Prototypes required",
    fill = "Weighting"
  ) +
  theme_paper()

save_plot(
  p_weighting,
  file.path(
    fig_greedy_dir,
    "12_greedy_weighting_comparison.png"
  ),
  width = 9,
  height = 6
)


# Para cada D, seleccionar el mayor radio Hamming que todavía garantiza
# al menos un 75% de determinantes comunes entre patrón y prototipo.

hamming_reference <- hamming_interpretation %>%
  filter(
    d_det %in%
      D_DET_GRID,
    
    d_hamming %in%
      HAMMING_GRID_PLOT,
    
    min_common_pct >=
      REFERENCE_COMMON_PCT
  ) %>%
  group_by(
    d_det
  ) %>%
  slice_max(
    order_by =
      d_hamming,
    
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  select(
    d_det,
    d_hamming,
    min_common_determinants,
    min_common_pct
  ) %>%
  arrange(
    d_det
  )

write_output(
  hamming_reference,
  "10_GREEDY_hamming_reference_75pct.csv"
)


# Cobertura obtenida utilizando la referencia de >=75% de
# determinantes comunes para cada D.

reference_thresholds <- greedy_thresholds %>%
  filter(
    analysis_sample ==
      REFERENCE_SAMPLE,
    
    weighting ==
      REFERENCE_WEIGHTING,
    
    method ==
      REFERENCE_METHOD,
    
    matrix_name ==
      REFERENCE_MATRIX,
    
    coverage_threshold %in%
      COVERAGE_THRESHOLDS_PLOT
  ) %>%
  inner_join(
    hamming_reference,
    by = c(
      "d_det",
      "d_hamming"
    ),
    suffix = c(
      "",
      "_reference"
    )
  ) %>%
  select(
    d_det,
    d_hamming,
    
    min_common_pct =
      min_common_pct_reference,
    
    coverage_threshold,
    first_prototype_reaching_threshold,
    max_coverage_available
  ) %>%
  pivot_wider(
    names_from =
      coverage_threshold,
    
    values_from =
      first_prototype_reaching_threshold,
    
    names_prefix =
      "prototypes_for_"
  ) %>%
  arrange(
    d_det
  )

write_output(
  reference_thresholds,
  "11_GREEDY_reference75_thresholds_by_D.csv"
)


# Zona candidata con similitud entre 75% y 85%.
#
# Se conserva únicamente como diagnóstico para facilitar la comparación
# entre combinaciones D × Hamming; todavía no selecciona una solución final.

candidate_thresholds <- greedy_reference_thresholds %>%
  filter(
    min_common_pct >=
      SIMILARITY_MIN,
    
    min_common_pct <=
      SIMILARITY_MAX
  ) %>%
  select(
    d_det,
    d_hamming,
    min_common_pct,
    coverage_threshold,
    first_prototype_reaching_threshold
  ) %>%
  pivot_wider(
    names_from =
      coverage_threshold,
    
    values_from =
      first_prototype_reaching_threshold,
    
    names_prefix =
      "prototypes_for_"
  )

candidate_fixed_profiles <- greedy_reference_steps %>%
  filter(
    min_common_pct >=
      SIMILARITY_MIN,
    
    min_common_pct <=
      SIMILARITY_MAX,
    
    prototype %in%
      PROTOTYPES_OF_INTEREST
  ) %>%
  select(
    d_det,
    d_hamming,
    min_common_pct,
    prototype,
    cumulative_covered_pct
  ) %>%
  pivot_wider(
    names_from =
      prototype,
    
    values_from =
      cumulative_covered_pct,
    
    names_prefix =
      "coverage_with_"
  )

greedy_candidate_zone <- candidate_thresholds %>%
  left_join(
    candidate_fixed_profiles,
    by = c(
      "d_det",
      "d_hamming",
      "min_common_pct"
    )
  ) %>%
  arrange(
    min_common_pct,
    d_det
  )

write_output(
  greedy_candidate_zone,
  "12_GREEDY_candidate_similarity_75_85.csv"
)


# Estabilidad de los determinantes dentro de los prototipos provisionales
# de la configuración de referencia.

profile_reference <- greedy_prevalence %>%
  filter(
    analysis_sample ==
      REFERENCE_SAMPLE,
    
    weighting ==
      REFERENCE_WEIGHTING,
    
    method ==
      REFERENCE_METHOD,
    
    matrix_name ==
      REFERENCE_MATRIX,
    
    d_det ==
      REFERENCE_D_DET,
    
    d_hamming ==
      REFERENCE_D_HAMMING,
    
    prototype <= 8
  ) %>%
  mutate(
    determinant_label =
      clean_determinant_label(
        determinant
      )
  )

if (nrow(profile_reference)) {
  determinant_order <- profile_reference %>%
    group_by(
      determinant_label
    ) %>%
    summarise(
      max_prevalence = max(
        pct_active_in_ball,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) %>%
    arrange(
      max_prevalence
    ) %>%
    pull(
      determinant_label
    )
  
  profile_reference <- profile_reference %>%
    mutate(
      determinant_label = factor(
        determinant_label,
        levels = determinant_order
      )
    )
  
  p_profile <- ggplot(
    profile_reference,
    aes(
      x = factor(
        prototype
      ),
      
      y =
        determinant_label,
      
      fill =
        pct_active_in_ball
    )
  ) +
    geom_tile(
      color = "white",
      linewidth = 0.3
    ) +
    geom_text(
      aes(
        label = if_else(
          pct_active_in_ball >= 50,
          
          paste0(
            round(
              pct_active_in_ball
            ),
            "%"
          ),
          
          ""
        )
      ),
      size = 2.5
    ) +
    labs(
      title =
        "Determinant stability within provisional Greedy prototypes",
      
      subtitle =
        "COMPLETE | K-means RAW | D=8 | Hamming=4 | equal_candidate",
      
      x = "Greedy prototype",
      y = "Determinant",
      fill = "% present"
    ) +
    theme_paper(
      base_size = 10
    )
  
  save_plot(
    p_profile,
    file.path(
      fig_profiles_dir,
      "13_profile_stability_COMPLETE_RAW_provisional.png"
    ),
    width = 11,
    height = 11
  )
}


# Resumen en consola

cat("\n09. ANÁLISIS Y GRÁFICOS COMPLETADO\n")

cat("\nK-MEANS - COMPLETE - RAW\n\n")

print(
  kmeans_summary %>%
    filter(
      analysis_sample ==
        REFERENCE_SAMPLE,
      
      matrix_name ==
        REFERENCE_MATRIX
    ) %>%
    select(
      k,
      mean_tot_withinss,
      mean_between_over_total,
      mean_calinski_harabasz,
      mean_silhouette,
      mean_min_cluster_distance
    ),
  n = Inf,
  width = Inf
)

cat("\nEFA - COMPLETE - RAW\n\n")

print(
  efa_summary %>%
    filter(
      analysis_sample ==
        REFERENCE_SAMPLE,
      
      matrix_name ==
        REFERENCE_MATRIX
    ) %>%
    select(
      n_factors,
      n_ok,
      n_error,
      mean_RMSR,
      mean_TLI,
      mean_RMSEA,
      mean_BIC,
      mean_abs_factor_correlation
    ),
  n = Inf,
  width = Inf
)

cat(
  "\nGREEDY - COMPARACIÓN ENTRE MUESTRAS - D=8 / H=4\n\n"
)

print(
  sample_comparison,
  n = Inf,
  width = Inf
)

cat("\nGREEDY - ROBUSTEZ A PONDERACIÓN\n\n")

print(
  weighting_robustness_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nGREEDY - REFERENCIA >=75% DE DETERMINANTES COMUNES\n\n"
)

print(
  reference_thresholds,
  n = Inf,
  width = Inf
)

cat(
  "\nFiguras guardadas en:\n",
  fig_dir,
  "\n",
  sep = ""
)

message(
  "\nResultados guardados en: ",
  out_dir
)