#
# Objetivo
# Resumir y visualizar el Greedy D-pooled generado en 08b, manteniendo
# los diagnósticos generales de K-means y EFA.
#
# El script:
# - analiza las 7 muestras, las 4 matrices y los métodos KMEANS y EFA;
# - resume Greedy por H = 0:10 con D = 8:15 integrado en un único pool;
# - calcula cobertura acumulada P1:P8 y los prototipos necesarios para
#   alcanzar 80%, 85%, 90% y 95% de cobertura;
# - compara K-means frente a EFA y equal_candidate frente a equal_element;
# - compara EUROPE y LATAM y genera tablas y figuras de sensibilidad.
#
# D ya no separa las ejecuciones Greedy. Este script no selecciona todavía
# una matriz, H o número de prototipos final y no contiene aleatoriedad.

suppressPackageStartupMessages({
  library(tidyverse)
})

# Configuración
project_root <- path.expand("~/Desktop/MASTER/recommendation-engine/TFM")
processed_root <- file.path(project_root, "paper1_cluster/data/processed")

kmeans_dir <- file.path(processed_root, "06_kmeans_bootstrap")
efa_dir <- file.path(processed_root, "07_efa_bootstrap")
greedy_dir <- file.path(processed_root, "08b_greedy_kmeans_efa_Dpooled")

out_dir <- file.path(processed_root, "09b_analysis_Dpooled")
fig_dir <- file.path(out_dir, "figures")
fig_kmeans_dir <- file.path(fig_dir, "kmeans")
fig_efa_dir <- file.path(fig_dir, "efa")
fig_greedy_dir <- file.path(fig_dir, "greedy")

walk(
  c(out_dir, fig_dir, fig_kmeans_dir, fig_efa_dir, fig_greedy_dir),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
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

METHODS <- c(
  "KMEANS",
  "EFA"
)

H_GRID <- 0:10
PROTOTYPES_TO_SUMMARISE <- 1:8
COVERAGE_THRESHOLDS <- c(80, 85, 90, 95)

PRIMARY_WEIGHTING <- "equal_candidate"

WEIGHTINGS <- c(
  "equal_candidate",
  "equal_element"
)

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
  
  cumulative_covered_pct[
    valid
  ][
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
  
  min(
    prototype[
      valid
    ]
  )
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
    base_size = 11
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
  "07_greedy_prototype_steps_Dpooled.csv"
)

greedy_thresholds_file <- file.path(
  greedy_dir,
  "06_coverage_threshold_by_H_Dpooled.csv"
)

greedy_prevalence_file <- file.path(
  greedy_dir,
  "09_greedy_ball_determinant_prevalence_Dpooled.csv.gz"
)

required_files <- c(
  kmeans_metrics_file,
  kmeans_sizes_file,
  efa_summary_file,
  greedy_steps_file,
  greedy_thresholds_file,
  greedy_prevalence_file
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
  show_col_types = FALSE,
  progress = FALSE
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
      lag(mean_between_over_total),
    
    withinss_reduction_pct =
      100 *
      (
        lag(mean_tot_withinss) -
          mean_tot_withinss
      ) /
      lag(mean_tot_withinss)
  ) %>%
  ungroup()

cluster_size_summary <- read_csv(
  kmeans_sizes_file,
  show_col_types = FALSE,
  progress = FALSE
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

# Figuras K-means
complete_kmeans <- kmeans_summary %>%
  filter(
    analysis_sample == "COMPLETE"
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
    size = 2.3
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
    size = 2.3
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
    size = 2.3
  ) +
  facet_wrap(
    ~ matrix
  ) +
  scale_x_continuous(
    breaks = 2:8
  ) +
  labs(
    title = "K-means - COMPLETE - Silhouette",
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

# EFA
efa_summary <- read_csv(
  efa_summary_file,
  show_col_types = FALSE,
  progress = FALSE
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
    analysis_sample == "COMPLETE"
  )

p_efa_bic <- ggplot(
  complete_efa,
  aes(
    x = n_factors,
    y = mean_BIC
  )
) +
  geom_line() +
  geom_point(
    size = 2.3
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
    x = "Number of factors",
    y = "Mean BIC"
  ) +
  theme_paper()

save_plot(
  p_efa_bic,
  file.path(
    fig_efa_dir,
    "04_efa_bic_COMPLETE.png"
  ),
  width = 11,
  height = 7
)

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
    group = matrix,
    linetype = matrix
  )
) +
  geom_line() +
  geom_point(
    size = 2
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
    linetype = "Matrix"
  ) +
  theme_paper()

save_plot(
  p_efa_fit,
  file.path(
    fig_efa_dir,
    "05_efa_fit_COMPLETE.png"
  ),
  width = 10,
  height = 10
)

# Greedy D-pooled
greedy_steps <- read_csv(
  greedy_steps_file,
  show_col_types = FALSE,
  progress = FALSE
) %>%
  mutate(
    analysis_sample = as.character(
      analysis_sample
    ),
    method = as.character(
      method
    ),
    matrix_name = as.character(
      matrix_name
    ),
    d_hamming = as.integer(
      d_hamming
    ),
    prototype = as.integer(
      prototype
    ),
    matrix = matrix_label(
      matrix_name
    )
  ) %>%
  filter(
    analysis_sample %in% ANALYSIS_SAMPLES,
    method %in% METHODS,
    matrix_name %in% MATRICES_TO_RUN,
    d_hamming %in% H_GRID
  )

required_greedy_cols <- c(
  "analysis_sample",
  "weighting",
  "method",
  "matrix_name",
  "d_hamming",
  "prototype",
  "incremental_covered_pct",
  "cumulative_covered_pct",
  "center_signature_size",
  "center_active_determinants"
)

missing_greedy_cols <- setdiff(
  required_greedy_cols,
  names(greedy_steps)
)

if (length(missing_greedy_cols)) {
  stop(
    "Faltan columnas Greedy 08b: ",
    paste(
      missing_greedy_cols,
      collapse = ", "
    )
  )
}

greedy_thresholds <- read_csv(
  greedy_thresholds_file,
  show_col_types = FALSE,
  progress = FALSE
) %>%
  mutate(
    analysis_sample = as.character(
      analysis_sample
    ),
    method = as.character(
      method
    ),
    matrix_name = as.character(
      matrix_name
    ),
    d_hamming = as.integer(
      d_hamming
    ),
    coverage_threshold = as.numeric(
      coverage_threshold
    ),
    matrix = matrix_label(
      matrix_name
    )
  ) %>%
  filter(
    analysis_sample %in% ANALYSIS_SAMPLES,
    method %in% METHODS,
    matrix_name %in% MATRICES_TO_RUN,
    d_hamming %in% H_GRID,
    coverage_threshold %in% COVERAGE_THRESHOLDS
  )

greedy_prevalence <- read_csv(
  greedy_prevalence_file,
  show_col_types = FALSE,
  progress = FALSE
)

# D ya no debe aparecer como dimensión del Greedy
if (
  "d_det" %in%
  names(greedy_steps)
) {
  stop(
    paste0(
      "El fichero Greedy contiene d_det como dimensión. ",
      "Parece que se está leyendo el 08 antiguo y no el 08b D-pooled."
    )
  )
}

# Resumen principal H × P
greedy_primary <- greedy_steps %>%
  filter(
    weighting == PRIMARY_WEIGHTING
  )

greedy_HP_summary <- greedy_primary %>%
  group_by(
    analysis_sample,
    method,
    matrix,
    matrix_name,
    d_hamming
  ) %>%
  summarise(
    coverage_P1 = coverage_at_n(
      prototype,
      cumulative_covered_pct,
      1
    ),
    
    coverage_P2 = coverage_at_n(
      prototype,
      cumulative_covered_pct,
      2
    ),
    
    coverage_P3 = coverage_at_n(
      prototype,
      cumulative_covered_pct,
      3
    ),
    
    coverage_P4 = coverage_at_n(
      prototype,
      cumulative_covered_pct,
      4
    ),
    
    coverage_P5 = coverage_at_n(
      prototype,
      cumulative_covered_pct,
      5
    ),
    
    coverage_P6 = coverage_at_n(
      prototype,
      cumulative_covered_pct,
      6
    ),
    
    coverage_P7 = coverage_at_n(
      prototype,
      cumulative_covered_pct,
      7
    ),
    
    coverage_P8 = coverage_at_n(
      prototype,
      cumulative_covered_pct,
      8
    ),
    
    P_for_80 = first_prototype_reaching(
      prototype,
      cumulative_covered_pct,
      80
    ),
    
    P_for_85 = first_prototype_reaching(
      prototype,
      cumulative_covered_pct,
      85
    ),
    
    P_for_90 = first_prototype_reaching(
      prototype,
      cumulative_covered_pct,
      90
    ),
    
    P_for_95 = first_prototype_reaching(
      prototype,
      cumulative_covered_pct,
      95
    ),
    
    max_coverage_P12 = max(
      cumulative_covered_pct,
      na.rm = TRUE
    ),
    
    max_prototype_available = max(
      prototype,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  arrange(
    factor(
      analysis_sample,
      levels = ANALYSIS_SAMPLES
    ),
    factor(
      method,
      levels = METHODS
    ),
    factor(
      matrix,
      levels = c(
        "RAW",
        "POS",
        "EXT",
        "Z_ABS"
      )
    ),
    d_hamming
  )

write_output(
  greedy_HP_summary,
  "05_GREEDY_H_P_summary_all_samples.csv"
)

# Tabla de umbrales
greedy_thresholds_primary <- greedy_thresholds %>%
  filter(
    weighting == PRIMARY_WEIGHTING
  ) %>%
  arrange(
    factor(
      analysis_sample,
      levels = ANALYSIS_SAMPLES
    ),
    factor(
      method,
      levels = METHODS
    ),
    factor(
      matrix,
      levels = c(
        "RAW",
        "POS",
        "EXT",
        "Z_ABS"
      )
    ),
    d_hamming,
    coverage_threshold
  )

write_output(
  greedy_thresholds_primary,
  "06_GREEDY_thresholds_all_samples.csv"
)

# Comparación K-means frente a EFA en cobertura
method_comparison <- greedy_primary %>%
  filter(
    prototype %in% PROTOTYPES_TO_SUMMARISE
  ) %>%
  select(
    analysis_sample,
    matrix,
    matrix_name,
    d_hamming,
    prototype,
    method,
    cumulative_covered_pct
  ) %>%
  pivot_wider(
    names_from = method,
    values_from = cumulative_covered_pct
  ) %>%
  mutate(
    delta_KMEANS_minus_EFA =
      KMEANS -
      EFA
  ) %>%
  arrange(
    factor(
      analysis_sample,
      levels = ANALYSIS_SAMPLES
    ),
    factor(
      matrix,
      levels = c(
        "RAW",
        "POS",
        "EXT",
        "Z_ABS"
      )
    ),
    d_hamming,
    prototype
  )

write_output(
  method_comparison,
  "07_GREEDY_KMEANS_vs_EFA_coverage.csv"
)

# Comparación entre ponderaciones
weighting_thresholds <- greedy_thresholds %>%
  select(
    analysis_sample,
    method,
    matrix,
    matrix_name,
    d_hamming,
    coverage_threshold,
    weighting,
    first_prototype_reaching_threshold
  ) %>%
  pivot_wider(
    names_from = weighting,
    values_from = first_prototype_reaching_threshold
  ) %>%
  mutate(
    both_not_reached =
      is.na(equal_candidate) &
      is.na(equal_element),
    
    same_result = case_when(
      both_not_reached ~ TRUE,
      
      !is.na(equal_candidate) &
        !is.na(equal_element) ~
        equal_candidate ==
        equal_element,
      
      TRUE ~ FALSE
    ),
    
    difference_candidate_minus_element =
      equal_candidate -
      equal_element,
    
    abs_difference = abs(
      difference_candidate_minus_element
    )
  )

write_output(
  weighting_thresholds,
  "08_GREEDY_weighting_comparison.csv"
)

weighting_robustness <- weighting_thresholds %>%
  group_by(
    method,
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
    
    n_both_not_reached = sum(
      both_not_reached,
      na.rm = TRUE
    ),
    
    mean_abs_difference = if (
      any(
        is.finite(
          abs_difference
        )
      )
    ) {
      mean(
        abs_difference,
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    
    max_abs_difference = if (
      any(
        is.finite(
          abs_difference
        )
      )
    ) {
      max(
        abs_difference,
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    
    .groups = "drop"
  )

write_output(
  weighting_robustness,
  "09_GREEDY_weighting_robustness_summary.csv"
)

# Comparación EUROPE vs LATAM
region_comparison <- greedy_HP_summary %>%
  filter(
    analysis_sample %in%
      c(
        "EUROPE",
        "LATAM"
      )
  ) %>%
  select(
    analysis_sample,
    method,
    matrix,
    d_hamming,
    coverage_P1:coverage_P8,
    P_for_80:P_for_95
  ) %>%
  pivot_wider(
    names_from = analysis_sample,
    values_from = c(
      coverage_P1,
      coverage_P2,
      coverage_P3,
      coverage_P4,
      coverage_P5,
      coverage_P6,
      coverage_P7,
      coverage_P8,
      P_for_80,
      P_for_85,
      P_for_90,
      P_for_95
    )
  )

write_output(
  region_comparison,
  "10_GREEDY_EUROPE_vs_LATAM.csv"
)

# Tabla COMPLETE
complete_greedy_summary <- greedy_HP_summary %>%
  filter(
    analysis_sample == "COMPLETE"
  )

write_output(
  complete_greedy_summary,
  "11_GREEDY_COMPLETE_H_P_summary.csv"
)

# Gráficos H × P
plot_data <- greedy_primary %>%
  filter(
    prototype %in% PROTOTYPES_TO_SUMMARISE
  ) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = ANALYSIS_SAMPLES
    ),
    
    method = factor(
      method,
      levels = METHODS
    ),
    
    matrix = factor(
      matrix,
      levels = c(
        "RAW",
        "POS",
        "EXT",
        "Z_ABS"
      )
    ),
    
    prototype_label = paste0(
      "P",
      prototype
    )
  )

# COMPLETE K-means
p_complete_kmeans <- plot_data %>%
  filter(
    analysis_sample == "COMPLETE",
    method == "KMEANS"
  ) %>%
  ggplot(
    aes(
      x = d_hamming,
      y = cumulative_covered_pct,
      color = prototype_label,
      group = prototype_label
    )
  ) +
  geom_hline(
    yintercept = 90,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 1.7
  ) +
  facet_wrap(
    ~ matrix,
    ncol = 2
  ) +
  scale_x_continuous(
    breaks = H_GRID
  ) +
  coord_cartesian(
    ylim = c(
      0,
      100
    )
  ) +
  labs(
    title = "Greedy D-pooled - COMPLETE - KMEANS",
    subtitle = "D=8:15 pooled | equal_candidate",
    x = "Hamming radius (H)",
    y = "Cumulative coverage (%)",
    color = "Number of prototypes"
  ) +
  theme_paper()

save_plot(
  p_complete_kmeans,
  file.path(
    fig_greedy_dir,
    "06_COMPLETE_KMEANS_H_by_P.png"
  ),
  width = 11,
  height = 8
)

# COMPLETE EFA
p_complete_efa <- plot_data %>%
  filter(
    analysis_sample == "COMPLETE",
    method == "EFA"
  ) %>%
  ggplot(
    aes(
      x = d_hamming,
      y = cumulative_covered_pct,
      color = prototype_label,
      group = prototype_label
    )
  ) +
  geom_hline(
    yintercept = 90,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 1.7
  ) +
  facet_wrap(
    ~ matrix,
    ncol = 2
  ) +
  scale_x_continuous(
    breaks = H_GRID
  ) +
  coord_cartesian(
    ylim = c(
      0,
      100
    )
  ) +
  labs(
    title = "Greedy D-pooled - COMPLETE - EFA",
    subtitle = "D=8:15 pooled | equal_candidate",
    x = "Hamming radius (H)",
    y = "Cumulative coverage (%)",
    color = "Number of prototypes"
  ) +
  theme_paper()

save_plot(
  p_complete_efa,
  file.path(
    fig_greedy_dir,
    "07_COMPLETE_EFA_H_by_P.png"
  ),
  width = 11,
  height = 8
)

# Todas las muestras - K-means
p_all_kmeans <- plot_data %>%
  filter(
    method == "KMEANS"
  ) %>%
  ggplot(
    aes(
      x = d_hamming,
      y = cumulative_covered_pct,
      color = prototype_label,
      group = prototype_label
    )
  ) +
  geom_hline(
    yintercept = 90,
    linetype = "dashed",
    linewidth = 0.25
  ) +
  geom_line(
    linewidth = 0.55
  ) +
  facet_grid(
    analysis_sample ~ matrix
  ) +
  scale_x_continuous(
    breaks = seq(
      0,
      10,
      by = 2
    )
  ) +
  coord_cartesian(
    ylim = c(
      0,
      100
    )
  ) +
  labs(
    title = "Greedy D-pooled - KMEANS - all samples",
    subtitle = "D=8:15 pooled | equal_candidate",
    x = "Hamming radius (H)",
    y = "Cumulative coverage (%)",
    color = "Prototypes"
  ) +
  theme_paper(
    base_size = 8
  )

save_plot(
  p_all_kmeans,
  file.path(
    fig_greedy_dir,
    "08_KMEANS_H_by_P_all_samples.png"
  ),
  width = 15,
  height = 20
)

# Todas las muestras - EFA
p_all_efa <- plot_data %>%
  filter(
    method == "EFA"
  ) %>%
  ggplot(
    aes(
      x = d_hamming,
      y = cumulative_covered_pct,
      color = prototype_label,
      group = prototype_label
    )
  ) +
  geom_hline(
    yintercept = 90,
    linetype = "dashed",
    linewidth = 0.25
  ) +
  geom_line(
    linewidth = 0.55
  ) +
  facet_grid(
    analysis_sample ~ matrix
  ) +
  scale_x_continuous(
    breaks = seq(
      0,
      10,
      by = 2
    )
  ) +
  coord_cartesian(
    ylim = c(
      0,
      100
    )
  ) +
  labs(
    title = "Greedy D-pooled - EFA - all samples",
    subtitle = "D=8:15 pooled | equal_candidate",
    x = "Hamming radius (H)",
    y = "Cumulative coverage (%)",
    color = "Prototypes"
  ) +
  theme_paper(
    base_size = 8
  )

save_plot(
  p_all_efa,
  file.path(
    fig_greedy_dir,
    "09_EFA_H_by_P_all_samples.png"
  ),
  width = 15,
  height = 20
)

# Heatmap: número de prototipos necesarios para 90%
threshold_90 <- greedy_thresholds_primary %>%
  filter(
    coverage_threshold == 90
  ) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = ANALYSIS_SAMPLES
    ),
    
    method = factor(
      method,
      levels = METHODS
    ),
    
    matrix = factor(
      matrix,
      levels = c(
        "RAW",
        "POS",
        "EXT",
        "Z_ABS"
      )
    ),
    
    prototype_label = if_else(
      is.na(
        first_prototype_reaching_threshold
      ),
      ">12",
      as.character(
        first_prototype_reaching_threshold
      )
    )
  )

p_threshold_90 <- ggplot(
  threshold_90,
  aes(
    x = factor(
      d_hamming
    ),
    y = analysis_sample,
    fill = first_prototype_reaching_threshold
  )
) +
  geom_tile(
    linewidth = 0.4
  ) +
  geom_text(
    aes(
      label = prototype_label
    ),
    size = 2.7
  ) +
  facet_grid(
    method ~ matrix
  ) +
  labs(
    title = "Greedy D-pooled - prototypes needed for 90% coverage",
    subtitle = "D=8:15 pooled | equal_candidate | >12 = threshold not reached with P1:P12",
    x = "Hamming radius (H)",
    y = "Analysis sample",
    fill = "Prototypes"
  ) +
  theme_paper(
    base_size = 9
  ) +
  theme(
    panel.grid = element_blank()
  )

save_plot(
  p_threshold_90,
  file.path(
    fig_greedy_dir,
    "10_prototypes_needed_for_90pct.png"
  ),
  width = 15,
  height = 10
)

# Heatmap COMPLETE para 80/85/90/95%
complete_thresholds <- greedy_thresholds_primary %>%
  filter(
    analysis_sample == "COMPLETE"
  ) %>%
  mutate(
    method = factor(
      method,
      levels = METHODS
    ),
    
    matrix = factor(
      matrix,
      levels = c(
        "RAW",
        "POS",
        "EXT",
        "Z_ABS"
      )
    ),
    
    threshold_label = paste0(
      coverage_threshold,
      "%"
    ),
    
    prototype_label = if_else(
      is.na(
        first_prototype_reaching_threshold
      ),
      ">12",
      as.character(
        first_prototype_reaching_threshold
      )
    )
  )

p_complete_thresholds <- ggplot(
  complete_thresholds,
  aes(
    x = factor(
      d_hamming
    ),
    y = matrix,
    fill = first_prototype_reaching_threshold
  )
) +
  geom_tile(
    linewidth = 0.4
  ) +
  geom_text(
    aes(
      label = prototype_label
    ),
    size = 2.8
  ) +
  facet_grid(
    method ~ threshold_label
  ) +
  labs(
    title = "COMPLETE - prototypes needed by H and coverage target",
    subtitle = "D=8:15 pooled | equal_candidate",
    x = "Hamming radius (H)",
    y = "Matrix",
    fill = "Prototypes"
  ) +
  theme_paper(
    base_size = 9
  ) +
  theme(
    panel.grid = element_blank()
  )

save_plot(
  p_complete_thresholds,
  file.path(
    fig_greedy_dir,
    "11_COMPLETE_prototypes_needed_80_85_90_95.png"
  ),
  width = 16,
  height = 7
)

# Parámetros
parameters <- tibble(
  parameter = c(
    "analysis_samples",
    "matrices",
    "methods",
    "D_strategy",
    "H_grid",
    "prototypes_summarised",
    "coverage_thresholds",
    "primary_weighting",
    "greedy_steps_file",
    "greedy_thresholds_file",
    "random_seed"
  ),
  
  value = c(
    paste(
      ANALYSIS_SAMPLES,
      collapse = ", "
    ),
    
    paste(
      MATRICES_TO_RUN,
      collapse = ", "
    ),
    
    paste(
      METHODS,
      collapse = ", "
    ),
    
    "D=8:15 pooled in one Greedy pool",
    
    paste(
      H_GRID,
      collapse = ", "
    ),
    
    paste(
      PROTOTYPES_TO_SUMMARISE,
      collapse = ", "
    ),
    
    paste(
      COVERAGE_THRESHOLDS,
      collapse = ", "
    ),
    
    PRIMARY_WEIGHTING,
    
    greedy_steps_file,
    
    greedy_thresholds_file,
    
    "NONE"
  )
)

write_output(
  parameters,
  "12_parameters.csv"
)

# Comprobaciones finales
expected_summary_grid <- crossing(
  analysis_sample = ANALYSIS_SAMPLES,
  method = METHODS,
  matrix_name = MATRICES_TO_RUN,
  d_hamming = H_GRID
)

available_summary_grid <- greedy_HP_summary %>%
  distinct(
    analysis_sample,
    method,
    matrix_name,
    d_hamming
  )

missing_summary_grid <- expected_summary_grid %>%
  anti_join(
    available_summary_grid,
    by = c(
      "analysis_sample",
      "method",
      "matrix_name",
      "d_hamming"
    )
  )

if (
  nrow(
    missing_summary_grid
  ) > 0
) {
  print(
    missing_summary_grid,
    n = Inf
  )
  
  stop(
    "Faltan combinaciones muestra × método × matriz × H."
  )
}

expected_n_rows <-
  length(ANALYSIS_SAMPLES) *
  length(METHODS) *
  length(MATRICES_TO_RUN) *
  length(H_GRID)

if (
  nrow(greedy_HP_summary) !=
  expected_n_rows
) {
  stop(
    paste0(
      "Esperaba ",
      expected_n_rows,
      " filas en greedy_HP_summary y encuentro ",
      nrow(greedy_HP_summary),
      "."
    )
  )
}

# Resumen en consola
cat(
  "\n09b. ANÁLISIS D-POOLED COMPLETADO\n"
)

cat(
  "\nEstrategia Greedy:\n",
  "D=8:15 integradas en un único pool.\n",
  "H=0:10.\n",
  "P resumidos=P1:P8.\n",
  sep = ""
)

cat(
  "\nNúmero de combinaciones muestra × método × matriz × H: ",
  nrow(greedy_HP_summary),
  "\n",
  sep = ""
)

cat(
  "\nCOMPLETE | KMEANS | P NECESARIOS PARA 90%\n\n"
)

print(
  greedy_HP_summary %>%
    filter(
      analysis_sample == "COMPLETE",
      method == "KMEANS"
    ) %>%
    select(
      matrix,
      d_hamming,
      P_for_90,
      coverage_P4,
      coverage_P5,
      coverage_P6,
      coverage_P7,
      coverage_P8,
      max_coverage_P12
    ),
  n = Inf,
  width = Inf
)

cat(
  "\nCOMPLETE | EFA | P NECESARIOS PARA 90%\n\n"
)

print(
  greedy_HP_summary %>%
    filter(
      analysis_sample == "COMPLETE",
      method == "EFA"
    ) %>%
    select(
      matrix,
      d_hamming,
      P_for_90,
      coverage_P4,
      coverage_P5,
      coverage_P6,
      coverage_P7,
      coverage_P8,
      max_coverage_P12
    ),
  n = Inf,
  width = Inf
)

cat(
  "\nROBUSTEZ A LA PONDERACIÓN\n\n"
)

print(
  weighting_robustness,
  n = Inf,
  width = Inf
)

cat(
  "\nOUTPUTS PRINCIPALES\n",
  "05_GREEDY_H_P_summary_all_samples.csv\n",
  "06_GREEDY_thresholds_all_samples.csv\n",
  "07_GREEDY_KMEANS_vs_EFA_coverage.csv\n",
  "08_GREEDY_weighting_comparison.csv\n",
  "09_GREEDY_weighting_robustness_summary.csv\n",
  "10_GREEDY_EUROPE_vs_LATAM.csv\n",
  "11_GREEDY_COMPLETE_H_P_summary.csv\n",
  "12_parameters.csv\n",
  sep = ""
)

cat(
  "\nFiguras Greedy en:\n",
  fig_greedy_dir,
  "\n",
  sep = ""
)

cat(
  "\nResultados guardados en:\n",
  out_dir,
  "\n",
  sep = ""
)

message(
  "\nListo. 09b D-pooled completado."
)