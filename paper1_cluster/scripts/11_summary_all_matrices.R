# 
# Objetivo
# Integrar y resumir los resultados de 08 Greedy y 10 comparación
# K-means / EFA / expertos para las 7 muestras y las 4 matrices.
#
# El script:
# - resume la sensibilidad Greedy para D = 8:15 y un Hamming de referencia
#   que mantiene aproximadamente >=75% de determinantes comunes;
# - calcula cobertura P1-P8 y prototipos necesarios para 80%, 85% y 90%;
# - compara Greedy K-means ↔ EFA, Greedy K-means ↔ expertos y EFA ↔ expertos.
#
# La configuración comparativa D=8, H=4, equal_candidate, 6 prototipos y
# 6 factores EFA es provisional. El script no selecciona automáticamente
# una matriz, K, F, D o H finales y mantiene separadas las métricas:
# KM ↔ EFA usa overlap_left_pct; las comparaciones con expertos usan Jaccard.

suppressPackageStartupMessages({
  library(tidyverse)
})

# Dependencias
if (!requireNamespace("clue", quietly = TRUE)) {
  stop(
    "\nNecesitas instalar el paquete 'clue'.\n\n",
    "Ejecuta una vez:\n\n",
    "install.packages(\"clue\")\n\n",
    "y vuelve a ejecutar este script."
  )
}

# Configuración
processed_root <- file.path("paper1_cluster/data/processed")

greedy_dir <- file.path(
  processed_root,
  "08_greedy_kmeans_efa"
)

greedy_steps_file <- file.path(
  greedy_dir,
  "03_greedy_prototype_steps_pooled.csv"
)

comparison_root <- file.path(
  processed_root,
  "10_compare_kmeans_efa_experts"
)

comparison_summary_file <- file.path(
  comparison_root,
  "00_comparison_summary_all_samples_matrices.csv"
)

out_dir <- file.path(
  processed_root,
  "11_summary_all_matrices"
)

fig_dir <- file.path(
  out_dir,
  "figures"
)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  fig_dir,
  recursive = TRUE,
  showWarnings = FALSE
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

MATRICES <- c(
  "RAW",
  "POS",
  "EXT",
  "Z_ABS"
)

MATRIX_NAMES <- c(
  "matrix_32_raw_0_1",
  "matrix_32_pos_0_1",
  "matrix_32_ext_0_1",
  "matrix_32_z_abs"
)

D_DET_GRID <- 8:15

HAMMING_GRID <- c(
  0,
  2,
  4,
  6,
  8,
  10
)

REFERENCE_COMMON_PCT <- 75
REFERENCE_WEIGHTING <- "equal_candidate"

REFERENCE_D <- 8L
REFERENCE_H <- 4L
REFERENCE_N_PROTOTYPES <- 6L
REFERENCE_N_EFA_FACTORS <- 6L

MATCH_THRESHOLD <- 50

# Funciones auxiliares
check_file <- function(path) {
  if (!file.exists(path)) {
    stop(
      "\nNo encuentro el fichero:\n",
      path,
      "\n"
    )
  }
}

read_csv_safe <- function(path) {
  read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE
  )
}

matrix_short_from_name <- function(x) {
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
  value <- cumulative_covered_pct[
    prototype == n_target
  ]
  
  if (length(value) == 0) {
    return(
      NA_real_
    )
  }
  
  value[1]
}

first_prototype_reaching <- function(
    prototype,
    cumulative_covered_pct,
    threshold
) {
  valid <-
    !is.na(
      cumulative_covered_pct
    ) &
    cumulative_covered_pct >= threshold
  
  if (!any(valid)) {
    return(
      NA_integer_
    )
  }
  
  min(
    prototype[
      valid
    ]
  )
}

optimal_matching <- function(
    pairwise,
    left_ids,
    right_ids,
    metric_col,
    analysis_sample_current,
    matrix_current,
    comparison_name
) {
  if (
    length(left_ids) >
    length(right_ids)
  ) {
    stop(
      "Matching inválido en ",
      analysis_sample_current,
      " / ",
      matrix_current,
      " / ",
      comparison_name,
      ": nº filas > nº columnas."
    )
  }
  
  similarity_matrix <- matrix(
    0,
    nrow = length(left_ids),
    ncol = length(right_ids),
    dimnames = list(
      left_ids,
      right_ids
    )
  )
  
  for (i in seq_along(left_ids)) {
    for (j in seq_along(right_ids)) {
      value <- pairwise %>%
        filter(
          left_id == left_ids[i],
          right_id == right_ids[j]
        ) %>%
        pull(
          all_of(metric_col)
        )
      
      if (
        length(value) > 0 &&
        is.finite(value[1])
      ) {
        similarity_matrix[i, j] <- value[1]
      }
    }
  }
  
  assignment <- clue::solve_LSAP(
    similarity_matrix,
    maximum = TRUE
  )
  
  assignment_int <- as.integer(
    assignment
  )
  
  matching_base <- tibble(
    analysis_sample = analysis_sample_current,
    matrix = matrix_current,
    comparison = comparison_name,
    metric = metric_col,
    left_id = left_ids,
    right_id = right_ids[
      assignment_int
    ],
    matching_similarity_pct = similarity_matrix[
      cbind(
        seq_along(left_ids),
        assignment_int
      )
    ]
  )
  
  matching_base %>%
    left_join(
      pairwise %>%
        select(
          left_id,
          right_id,
          n_left,
          n_right,
          n_common,
          overlap_left_pct,
          overlap_right_pct,
          jaccard_pct,
          common_determinants
        ),
      by = c(
        "left_id",
        "right_id"
      )
    ) %>%
    mutate(
      greater_than_50 =
        matching_similarity_pct >
        MATCH_THRESHOLD,
      
      greater_or_equal_50 =
        matching_similarity_pct >=
        MATCH_THRESHOLD
    ) %>%
    arrange(
      desc(
        matching_similarity_pct
      )
    )
}

save_plot <- function(
    p,
    filename,
    width = 10,
    height = 7
) {
  ggsave(
    filename = filename,
    plot = p,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

# Comprobar inputs
check_file(
  greedy_steps_file
)

check_file(
  comparison_summary_file
)

# Leer Greedy
greedy_steps <- read_csv_safe(
  greedy_steps_file
)

required_greedy_columns <- c(
  "analysis_sample",
  "weighting",
  "method",
  "matrix_name",
  "d_det",
  "d_hamming",
  "prototype",
  "cumulative_covered_pct"
)

missing_greedy_columns <- setdiff(
  required_greedy_columns,
  names(greedy_steps)
)

if (
  length(
    missing_greedy_columns
  ) > 0
) {
  stop(
    "\nFaltan columnas en Greedy:\n",
    paste(
      missing_greedy_columns,
      collapse = "\n"
    )
  )
}

greedy_steps <- greedy_steps %>%
  mutate(
    d_det = as.integer(
      d_det
    ),
    d_hamming = as.integer(
      d_hamming
    ),
    prototype = as.integer(
      prototype
    )
  )

# Hamming de referencia para cada D
hamming_reference_grid <- crossing(
  d_det = D_DET_GRID,
  d_hamming = HAMMING_GRID
) %>%
  mutate(
    min_common_determinants =
      d_det -
      d_hamming / 2,
    
    min_common_pct =
      100 *
      min_common_determinants /
      d_det
  ) %>%
  filter(
    min_common_pct >=
      REFERENCE_COMMON_PCT
  )

reference_hamming_by_d <- hamming_reference_grid %>%
  group_by(
    d_det
  ) %>%
  arrange(
    desc(
      d_hamming
    ),
    .by_group = TRUE
  ) %>%
  slice_head(
    n = 1
  ) %>%
  ungroup() %>%
  arrange(
    d_det
  )

write_csv(
  reference_hamming_by_d,
  file.path(
    out_dir,
    "01_reference_hamming_by_D.csv"
  )
)

# Sensibilidad Greedy por muestra × matriz × D
greedy_kmeans_reference <- greedy_steps %>%
  filter(
    analysis_sample %in% ANALYSIS_SAMPLES,
    weighting == REFERENCE_WEIGHTING,
    method == "KMEANS",
    matrix_name %in% MATRIX_NAMES,
    d_det %in% D_DET_GRID
  ) %>%
  select(
    -any_of(
      c(
        "min_common_determinants",
        "min_common_pct"
      )
    )
  ) %>%
  inner_join(
    reference_hamming_by_d %>%
      select(
        d_det,
        d_hamming,
        min_common_determinants,
        min_common_pct
      ),
    by = c(
      "d_det",
      "d_hamming"
    )
  ) %>%
  mutate(
    matrix = matrix_short_from_name(
      matrix_name
    )
  ) %>%
  filter(
    matrix %in% MATRICES
  )

greedy_D_summary <- greedy_kmeans_reference %>%
  group_by(
    analysis_sample,
    matrix,
    matrix_name,
    d_det,
    d_hamming,
    min_common_determinants,
    min_common_pct
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
    prototypes_for_80 = first_prototype_reaching(
      prototype,
      cumulative_covered_pct,
      80
    ),
    prototypes_for_85 = first_prototype_reaching(
      prototype,
      cumulative_covered_pct,
      85
    ),
    prototypes_for_90 = first_prototype_reaching(
      prototype,
      cumulative_covered_pct,
      90
    ),
    max_coverage_available = max(
      cumulative_covered_pct,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = ANALYSIS_SAMPLES
    ),
    matrix = factor(
      matrix,
      levels = MATRICES
    )
  ) %>%
  arrange(
    analysis_sample,
    matrix,
    d_det
  ) %>%
  mutate(
    analysis_sample = as.character(
      analysis_sample
    ),
    matrix = as.character(
      matrix
    )
  )

write_csv(
  greedy_D_summary,
  file.path(
    out_dir,
    "02_greedy_D8_D15_summary_all_samples.csv"
  )
)

greedy_curve_P1_P8 <- greedy_kmeans_reference %>%
  filter(
    prototype %in% 1:8
  ) %>%
  select(
    analysis_sample,
    matrix,
    matrix_name,
    d_det,
    d_hamming,
    min_common_pct,
    prototype,
    cumulative_covered_pct
  ) %>%
  mutate(
    prototype_label = paste0(
      "P",
      prototype
    )
  ) %>%
  arrange(
    factor(
      analysis_sample,
      levels = ANALYSIS_SAMPLES
    ),
    factor(
      matrix,
      levels = MATRICES
    ),
    d_det,
    prototype
  )

write_csv(
  greedy_curve_P1_P8,
  file.path(
    out_dir,
    "03_greedy_curve_P1_P8_all_samples.csv"
  )
)

# Leer resumen generado por 10
comparison_summary_10 <- read_csv_safe(
  comparison_summary_file
)

required_summary_columns <- c(
  "analysis_sample",
  "matrix_name",
  "matrix",
  "n_valid_efa_bootstraps",
  "mean_efa_alignment_similarity",
  "mean_kmeans_efa_similarity",
  "coverage_after_6_prototypes"
)

missing_summary_columns <- setdiff(
  required_summary_columns,
  names(comparison_summary_10)
)

if (
  length(
    missing_summary_columns
  ) > 0
) {
  stop(
    "\nFaltan columnas en el resumen del 10:\n",
    paste(
      missing_summary_columns,
      collapse = "\n"
    )
  )
}

expected_grid <- crossing(
  analysis_sample = ANALYSIS_SAMPLES,
  matrix = MATRICES
)

summary_grid <- comparison_summary_10 %>%
  distinct(
    analysis_sample,
    matrix
  )

missing_summary_grid <- expected_grid %>%
  anti_join(
    summary_grid,
    by = c(
      "analysis_sample",
      "matrix"
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
    "El script 10 no contiene las 28 combinaciones esperadas."
  )
}

# Comparaciones triangulares
all_matchings_list <- list()
matching_counter <- 0L

for (sample_current in ANALYSIS_SAMPLES) {
  for (matrix_current in MATRICES) {
    cat(
      "\nMUESTRA: ",
      sample_current,
      " | MATRIZ: ",
      matrix_current,
      "\n",
      sep = ""
    )
    
    matrix_dir <- file.path(
      comparison_root,
      sample_current,
      matrix_current
    )
    
    km_efa_file <- file.path(
      matrix_dir,
      "06_kmeans_vs_efa_pairwise.csv"
    )
    
    km_expert_file <- file.path(
      matrix_dir,
      "10_kmeans_vs_experts_pairwise.csv"
    )
    
    efa_expert_file <- file.path(
      matrix_dir,
      "14_efa_vs_experts_pairwise.csv"
    )
    
    check_file(
      km_efa_file
    )
    
    check_file(
      km_expert_file
    )
    
    check_file(
      efa_expert_file
    )
    
    km_efa_pairwise <- read_csv_safe(
      km_efa_file
    )
    
    km_expert_pairwise <- read_csv_safe(
      km_expert_file
    )
    
    efa_expert_pairwise <- read_csv_safe(
      efa_expert_file
    )
    
    km_efa_matching <- optimal_matching(
      pairwise = km_efa_pairwise,
      left_ids = sort(
        unique(
          km_efa_pairwise$left_id
        )
      ),
      right_ids = sort(
        unique(
          km_efa_pairwise$right_id
        )
      ),
      metric_col = "overlap_left_pct",
      analysis_sample_current = sample_current,
      matrix_current = matrix_current,
      comparison_name = "KM_vs_EFA"
    )
    
    km_expert_matching <- optimal_matching(
      pairwise = km_expert_pairwise,
      left_ids = sort(
        unique(
          km_expert_pairwise$left_id
        )
      ),
      right_ids = sort(
        unique(
          km_expert_pairwise$right_id
        )
      ),
      metric_col = "jaccard_pct",
      analysis_sample_current = sample_current,
      matrix_current = matrix_current,
      comparison_name = "KM_vs_EXPERTS"
    )
    
    efa_expert_matching <- optimal_matching(
      pairwise = efa_expert_pairwise,
      left_ids = sort(
        unique(
          efa_expert_pairwise$left_id
        )
      ),
      right_ids = sort(
        unique(
          efa_expert_pairwise$right_id
        )
      ),
      metric_col = "jaccard_pct",
      analysis_sample_current = sample_current,
      matrix_current = matrix_current,
      comparison_name = "EFA_vs_EXPERTS"
    )
    
    matching_counter <- matching_counter + 1L
    
    all_matchings_list[[matching_counter]] <- bind_rows(
      km_efa_matching,
      km_expert_matching,
      efa_expert_matching
    )
  }
}

all_optimal_matches <- bind_rows(
  all_matchings_list
) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = ANALYSIS_SAMPLES
    ),
    matrix = factor(
      matrix,
      levels = MATRICES
    ),
    comparison = factor(
      comparison,
      levels = c(
        "KM_vs_EFA",
        "KM_vs_EXPERTS",
        "EFA_vs_EXPERTS"
      )
    )
  ) %>%
  arrange(
    analysis_sample,
    matrix,
    comparison,
    desc(
      matching_similarity_pct
    )
  ) %>%
  mutate(
    analysis_sample = as.character(
      analysis_sample
    ),
    matrix = as.character(
      matrix
    ),
    comparison = as.character(
      comparison
    )
  )

write_csv(
  all_optimal_matches,
  file.path(
    out_dir,
    "04_optimal_matching_pairs_all_samples_matrices.csv"
  )
)

# Resumen matching por muestra × matriz × comparación
matching_summary <- all_optimal_matches %>%
  group_by(
    analysis_sample,
    matrix,
    comparison,
    metric
  ) %>%
  summarise(
    n_pairs = n(),
    
    sum_similarity = sum(
      matching_similarity_pct,
      na.rm = TRUE
    ),
    
    mean_similarity = mean(
      matching_similarity_pct,
      na.rm = TRUE
    ),
    
    median_similarity = median(
      matching_similarity_pct,
      na.rm = TRUE
    ),
    
    min_similarity = min(
      matching_similarity_pct,
      na.rm = TRUE
    ),
    
    max_similarity = max(
      matching_similarity_pct,
      na.rm = TRUE
    ),
    
    n_gt50 = sum(
      matching_similarity_pct >
        MATCH_THRESHOLD,
      na.rm = TRUE
    ),
    
    n_ge50 = sum(
      matching_similarity_pct >=
        MATCH_THRESHOLD,
      na.rm = TRUE
    ),
    
    pct_pairs_gt50 =
      100 *
      n_gt50 /
      n_pairs,
    
    .groups = "drop"
  ) %>%
  arrange(
    factor(
      analysis_sample,
      levels = ANALYSIS_SAMPLES
    ),
    factor(
      matrix,
      levels = MATRICES
    ),
    comparison
  )

write_csv(
  matching_summary,
  file.path(
    out_dir,
    "05_matching_summary_by_sample_matrix_comparison.csv"
  )
)

# Pasar las tres comparaciones a columnas
matching_wide <- matching_summary %>%
  select(
    analysis_sample,
    matrix,
    comparison,
    mean_similarity,
    median_similarity,
    n_gt50,
    n_ge50
  ) %>%
  pivot_wider(
    names_from = comparison,
    values_from = c(
      mean_similarity,
      median_similarity,
      n_gt50,
      n_ge50
    ),
    names_glue = "{.value}__{comparison}"
  )

# Resumen integrado muestra × matriz
sample_matrix_summary <- comparison_summary_10 %>%
  select(
    analysis_sample,
    matrix_name,
    matrix,
    n_valid_efa_bootstraps,
    efa_reference_bootstrap,
    mean_efa_alignment_similarity,
    median_efa_alignment_similarity,
    coverage_after_6_prototypes
  ) %>%
  left_join(
    matching_wide,
    by = c(
      "analysis_sample",
      "matrix"
    )
  ) %>%
  arrange(
    factor(
      analysis_sample,
      levels = ANALYSIS_SAMPLES
    ),
    factor(
      matrix,
      levels = MATRICES
    )
  )

write_csv(
  sample_matrix_summary,
  file.path(
    out_dir,
    "06_sample_matrix_summary.csv"
  )
)

# COMPLETE: resumen de las cuatro matrices
complete_matrix_summary <- sample_matrix_summary %>%
  filter(
    analysis_sample == "COMPLETE"
  ) %>%
  arrange(
    factor(
      matrix,
      levels = MATRICES
    )
  )

write_csv(
  complete_matrix_summary,
  file.path(
    out_dir,
    "07_COMPLETE_matrix_summary.csv"
  )
)

# Resumen de cada matriz a través de las siete muestras
matrix_summary_across_samples <- sample_matrix_summary %>%
  group_by(
    matrix
  ) %>%
  summarise(
    n_samples = n(),
    
    mean_coverage_P6 = mean(
      coverage_after_6_prototypes,
      na.rm = TRUE
    ),
    
    median_coverage_P6 = median(
      coverage_after_6_prototypes,
      na.rm = TRUE
    ),
    
    min_coverage_P6 = min(
      coverage_after_6_prototypes,
      na.rm = TRUE
    ),
    
    max_coverage_P6 = max(
      coverage_after_6_prototypes,
      na.rm = TRUE
    ),
    
    mean_efa_alignment = mean(
      mean_efa_alignment_similarity,
      na.rm = TRUE
    ),
    
    mean_KM_EFA = mean(
      mean_similarity__KM_vs_EFA,
      na.rm = TRUE
    ),
    
    mean_KM_EXPERTS = mean(
      mean_similarity__KM_vs_EXPERTS,
      na.rm = TRUE
    ),
    
    mean_EFA_EXPERTS = mean(
      mean_similarity__EFA_vs_EXPERTS,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  arrange(
    factor(
      matrix,
      levels = MATRICES
    )
  )

write_csv(
  matrix_summary_across_samples,
  file.path(
    out_dir,
    "08_matrix_summary_across_samples.csv"
  )
)

# Resumen de cada muestra a través de las cuatro matrices
sample_summary_across_matrices <- sample_matrix_summary %>%
  group_by(
    analysis_sample
  ) %>%
  summarise(
    n_matrices = n(),
    
    mean_coverage_P6 = mean(
      coverage_after_6_prototypes,
      na.rm = TRUE
    ),
    
    mean_efa_alignment = mean(
      mean_efa_alignment_similarity,
      na.rm = TRUE
    ),
    
    mean_KM_EFA = mean(
      mean_similarity__KM_vs_EFA,
      na.rm = TRUE
    ),
    
    mean_KM_EXPERTS = mean(
      mean_similarity__KM_vs_EXPERTS,
      na.rm = TRUE
    ),
    
    mean_EFA_EXPERTS = mean(
      mean_similarity__EFA_vs_EXPERTS,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  arrange(
    factor(
      analysis_sample,
      levels = ANALYSIS_SAMPLES
    )
  )

write_csv(
  sample_summary_across_matrices,
  file.path(
    out_dir,
    "09_sample_summary_across_matrices.csv"
  )
)

# Figura 1: Greedy COMPLETE, sensibilidad D y cobertura P1-P8
complete_curve <- greedy_curve_P1_P8 %>%
  filter(
    analysis_sample == "COMPLETE"
  ) %>%
  mutate(
    matrix = factor(
      matrix,
      levels = MATRICES
    )
  )

p_complete_curve <- ggplot(
  complete_curve,
  aes(
    x = prototype,
    y = cumulative_covered_pct,
    color = factor(d_det),
    group = factor(d_det)
  )
) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 1.8
  ) +
  facet_wrap(
    ~ matrix,
    ncol = 2
  ) +
  scale_x_continuous(
    breaks = 1:8,
    labels = paste0(
      "P",
      1:8
    )
  ) +
  coord_cartesian(
    ylim = c(
      0,
      100
    )
  ) +
  labs(
    title = "Greedy K-means - COMPLETE - cobertura P1-P8",
    subtitle = "D=8:15; H elegido para mantener aproximadamente >=75% de determinantes comunes",
    x = "Número de prototipos",
    y = "Cobertura acumulada (%)",
    color = "D"
  ) +
  theme_minimal(
    base_size = 11
  )

save_plot(
  p_complete_curve,
  file.path(
    fig_dir,
    "01_COMPLETE_greedy_coverage_P1_P8.png"
  ),
  width = 11,
  height = 8
)

# Figura 2: Cobertura de seis prototipos en todas las muestras y matrices
coverage_plot_data <- sample_matrix_summary %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = ANALYSIS_SAMPLES
    ),
    matrix = factor(
      matrix,
      levels = MATRICES
    )
  )

p_coverage <- ggplot(
  coverage_plot_data,
  aes(
    x = matrix,
    y = analysis_sample,
    fill = coverage_after_6_prototypes
  )
) +
  geom_tile(
    linewidth = 0.5
  ) +
  geom_text(
    aes(
      label = paste0(
        round(
          coverage_after_6_prototypes,
          1
        ),
        "%"
      )
    ),
    size = 3.3
  ) +
  scale_fill_gradient(
    limits = c(
      0,
      100
    ),
    name = "Coverage %"
  ) +
  labs(
    title = "Greedy coverage with 6 candidate prototypes",
    subtitle = "D=8 | Hamming=4 | equal_candidate",
    x = "Matrix",
    y = "Analysis sample"
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    panel.grid = element_blank()
  )

save_plot(
  p_coverage,
  file.path(
    fig_dir,
    "02_coverage_P6_all_samples_matrices.png"
  ),
  width = 9,
  height = 7
)

# Figura 3: Similitud de las tres comparaciones para todas las muestras
similarity_plot_data <- matching_summary %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = ANALYSIS_SAMPLES
    ),
    matrix = factor(
      matrix,
      levels = MATRICES
    ),
    comparison = factor(
      comparison,
      levels = c(
        "KM_vs_EFA",
        "KM_vs_EXPERTS",
        "EFA_vs_EXPERTS"
      )
    )
  )

p_similarity_all <- ggplot(
  similarity_plot_data,
  aes(
    x = matrix,
    y = analysis_sample,
    fill = mean_similarity
  )
) +
  geom_tile(
    linewidth = 0.4
  ) +
  geom_text(
    aes(
      label = paste0(
        round(
          mean_similarity,
          1
        ),
        "%"
      )
    ),
    size = 2.8
  ) +
  facet_wrap(
    ~ comparison,
    nrow = 1
  ) +
  scale_fill_gradient(
    limits = c(
      0,
      100
    ),
    name = "Mean %"
  ) +
  labs(
    title = "Optimal matching similarity by sample and matrix",
    subtitle = "KM-EFA uses overlap; comparisons with experts use Jaccard",
    x = "Matrix",
    y = "Analysis sample"
  ) +
  theme_minimal(
    base_size = 10
  ) +
  theme(
    panel.grid = element_blank()
  )

save_plot(
  p_similarity_all,
  file.path(
    fig_dir,
    "03_matching_similarity_all_samples_matrices.png"
  ),
  width = 15,
  height = 7
)

# Figura 4: COMPLETE, comparación triangular por matriz
complete_matching_plot <- matching_summary %>%
  filter(
    analysis_sample == "COMPLETE"
  ) %>%
  mutate(
    matrix = factor(
      matrix,
      levels = MATRICES
    ),
    comparison = factor(
      comparison,
      levels = c(
        "KM_vs_EFA",
        "KM_vs_EXPERTS",
        "EFA_vs_EXPERTS"
      )
    )
  )

p_complete_matching <- ggplot(
  complete_matching_plot,
  aes(
    x = matrix,
    y = comparison,
    fill = mean_similarity
  )
) +
  geom_tile(
    linewidth = 0.6
  ) +
  geom_text(
    aes(
      label = paste0(
        round(
          mean_similarity,
          1
        ),
        "%"
      )
    ),
    size = 4
  ) +
  scale_fill_gradient(
    limits = c(
      0,
      100
    ),
    name = "Mean %"
  ) +
  labs(
    title = "COMPLETE - triangular comparison",
    subtitle = "KM-EFA uses overlap; expert comparisons use Jaccard",
    x = "Matrix",
    y = NULL
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    panel.grid = element_blank()
  )

save_plot(
  p_complete_matching,
  file.path(
    fig_dir,
    "04_COMPLETE_triangular_comparison.png"
  ),
  width = 9,
  height = 5.5
)

# Figura 5: Estabilidad EFA por muestra y matriz
efa_stability_plot <- sample_matrix_summary %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = ANALYSIS_SAMPLES
    ),
    matrix = factor(
      matrix,
      levels = MATRICES
    )
  )

p_efa_stability <- ggplot(
  efa_stability_plot,
  aes(
    x = matrix,
    y = analysis_sample,
    fill = mean_efa_alignment_similarity
  )
) +
  geom_tile(
    linewidth = 0.5
  ) +
  geom_text(
    aes(
      label = paste0(
        round(
          mean_efa_alignment_similarity,
          1
        ),
        "%"
      )
    ),
    size = 3.3
  ) +
  scale_fill_gradient(
    limits = c(
      0,
      100
    ),
    name = "Alignment %"
  ) +
  labs(
    title = "EFA factor-alignment stability",
    subtitle = "Mean alignment similarity across bootstrap solutions",
    x = "Matrix",
    y = "Analysis sample"
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    panel.grid = element_blank()
  )

save_plot(
  p_efa_stability,
  file.path(
    fig_dir,
    "05_EFA_alignment_all_samples_matrices.png"
  ),
  width = 9,
  height = 7
)

# Figura 6: Resumen descriptivo de matrices a través de muestras

matrix_summary_long <- matrix_summary_across_samples %>%
  select(
    matrix,
    mean_coverage_P6,
    mean_efa_alignment,
    mean_KM_EFA,
    mean_KM_EXPERTS,
    mean_EFA_EXPERTS
  ) %>%
  pivot_longer(
    cols = -matrix,
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    matrix = factor(
      matrix,
      levels = MATRICES
    ),
    metric = recode(
      metric,
      "mean_coverage_P6" = "Greedy coverage P6",
      "mean_efa_alignment" = "EFA alignment",
      "mean_KM_EFA" = "KM-EFA",
      "mean_KM_EXPERTS" = "KM-Experts",
      "mean_EFA_EXPERTS" = "EFA-Experts"
    )
  )

p_matrix_summary <- ggplot(
  matrix_summary_long,
  aes(
    x = matrix,
    y = value,
    group = metric
  )
) +
  geom_line(
    aes(
      linetype = metric
    ),
    linewidth = 0.8
  ) +
  geom_point(
    aes(
      shape = metric
    ),
    size = 2.5
  ) +
  coord_cartesian(
    ylim = c(
      0,
      100
    )
  ) +
  labs(
    title = "Matrix diagnostics averaged across samples",
    subtitle = "Metrics remain separate; no composite score is calculated",
    x = "Matrix",
    y = "Percentage",
    linetype = "Metric",
    shape = "Metric"
  ) +
  theme_minimal(
    base_size = 11
  )

save_plot(
  p_matrix_summary,
  file.path(
    fig_dir,
    "06_matrix_diagnostics_across_samples.png"
  ),
  width = 11,
  height = 7
)

# Parámetros

parameters <- tibble(
  parameter = c(
    "analysis_samples",
    "matrices",
    "d_det_grid",
    "hamming_grid",
    "reference_common_pct",
    "greedy_weighting",
    "reference_D",
    "reference_H",
    "reference_n_prototypes",
    "reference_n_efa_factors",
    "matching_threshold",
    "KM_EFA_metric",
    "KM_EXPERTS_metric",
    "EFA_EXPERTS_metric"
  ),
  
  value = c(
    paste(
      ANALYSIS_SAMPLES,
      collapse = ", "
    ),
    paste(
      MATRICES,
      collapse = ", "
    ),
    paste(
      D_DET_GRID,
      collapse = ", "
    ),
    paste(
      HAMMING_GRID,
      collapse = ", "
    ),
    as.character(
      REFERENCE_COMMON_PCT
    ),
    REFERENCE_WEIGHTING,
    as.character(
      REFERENCE_D
    ),
    as.character(
      REFERENCE_H
    ),
    as.character(
      REFERENCE_N_PROTOTYPES
    ),
    as.character(
      REFERENCE_N_EFA_FACTORS
    ),
    paste0(
      ">",
      MATCH_THRESHOLD
    ),
    "overlap_left_pct",
    "jaccard_pct",
    "jaccard_pct"
  )
)

write_csv(
  parameters,
  file.path(
    out_dir,
    "10_parameters.csv"
  )
)

# Comprobaciones finales
expected_n_matching_groups <-
  length(ANALYSIS_SAMPLES) *
  length(MATRICES) *
  3L

if (
  nrow(matching_summary) !=
  expected_n_matching_groups
) {
  stop(
    "Esperaba ",
    expected_n_matching_groups,
    " combinaciones muestra × matriz × comparación y encuentro ",
    nrow(matching_summary),
    "."
  )
}

if (
  nrow(sample_matrix_summary) !=
  length(ANALYSIS_SAMPLES) *
  length(MATRICES)
) {
  stop(
    "El resumen muestra × matriz no contiene las 28 combinaciones esperadas."
  )
}

# Resultados en consola
cat(
  "RESUMEN GLOBAL COMPLETADO\n"
)

cat(
  "\nH DE REFERENCIA PARA D=8,...,15\n\n"
)

print(
  reference_hamming_by_d,
  n = Inf,
  width = Inf
)

cat(
  "\nCOMPLETE - GREEDY D=8,...,15\n\n"
)

print(
  greedy_D_summary %>%
    filter(
      analysis_sample == "COMPLETE"
    ),
  n = Inf,
  width = Inf
)

cat(
  "\nCOMPLETE - RESUMEN DE LAS CUATRO MATRICES\n\n"
)

print(
  complete_matrix_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nRESUMEN DE MATRICES A TRAVÉS DE TODAS LAS MUESTRAS\n\n"
)

print(
  matrix_summary_across_samples,
  n = Inf,
  width = Inf
)

cat(
  "\nRESUMEN DE MUESTRAS A TRAVÉS DE LAS CUATRO MATRICES\n\n"
)

print(
  sample_summary_across_matrices,
  n = Inf,
  width = Inf
)

cat(
  "\nMATCHING - COMPLETE\n\n"
)

print(
  matching_summary %>%
    filter(
      analysis_sample == "COMPLETE"
    ) %>%
    select(
      matrix,
      comparison,
      metric,
      mean_similarity,
      median_similarity,
      min_similarity,
      max_similarity,
      n_gt50,
      n_pairs
    ),
  n = Inf,
  width = Inf
)

cat(
  "\nResultados guardados en:\n",
  out_dir,
  "\n",
  sep = ""
)

cat(
  "\nFICHEROS PRINCIPALES:\n",
  "01_reference_hamming_by_D.csv\n",
  "02_greedy_D8_D15_summary_all_samples.csv\n",
  "03_greedy_curve_P1_P8_all_samples.csv\n",
  "04_optimal_matching_pairs_all_samples_matrices.csv\n",
  "05_matching_summary_by_sample_matrix_comparison.csv\n",
  "06_sample_matrix_summary.csv\n",
  "07_COMPLETE_matrix_summary.csv\n",
  "08_matrix_summary_across_samples.csv\n",
  "09_sample_summary_across_matrices.csv\n",
  "10_parameters.csv\n",
  sep = ""
)

cat(
  "\nFIGURAS PRINCIPALES:\n",
  "01_COMPLETE_greedy_coverage_P1_P8.png\n",
  "02_coverage_P6_all_samples_matrices.png\n",
  "03_matching_similarity_all_samples_matrices.png\n",
  "04_COMPLETE_triangular_comparison.png\n",
  "05_EFA_alignment_all_samples_matrices.png\n",
  "06_matrix_diagnostics_across_samples.png\n",
  sep = ""
)

message(
  "\nListo."
)
