# 11b. Resumen global de la rama D-pooled
#
# Objetivo:
# Integrar los resultados de 09b y 10b para las 7 muestras y las 4 matrices.
#
# Arquitectura:
# - D=8:15 está integrado en un único pool tanto para K-means como para EFA.
# - 09b resume la sensibilidad H x P para ambos métodos.
# - 10b compara Greedy-Kmeans, Greedy-EFA y arquetipos expertos.
#
# Configuración comparativa provisional:
# - weighting = equal_candidate
# - H = 6
# - P = 6
#
# El script:
# - resume cobertura P1-P8 para H=0:10;
# - resume P necesarios para 80%, 85%, 90% y 95%;
# - compara cobertura K-means y EFA;
# - compara K-means vs EFA, K-means vs expertos y EFA vs expertos;
# - resume resultados por muestra y por matriz;
# - genera figuras integradas.
#
# No selecciona automáticamente una matriz, H o P final.
# No contiene operaciones aleatorias y no necesita set.seed().

suppressPackageStartupMessages({
  library(tidyverse)
})

if (!requireNamespace("clue", quietly = TRUE)) {
  stop(
    "Necesitas instalar 'clue': install.packages(\"clue\")"
  )
}


# Configuración

processed_root <- file.path("paper1_cluster/data/processed")

analysis_09b_dir <- file.path(
  processed_root,
  "09b_analysis_Dpooled"
)

greedy_hp_file <- file.path(
  analysis_09b_dir,
  "05_GREEDY_H_P_summary_all_samples.csv"
)

comparison_root <- file.path(
  processed_root,
  "10b_compare_greedy_kmeans_efa_experts_Dpooled_H6"
)

comparison_summary_file <- file.path(
  comparison_root,
  "00_comparison_summary_all_samples_matrices.csv"
)

out_dir <- file.path(
  processed_root,
  "11b_summary_all_matrices_Dpooled"
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

METHODS <- c(
  "KMEANS",
  "EFA"
)

H_GRID <- 0:10

PROTOTYPE_GRID <- 1:8

COVERAGE_THRESHOLDS <- c(
  80,
  85,
  90,
  95
)

REFERENCE_WEIGHTING <- "equal_candidate"

REFERENCE_H <- 6L

REFERENCE_N_PROTOTYPES <- 6L

MATCH_THRESHOLD <- 50


# Funciones auxiliares

check_file <- function(path) {
  if (!file.exists(path)) {
    stop(
      "No encuentro el fichero:\n",
      path
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
    nrow = length(
      left_ids
    ),
    ncol = length(
      right_ids
    ),
    dimnames = list(
      left_ids,
      right_ids
    )
  )
  
  for (
    i in seq_along(
      left_ids
    )
  ) {
    for (
      j in seq_along(
        right_ids
      )
    ) {
      value <- pairwise %>%
        filter(
          left_id ==
            left_ids[i],
          
          right_id ==
            right_ids[j]
        ) %>%
        pull(
          all_of(
            metric_col
          )
        )
      
      if (
        length(value) > 0 &&
        is.finite(
          value[1]
        )
      ) {
        similarity_matrix[
          i,
          j
        ] <- value[1]
      }
    }
  }
  
  assignment <- as.integer(
    clue::solve_LSAP(
      similarity_matrix,
      maximum = TRUE
    )
  )
  
  tibble(
    analysis_sample =
      analysis_sample_current,
    
    matrix =
      matrix_current,
    
    comparison =
      comparison_name,
    
    metric =
      metric_col,
    
    left_id =
      left_ids,
    
    right_id =
      right_ids[
        assignment
      ],
    
    matching_similarity_pct =
      similarity_matrix[
        cbind(
          seq_along(
            left_ids
          ),
          assignment
        )
      ]
  ) %>%
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
  greedy_hp_file
)

check_file(
  comparison_summary_file
)


# Leer resumen H x P del 09b

greedy_hp <- read_csv_safe(
  greedy_hp_file
)

required_hp_columns <- c(
  "analysis_sample",
  "method",
  "matrix",
  "matrix_name",
  "d_hamming",
  paste0(
    "coverage_P",
    1:8
  ),
  "P_for_80",
  "P_for_85",
  "P_for_90",
  "P_for_95",
  "max_coverage_P12"
)

missing_hp_columns <- setdiff(
  required_hp_columns,
  names(
    greedy_hp
  )
)

if (
  length(
    missing_hp_columns
  ) > 0
) {
  stop(
    "Faltan columnas en el output del 09b:\n",
    paste(
      missing_hp_columns,
      collapse = "\n"
    )
  )
}

greedy_hp <- greedy_hp %>%
  mutate(
    d_hamming =
      as.integer(
        d_hamming
      ),
    
    analysis_sample =
      as.character(
        analysis_sample
      ),
    
    method =
      as.character(
        method
      ),
    
    matrix =
      as.character(
        matrix
      )
  ) %>%
  filter(
    analysis_sample %in%
      ANALYSIS_SAMPLES,
    
    method %in%
      METHODS,
    
    matrix %in%
      MATRICES,
    
    d_hamming %in%
      H_GRID
  )


# Comprobar grid H x método

expected_hp_grid <- crossing(
  analysis_sample =
    ANALYSIS_SAMPLES,
  
  method =
    METHODS,
  
  matrix =
    MATRICES,
  
  d_hamming =
    H_GRID
)

available_hp_grid <- greedy_hp %>%
  distinct(
    analysis_sample,
    method,
    matrix,
    d_hamming
  )

missing_hp_grid <- expected_hp_grid %>%
  anti_join(
    available_hp_grid,
    
    by = c(
      "analysis_sample",
      "method",
      "matrix",
      "d_hamming"
    )
  )

if (
  nrow(
    missing_hp_grid
  ) > 0
) {
  print(
    missing_hp_grid,
    n = Inf
  )
  
  stop(
    "El 09b no contiene todas las combinaciones muestra × método × matriz × H."
  )
}


# Guardar resumen completo H x P

write_csv(
  greedy_hp,
  
  file.path(
    out_dir,
    "01_H_P_summary_all_samples_methods.csv"
  )
)


# COMPLETE: curvas H x P

complete_hp <- greedy_hp %>%
  filter(
    analysis_sample ==
      "COMPLETE"
  )

write_csv(
  complete_hp,
  
  file.path(
    out_dir,
    "02_COMPLETE_H_P_summary.csv"
  )
)


# Configuración de referencia H=6

reference_hp <- greedy_hp %>%
  filter(
    d_hamming ==
      REFERENCE_H
  ) %>%
  select(
    analysis_sample,
    matrix_name,
    matrix,
    method,
    coverage_P6,
    P_for_80,
    P_for_85,
    P_for_90,
    P_for_95,
    max_coverage_P12
  ) %>%
  pivot_wider(
    names_from =
      method,
    
    values_from = c(
      coverage_P6,
      P_for_80,
      P_for_85,
      P_for_90,
      P_for_95,
      max_coverage_P12
    ),
    
    names_glue =
      "{.value}_{method}"
  ) %>%
  arrange(
    factor(
      analysis_sample,
      levels =
        ANALYSIS_SAMPLES
    ),
    
    factor(
      matrix,
      levels =
        MATRICES
    )
  )

write_csv(
  reference_hp,
  
  file.path(
    out_dir,
    "03_H6_reference_summary.csv"
  )
)


# Leer resumen del 10b

comparison_summary_10b <- read_csv_safe(
  comparison_summary_file
)

required_comparison_columns <- c(
  "analysis_sample",
  "matrix_name",
  "matrix",
  "greedy_d_strategy",
  "greedy_h",
  "weighting",
  "n_kmeans_prototypes",
  "min_kmeans_signature_size",
  "max_kmeans_signature_size",
  "n_efa_prototypes",
  "min_efa_signature_size",
  "max_efa_signature_size",
  "mean_kmeans_efa_similarity",
  "mean_best_kmeans_expert_jaccard",
  "mean_best_efa_expert_jaccard",
  "coverage_kmeans_after_6_prototypes",
  "coverage_efa_after_6_prototypes"
)

missing_comparison_columns <- setdiff(
  required_comparison_columns,
  names(
    comparison_summary_10b
  )
)

if (
  length(
    missing_comparison_columns
  ) > 0
) {
  stop(
    "Faltan columnas en el resumen del 10b:\n",
    paste(
      missing_comparison_columns,
      collapse = "\n"
    )
  )
}


# Comprobar las 28 combinaciones

expected_sample_matrix_grid <- crossing(
  analysis_sample =
    ANALYSIS_SAMPLES,
  
  matrix =
    MATRICES
)

available_sample_matrix_grid <- comparison_summary_10b %>%
  distinct(
    analysis_sample,
    matrix
  )

missing_sample_matrix_grid <- expected_sample_matrix_grid %>%
  anti_join(
    available_sample_matrix_grid,
    
    by = c(
      "analysis_sample",
      "matrix"
    )
  )

if (
  nrow(
    missing_sample_matrix_grid
  ) > 0
) {
  print(
    missing_sample_matrix_grid,
    n = Inf
  )
  
  stop(
    "El 10b no contiene las 28 combinaciones esperadas."
  )
}


# Comprobar configuración del 10b

if (
  any(
    comparison_summary_10b$greedy_h !=
    REFERENCE_H
  )
) {
  stop(
    "El 10b no está usando H=6 en todas las combinaciones."
  )
}

if (
  any(
    comparison_summary_10b$weighting !=
    REFERENCE_WEIGHTING
  )
) {
  stop(
    "El 10b no está usando equal_candidate en todas las combinaciones."
  )
}


# Comprobar que cobertura 09b y 10b coincide en H=6, P=6

coverage_check <- comparison_summary_10b %>%
  select(
    analysis_sample,
    matrix,
    coverage_kmeans_after_6_prototypes,
    coverage_efa_after_6_prototypes
  ) %>%
  left_join(
    reference_hp %>%
      select(
        analysis_sample,
        matrix,
        coverage_P6_KMEANS,
        coverage_P6_EFA
      ),
    
    by = c(
      "analysis_sample",
      "matrix"
    )
  ) %>%
  mutate(
    diff_kmeans =
      abs(
        coverage_kmeans_after_6_prototypes -
          coverage_P6_KMEANS
      ),
    
    diff_efa =
      abs(
        coverage_efa_after_6_prototypes -
          coverage_P6_EFA
      )
  )

if (
  any(
    coverage_check$diff_kmeans >
    1e-6,
    na.rm = TRUE
  ) ||
  any(
    coverage_check$diff_efa >
    1e-6,
    na.rm = TRUE
  )
) {
  print(
    coverage_check,
    n = Inf
  )
  
  stop(
    "La cobertura de H=6/P=6 no coincide entre 09b y 10b."
  )
}


# Comparaciones triangulares

all_matchings_list <- list()

matching_counter <- 0L

for (
  sample_current in
  ANALYSIS_SAMPLES
) {
  for (
    matrix_current in
    MATRICES
  ) {
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
      "03_kmeans_vs_efa_pairwise.csv"
    )
    
    km_expert_file <- file.path(
      matrix_dir,
      "07_kmeans_vs_experts_pairwise.csv"
    )
    
    efa_expert_file <- file.path(
      matrix_dir,
      "11_efa_vs_experts_pairwise.csv"
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
      pairwise =
        km_efa_pairwise,
      
      left_ids =
        sort(
          unique(
            km_efa_pairwise$left_id
          )
        ),
      
      right_ids =
        sort(
          unique(
            km_efa_pairwise$right_id
          )
        ),
      
      metric_col =
        "overlap_left_pct",
      
      analysis_sample_current =
        sample_current,
      
      matrix_current =
        matrix_current,
      
      comparison_name =
        "KM_vs_EFA"
    )
    
    km_expert_matching <- optimal_matching(
      pairwise =
        km_expert_pairwise,
      
      left_ids =
        sort(
          unique(
            km_expert_pairwise$left_id
          )
        ),
      
      right_ids =
        sort(
          unique(
            km_expert_pairwise$right_id
          )
        ),
      
      metric_col =
        "jaccard_pct",
      
      analysis_sample_current =
        sample_current,
      
      matrix_current =
        matrix_current,
      
      comparison_name =
        "KM_vs_EXPERTS"
    )
    
    efa_expert_matching <- optimal_matching(
      pairwise =
        efa_expert_pairwise,
      
      left_ids =
        sort(
          unique(
            efa_expert_pairwise$left_id
          )
        ),
      
      right_ids =
        sort(
          unique(
            efa_expert_pairwise$right_id
          )
        ),
      
      metric_col =
        "jaccard_pct",
      
      analysis_sample_current =
        sample_current,
      
      matrix_current =
        matrix_current,
      
      comparison_name =
        "EFA_vs_EXPERTS"
    )
    
    matching_counter <-
      matching_counter + 1L
    
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
    analysis_sample =
      factor(
        analysis_sample,
        levels =
          ANALYSIS_SAMPLES
      ),
    
    matrix =
      factor(
        matrix,
        levels =
          MATRICES
      ),
    
    comparison =
      factor(
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
    analysis_sample =
      as.character(
        analysis_sample
      ),
    
    matrix =
      as.character(
        matrix
      ),
    
    comparison =
      as.character(
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


# Resumen de matching

matching_summary <- all_optimal_matches %>%
  group_by(
    analysis_sample,
    matrix,
    comparison,
    metric
  ) %>%
  summarise(
    n_pairs =
      n(),
    
    sum_similarity =
      sum(
        matching_similarity_pct,
        na.rm = TRUE
      ),
    
    mean_similarity =
      mean(
        matching_similarity_pct,
        na.rm = TRUE
      ),
    
    median_similarity =
      median(
        matching_similarity_pct,
        na.rm = TRUE
      ),
    
    min_similarity =
      min(
        matching_similarity_pct,
        na.rm = TRUE
      ),
    
    max_similarity =
      max(
        matching_similarity_pct,
        na.rm = TRUE
      ),
    
    n_gt50 =
      sum(
        matching_similarity_pct >
          MATCH_THRESHOLD,
        na.rm = TRUE
      ),
    
    n_ge50 =
      sum(
        matching_similarity_pct >=
          MATCH_THRESHOLD,
        na.rm = TRUE
      ),
    
    pct_pairs_gt50 =
      100 *
      n_gt50 /
      n_pairs,
    
    .groups =
      "drop"
  ) %>%
  arrange(
    factor(
      analysis_sample,
      levels =
        ANALYSIS_SAMPLES
    ),
    
    factor(
      matrix,
      levels =
        MATRICES
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
    names_from =
      comparison,
    
    values_from = c(
      mean_similarity,
      median_similarity,
      n_gt50,
      n_ge50
    ),
    
    names_glue =
      "{.value}__{comparison}"
  )


# Resumen integrado muestra × matriz

sample_matrix_summary <- comparison_summary_10b %>%
  select(
    analysis_sample,
    matrix_name,
    matrix,
    greedy_d_strategy,
    greedy_h,
    weighting,
    
    n_kmeans_prototypes,
    min_kmeans_signature_size,
    max_kmeans_signature_size,
    
    n_efa_prototypes,
    min_efa_signature_size,
    max_efa_signature_size,
    
    coverage_kmeans_after_6_prototypes,
    coverage_efa_after_6_prototypes
  ) %>%
  left_join(
    reference_hp,
    
    by = c(
      "analysis_sample",
      "matrix_name",
      "matrix"
    )
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
      levels =
        ANALYSIS_SAMPLES
    ),
    
    factor(
      matrix,
      levels =
        MATRICES
    )
  )

write_csv(
  sample_matrix_summary,
  
  file.path(
    out_dir,
    "06_sample_matrix_summary.csv"
  )
)


# COMPLETE

complete_matrix_summary <- sample_matrix_summary %>%
  filter(
    analysis_sample ==
      "COMPLETE"
  ) %>%
  arrange(
    factor(
      matrix,
      levels =
        MATRICES
    )
  )

write_csv(
  complete_matrix_summary,
  
  file.path(
    out_dir,
    "07_COMPLETE_matrix_summary.csv"
  )
)


# Resumen de matrices a través de muestras

matrix_summary_across_samples <- sample_matrix_summary %>%
  group_by(
    matrix
  ) %>%
  summarise(
    n_samples =
      n(),
    
    mean_coverage_KM_P6 =
      mean_or_na(
        coverage_kmeans_after_6_prototypes
      ),
    
    median_coverage_KM_P6 =
      median_or_na(
        coverage_kmeans_after_6_prototypes
      ),
    
    min_coverage_KM_P6 =
      min(
        coverage_kmeans_after_6_prototypes,
        na.rm = TRUE
      ),
    
    mean_coverage_EFA_P6 =
      mean_or_na(
        coverage_efa_after_6_prototypes
      ),
    
    median_coverage_EFA_P6 =
      median_or_na(
        coverage_efa_after_6_prototypes
      ),
    
    min_coverage_EFA_P6 =
      min(
        coverage_efa_after_6_prototypes,
        na.rm = TRUE
      ),
    
    n_samples_KM_reach_90_H6 =
      sum(
        !is.na(
          P_for_90_KMEANS
        )
      ),
    
    n_samples_EFA_reach_90_H6 =
      sum(
        !is.na(
          P_for_90_EFA
        )
      ),
    
    mean_P_for_90_KM_when_reached =
      mean_or_na(
        P_for_90_KMEANS
      ),
    
    mean_P_for_90_EFA_when_reached =
      mean_or_na(
        P_for_90_EFA
      ),
    
    mean_KM_EFA =
      mean_or_na(
        mean_similarity__KM_vs_EFA
      ),
    
    mean_KM_EXPERTS =
      mean_or_na(
        mean_similarity__KM_vs_EXPERTS
      ),
    
    mean_EFA_EXPERTS =
      mean_or_na(
        mean_similarity__EFA_vs_EXPERTS
      ),
    
    .groups =
      "drop"
  ) %>%
  arrange(
    factor(
      matrix,
      levels =
        MATRICES
    )
  )

write_csv(
  matrix_summary_across_samples,
  
  file.path(
    out_dir,
    "08_matrix_summary_across_samples.csv"
  )
)


# Resumen de muestras a través de matrices

sample_summary_across_matrices <- sample_matrix_summary %>%
  group_by(
    analysis_sample
  ) %>%
  summarise(
    n_matrices =
      n(),
    
    mean_coverage_KM_P6 =
      mean_or_na(
        coverage_kmeans_after_6_prototypes
      ),
    
    mean_coverage_EFA_P6 =
      mean_or_na(
        coverage_efa_after_6_prototypes
      ),
    
    n_matrices_KM_reach_90_H6 =
      sum(
        !is.na(
          P_for_90_KMEANS
        )
      ),
    
    n_matrices_EFA_reach_90_H6 =
      sum(
        !is.na(
          P_for_90_EFA
        )
      ),
    
    mean_KM_EFA =
      mean_or_na(
        mean_similarity__KM_vs_EFA
      ),
    
    mean_KM_EXPERTS =
      mean_or_na(
        mean_similarity__KM_vs_EXPERTS
      ),
    
    mean_EFA_EXPERTS =
      mean_or_na(
        mean_similarity__EFA_vs_EXPERTS
      ),
    
    .groups =
      "drop"
  ) %>%
  arrange(
    factor(
      analysis_sample,
      levels =
        ANALYSIS_SAMPLES
    )
  )

write_csv(
  sample_summary_across_matrices,
  
  file.path(
    out_dir,
    "09_sample_summary_across_matrices.csv"
  )
)


# Figura 1: COMPLETE, H x P para K-means y EFA

complete_curve_long <- complete_hp %>%
  select(
    method,
    matrix,
    d_hamming,
    starts_with(
      "coverage_P"
    )
  ) %>%
  pivot_longer(
    cols =
      starts_with(
        "coverage_P"
      ),
    
    names_to =
      "prototype",
    
    names_pattern =
      "coverage_P(\\d+)",
    
    values_to =
      "cumulative_covered_pct"
  ) %>%
  mutate(
    prototype =
      as.integer(
        prototype
      ),
    
    prototype_label =
      paste0(
        "P",
        prototype
      ),
    
    method =
      factor(
        method,
        levels =
          METHODS
      ),
    
    matrix =
      factor(
        matrix,
        levels =
          MATRICES
      )
  )

p_complete_hp <- ggplot(
  complete_curve_long,
  aes(
    x =
      d_hamming,
    
    y =
      cumulative_covered_pct,
    
    color =
      prototype_label,
    
    group =
      prototype_label
  )
) +
  geom_hline(
    yintercept =
      75,
    
    linetype =
      "dashed",
    
    linewidth =
      0.4
  ) +
  geom_line(
    linewidth =
      0.7
  ) +
  geom_point(
    size =
      1.5
  ) +
  facet_grid(
    method ~ matrix
  ) +
  scale_x_continuous(
    breaks =
      H_GRID
  ) +
  coord_cartesian(
    ylim = c(
      0,
      100
    )
  ) +
  labs(
    title =
      "COMPLETE - Greedy D-pooled - H x P",
    
    subtitle =
      "D=8:15 pooled | equal_candidate | dashed line = 75% coverage",
    
    x =
      "Hamming radius (H)",
    
    y =
      "Cumulative coverage (%)",
    
    color =
      "Prototypes"
  ) +
  theme_minimal(
    base_size =
      10
  ) +
  theme(
    panel.grid.minor =
      element_blank()
  )

save_plot(
  p_complete_hp,
  
  file.path(
    fig_dir,
    "01_COMPLETE_H_by_P_KMEANS_EFA.png"
  ),
  
  width =
    14,
  
  height =
    8
)


# Figura 2: cobertura P6 con H=6

coverage_plot_data <- sample_matrix_summary %>%
  select(
    analysis_sample,
    matrix,
    coverage_kmeans_after_6_prototypes,
    coverage_efa_after_6_prototypes
  ) %>%
  pivot_longer(
    cols = c(
      coverage_kmeans_after_6_prototypes,
      coverage_efa_after_6_prototypes
    ),
    
    names_to =
      "method",
    
    values_to =
      "coverage"
  ) %>%
  mutate(
    method =
      recode(
        method,
        "coverage_kmeans_after_6_prototypes" =
          "KMEANS",
        "coverage_efa_after_6_prototypes" =
          "EFA"
      ),
    
    analysis_sample =
      factor(
        analysis_sample,
        levels =
          ANALYSIS_SAMPLES
      ),
    
    matrix =
      factor(
        matrix,
        levels =
          MATRICES
      )
  )

p_coverage <- ggplot(
  coverage_plot_data,
  aes(
    x =
      matrix,
    
    y =
      analysis_sample,
    
    fill =
      coverage
  )
) +
  geom_tile(
    linewidth =
      0.5
  ) +
  geom_text(
    aes(
      label =
        paste0(
          round(
            coverage,
            1
          ),
          "%"
        )
    ),
    
    size =
      3
  ) +
  facet_wrap(
    ~ method,
    nrow =
      1
  ) +
  scale_fill_gradient(
    limits = c(
      0,
      100
    ),
    
    name =
      "Coverage %"
  ) +
  labs(
    title =
      "Greedy coverage with 6 prototypes",
    
    subtitle =
      "D=8:15 pooled | H=6 | equal_candidate",
    
    x =
      "Matrix",
    
    y =
      "Analysis sample"
  ) +
  theme_minimal(
    base_size =
      11
  ) +
  theme(
    panel.grid =
      element_blank()
  )

save_plot(
  p_coverage,
  
  file.path(
    fig_dir,
    "02_coverage_P6_H6_KMEANS_EFA.png"
  ),
  
  width =
    12,
  
  height =
    7
)


# Figura 3: prototipos necesarios para 90% con H=6

p90_plot_data <- reference_hp %>%
  select(
    analysis_sample,
    matrix,
    P_for_90_KMEANS,
    P_for_90_EFA
  ) %>%
  pivot_longer(
    cols = c(
      P_for_90_KMEANS,
      P_for_90_EFA
    ),
    
    names_to =
      "method",
    
    values_to =
      "prototypes_needed"
  ) %>%
  mutate(
    method =
      recode(
        method,
        "P_for_90_KMEANS" =
          "KMEANS",
        "P_for_90_EFA" =
          "EFA"
      ),
    
    prototype_label =
      if_else(
        is.na(
          prototypes_needed
        ),
        ">12",
        as.character(
          prototypes_needed
        )
      ),
    
    analysis_sample =
      factor(
        analysis_sample,
        levels =
          ANALYSIS_SAMPLES
      ),
    
    matrix =
      factor(
        matrix,
        levels =
          MATRICES
      )
  )

p_p90 <- ggplot(
  p90_plot_data,
  aes(
    x =
      matrix,
    
    y =
      analysis_sample,
    
    fill =
      prototypes_needed
  )
) +
  geom_tile(
    linewidth =
      0.5
  ) +
  geom_text(
    aes(
      label =
        prototype_label
    ),
    
    size =
      3
  ) +
  facet_wrap(
    ~ method,
    nrow =
      1
  ) +
  labs(
    title =
      "Prototypes needed for 90% coverage at H=6",
    
    subtitle =
      "D=8:15 pooled | >12 = threshold not reached",
    
    x =
      "Matrix",
    
    y =
      "Analysis sample",
    
    fill =
      "Prototypes"
  ) +
  theme_minimal(
    base_size =
      11
  ) +
  theme(
    panel.grid =
      element_blank()
  )

save_plot(
  p_p90,
  
  file.path(
    fig_dir,
    "03_prototypes_needed_90pct_H6.png"
  ),
  
  width =
    12,
  
  height =
    7
)


# Figura 4: similitud de las tres comparaciones

similarity_plot_data <- matching_summary %>%
  mutate(
    analysis_sample =
      factor(
        analysis_sample,
        levels =
          ANALYSIS_SAMPLES
      ),
    
    matrix =
      factor(
        matrix,
        levels =
          MATRICES
      ),
    
    comparison =
      factor(
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
    x =
      matrix,
    
    y =
      analysis_sample,
    
    fill =
      mean_similarity
  )
) +
  geom_tile(
    linewidth =
      0.4
  ) +
  geom_text(
    aes(
      label =
        paste0(
          round(
            mean_similarity,
            1
          ),
          "%"
        )
    ),
    
    size =
      2.8
  ) +
  facet_wrap(
    ~ comparison,
    nrow =
      1
  ) +
  scale_fill_gradient(
    limits = c(
      0,
      100
    ),
    
    name =
      "Mean %"
  ) +
  labs(
    title =
      "Optimal matching similarity by sample and matrix",
    
    subtitle =
      "KM-EFA uses overlap; comparisons with experts use Jaccard",
    
    x =
      "Matrix",
    
    y =
      "Analysis sample"
  ) +
  theme_minimal(
    base_size =
      10
  ) +
  theme(
    panel.grid =
      element_blank()
  )

save_plot(
  p_similarity_all,
  
  file.path(
    fig_dir,
    "04_matching_similarity_all_samples_matrices.png"
  ),
  
  width =
    15,
  
  height =
    7
)


# Figura 5: COMPLETE, comparación triangular

complete_matching_plot <- matching_summary %>%
  filter(
    analysis_sample ==
      "COMPLETE"
  ) %>%
  mutate(
    matrix =
      factor(
        matrix,
        levels =
          MATRICES
      ),
    
    comparison =
      factor(
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
    x =
      matrix,
    
    y =
      comparison,
    
    fill =
      mean_similarity
  )
) +
  geom_tile(
    linewidth =
      0.6
  ) +
  geom_text(
    aes(
      label =
        paste0(
          round(
            mean_similarity,
            1
          ),
          "%"
        )
    ),
    
    size =
      4
  ) +
  scale_fill_gradient(
    limits = c(
      0,
      100
    ),
    
    name =
      "Mean %"
  ) +
  labs(
    title =
      "COMPLETE - triangular comparison",
    
    subtitle =
      "Greedy-Kmeans vs Greedy-EFA vs experts | D=8:15 pooled | H=6 | P=6",
    
    x =
      "Matrix",
    
    y =
      NULL
  ) +
  theme_minimal(
    base_size =
      11
  ) +
  theme(
    panel.grid =
      element_blank()
  )

save_plot(
  p_complete_matching,
  
  file.path(
    fig_dir,
    "05_COMPLETE_triangular_comparison.png"
  ),
  
  width =
    9,
  
  height =
    5.5
)


# Figura 6: resumen descriptivo de matrices

matrix_summary_long <- matrix_summary_across_samples %>%
  select(
    matrix,
    mean_coverage_KM_P6,
    mean_coverage_EFA_P6,
    mean_KM_EFA,
    mean_KM_EXPERTS,
    mean_EFA_EXPERTS
  ) %>%
  pivot_longer(
    cols =
      -matrix,
    
    names_to =
      "metric",
    
    values_to =
      "value"
  ) %>%
  mutate(
    matrix =
      factor(
        matrix,
        levels =
          MATRICES
      ),
    
    metric =
      recode(
        metric,
        
        "mean_coverage_KM_P6" =
          "K-means coverage P6",
        
        "mean_coverage_EFA_P6" =
          "EFA coverage P6",
        
        "mean_KM_EFA" =
          "KM-EFA",
        
        "mean_KM_EXPERTS" =
          "KM-Experts",
        
        "mean_EFA_EXPERTS" =
          "EFA-Experts"
      )
  )

p_matrix_summary <- ggplot(
  matrix_summary_long,
  aes(
    x =
      matrix,
    
    y =
      value,
    
    group =
      metric
  )
) +
  geom_line(
    aes(
      linetype =
        metric
    ),
    
    linewidth =
      0.8
  ) +
  geom_point(
    aes(
      shape =
        metric
    ),
    
    size =
      2.5
  ) +
  coord_cartesian(
    ylim = c(
      0,
      100
    )
  ) +
  labs(
    title =
      "Matrix diagnostics averaged across samples",
    
    subtitle =
      "Metrics remain separate; no composite score is calculated",
    
    x =
      "Matrix",
    
    y =
      "Percentage",
    
    linetype =
      "Metric",
    
    shape =
      "Metric"
  ) +
  theme_minimal(
    base_size =
      11
  )

save_plot(
  p_matrix_summary,
  
  file.path(
    fig_dir,
    "06_matrix_diagnostics_across_samples.png"
  ),
  
  width =
    11,
  
  height =
    7
)


# Parámetros

parameters <- tibble(
  parameter = c(
    "analysis_samples",
    "matrices",
    "methods",
    "D_strategy",
    "H_grid",
    "prototype_grid",
    "coverage_thresholds",
    "greedy_weighting",
    "reference_H",
    "reference_n_prototypes",
    "matching_threshold",
    "KM_EFA_metric",
    "KM_EXPERTS_metric",
    "EFA_EXPERTS_metric",
    "09b_input",
    "10b_input",
    "random_seed"
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
      METHODS,
      collapse = ", "
    ),
    
    "D=8:15 pooled for KMEANS and EFA",
    
    paste(
      H_GRID,
      collapse = ", "
    ),
    
    paste(
      PROTOTYPE_GRID,
      collapse = ", "
    ),
    
    paste(
      COVERAGE_THRESHOLDS,
      collapse = ", "
    ),
    
    REFERENCE_WEIGHTING,
    
    as.character(
      REFERENCE_H
    ),
    
    as.character(
      REFERENCE_N_PROTOTYPES
    ),
    
    paste0(
      ">",
      MATCH_THRESHOLD
    ),
    
    "overlap_left_pct",
    
    "jaccard_pct",
    
    "jaccard_pct",
    
    greedy_hp_file,
    
    comparison_summary_file,
    
    "NONE"
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
  length(
    ANALYSIS_SAMPLES
  ) *
  length(
    MATRICES
  ) *
  3L

if (
  nrow(
    matching_summary
  ) !=
  expected_n_matching_groups
) {
  stop(
    "Esperaba ",
    expected_n_matching_groups,
    " combinaciones muestra × matriz × comparación y encuentro ",
    nrow(
      matching_summary
    ),
    "."
  )
}

if (
  nrow(
    sample_matrix_summary
  ) !=
  length(
    ANALYSIS_SAMPLES
  ) *
  length(
    MATRICES
  )
) {
  stop(
    "El resumen muestra × matriz no contiene las 28 combinaciones esperadas."
  )
}


# Resultados en consola

cat(
  "\n11b. RESUMEN GLOBAL D-POOLED COMPLETADO\n"
)

cat(
  "\nCONFIGURACIÓN DE REFERENCIA\n\n",
  "D = 8:15 pooled\n",
  "H = ",
  REFERENCE_H,
  "\n",
  "P = ",
  REFERENCE_N_PROTOTYPES,
  "\n",
  "Weighting = ",
  REFERENCE_WEIGHTING,
  "\n",
  sep = ""
)

cat(
  "\nCOMPLETE - H=6\n\n"
)

print(
  reference_hp %>%
    filter(
      analysis_sample ==
        "COMPLETE"
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
      analysis_sample ==
        "COMPLETE"
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
  "01_H_P_summary_all_samples_methods.csv\n",
  "02_COMPLETE_H_P_summary.csv\n",
  "03_H6_reference_summary.csv\n",
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
  "01_COMPLETE_H_by_P_KMEANS_EFA.png\n",
  "02_coverage_P6_H6_KMEANS_EFA.png\n",
  "03_prototypes_needed_90pct_H6.png\n",
  "04_matching_similarity_all_samples_matrices.png\n",
  "05_COMPLETE_triangular_comparison.png\n",
  "06_matrix_diagnostics_across_samples.png\n",
  sep = ""
)

message(
  "\nListo."
)


### DE CADA SUBMUESRTA EL GRAFICO
# library(tidyverse)
# 
# input_file <- path.expand(
#   "~/Desktop/MASTER/recommendation-engine/TFM/paper1_cluster/data/processed/11b_summary_all_matrices_Dpooled/01_H_P_summary_all_samples_methods.csv"
# )
# 
# out_dir <- path.expand(
#   "~/Desktop/MASTER/recommendation-engine/TFM/paper1_cluster/data/processed/11b_summary_all_matrices_Dpooled/figures/H_P_all_samples"
# )
# 
# dir.create(
#   out_dir,
#   recursive = TRUE,
#   showWarnings = FALSE
# )
# 
# samples <- c(
#   "COMPLETE",
#   "EUROPE",
#   "LATAM",
#   "DIEGO",
#   "RENOVISOR",
#   "WHY_EUROPE",
#   "WHY_LATAM"
# )
# 
# matrix_levels <- c(
#   "RAW",
#   "POS",
#   "EXT",
#   "Z_ABS"
# )
# 
# method_levels <- c(
#   "KMEANS",
#   "EFA"
# )
# 
# df <- read_csv(
#   input_file,
#   show_col_types = FALSE
# )
# 
# plot_data <- df %>%
#   select(
#     analysis_sample,
#     method,
#     matrix,
#     d_hamming,
#     starts_with("coverage_P")
#   ) %>%
#   pivot_longer(
#     cols = starts_with("coverage_P"),
#     names_to = "prototype",
#     names_pattern = "coverage_P(\\d+)",
#     values_to = "coverage"
#   ) %>%
#   mutate(
#     prototype = as.integer(prototype),
#     prototype_label = paste0("P", prototype),
# 
#     prototype_label = factor(
#       prototype_label,
#       levels = paste0("P", 1:8)
#     ),
# 
#     analysis_sample = factor(
#       analysis_sample,
#       levels = samples
#     ),
# 
#     method = factor(
#       method,
#       levels = method_levels
#     ),
# 
#     matrix = factor(
#       matrix,
#       levels = matrix_levels
#     )
#   )
# 
# for (sample_current in samples) {
# 
#   plot_sample <- plot_data %>%
#     filter(
#       analysis_sample == sample_current
#     )
# 
#   p <- ggplot(
#     plot_sample,
#     aes(
#       x = d_hamming,
#       y = coverage,
#       color = prototype_label,
#       group = prototype_label
#     )
#   ) +
#     geom_hline(
#       yintercept = 75,
#       linetype = "dashed",
#       linewidth = 0.5
#     ) +
#     geom_line(
#       linewidth = 0.8
#     ) +
#     geom_point(
#       size = 1.8
#     ) +
#     facet_grid(
#       method ~ matrix
#     ) +
#     scale_x_continuous(
#       breaks = 0:10
#     ) +
#     coord_cartesian(
#       ylim = c(0, 100)
#     ) +
#     labs(
#       title = paste0(
#         sample_current,
#         " - Greedy D-pooled - H x P"
#       ),
#       subtitle = paste0(
#         "D=8:15 pooled | equal_candidate | ",
#         "dashed line = 75% coverage"
#       ),
#       x = "Hamming radius (H)",
#       y = "Cumulative coverage (%)",
#       color = "Prototypes"
#     ) +
#     theme_minimal(
#       base_size = 11
#     ) +
#     theme(
#       panel.grid.minor = element_blank(),
#       plot.title = element_text(
#         face = "bold"
#       ),
#       strip.text = element_text(
#         face = "bold"
#       )
#     )
# 
#   ggsave(
#     filename = file.path(
#       out_dir,
#       paste0(
#         sample_current,
#         "_Greedy_Dpooled_H_by_P.png"
#       )
#     ),
#     plot = p,
#     width = 16,
#     height = 9,
#     dpi = 300,
#     bg = "white"
#   )
# 
#   cat(
#     "Guardado: ",
#     sample_current,
#     "\n",
#     sep = ""
#   )
# }
# 
# cat(
#   "\n7 gráficos guardados en:\n",
#   out_dir,
#   "\n",
#   sep = ""
# )
