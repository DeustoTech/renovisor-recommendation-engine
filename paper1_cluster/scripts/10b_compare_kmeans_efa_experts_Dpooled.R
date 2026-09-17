# 
# Objetivo:
# Comparar los perfiles obtenidos con Greedy D-pooled, la solución EFA
# y los arquetipos expertos para las 7 muestras y las 4 matrices.
#
# Configuración comparativa provisional:
# - Greedy utiliza el pool D=8:15 generado en 08b; D ya no se filtra aquí.
# - H=6 y weighting=equal_candidate.
# - Se comparan 6 prototipos Greedy-Kmeans con 6 factores EFA.
# - Cada factor EFA se representa mediante sus 8 determinantes más relevantes.
# - El consenso EFA entre bootstraps se alinea mediante matching húngaro.
#
# El script compara K-means vs EFA, K-means vs expertos y EFA vs expertos,
# y reutiliza las combinaciones muestra × matriz que ya estén completas.
#
# Esta configuración es provisional y no fija una solución final.
# El script no contiene operaciones aleatorias y no necesita set.seed().

suppressPackageStartupMessages(
  library(tidyverse)
)

if (!requireNamespace("clue", quietly = TRUE)) {
  stop(
    "Necesitas instalar 'clue': install.packages(\"clue\")"
  )
}


# Configuración

project_root <- path.expand(
  "~/Desktop/MASTER/recommendation-engine/TFM"
)

processed_root <- file.path(
  project_root,
  "paper1_cluster/data/processed"
)

greedy_file <- file.path(
  processed_root,
  "08b_greedy_kmeans_efa_Dpooled",
  "07_greedy_prototype_steps_Dpooled.csv"
)

efa_file <- file.path(
  processed_root,
  "07_efa_bootstrap",
  "03_efa_loadings_long.csv.gz"
)

expert_file <- file.path(
  project_root,
  "initial_descriptive_analysis",
  "data",
  "archetypes",
  "archetypeExperts_bin_32.csv"
)

out_root <- file.path(
  processed_root,
  "10b_compare_kmeans_efa_experts_Dpooled_H6"
)

dir.create(
  out_root,
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

MATRICES_TO_RUN <- c(
  "matrix_32_raw_0_1",
  "matrix_32_pos_0_1",
  "matrix_32_ext_0_1",
  "matrix_32_z_abs"
)

REFERENCE_WEIGHTING <- "equal_candidate"

GREEDY_H <- 6L

N_KMEANS_PROTOTYPES <- 6L
N_EFA_FACTORS <- 6L

EFA_TOP_N <- 8L

MAX_BOOTSTRAPS <- 100L

GOOD_SIMILARITY_THRESHOLD <- 75


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

normalize_determinant_key <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_trim() %>%
    str_remove(
      "^det[_\\.-]*\\d+[_\\.-]*"
    ) %>%
    str_replace_all(
      "[^a-z0-9]+",
      "_"
    ) %>%
    str_remove(
      "^_+"
    ) %>%
    str_remove(
      "_+$"
    )
}

parse_determinants <- function(x) {
  if (
    !length(x) ||
    is.na(x) ||
    str_trim(x) == ""
  ) {
    return(
      character()
    )
  }
  
  str_split(
    x,
    ";\\s*"
  )[[1]] %>%
    str_trim() %>%
    unique() %>%
    sort()
}

collapse_determinants <- function(x) {
  paste(
    sort(
      unique(x)
    ),
    collapse = "; "
  )
}

coerce_binary_numeric <- function(x) {
  if (is.logical(x)) {
    return(
      as.numeric(x)
    )
  }
  
  x_chr <- as.character(x) %>%
    str_trim() %>%
    str_to_lower()
  
  out <- suppressWarnings(
    as.numeric(x_chr)
  )
  
  out[
    is.na(out) &
      x_chr %in%
      c(
        "true",
        "yes",
        "y",
        "x"
      )
  ] <- 1
  
  out[
    is.na(out) &
      x_chr %in%
      c(
        "false",
        "no",
        "n",
        "",
        "-"
      )
  ] <- 0
  
  out
}

compare_sets <- function(
    left_set,
    right_set
) {
  left_set <- unique(
    left_set
  )
  
  right_set <- unique(
    right_set
  )
  
  common_set <- intersect(
    left_set,
    right_set
  )
  
  union_set <- union(
    left_set,
    right_set
  )
  
  n_common <- length(
    common_set
  )
  
  tibble(
    n_left = length(
      left_set
    ),
    
    n_right = length(
      right_set
    ),
    
    n_common = n_common,
    
    overlap_left_pct = if (
      length(left_set)
    ) {
      100 *
        n_common /
        length(left_set)
    } else {
      NA_real_
    },
    
    overlap_right_pct = if (
      length(right_set)
    ) {
      100 *
        n_common /
        length(right_set)
    } else {
      NA_real_
    },
    
    jaccard_pct = if (
      length(union_set)
    ) {
      100 *
        n_common /
        length(union_set)
    } else {
      NA_real_
    },
    
    common_determinants =
      collapse_determinants(
        common_set
      )
  )
}

make_pairwise_comparison <- function(
    left,
    right
) {
  crossing(
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
    unnest(
      comparison
    ) %>%
    ungroup()
}

hungarian_matching <- function(
    pairwise,
    left_ids,
    right_ids,
    metric_col
) {
  if (
    length(left_ids) >
    length(right_ids)
  ) {
    stop(
      "El matching húngaro requiere nº izquierda <= nº derecha."
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
        length(value) &&
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
    left_id = left_ids,
    
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
  )
}

save_plot <- function(
    p,
    filename,
    width = 9,
    height = 7
) {
  ggsave(
    filename,
    p,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

write_output <- function(
    data,
    directory,
    filename
) {
  write_csv(
    data,
    file.path(
      directory,
      filename
    )
  )
}

read_csv_safe <- function(path) {
  read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE
  )
}

add_context <- function(
    df,
    sample_value,
    matrix_value
) {
  df %>%
    mutate(
      analysis_sample =
        sample_value,
      
      matrix_name =
        matrix_value,
      
      .before = 1
    )
}

files_complete <- function(paths) {
  if (
    !all(
      file.exists(
        paths
      )
    )
  ) {
    return(
      FALSE
    )
  }
  
  info <- file.info(
    paths
  )
  
  all(
    !is.na(
      info$size
    ) &
      info$size > 0
  )
}

combination_complete <- function(
    current_out_dir,
    current_fig_dir
) {
  csv_files <- file.path(
    current_out_dir,
    c(
      "01_kmeans_6_prototypes.csv",
      "02_efa_reference_bootstrap_scores.csv",
      "03_efa_factor_alignment.csv",
      "04_efa_consensus_determinants_long.csv",
      "05_efa_6_consensus_factors.csv",
      "06_kmeans_vs_efa_pairwise.csv",
      "07_kmeans_vs_efa_optimal_matching.csv",
      "08_kmeans_vs_efa_summary.csv",
      "09_expert_profiles_bin32.csv",
      "10_kmeans_vs_experts_pairwise.csv",
      "11_best_expert_for_each_kmeans.csv",
      "12_best_kmeans_for_each_expert.csv",
      "13_kmeans_vs_experts_hungarian.csv",
      "14_efa_vs_experts_pairwise.csv",
      "15_best_expert_for_each_efa.csv",
      "16_FINAL_kmeans_efa_experts_comparison.csv",
      "17_efa_alignment_summary.csv"
    )
  )
  
  figure_files <- file.path(
    current_fig_dir,
    c(
      "01_kmeans_vs_efa_heatmap.png",
      "02_kmeans_vs_experts_heatmap.png",
      "03_efa_vs_experts_heatmap.png"
    )
  )
  
  files_complete(
    c(
      csv_files,
      figure_files
    )
  )
}


# Comprobar inputs

required_files <- c(
  greedy_file,
  efa_file,
  expert_file
)

missing_files <- required_files[
  !file.exists(
    required_files
  )
]

if (
  length(
    missing_files
  )
) {
  stop(
    "Faltan archivos necesarios:\n",
    paste(
      missing_files,
      collapse = "\n"
    )
  )
}


# Greedy 08b

greedy_steps <- read_csv_safe(
  greedy_file
)

required_greedy <- c(
  "analysis_sample",
  "weighting",
  "method",
  "matrix_name",
  "d_hamming",
  "prototype",
  "center_active_determinants",
  "center_signature_size",
  "incremental_covered_pct",
  "cumulative_covered_pct"
)

missing_greedy <- setdiff(
  required_greedy,
  names(
    greedy_steps
  )
)

if (
  length(
    missing_greedy
  )
) {
  stop(
    "Faltan columnas en Greedy 08b: ",
    paste(
      missing_greedy,
      collapse = ", "
    )
  )
}

if (
  "d_det" %in%
  names(
    greedy_steps
  )
) {
  stop(
    paste0(
      "El fichero contiene d_det. ",
      "Revisa que estés leyendo el output del 08b D-pooled."
    )
  )
}

greedy_steps <- greedy_steps %>%
  mutate(
    prototype =
      as.integer(
        prototype
      ),
    
    d_hamming =
      as.integer(
        d_hamming
      ),
    
    center_signature_size =
      as.integer(
        center_signature_size
      )
  )


# EFA

efa_all <- read_csv_safe(
  efa_file
) %>%
  mutate(
    bootstrap_id =
      as.integer(
        bootstrap_id
      ),
    
    n_factors =
      as.integer(
        n_factors
      ),
    
    determinant =
      as.character(
        determinant
      ),
    
    factor =
      as.character(
        factor
      ),
    
    loading =
      as.numeric(
        loading
      ),
    
    abs_loading =
      abs(
        loading
      )
  )

required_efa <- c(
  "analysis_sample",
  "matrix_name",
  "bootstrap_id",
  "n_factors",
  "determinant",
  "factor",
  "loading"
)

missing_efa <- setdiff(
  required_efa,
  names(
    efa_all
  )
)

if (
  length(
    missing_efa
  )
) {
  stop(
    "Faltan columnas EFA: ",
    paste(
      missing_efa,
      collapse = ", "
    )
  )
}

canonical_determinants <- sort(
  unique(
    efa_all$determinant
  )
)

if (
  length(
    canonical_determinants
  ) != 32
) {
  stop(
    "Esperaba 32 determinantes en EFA y encuentro ",
    length(
      canonical_determinants
    ),
    "."
  )
}

canonical_dictionary <- tibble(
  determinant =
    canonical_determinants,
  
  determinant_key =
    normalize_determinant_key(
      canonical_determinants
    )
)


# Arquetipos expertos

expert_raw <- read_csv_safe(
  expert_file
)

possible_profile_columns <- c(
  "archetype",
  "archetype_name",
  "expert_profile",
  "profile",
  "persona",
  "name",
  "label",
  "type",
  "id"
)

profile_matches <- intersect(
  possible_profile_columns,
  names(
    expert_raw
  )
)

if (
  length(
    profile_matches
  )
) {
  expert_profile_col <-
    profile_matches[1]
  
} else {
  character_columns <- names(
    expert_raw
  )[
    map_lgl(
      expert_raw,
      ~ is.character(.x) ||
        is.factor(.x)
    )
  ]
  
  expert_profile_col <-
    if (
      length(
        character_columns
      )
    ) {
      character_columns[1]
      
    } else {
      names(
        expert_raw
      )[1]
    }
}

expert_alias_dictionary <- tribble(
  ~expert_key,
  ~canonical_key,
  "autarky",
  "autonomy"
)

expert_candidate_cols <- setdiff(
  names(
    expert_raw
  ),
  expert_profile_col
)

expert_column_dictionary <- tibble(
  input_col =
    expert_candidate_cols,
  
  determinant_key_original =
    normalize_determinant_key(
      expert_candidate_cols
    )
) %>%
  left_join(
    expert_alias_dictionary,
    by = c(
      "determinant_key_original" =
        "expert_key"
    )
  ) %>%
  mutate(
    determinant_key =
      if_else(
        !is.na(
          canonical_key
        ),
        canonical_key,
        determinant_key_original
      )
  )

expert_column_mapping <- expert_column_dictionary %>%
  inner_join(
    canonical_dictionary,
    by = "determinant_key"
  )

if (
  nrow(
    expert_column_mapping
  ) != 32 ||
  n_distinct(
    expert_column_mapping$determinant
  ) != 32
) {
  missing_from_expert <-
    canonical_dictionary %>%
    anti_join(
      expert_column_mapping %>%
        select(
          determinant,
          determinant_key
        ),
      by = c(
        "determinant",
        "determinant_key"
      )
    )
  
  print(
    missing_from_expert,
    n = Inf
  )
  
  stop(
    "No se han podido mapear exactamente los 32 determinantes expertos."
  )
}

expert_long <- expert_raw %>%
  mutate(
    expert_profile =
      as.character(.data[[expert_profile_col]]
      )
  ) %>%
  select(
    expert_profile,
    all_of(
      expert_column_mapping$input_col
    )
  ) %>%
  pivot_longer(
    cols =
      -expert_profile,
    
    names_to =
      "input_col",
    
    values_to =
      "value_raw"
  ) %>%
  left_join(
    expert_column_mapping %>%
      select(
        input_col,
        determinant
      ),
    by =
      "input_col"
  ) %>%
  mutate(
    value =
      coerce_binary_numeric(
        value_raw
      ),
    
    active =
      !is.na(
        value
      ) &
      value > 0.5
  )

expert_profiles <- expert_long %>%
  filter(
    active
  ) %>%
  group_by(
    expert_profile
  ) %>%
  summarise(
    id =
      first(
        expert_profile
      ),
    
    signature =
      list(
        sort(
          determinant
        )
      ),
    
    n_active =
      n_distinct(
        determinant
      ),
    
    determinants =
      collapse_determinants(
        determinant
      ),
    
    .groups =
      "drop"
  )

if (
  nrow(
    expert_profiles
  ) != 8
) {
  warning(
    "Esperaba 8 arquetipos expertos y encuentro ",
    nrow(
      expert_profiles
    ),
    "."
  )
}

write_output(
  expert_profiles %>%
    select(
      -signature
    ),
  out_root,
  "00_expert_profiles_bin32.csv"
)


# Consenso EFA

build_efa_consensus <- function(
    efa_all,
    analysis_sample_current,
    matrix_name_current
) {
  efa <- efa_all %>%
    filter(
      analysis_sample ==
        analysis_sample_current,
      
      matrix_name ==
        matrix_name_current,
      
      n_factors ==
        N_EFA_FACTORS
    )
  
  if (
    !nrow(
      efa
    )
  ) {
    stop(
      "No hay EFA para ",
      analysis_sample_current,
      " / ",
      matrix_name_current,
      "."
    )
  }
  
  boot_ids <- sort(
    unique(
      efa$bootstrap_id
    )
  )
  
  if (
    is.finite(
      MAX_BOOTSTRAPS
    )
  ) {
    boot_ids <- head(
      boot_ids,
      MAX_BOOTSTRAPS
    )
  }
  
  efa <- efa %>%
    filter(
      bootstrap_id %in%
        boot_ids
    )
  
  efa_top <- efa %>%
    group_by(
      bootstrap_id,
      factor
    ) %>%
    arrange(
      desc(
        abs_loading
      ),
      determinant,
      .by_group = TRUE
    ) %>%
    slice_head(
      n = EFA_TOP_N
    ) %>%
    ungroup()
  
  valid_boot_ids <- efa_top %>%
    group_by(
      bootstrap_id
    ) %>%
    summarise(
      n_factors_observed =
        n_distinct(
          factor
        ),
      
      n_rows =
        n(),
      
      .groups =
        "drop"
    ) %>%
    filter(
      n_factors_observed ==
        N_EFA_FACTORS,
      
      n_rows ==
        N_EFA_FACTORS *
        EFA_TOP_N
    ) %>%
    pull(
      bootstrap_id
    ) %>%
    sort()
  
  if (
    length(
      valid_boot_ids
    ) < 2
  ) {
    stop(
      "No hay suficientes bootstraps EFA válidos en ",
      analysis_sample_current,
      " / ",
      matrix_name_current,
      "."
    )
  }
  
  efa <- efa %>%
    filter(
      bootstrap_id %in%
        valid_boot_ids
    )
  
  efa_top <- efa_top %>%
    filter(
      bootstrap_id %in%
        valid_boot_ids
    )
  
  factor_sets <- efa_top %>%
    group_by(
      bootstrap_id,
      factor
    ) %>%
    summarise(
      signature =
        list(
          sort(
            determinant
          )
        ),
      
      .groups =
        "drop"
    ) %>%
    arrange(
      bootstrap_id,
      factor
    )
  
  sets_by_boot <- split(
    factor_sets,
    factor_sets$bootstrap_id
  )
  
  get_factor_sets <- function(
    bootstrap_current
  ) {
    sets_by_boot[[as.character(bootstrap_current)]] %>%
      arrange(
        factor
      )
  }
  
  solution_similarity <- function(
    boot_a,
    boot_b
  ) {
    a <- get_factor_sets(
      boot_a
    )
    
    b <- get_factor_sets(
      boot_b
    )
    
    sim_matrix <- matrix(
      0,
      nrow = N_EFA_FACTORS,
      ncol = N_EFA_FACTORS
    )
    
    for (
      i in seq_len(
        N_EFA_FACTORS
      )
    ) {
      for (
        j in seq_len(
          N_EFA_FACTORS
        )
      ) {
        sim_matrix[
          i,
          j
        ] <-
          100 *
          length(
            intersect(
              a$signature[[i]],
              b$signature[[j]]
            )
          ) /
          EFA_TOP_N
      }
    }
    
    assignment <- clue::solve_LSAP(
      sim_matrix,
      maximum = TRUE
    )
    
    mean(
      sim_matrix[
        cbind(
          seq_len(
            N_EFA_FACTORS
          ),
          as.integer(
            assignment
          )
        )
      ],
      na.rm = TRUE
    )
  }
  
  boot_pairs <- combn(
    valid_boot_ids,
    2,
    simplify = FALSE
  )
  
  pair_similarity <- map_dfr(
    boot_pairs,
    ~ tibble(
      boot_a =
        .x[1],
      
      boot_b =
        .x[2],
      
      similarity_pct =
        solution_similarity(
          .x[1],
          .x[2]
        )
    )
  )
  
  reference_scores <- bind_rows(
    pair_similarity %>%
      transmute(
        bootstrap_id =
          boot_a,
        
        similarity_pct
      ),
    
    pair_similarity %>%
      transmute(
        bootstrap_id =
          boot_b,
        
        similarity_pct
      )
  ) %>%
    group_by(
      bootstrap_id
    ) %>%
    summarise(
      mean_similarity_to_others =
        mean(
          similarity_pct,
          na.rm = TRUE
        ),
      
      .groups =
        "drop"
    ) %>%
    arrange(
      desc(
        mean_similarity_to_others
      ),
      bootstrap_id
    )
  
  reference_bootstrap <-
    reference_scores$bootstrap_id[1]
  
  reference_factors <-
    get_factor_sets(
      reference_bootstrap
    )
  
  alignment <- map_dfr(
    valid_boot_ids,
    function(
    boot_current
    ) {
      current_factors <-
        get_factor_sets(
          boot_current
        )
      
      sim_matrix <- matrix(
        0,
        nrow = N_EFA_FACTORS,
        ncol = N_EFA_FACTORS
      )
      
      for (
        i in seq_len(
          N_EFA_FACTORS
        )
      ) {
        for (
          j in seq_len(
            N_EFA_FACTORS
          )
        ) {
          sim_matrix[
            i,
            j
          ] <-
            100 *
            length(
              intersect(
                reference_factors$signature[[i]],
                current_factors$signature[[j]]
              )
            ) /
            EFA_TOP_N
        }
      }
      
      assignment <- as.integer(
        clue::solve_LSAP(
          sim_matrix,
          maximum = TRUE
        )
      )
      
      tibble(
        bootstrap_id =
          boot_current,
        
        consensus_factor =
          seq_len(
            N_EFA_FACTORS
          ),
        
        reference_factor =
          reference_factors$factor,
        
        original_factor =
          current_factors$factor[
            assignment
          ],
        
        alignment_similarity_pct =
          sim_matrix[
            cbind(
              seq_len(
                N_EFA_FACTORS
              ),
              assignment
            )
          ]
      )
    }
  )
  
  efa_aligned <- efa %>%
    left_join(
      alignment %>%
        select(
          bootstrap_id,
          original_factor,
          consensus_factor
        ),
      by = c(
        "bootstrap_id",
        "factor" =
          "original_factor"
      )
    )
  
  efa_top_aligned <- efa_top %>%
    left_join(
      alignment %>%
        select(
          bootstrap_id,
          original_factor,
          consensus_factor
        ),
      by = c(
        "bootstrap_id",
        "factor" =
          "original_factor"
      )
    )
  
  if (
    any(
      is.na(
        efa_top_aligned$consensus_factor
      )
    )
  ) {
    stop(
      "Hay factores EFA sin alinear en ",
      analysis_sample_current,
      " / ",
      matrix_name_current,
      "."
    )
  }
  
  selection_frequency <- efa_top_aligned %>%
    count(
      consensus_factor,
      determinant,
      name =
        "n_bootstraps_selected"
    ) %>%
    mutate(
      selection_frequency =
        n_bootstraps_selected /
        length(
          valid_boot_ids
        )
    )
  
  mean_loading <- efa_aligned %>%
    group_by(
      consensus_factor,
      determinant
    ) %>%
    summarise(
      mean_abs_loading =
        mean(
          abs_loading,
          na.rm = TRUE
        ),
      
      .groups =
        "drop"
    )
  
  consensus_long <- selection_frequency %>%
    full_join(
      mean_loading,
      by = c(
        "consensus_factor",
        "determinant"
      )
    ) %>%
    mutate(
      n_bootstraps_selected =
        replace_na(
          n_bootstraps_selected,
          0L
        ),
      
      selection_frequency =
        replace_na(
          selection_frequency,
          0
        )
    )
  
  consensus_top <- consensus_long %>%
    group_by(
      consensus_factor
    ) %>%
    arrange(
      desc(
        selection_frequency
      ),
      desc(
        mean_abs_loading
      ),
      determinant,
      .by_group = TRUE
    ) %>%
    slice_head(
      n = EFA_TOP_N
    ) %>%
    ungroup()
  
  profiles <- consensus_top %>%
    group_by(
      consensus_factor
    ) %>%
    summarise(
      id =
        paste0(
          "EFA",
          first(
            consensus_factor
          )
        ),
      
      signature =
        list(
          sort(
            determinant
          )
        ),
      
      determinants =
        collapse_determinants(
          determinant
        ),
      
      mean_selection_frequency =
        mean(
          selection_frequency,
          na.rm = TRUE
        ),
      
      mean_abs_loading =
        mean(
          mean_abs_loading,
          na.rm = TRUE
        ),
      
      .groups =
        "drop"
    ) %>%
    arrange(
      consensus_factor
    )
  
  alignment_summary <- alignment %>%
    summarise(
      mean_alignment_similarity =
        mean(
          alignment_similarity_pct,
          na.rm = TRUE
        ),
      
      median_alignment_similarity =
        median(
          alignment_similarity_pct,
          na.rm = TRUE
        ),
      
      min_alignment_similarity =
        min(
          alignment_similarity_pct,
          na.rm = TRUE
        ),
      
      max_alignment_similarity =
        max(
          alignment_similarity_pct,
          na.rm = TRUE
        )
    )
  
  list(
    valid_boot_ids =
      valid_boot_ids,
    
    reference_bootstrap =
      reference_bootstrap,
    
    reference_scores =
      reference_scores,
    
    alignment =
      alignment,
    
    consensus_top =
      consensus_top,
    
    profiles =
      profiles,
    
    alignment_summary =
      alignment_summary
  )
}


# Disponibilidad Greedy

availability_greedy <- greedy_steps %>%
  filter(
    analysis_sample %in%
      ANALYSIS_SAMPLES,
    
    weighting ==
      REFERENCE_WEIGHTING,
    
    method ==
      "KMEANS",
    
    matrix_name %in%
      MATRICES_TO_RUN,
    
    d_hamming ==
      GREEDY_H
  ) %>%
  group_by(
    analysis_sample,
    matrix_name
  ) %>%
  summarise(
    max_prototype =
      max(
        prototype,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  )

expected_grid <- crossing(
  analysis_sample =
    ANALYSIS_SAMPLES,
  
  matrix_name =
    MATRICES_TO_RUN
)

availability_check <- expected_grid %>%
  left_join(
    availability_greedy,
    by = c(
      "analysis_sample",
      "matrix_name"
    )
  )

if (
  any(
    is.na(
      availability_check$max_prototype
    )
  ) ||
  any(
    availability_check$max_prototype <
    N_KMEANS_PROTOTYPES,
    na.rm = TRUE
  )
) {
  print(
    availability_check,
    n = Inf
  )
  
  stop(
    "No hay al menos 6 prototipos Greedy para todas las combinaciones."
  )
}


# Contenedores

run_summary_list <- list()
final_comparison_list <- list()
km_efa_matching_list <- list()
best_expert_km_list <- list()
best_expert_efa_list <- list()
efa_alignment_list <- list()

run_counter <- 0L
n_reused <- 0L
n_computed <- 0L


# Ejecutar cada muestra × matriz

for (
  sample_name in
  ANALYSIS_SAMPLES
) {
  for (
    matrix_name_current in
    MATRICES_TO_RUN
  ) {
    run_counter <-
      run_counter + 1L
    
    matrix_short <-
      matrix_label(
        matrix_name_current
      )
    
    current_out_dir <- file.path(
      out_root,
      sample_name,
      matrix_short
    )
    
    current_fig_dir <- file.path(
      current_out_dir,
      "figures"
    )
    
    dir.create(
      current_out_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    dir.create(
      current_fig_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    cat(
      "\n10b | ",
      sample_name,
      " | ",
      matrix_short,
      "\n",
      sep = ""
    )
    
    if (
      combination_complete(
        current_out_dir,
        current_fig_dir
      )
    ) {
      n_reused <-
        n_reused + 1L
      
      cat(
        "  YA COMPLETADO -> reutilizando resultados existentes.\n"
      )
      
    } else {
      n_computed <-
        n_computed + 1L
      
      cat(
        "  INCOMPLETO -> calculando esta combinación.\n"
      )
      
      
      # 6 prototipos Greedy-Kmeans
      
      kmeans_profiles <- greedy_steps %>%
        filter(
          analysis_sample ==
            sample_name,
          
          weighting ==
            REFERENCE_WEIGHTING,
          
          method ==
            "KMEANS",
          
          matrix_name ==
            matrix_name_current,
          
          d_hamming ==
            GREEDY_H,
          
          prototype <=
            N_KMEANS_PROTOTYPES
        ) %>%
        arrange(
          prototype
        ) %>%
        distinct(
          prototype,
          .keep_all = TRUE
        ) %>%
        transmute(
          id =
            paste0(
              "KM",
              prototype
            ),
          
          prototype,
          
          signature =
            map(
              center_active_determinants,
              parse_determinants
            ),
          
          determinants =
            center_active_determinants,
          
          signature_size =
            center_signature_size,
          
          incremental_covered_pct,
          
          cumulative_covered_pct
        )
      
      if (
        nrow(
          kmeans_profiles
        ) !=
        N_KMEANS_PROTOTYPES
      ) {
        stop(
          "Esperaba ",
          N_KMEANS_PROTOTYPES,
          " prototipos en ",
          sample_name,
          " / ",
          matrix_short,
          "."
        )
      }
      
      km_signature_sizes <- map_int(
        kmeans_profiles$signature,
        length
      )
      
      if (
        any(
          km_signature_sizes < 8L |
          km_signature_sizes > 15L
        )
      ) {
        stop(
          "Algún prototipo Greedy tiene un tamaño fuera de 8:15 en ",
          sample_name,
          " / ",
          matrix_short,
          "."
        )
      }
      
      if (
        any(
          km_signature_sizes !=
          kmeans_profiles$signature_size
        )
      ) {
        stop(
          "center_signature_size no coincide con la firma en ",
          sample_name,
          " / ",
          matrix_short,
          "."
        )
      }
      
      write_output(
        kmeans_profiles %>%
          select(
            -signature
          ),
        current_out_dir,
        "01_kmeans_6_prototypes.csv"
      )
      
      
      # Consenso EFA
      
      cat(
        "  Construyendo consenso EFA...\n"
      )
      
      efa_result <- build_efa_consensus(
        efa_all,
        sample_name,
        matrix_name_current
      )
      
      efa_profiles <-
        efa_result$profiles
      
      if (
        nrow(
          efa_profiles
        ) !=
        N_EFA_FACTORS
      ) {
        stop(
          "Esperaba ",
          N_EFA_FACTORS,
          " factores EFA consenso en ",
          sample_name,
          " / ",
          matrix_short,
          "."
        )
      }
      
      efa_outputs <- list(
        "02_efa_reference_bootstrap_scores.csv" =
          efa_result$reference_scores,
        
        "03_efa_factor_alignment.csv" =
          efa_result$alignment,
        
        "04_efa_consensus_determinants_long.csv" =
          efa_result$consensus_top,
        
        "05_efa_6_consensus_factors.csv" =
          efa_profiles %>%
          select(
            -signature
          )
      )
      
      iwalk(
        efa_outputs,
        ~ write_output(
          .x,
          current_out_dir,
          .y
        )
      )
      
      
      # K-means vs EFA
      
      km_efa_pairwise <- make_pairwise_comparison(
        kmeans_profiles %>%
          select(
            id,
            signature
          ),
        
        efa_profiles %>%
          select(
            id,
            signature
          )
      )
      
      write_output(
        km_efa_pairwise,
        current_out_dir,
        "06_kmeans_vs_efa_pairwise.csv"
      )
      
      # Se mantiene la misma métrica del script 10.
      
      km_efa_matching <- hungarian_matching(
        pairwise =
          km_efa_pairwise,
        
        left_ids =
          kmeans_profiles$id,
        
        right_ids =
          efa_profiles$id,
        
        metric_col =
          "overlap_left_pct"
      ) %>%
        left_join(
          km_efa_pairwise,
          by = c(
            "left_id",
            "right_id"
          )
        ) %>%
        arrange(
          left_id
        )
      
      write_output(
        km_efa_matching,
        current_out_dir,
        "07_kmeans_vs_efa_optimal_matching.csv"
      )
      
      km_efa_summary <- km_efa_matching %>%
        summarise(
          mean_similarity_pct =
            mean(
              overlap_left_pct,
              na.rm = TRUE
            ),
          
          median_similarity_pct =
            median(
              overlap_left_pct,
              na.rm = TRUE
            ),
          
          min_similarity_pct =
            min(
              overlap_left_pct,
              na.rm = TRUE
            ),
          
          max_similarity_pct =
            max(
              overlap_left_pct,
              na.rm = TRUE
            ),
          
          n_matches_ge_75 =
            sum(
              overlap_left_pct >=
                GOOD_SIMILARITY_THRESHOLD,
              na.rm = TRUE
            )
        )
      
      write_output(
        km_efa_summary,
        current_out_dir,
        "08_kmeans_vs_efa_summary.csv"
      )
      
      
      # Expertos
      
      write_output(
        expert_profiles %>%
          select(
            -signature
          ),
        current_out_dir,
        "09_expert_profiles_bin32.csv"
      )
      
      
      # K-means vs expertos
      
      km_expert_pairwise <- make_pairwise_comparison(
        kmeans_profiles %>%
          select(
            id,
            signature
          ),
        
        expert_profiles %>%
          select(
            id,
            signature
          )
      )
      
      write_output(
        km_expert_pairwise,
        current_out_dir,
        "10_kmeans_vs_experts_pairwise.csv"
      )
      
      best_expert_for_km <- km_expert_pairwise %>%
        group_by(
          left_id
        ) %>%
        arrange(
          desc(
            jaccard_pct
          ),
          desc(
            overlap_left_pct
          ),
          desc(
            n_common
          ),
          right_id,
          .by_group = TRUE
        ) %>%
        slice_head(
          n = 1
        ) %>%
        ungroup() %>%
        rename(
          kmeans_prototype =
            left_id,
          
          expert_profile =
            right_id
        )
      
      write_output(
        best_expert_for_km,
        current_out_dir,
        "11_best_expert_for_each_kmeans.csv"
      )
      
      best_km_for_expert <- km_expert_pairwise %>%
        group_by(
          right_id
        ) %>%
        arrange(
          desc(
            jaccard_pct
          ),
          desc(
            overlap_right_pct
          ),
          desc(
            n_common
          ),
          left_id,
          .by_group = TRUE
        ) %>%
        slice_head(
          n = 1
        ) %>%
        ungroup() %>%
        rename(
          expert_profile =
            right_id,
          
          kmeans_prototype =
            left_id
        )
      
      write_output(
        best_km_for_expert,
        current_out_dir,
        "12_best_kmeans_for_each_expert.csv"
      )
      
      km_expert_hungarian <- hungarian_matching(
        pairwise =
          km_expert_pairwise,
        
        left_ids =
          kmeans_profiles$id,
        
        right_ids =
          expert_profiles$id,
        
        metric_col =
          "jaccard_pct"
      )
      
      write_output(
        km_expert_hungarian,
        current_out_dir,
        "13_kmeans_vs_experts_hungarian.csv"
      )
      
      
      # EFA vs expertos
      
      efa_expert_pairwise <- make_pairwise_comparison(
        efa_profiles %>%
          select(
            id,
            signature
          ),
        
        expert_profiles %>%
          select(
            id,
            signature
          )
      )
      
      write_output(
        efa_expert_pairwise,
        current_out_dir,
        "14_efa_vs_experts_pairwise.csv"
      )
      
      best_expert_for_efa <- efa_expert_pairwise %>%
        group_by(
          left_id
        ) %>%
        arrange(
          desc(
            jaccard_pct
          ),
          desc(
            overlap_left_pct
          ),
          desc(
            n_common
          ),
          right_id,
          .by_group = TRUE
        ) %>%
        slice_head(
          n = 1
        ) %>%
        ungroup() %>%
        rename(
          efa_factor =
            left_id,
          
          expert_profile =
            right_id
        )
      
      write_output(
        best_expert_for_efa,
        current_out_dir,
        "15_best_expert_for_each_efa.csv"
      )
      
      
      # Tabla final
      
      final_comparison <- kmeans_profiles %>%
        select(
          kmeans_prototype =
            id,
          
          kmeans_signature_size =
            signature_size,
          
          kmeans_determinants =
            determinants,
          
          incremental_covered_pct,
          
          cumulative_covered_pct
        ) %>%
        left_join(
          km_efa_matching %>%
            transmute(
              kmeans_prototype =
                left_id,
              
              efa_match =
                right_id,
              
              common_km_efa =
                n_common,
              
              similarity_km_efa_pct =
                overlap_left_pct,
              
              jaccard_km_efa_pct =
                jaccard_pct,
              
              common_determinants_km_efa =
                common_determinants
            ),
          by =
            "kmeans_prototype"
        ) %>%
        left_join(
          efa_profiles %>%
            transmute(
              efa_match =
                id,
              
              efa_determinants =
                determinants
            ),
          by =
            "efa_match"
        ) %>%
        left_join(
          best_expert_for_km %>%
            transmute(
              kmeans_prototype,
              
              expert_match =
                expert_profile,
              
              expert_n_active =
                n_right,
              
              common_km_expert =
                n_common,
              
              km_determinants_found_in_expert_pct =
                overlap_left_pct,
              
              expert_determinants_found_in_km_pct =
                overlap_right_pct,
              
              jaccard_km_expert_pct =
                jaccard_pct,
              
              common_determinants_km_expert =
                common_determinants
            ),
          by =
            "kmeans_prototype"
        ) %>%
        left_join(
          expert_profiles %>%
            transmute(
              expert_match =
                id,
              
              expert_determinants =
                determinants
            ),
          by =
            "expert_match"
        )
      
      write_output(
        final_comparison,
        current_out_dir,
        "16_FINAL_kmeans_efa_experts_comparison.csv"
      )
      
      write_output(
        efa_result$alignment_summary,
        current_out_dir,
        "17_efa_alignment_summary.csv"
      )
      
      
      # Figuras
      
      p_km_efa <- ggplot(
        km_efa_pairwise,
        aes(
          x =
            right_id,
          
          y =
            left_id,
          
          fill =
            overlap_left_pct
        )
      ) +
        geom_tile(
          linewidth = 0.5
        ) +
        geom_text(
          aes(
            label =
              paste0(
                round(
                  overlap_left_pct,
                  1
                ),
                "%"
              )
          ),
          size = 3.5
        ) +
        scale_fill_gradient(
          limits = c(
            0,
            100
          ),
          name =
            "Common %"
        ) +
        labs(
          title =
            paste0(
              sample_name,
              " | ",
              matrix_short,
              " | Greedy K-means vs EFA"
            ),
          
          subtitle =
            paste0(
              "6 candidate profiles | D=8:15 pooled | H=",
              GREEDY_H
            ),
          
          x =
            "EFA consensus factor",
          
          y =
            "Greedy K-means prototype"
        ) +
        theme_minimal(
          base_size = 11
        ) +
        theme(
          panel.grid =
            element_blank(),
          
          plot.title =
            element_text(
              face = "bold"
            )
        )
      
      save_plot(
        p_km_efa,
        file.path(
          current_fig_dir,
          "01_kmeans_vs_efa_heatmap.png"
        ),
        width = 8,
        height = 7
      )
      
      p_km_experts <- ggplot(
        km_expert_pairwise,
        aes(
          x =
            right_id,
          
          y =
            left_id,
          
          fill =
            jaccard_pct
        )
      ) +
        geom_tile(
          linewidth = 0.5
        ) +
        geom_text(
          aes(
            label =
              paste0(
                round(
                  jaccard_pct,
                  1
                ),
                "%"
              )
          ),
          size = 3
        ) +
        scale_fill_gradient(
          limits = c(
            0,
            100
          ),
          name =
            "Jaccard %"
        ) +
        labs(
          title =
            paste0(
              sample_name,
              " | ",
              matrix_short,
              " | K-means vs experts"
            ),
          
          subtitle =
            "6 empirical candidate prototypes",
          
          x =
            "Expert archetype",
          
          y =
            "Greedy K-means prototype"
        ) +
        theme_minimal(
          base_size = 10
        ) +
        theme(
          panel.grid =
            element_blank(),
          
          axis.text.x =
            element_text(
              angle = 45,
              hjust = 1
            ),
          
          plot.title =
            element_text(
              face = "bold"
            )
        )
      
      save_plot(
        p_km_experts,
        file.path(
          current_fig_dir,
          "02_kmeans_vs_experts_heatmap.png"
        ),
        width = 12,
        height = 7
      )
      
      p_efa_experts <- ggplot(
        efa_expert_pairwise,
        aes(
          x =
            right_id,
          
          y =
            left_id,
          
          fill =
            jaccard_pct
        )
      ) +
        geom_tile(
          linewidth = 0.5
        ) +
        geom_text(
          aes(
            label =
              paste0(
                round(
                  jaccard_pct,
                  1
                ),
                "%"
              )
          ),
          size = 3
        ) +
        scale_fill_gradient(
          limits = c(
            0,
            100
          ),
          name =
            "Jaccard %"
        ) +
        labs(
          title =
            paste0(
              sample_name,
              " | ",
              matrix_short,
              " | EFA vs experts"
            ),
          
          subtitle =
            "6-factor EFA candidate solution",
          
          x =
            "Expert archetype",
          
          y =
            "EFA consensus factor"
        ) +
        theme_minimal(
          base_size = 10
        ) +
        theme(
          panel.grid =
            element_blank(),
          
          axis.text.x =
            element_text(
              angle = 45,
              hjust = 1
            ),
          
          plot.title =
            element_text(
              face = "bold"
            )
        )
      
      save_plot(
        p_efa_experts,
        file.path(
          current_fig_dir,
          "03_efa_vs_experts_heatmap.png"
        ),
        width = 12,
        height = 7
      )
    }
    
    
    # Leer resultados de disco para agregación global
    
    reference_scores_disk <- read_csv_safe(
      file.path(
        current_out_dir,
        "02_efa_reference_bootstrap_scores.csv"
      )
    )
    
    alignment_disk <- read_csv_safe(
      file.path(
        current_out_dir,
        "03_efa_factor_alignment.csv"
      )
    )
    
    km_efa_matching_disk <- read_csv_safe(
      file.path(
        current_out_dir,
        "07_kmeans_vs_efa_optimal_matching.csv"
      )
    )
    
    km_efa_summary_disk <- read_csv_safe(
      file.path(
        current_out_dir,
        "08_kmeans_vs_efa_summary.csv"
      )
    )
    
    best_expert_km_disk <- read_csv_safe(
      file.path(
        current_out_dir,
        "11_best_expert_for_each_kmeans.csv"
      )
    )
    
    best_expert_efa_disk <- read_csv_safe(
      file.path(
        current_out_dir,
        "15_best_expert_for_each_efa.csv"
      )
    )
    
    final_comparison_disk <- read_csv_safe(
      file.path(
        current_out_dir,
        "16_FINAL_kmeans_efa_experts_comparison.csv"
      )
    )
    
    alignment_summary_disk <- read_csv_safe(
      file.path(
        current_out_dir,
        "17_efa_alignment_summary.csv"
      )
    )
    
    
    # Validaciones de resultados
    
    if (
      nrow(
        km_efa_matching_disk
      ) !=
      N_KMEANS_PROTOTYPES
    ) {
      stop(
        "Resultado KM-EFA incompleto en ",
        sample_name,
        " / ",
        matrix_short,
        "."
      )
    }
    
    if (
      nrow(
        best_expert_km_disk
      ) !=
      N_KMEANS_PROTOTYPES
    ) {
      stop(
        "Resultado KM-expertos incompleto en ",
        sample_name,
        " / ",
        matrix_short,
        "."
      )
    }
    
    if (
      nrow(
        best_expert_efa_disk
      ) !=
      N_EFA_FACTORS
    ) {
      stop(
        "Resultado EFA-expertos incompleto en ",
        sample_name,
        " / ",
        matrix_short,
        "."
      )
    }
    
    if (
      nrow(
        final_comparison_disk
      ) !=
      N_KMEANS_PROTOTYPES
    ) {
      stop(
        "Tabla final incompleta en ",
        sample_name,
        " / ",
        matrix_short,
        "."
      )
    }
    
    reference_bootstrap_disk <- reference_scores_disk %>%
      arrange(
        desc(
          mean_similarity_to_others
        ),
        bootstrap_id
      ) %>%
      slice_head(
        n = 1
      ) %>%
      pull(
        bootstrap_id
      )
    
    n_valid_bootstraps_disk <- n_distinct(
      alignment_disk$bootstrap_id
    )
    
    
    # Añadir contexto a outputs globales
    
    km_efa_matching_list[[run_counter]] <- add_context(
      km_efa_matching_disk,
      sample_name,
      matrix_name_current
    )
    
    best_expert_km_list[[run_counter]] <- add_context(
      best_expert_km_disk,
      sample_name,
      matrix_name_current
    )
    
    best_expert_efa_list[[run_counter]] <- add_context(
      best_expert_efa_disk,
      sample_name,
      matrix_name_current
    )
    
    final_comparison_list[[run_counter]] <- add_context(
      final_comparison_disk,
      sample_name,
      matrix_name_current
    )
    
    efa_alignment_list[[run_counter]] <- add_context(
      alignment_disk,
      sample_name,
      matrix_name_current
    )
    
    
    # Resumen de la combinación
    
    run_summary_list[[run_counter]] <- tibble(
      analysis_sample =
        sample_name,
      
      matrix_name =
        matrix_name_current,
      
      matrix =
        matrix_short,
      
      greedy_d_strategy =
        "D=8:15 pooled",
      
      greedy_h =
        GREEDY_H,
      
      weighting =
        REFERENCE_WEIGHTING,
      
      n_kmeans_prototypes =
        N_KMEANS_PROTOTYPES,
      
      min_kmeans_signature_size =
        min(
          final_comparison_disk$kmeans_signature_size,
          na.rm = TRUE
        ),
      
      max_kmeans_signature_size =
        max(
          final_comparison_disk$kmeans_signature_size,
          na.rm = TRUE
        ),
      
      n_efa_factors =
        N_EFA_FACTORS,
      
      efa_top_n_determinants =
        EFA_TOP_N,
      
      n_valid_efa_bootstraps =
        n_valid_bootstraps_disk,
      
      efa_reference_bootstrap =
        reference_bootstrap_disk,
      
      mean_efa_alignment_similarity =
        alignment_summary_disk$mean_alignment_similarity[1],
      
      median_efa_alignment_similarity =
        alignment_summary_disk$median_alignment_similarity[1],
      
      mean_kmeans_efa_similarity =
        km_efa_summary_disk$mean_similarity_pct[1],
      
      median_kmeans_efa_similarity =
        km_efa_summary_disk$median_similarity_pct[1],
      
      min_kmeans_efa_similarity =
        km_efa_summary_disk$min_similarity_pct[1],
      
      max_kmeans_efa_similarity =
        km_efa_summary_disk$max_similarity_pct[1],
      
      n_kmeans_efa_matches_ge_75 =
        km_efa_summary_disk$n_matches_ge_75[1],
      
      mean_best_kmeans_expert_jaccard =
        mean(
          best_expert_km_disk$jaccard_pct,
          na.rm = TRUE
        ),
      
      max_best_kmeans_expert_jaccard =
        max(
          best_expert_km_disk$jaccard_pct,
          na.rm = TRUE
        ),
      
      mean_best_efa_expert_jaccard =
        mean(
          best_expert_efa_disk$jaccard_pct,
          na.rm = TRUE
        ),
      
      max_best_efa_expert_jaccard =
        max(
          best_expert_efa_disk$jaccard_pct,
          na.rm = TRUE
        ),
      
      coverage_after_6_prototypes =
        max(
          final_comparison_disk$cumulative_covered_pct,
          na.rm = TRUE
        )
    )
    
    cat(
      "  EFA bootstraps válidos: ",
      n_valid_bootstraps_disk,
      "\n",
      
      "  Similitud media KM-EFA: ",
      round(
        km_efa_summary_disk$mean_similarity_pct[1],
        1
      ),
      "%\n",
      
      "  Cobertura Greedy con 6 prototipos: ",
      round(
        max(
          final_comparison_disk$cumulative_covered_pct,
          na.rm = TRUE
        ),
        1
      ),
      "%\n",
      
      sep = ""
    )
    
    invisible(
      gc()
    )
  }
}


# Consolidar resultados globales

run_summary_all <- bind_rows(
  run_summary_list
)

km_efa_matching_all <- bind_rows(
  km_efa_matching_list
)

best_expert_km_all <- bind_rows(
  best_expert_km_list
)

best_expert_efa_all <- bind_rows(
  best_expert_efa_list
)

final_comparison_all <- bind_rows(
  final_comparison_list
)

efa_alignment_all <- bind_rows(
  efa_alignment_list
)


# Comprobaciones finales

expected_n <-
  length(
    ANALYSIS_SAMPLES
  ) *
  length(
    MATRICES_TO_RUN
  )

if (
  nrow(
    run_summary_all
  ) !=
  expected_n
) {
  stop(
    "El resumen global no contiene las 28 combinaciones esperadas."
  )
}

missing_global_grid <- expected_grid %>%
  anti_join(
    run_summary_all %>%
      select(
        analysis_sample,
        matrix_name
      ),
    by = c(
      "analysis_sample",
      "matrix_name"
    )
  )

if (
  nrow(
    missing_global_grid
  )
) {
  print(
    missing_global_grid,
    n = Inf
  )
  
  stop(
    "Faltan combinaciones muestra × matriz en el resultado global."
  )
}


# Parámetros

parameters <- tibble(
  parameter = c(
    "analysis_samples",
    "matrices",
    "weighting",
    "greedy_d_strategy",
    "greedy_h",
    "n_kmeans_prototypes",
    "n_efa_factors",
    "efa_top_n_determinants",
    "max_bootstraps",
    "good_similarity_threshold",
    "greedy_file",
    "efa_file",
    "expert_file",
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
    
    REFERENCE_WEIGHTING,
    
    "D=8:15 pooled",
    
    as.character(
      GREEDY_H
    ),
    
    as.character(
      N_KMEANS_PROTOTYPES
    ),
    
    as.character(
      N_EFA_FACTORS
    ),
    
    as.character(
      EFA_TOP_N
    ),
    
    as.character(
      MAX_BOOTSTRAPS
    ),
    
    as.character(
      GOOD_SIMILARITY_THRESHOLD
    ),
    
    greedy_file,
    
    efa_file,
    
    expert_file,
    
    "NONE"
  )
)


# Guardar outputs globales

global_outputs <- list(
  "00_comparison_summary_all_samples_matrices.csv" =
    run_summary_all,
  
  "00_kmeans_vs_efa_matching_all_samples_matrices.csv" =
    km_efa_matching_all,
  
  "00_best_expert_for_kmeans_all_samples_matrices.csv" =
    best_expert_km_all,
  
  "00_best_expert_for_efa_all_samples_matrices.csv" =
    best_expert_efa_all,
  
  "00_FINAL_comparison_all_samples_matrices.csv" =
    final_comparison_all,
  
  "00_efa_alignment_all_samples_matrices.csv.gz" =
    efa_alignment_all,
  
  "00_parameters.csv" =
    parameters
)

iwalk(
  global_outputs,
  ~ write_output(
    .x,
    out_root,
    .y
  )
)


# Resumen final

cat(
  "\n10b. COMPARACIÓN D-POOLED COMPLETADA\n",
  
  "\nConfiguraciones totales: ",
  nrow(
    run_summary_all
  ),
  
  "\nConfiguraciones reutilizadas: ",
  n_reused,
  
  "\nConfiguraciones calculadas ahora: ",
  n_computed,
  
  "\n\nResumen global:\n\n",
  
  sep = ""
)

print(
  run_summary_all,
  n = Inf,
  width = Inf
)

cat(
  "\nResultados guardados en:\n",
  out_root,
  
  "\n\nFicheros globales principales:\n",
  
  "00_comparison_summary_all_samples_matrices.csv\n",
  
  "00_kmeans_vs_efa_matching_all_samples_matrices.csv\n",
  
  "00_best_expert_for_kmeans_all_samples_matrices.csv\n",
  
  "00_best_expert_for_efa_all_samples_matrices.csv\n",
  
  "00_FINAL_comparison_all_samples_matrices.csv\n",
  
  sep = ""
)

message(
  "\nListo."
)