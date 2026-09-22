# 10b. Comparación Greedy-Kmeans D-pooled, Greedy-EFA D-pooled y expertos
#
# Objetivo:
# Comparar, para las 7 muestras y las 4 matrices, los prototipos obtenidos
# mediante Greedy a partir de K-means y EFA con los arquetipos expertos.
#
# Configuración comparativa provisional:
# - K-means y EFA usan exactamente la misma lógica D-pooled generada en 08b.
# - D=8:15 está integrado en un único pool y no se filtra aquí.
# - H=6 y weighting=equal_candidate para ambos métodos.
# - Se comparan 6 prototipos Greedy-Kmeans y 6 prototipos Greedy-EFA.
# - Se mantiene la lógica del script 10:
#   K-means vs EFA, K-means vs expertos y EFA vs expertos.
# - El matching K-means vs EFA sigue usando overlap_left_pct.
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
processed_root <- file.path("paper1_cluster/data/processed")

greedy_dir <- file.path(
  processed_root,
  "08b_greedy_kmeans_efa_Dpooled"
)

greedy_file <- file.path(
  greedy_dir,
  "07_greedy_prototype_steps_Dpooled.csv"
)

greedy_patterns_file <- file.path(
  greedy_dir,
  "02_pattern_frequency_Dpooled.csv.gz"
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
  "10b_compare_greedy_kmeans_efa_experts_Dpooled_H6"
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
N_EFA_PROTOTYPES <- 6L

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
    str_remove("^det[_\\.-]*\\d+[_\\.-]*") %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_remove("^_+") %>%
    str_remove("_+$")
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
    n_left =
      length(
        left_set
      ),
    
    n_right =
      length(
        right_set
      ),
    
    n_common =
      n_common,
    
    overlap_left_pct =
      if (length(left_set)) {
        100 *
          n_common /
          length(left_set)
      } else {
        NA_real_
      },
    
    overlap_right_pct =
      if (length(right_set)) {
        100 *
          n_common /
          length(right_set)
      } else {
        NA_real_
      },
    
    jaccard_pct =
      if (length(union_set)) {
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
    left_id =
      left$id,
    
    right_id =
      right$id
  ) %>%
    rowwise() %>%
    mutate(
      comparison = list(
        compare_sets(
          left$signature[
            left$id ==
              left_id
          ][[1]],
          
          right$signature[
            right$id ==
              right_id
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
    nrow =
      length(
        left_ids
      ),
    ncol =
      length(
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
      "02_efa_6_prototypes.csv",
      "03_kmeans_vs_efa_pairwise.csv",
      "04_kmeans_vs_efa_optimal_matching.csv",
      "05_kmeans_vs_efa_summary.csv",
      "06_expert_profiles_bin32.csv",
      "07_kmeans_vs_experts_pairwise.csv",
      "08_best_expert_for_each_kmeans.csv",
      "09_best_kmeans_for_each_expert.csv",
      "10_kmeans_vs_experts_hungarian.csv",
      "11_efa_vs_experts_pairwise.csv",
      "12_best_expert_for_each_efa.csv",
      "13_FINAL_kmeans_efa_experts_comparison.csv"
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


get_greedy_profiles <- function(
    greedy_steps,
    sample_name,
    matrix_name_current,
    method_name,
    n_prototypes,
    id_prefix
) {
  profiles <- greedy_steps %>%
    filter(
      analysis_sample ==
        sample_name,
      
      weighting ==
        REFERENCE_WEIGHTING,
      
      method ==
        method_name,
      
      matrix_name ==
        matrix_name_current,
      
      d_hamming ==
        GREEDY_H,
      
      prototype <=
        n_prototypes
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
          id_prefix,
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
      profiles
    ) !=
    n_prototypes
  ) {
    stop(
      "Esperaba ",
      n_prototypes,
      " prototipos ",
      method_name,
      " en ",
      sample_name,
      " / ",
      matrix_label(
        matrix_name_current
      ),
      "."
    )
  }
  
  signature_sizes <- map_int(
    profiles$signature,
    length
  )
  
  if (
    any(
      signature_sizes < 8L |
      signature_sizes > 15L
    )
  ) {
    stop(
      "Algún prototipo ",
      method_name,
      " tiene un tamaño fuera de 8:15 en ",
      sample_name,
      " / ",
      matrix_label(
        matrix_name_current
      ),
      "."
    )
  }
  
  if (
    any(
      signature_sizes !=
      profiles$signature_size
    )
  ) {
    stop(
      "center_signature_size no coincide con la firma ",
      method_name,
      " en ",
      sample_name,
      " / ",
      matrix_label(
        matrix_name_current
      ),
      "."
    )
  }
  
  profiles
}


# Comprobar inputs

required_files <- c(
  greedy_file,
  greedy_patterns_file,
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

if (
  !all(
    c(
      "KMEANS",
      "EFA"
    ) %in%
    unique(
      greedy_steps$method
    )
  )
) {
  stop(
    "El output 08b debe contener method = KMEANS y method = EFA."
  )
}


# Diccionario de los 32 determinantes

greedy_patterns <- read_csv_safe(
  greedy_patterns_file
)

if (
  !"active_determinants" %in%
  names(
    greedy_patterns
  )
) {
  stop(
    "Falta active_determinants en 02_pattern_frequency_Dpooled.csv.gz."
  )
}

canonical_determinants <- greedy_patterns$active_determinants %>%
  map(
    parse_determinants
  ) %>%
  unlist(
    use.names = FALSE
  ) %>%
  unique() %>%
  sort()

if (
  length(
    canonical_determinants
  ) != 32
) {
  stop(
    "Esperaba 32 determinantes y encuentro ",
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
  ~expert_key, ~canonical_key,
  "autarky", "autonomy"
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
    by =
      "determinant_key"
  )

if (
  nrow(
    expert_column_mapping
  ) != 32 ||
  n_distinct(
    expert_column_mapping$determinant
  ) != 32
) {
  missing_from_expert <- canonical_dictionary %>%
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


# Disponibilidad Greedy para ambos métodos

availability_greedy <- greedy_steps %>%
  filter(
    analysis_sample %in%
      ANALYSIS_SAMPLES,
    
    weighting ==
      REFERENCE_WEIGHTING,
    
    method %in%
      c(
        "KMEANS",
        "EFA"
      ),
    
    matrix_name %in%
      MATRICES_TO_RUN,
    
    d_hamming ==
      GREEDY_H
  ) %>%
  group_by(
    analysis_sample,
    matrix_name,
    method
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

expected_availability <- crossing(
  analysis_sample =
    ANALYSIS_SAMPLES,
  
  matrix_name =
    MATRICES_TO_RUN,
  
  method =
    c(
      "KMEANS",
      "EFA"
    )
)

availability_check <- expected_availability %>%
  left_join(
    availability_greedy,
    
    by = c(
      "analysis_sample",
      "matrix_name",
      "method"
    )
  ) %>%
  mutate(
    required_prototypes =
      if_else(
        method ==
          "KMEANS",
        
        N_KMEANS_PROTOTYPES,
        
        N_EFA_PROTOTYPES
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
    availability_check$required_prototypes,
    na.rm = TRUE
  )
) {
  print(
    availability_check,
    n = Inf
  )
  
  stop(
    "No hay suficientes prototipos Greedy para todas las combinaciones."
  )
}


# Contenedores

run_summary_list <- list()

final_comparison_list <- list()

km_efa_matching_list <- list()

best_expert_km_list <- list()

best_expert_efa_list <- list()

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
      
      kmeans_profiles <- get_greedy_profiles(
        greedy_steps =
          greedy_steps,
        
        sample_name =
          sample_name,
        
        matrix_name_current =
          matrix_name_current,
        
        method_name =
          "KMEANS",
        
        n_prototypes =
          N_KMEANS_PROTOTYPES,
        
        id_prefix =
          "KM"
      )
      
      write_output(
        kmeans_profiles %>%
          select(
            -signature
          ),
        
        current_out_dir,
        
        "01_kmeans_6_prototypes.csv"
      )
      
      
      # 6 prototipos Greedy-EFA
      
      efa_profiles <- get_greedy_profiles(
        greedy_steps =
          greedy_steps,
        
        sample_name =
          sample_name,
        
        matrix_name_current =
          matrix_name_current,
        
        method_name =
          "EFA",
        
        n_prototypes =
          N_EFA_PROTOTYPES,
        
        id_prefix =
          "EFA"
      )
      
      write_output(
        efa_profiles %>%
          select(
            -signature
          ),
        
        current_out_dir,
        
        "02_efa_6_prototypes.csv"
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
        "03_kmeans_vs_efa_pairwise.csv"
      )
      
      
      # Se mantiene la misma métrica del script 10
      
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
        "04_kmeans_vs_efa_optimal_matching.csv"
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
        "05_kmeans_vs_efa_summary.csv"
      )
      
      
      # Expertos
      
      write_output(
        expert_profiles %>%
          select(
            -signature
          ),
        
        current_out_dir,
        
        "06_expert_profiles_bin32.csv"
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
        "07_kmeans_vs_experts_pairwise.csv"
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
        "08_best_expert_for_each_kmeans.csv"
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
        "09_best_kmeans_for_each_expert.csv"
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
        "10_kmeans_vs_experts_hungarian.csv"
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
        "11_efa_vs_experts_pairwise.csv"
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
          efa_prototype =
            left_id,
          
          expert_profile =
            right_id
        )
      
      write_output(
        best_expert_for_efa,
        current_out_dir,
        "12_best_expert_for_each_efa.csv"
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
          
          kmeans_incremental_covered_pct =
            incremental_covered_pct,
          
          kmeans_cumulative_covered_pct =
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
              
              efa_determinants_found_in_km_pct =
                overlap_right_pct,
              
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
              
              efa_signature_size =
                signature_size,
              
              efa_determinants =
                determinants,
              
              efa_incremental_covered_pct =
                incremental_covered_pct,
              
              efa_cumulative_covered_pct =
                cumulative_covered_pct
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
        "13_FINAL_kmeans_efa_experts_comparison.csv"
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
              " | Greedy K-means vs Greedy EFA"
            ),
          
          subtitle =
            paste0(
              "6 prototypes per method | D=8:15 pooled | H=",
              GREEDY_H
            ),
          
          x =
            "Greedy-EFA prototype",
          
          y =
            "Greedy-Kmeans prototype"
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
              " | Greedy K-means vs experts"
            ),
          
          subtitle =
            "6 empirical Greedy-Kmeans prototypes",
          
          x =
            "Expert archetype",
          
          y =
            "Greedy-Kmeans prototype"
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
              " | Greedy EFA vs experts"
            ),
          
          subtitle =
            "6 empirical Greedy-EFA prototypes",
          
          x =
            "Expert archetype",
          
          y =
            "Greedy-EFA prototype"
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
    
    km_profiles_disk <- read_csv_safe(
      file.path(
        current_out_dir,
        "01_kmeans_6_prototypes.csv"
      )
    )
    
    efa_profiles_disk <- read_csv_safe(
      file.path(
        current_out_dir,
        "02_efa_6_prototypes.csv"
      )
    )
    
    km_efa_matching_disk <- read_csv_safe(
      file.path(
        current_out_dir,
        "04_kmeans_vs_efa_optimal_matching.csv"
      )
    )
    
    km_efa_summary_disk <- read_csv_safe(
      file.path(
        current_out_dir,
        "05_kmeans_vs_efa_summary.csv"
      )
    )
    
    best_expert_km_disk <- read_csv_safe(
      file.path(
        current_out_dir,
        "08_best_expert_for_each_kmeans.csv"
      )
    )
    
    best_expert_efa_disk <- read_csv_safe(
      file.path(
        current_out_dir,
        "12_best_expert_for_each_efa.csv"
      )
    )
    
    final_comparison_disk <- read_csv_safe(
      file.path(
        current_out_dir,
        "13_FINAL_kmeans_efa_experts_comparison.csv"
      )
    )
    
    
    # Validaciones
    
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
      N_EFA_PROTOTYPES
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
    
    
    # Resumen de la combinación
    
    coverage_kmeans_p6 <- max(
      km_profiles_disk$cumulative_covered_pct,
      na.rm = TRUE
    )
    
    coverage_efa_p6 <- max(
      efa_profiles_disk$cumulative_covered_pct,
      na.rm = TRUE
    )
    
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
          km_profiles_disk$signature_size,
          na.rm = TRUE
        ),
      
      max_kmeans_signature_size =
        max(
          km_profiles_disk$signature_size,
          na.rm = TRUE
        ),
      
      n_efa_prototypes =
        N_EFA_PROTOTYPES,
      
      min_efa_signature_size =
        min(
          efa_profiles_disk$signature_size,
          na.rm = TRUE
        ),
      
      max_efa_signature_size =
        max(
          efa_profiles_disk$signature_size,
          na.rm = TRUE
        ),
      
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
      
      coverage_kmeans_after_6_prototypes =
        coverage_kmeans_p6,
      
      coverage_efa_after_6_prototypes =
        coverage_efa_p6,
      
      coverage_after_6_prototypes =
        coverage_kmeans_p6
    )
    
    cat(
      "  Similitud media Greedy KM-EFA: ",
      round(
        km_efa_summary_disk$mean_similarity_pct[1],
        1
      ),
      "%\n",
      
      "  Cobertura K-means con 6 prototipos: ",
      round(
        coverage_kmeans_p6,
        1
      ),
      "%\n",
      
      "  Cobertura EFA con 6 prototipos: ",
      round(
        coverage_efa_p6,
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


# Comprobaciones finales

expected_grid <- crossing(
  analysis_sample =
    ANALYSIS_SAMPLES,
  
  matrix_name =
    MATRICES_TO_RUN
)

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
    "n_efa_prototypes",
    "good_similarity_threshold",
    "greedy_file",
    "greedy_patterns_file",
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
    
    "D=8:15 pooled for KMEANS and EFA",
    
    as.character(
      GREEDY_H
    ),
    
    as.character(
      N_KMEANS_PROTOTYPES
    ),
    
    as.character(
      N_EFA_PROTOTYPES
    ),
    
    as.character(
      GOOD_SIMILARITY_THRESHOLD
    ),
    
    greedy_file,
    
    greedy_patterns_file,
    
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
  "\n10b. COMPARACIÓN GREEDY D-POOLED COMPLETADA\n",
  
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
