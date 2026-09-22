# 10c. Sensibilidad H x P de la comparación D-pooled
#
# Objetivo:
# Evaluar la sensibilidad de la comparación entre Greedy-Kmeans,
# Greedy-EFA y arquetipos expertos para diferentes valores de H y P.
#
# Configuración:
# - D=8:15 pooled, ya calculado en 08b.
# - H = 2:8.
# - P = 4:8.
# - weighting = equal_candidate.
# - 7 muestras y 4 matrices.
#
# El script calcula:
# - cobertura Greedy para K-means y EFA;
# - similitud K-means vs EFA;
# - similitud K-means vs expertos;
# - similitud EFA vs expertos;
# - tamaño de las firmas seleccionadas;
# - tablas comparativas y figuras H x P.
#
# No selecciona automáticamente una configuración final.
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

out_dir <- file.path(
  processed_root,
  "10c_sensitivity_H_P_Dpooled"
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

MATRICES_TO_RUN <- c(
  "matrix_32_raw_0_1",
  "matrix_32_pos_0_1",
  "matrix_32_ext_0_1",
  "matrix_32_z_abs"
)

H_GRID <- 2:8

P_GRID <- 4:8

REFERENCE_WEIGHTING <- "equal_candidate"

MATCH_THRESHOLD <- 50


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
    as.numeric(
      x_chr
    )
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
      if (
        length(
          left_set
        )
      ) {
        100 *
          n_common /
          length(
            left_set
          )
      } else {
        NA_real_
      },
    
    overlap_right_pct =
      if (
        length(
          right_set
        )
      ) {
        100 *
          n_common /
          length(
            right_set
          )
      } else {
        NA_real_
      },
    
    jaccard_pct =
      if (
        length(
          union_set
        )
      ) {
        100 *
          n_common /
          length(
            union_set
          )
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
    length(
      left_ids
    ) >
    length(
      right_ids
    )
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
        length(
          value
        ) &&
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


get_greedy_profiles <- function(
    greedy_steps,
    sample_name,
    matrix_name_current,
    method_name,
    h_current,
    p_current,
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
        h_current,
      
      prototype <=
        p_current
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
    p_current
  ) {
    stop(
      "Esperaba ",
      p_current,
      " prototipos ",
      method_name,
      " en ",
      sample_name,
      " / ",
      matrix_label(
        matrix_name_current
      ),
      " / H=",
      h_current,
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
      "Hay firmas fuera de 8:15 en ",
      sample_name,
      " / ",
      matrix_label(
        matrix_name_current
      ),
      " / ",
      method_name,
      " / H=",
      h_current,
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
      "center_signature_size no coincide con la firma en ",
      sample_name,
      " / ",
      matrix_label(
        matrix_name_current
      ),
      " / ",
      method_name,
      " / H=",
      h_current,
      "."
    )
  }
  
  profiles
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


read_csv_safe <- function(path) {
  read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE
  )
}


check_file <- function(path) {
  if (
    !file.exists(
      path
    )
  ) {
    stop(
      "No encuentro el fichero:\n",
      path
    )
  }
}


mean_or_na <- function(x) {
  if (
    all(
      is.na(
        x
      )
    )
  ) {
    return(
      NA_real_
    )
  }
  
  mean(
    x,
    na.rm = TRUE
  )
}


heatmap_scale <- function(name) {
  scale_fill_viridis_c(
    option = "turbo",
    limits = c(
      0,
      100
    ),
    breaks = seq(
      0,
      100,
      by = 20
    ),
    name = name
  )
}


# Comprobar inputs

check_file(
  greedy_file
)

check_file(
  greedy_patterns_file
)

check_file(
  expert_file
)


# Greedy 08b

greedy_steps <- read_csv_safe(
  greedy_file
) %>%
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
    "El fichero contiene d_det. Revisa que estés leyendo el 08b D-pooled."
  )
}

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
    "El 08b debe contener KMEANS y EFA."
  )
}


# Diccionario de determinantes

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


# Comprobar disponibilidad H x P

availability <- greedy_steps %>%
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
    
    d_hamming %in%
      H_GRID
  ) %>%
  group_by(
    analysis_sample,
    method,
    matrix_name,
    d_hamming
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
  
  method =
    c(
      "KMEANS",
      "EFA"
    ),
  
  matrix_name =
    MATRICES_TO_RUN,
  
  d_hamming =
    H_GRID
)

availability_check <- expected_availability %>%
  left_join(
    availability,
    
    by = c(
      "analysis_sample",
      "method",
      "matrix_name",
      "d_hamming"
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
    max(
      P_GRID
    ),
    na.rm = TRUE
  )
) {
  print(
    availability_check,
    n = Inf
  )
  
  stop(
    "No hay suficientes prototipos para alguna combinación H x P."
  )
}

expected_n <-
  length(ANALYSIS_SAMPLES) *
  length(MATRICES_TO_RUN) *
  length(H_GRID) *
  length(P_GRID)


# Sensibilidad H x P

summary_list <- list()

optimal_matches_list <- list()

best_expert_list <- list()

counter <- 0L

for (
  sample_name in
  ANALYSIS_SAMPLES
) {
  for (
    matrix_name_current in
    MATRICES_TO_RUN
  ) {
    matrix_short <- matrix_label(
      matrix_name_current
    )
    
    for (
      h_current in
      H_GRID
    ) {
      for (
        p_current in
        P_GRID
      ) {
        counter <-
          counter + 1L
        
        cat(
          "\n",
          counter,
          "/",
          expected_n,
          " | ",
          sample_name,
          " | ",
          matrix_short,
          " | H=",
          h_current,
          " | P=",
          p_current,
          "\n",
          sep = ""
        )
        
        
        # Greedy-Kmeans
        
        kmeans_profiles <- get_greedy_profiles(
          greedy_steps =
            greedy_steps,
          
          sample_name =
            sample_name,
          
          matrix_name_current =
            matrix_name_current,
          
          method_name =
            "KMEANS",
          
          h_current =
            h_current,
          
          p_current =
            p_current,
          
          id_prefix =
            "KM"
        )
        
        
        # Greedy-EFA
        
        efa_profiles <- get_greedy_profiles(
          greedy_steps =
            greedy_steps,
          
          sample_name =
            sample_name,
          
          matrix_name_current =
            matrix_name_current,
          
          method_name =
            "EFA",
          
          h_current =
            h_current,
          
          p_current =
            p_current,
          
          id_prefix =
            "EFA"
        )
        
        
        # Cobertura
        
        coverage_kmeans <-
          max(
            kmeans_profiles$cumulative_covered_pct,
            na.rm = TRUE
          )
        
        coverage_efa <-
          max(
            efa_profiles$cumulative_covered_pct,
            na.rm = TRUE
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
        
        km_efa_match_overlap <- hungarian_matching(
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
          )
        
        km_efa_match_jaccard <- hungarian_matching(
          pairwise =
            km_efa_pairwise,
          
          left_ids =
            kmeans_profiles$id,
          
          right_ids =
            efa_profiles$id,
          
          metric_col =
            "jaccard_pct"
        ) %>%
          left_join(
            km_efa_pairwise,
            
            by = c(
              "left_id",
              "right_id"
            )
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
        
        km_expert_match <- hungarian_matching(
          pairwise =
            km_expert_pairwise,
          
          left_ids =
            kmeans_profiles$id,
          
          right_ids =
            expert_profiles$id,
          
          metric_col =
            "jaccard_pct"
        ) %>%
          left_join(
            km_expert_pairwise,
            
            by = c(
              "left_id",
              "right_id"
            )
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
            right_id,
            .by_group = TRUE
          ) %>%
          slice_head(
            n = 1
          ) %>%
          ungroup()
        
        
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
        
        efa_expert_match <- hungarian_matching(
          pairwise =
            efa_expert_pairwise,
          
          left_ids =
            efa_profiles$id,
          
          right_ids =
            expert_profiles$id,
          
          metric_col =
            "jaccard_pct"
        ) %>%
          left_join(
            efa_expert_pairwise,
            
            by = c(
              "left_id",
              "right_id"
            )
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
            right_id,
            .by_group = TRUE
          ) %>%
          slice_head(
            n = 1
          ) %>%
          ungroup()
        
        
        # Resumen configuración
        
        summary_list[[counter]] <- tibble(
          analysis_sample =
            sample_name,
          
          matrix_name =
            matrix_name_current,
          
          matrix =
            matrix_short,
          
          h =
            h_current,
          
          p =
            p_current,
          
          coverage_kmeans =
            coverage_kmeans,
          
          coverage_efa =
            coverage_efa,
          
          mean_signature_size_kmeans =
            mean(
              kmeans_profiles$signature_size
            ),
          
          mean_signature_size_efa =
            mean(
              efa_profiles$signature_size
            ),
          
          mean_km_efa_overlap =
            mean(
              km_efa_match_overlap$overlap_left_pct,
              na.rm = TRUE
            ),
          
          median_km_efa_overlap =
            median(
              km_efa_match_overlap$overlap_left_pct,
              na.rm = TRUE
            ),
          
          mean_km_efa_jaccard =
            mean(
              km_efa_match_jaccard$jaccard_pct,
              na.rm = TRUE
            ),
          
          median_km_efa_jaccard =
            median(
              km_efa_match_jaccard$jaccard_pct,
              na.rm = TRUE
            ),
          
          mean_km_expert_jaccard_hungarian =
            mean(
              km_expert_match$jaccard_pct,
              na.rm = TRUE
            ),
          
          mean_km_expert_best_jaccard =
            mean(
              best_expert_for_km$jaccard_pct,
              na.rm = TRUE
            ),
          
          mean_efa_expert_jaccard_hungarian =
            mean(
              efa_expert_match$jaccard_pct,
              na.rm = TRUE
            ),
          
          mean_efa_expert_best_jaccard =
            mean(
              best_expert_for_efa$jaccard_pct,
              na.rm = TRUE
            ),
          
          n_km_efa_overlap_ge_50 =
            sum(
              km_efa_match_overlap$overlap_left_pct >=
                MATCH_THRESHOLD,
              na.rm = TRUE
            ),
          
          n_km_expert_jaccard_ge_50 =
            sum(
              km_expert_match$jaccard_pct >=
                MATCH_THRESHOLD,
              na.rm = TRUE
            ),
          
          n_efa_expert_jaccard_ge_50 =
            sum(
              efa_expert_match$jaccard_pct >=
                MATCH_THRESHOLD,
              na.rm = TRUE
            )
        )
        
        
        # Matching detallado
        
        optimal_matches_list[[counter]] <- bind_rows(
          km_efa_match_overlap %>%
            mutate(
              analysis_sample =
                sample_name,
              
              matrix =
                matrix_short,
              
              h =
                h_current,
              
              p =
                p_current,
              
              comparison =
                "KM_vs_EFA_OVERLAP",
              
              .before = 1
            ),
          
          km_efa_match_jaccard %>%
            mutate(
              analysis_sample =
                sample_name,
              
              matrix =
                matrix_short,
              
              h =
                h_current,
              
              p =
                p_current,
              
              comparison =
                "KM_vs_EFA_JACCARD",
              
              .before = 1
            ),
          
          km_expert_match %>%
            mutate(
              analysis_sample =
                sample_name,
              
              matrix =
                matrix_short,
              
              h =
                h_current,
              
              p =
                p_current,
              
              comparison =
                "KM_vs_EXPERTS",
              
              .before = 1
            ),
          
          efa_expert_match %>%
            mutate(
              analysis_sample =
                sample_name,
              
              matrix =
                matrix_short,
              
              h =
                h_current,
              
              p =
                p_current,
              
              comparison =
                "EFA_vs_EXPERTS",
              
              .before = 1
            )
        )
        
        
        # Mejor experto para cada prototipo
        
        best_expert_list[[counter]] <- bind_rows(
          best_expert_for_km %>%
            mutate(
              analysis_sample =
                sample_name,
              
              matrix =
                matrix_short,
              
              h =
                h_current,
              
              p =
                p_current,
              
              method =
                "KMEANS",
              
              .before = 1
            ),
          
          best_expert_for_efa %>%
            mutate(
              analysis_sample =
                sample_name,
              
              matrix =
                matrix_short,
              
              h =
                h_current,
              
              p =
                p_current,
              
              method =
                "EFA",
              
              .before = 1
            )
        )
      }
    }
  }
}


# Consolidar

sensitivity_summary <- bind_rows(
  summary_list
)

optimal_matches_all <- bind_rows(
  optimal_matches_list
)

best_expert_matches_all <- bind_rows(
  best_expert_list
)


# Comprobaciones

if (
  nrow(
    sensitivity_summary
  ) !=
  expected_n
) {
  stop(
    "Esperaba ",
    expected_n,
    " configuraciones y encuentro ",
    nrow(
      sensitivity_summary
    ),
    "."
  )
}


# Tabla 1: todas las configuraciones

write_csv(
  sensitivity_summary,
  
  file.path(
    out_dir,
    "01_sensitivity_summary_all_samples.csv"
  )
)


# Tabla 2: COMPLETE

complete_summary <- sensitivity_summary %>%
  filter(
    analysis_sample ==
      "COMPLETE"
  ) %>%
  arrange(
    factor(
      matrix,
      levels = c(
        "RAW",
        "POS",
        "EXT",
        "Z_ABS"
      )
    ),
    h,
    p
  )

write_csv(
  complete_summary,
  
  file.path(
    out_dir,
    "02_COMPLETE_H_P_comparison.csv"
  )
)


# Tabla 3: resumen por matriz a través de muestras

matrix_summary <- sensitivity_summary %>%
  group_by(
    matrix,
    h,
    p
  ) %>%
  summarise(
    n_samples =
      n(),
    
    mean_coverage_kmeans =
      mean_or_na(
        coverage_kmeans
      ),
    
    min_coverage_kmeans =
      min(
        coverage_kmeans,
        na.rm = TRUE
      ),
    
    mean_coverage_efa =
      mean_or_na(
        coverage_efa
      ),
    
    min_coverage_efa =
      min(
        coverage_efa,
        na.rm = TRUE
      ),
    
    mean_km_efa_overlap =
      mean_or_na(
        mean_km_efa_overlap
      ),
    
    mean_km_efa_jaccard =
      mean_or_na(
        mean_km_efa_jaccard
      ),
    
    mean_km_expert_jaccard =
      mean_or_na(
        mean_km_expert_jaccard_hungarian
      ),
    
    mean_efa_expert_jaccard =
      mean_or_na(
        mean_efa_expert_jaccard_hungarian
      ),
    
    .groups =
      "drop"
  ) %>%
  arrange(
    factor(
      matrix,
      levels = c(
        "RAW",
        "POS",
        "EXT",
        "Z_ABS"
      )
    ),
    h,
    p
  )

write_csv(
  matrix_summary,
  
  file.path(
    out_dir,
    "03_matrix_H_P_summary_across_samples.csv"
  )
)


# Tabla 4: resumen por muestra

sample_summary <- sensitivity_summary %>%
  group_by(
    analysis_sample,
    h,
    p
  ) %>%
  summarise(
    mean_coverage_kmeans =
      mean_or_na(
        coverage_kmeans
      ),
    
    mean_coverage_efa =
      mean_or_na(
        coverage_efa
      ),
    
    mean_km_efa_overlap =
      mean_or_na(
        mean_km_efa_overlap
      ),
    
    mean_km_efa_jaccard =
      mean_or_na(
        mean_km_efa_jaccard
      ),
    
    mean_km_expert_jaccard =
      mean_or_na(
        mean_km_expert_jaccard_hungarian
      ),
    
    mean_efa_expert_jaccard =
      mean_or_na(
        mean_efa_expert_jaccard_hungarian
      ),
    
    .groups =
      "drop"
  ) %>%
  arrange(
    factor(
      analysis_sample,
      levels =
        ANALYSIS_SAMPLES
    ),
    h,
    p
  )

write_csv(
  sample_summary,
  
  file.path(
    out_dir,
    "04_sample_H_P_summary_across_matrices.csv"
  )
)


# Tabla 5: matching detallado

write_csv(
  optimal_matches_all,
  
  file.path(
    out_dir,
    "05_optimal_matching_pairs_all_H_P.csv"
  )
)


# Tabla 6: mejor experto para cada prototipo

write_csv(
  best_expert_matches_all,
  
  file.path(
    out_dir,
    "06_best_expert_for_each_prototype_all_H_P.csv"
  )
)


# Tabla 7: configuraciones COMPLETE con buena cobertura K-means

complete_candidates <- complete_summary %>%
  filter(
    coverage_kmeans >=
      75
  ) %>%
  arrange(
    factor(
      matrix,
      levels = c(
        "RAW",
        "POS",
        "EXT",
        "Z_ABS"
      )
    ),
    h,
    p
  )

write_csv(
  complete_candidates,
  
  file.path(
    out_dir,
    "07_COMPLETE_configs_KM_coverage_ge75.csv"
  )
)


# Figura 1: cobertura K-means COMPLETE

p_complete_km_coverage <- ggplot(
  complete_summary,
  aes(
    x =
      factor(
        p
      ),
    
    y =
      factor(
        h
      ),
    
    fill =
      coverage_kmeans
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
            coverage_kmeans,
            1
          ),
          "%"
        )
    ),
    size =
      3
  ) +
  facet_wrap(
    ~ matrix,
    nrow =
      1
  ) +
  heatmap_scale("Coverage %") +
  labs(
    title =
      "COMPLETE - Greedy K-means coverage",
    
    subtitle =
      "D=8:15 pooled | H=2:8 | P=4:8",
    
    x =
      "Number of prototypes (P)",
    
    y =
      "Hamming radius (H)"
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
  p_complete_km_coverage,
  
  file.path(
    fig_dir,
    "01_COMPLETE_KMEANS_coverage_H_P.png"
  ),
  
  width =
    14,
  
  height =
    5.5
)


# Figura 2: cobertura EFA COMPLETE

p_complete_efa_coverage <- ggplot(
  complete_summary,
  aes(
    x =
      factor(
        p
      ),
    
    y =
      factor(
        h
      ),
    
    fill =
      coverage_efa
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
            coverage_efa,
            1
          ),
          "%"
        )
    ),
    size =
      3
  ) +
  facet_wrap(
    ~ matrix,
    nrow =
      1
  ) +
  heatmap_scale("Coverage %") +
  labs(
    title =
      "COMPLETE - Greedy EFA coverage",
    
    subtitle =
      "D=8:15 pooled | H=2:8 | P=4:8",
    
    x =
      "Number of prototypes (P)",
    
    y =
      "Hamming radius (H)"
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
  p_complete_efa_coverage,
  
  file.path(
    fig_dir,
    "02_COMPLETE_EFA_coverage_H_P.png"
  ),
  
  width =
    14,
  
  height =
    5.5
)


# Figura 3: K-means vs EFA

p_complete_km_efa <- ggplot(
  complete_summary,
  aes(
    x =
      factor(
        p
      ),
    
    y =
      factor(
        h
      ),
    
    fill =
      mean_km_efa_jaccard
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
            mean_km_efa_jaccard,
            1
          ),
          "%"
        )
    ),
    size =
      3
  ) +
  facet_wrap(
    ~ matrix,
    nrow =
      1
  ) +
  heatmap_scale("Jaccard %") +
  labs(
    title =
      "COMPLETE - Greedy K-means vs Greedy EFA",
    
    subtitle =
      "Symmetric Jaccard matching | D=8:15 pooled",
    
    x =
      "Number of prototypes (P)",
    
    y =
      "Hamming radius (H)"
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
  p_complete_km_efa,
  
  file.path(
    fig_dir,
    "03_COMPLETE_KM_EFA_Jaccard_H_P.png"
  ),
  
  width =
    14,
  
  height =
    5.5
)


# Figura 4: K-means vs expertos

p_complete_km_experts <- ggplot(
  complete_summary,
  aes(
    x =
      factor(
        p
      ),
    
    y =
      factor(
        h
      ),
    
    fill =
      mean_km_expert_jaccard_hungarian
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
            mean_km_expert_jaccard_hungarian,
            1
          ),
          "%"
        )
    ),
    size =
      3
  ) +
  facet_wrap(
    ~ matrix,
    nrow =
      1
  ) +
  heatmap_scale("Jaccard %") +
  labs(
    title =
      "COMPLETE - Greedy K-means vs experts",
    
    subtitle =
      "Hungarian matching | D=8:15 pooled",
    
    x =
      "Number of prototypes (P)",
    
    y =
      "Hamming radius (H)"
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
  p_complete_km_experts,
  
  file.path(
    fig_dir,
    "04_COMPLETE_KM_EXPERTS_H_P.png"
  ),
  
  width =
    14,
  
  height =
    5.5
)


# Figura 5: EFA vs expertos

p_complete_efa_experts <- ggplot(
  complete_summary,
  aes(
    x =
      factor(
        p
      ),
    
    y =
      factor(
        h
      ),
    
    fill =
      mean_efa_expert_jaccard_hungarian
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
            mean_efa_expert_jaccard_hungarian,
            1
          ),
          "%"
        )
    ),
    size =
      3
  ) +
  facet_wrap(
    ~ matrix,
    nrow =
      1
  ) +
  heatmap_scale("Jaccard %") +
  labs(
    title =
      "COMPLETE - Greedy EFA vs experts",
    
    subtitle =
      "Hungarian matching | D=8:15 pooled",
    
    x =
      "Number of prototypes (P)",
    
    y =
      "Hamming radius (H)"
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
  p_complete_efa_experts,
  
  file.path(
    fig_dir,
    "05_COMPLETE_EFA_EXPERTS_H_P.png"
  ),
  
  width =
    14,
  
  height =
    5.5
)


# Figura 6: resumen medio por matriz

matrix_plot_data <- matrix_summary %>%
  select(
    matrix,
    h,
    p,
    mean_coverage_kmeans,
    mean_coverage_efa,
    mean_km_efa_jaccard,
    mean_km_expert_jaccard,
    mean_efa_expert_jaccard
  ) %>%
  pivot_longer(
    cols = c(
      mean_coverage_kmeans,
      mean_coverage_efa,
      mean_km_efa_jaccard,
      mean_km_expert_jaccard,
      mean_efa_expert_jaccard
    ),
    
    names_to =
      "metric",
    
    values_to =
      "value"
  ) %>%
  mutate(
    metric =
      recode(
        metric,
        
        "mean_coverage_kmeans" =
          "K-means coverage",
        
        "mean_coverage_efa" =
          "EFA coverage",
        
        "mean_km_efa_jaccard" =
          "KM-EFA Jaccard",
        
        "mean_km_expert_jaccard" =
          "KM-Experts Jaccard",
        
        "mean_efa_expert_jaccard" =
          "EFA-Experts Jaccard"
      )
  )

p_matrix_summary <- ggplot(
  matrix_plot_data,
  aes(
    x =
      factor(
        p
      ),
    
    y =
      factor(
        h
      ),
    
    fill =
      value
  )
) +
  geom_tile(
    linewidth =
      0.4
  ) +
  geom_text(
    aes(
      label =
        round(
          value,
          1
        )
    ),
    size =
      2.4
  ) +
  facet_grid(
    metric ~ matrix
  ) +
  heatmap_scale("%") +
  labs(
    title =
      "H x P sensitivity averaged across samples",
    
    subtitle =
      "Metrics remain separate; no composite score is calculated",
    
    x =
      "Number of prototypes (P)",
    
    y =
      "Hamming radius (H)"
  ) +
  theme_minimal(
    base_size =
      9
  ) +
  theme(
    panel.grid =
      element_blank()
  )

save_plot(
  p_matrix_summary,
  
  file.path(
    fig_dir,
    "06_matrix_H_P_summary_across_samples.png"
  ),
  
  width =
    15,
  
  height =
    12
)


# Figuras comparativas por muestra

comparison_fig_dir <- file.path(
  fig_dir,
  "comparisons_by_sample"
)

dir.create(
  comparison_fig_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

comparison_metrics <- tribble(
  ~variable, ~title, ~filename_suffix,
  "mean_km_efa_jaccard",
  "Greedy K-means vs Greedy EFA",
  "KM_EFA_Jaccard",
  
  "mean_km_expert_jaccard_hungarian",
  "Greedy K-means vs experts",
  "KM_EXPERTS_Jaccard",
  
  "mean_efa_expert_jaccard_hungarian",
  "Greedy EFA vs experts",
  "EFA_EXPERTS_Jaccard"
)

for (sample_current in ANALYSIS_SAMPLES) {
  
  sample_data <- sensitivity_summary %>%
    filter(
      analysis_sample == sample_current
    ) %>%
    mutate(
      matrix = factor(
        matrix,
        levels = c(
          "RAW",
          "POS",
          "EXT",
          "Z_ABS"
        )
      )
    )
  
  for (metric_i in seq_len(nrow(comparison_metrics))) {
    
    variable_current <- comparison_metrics$variable[metric_i]
    title_current <- comparison_metrics$title[metric_i]
    suffix_current <- comparison_metrics$filename_suffix[metric_i]
    
    plot_data <- sample_data %>%
      transmute(
        analysis_sample,
        matrix,
        h,
        p,
        value = .data[[variable_current]]
      )
    
    p_comparison <- ggplot(
      plot_data,
      aes(
        x = factor(p),
        y = factor(h),
        fill = value
      )
    ) +
      geom_tile(
        linewidth = 0.5
      ) +
      geom_text(
        aes(
          label = paste0(
            round(value, 1),
            "%"
          )
        ),
        size = 3
      ) +
      facet_wrap(
        ~ matrix,
        nrow = 1
      ) +
      heatmap_scale("Jaccard %") +
      labs(
        title = paste0(
          sample_current,
          " - ",
          title_current
        ),
        subtitle = "D=8:15 pooled | H=2:8 | P=4:8 | Hungarian matching",
        x = "Number of prototypes (P)",
        y = "Hamming radius (H)"
      ) +
      theme_minimal(
        base_size = 10
      ) +
      theme(
        panel.grid = element_blank(),
        plot.title = element_text(
          face = "bold"
        ),
        strip.text = element_text(
          face = "bold"
        )
      )
    
    ggsave(
      filename = file.path(
        comparison_fig_dir,
        paste0(
          sample_current,
          "_",
          suffix_current,
          ".png"
        )
      ),
      plot = p_comparison,
      width = 14,
      height = 6,
      dpi = 300,
      bg = "white"
    )
  }
}

cat(
  "\n21 gráficos comparativos guardados en:\n",
  comparison_fig_dir,
  "\n",
  sep = ""
)


# Parámetros

parameters <- tibble(
  parameter = c(
    "analysis_samples",
    "matrices",
    "D_strategy",
    "H_grid",
    "P_grid",
    "weighting",
    "KM_EFA_overlap_metric",
    "KM_EFA_symmetric_metric",
    "KM_EXPERTS_metric",
    "EFA_EXPERTS_metric",
    "greedy_file",
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
    
    "D=8:15 pooled for KMEANS and EFA",
    
    paste(
      H_GRID,
      collapse = ", "
    ),
    
    paste(
      P_GRID,
      collapse = ", "
    ),
    
    REFERENCE_WEIGHTING,
    
    "overlap_left_pct",
    
    "jaccard_pct",
    
    "jaccard_pct",
    
    "jaccard_pct",
    
    greedy_file,
    
    expert_file,
    
    "NONE"
  )
)

write_csv(
  parameters,
  
  file.path(
    out_dir,
    "08_parameters.csv"
  )
)


# Consola

cat(
  "\n10c. SENSIBILIDAD H x P COMPLETADA\n",
  
  "\nConfiguraciones analizadas: ",
  nrow(
    sensitivity_summary
  ),
  
  "\nH: ",
  paste(
    H_GRID,
    collapse = ", "
  ),
  
  "\nP: ",
  paste(
    P_GRID,
    collapse = ", "
  ),
  
  "\n\nCOMPLETE:\n\n",
  
  sep = ""
)

print(
  complete_summary %>%
    select(
      matrix,
      h,
      p,
      coverage_kmeans,
      coverage_efa,
      mean_km_efa_overlap,
      mean_km_efa_jaccard,
      mean_km_expert_jaccard_hungarian,
      mean_efa_expert_jaccard_hungarian
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n\nRESUMEN POR MATRIZ A TRAVÉS DE TODAS LAS MUESTRAS:\n\n"
)

print(
  matrix_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nResultados guardados en:\n",
  out_dir,
  "\n",
  
  "\nTABLAS:\n",
  "01_sensitivity_summary_all_samples.csv\n",
  "02_COMPLETE_H_P_comparison.csv\n",
  "03_matrix_H_P_summary_across_samples.csv\n",
  "04_sample_H_P_summary_across_matrices.csv\n",
  "05_optimal_matching_pairs_all_H_P.csv\n",
  "06_best_expert_for_each_prototype_all_H_P.csv\n",
  "07_COMPLETE_configs_KM_coverage_ge75.csv\n",
  "08_parameters.csv\n",
  
  "\nFIGURAS:\n",
  "01_COMPLETE_KMEANS_coverage_H_P.png\n",
  "02_COMPLETE_EFA_coverage_H_P.png\n",
  "03_COMPLETE_KM_EFA_Jaccard_H_P.png\n",
  "04_COMPLETE_KM_EXPERTS_H_P.png\n",
  "05_COMPLETE_EFA_EXPERTS_H_P.png\n",
  "06_matrix_H_P_summary_across_samples.png\n",
  
  sep = ""
)

message(
  "\nListo."
)
