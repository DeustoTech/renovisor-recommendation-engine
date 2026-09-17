
# Objetivo -- Análisis de sensibilidad de D
#
# Construir prototipos representativos a partir de las estructuras
# obtenidas con K-means y EFA a lo largo de los bootstraps.
#
# El análisis se realiza para COMPLETE, EUROPE, LATAM y cada submuestra.
#
# K-means:
# - K = 4:8 genera los clusters candidatos.
#
# EFA:
# - F = 4:8 genera los factores candidatos.
#
# Cada cluster/factor se transforma en una firma binaria de 32
# determinantes seleccionando D = 8:15 determinantes.
#
# Greedy se ejecuta con radios Hamming pares entre 0 y 10.
#
# Se consideran dos ponderaciones:
# - equal_candidate: cada K/F aporta el mismo peso total.
# - equal_element: cada cluster/factor aporta el mismo peso.
#
# K=2 y K=3 se mantienen como diagnóstico en el bloque 06,
# pero no forman parte del universo principal Greedy.
#
# El número de prototipos Greedy es independiente del K/F utilizado
# para generar los clusters o factores candidatos.

suppressPackageStartupMessages({
  library(tidyverse)
})

set.seed(123)

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

efa_file <- file.path(
  processed_root,
  "07_efa_bootstrap",
  "03_efa_loadings_long.csv.gz"
)

out_dir <- file.path(
  processed_root,
  "08_greedy_kmeans_efa"
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

KMEANS_POOL_GRID <- 4:8
EFA_POOL_GRID <- 4:8

D_DET_GRID <- 8:15
D_HAMMING_GRID <- seq(
  0,
  10,
  by = 2
)

MAX_BOOTSTRAPS <- 100L
MAX_GREEDY_PROTOTYPES <- 12L

COVERAGE_THRESHOLDS <- c(
  80,
  85,
  90,
  95
)

POOL_WEIGHTINGS <- c(
  "equal_candidate",
  "equal_element"
)

PRIMARY_WEIGHTING <- "equal_candidate"


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

patterns_to_matrix <- function(
    keys,
    determinant_names
) {
  mat <- do.call(
    rbind,
    lapply(
      strsplit(
        keys,
        ""
      ),
      as.integer
    )
  )
  
  colnames(mat) <- determinant_names
  mat
}

hamming_distance <- function(
    center,
    mat
) {
  rowSums(
    sweep(
      mat,
      2,
      center,
      FUN = "!="
    )
  )
}

run_greedy_sequence <- function(
    patterns,
    frequencies,
    max_prototypes,
    d_hamming
) {
  used <- rep(
    FALSE,
    nrow(patterns)
  )
  
  steps <- vector(
    "list",
    max_prototypes
  )
  
  cumulative_frequency <- 0
  
  for (prototype_id in seq_len(max_prototypes)) {
    if (all(used)) {
      break
    }
    
    candidates <- which(
      !used
    )
    
    # El siguiente prototipo se centra en el patrón todavía no cubierto
    # con mayor peso/frecuencia.
    center_idx <- candidates[
      which.max(
        frequencies[candidates]
      )
    ]
    
    center <- patterns[
      center_idx,
      ,
      drop = TRUE
    ]
    
    free_idx <- which(
      !used
    )
    
    free_patterns <- patterns[
      free_idx,
      ,
      drop = FALSE
    ]
    
    free_distances <- hamming_distance(
      center,
      free_patterns
    )
    
    # La bola del prototipo incluye los patrones no cubiertos cuya
    # distancia Hamming sea menor o igual al radio actual.
    selected_local <- which(
      free_distances <=
        d_hamming
    )
    
    selected <- free_idx[
      selected_local
    ]
    
    selected_distances <- free_distances[
      selected_local
    ]
    
    incremental_frequency <- sum(
      frequencies[selected],
      na.rm = TRUE
    )
    
    cumulative_frequency <-
      cumulative_frequency +
      incremental_frequency
    
    used[selected] <- TRUE
    
    member_matrix <- patterns[
      selected,
      ,
      drop = FALSE
    ]
    
    member_weights <- frequencies[
      selected
    ]
    
    weighted_member_matrix <- sweep(
      member_matrix,
      1,
      member_weights,
      FUN = "*"
    )
    
    # Prevalencia ponderada de cada determinante dentro de la bola
    # cubierta por el prototipo.
    determinant_prevalence <- colSums(
      weighted_member_matrix,
      na.rm = TRUE
    ) /
      sum(
        member_weights,
        na.rm = TRUE
      )
    
    weighted_mean_hamming <- weighted.mean(
      selected_distances,
      w = member_weights
    )
    
    steps[[prototype_id]] <- list(
      prototype = prototype_id,
      center_index = center_idx,
      center = center,
      members = selected,
      center_frequency = frequencies[center_idx],
      incremental_frequency = incremental_frequency,
      cumulative_frequency = cumulative_frequency,
      determinant_prevalence = determinant_prevalence,
      n_unique_patterns_in_ball = length(selected),
      weighted_mean_hamming = weighted_mean_hamming,
      max_hamming_in_ball = max(selected_distances)
    )
  }
  
  steps
}


# Leer EFA
if (!file.exists(efa_file)) {
  stop(
    "No encuentro el archivo EFA: ",
    efa_file
  )
}

efa_all <- read_csv(
  efa_file,
  show_col_types = FALSE
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
  names(efa_all)
)

if (length(missing_efa)) {
  stop(
    "Faltan columnas en EFA: ",
    paste(
      missing_efa,
      collapse = ", "
    )
  )
}

efa_all <- efa_all %>%
  mutate(
    bootstrap_id = as.integer(
      bootstrap_id
    ),
    
    n_factors = as.integer(
      n_factors
    ),
    
    determinant = as.character(
      determinant
    ),
    
    factor = as.character(
      factor
    ),
    
    loading = as.numeric(
      loading
    )
  ) %>%
  filter(
    analysis_sample %in%
      ANALYSIS_SAMPLES,
    
    matrix_name %in%
      MATRICES_TO_RUN,
    
    n_factors %in%
      EFA_POOL_GRID,
    
    bootstrap_id <=
      MAX_BOOTSTRAPS
  )

boot_ids <- seq_len(
  MAX_BOOTSTRAPS
)

determinants <- sort(
  unique(
    efa_all$determinant
  )
)

if (length(determinants) != 32) {
  stop(
    "Esperaba 32 determinantes en EFA y encuentro ",
    length(determinants),
    "."
  )
}


# Interpretación de los radios Hamming.
#
# Cada patrón tiene exactamente D determinantes activos.
# Por ello, la distancia máxima posible entre dos firmas depende de D.
# Se calcula también el número mínimo de determinantes comunes que
# implica cada combinación D × Hamming.
hamming_interpretation <- crossing(
  d_det = D_DET_GRID,
  d_hamming = D_HAMMING_GRID
) %>%
  mutate(
    max_possible_hamming =
      2 *
      pmin(
        d_det,
        32 - d_det
      ),
    
    effective_hamming_radius =
      pmin(
        d_hamming,
        max_possible_hamming
      ),
    
    min_common_determinants =
      d_det -
      effective_hamming_radius /
      2,
    
    min_common_pct =
      100 *
      min_common_determinants /
      d_det,
    
    radius_can_cover_any_pattern =
      d_hamming >=
      max_possible_hamming
  )


# Contenedores de resultados

binary_patterns_list <- list()
pattern_frequency_list <- list()
prototype_steps_list <- list()
ball_prevalence_list <- list()
coverage_summary_list <- list()
score_rank_summary_list <- list()
candidate_contribution_list <- list()

sample_counter <- 0L
greedy_counter <- 0L


# Procesar cada muestra de análisis
for (sample_name in ANALYSIS_SAMPLES) {
  sample_counter <-
    sample_counter + 1L
  
  cat(
    "\nMUESTRA: ",
    sample_name,
    "\n",
    sep = ""
  )
  
  kmeans_file <- file.path(
    kmeans_dir,
    sample_name,
    "kmeans_centers_long.csv.gz"
  )
  
  if (!file.exists(kmeans_file)) {
    stop(
      "No encuentro K-means para ",
      sample_name,
      ": ",
      kmeans_file
    )
  }
  
  kmeans_sample <- read_csv(
    kmeans_file,
    show_col_types = FALSE
  )
  
  required_kmeans <- c(
    "matrix_name",
    "bootstrap_id",
    "k",
    "cluster_rank",
    "determinant",
    "center_value"
  )
  
  missing_kmeans <- setdiff(
    required_kmeans,
    names(kmeans_sample)
  )
  
  if (length(missing_kmeans)) {
    stop(
      "Faltan columnas K-means en ",
      sample_name,
      ": ",
      paste(
        missing_kmeans,
        collapse = ", "
      )
    )
  }
  
  kmeans_sample <- kmeans_sample %>%
    mutate(
      analysis_sample =
        sample_name,
      
      bootstrap_id = as.integer(
        bootstrap_id
      ),
      
      k = as.integer(
        k
      ),
      
      cluster_rank = as.integer(
        cluster_rank
      ),
      
      determinant = as.character(
        determinant
      ),
      
      center_value = as.numeric(
        center_value
      )
    ) %>%
    filter(
      matrix_name %in%
        MATRICES_TO_RUN,
      
      k %in%
        KMEANS_POOL_GRID,
      
      bootstrap_id %in%
        boot_ids
    )
  
  efa_sample <- efa_all %>%
    filter(
      analysis_sample ==
        sample_name,
      
      bootstrap_id %in%
        boot_ids
    )
  
  if (!nrow(kmeans_sample)) {
    stop(
      "No quedan resultados K-means para ",
      sample_name,
      "."
    )
  }
  
  if (!nrow(efa_sample)) {
    stop(
      "No quedan resultados EFA para ",
      sample_name,
      "."
    )
  }
  
  
  # Comprobar disponibilidad de los bootstraps
  kmeans_boots <- sort(
    unique(
      kmeans_sample$bootstrap_id
    )
  )
  
  efa_boots <- sort(
    unique(
      efa_sample$bootstrap_id
    )
  )
  
  if (
    length(
      setdiff(
        boot_ids,
        kmeans_boots
      )
    )
  ) {
    stop(
      "Faltan bootstraps K-means en ",
      sample_name,
      "."
    )
  }
  
  if (
    length(
      setdiff(
        boot_ids,
        efa_boots
      )
    )
  ) {
    warning(
      "Hay bootstraps sin cargas EFA en ",
      sample_name,
      ". Se trabajará con los ajustes EFA disponibles."
    )
  }
  
  
  # Scores de los determinantes para K-means.
  #
  # RAW:
  # distancia absoluta respecto al punto neutral 0.5.
  #
  # POS:
  # solo se considera la desviación positiva respecto a 0.5.
  #
  # EXT y Z_ABS:
  # el propio centro ya representa intensidad/extremidad
  kmeans_scores <- kmeans_sample %>%
    mutate(
      score = case_when(
        matrix_name ==
          "matrix_32_raw_0_1" ~
          abs(
            center_value - 0.5
          ),
        
        matrix_name ==
          "matrix_32_pos_0_1" ~
          pmax(
            center_value - 0.5,
            0
          ),
        
        matrix_name ==
          "matrix_32_ext_0_1" ~
          center_value,
        
        matrix_name ==
          "matrix_32_z_abs" ~
          center_value,
        
        TRUE ~
          NA_real_
      )
    ) %>%
    transmute(
      analysis_sample,
      method = "KMEANS",
      matrix_name,
      bootstrap_id,
      source_candidate = k,
      
      element_id = paste0(
        "K",
        k,
        "_cluster_",
        cluster_rank
      ),
      
      determinant,
      score
    )
  
  
  # En EFA se utiliza el valor absoluto de la carga factorial
  efa_scores <- efa_sample %>%
    transmute(
      analysis_sample,
      method = "EFA",
      matrix_name,
      bootstrap_id,
      source_candidate = n_factors,
      
      element_id = paste0(
        "F",
        n_factors,
        "_",
        factor
      ),
      
      determinant,
      score = abs(
        loading
      )
    )
  
  scores <- bind_rows(
    kmeans_scores,
    efa_scores
  )
  
  key_cols <- c(
    "analysis_sample",
    "method",
    "matrix_name",
    "bootstrap_id",
    "source_candidate",
    "element_id"
  )
  
  
  # Algunos ajustes EFA pueden no devolver los 32 determinantes.
  # Los determinantes ausentes se completan con score = 0 para poder
  # construir firmas binarias comparables
  element_counts <- scores %>%
    count(
      across(
        all_of(
          key_cols
        )
      ),
      name = "n_determinants"
    )
  
  deficient_elements <- element_counts %>%
    filter(
      n_determinants < 32
    ) %>%
    select(
      all_of(
        key_cols
      )
    )
  
  if (nrow(deficient_elements)) {
    missing_scores <- deficient_elements %>%
      crossing(
        determinant =
          determinants
      ) %>%
      anti_join(
        scores %>%
          select(
            all_of(
              key_cols
            ),
            determinant
          ),
        by = c(
          key_cols,
          "determinant"
        )
      ) %>%
      mutate(
        score = 0
      )
    
    scores <- bind_rows(
      scores,
      missing_scores
    )
  }
  
  check_counts <- scores %>%
    count(
      across(
        all_of(
          key_cols
        )
      ),
      name = "n_determinants"
    )
  
  if (
    any(
      check_counts$n_determinants <
      max(D_DET_GRID)
    )
  ) {
    stop(
      "Hay elementos con menos determinantes de los necesarios en ",
      sample_name,
      "."
    )
  }
  
  
  # Diagnóstico del score según posición en el ranking de determinantes
  score_rank_summary <- scores %>%
    group_by(
      across(
        all_of(
          key_cols
        )
      )
    ) %>%
    arrange(
      desc(score),
      determinant,
      .by_group = TRUE
    ) %>%
    mutate(
      score_rank =
        row_number()
    ) %>%
    ungroup() %>%
    group_by(
      analysis_sample,
      method,
      matrix_name,
      score_rank
    ) %>%
    summarise(
      mean_score = mean(
        score,
        na.rm = TRUE
      ),
      
      median_score = median(
        score,
        na.rm = TRUE
      ),
      
      q25_score = quantile(
        score,
        0.25,
        na.rm = TRUE
      ),
      
      q75_score = quantile(
        score,
        0.75,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    )
  
  score_rank_summary_list[[sample_counter]] <- score_rank_summary
  
  
  # Construir firmas binarias.
  #
  # Para cada cluster/factor y cada D se seleccionan los D determinantes
  # con mayor score. La firma binaria contiene:
  # 1 = determinante seleccionado
  # 0 = determinante no seleccionado
  binary_patterns <- map_dfr(
    D_DET_GRID,
    function(d_det_current) {
      scores %>%
        group_by(
          across(
            all_of(
              key_cols
            )
          )
        ) %>%
        arrange(
          desc(score),
          determinant,
          .by_group = TRUE
        ) %>%
        slice_head(
          n = d_det_current
        ) %>%
        summarise(
          d_det =
            d_det_current,
          
          active_determinants = paste(
            sort(
              determinant
            ),
            collapse = "; "
          ),
          
          pattern_key = paste0(
            as.integer(
              determinants %in%
                determinant
            ),
            collapse = ""
          ),
          
          mean_selected_score = mean(
            score,
            na.rm = TRUE
          ),
          
          min_selected_score = min(
            score,
            na.rm = TRUE
          ),
          
          max_selected_score = max(
            score,
            na.rm = TRUE
          ),
          
          .groups = "drop"
        )
    }
  )
  
  binary_patterns_list[[sample_counter]] <- binary_patterns
  
  
  # Ejecutar las dos formas de ponderación del pool
  for (weighting_current in POOL_WEIGHTINGS) {
    binary_weighted <- binary_patterns %>%
      mutate(
        weighting =
          weighting_current,
        
        # equal_element:
        # cada cluster/factor tiene peso 1.
        #
        # equal_candidate:
        # un candidato K/F contiene K/F elementos, por lo que cada uno
        # recibe peso 1/K o 1/F y el candidato completo suma peso 1.
        pattern_weight = case_when(
          weighting_current ==
            "equal_element" ~
            1,
          
          weighting_current ==
            "equal_candidate" ~
            1 /
            source_candidate,
          
          TRUE ~
            NA_real_
        )
      )
    
    
    # Frecuencia/peso de cada firma binaria dentro de cada combinación
    pattern_frequency <- binary_weighted %>%
      group_by(
        analysis_sample,
        method,
        matrix_name,
        d_det,
        weighting,
        pattern_key,
        active_determinants
      ) %>%
      summarise(
        n_occurrences = n(),
        
        weight = sum(
          pattern_weight,
          na.rm = TRUE
        ),
        
        .groups = "drop"
      ) %>%
      group_by(
        analysis_sample,
        method,
        matrix_name,
        d_det,
        weighting
      ) %>%
      mutate(
        total_pattern_occurrences = sum(
          n_occurrences
        ),
        
        total_weight = sum(
          weight
        ),
        
        frequency_pct =
          100 *
          weight /
          total_weight
      ) %>%
      ungroup()
    
    pattern_frequency_list[[length(pattern_frequency_list) + 1L]] <- pattern_frequency
    
    # Diagnóstico de cuánto aporta cada K/F al pool bajo cada ponderación.
    candidate_contribution <- binary_weighted %>%
      group_by(
        analysis_sample,
        method,
        matrix_name,
        d_det,
        weighting,
        source_candidate
      ) %>%
      summarise(
        n_elements = n(),
        
        total_weight = sum(
          pattern_weight,
          na.rm = TRUE
        ),
        
        .groups = "drop"
      ) %>%
      group_by(
        analysis_sample,
        method,
        matrix_name,
        d_det,
        weighting
      ) %>%
      mutate(
        weight_pct =
          100 *
          total_weight /
          sum(total_weight)
      ) %>%
      ungroup()
    
    candidate_contribution_list[[length(candidate_contribution_list) + 1L]] <- candidate_contribution
    
    
    # Cada combinación método × matriz × D × weighting constituye
    # un pool independiente para Greedy
    groups <- pattern_frequency %>%
      distinct(
        analysis_sample,
        method,
        matrix_name,
        d_det,
        weighting
      )
    
    for (g in seq_len(nrow(groups))) {
      group_current <- groups[
        g,
      ]
      
      pattern_df <- pattern_frequency %>%
        filter(
          analysis_sample ==
            group_current$analysis_sample,
          
          method ==
            group_current$method,
          
          matrix_name ==
            group_current$matrix_name,
          
          d_det ==
            group_current$d_det,
          
          weighting ==
            group_current$weighting
        ) %>%
        arrange(
          desc(weight),
          pattern_key
        )
      
      pattern_matrix <- patterns_to_matrix(
        pattern_df$pattern_key,
        determinants
      )
      
      frequencies <-
        pattern_df$weight
      
      total_frequency <- sum(
        frequencies
      )
      
      n_unique_patterns <- nrow(
        pattern_df
      )
      
      
      # Ejecutar Greedy para cada radio Hamming
      for (d_hamming_current in D_HAMMING_GRID) {
        greedy_counter <-
          greedy_counter + 1L
        
        result_steps <- run_greedy_sequence(
          patterns =
            pattern_matrix,
          
          frequencies =
            frequencies,
          
          max_prototypes =
            MAX_GREEDY_PROTOTYPES,
          
          d_hamming =
            d_hamming_current
        )
        
        
        # Evolución de cobertura prototipo a prototipo
        steps_df <- map_dfr(
          result_steps,
          function(step) {
            if (is.null(step)) {
              return(NULL)
            }
            
            active_center <- determinants[
              step$center == 1
            ]
            
            tibble(
              analysis_sample =
                group_current$analysis_sample,
              
              weighting =
                group_current$weighting,
              
              method =
                group_current$method,
              
              matrix_name =
                group_current$matrix_name,
              
              d_det =
                group_current$d_det,
              
              d_hamming =
                d_hamming_current,
              
              prototype =
                step$prototype,
              
              center_pattern_key = paste0(
                step$center,
                collapse = ""
              ),
              
              center_active_determinants = paste(
                active_center,
                collapse = "; "
              ),
              
              center_weight =
                step$center_frequency,
              
              center_weight_pct =
                100 *
                step$center_frequency /
                total_frequency,
              
              n_unique_patterns_in_ball =
                step$n_unique_patterns_in_ball,
              
              incremental_covered_weight =
                step$incremental_frequency,
              
              incremental_covered_pct =
                100 *
                step$incremental_frequency /
                total_frequency,
              
              cumulative_covered_weight =
                step$cumulative_frequency,
              
              cumulative_covered_pct =
                100 *
                step$cumulative_frequency /
                total_frequency,
              
              weighted_mean_hamming =
                step$weighted_mean_hamming,
              
              max_hamming_in_ball =
                step$max_hamming_in_ball,
              
              total_weight =
                total_frequency,
              
              n_unique_patterns =
                n_unique_patterns,
              
              n_bootstraps =
                length(boot_ids)
            )
          }
        )
        
        prototype_steps_list[[greedy_counter]] <- steps_df
        
        
        # Prevalencia de cada determinante dentro de la bola cubierta
        # por cada prototipo
        ball_df <- map_dfr(
          result_steps,
          function(step) {
            if (is.null(step)) {
              return(NULL)
            }
            
            tibble(
              analysis_sample =
                group_current$analysis_sample,
              
              weighting =
                group_current$weighting,
              
              method =
                group_current$method,
              
              matrix_name =
                group_current$matrix_name,
              
              d_det =
                group_current$d_det,
              
              d_hamming =
                d_hamming_current,
              
              prototype =
                step$prototype,
              
              determinant =
                determinants,
              
              center_selected =
                as.integer(
                  step$center
                ),
              
              prop_active_in_ball =
                as.numeric(
                  step$determinant_prevalence
                ),
              
              pct_active_in_ball =
                100 *
                prop_active_in_ball,
              
              covered_weight =
                step$incremental_frequency,
              
              n_unique_patterns_in_ball =
                step$n_unique_patterns_in_ball
            )
          }
        )
        
        ball_prevalence_list[[greedy_counter]] <- ball_df
        
        
        # Resumen final de cobertura para esta combinación.
        if (nrow(steps_df)) {
          last_step <- steps_df %>%
            slice_tail(
              n = 1
            )
          
          coverage_summary_list[[greedy_counter]] <- tibble(
            analysis_sample =
              group_current$analysis_sample,
            
            weighting =
              group_current$weighting,
            
            method =
              group_current$method,
            
            matrix_name =
              group_current$matrix_name,
            
            d_det =
              group_current$d_det,
            
            d_hamming =
              d_hamming_current,
            
            n_prototypes_used = max(
              steps_df$prototype
            ),
            
            n_bootstraps =
              length(boot_ids),
            
            n_unique_patterns =
              n_unique_patterns,
            
            final_covered_pct =
              last_step$cumulative_covered_pct,
            
            first_prototype_covered_pct =
              steps_df$incremental_covered_pct[1],
            
            last_prototype_increment_pct =
              last_step$incremental_covered_pct
          )
        }
      }
    }
  }
  
  rm(
    kmeans_sample,
    efa_sample,
    kmeans_scores,
    efa_scores,
    scores,
    binary_patterns
  )
  
  invisible(
    gc()
  )
}


# Consolidar resultados de todas las muestras
binary_patterns_all <- bind_rows(
  binary_patterns_list
)

pattern_frequency_all <- bind_rows(
  pattern_frequency_list
)

greedy_prototype_steps <- bind_rows(
  prototype_steps_list
)

greedy_ball_determinant_prevalence <- bind_rows(
  ball_prevalence_list
)

greedy_coverage_summary <- bind_rows(
  coverage_summary_list
)

score_rank_summary <- bind_rows(
  score_rank_summary_list
)

source_candidate_contribution <- bind_rows(
  candidate_contribution_list
)


# Añadir interpretación del radio Hamming a los resultados principales
greedy_prototype_steps <- greedy_prototype_steps %>%
  left_join(
    hamming_interpretation,
    by = c(
      "d_det",
      "d_hamming"
    )
  )

greedy_ball_determinant_prevalence <-
  greedy_ball_determinant_prevalence %>%
  left_join(
    hamming_interpretation,
    by = c(
      "d_det",
      "d_hamming"
    )
  )

greedy_coverage_summary <- greedy_coverage_summary %>%
  left_join(
    hamming_interpretation,
    by = c(
      "d_det",
      "d_hamming"
    )
  )


# Curva prototipo-cobertura.
# La mejora marginal de cada prototipo coincide con su cobertura incremental
greedy_prototype_curve <- greedy_prototype_steps %>%
  mutate(
    marginal_gain_pct =
      incremental_covered_pct
  ) %>%
  arrange(
    analysis_sample,
    weighting,
    method,
    matrix_name,
    d_det,
    d_hamming,
    prototype
  )


# Número mínimo de prototipos necesarios para alcanzar cada
# umbral de cobertura
coverage_threshold_table <- greedy_prototype_steps %>%
  select(
    analysis_sample,
    weighting,
    method,
    matrix_name,
    d_det,
    d_hamming,
    prototype,
    cumulative_covered_pct
  ) %>%
  crossing(
    coverage_threshold =
      COVERAGE_THRESHOLDS
  ) %>%
  group_by(
    analysis_sample,
    weighting,
    method,
    matrix_name,
    d_det,
    d_hamming,
    coverage_threshold
  ) %>%
  summarise(
    first_prototype_reaching_threshold = if (
      any(
        cumulative_covered_pct >=
        coverage_threshold,
        na.rm = TRUE
      )
    ) {
      min(
        prototype[
          cumulative_covered_pct >=
            coverage_threshold
        ]
      )
    } else {
      NA_integer_
    },
    
    max_coverage_available = max(
      cumulative_covered_pct,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  left_join(
    hamming_interpretation,
    by = c(
      "d_det",
      "d_hamming"
    )
  )


# Tabla ancha para comparar directamente matrices y métodos
greedy_matrix_x_method <- greedy_prototype_steps %>%
  mutate(
    matrix_short = matrix_label(
      matrix_name
    ),
    
    method_matrix = paste0(
      method,
      "_",
      matrix_short
    )
  ) %>%
  select(
    analysis_sample,
    weighting,
    d_det,
    d_hamming,
    prototype,
    method_matrix,
    cumulative_covered_pct
  ) %>%
  pivot_wider(
    names_from =
      method_matrix,
    
    values_from =
      cumulative_covered_pct
  ) %>%
  arrange(
    analysis_sample,
    weighting,
    d_det,
    d_hamming,
    prototype
  )


# Parámetros utilizados
parameters <- tibble(
  parameter = c(
    "analysis_samples",
    "matrices",
    "kmeans_pool_grid",
    "efa_pool_grid",
    "d_det_grid",
    "d_hamming_grid",
    "max_bootstraps",
    "max_greedy_prototypes",
    "coverage_thresholds",
    "pool_weightings",
    "primary_weighting"
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
      KMEANS_POOL_GRID,
      collapse = ", "
    ),
    
    paste(
      EFA_POOL_GRID,
      collapse = ", "
    ),
    
    paste(
      D_DET_GRID,
      collapse = ", "
    ),
    
    paste(
      D_HAMMING_GRID,
      collapse = ", "
    ),
    
    as.character(
      MAX_BOOTSTRAPS
    ),
    
    as.character(
      MAX_GREEDY_PROTOTYPES
    ),
    
    paste(
      COVERAGE_THRESHOLDS,
      collapse = ", "
    ),
    
    paste(
      POOL_WEIGHTINGS,
      collapse = ", "
    ),
    
    PRIMARY_WEIGHTING
  )
)


# Guardar resultados
outputs <- list(
  "01_binary_patterns_pooled.csv.gz" =
    binary_patterns_all,
  
  "02_pattern_frequency_pooled.csv.gz" =
    pattern_frequency_all,
  
  "03_greedy_prototype_steps_pooled.csv" =
    greedy_prototype_steps,
  
  "04_greedy_coverage_summary_pooled.csv" =
    greedy_coverage_summary,
  
  "05_greedy_ball_determinant_prevalence_pooled.csv.gz" =
    greedy_ball_determinant_prevalence,
  
  "06_greedy_prototype_curve_pooled.csv" =
    greedy_prototype_curve,
  
  "07_hamming_interpretation_pooled.csv" =
    hamming_interpretation,
  
  "08_coverage_threshold_diagnostic_pooled.csv" =
    coverage_threshold_table,
  
  "09_greedy_matrix_x_method_pooled.csv" =
    greedy_matrix_x_method,
  
  "10_score_by_determinant_rank_pooled.csv" =
    score_rank_summary,
  
  "11_source_candidate_contribution.csv" =
    source_candidate_contribution,
  
  "12_greedy_parameters_pooled.csv" =
    parameters
)

iwalk(
  outputs,
  ~ write_csv(
    .x,
    file.path(
      out_dir,
      .y
    )
  )
)


# Figuras de diagnóstico para COMPLETE con la ponderación principal
plot_data <- greedy_prototype_steps %>%
  filter(
    analysis_sample ==
      "COMPLETE",
    
    weighting ==
      PRIMARY_WEIGHTING,
    
    d_hamming %in%
      c(
        0,
        2,
        4,
        6
      )
  ) %>%
  mutate(
    matrix = matrix_label(
      matrix_name
    )
  )

for (
  method_current in
  unique(plot_data$method)
) {
  p <- plot_data %>%
    filter(
      method ==
        method_current
    ) %>%
    ggplot(
      aes(
        x = prototype,
        y = cumulative_covered_pct,
        color = factor(d_hamming),
        group = factor(d_hamming)
      )
    ) +
    geom_line() +
    geom_point(
      size = 1.3
    ) +
    facet_grid(
      d_det ~ matrix
    ) +
    scale_y_continuous(
      limits = c(
        0,
        100
      )
    ) +
    labs(
      title = paste0(
        "Greedy coverage - COMPLETE - ",
        method_current
      ),
      
      subtitle = paste0(
        "Weighting = ",
        PRIMARY_WEIGHTING
      ),
      
      x =
        "Number of Greedy prototypes",
      
      y =
        "Cumulative coverage (%)",
      
      color =
        "Hamming"
    ) +
    theme_minimal(
      base_size = 9
    ) +
    theme(
      panel.grid.minor =
        element_blank(),
      
      strip.text =
        element_text(
          face = "bold"
        )
    )
  
  ggsave(
    filename = file.path(
      fig_dir,
      paste0(
        "greedy_COMPLETE_",
        tolower(
          method_current
        ),
        ".png"
      )
    ),
    plot = p,
    width = 15,
    height = 18,
    dpi = 300
  )
}


# Resumen en consola
cat("\n08. GREEDY COMPLETADO\n")

cat("\nMuestras:\n")

print(
  ANALYSIS_SAMPLES
)

cat(
  "\nK-means generadores: ",
  paste(
    KMEANS_POOL_GRID,
    collapse = ", "
  ),
  "\n",
  sep = ""
)

cat(
  "EFA generadores: ",
  paste(
    EFA_POOL_GRID,
    collapse = ", "
  ),
  "\n",
  sep = ""
)

cat(
  "Ponderaciones: ",
  paste(
    POOL_WEIGHTINGS,
    collapse = ", "
  ),
  "\n",
  sep = ""
)

cat(
  "\nCOMPLETE | K-MEANS | RAW | D=8 | H=4 | equal_candidate\n\n"
)

print(
  greedy_prototype_steps %>%
    filter(
      analysis_sample ==
        "COMPLETE",
      
      weighting ==
        "equal_candidate",
      
      method ==
        "KMEANS",
      
      matrix_name ==
        "matrix_32_raw_0_1",
      
      d_det == 8,
      d_hamming == 4
    ) %>%
    select(
      prototype,
      incremental_covered_pct,
      cumulative_covered_pct,
      center_active_determinants
    ),
  n = Inf,
  width = Inf
)

cat("\nUmbrales de cobertura:\n\n")

print(
  coverage_threshold_table %>%
    filter(
      analysis_sample ==
        "COMPLETE",
      
      weighting ==
        "equal_candidate",
      
      method ==
        "KMEANS",
      
      matrix_name ==
        "matrix_32_raw_0_1",
      
      d_det == 8,
      d_hamming == 4
    ) %>%
    select(
      coverage_threshold,
      first_prototype_reaching_threshold,
      max_coverage_available
    ),
  n = Inf,
  width = Inf
)

message(
  "\nResultados guardados en: ",
  out_dir
)