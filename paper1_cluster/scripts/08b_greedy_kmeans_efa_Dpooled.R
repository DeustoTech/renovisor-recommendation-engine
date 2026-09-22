#
# Objetivo
# Complementar el análisis 08 con una estrategia Greedy D-pooled.
#
# Las firmas binarias generadas por 08 para D = 8:15 se integran
# en un único pool por muestra × método × matriz × ponderación.
# Sobre ese mismo pool se ejecuta Greedy para H = 0:10.
#
# Se analizan:
# - 7 muestras: COMPLETE, EUROPE, LATAM, DIEGO, RENOVISOR,
#   WHY_EUROPE y WHY_LATAM.
# - 2 métodos generadores: KMEANS y EFA.
# - 4 matrices: RAW, POS, EXT y Z_ABS.
# - 2 ponderaciones: equal_candidate y equal_element.
# - hasta 12 prototipos Greedy; los gráficos muestran P1:P8.
#
# D solo genera diversidad de firmas y no separa las ejecuciones Greedy.
# Al mezclar firmas con distinto D, Hamming puede tomar valores pares e impares.
# Este script no sustituye ni sobrescribe 08 y no introduce aleatoriedad nueva.

suppressPackageStartupMessages({
  library(tidyverse)
})

# Configuración
processed_root <- file.path("paper1_cluster/data/processed")

input_dir <- file.path(
  processed_root,
  "08_greedy_kmeans_efa"
)

binary_pattern_candidates <- c(
  file.path(input_dir, "01_binary_patterns_pooled.csv.gz"),
  file.path(input_dir, "01_binary_patterns_pooled.csv")
)

binary_patterns_file <- binary_pattern_candidates[
  file.exists(binary_pattern_candidates)
][1]

if (
  length(binary_patterns_file) == 0 ||
  is.na(binary_patterns_file)
) {
  stop(
    paste0(
      "\nNo encuentro 01_binary_patterns_pooled del script 08 en:\n",
      input_dir
    )
  )
}

out_dir <- file.path(
  processed_root,
  "08b_greedy_kmeans_efa_Dpooled"
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

# Parámetros
ANALYSIS_SAMPLES <- c(
  "COMPLETE",
  "EUROPE",
  "LATAM",
  "DIEGO",
  "RENOVISOR",
  "WHY_EUROPE",
  "WHY_LATAM"
)

METHODS <- c(
  "KMEANS",
  "EFA"
)

MATRICES_TO_RUN <- c(
  "matrix_32_raw_0_1",
  "matrix_32_pos_0_1",
  "matrix_32_ext_0_1",
  "matrix_32_z_abs"
)

EXPECTED_KMEANS_GRID <- 4:8
EXPECTED_EFA_GRID <- 4:8

# D solo genera las firmas. Después todas se integran en el mismo pool.
D_DET_GRID <- 8:15
N_D <- length(D_DET_GRID)

# Al mezclar distintos D pueden aparecer distancias Hamming pares e impares.
H_GRID <- 0:10

MAX_GREEDY_PROTOTYPES <- 12L
PLOT_PROTOTYPES <- 1:8

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
      strsplit(keys, ""),
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
    
    # Elegir el patrón libre con mayor frecuencia/peso.
    candidates <- which(!used)
    
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
    
    # Comparar el prototipo con los patrones todavía libres.
    free_idx <- which(!used)
    
    free_patterns <- patterns[
      free_idx,
      ,
      drop = FALSE
    ]
    
    free_distances <- hamming_distance(
      center,
      free_patterns
    )
    
    # La bola Hamming cubre los patrones con distancia <= H.
    selected_local <- which(
      free_distances <= d_hamming
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
    
    cumulative_frequency <- cumulative_frequency +
      incremental_frequency
    
    used[selected] <- TRUE
    
    # Prevalencia de determinantes dentro de la bola.
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


# Leer las firmas generadas por 08
cat(
  "\nLeyendo firmas de:\n",
  binary_patterns_file,
  "\n",
  sep = ""
)

binary_patterns <- read_csv(
  binary_patterns_file,
  show_col_types = FALSE,
  progress = FALSE
)

required_columns <- c(
  "analysis_sample",
  "method",
  "matrix_name",
  "bootstrap_id",
  "source_candidate",
  "element_id",
  "d_det",
  "active_determinants",
  "pattern_key"
)

missing_columns <- setdiff(
  required_columns,
  names(binary_patterns)
)

if (length(missing_columns) > 0) {
  stop(
    paste0(
      "\nFaltan columnas en 01_binary_patterns_pooled:\n",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}

binary_patterns <- binary_patterns %>%
  mutate(
    analysis_sample = as.character(analysis_sample),
    method = as.character(method),
    matrix_name = as.character(matrix_name),
    bootstrap_id = as.integer(bootstrap_id),
    source_candidate = as.integer(source_candidate),
    d_det = as.integer(d_det),
    element_id = as.character(element_id),
    pattern_key = as.character(pattern_key),
    active_determinants = as.character(active_determinants)
  ) %>%
  filter(
    analysis_sample %in% ANALYSIS_SAMPLES,
    method %in% METHODS,
    matrix_name %in% MATRICES_TO_RUN,
    d_det %in% D_DET_GRID
  )

if (!nrow(binary_patterns)) {
  stop(
    "No quedan firmas después de aplicar los filtros."
  )
}


# Comprobar las firmas
invalid_pattern_key <-
  str_length(
    binary_patterns$pattern_key
  ) != 32 |
  !str_detect(
    binary_patterns$pattern_key,
    "^[01]{32}$"
  )

if (any(invalid_pattern_key)) {
  stop(
    "Hay pattern_key que no son firmas binarias de longitud 32."
  )
}

binary_patterns <- binary_patterns %>%
  mutate(
    signature_size = str_count(
      pattern_key,
      "1"
    )
  )

if (
  any(
    binary_patterns$signature_size !=
    binary_patterns$d_det
  )
) {
  stop(
    paste0(
      "Hay firmas cuyo número de 1 no coincide con d_det. ",
      "Revisar el output 01 del script 08."
    )
  )
}


# Comprobar disponibilidad de D = 8:15
expected_D_grid <- crossing(
  analysis_sample = ANALYSIS_SAMPLES,
  method = METHODS,
  matrix_name = MATRICES_TO_RUN,
  d_det = D_DET_GRID
)

available_D_grid <- binary_patterns %>%
  distinct(
    analysis_sample,
    method,
    matrix_name,
    d_det
  )

missing_D_grid <- expected_D_grid %>%
  anti_join(
    available_D_grid,
    by = c(
      "analysis_sample",
      "method",
      "matrix_name",
      "d_det"
    )
  )

if (nrow(missing_D_grid) > 0) {
  print(
    missing_D_grid,
    n = Inf
  )
  
  stop(
    paste0(
      "Faltan combinaciones D=8:15. ",
      "No se construirá un pool D-pooled incompleto."
    )
  )
}

D_input_summary <- binary_patterns %>%
  count(
    analysis_sample,
    method,
    matrix_name,
    d_det,
    name = "n_signature_instances"
  ) %>%
  mutate(
    matrix = matrix_label(
      matrix_name
    )
  ) %>%
  arrange(
    analysis_sample,
    method,
    matrix,
    d_det
  )

write_csv(
  D_input_summary,
  file.path(
    out_dir,
    "01_input_D8_D15_check.csv"
  )
)


# Recuperar los 32 determinantes
determinants <- binary_patterns$active_determinants %>%
  str_split(";\\s*") %>%
  unlist() %>%
  str_trim() %>%
  unique() %>%
  sort()

determinants <- determinants[
  !is.na(determinants) &
    determinants != ""
]

if (length(determinants) != 32) {
  stop(
    paste0(
      "\nEsperaba recuperar los 32 determinantes ",
      "a partir de las firmas y encuentro ",
      length(determinants),
      ".\n"
    )
  )
}


# Crear las dos ponderaciones
#
# Cada elemento genera N_D firmas.
# equal_element: cada firma recibe 1 / N_D.
# equal_candidate: cada firma recibe 1 / (source_candidate × N_D).
binary_weighted <- map_dfr(
  POOL_WEIGHTINGS,
  function(weighting_current) {
    binary_patterns %>%
      mutate(
        weighting = weighting_current,
        pattern_weight =
          if (weighting_current == "equal_element") {
            
            rep(
              1 / N_D,
              n()
            )
            
          } else if (weighting_current == "equal_candidate") {
            
            1 /
              (
                source_candidate *
                  N_D
              )
            
          } else {
            
            rep(
              NA_real_,
              n()
            )
          }
      )
  }
)

if (
  any(
    !is.finite(
      binary_weighted$pattern_weight
    ) |
    binary_weighted$pattern_weight <= 0
  )
) {
  stop(
    "Hay pesos no válidos en el pool D-pooled."
  )
}


# Frecuencia y peso de los patrones
#
# D no entra en el group_by: las firmas D = 8:15 permanecen
# juntas en cada pool.
pattern_frequency <- binary_weighted %>%
  group_by(
    analysis_sample,
    method,
    matrix_name,
    weighting,
    pattern_key,
    active_determinants
  ) %>%
  summarise(
    signature_size = first(d_det),
    n_distinct_D_for_pattern = n_distinct(d_det),
    n_occurrences = n(),
    weight = sum(
      pattern_weight,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# Una misma firma binaria debe tener un único número
# de determinantes activos.
if (
  any(
    pattern_frequency$n_distinct_D_for_pattern != 1
  )
) {
  stop(
    "Una misma firma aparece asociada a más de un D. Revisar."
  )
}

pattern_frequency <- pattern_frequency %>%
  group_by(
    analysis_sample,
    method,
    matrix_name,
    weighting
  ) %>%
  mutate(
    total_pattern_occurrences = sum(
      n_occurrences
    ),
    total_weight = sum(
      weight
    ),
    frequency_pct = 100 *
      weight /
      total_weight
  ) %>%
  ungroup() %>%
  arrange(
    analysis_sample,
    method,
    matrix_name,
    weighting,
    desc(weight),
    pattern_key
  )

write_csv(
  pattern_frequency,
  file.path(
    out_dir,
    "02_pattern_frequency_Dpooled.csv.gz"
  )
)


# Diagnóstico: contribución de cada D al pool
D_contribution <- binary_weighted %>%
  group_by(
    analysis_sample,
    method,
    matrix_name,
    weighting,
    d_det
  ) %>%
  summarise(
    n_signature_instances = n(),
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
    weighting
  ) %>%
  mutate(
    weight_pct = 100 *
      total_weight /
      sum(total_weight)
  ) %>%
  ungroup() %>%
  arrange(
    analysis_sample,
    method,
    matrix_name,
    weighting,
    d_det
  )

write_csv(
  D_contribution,
  file.path(
    out_dir,
    "03_D_contribution_to_pooled_pool.csv"
  )
)


# Diagnóstico: contribución de K/F
source_candidate_contribution <- binary_weighted %>%
  group_by(
    analysis_sample,
    method,
    matrix_name,
    weighting,
    source_candidate
  ) %>%
  summarise(
    n_signature_instances = n(),
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
    weighting
  ) %>%
  mutate(
    weight_pct = 100 *
      total_weight /
      sum(total_weight)
  ) %>%
  ungroup() %>%
  arrange(
    analysis_sample,
    method,
    matrix_name,
    weighting,
    source_candidate
  )

write_csv(
  source_candidate_contribution,
  file.path(
    out_dir,
    "04_source_candidate_contribution_Dpooled.csv"
  )
)


# Definir los pools Greedy
#
# Cada pool se define por muestra × método × matriz × weighting.
# D ya no separa las ejecuciones.
groups <- pattern_frequency %>%
  distinct(
    analysis_sample,
    method,
    matrix_name,
    weighting
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
      matrix_name,
      levels = MATRICES_TO_RUN
    ),
    weighting
  )

expected_pool_count <-
  length(ANALYSIS_SAMPLES) *
  length(METHODS) *
  length(MATRICES_TO_RUN) *
  length(POOL_WEIGHTINGS)

if (
  nrow(groups) !=
  expected_pool_count
) {
  print(
    groups,
    n = Inf
  )
  
  stop(
    paste0(
      "\nEsperaba ",
      expected_pool_count,
      " pools y encuentro ",
      nrow(groups),
      "."
    )
  )
}


# Greedy sobre el pool D = 8:15
prototype_steps_list <- list()
ball_prevalence_list <- list()
coverage_summary_list <- list()

greedy_counter <- 0L

for (g in seq_len(nrow(groups))) {
  group_current <- groups[
    g,
  ]
  
  cat(
    "\nPOOL ",
    g,
    "/",
    nrow(groups),
    " | ",
    group_current$analysis_sample,
    " | ",
    group_current$method,
    " | ",
    matrix_label(
      group_current$matrix_name
    ),
    " | ",
    group_current$weighting,
    "\n",
    sep = ""
  )
  
  # El mismo pool D = 8:15 se evalúa para todos los H.
  pattern_df <- pattern_frequency %>%
    filter(
      analysis_sample ==
        group_current$analysis_sample,
      method ==
        group_current$method,
      matrix_name ==
        group_current$matrix_name,
      weighting ==
        group_current$weighting
    ) %>%
    arrange(
      desc(weight),
      pattern_key
    )
  
  if (!nrow(pattern_df)) {
    stop(
      "Pool vacío."
    )
  }
  
  pattern_matrix <- patterns_to_matrix(
    pattern_df$pattern_key,
    determinants
  )
  
  frequencies <- pattern_df$weight
  total_frequency <- sum(frequencies)
  n_unique_patterns <- nrow(pattern_df)
  
  for (h_current in H_GRID) {
    greedy_counter <- greedy_counter + 1L
    
    result_steps <- run_greedy_sequence(
      patterns = pattern_matrix,
      frequencies = frequencies,
      max_prototypes = MAX_GREEDY_PROTOTYPES,
      d_hamming = h_current
    )
    
    # Evolución P1, P2, ...
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
          
          d_hamming =
            h_current,
          
          prototype =
            step$prototype,
          
          center_pattern_key =
            paste0(
              step$center,
              collapse = ""
            ),
          
          center_signature_size =
            sum(
              step$center
            ),
          
          center_active_determinants =
            paste(
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
            n_unique_patterns
        )
      }
    )
    
    prototype_steps_list[[greedy_counter]] <- steps_df
    
    # Prevalencia de determinantes dentro de la bola cubierta.
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
          
          d_hamming =
            h_current,
          
          prototype =
            step$prototype,
          
          center_signature_size =
            sum(
              step$center
            ),
          
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
    
    # Resumen de cobertura para este H.
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
        
        d_hamming =
          h_current,
        
        n_prototypes_used =
          max(
            steps_df$prototype
          ),
        
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


# Consolidar resultados
greedy_prototype_steps <- bind_rows(
  prototype_steps_list
)

greedy_ball_determinant_prevalence <- bind_rows(
  ball_prevalence_list
)

greedy_coverage_summary <- bind_rows(
  coverage_summary_list
)

if (!nrow(greedy_prototype_steps)) {
  stop(
    "Greedy no ha generado resultados."
  )
}


# Curva H × P
#
# Salida principal para los gráficos:
# X = H, Y = cobertura acumulada y color = P.
greedy_H_by_P_curve <- greedy_prototype_steps %>%
  transmute(
    analysis_sample,
    weighting,
    method,
    matrix_name,
    matrix = matrix_label(
      matrix_name
    ),
    d_hamming,
    prototype,
    incremental_covered_pct,
    cumulative_covered_pct,
    center_signature_size,
    center_active_determinants
  ) %>%
  arrange(
    analysis_sample,
    weighting,
    method,
    matrix_name,
    prototype,
    d_hamming
  )

write_csv(
  greedy_H_by_P_curve,
  file.path(
    out_dir,
    "05_greedy_H_by_P_curve_Dpooled.csv"
  )
)


# Número de prototipos necesarios para cada umbral
coverage_threshold_table <- greedy_prototype_steps %>%
  select(
    analysis_sample,
    weighting,
    method,
    matrix_name,
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
  mutate(
    matrix = matrix_label(
      matrix_name
    )
  ) %>%
  arrange(
    analysis_sample,
    weighting,
    method,
    matrix,
    d_hamming,
    coverage_threshold
  )

write_csv(
  coverage_threshold_table,
  file.path(
    out_dir,
    "06_coverage_threshold_by_H_Dpooled.csv"
  )
)


# Guardar resultados Greedy
write_csv(
  greedy_prototype_steps,
  file.path(
    out_dir,
    "07_greedy_prototype_steps_Dpooled.csv"
  )
)

write_csv(
  greedy_coverage_summary,
  file.path(
    out_dir,
    "08_greedy_coverage_summary_Dpooled.csv"
  )
)

write_csv(
  greedy_ball_determinant_prevalence,
  file.path(
    out_dir,
    "09_greedy_ball_determinant_prevalence_Dpooled.csv.gz"
  )
)


# Parámetros
parameters <- tibble(
  parameter = c(
    "input_binary_patterns",
    "analysis_samples",
    "methods",
    "matrices",
    "D_values_pooled",
    "n_D_values",
    "H_grid",
    "max_greedy_prototypes",
    "plot_prototypes",
    "coverage_thresholds",
    "pool_weightings",
    "primary_weighting",
    "random_seed"
  ),
  
  value = c(
    binary_patterns_file,
    paste(
      ANALYSIS_SAMPLES,
      collapse = ", "
    ),
    paste(
      METHODS,
      collapse = ", "
    ),
    paste(
      MATRICES_TO_RUN,
      collapse = ", "
    ),
    paste(
      D_DET_GRID,
      collapse = ", "
    ),
    as.character(
      N_D
    ),
    paste(
      H_GRID,
      collapse = ", "
    ),
    as.character(
      MAX_GREEDY_PROTOTYPES
    ),
    paste(
      PLOT_PROTOTYPES,
      collapse = ", "
    ),
    paste(
      COVERAGE_THRESHOLDS,
      collapse = ", "
    ),
    paste(
      POOL_WEIGHTINGS,
      collapse = ", "
    ),
    PRIMARY_WEIGHTING,
    "NONE - no random operation is performed in 08b"
  )
)

write_csv(
  parameters,
  file.path(
    out_dir,
    "10_parameters_Dpooled.csv"
  )
)


# Figuras principales
#
# Se genera un gráfico por muestra × método con cuatro paneles:
# RAW, POS, EXT y Z_ABS.
plot_data <- greedy_H_by_P_curve %>%
  filter(
    weighting ==
      PRIMARY_WEIGHTING,
    prototype %in%
      PLOT_PROTOTYPES
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

for (sample_current in ANALYSIS_SAMPLES) {
  for (method_current in METHODS) {
    current_plot_data <- plot_data %>%
      filter(
        analysis_sample ==
          sample_current,
        method ==
          method_current
      )
    
    if (!nrow(current_plot_data)) {
      next
    }
    
    p <- ggplot(
      current_plot_data,
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
        size = 1.8
      ) +
      facet_wrap(
        ~ matrix,
        ncol = 2
      ) +
      scale_x_continuous(
        breaks = H_GRID
      ) +
      scale_y_continuous(
        limits = c(
          0,
          100
        ),
        breaks = seq(
          0,
          100,
          by = 20
        )
      ) +
      labs(
        title = paste0(
          "Greedy D-pooled - ",
          sample_current,
          " - ",
          method_current
        ),
        subtitle = paste0(
          "D=8:15 pooled | Weighting=",
          PRIMARY_WEIGHTING,
          " | dashed line = 90% coverage"
        ),
        x = "Hamming radius (H)",
        y = "Cumulative coverage (%)",
        color = "Number of prototypes"
      ) +
      theme_minimal(
        base_size = 11
      ) +
      theme(
        panel.grid.minor = element_blank(),
        strip.text = element_text(
          face = "bold"
        ),
        plot.title = element_text(
          face = "bold"
        )
      )
    
    ggsave(
      filename = file.path(
        fig_dir,
        paste0(
          "greedy_Dpooled_",
          sample_current,
          "_",
          tolower(
            method_current
          ),
          "_H_by_P.png"
        )
      ),
      plot = p,
      width = 11,
      height = 8,
      dpi = 300,
      bg = "white"
    )
  }
}


# Comprobaciones finales
expected_H_results <- crossing(
  analysis_sample = ANALYSIS_SAMPLES,
  weighting = POOL_WEIGHTINGS,
  method = METHODS,
  matrix_name = MATRICES_TO_RUN,
  d_hamming = H_GRID
)

available_H_results <- greedy_coverage_summary %>%
  distinct(
    analysis_sample,
    weighting,
    method,
    matrix_name,
    d_hamming
  )

missing_H_results <- expected_H_results %>%
  anti_join(
    available_H_results,
    by = c(
      "analysis_sample",
      "weighting",
      "method",
      "matrix_name",
      "d_hamming"
    )
  )

if (nrow(missing_H_results) > 0) {
  print(
    missing_H_results,
    n = Inf
  )
  
  stop(
    "Faltan combinaciones en el resultado Greedy D-pooled."
  )
}


# Resumen COMPLETE - KMEANS - 90%
complete_kmeans_90 <- coverage_threshold_table %>%
  filter(
    analysis_sample == "COMPLETE",
    weighting == PRIMARY_WEIGHTING,
    method == "KMEANS",
    coverage_threshold == 90
  ) %>%
  select(
    matrix,
    d_hamming,
    first_prototype_reaching_threshold,
    max_coverage_available
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
    d_hamming
  )


# Resumen COMPLETE - EFA - 90%
complete_efa_90 <- coverage_threshold_table %>%
  filter(
    analysis_sample == "COMPLETE",
    weighting == PRIMARY_WEIGHTING,
    method == "EFA",
    coverage_threshold == 90
  ) %>%
  select(
    matrix,
    d_hamming,
    first_prototype_reaching_threshold,
    max_coverage_available
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
    d_hamming
  )


# Resumen en consola
cat(
  "\n08b. GREEDY D-POOLED COMPLETADO\n"
)

cat(
  "\nFirmas utilizadas:\nD = ",
  paste(
    D_DET_GRID,
    collapse = ", "
  ),
  "\n",
  sep = ""
)

cat(
  "\nD=8:15 se ha integrado en un único pool.",
  "\nD no separa las ejecuciones Greedy.\n",
  sep = ""
)

cat(
  "\nRadios Hamming analizados:\nH = ",
  paste(
    H_GRID,
    collapse = ", "
  ),
  "\n",
  sep = ""
)

cat(
  "\nMáximo de prototipos Greedy: ",
  MAX_GREEDY_PROTOTYPES,
  "\n",
  sep = ""
)

cat(
  "\nPonderación principal: ",
  PRIMARY_WEIGHTING,
  "\n",
  sep = ""
)

cat(
  "\nNúmero total de pools D-pooled: ",
  nrow(groups),
  "\n",
  sep = ""
)

cat(
  "\nNúmero de ejecuciones pool × H: ",
  nrow(groups) *
    length(H_GRID),
  "\n",
  sep = ""
)

cat(
  "\nCOMPLETE | KMEANS | Nº P NECESARIOS PARA 90%\n\n"
)

print(
  complete_kmeans_90,
  n = Inf,
  width = Inf
)

cat(
  "\nCOMPLETE | EFA | Nº P NECESARIOS PARA 90%\n\n"
)

print(
  complete_efa_90,
  n = Inf,
  width = Inf
)

cat(
  "\nOUTPUTS PRINCIPALES\n\n",
  "01_input_D8_D15_check.csv\n",
  "02_pattern_frequency_Dpooled.csv.gz\n",
  "03_D_contribution_to_pooled_pool.csv\n",
  "04_source_candidate_contribution_Dpooled.csv\n",
  "05_greedy_H_by_P_curve_Dpooled.csv\n",
  "06_coverage_threshold_by_H_Dpooled.csv\n",
  "07_greedy_prototype_steps_Dpooled.csv\n",
  "08_greedy_coverage_summary_Dpooled.csv\n",
  "09_greedy_ball_determinant_prevalence_Dpooled.csv.gz\n",
  "10_parameters_Dpooled.csv\n",
  sep = ""
)

cat(
  "\nFiguras en:\n",
  fig_dir,
  "\n",
  sep = ""
)

cat(
  "\nGráfico principal:\n",
  "X = H | Y = cobertura acumulada | color = P\n",
  sep = ""
)

cat(
  "\nResultados guardados en:\n",
  out_dir,
  "\n",
  sep = ""
)

message(
  "\nListo. 08b D-pooled generado sin modificar el 08 original."
)
