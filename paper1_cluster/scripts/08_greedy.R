
# Resumir los patrones obtenidos mediante:
#
#   A) K-means
#   B) EFA
#
# sobre los distintos bootstraps.

# Para cada:
#
#   - método: KMEANS / EFA
#   - matriz: RAW / POS / EXT / Z_ABS
#   - K = 4,...,8
#       * KMEANS -> número de clusters
#       * EFA    -> número de factores
#   - D_DET = 8,...,15
#
# hacemos:
#
#   1. coger cada cluster/factor
#   2. coger sus D_DET determinantes más importantes
#   3. poner esos determinantes a 1
#   4. poner el resto a 0
#
# Así obtenemos un patrón binario de 32 determinantes.
#
# Después GREEDY:
#
#   para D_HAMMING = 0,...,10
#
#   1. buscamos el patrón NO cubierto más frecuente
#   2. ese patrón se convierte en centro/prototipo
#   3. creamos una bola de radio D_HAMMING
#   4. cubrimos todos los patrones dentro de esa bola
#   5. calculamos qué % hemos cubierto
#   6. repetimos hasta tener como máximo K prototipos
#
# SALIDAS IMPORTANTES
#
# 03_greedy_prototype_steps.csv
#     cobertura incremental y acumulada prototipo a prototipo.
#
# 04_greedy_coverage_summary.csv
#     cobertura final de cada combinación.
#
# 05_greedy_ball_determinant_prevalence.csv
#     % de veces que cada determinante aparece dentro de cada bola.
#
# 06_greedy_k_comparison.csv
#     comparación K=4,...,8 y mejora al aumentar K.
#
# 07_hamming_interpretation.csv
#     interpretación de D_HAMMING en nº/% de determinantes comunes.
#
# 10_greedy_matrix_x_method.csv
#     salida compacta para comparar matriz × método.

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(stringr)
  library(ggplot2)
})

set.seed(123)

# RUTAS
project_root <- path.expand("~/Desktop/MASTER/recommendation-engine/TFM")

processed_root <- file.path(project_root,"paper1_cluster/data/processed")

# K-MEANS
kmeans_file <- file.path(processed_root, "06_kmeans_bootstrap", "kmeans_centers_long.csv")

# EFA
efa_file <- file.path(processed_root, "07_efa_bootstrap", "03_efa_loadings_long.csv")

# OUTPUT
out_dir <- file.path(processed_root, "08_greedy_kmeans_efa")

fig_dir <- file.path(out_dir, "figures")

dir.create(out_dir, recursive = TRUE,showWarnings = FALSE)

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# PARÁMETROS

# D = número de determinantes que definen cada patrón
D_DET_GRID <- 8:15

# Distancia Hamming
D_HAMMING_GRID <- 0:10


# Bootstrap
MAX_BOOTSTRAPS <- 100
# MAX_BOOTSTRAPS <- Inf

# Umbrales de cobertura
# para responder a ¿Cuántos prototipos necesito para alcanzar X% de cobertura?
# ejemplo
#P1 → 35%
#P2 → 57%
#P3 → 74%
#P4 → 84%
#P5 → 91%
#P6 → 94%
#Entonces:
#80% → 4 prototipos
#85% → 5
#90% → 5
#95% → no se alcanza
COVERAGE_THRESHOLDS <- c(
  80,
  85,
  90,
  95
)

# LEER K-MEANS
if (!file.exists(kmeans_file)) {
  
  stop(
    "No encuentro el archivo K-means: ",
    kmeans_file
  )
}


kmeans <- read_csv(
  kmeans_file,
  show_col_types = FALSE
) %>%
  
  mutate(
    
    bootstrap_id =
      as.integer(bootstrap_id),
    
    k =
      as.integer(k),
    
    cluster_rank =
      as.integer(cluster_rank),
    
    determinant =
      as.character(determinant),
    
    center_value =
      as.numeric(center_value)
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
  names(kmeans)
)


if (length(missing_kmeans) > 0) {
  
  stop(
    "Faltan columnas en kmeans_centers_long.csv: ",
    paste(
      missing_kmeans,
      collapse = ", "
    )
  )
}



# ============================================================
# 4. LEER EFA
# ============================================================

if (!file.exists(efa_file)) {
  
  stop(
    "No encuentro el archivo EFA: ",
    efa_file
  )
}


efa <- read_csv(
  efa_file,
  show_col_types = FALSE
) %>%
  
  mutate(
    
    bootstrap_id =
      as.integer(bootstrap_id),
    
    n_factors =
      as.integer(n_factors),
    
    determinant =
      as.character(determinant),
    
    factor =
      as.character(factor),
    
    loading =
      as.numeric(loading)
  )



required_efa <- c(
  
  "matrix_name",
  
  "bootstrap_id",
  
  "n_factors",
  
  "determinant",
  
  "factor",
  
  "loading"
)


missing_efa <- setdiff(
  required_efa,
  names(efa)
)


if (length(missing_efa) > 0) {
  
  stop(
    "Faltan columnas en 03_efa_loadings_long.csv: ",
    paste(
      missing_efa,
      collapse = ", "
    )
  )
}



# ============================================================
# 5. BOOTSTRAPS COMUNES
# ============================================================

all_boot_ids <- sort(
  
  intersect(
    
    unique(
      kmeans$bootstrap_id
    ),
    
    unique(
      efa$bootstrap_id
    )
  )
)



if (length(all_boot_ids) == 0) {
  
  stop(
    "K-means y EFA no tienen bootstrap_id en común."
  )
}



if (is.finite(MAX_BOOTSTRAPS)) {
  
  boot_ids <- head(
    all_boot_ids,
    MAX_BOOTSTRAPS
  )
  
} else {
  
  boot_ids <- all_boot_ids
}



kmeans <- kmeans %>%
  
  filter(
    bootstrap_id %in%
      boot_ids
  )



efa <- efa %>%
  
  filter(
    bootstrap_id %in%
      boot_ids
  )



cat(
  "\nBootstraps usados: ",
  length(boot_ids),
  "\n",
  sep = ""
)



# ============================================================
# 6. UNIVERSO DE 32 DETERMINANTES
# ============================================================

determinants <- sort(
  
  union(
    
    unique(
      kmeans$determinant
    ),
    
    unique(
      efa$determinant
    )
  )
)



if (length(determinants) != 32) {
  
  stop(
    "Esperaba 32 determinantes y encuentro ",
    length(determinants),
    "."
  )
}



# ============================================================
# 7. CREAR UNA ESTRUCTURA COMÚN
# ============================================================
#
# Queremos acabar con:
#
# method
# matrix_name
# bootstrap_id
# k_candidate
# element_id
# determinant
# score
#
#
# KMEANS
# ------
#
# element_id = cluster
# score      = abs(valor centroide)
#
#
# EFA
# ---
#
# element_id = factor
# score      = abs(loading)
#
#
# Esto sigue la nota:
#
#          MAX(abs(x))
#
# y después coger los D valores más altos.
# ============================================================



# ------------------------------------------------------------
# K-MEANS
# ------------------------------------------------------------

# ------------------------------------------------------------
# K-MEANS
# ------------------------------------------------------------
#
# La importancia de un determinante depende del significado
# de cada transformación.
#
# RAW:
#   baseline neutral = 0.5
#   son importantes tanto valores bajos como altos:
#
#       score = |centro - 0.5|
#
#   Ejemplo:
#       centro = 0.2 -> score = 0.3
#       centro = 0.8 -> score = 0.3
#
#
# POS:
#   los valores originales < 0.5 ya fueron llevados a 0.5.
#   Nos interesa cuánto sobresale por encima del baseline:
#
#       score = max(centro - 0.5, 0)
#
#
# EXT:
#   la propia matriz ya representa distancia respecto al
#   punto neutral.
#
#       score = centro
#
#
# Z_ABS:
#   la propia matriz ya representa |z|.
#
#       score = centro
#
# ------------------------------------------------------------

kmeans_scores <- kmeans %>%
  
  mutate(
    
    score = case_when(
      
      # RAW:
      # importancia = alejamiento del valor neutral 0.5
      matrix_name ==
        "matrix_32_raw_0_1" ~
        
        abs(
          center_value - 0.5
        ),
      
      
      # POS:
      # importancia = cuánto sobresale por encima de 0.5
      matrix_name ==
        "matrix_32_pos_0_1" ~
        
        pmax(
          center_value - 0.5,
          0
        ),
      
      
      # EXT:
      # ya representa extremidad respecto al centro
      matrix_name ==
        "matrix_32_ext_0_1" ~
        
        center_value,
      
      
      # Z_ABS:
      # ya representa desviación absoluta estandarizada
      matrix_name ==
        "matrix_32_z_abs" ~
        
        center_value,
      
      
      # Seguridad por si apareciese alguna matriz inesperada
      TRUE ~
        NA_real_
    )
  ) %>%
  
  transmute(
    
    method =
      "KMEANS",
    
    matrix_name,
    
    bootstrap_id,
    
    k_candidate =
      k,
    
    element_id =
      paste0(
        "cluster_",
        cluster_rank
      ),
    
    determinant,
    
    score
  )



# ------------------------------------------------------------
# EFA
# ------------------------------------------------------------

efa_scores <- efa %>%
  
  transmute(
    
    method =
      "EFA",
    
    matrix_name,
    
    bootstrap_id,
    
    k_candidate =
      n_factors,
    
    element_id =
      factor,
    
    determinant,
    
    score =
      abs(loading)
  )



# ------------------------------------------------------------
# UNIR
# ------------------------------------------------------------

scores <- bind_rows(
  
  kmeans_scores,
  
  efa_scores
)



# ============================================================
# 8. COMPLETAR LOS 32 DETERMINANTES
# ============================================================
#
# Si algún EFA hubiese eliminado una variable constante,
# la reintroducimos con score = 0.
#
# Así todos los patrones tienen siempre 32 posiciones.
# ============================================================

elements <- scores %>%
  
  distinct(
    
    method,
    
    matrix_name,
    
    bootstrap_id,
    
    k_candidate,
    
    element_id
  )



scores_complete <- elements %>%
  
  crossing(
    determinant = determinants
  ) %>%
  
  left_join(
    
    scores,
    
    by = c(
      
      "method",
      
      "matrix_name",
      
      "bootstrap_id",
      
      "k_candidate",
      
      "element_id",
      
      "determinant"
    )
  ) %>%
  
  mutate(
    
    score =
      replace_na(
        score,
        0
      )
  )

# ============================================================
# DIAGNÓSTICO PARA ELEGIR D_DET
# ============================================================
#
# Para cada cluster/factor:
#
#   1. ordenamos los 32 determinantes de mayor a menor score
#   2. asignamos un rank:
#
#        rank 1  = determinante más importante
#        rank 2  = segundo
#        ...
#        rank 32 = menos importante
#
# Después resumimos estos scores entre todos los bootstraps
# y clusters/factores.
#
# Esto nos permitirá ver si aparece una caída clara, por ejemplo:
#
# rank 7 -> 0.55
# rank 8 -> 0.52
# rank 9 -> 0.30
#
# En ese caso D_DET = 8 tendría una justificación clara.
#
# Si la caída es gradual y no existe un corte evidente,
# elegiremos D combinando estabilidad + parsimonia +
# sensibilidad de los resultados.
# ============================================================

score_ranks <- scores_complete %>%
  
  group_by(
    method,
    matrix_name,
    bootstrap_id,
    k_candidate,
    element_id
  ) %>%
  
  arrange(
    desc(score),
    determinant,
    .by_group = TRUE
  ) %>%
  
  mutate(
    score_rank = row_number()
  ) %>%
  
  ungroup()

# ============================================================
# RESUMEN DEL SCORE POR RANK
# ============================================================

score_rank_summary <- score_ranks %>%
  
  group_by(
    method,
    matrix_name,
    k_candidate,
    score_rank
  ) %>%
  
  summarise(
    
    mean_score =
      mean(
        score,
        na.rm = TRUE
      ),
    
    median_score =
      median(
        score,
        na.rm = TRUE
      ),
    
    q25_score =
      quantile(
        score,
        0.25,
        na.rm = TRUE
      ),
    
    q75_score =
      quantile(
        score,
        0.75,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) %>%
  
  group_by(
    method,
    matrix_name,
    k_candidate
  ) %>%
  
  arrange(
    score_rank,
    .by_group = TRUE
  ) %>%
  
  mutate(
    
    # Cuánto cae el score al pasar del determinante
    # anterior al actual.
    drop_from_previous_rank =
      lag(mean_score) -
      mean_score
  ) %>%
  
  ungroup()

# ============================================================
# 9. CREAR PATRONES BINARIOS
# ============================================================
#
# Para cada:
#
#   método
#   matriz
#   bootstrap
#   K
#   cluster/factor
#   D_DET
#
# cogemos los D_DET scores más altos.
#
#
# Ejemplo D_DET = 8:
#
#  1 0 1 0 0 1 ... 0
#
# exactamente 8 posiciones quedan a 1.
#
#
# En caso de empate:
#
#   1. score más alto
#   2. nombre del determinante
#
# para que sea reproducible.
# ============================================================

binary_patterns <- map_dfr(
  
  D_DET_GRID,
  
  function(d_det_current) {
    
    
    scores_complete %>%
      
      group_by(
        
        method,
        
        matrix_name,
        
        bootstrap_id,
        
        k_candidate,
        
        element_id
      ) %>%
      
      arrange(
        
        desc(score),
        
        determinant,
        
        .by_group = TRUE
      ) %>%
      
      slice_head(
        n =
          d_det_current
      ) %>%
      
      summarise(
        
        d_det =
          d_det_current,
        
        
        active_determinants =
          paste(
            
            sort(
              determinant
            ),
            
            collapse = "; "
          ),
        
        
        pattern_key =
          paste0(
            
            as.integer(
              
              determinants %in%
                determinant
            ),
            
            collapse = ""
          ),
        
        
        mean_selected_score =
          mean(
            score,
            na.rm = TRUE
          ),
        
        
        min_selected_score =
          min(
            score,
            na.rm = TRUE
          ),
        
        
        max_selected_score =
          max(
            score,
            na.rm = TRUE
          ),
        
        
        .groups =
          "drop"
      )
  }
)



# ============================================================
# 10. FRECUENCIA DE CADA PATRÓN
# ============================================================
#
# Ejemplo:
#
# K = 4
# 100 bootstraps
#
# -> tenemos 400 clusters/factores.
#
# Si un patrón aparece 30 veces:
#
# N = 30
#
# ============================================================

pattern_frequency <- binary_patterns %>%
  
  count(
    
    method,
    
    matrix_name,
    
    k_candidate,
    
    d_det,
    
    pattern_key,
    
    active_determinants,
    
    name = "N"
  ) %>%
  
  group_by(
    
    method,
    
    matrix_name,
    
    k_candidate,
    
    d_det
  ) %>%
  
  mutate(
    
    total_pattern_occurrences =
      sum(N),
    
    
    frequency_pct =
      100 *
      N /
      total_pattern_occurrences
  ) %>%
  
  ungroup() %>%
  
  arrange(
    
    method,
    
    matrix_name,
    
    k_candidate,
    
    d_det,
    
    desc(N)
  )



# ============================================================
# 11. CONVERTIR pattern_key A MATRIZ 0/1
# ============================================================

patterns_to_matrix <- function(keys) {
  
  
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
  
  
  colnames(mat) <-
    determinants
  
  
  mat
}



# ============================================================
# 12. DISTANCIA HAMMING
# ============================================================

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



# ============================================================
# 13. INTERPRETACIÓN DE HAMMING
# ============================================================
#
# Como todos los patrones de un mismo D_DET
# tienen EXACTAMENTE D_DET unos:
#
#
# Hamming =
#
#   2 × (D_DET - nº determinantes comunes)
#
#
# Ejemplo D_DET = 8:
#
# Hamming 0 -> 8 comunes -> 100%
#
# Hamming 2 -> 7 comunes -> 87.5%
#
# Hamming 4 -> 6 comunes -> 75%
#
# Hamming 6 -> 5 comunes -> 62.5%
#
# Hamming 8 -> 4 comunes -> 50%
#
# Hamming 10 -> 3 comunes -> 37.5%
#
#
# IMPORTANTE:
#
# Las distancias reales son siempre pares.
#
# Por tanto:
#
# D=0 y D=1 producen lo mismo.
# D=2 y D=3 producen lo mismo.
# etc.
#
# Conservamos 0:10 porque así está definido
# en el checklist.
# ============================================================

hamming_interpretation <- crossing(
  
  d_det =
    D_DET_GRID,
  
  d_hamming =
    D_HAMMING_GRID
  
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
        
        2 *
          floor(
            d_hamming /
              2
          ),
        
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
    
    
    odd_radius_redundant =
      d_hamming %% 2 == 1,
    
    
    radius_can_cover_any_pattern =
      d_hamming >=
      max_possible_hamming
  )



# ============================================================
# 14. GREEDY SECUENCIAL
# ============================================================
#
# Para una combinación:
#
#   método
#   matriz
#   K
#   D_DET
#   D_HAMMING
#
#
# hacemos:
#
# Prototipo 1
# ------------
#
# escoger patrón libre más frecuente
#          ↓
# crear bola Hamming
#          ↓
# cubrir patrones
#          ↓
# calcular cobertura
#
#
# Prototipo 2
# ------------
#
# coger el patrón más frecuente
# ENTRE LOS QUE QUEDAN
#
# etc.
#
#
# Paramos:
#
#   - si todo está cubierto
#   - o al llegar a K prototipos
#
# ============================================================

run_greedy_sequence <- function(
    
  patterns,
  
  frequencies,
  
  n_prototypes,
  
  d_hamming
) {
  
  
  used <- rep(
    FALSE,
    nrow(patterns)
  )
  
  
  steps <- vector(
    "list",
    n_prototypes
  )
  
  
  cumulative_frequency <- 0
  
  
  
  for (
    prototype_id in
    seq_len(
      n_prototypes
    )
  ) {
    
    
    # --------------------------------------------------------
    # Si ya hemos cubierto todo
    # --------------------------------------------------------
    
    if (all(used)) {
      
      break
    }
    
    
    
    # --------------------------------------------------------
    # Patrón libre más frecuente
    # --------------------------------------------------------
    
    candidates <- which(
      !used
    )
    
    
    center_idx <- candidates[
      
      which.max(
        
        frequencies[
          candidates
        ]
      )
    ]
    
    
    
    center <- patterns[
      
      center_idx,
      
      ,
      
      drop = TRUE
    ]
    
    
    
    # --------------------------------------------------------
    # Patrones que todavía están libres
    # --------------------------------------------------------
    
    free_idx <- which(
      !used
    )
    
    
    free_patterns <- patterns[
      
      free_idx,
      
      ,
      
      drop = FALSE
    ]
    
    
    
    # --------------------------------------------------------
    # Distancia Hamming al centro
    # --------------------------------------------------------
    
    free_distances <- hamming_distance(
      
      center,
      
      free_patterns
    )
    
    
    
    # --------------------------------------------------------
    # Bola de radio D_HAMMING
    # --------------------------------------------------------
    
    selected_local <- which(
      
      free_distances <=
        d_hamming
    )
    
    
    selected <- free_idx[
      selected_local
    ]
    
    
    selected_distances <-
      free_distances[
        selected_local
      ]
    
    
    
    # --------------------------------------------------------
    # Cobertura incremental
    # --------------------------------------------------------
    
    incremental_frequency <- sum(
      
      frequencies[
        selected
      ],
      
      na.rm = TRUE
    )
    
    
    
    cumulative_frequency <-
      
      cumulative_frequency +
      incremental_frequency
    
    
    
    # --------------------------------------------------------
    # Marcar patrones cubiertos
    # --------------------------------------------------------
    
    used[
      selected
    ] <- TRUE
    
    
    
    # ========================================================
    # % DE APARICIÓN DE CADA DETERMINANTE DENTRO DE LA BOLA
    # ========================================================
    #
    # IMPORTANTE:
    #
    # Ponderamos por frecuencia.
    #
    # Un patrón que aparece 20 veces pesa 20.
    #
    # Un patrón que aparece 1 vez pesa 1.
    #
    # ========================================================
    
    member_matrix <- patterns[
      
      selected,
      
      ,
      
      drop = FALSE
    ]
    
    
    member_weights <-
      frequencies[
        selected
      ]
    
    
    
    weighted_member_matrix <- sweep(
      
      member_matrix,
      
      1,
      
      member_weights,
      
      FUN = "*"
    )
    
    
    
    determinant_prevalence <-
      
      colSums(
        
        weighted_member_matrix,
        
        na.rm = TRUE
        
      ) /
      
      sum(
        
        member_weights,
        
        na.rm = TRUE
      )
    
    
    
    # --------------------------------------------------------
    # Distancia media de los patrones dentro de la bola
    # --------------------------------------------------------
    
    weighted_mean_hamming <- if (
      
      length(
        selected_distances
      ) > 0
      
    ) {
      
      
      weighted.mean(
        
        selected_distances,
        
        w =
          member_weights
      )
      
      
    } else {
      
      
      NA_real_
    }
    
    
    
    # --------------------------------------------------------
    # Guardar paso
    # --------------------------------------------------------
    
    steps[[prototype_id]] <- list(
      
      prototype =
        prototype_id,
      
      
      center_index =
        center_idx,
      
      
      center =
        center,
      
      
      members =
        selected,
      
      
      center_frequency =
        frequencies[
          center_idx
        ],
      
      
      incremental_frequency =
        incremental_frequency,
      
      
      cumulative_frequency =
        cumulative_frequency,
      
      
      determinant_prevalence =
        determinant_prevalence,
      
      
      n_unique_patterns_in_ball =
        length(
          selected
        ),
      
      
      weighted_mean_hamming =
        weighted_mean_hamming,
      
      
      max_hamming_in_ball =
        if (
          length(
            selected_distances
          ) > 0
        ) {
          
          max(
            selected_distances
          )
          
        } else {
          
          NA_real_
        }
    )
  }
  
  
  
  list(
    
    steps =
      steps,
    
    used =
      used
  )
}



# ============================================================
# 15. EJECUTAR GREEDY
# ============================================================

prototype_steps_list <- list()

ball_prevalence_list <- list()

coverage_summary_list <- list()


run_counter <- 0L



# ------------------------------------------------------------
# Una combinación por:
#
# método × matriz × K × D_DET
# ------------------------------------------------------------

groups <- pattern_frequency %>%
  
  distinct(
    
    method,
    
    matrix_name,
    
    k_candidate,
    
    d_det
  ) %>%
  
  arrange(
    
    method,
    
    matrix_name,
    
    k_candidate,
    
    d_det
  )



for (
  g in seq_len(
    nrow(groups)
  )
) {
  
  
  group_current <-
    groups[
      g,
    ]
  
  
  
  pattern_df <- pattern_frequency %>%
    
    filter(
      
      method ==
        group_current$method,
      
      
      matrix_name ==
        group_current$matrix_name,
      
      
      k_candidate ==
        group_current$k_candidate,
      
      
      d_det ==
        group_current$d_det
    ) %>%
    
    arrange(
      
      desc(N),
      
      pattern_key
    )
  
  
  
  pattern_matrix <- patterns_to_matrix(
    
    pattern_df$
      pattern_key
  )
  
  
  
  frequencies <-
    pattern_df$N
  
  
  
  total_frequency <-
    sum(
      frequencies
    )
  
  
  
  n_unique_patterns <-
    nrow(
      pattern_df
    )
  
  
  
  # ==========================================================
  # Nº máximo de prototipos = K que estamos evaluando
  # ==========================================================
  
  n_prototypes_current <-
    
    as.integer(
      group_current$
        k_candidate
    )
  
  
  
  # ==========================================================
  # DISTANCIA HAMMING
  # ==========================================================
  
  for (
    d_hamming_current in
    D_HAMMING_GRID
  ) {
    
    
    run_counter <-
      run_counter +
      1L
    
    
    
    result <- run_greedy_sequence(
      
      patterns =
        pattern_matrix,
      
      
      frequencies =
        frequencies,
      
      
      n_prototypes =
        n_prototypes_current,
      
      
      d_hamming =
        d_hamming_current
    )
    
    
    
    # ========================================================
    # 15A. COBERTURA PASO A PASO
    # ========================================================
    
    steps_df <- map_dfr(
      
      result$steps,
      
      function(step) {
        
        
        if (is.null(step)) {
          
          return(NULL)
        }
        
        
        
        active_center <- determinants[
          
          step$center ==
            1
        ]
        
        
        
        tibble(
          
          method =
            group_current$method,
          
          
          matrix_name =
            group_current$matrix_name,
          
          
          k_candidate =
            group_current$k_candidate,
          
          
          d_det =
            group_current$d_det,
          
          
          d_hamming =
            d_hamming_current,
          
          
          prototype =
            step$prototype,
          
          
          center_pattern_key =
            paste0(
              
              step$center,
              
              collapse = ""
            ),
          
          
          center_active_determinants =
            paste(
              
              active_center,
              
              collapse = "; "
            ),
          
          
          center_frequency =
            step$
            center_frequency,
          
          
          center_frequency_pct =
            100 *
            step$
            center_frequency /
            total_frequency,
          
          
          n_unique_patterns_in_ball =
            step$
            n_unique_patterns_in_ball,
          
          
          incremental_covered_frequency =
            step$
            incremental_frequency,
          
          
          incremental_covered_pct =
            100 *
            step$
            incremental_frequency /
            total_frequency,
          
          
          cumulative_covered_frequency =
            step$
            cumulative_frequency,
          
          
          cumulative_covered_pct =
            100 *
            step$
            cumulative_frequency /
            total_frequency,
          
          
          weighted_mean_hamming =
            step$
            weighted_mean_hamming,
          
          
          max_hamming_in_ball =
            step$
            max_hamming_in_ball,
          
          
          total_pattern_occurrences =
            total_frequency,
          
          
          n_unique_patterns =
            n_unique_patterns,
          
          
          n_bootstraps =
            length(
              boot_ids
            )
        )
      }
    )
    
    
    
    prototype_steps_list[[
      run_counter
    ]] <- steps_df
    
    
    
    # ========================================================
    # 15B. % DETERMINANTES DENTRO DE CADA BOLA
    # ========================================================
    
    ball_df <- map_dfr(
      
      result$steps,
      
      function(step) {
        
        
        if (is.null(step)) {
          
          return(NULL)
        }
        
        
        
        tibble(
          
          method =
            group_current$method,
          
          
          matrix_name =
            group_current$matrix_name,
          
          
          k_candidate =
            group_current$k_candidate,
          
          
          d_det =
            group_current$d_det,
          
          
          d_hamming =
            d_hamming_current,
          
          
          prototype =
            step$prototype,
          
          
          determinant =
            determinants,
          
          
          # ¿Está el determinante en el centro?
          center_selected =
            as.integer(
              step$center
            ),
          
          
          # Proporción dentro de la bola
          prop_active_in_ball =
            as.numeric(
              step$
                determinant_prevalence
            ),
          
          
          pct_active_in_ball =
            100 *
            prop_active_in_ball,
          
          
          covered_frequency =
            step$
            incremental_frequency,
          
          
          n_unique_patterns_in_ball =
            step$
            n_unique_patterns_in_ball
        )
      }
    )
    
    
    
    ball_prevalence_list[[
      run_counter
    ]] <- ball_df
    
    
    
    # ========================================================
    # 15C. COBERTURA FINAL DE ESTE K
    # ========================================================
    
    if (nrow(steps_df) > 0) {
      
      
      last_step <- steps_df %>%
        
        slice_tail(
          n = 1
        )
      
      
      
      coverage_summary_list[[
        run_counter
      ]] <- tibble(
        
        method =
          group_current$method,
        
        
        matrix_name =
          group_current$matrix_name,
        
        
        k_candidate =
          group_current$k_candidate,
        
        
        d_det =
          group_current$d_det,
        
        
        d_hamming =
          d_hamming_current,
        
        
        n_prototypes_used =
          max(
            steps_df$
              prototype
          ),
        
        
        n_bootstraps =
          length(
            boot_ids
          ),
        
        
        n_unique_patterns =
          n_unique_patterns,
        
        
        total_pattern_occurrences =
          total_frequency,
        
        
        final_covered_frequency =
          last_step$
          cumulative_covered_frequency,
        
        
        final_covered_pct =
          last_step$
          cumulative_covered_pct,
        
        
        final_uncovered_frequency =
          total_frequency -
          last_step$
          cumulative_covered_frequency,
        
        
        final_uncovered_pct =
          100 -
          last_step$
          cumulative_covered_pct,
        
        
        first_prototype_covered_pct =
          steps_df$
          incremental_covered_pct[
            steps_df$prototype == 1
          ][1],
        
        
        last_prototype_increment_pct =
          last_step$
          incremental_covered_pct
      )
      
      
    } else {
      
      
      coverage_summary_list[[
        run_counter
      ]] <- tibble(
        
        method =
          group_current$method,
        
        
        matrix_name =
          group_current$matrix_name,
        
        
        k_candidate =
          group_current$k_candidate,
        
        
        d_det =
          group_current$d_det,
        
        
        d_hamming =
          d_hamming_current,
        
        
        n_prototypes_used =
          0L,
        
        
        n_bootstraps =
          length(
            boot_ids
          ),
        
        
        n_unique_patterns =
          n_unique_patterns,
        
        
        total_pattern_occurrences =
          total_frequency,
        
        
        final_covered_frequency =
          0,
        
        
        final_covered_pct =
          0,
        
        
        final_uncovered_frequency =
          total_frequency,
        
        
        final_uncovered_pct =
          100,
        
        
        first_prototype_covered_pct =
          NA_real_,
        
        
        last_prototype_increment_pct =
          NA_real_
      )
    }
  }
}



# ============================================================
# 16. UNIR RESULTADOS
# ============================================================

greedy_prototype_steps <- bind_rows(
  
  prototype_steps_list
)



greedy_ball_determinant_prevalence <- bind_rows(
  
  ball_prevalence_list
)



greedy_coverage_summary <- bind_rows(
  
  coverage_summary_list
)



# ============================================================
# 17. AÑADIR INTERPRETACIÓN DE HAMMING
# ============================================================

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



greedy_coverage_summary <-
  
  greedy_coverage_summary %>%
  
  left_join(
    
    hamming_interpretation,
    
    by = c(
      
      "d_det",
      
      "d_hamming"
    )
  )



# ============================================================
# 18. COMPARACIÓN DE K = 4,...,8
# ============================================================
#
# Para cada:
#
# método
# matriz
# D_DET
# D_HAMMING
#
# calculamos cuánto cambia la cobertura al pasar:
#
# K4 -> K5
# K5 -> K6
# K6 -> K7
# K7 -> K8
#
#
# Esto permite buscar rendimientos decrecientes.
#
# NO seleccionamos K automáticamente.
# ============================================================

greedy_k_comparison <- greedy_coverage_summary %>%
  
  group_by(
    
    method,
    
    matrix_name,
    
    d_det,
    
    d_hamming
  ) %>%
  
  arrange(
    
    k_candidate,
    
    .by_group = TRUE
  ) %>%
  
  mutate(
    
    delta_coverage_vs_previous_k =
      
      final_covered_pct -
      
      lag(
        final_covered_pct
      ),
    
    
    delta_unique_patterns_vs_previous_k =
      
      n_unique_patterns -
      
      lag(
        n_unique_patterns
      )
  ) %>%
  
  ungroup()



# ============================================================
# 19. DIAGNÓSTICO DE UMBRALES DE COBERTURA
# ============================================================
#
# Pregunta:
#
# ¿con cuántos prototipos llegamos por primera vez a
# 80%, 85%, 90% o 95%?
#
#
# Esto NO decide automáticamente el mejor K.
# ============================================================

coverage_threshold_table <-
  
  greedy_prototype_steps %>%
  
  select(
    
    method,
    
    matrix_name,
    
    k_candidate,
    
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
    
    method,
    
    matrix_name,
    
    k_candidate,
    
    d_det,
    
    d_hamming,
    
    coverage_threshold
  ) %>%
  
  summarise(
    
    first_prototype_reaching_threshold =
      
      if (
        
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
    
    
    max_coverage_available =
      
      max(
        
        cumulative_covered_pct,
        
        na.rm = TRUE
      ),
    
    
    .groups =
      "drop"
  )



# ============================================================
# 20. SALIDA COMPACTA MATRIZ × MÉTODO
# ============================================================
#
# Esto genera una tabla tipo:
#
# K | D_DET | HAMMING | EFA_RAW | KMEANS_RAW | ...
#
# útil para la comparación posterior.
# ============================================================

greedy_matrix_x_method <-
  
  greedy_coverage_summary %>%
  
  mutate(
    
    matrix_short =
      case_when(
        
        matrix_name ==
          "matrix_32_raw_0_1" ~
          "RAW",
        
        matrix_name ==
          "matrix_32_pos_0_1" ~
          "POS",
        
        matrix_name ==
          "matrix_32_ext_0_1" ~
          "EXT",
        
        matrix_name ==
          "matrix_32_z_abs" ~
          "Z_ABS",
        
        TRUE ~
          matrix_name
      ),
    
    
    method_matrix =
      paste0(
        method,
        "_",
        matrix_short
      )
  ) %>%
  
  select(
    
    k_candidate,
    
    d_det,
    
    d_hamming,
    
    method_matrix,
    
    final_covered_pct
  ) %>%
  
  pivot_wider(
    
    names_from =
      method_matrix,
    
    values_from =
      final_covered_pct
  ) %>%
  
  arrange(
    
    d_det,
    
    d_hamming,
    
    k_candidate
  )



# ============================================================
# 21. GUARDAR RESULTADOS
# ============================================================


# ------------------------------------------------------------
# Patrones individuales
# ------------------------------------------------------------

write_csv(
  
  binary_patterns,
  
  file.path(
    
    out_dir,
    
    "01_binary_patterns.csv"
  )
)



# ------------------------------------------------------------
# Frecuencia de patrones
# ------------------------------------------------------------

write_csv(
  
  pattern_frequency,
  
  file.path(
    
    out_dir,
    
    "02_pattern_frequency.csv"
  )
)



# ------------------------------------------------------------
# Cobertura paso a paso de cada prototipo
# ------------------------------------------------------------

write_csv(
  
  greedy_prototype_steps,
  
  file.path(
    
    out_dir,
    
    "03_greedy_prototype_steps.csv"
  )
)



# ------------------------------------------------------------
# Cobertura final por combinación
# ------------------------------------------------------------

write_csv(
  
  greedy_coverage_summary,
  
  file.path(
    
    out_dir,
    
    "04_greedy_coverage_summary.csv"
  )
)



# ------------------------------------------------------------
# % de determinantes dentro de cada bola
# ------------------------------------------------------------

write_csv(
  
  greedy_ball_determinant_prevalence,
  
  file.path(
    
    out_dir,
    
    "05_greedy_ball_determinant_prevalence.csv"
  )
)



# ------------------------------------------------------------
# Comparación K = 4,...,8
# ------------------------------------------------------------

write_csv(
  
  greedy_k_comparison,
  
  file.path(
    
    out_dir,
    
    "06_greedy_k_comparison.csv"
  )
)



# Interpretación de Hamming
write_csv(
  
  hamming_interpretation,
  
  file.path(
    
    out_dir,
    
    "07_hamming_interpretation.csv"
  )
)

# Diagnóstico de cobertura
write_csv(
  
  coverage_threshold_table,
  
  file.path(
    
    out_dir,
    
    "08_coverage_threshold_diagnostic.csv"
  )
)

write_csv(
  
  score_rank_summary,
  
  file.path(
    out_dir,
    "11_score_by_determinant_rank.csv"
  )
)

# ============================================================
# 22. PARÁMETROS
# ============================================================

parameters <- tibble(
  
  parameter = c(
    
    "D_DET_GRID",
    
    "D_HAMMING_GRID",
    
    "MAX_BOOTSTRAPS",
    
    "COVERAGE_THRESHOLDS",
    
    "KMEANS_SCORE",
    
    "EFA_SCORE"
  ),
  
  
  value = c(
    
    paste(
      D_DET_GRID,
      collapse = ","
    ),
    
    paste(
      D_HAMMING_GRID,
      collapse = ","
    ),
    
    as.character(
      MAX_BOOTSTRAPS
    ),
    
    paste(
      COVERAGE_THRESHOLDS,
      collapse = ","
    ),
    
    "RAW=abs(center-0.5); POS=max(center-0.5,0); EXT=center; Z_ABS=center",
    
    "abs(loading)"
  )
)



write_csv(
  
  parameters,
  
  file.path(
    
    out_dir,
    
    "09_greedy_parameters.csv"
  )
)



# ------------------------------------------------------------
# Matriz × método
# ------------------------------------------------------------

write_csv(
  
  greedy_matrix_x_method,
  
  file.path(
    
    out_dir,
    
    "10_greedy_matrix_x_method.csv"
  )
)



# ============================================================
# 23. FIGURAS
# ============================================================
#
# Una figura por:
#
# método × matriz
#
# Eje X:
#   distancia Hamming
#
# Eje Y:
#   cobertura final
#
# Líneas:
#   K = 4,...,8
#
# Paneles:
#   D_DET
#
# ============================================================

for (
  
  method_current in
  unique(
    greedy_coverage_summary$
    method
  )
  
) {
  
  
  for (
    
    matrix_current in
    unique(
      greedy_coverage_summary$
      matrix_name
    )
    
  ) {
    
    
    plot_data <- greedy_coverage_summary %>%
      
      filter(
        
        method ==
          method_current,
        
        matrix_name ==
          matrix_current
      )
    
    
    
    if (nrow(plot_data) == 0) {
      
      next
    }
    
    
    
    p <- ggplot(
      
      plot_data,
      
      aes(
        
        x =
          d_hamming,
        
        y =
          final_covered_pct,
        
        group =
          factor(
            k_candidate
          ),
        
        color =
          factor(
            k_candidate
          )
      )
      
    ) +
      
      geom_line() +
      
      geom_point(
        size = 1.3
      ) +
      
      facet_wrap(
        
        ~ d_det,
        
        ncol = 4
      ) +
      
      theme_minimal(
        base_size = 10
      ) +
      
      labs(
        
        title =
          paste0(
            
            "Greedy coverage | ",
            
            method_current,
            
            " | ",
            
            matrix_current
          ),
        
        
        subtitle =
          paste0(
            
            length(boot_ids),
            
            " bootstrap samples"
          ),
        
        
        x =
          "Hamming radius",
        
        
        y =
          "Final coverage (%)",
        
        
        color =
          "K"
      )
    
    
    
    ggsave(
      
      filename = file.path(
        
        fig_dir,
        
        paste0(
          
          "coverage_",
          
          tolower(
            method_current
          ),
          
          "_",
          
          matrix_current,
          
          ".png"
        )
      ),
      
      
      plot =
        p,
      
      
      width =
        12,
      
      
      height =
        8,
      
      
      dpi =
        300
    )
  }
}



# ============================================================
# 24. RESUMEN EN CONSOLA
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "08. GREEDY K-MEANS + EFA COMPLETADO\n"
)

cat(
  "============================================================\n"
)



cat(
  "\nBootstraps usados: ",
  length(boot_ids),
  "\n",
  sep = ""
)



cat(
  "\nD determinantes:\n"
)

print(
  D_DET_GRID
)



cat(
  "\nD Hamming:\n"
)

print(
  D_HAMMING_GRID
)



cat(
  "\nInterpretación de Hamming para D_DET = 8:\n"
)

print(
  
  hamming_interpretation %>%
    
    filter(
      d_det == 8
    ),
  
  n = Inf,
  
  width = Inf
)



cat(
  "\n============================================================\n"
)

cat(
  "COBERTURA FINAL - PRIMERAS FILAS\n"
)

cat(
  "============================================================\n"
)



print(
  
  greedy_coverage_summary %>%
    
    arrange(
      
      method,
      
      matrix_name,
      
      d_det,
      
      d_hamming,
      
      k_candidate
    ) %>%
    
    head(
      40
    ),
  
  n = 40,
  
  width = Inf
)



cat(
  "\n============================================================\n"
)

cat(
  "COMPARACIÓN K - EJEMPLO D_DET = 8\n"
)

cat(
  "============================================================\n"
)



print(
  
  greedy_k_comparison %>%
    
    filter(
      d_det == 8
    ) %>%
    
    arrange(
      
      method,
      
      matrix_name,
      
      d_hamming,
      
      k_candidate
    ) %>%
    
    head(
      80
    ),
  
  n = 80,
  
  width = Inf
)



cat(
  "\n============================================================\n"
)

cat(
  "OUTPUTS\n"
)

cat(
  "============================================================\n"
)



cat(
  "\nResultados guardados en:\n"
)

cat(
  out_dir,
  "\n"
)



message(
  "\nListo."
)