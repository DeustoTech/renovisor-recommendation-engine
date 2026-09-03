


# ============================================================
# 10_compare_efa_kmeans.R
# ============================================================
#
# OBJETIVO
# --------
#
# Comparar los prototipos obtenidos mediante:
#
#   - K-MEANS
#   - EFA
#
# después del Greedy.
#
#
# Hasta ahora tenemos, para cada:
#
#   matriz
#   K = 4,...,8
#   D_DET = 8,...,15
#   D_HAMMING = 0,...,10
#
# una serie de prototipos binarios:
#
#   1 0 0 1 1 0 ... 1
#
# donde:
#
#   1 = determinante seleccionado
#   0 = determinante no seleccionado
#
#
# La pregunta ahora es:
#
# ¿Los prototipos encontrados por EFA y K-means
# contienen determinantes similares?
#
#
# Ejemplo:
#
# KMEANS P1:
#
# A B C D E F G H
#
# EFA P4:
#
# A B C D E X G H
#
# Tienen 7 de 8 determinantes comunes:
#
# similitud = 7 / 8 * 100 = 87.5%
#
#
# ============================================================
# MATCHING
# ============================================================
#
# No podemos comparar simplemente:
#
# KMEANS P1 <-> EFA P1
# KMEANS P2 <-> EFA P2
#
# porque los números de prototipo son arbitrarios.
#
#
# Tampoco podemos coger simplemente el máximo de cada fila,
# porque podría pasar:
#
# KMEANS P1 -> EFA P3
# KMEANS P2 -> EFA P3
#
# Es decir, dos perfiles K-means podrían querer emparejarse
# con el mismo perfil EFA.
#
#
# Por eso usamos un MATCHING UNO-A-UNO óptimo mediante
# el algoritmo húngaro.
#
# Buscamos la combinación de parejas que maximiza
# la similitud TOTAL entre EFA y K-means.
#
#
# ============================================================
# IMPORTANTE
# ============================================================
#
# Un factor EFA NO es un cluster de personas.
#
# La comparación NO pretende demostrar:
#
#       EFA factor 1 = K-means cluster 1
#
# Lo que buscamos es convergencia:
#
# ¿dos métodos distintos identifican firmas de
# determinantes parecidas?
#
#
# ============================================================
# SALIDAS PRINCIPALES
# ============================================================
#
# 01_pairwise_similarity.csv
#
#   Todas las comparaciones posibles:
#
#   KM1 vs EFA1
#   KM1 vs EFA2
#   ...
#
#
# 02_optimal_matching.csv
#
#   Emparejamiento óptimo:
#
#   KM1 <-> EFA4
#   KM2 <-> EFA1
#   ...
#
#
# 03_solution_summary.csv
#
#   Para cada matriz/K/D/Hamming:
#
#   - similitud media
#   - similitud mínima
#   - similitud máxima
#   - nº de perfiles con >=75% similitud
#   - perfiles no emparejados
#
#
# 04_convergence_and_greedy.csv
#
#   Une:
#
#   similitud EFA-Kmeans
#
#            +
#
#   estabilidad Greedy EFA
#   estabilidad Greedy Kmeans
#
#
# Esto será lo que utilizaremos para decidir:
#
# K
# D_DET
# D_HAMMING
# matriz principal
#
# ============================================================



suppressPackageStartupMessages({
  
  library(tidyverse)
  
  library(readr)
  
  library(stringr)
  
  library(ggplot2)
})



# ============================================================
# 0. COMPROBAR PAQUETE PARA MATCHING HÚNGARO
# ============================================================
#
# Utilizamos clue::solve_LSAP()
#
# Si no está instalado:
#
# install.packages("clue")
#
# ============================================================

if (!requireNamespace(
  "clue",
  quietly = TRUE
)) {
  
  stop(
    paste0(
      "\nNecesitas instalar el paquete 'clue'.\n\n",
      "Ejecuta una vez:\n\n",
      "install.packages(\"clue\")\n\n",
      "y después vuelve a ejecutar este script."
    )
  )
}



# ============================================================
# 1. RUTAS
# ============================================================

project_root <- path.expand(
  "~/Desktop/MASTER/recommendation-engine/TFM"
)


processed_root <- file.path(
  project_root,
  "paper1_cluster/data/processed"
)



# ------------------------------------------------------------
# Resultados Greedy
# ------------------------------------------------------------

greedy_dir <- file.path(
  processed_root,
  "08_greedy_kmeans_efa"
)



# ------------------------------------------------------------
# OUTPUT
# ------------------------------------------------------------

out_dir <- file.path(
  processed_root,
  "10_compare_efa_kmeans"
)


fig_dir <- file.path(
  out_dir,
  "figures"
)


fig_matching_dir <- file.path(
  fig_dir,
  "matching"
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


dir.create(
  fig_matching_dir,
  recursive = TRUE,
  showWarnings = FALSE
)



# ============================================================
# 2. PARÁMETROS
# ============================================================


# ------------------------------------------------------------
# Los radios Hamming impares son redundantes.
#
# Ejemplo:
#
# H=0 = H=1
# H=2 = H=3
# H=4 = H=5
#
# porque todos los patrones tienen exactamente D unos.
#
# Por tanto, para esta comparación usamos:
#
# 0,2,4,6,8,10
# ------------------------------------------------------------

USE_EVEN_HAMMING_ONLY <- TRUE



# ------------------------------------------------------------
# Umbral DESCRIPTIVO de similitud
# ------------------------------------------------------------
#
# Si dos perfiles comparten al menos el 75% de sus
# determinantes los consideraremos, a efectos descriptivos,
# una correspondencia bastante fuerte.
#
# NO significa que 75% sea una frontera estadística universal.
# ------------------------------------------------------------

GOOD_SIMILARITY_THRESHOLD <- 75



# ------------------------------------------------------------
# Configuración de referencia para gráficos detallados
# ------------------------------------------------------------
#
# Todavía NO son parámetros finales.
#
# K=6 es nuestra hipótesis principal.
#
# D=8/H=4 se utiliza como ejemplo interpretable:
#
# 6 de 8 determinantes comunes = 75%
# ------------------------------------------------------------

REFERENCE_K <- 6

REFERENCE_D_DET <- 8

REFERENCE_D_HAMMING <- 4



# ============================================================
# 3. LEER RESULTADOS GREEDY
# ============================================================

greedy_steps_file <- file.path(
  greedy_dir,
  "03_greedy_prototype_steps.csv"
)


greedy_coverage_file <- file.path(
  greedy_dir,
  "04_greedy_coverage_summary.csv"
)


hamming_file <- file.path(
  greedy_dir,
  "07_hamming_interpretation.csv"
)



if (!file.exists(greedy_steps_file)) {
  
  stop(
    "No encuentro: ",
    greedy_steps_file
  )
}


if (!file.exists(greedy_coverage_file)) {
  
  stop(
    "No encuentro: ",
    greedy_coverage_file
  )
}


if (!file.exists(hamming_file)) {
  
  stop(
    "No encuentro: ",
    hamming_file
  )
}



greedy_steps <- read_csv(
  greedy_steps_file,
  show_col_types = FALSE
)


greedy_coverage <- read_csv(
  greedy_coverage_file,
  show_col_types = FALSE
)


hamming_interpretation <- read_csv(
  hamming_file,
  show_col_types = FALSE
)



# ============================================================
# 4. COMPROBAR COLUMNAS
# ============================================================

required_steps <- c(
  
  "method",
  
  "matrix_name",
  
  "k_candidate",
  
  "d_det",
  
  "d_hamming",
  
  "prototype",
  
  "center_pattern_key",
  
  "center_active_determinants"
)



missing_steps <- setdiff(
  required_steps,
  names(greedy_steps)
)



if (length(missing_steps) > 0) {
  
  stop(
    "Faltan columnas en greedy_prototype_steps: ",
    paste(
      missing_steps,
      collapse = ", "
    )
  )
}



# ============================================================
# 5. LIMPIAR TIPOS
# ============================================================

greedy_steps <- greedy_steps %>%
  
  mutate(
    
    k_candidate =
      as.integer(k_candidate),
    
    d_det =
      as.integer(d_det),
    
    d_hamming =
      as.integer(d_hamming),
    
    prototype =
      as.integer(prototype),
    
    center_pattern_key =
      as.character(center_pattern_key)
  )



greedy_coverage <- greedy_coverage %>%
  
  mutate(
    
    k_candidate =
      as.integer(k_candidate),
    
    d_det =
      as.integer(d_det),
    
    d_hamming =
      as.integer(d_hamming)
  )



# ============================================================
# 6. QUITAR HAMMING IMPARES
# ============================================================

if (USE_EVEN_HAMMING_ONLY) {
  
  greedy_steps <- greedy_steps %>%
    
    filter(
      d_hamming %% 2 == 0
    )
  
  
  greedy_coverage <- greedy_coverage %>%
    
    filter(
      d_hamming %% 2 == 0
    )
}



# ============================================================
# 7. FUNCIONES AUXILIARES
# ============================================================


# ------------------------------------------------------------
# Nombre corto de las matrices
# ------------------------------------------------------------

matrix_label <- function(x) {
  
  case_when(
    
    x ==
      "matrix_32_raw_0_1" ~
      "RAW",
    
    x ==
      "matrix_32_pos_0_1" ~
      "POS",
    
    x ==
      "matrix_32_ext_0_1" ~
      "EXT",
    
    x ==
      "matrix_32_z_abs" ~
      "Z_ABS",
    
    TRUE ~
      x
  )
}



# ------------------------------------------------------------
# Convertir:
#
# "1000100..."
#
# en:
#
# 1 0 0 0 1 0 0 ...
#
# ------------------------------------------------------------

pattern_to_vector <- function(pattern_key) {
  
  vec <- as.integer(
    strsplit(
      pattern_key,
      ""
    )[[1]]
  )
  
  
  if (length(vec) != 32) {
    
    stop(
      "He encontrado un patrón que no tiene 32 posiciones: ",
      pattern_key
    )
  }
  
  
  vec
}



# ============================================================
# 8. CALCULAR SIMILITUD ENTRE DOS PATRONES
# ============================================================
#
# Como ambos patrones tienen exactamente D unos:
#
# common =
#   nº determinantes seleccionados en ambos perfiles
#
#
# similarity =
#
#     common
#   ---------- × 100
#       D
#
#
# También guardamos distancia Hamming:
#
# número de posiciones distintas.
# ============================================================

compare_two_patterns <- function(
    pattern_kmeans,
    pattern_efa,
    d_det
) {
  
  km_vec <- pattern_to_vector(
    pattern_kmeans
  )
  
  
  efa_vec <- pattern_to_vector(
    pattern_efa
  )
  
  
  common <- sum(
    km_vec == 1 &
      efa_vec == 1
  )
  
  
  hamming <- sum(
    km_vec !=
      efa_vec
  )
  
  
  similarity_pct <-
    100 *
    common /
    d_det
  
  
  tibble(
    
    n_common_determinants =
      common,
    
    similarity_pct =
      similarity_pct,
    
    hamming_between_centers =
      hamming
  )
}



# ============================================================
# 9. COMPARAR TODAS LAS PAREJAS DE UNA SOLUCIÓN
# ============================================================
#
# Ejemplo K=6:
#
# tendremos:
#
#          EFA
#       1 2 3 4 5 6
#
# KM 1
#    2
#    3
#    4
#    5
#    6
#
#
# Calculamos las 36 combinaciones.
# ============================================================

make_pairwise_similarity <- function(
    km_df,
    efa_df,
    d_det
) {
  
  
  km_ids <- sort(
    unique(
      km_df$prototype
    )
  )
  
  
  efa_ids <- sort(
    unique(
      efa_df$prototype
    )
  )
  
  
  pair_grid <- crossing(
    
    kmeans_prototype =
      km_ids,
    
    efa_prototype =
      efa_ids
  )
  
  
  
  pair_grid <- pair_grid %>%
    
    rowwise() %>%
    
    mutate(
      
      comparison =
        list(
          
          compare_two_patterns(
            
            pattern_kmeans =
              
              km_df$
              center_pattern_key[
                km_df$prototype ==
                  kmeans_prototype
              ][1],
            
            pattern_efa =
              
              efa_df$
              center_pattern_key[
                efa_df$prototype ==
                  efa_prototype
              ][1],
            
            d_det =
              d_det
          )
        )
    ) %>%
    
    unnest(
      comparison
    ) %>%
    
    ungroup()
  
  
  
  pair_grid
}



# ============================================================
# 10. MATCHING ÓPTIMO
# ============================================================
#
# Utilizamos el algoritmo húngaro.
#
#
# IMPORTANTE:
#
# Greedy puede haber cubierto todo antes de utilizar
# K prototipos.
#
# Ejemplo:
#
# K candidato = 6
#
# Kmeans obtiene 6 prototipos
# EFA solo necesita 5 porque ya cubrió todo.
#
#
# En ese caso hay un perfil "sin correspondencia".
#
#
# Para penalizar esto construimos una matriz K × K.
#
# Las posiciones que no corresponden a perfiles reales
# tienen similitud 0.
#
# Así una solución con perfiles desaparecidos/no emparejados
# no obtiene artificialmente una media alta.
# ============================================================

optimal_matching <- function(
    pairwise_df,
    km_df,
    efa_df,
    k_candidate,
    d_det
) {
  
  
  km_ids <- sort(
    unique(
      km_df$prototype
    )
  )
  
  
  efa_ids <- sort(
    unique(
      efa_df$prototype
    )
  )
  
  
  n_km <- length(
    km_ids
  )
  
  
  n_efa <- length(
    efa_ids
  )
  
  
  
  # ----------------------------------------------------------
  # Creamos matriz K × K llena inicialmente de ceros
  # ----------------------------------------------------------
  
  similarity_matrix <- matrix(
    
    0,
    
    nrow =
      k_candidate,
    
    ncol =
      k_candidate
  )
  
  
  
  # ----------------------------------------------------------
  # Introducir las similitudes reales
  # ----------------------------------------------------------
  
  for (
    i in seq_len(n_km)
  ) {
    
    for (
      j in seq_len(n_efa)
    ) {
      
      
      similarity_matrix[
        i,
        j
      ] <-
        
        pairwise_df %>%
        
        filter(
          
          kmeans_prototype ==
            km_ids[i],
          
          efa_prototype ==
            efa_ids[j]
        ) %>%
        
        pull(
          similarity_pct
        ) %>%
        
        first()
    }
  }
  
  
  
  # ----------------------------------------------------------
  # Algoritmo húngaro
  #
  # maximum = TRUE:
  #
  # queremos MAXIMIZAR la similitud total.
  # ----------------------------------------------------------
  
  assignment <- clue::solve_LSAP(
    
    similarity_matrix,
    
    maximum = TRUE
  )
  
  
  
  assignment <- as.integer(
    assignment
  )
  
  
  
  # ----------------------------------------------------------
  # Construir tabla de correspondencias
  # ----------------------------------------------------------
  
  matching <- map_dfr(
    
    seq_len(
      k_candidate
    ),
    
    function(i) {
      
      
      j <- assignment[i]
      
      
      real_km <-
        i <= n_km
      
      
      real_efa <-
        j <= n_efa
      
      
      is_real_match <-
        real_km &&
        real_efa
      
      
      
      similarity <-
        
        similarity_matrix[
          i,
          j
        ]
      
      
      
      # ------------------------------------------------------
      # Si la pareja es real podemos calcular nº comunes
      # ------------------------------------------------------
      
      common <- if (
        is_real_match
      ) {
        
        round(
          similarity *
            d_det /
            100
        )
        
      } else {
        
        0
      }
      
      
      
      tibble(
        
        kmeans_prototype =
          
          if (
            real_km
          ) {
            
            km_ids[i]
            
          } else {
            
            NA_integer_
          },
        
        
        efa_prototype =
          
          if (
            real_efa
          ) {
            
            efa_ids[j]
            
          } else {
            
            NA_integer_
          },
        
        
        similarity_pct =
          similarity,
        
        
        n_common_determinants =
          common,
        
        
        is_real_match =
          is_real_match,
        
        
        is_dummy_match =
          !is_real_match
      )
    }
  )
  
  
  
  matching
}



# ============================================================
# 11. IDENTIFICAR SOLUCIONES PRESENTES EN AMBOS MÉTODOS
# ============================================================

groups <- greedy_steps %>%
  
  distinct(
    
    method,
    
    matrix_name,
    
    k_candidate,
    
    d_det,
    
    d_hamming
  ) %>%
  
  count(
    
    matrix_name,
    
    k_candidate,
    
    d_det,
    
    d_hamming,
    
    name =
      "n_methods"
  ) %>%
  
  filter(
    n_methods == 2
  ) %>%
  
  arrange(
    
    matrix_name,
    
    k_candidate,
    
    d_det,
    
    d_hamming
  )



cat(
  "\nNúmero de combinaciones EFA/K-means a comparar: ",
  nrow(groups),
  "\n",
  sep = ""
)



# ============================================================
# 12. EJECUTAR COMPARACIÓN
# ============================================================

pairwise_list <- list()

matching_list <- list()


run_counter <- 0L



for (
  g in seq_len(
    nrow(groups)
  )
) {
  
  
  # ----------------------------------------------------------
  # Extraer valores escalares de la combinación actual
  # ----------------------------------------------------------
  
  matrix_current <-
    groups$
    matrix_name[[g]]
  
  
  k_current <-
    groups$
    k_candidate[[g]]
  
  
  d_det_current <-
    groups$
    d_det[[g]]
  
  
  d_hamming_current <-
    groups$
    d_hamming[[g]]
  
  
  
  # ----------------------------------------------------------
  # Prototipos K-means
  # ----------------------------------------------------------
  
  km_df <- greedy_steps %>%
    
    filter(
      
      method ==
        "KMEANS",
      
      matrix_name ==
        matrix_current,
      
      k_candidate ==
        k_current,
      
      d_det ==
        d_det_current,
      
      d_hamming ==
        d_hamming_current
    ) %>%
    
    select(
      
      prototype,
      
      center_pattern_key,
      
      center_active_determinants
    ) %>%
    
    distinct() %>%
    
    arrange(
      prototype
    )
  
  
  
  # ----------------------------------------------------------
  # Prototipos EFA
  # ----------------------------------------------------------
  
  efa_df <- greedy_steps %>%
    
    filter(
      
      method ==
        "EFA",
      
      matrix_name ==
        matrix_current,
      
      k_candidate ==
        k_current,
      
      d_det ==
        d_det_current,
      
      d_hamming ==
        d_hamming_current
    ) %>%
    
    select(
      
      prototype,
      
      center_pattern_key,
      
      center_active_determinants
    ) %>%
    
    distinct() %>%
    
    arrange(
      prototype
    )
  
  
  
  # ----------------------------------------------------------
  # Si falta alguno de los métodos no continuamos
  # ----------------------------------------------------------
  
  if (
    nrow(km_df) == 0 ||
    nrow(efa_df) == 0
  ) {
    
    next
  }
  
  
  
  run_counter <-
    run_counter +
    1L
  
  
  
  # ==========================================================
  # TODAS LAS PAREJAS
  # ==========================================================
  
  pairwise_current <-
    
    make_pairwise_similarity(
      
      km_df =
        km_df,
      
      efa_df =
        efa_df,
      
      d_det =
        d_det_current
    ) %>%
    
    mutate(
      
      matrix_name =
        matrix_current,
      
      k_candidate =
        k_current,
      
      d_det =
        d_det_current,
      
      d_hamming =
        d_hamming_current,
      
      .before =
        1
    )
  
  
  
  # ==========================================================
  # MATCHING ÓPTIMO
  # ==========================================================
  
  matching_current <-
    
    optimal_matching(
      
      pairwise_df =
        pairwise_current,
      
      km_df =
        km_df,
      
      efa_df =
        efa_df,
      
      k_candidate =
        k_current,
      
      d_det =
        d_det_current
    ) %>%
    
    mutate(
      
      matrix_name =
        matrix_current,
      
      k_candidate =
        k_current,
      
      d_det =
        d_det_current,
      
      d_hamming =
        d_hamming_current,
      
      .before =
        1
    )
  
  
  
  # ----------------------------------------------------------
  # Marcar en pairwise qué parejas forman parte
  # del matching óptimo
  # ----------------------------------------------------------
  
  real_matches <- matching_current %>%
    
    filter(
      is_real_match
    ) %>%
    
    select(
      kmeans_prototype,
      efa_prototype
    ) %>%
    
    mutate(
      optimal_match =
        TRUE
    )
  
  
  
  pairwise_current <- pairwise_current %>%
    
    left_join(
      
      real_matches,
      
      by = c(
        
        "kmeans_prototype",
        
        "efa_prototype"
      )
    ) %>%
    
    mutate(
      
      optimal_match =
        replace_na(
          optimal_match,
          FALSE
        )
    )
  
  
  
  pairwise_list[[
    run_counter
  ]] <-
    pairwise_current
  
  
  matching_list[[
    run_counter
  ]] <-
    matching_current
}



# ============================================================
# 13. UNIR RESULTADOS
# ============================================================

pairwise_similarity <- bind_rows(
  pairwise_list
)


optimal_matches <- bind_rows(
  matching_list
)



# ============================================================
# 14. RESUMEN POR SOLUCIÓN
# ============================================================
#
# Dos tipos de medias:
#
#
# mean_similarity_actual
# ----------------------
#
# Media únicamente de las parejas reales.
#
#
# mean_similarity_penalized
# -------------------------
#
# Media sobre K posiciones.
#
# Si falta un perfil:
#
# similitud = 0
#
# Así penalizamos soluciones donde un método necesita
# menos perfiles que el otro.
#
#
# minimum similarity:
#
# especialmente importante para detectar:
#
# 90
# 87
# 85
# 82
# 80
# 25 <- perfil problemático
#
# ============================================================

solution_summary <- optimal_matches %>%
  
  group_by(
    
    matrix_name,
    
    k_candidate,
    
    d_det,
    
    d_hamming
  ) %>%
  
  summarise(
    
    n_expected_profiles =
      first(
        k_candidate
      ),
    
    
    n_actual_matches =
      sum(
        is_real_match
      ),
    
    
    n_dummy_matches =
      sum(
        is_dummy_match
      ),
    
    
    mean_similarity_penalized =
      mean(
        similarity_pct,
        na.rm = TRUE
      ),
    
    
    mean_similarity_actual =
      
      if (
        any(
          is_real_match
        )
      ) {
        
        mean(
          similarity_pct[
            is_real_match
          ],
          na.rm = TRUE
        )
        
      } else {
        
        NA_real_
      },
    
    
    min_similarity_actual =
      
      if (
        any(
          is_real_match
        )
      ) {
        
        min(
          similarity_pct[
            is_real_match
          ],
          na.rm = TRUE
        )
        
      } else {
        
        NA_real_
      },
    
    
    max_similarity_actual =
      
      if (
        any(
          is_real_match
        )
      ) {
        
        max(
          similarity_pct[
            is_real_match
          ],
          na.rm = TRUE
        )
        
      } else {
        
        NA_real_
      },
    
    
    mean_common_determinants =
      
      if (
        any(
          is_real_match
        )
      ) {
        
        mean(
          n_common_determinants[
            is_real_match
          ],
          na.rm = TRUE
        )
        
      } else {
        
        NA_real_
      },
    
    
    min_common_determinants =
      
      if (
        any(
          is_real_match
        )
      ) {
        
        min(
          n_common_determinants[
            is_real_match
          ],
          na.rm = TRUE
        )
        
      } else {
        
        NA_real_
      },
    
    
    n_matches_ge_75 =
      sum(
        
        is_real_match &
          similarity_pct >=
          GOOD_SIMILARITY_THRESHOLD
      ),
    
    
    pct_matches_ge_75 =
      
      if (
        sum(
          is_real_match
        ) > 0
      ) {
        
        100 *
          
          sum(
            
            is_real_match &
              similarity_pct >=
              GOOD_SIMILARITY_THRESHOLD
            
          ) /
          
          sum(
            is_real_match
          )
        
      } else {
        
        NA_real_
      },
    
    
    .groups =
      "drop"
  )



# ============================================================
# 15. AÑADIR COBERTURA GREEDY DE LOS DOS MÉTODOS
# ============================================================
#
# La similitud EFA/Kmeans por sí sola NO basta.
#
# Queremos una solución que sea:
#
#   A) parecida entre métodos
#
#          Y
#
#   B) estable dentro de cada método.
#
#
# Por eso unimos:
#
# final_covered_pct_EFA
# final_covered_pct_KMEANS
#
# ============================================================

coverage_compare <- greedy_coverage %>%
  
  select(
    
    method,
    
    matrix_name,
    
    k_candidate,
    
    d_det,
    
    d_hamming,
    
    final_covered_pct,
    
    first_prototype_covered_pct,
    
    last_prototype_increment_pct,
    
    n_unique_patterns
  ) %>%
  
  pivot_wider(
    
    names_from =
      method,
    
    values_from =
      c(
        
        final_covered_pct,
        
        first_prototype_covered_pct,
        
        last_prototype_increment_pct,
        
        n_unique_patterns
      ),
    
    names_glue =
      "{.value}_{method}"
  )



convergence_and_greedy <- solution_summary %>%
  
  left_join(
    
    coverage_compare,
    
    by = c(
      
      "matrix_name",
      
      "k_candidate",
      
      "d_det",
      
      "d_hamming"
    )
  ) %>%
  
  mutate(
    
    matrix =
      matrix_label(
        matrix_name
      ),
    
    
    # --------------------------------------------------------
    # Media de cobertura de ambos métodos
    # --------------------------------------------------------
    
    mean_coverage_both =
      
      rowMeans(
        
        cbind(
          
          final_covered_pct_EFA,
          
          final_covered_pct_KMEANS
        ),
        
        na.rm = TRUE
      ),
    
    
    # --------------------------------------------------------
    # Cobertura del método PEOR
    #
    # Esto es interesante porque evita que una cobertura
    # excelente de K-means esconda una mala cobertura EFA.
    # --------------------------------------------------------
    
    min_coverage_both =
      
      pmin(
        
        final_covered_pct_EFA,
        
        final_covered_pct_KMEANS,
        
        na.rm = TRUE
      )
  )



# ============================================================
# 16. AÑADIR INTERPRETACIÓN DE HAMMING
# ============================================================

hamming_unique <- hamming_interpretation %>%
  
  distinct(
    
    d_det,
    
    d_hamming,
    
    effective_hamming_radius,
    
    min_common_determinants,
    
    min_common_pct,
    
    odd_radius_redundant
  )



convergence_and_greedy <-
  
  convergence_and_greedy %>%
  
  left_join(
    
    hamming_unique,
    
    by = c(
      
      "d_det",
      
      "d_hamming"
    )
  )



# ============================================================
# 17. RADIO DE REFERENCIA ≈ 75% PARA CADA D
# ============================================================
#
# Elegimos, para cada D, el radio PAR más grande
# que todavía garantiza al menos 75% de determinantes comunes.
#
#
# Ejemplo:
#
# D=8:
#
# H=4 -> 6/8 = 75%
#
#
# D=12:
#
# H=6 -> 9/12 = 75%
#
#
# Esto NO decide el Hamming final.
#
# Solo nos permite hacer gráficos comparables entre D distintos.
# ============================================================

reference_hamming <- hamming_interpretation %>%
  
  filter(
    
    d_hamming %% 2 == 0,
    
    min_common_pct >=
      GOOD_SIMILARITY_THRESHOLD
  ) %>%
  
  group_by(
    d_det
  ) %>%
  
  slice_max(
    
    order_by =
      d_hamming,
    
    n =
      1,
    
    with_ties =
      FALSE
  ) %>%
  
  ungroup() %>%
  
  select(
    
    d_det,
    
    reference_d_hamming =
      d_hamming,
    
    reference_min_common_pct =
      min_common_pct
  )



reference_results <-
  
  convergence_and_greedy %>%
  
  left_join(
    
    reference_hamming,
    
    by =
      "d_det"
  ) %>%
  
  filter(
    
    d_hamming ==
      reference_d_hamming
  )



# ============================================================
# 18. GUARDAR CSV
# ============================================================

write_csv(
  
  pairwise_similarity,
  
  file.path(
    out_dir,
    "01_pairwise_similarity.csv"
  )
)



write_csv(
  
  optimal_matches,
  
  file.path(
    out_dir,
    "02_optimal_matching.csv"
  )
)



write_csv(
  
  solution_summary,
  
  file.path(
    out_dir,
    "03_solution_summary.csv"
  )
)



write_csv(
  
  convergence_and_greedy,
  
  file.path(
    out_dir,
    "04_convergence_and_greedy.csv"
  )
)



write_csv(
  
  reference_results,
  
  file.path(
    out_dir,
    "05_reference_75pct_results.csv"
  )
)



write_csv(
  
  reference_hamming,
  
  file.path(
    out_dir,
    "06_reference_hamming_by_D.csv"
  )
)



# ============================================================
# 19. GRÁFICO:
#     SIMILITUD MEDIA EFA ↔ KMEANS
# ============================================================
#
# Cada celda:
#
# K × D
#
# Color:
#
# similitud media del matching óptimo.
#
#
# Utilizamos los radios Hamming de referencia
# equivalentes aproximadamente a >=75% común.
#
#
# Si una celda tiene:
#
# 85%
#
# significa que, tras reordenar óptimamente los perfiles,
# EFA y K-means comparten de media aproximadamente
# el 85% de los determinantes seleccionados.
# ============================================================

p_mean_similarity <- ggplot(
  
  reference_results,
  
  aes(
    
    x =
      factor(
        k_candidate
      ),
    
    y =
      factor(
        d_det
      ),
    
    fill =
      mean_similarity_penalized
  )
  
) +
  
  geom_tile(
    
    color =
      "white",
    
    linewidth =
      0.5
  ) +
  
  geom_text(
    
    aes(
      
      label =
        paste0(
          
          round(
            mean_similarity_penalized,
            1
          ),
          
          "%"
        )
    ),
    
    size =
      3.1
  ) +
  
  facet_wrap(
    
    ~ matrix,
    
    ncol =
      2
  ) +
  
  scale_fill_gradientn(
    
    colours =
      c(
        
        "#F7FBFF",
        
        "#C6DBEF",
        
        "#6BAED6",
        
        "#2171B5",
        
        "#08306B"
      ),
    
    limits =
      c(
        0,
        100
      ),
    
    name =
      "Similarity"
  ) +
  
  theme_minimal(
    base_size = 11
  ) +
  
  theme(
    
    plot.title =
      element_text(
        face = "bold"
      ),
    
    panel.grid =
      element_blank(),
    
    strip.text =
      element_text(
        face = "bold"
      )
  ) +
  
  labs(
    
    title =
      "EFA vs K-means: mean similarity after optimal matching",
    
    subtitle =
      "Hamming radius selected for each D to guarantee approximately ≥75% common determinants",
    
    x =
      "K candidate",
    
    y =
      "Selected determinants (D)"
  )



ggsave(
  
  file.path(
    fig_dir,
    "01_mean_similarity_heatmap.png"
  ),
  
  p_mean_similarity,
  
  width =
    11,
  
  height =
    8,
  
  dpi =
    300,
  
  bg =
    "white"
)



# ============================================================
# 20. GRÁFICO:
#     PEOR PERFIL EMPAREJADO
# ============================================================
#
# Este gráfico es MUY importante.
#
#
# Ejemplo:
#
# similitudes:
#
# 90
# 88
# 87
# 85
# 82
# 20
#
#
# La media puede ser aceptable,
# pero existe un perfil sin equivalente claro.
#
#
# Por eso representamos:
#
# MINIMUM matched similarity.
# ============================================================

p_min_similarity <- ggplot(
  
  reference_results,
  
  aes(
    
    x =
      factor(
        k_candidate
      ),
    
    y =
      factor(
        d_det
      ),
    
    fill =
      min_similarity_actual
  )
  
) +
  
  geom_tile(
    
    color =
      "white",
    
    linewidth =
      0.5
  ) +
  
  geom_text(
    
    aes(
      
      label =
        paste0(
          
          round(
            min_similarity_actual,
            1
          ),
          
          "%"
        )
    ),
    
    size =
      3.1
  ) +
  
  facet_wrap(
    
    ~ matrix,
    
    ncol =
      2
  ) +
  
  scale_fill_gradientn(
    
    colours =
      c(
        
        "#FFF5F0",
        
        "#FCAE91",
        
        "#FB6A4A",
        
        "#CB181D",
        
        "#67000D"
      ),
    
    limits =
      c(
        0,
        100
      ),
    
    name =
      "Minimum\nsimilarity"
  ) +
  
  theme_minimal(
    base_size = 11
  ) +
  
  theme(
    
    plot.title =
      element_text(
        face = "bold"
      ),
    
    panel.grid =
      element_blank(),
    
    strip.text =
      element_text(
        face = "bold"
      )
  ) +
  
  labs(
    
    title =
      "EFA vs K-means: similarity of the worst matched profile",
    
    subtitle =
      "Low values reveal profiles for which the two methods do not find a clear equivalent",
    
    x =
      "K candidate",
    
    y =
      "Selected determinants (D)"
  )



ggsave(
  
  file.path(
    fig_dir,
    "02_minimum_similarity_heatmap.png"
  ),
  
  p_min_similarity,
  
  width =
    11,
  
  height =
    8,
  
  dpi =
    300,
  
  bg =
    "white"
)



# ============================================================
# 21. GRÁFICO:
#     COBERTURA DEL MÉTODO PEOR
# ============================================================
#
# Para una solución queremos:
#
# similitud EFA ↔ Kmeans
#
# PERO TAMBIÉN:
#
# estabilidad interna en ambos métodos.
#
#
# Aquí usamos:
#
# min(
#    coverage EFA,
#    coverage Kmeans
# )
#
#
# Es una forma conservadora de mirar la estabilidad.
# ============================================================

p_min_coverage <- ggplot(
  
  reference_results,
  
  aes(
    
    x =
      factor(
        k_candidate
      ),
    
    y =
      factor(
        d_det
      ),
    
    fill =
      min_coverage_both
  )
  
) +
  
  geom_tile(
    
    color =
      "white",
    
    linewidth =
      0.5
  ) +
  
  geom_text(
    
    aes(
      
      label =
        paste0(
          
          round(
            min_coverage_both,
            1
          ),
          
          "%"
        )
    ),
    
    size =
      3.1
  ) +
  
  facet_wrap(
    
    ~ matrix,
    
    ncol =
      2
  ) +
  
  scale_fill_gradientn(
    
    colours =
      c(
        
        "#F7FCF5",
        
        "#C7E9C0",
        
        "#74C476",
        
        "#238B45",
        
        "#00441B"
      ),
    
    limits =
      c(
        0,
        100
      ),
    
    name =
      "Minimum\ncoverage"
  ) +
  
  theme_minimal(
    base_size = 11
  ) +
  
  theme(
    
    plot.title =
      element_text(
        face = "bold"
      ),
    
    panel.grid =
      element_blank(),
    
    strip.text =
      element_text(
        face = "bold"
      )
  ) +
  
  labs(
    
    title =
      "Greedy stability: minimum coverage across EFA and K-means",
    
    subtitle =
      "The value corresponds to the less stable of the two methods",
    
    x =
      "K candidate",
    
    y =
      "Selected determinants (D)"
  )



ggsave(
  
  file.path(
    fig_dir,
    "03_minimum_coverage_both_methods.png"
  ),
  
  p_min_coverage,
  
  width =
    11,
  
  height =
    8,
  
  dpi =
    300,
  
  bg =
    "white"
)



# ============================================================
# 22. GRÁFICO:
#     PARA K=6, CÓMO CAMBIA LA SIMILITUD CON D
# ============================================================
#
# Nuestra hipótesis principal actualmente es K=6.
#
# Este gráfico permitirá comprobar:
#
# ¿D=8?
# ¿D=9?
# ¿D=10?
# ...
#
# da resultados consistentes?
# ============================================================

p_k6_D <- reference_results %>%
  
  filter(
    k_candidate ==
      REFERENCE_K
  ) %>%
  
  ggplot(
    
    aes(
      
      x =
        d_det,
      
      y =
        mean_similarity_penalized,
      
      color =
        matrix,
      
      group =
        matrix
    )
    
  ) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 3
  ) +
  
  geom_text(
    
    aes(
      
      label =
        paste0(
          
          round(
            mean_similarity_penalized,
            1
          ),
          
          "%"
        )
    ),
    
    vjust =
      -0.7,
    
    size =
      3,
    
    show.legend =
      FALSE
  ) +
  
  scale_x_continuous(
    breaks =
      8:15
  ) +
  
  scale_y_continuous(
    
    limits =
      c(
        0,
        100
      )
  ) +
  
  theme_minimal(
    base_size = 11
  ) +
  
  theme(
    
    plot.title =
      element_text(
        face = "bold"
      ),
    
    legend.position =
      "bottom"
  ) +
  
  labs(
    
    title =
      paste0(
        "EFA vs K-means similarity across D for K = ",
        REFERENCE_K
      ),
    
    subtitle =
      "Useful for evaluating whether the choice of D is robust",
    
    x =
      "Selected determinants (D)",
    
    y =
      "Mean matched similarity (%)",
    
    color =
      "Matrix"
  )



ggsave(
  
  file.path(
    fig_dir,
    "04_K6_similarity_across_D.png"
  ),
  
  p_k6_D,
  
  width =
    10,
  
  height =
    6.5,
  
  dpi =
    300,
  
  bg =
    "white"
)



# ============================================================
# 23. HEATMAP DETALLADO DEL MATCHING
# ============================================================
#
# Generamos uno por matriz para:
#
# K = 6
# D = 8
# Hamming = 4
#
#
# Cada celda:
#
# similitud entre un prototipo Kmeans
# y un prototipo EFA.
#
#
# Las celdas con borde negro son las parejas elegidas
# por el matching óptimo.
#
#
# IMPORTANTE:
#
# Esto NO significa que D=8/H=4 sean definitivos.
#
# Es simplemente nuestra configuración de referencia.
# ============================================================

for (
  matrix_current in
  unique(
    pairwise_similarity$matrix_name
  )
) {
  
  
  plot_data <- pairwise_similarity %>%
    
    filter(
      
      matrix_name ==
        matrix_current,
      
      k_candidate ==
        REFERENCE_K,
      
      d_det ==
        REFERENCE_D_DET,
      
      d_hamming ==
        REFERENCE_D_HAMMING
    )
  
  
  
  if (nrow(plot_data) == 0) {
    
    next
  }
  
  
  
  matrix_short <- matrix_label(
    matrix_current
  )
  
  
  
  p_match <- ggplot(
    
    plot_data,
    
    aes(
      
      x =
        factor(
          efa_prototype
        ),
      
      y =
        factor(
          kmeans_prototype
        ),
      
      fill =
        similarity_pct
    )
    
  ) +
    
    geom_tile(
      
      color =
        "white",
      
      linewidth =
        0.5
    ) +
    
    # --------------------------------------------------------
  # Borde negro:
  # parejas seleccionadas por el matching óptimo
  # --------------------------------------------------------
  
  geom_tile(
    
    data =
      plot_data %>%
      filter(
        optimal_match
      ),
    
    fill =
      NA,
    
    color =
      "black",
    
    linewidth =
      1.3
  ) +
    
    geom_text(
      
      aes(
        
        label =
          paste0(
            
            round(
              similarity_pct,
              1
            ),
            
            "%"
          )
      ),
      
      size =
        4
    ) +
    
    scale_fill_gradientn(
      
      colours =
        c(
          
          "#FFF7EC",
          
          "#FDD49E",
          
          "#FC8D59",
          
          "#D7301F",
          
          "#7F0000"
        ),
      
      limits =
        c(
          0,
          100
        ),
      
      name =
        "Similarity"
    ) +
    
    coord_equal() +
    
    theme_minimal(
      base_size = 12
    ) +
    
    theme(
      
      panel.grid =
        element_blank(),
      
      plot.title =
        element_text(
          face = "bold"
        )
    ) +
    
    labs(
      
      title =
        paste0(
          "EFA vs K-means optimal matching – ",
          matrix_short
        ),
      
      subtitle =
        paste0(
          "Reference: K=",
          REFERENCE_K,
          ", D=",
          REFERENCE_D_DET,
          ", Hamming=",
          REFERENCE_D_HAMMING,
          " | Black borders = optimal pairs"
        ),
      
      x =
        "EFA prototype",
      
      y =
        "K-means prototype"
    )
  
  
  
  ggsave(
    
    file.path(
      
      fig_matching_dir,
      
      paste0(
        "matching_",
        tolower(
          matrix_short
        ),
        "_K",
        REFERENCE_K,
        "_D",
        REFERENCE_D_DET,
        "_H",
        REFERENCE_D_HAMMING,
        ".png"
      )
    ),
    
    p_match,
    
    width =
      8,
    
    height =
      7,
    
    dpi =
      300,
    
    bg =
      "white"
  )
}



# ============================================================
# 24. GUARDAR MATCHING DE REFERENCIA
# ============================================================

reference_matching <- optimal_matches %>%
  
  filter(
    
    k_candidate ==
      REFERENCE_K,
    
    d_det ==
      REFERENCE_D_DET,
    
    d_hamming ==
      REFERENCE_D_HAMMING
  ) %>%
  
  mutate(
    
    matrix =
      matrix_label(
        matrix_name
      )
  ) %>%
  
  arrange(
    
    matrix,
    
    kmeans_prototype
  )



write_csv(
  
  reference_matching,
  
  file.path(
    out_dir,
    "07_reference_matching_K6_D8_H4.csv"
  )
)



# ============================================================
# 25. PARÁMETROS
# ============================================================

parameters <- tibble(
  
  parameter = c(
    
    "USE_EVEN_HAMMING_ONLY",
    
    "GOOD_SIMILARITY_THRESHOLD",
    
    "REFERENCE_K",
    
    "REFERENCE_D_DET",
    
    "REFERENCE_D_HAMMING",
    
    "MATCHING_METHOD"
  ),
  
  
  value = c(
    
    as.character(
      USE_EVEN_HAMMING_ONLY
    ),
    
    as.character(
      GOOD_SIMILARITY_THRESHOLD
    ),
    
    as.character(
      REFERENCE_K
    ),
    
    as.character(
      REFERENCE_D_DET
    ),
    
    as.character(
      REFERENCE_D_HAMMING
    ),
    
    "Hungarian / clue::solve_LSAP(maximum=TRUE)"
  )
)



write_csv(
  
  parameters,
  
  file.path(
    out_dir,
    "08_compare_parameters.csv"
  )
)



# ============================================================
# 26. CONSOLA
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "10. COMPARACIÓN EFA ↔ K-MEANS COMPLETADA\n"
)

cat(
  "============================================================\n"
)



cat(
  "\nRadios Hamming de referencia (~>=75% comunes):\n"
)


print(
  
  reference_hamming,
  
  n =
    Inf,
  
  width =
    Inf
)



cat(
  "\n============================================================\n"
)

cat(
  "REFERENCIA: K=6, D=8, H=4\n"
)

cat(
  "============================================================\n"
)



reference_summary_console <-
  
  convergence_and_greedy %>%
  
  filter(
    
    k_candidate ==
      REFERENCE_K,
    
    d_det ==
      REFERENCE_D_DET,
    
    d_hamming ==
      REFERENCE_D_HAMMING
  ) %>%
  
  select(
    
    matrix,
    
    mean_similarity_penalized,
    
    mean_similarity_actual,
    
    min_similarity_actual,
    
    max_similarity_actual,
    
    pct_matches_ge_75,
    
    final_covered_pct_EFA,
    
    final_covered_pct_KMEANS,
    
    min_coverage_both
  )



print(
  
  reference_summary_console,
  
  n =
    Inf,
  
  width =
    Inf
)



cat(
  "\n============================================================\n"
)

cat(
  "MATCHING DETALLADO K=6, D=8, H=4\n"
)

cat(
  "============================================================\n"
)



print(
  
  reference_matching %>%
    
    select(
      
      matrix,
      
      kmeans_prototype,
      
      efa_prototype,
      
      similarity_pct,
      
      n_common_determinants,
      
      is_real_match
    ),
  
  n =
    Inf,
  
  width =
    Inf
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
  "\nResultados:\n",
  out_dir,
  "\n",
  sep = ""
)


cat(
  "\nFiguras:\n",
  fig_dir,
  "\n",
  sep = ""
)



message(
  "\nListo. Comparación EFA ↔ K-means completada."
)