
# Visualizar conjuntamente los resultados obtenidos en:
#
#   06_kmeans.R  07_efa.R  08_greedy.R
#
# K-MEANS
# A) ¿Dónde aparece el "codo" al aumentar K?
# B) ¿Qué mejora marginal aporta cada K adicional?
# C) ¿Aparecen clusters demasiado pequeños?
# D) ¿Los centroides empiezan a estar demasiado cerca?
#
# EFA
# E) ¿Qué nº de factores tiene mejor compromiso de ajuste?
# F) ¿Cómo evolucionan BIC, TLI, RMSEA y RMSR?
#
# GREEDY
# G) ¿Qué cobertura conseguimos manteniendo una similitud razonable entre patrones?
# H) ¿Cuánto aporta cada prototipo?
# I) ¿Qué D_DET / K generan patrones más reproducibles?
# J) ¿Qué determinantes aparecen consistentemente dentro de cada prototipo?

# IMPORTANTE
# Para Greedy usamos como referencia:
#
#       mínimo 75% de determinantes comunes
#
# NO significa que 75% sea ya el criterio definitivo.
#
# Es una referencia interpretable para poder comparar
# D_DET distintos en igualdad de condiciones.

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(scales)
})



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
# K-MEANS
# ------------------------------------------------------------

kmeans_dir <- file.path(
  processed_root,
  "06_kmeans_bootstrap"
)



# ------------------------------------------------------------
# EFA
# ------------------------------------------------------------

efa_dir <- file.path(
  processed_root,
  "07_efa_bootstrap"
)



# ------------------------------------------------------------
# GREEDY
# ------------------------------------------------------------

greedy_dir <- file.path(
  processed_root,
  "08_greedy_kmeans_efa"
)



# ------------------------------------------------------------
# OUTPUT 09
# ------------------------------------------------------------

out_dir <- file.path(
  processed_root,
  "09_analysis_plots"
)


fig_dir <- file.path(
  out_dir,
  "figures"
)


fig_kmeans_dir <- file.path(
  fig_dir,
  "kmeans"
)


fig_efa_dir <- file.path(
  fig_dir,
  "efa"
)


fig_greedy_dir <- file.path(
  fig_dir,
  "greedy"
)


fig_profiles_dir <- file.path(
  fig_dir,
  "profiles"
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
  fig_kmeans_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  fig_efa_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  fig_greedy_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  fig_profiles_dir,
  recursive = TRUE,
  showWarnings = FALSE
)



# ============================================================
# 2. PARÁMETROS VISUALES
# ============================================================


# ------------------------------------------------------------
# Similitud mínima que usaremos como referencia para Greedy
# ------------------------------------------------------------

MIN_COMMON_PCT <- 75



# ------------------------------------------------------------
# Configuración de referencia para heatmaps de perfiles
#
# SOLO PARA VISUALIZACIÓN.
#
# NO significa que hayamos decidido todavía:
# K = 6
# D = 8
#
# Se puede cambiar después fácilmente.
# ------------------------------------------------------------

REFERENCE_K <- 6

REFERENCE_D_DET <- 8

REFERENCE_D_HAMMING <- 4



# ------------------------------------------------------------
# Colores de las matrices
# ------------------------------------------------------------

MATRIX_COLORS <- c(
  
  "RAW" =
    "#4C78A8",
  
  "POS" =
    "#59A14F",
  
  "EXT" =
    "#F28E2B",
  
  "Z_ABS" =
    "#B279A2"
)



# ------------------------------------------------------------
# Colores K
# ------------------------------------------------------------

K_COLORS <- c(
  
  "4" =
    "#4E79A7",
  
  "5" =
    "#59A14F",
  
  "6" =
    "#F28E2B",
  
  "7" =
    "#E15759",
  
  "8" =
    "#B07AA1"
)



# ------------------------------------------------------------
# Colores método
# ------------------------------------------------------------

METHOD_COLORS <- c(
  
  "KMEANS" =
    "#4C78A8",
  
  "EFA" =
    "#E15759"
)



# ============================================================
# 3. FUNCIONES AUXILIARES
# ============================================================


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



clean_determinant_label <- function(x) {
  
  x %>%
    
    str_remove(
      "^det_\\d+_"
    ) %>%
    
    str_replace_all(
      "_",
      " "
    ) %>%
    
    str_to_sentence()
}



save_plot <- function(
    plot,
    filename,
    width = 10,
    height = 7
) {
  
  ggsave(
    
    filename =
      filename,
    
    plot =
      plot,
    
    width =
      width,
    
    height =
      height,
    
    dpi =
      300,
    
    bg =
      "white"
  )
}



theme_paper <- function(
    base_size = 12
) {
  
  theme_minimal(
    base_size =
      base_size
  ) +
    
    theme(
      
      plot.title =
        element_text(
          face = "bold",
          size = base_size + 2
        ),
      
      plot.subtitle =
        element_text(
          size = base_size - 1
        ),
      
      panel.grid.minor =
        element_blank(),
      
      legend.position =
        "bottom",
      
      strip.text =
        element_text(
          face = "bold"
        ),
      
      axis.title =
        element_text(
          face = "bold"
        )
    )
}



# ============================================================
# 4. LEER RESULTADOS K-MEANS
# ============================================================

kmeans_metrics_by_run <- read_csv(
  
  file.path(
    kmeans_dir,
    "kmeans_metrics_by_run.csv"
  ),
  
  show_col_types = FALSE
)



kmeans_cluster_sizes <- read_csv(
  
  file.path(
    kmeans_dir,
    "kmeans_cluster_sizes_by_run.csv"
  ),
  
  show_col_types = FALSE
)



kmeans_distances <- read_csv(
  
  file.path(
    kmeans_dir,
    "kmeans_cluster_distances_by_run.csv"
  ),
  
  show_col_types = FALSE
)



# ============================================================
# 5. REGENERAR RESUMEN K-MEANS
# ============================================================
#
# Esto evita el problema de:
#
# sd_cluster_distance = NA
#
# del script 06.
#
# NO vuelve a calcular K-means.
# Solo vuelve a resumir las métricas ya existentes.
# ============================================================

kmeans_summary <- kmeans_metrics_by_run %>%
  
  filter(
    status == "ok"
  ) %>%
  
  group_by(
    matrix_name,
    k
  ) %>%
  
  summarise(
    
    mean_between_over_total =
      mean(
        between_over_total,
        na.rm = TRUE
      ),
    
    sd_between_over_total =
      sd(
        between_over_total,
        na.rm = TRUE
      ),
    
    mean_tot_withinss =
      mean(
        tot_withinss,
        na.rm = TRUE
      ),
    
    mean_cluster_distance_boot =
      mean(
        mean_cluster_distance,
        na.rm = TRUE
      ),
    
    sd_cluster_distance_boot =
      sd(
        mean_cluster_distance,
        na.rm = TRUE
      ),
    
    mean_min_cluster_distance =
      mean(
        min_cluster_distance,
        na.rm = TRUE
      ),
    
    mean_max_cluster_distance =
      mean(
        max_cluster_distance,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  ) %>%
  
  mutate(
    
    matrix =
      matrix_label(
        matrix_name
      )
  )



# ============================================================
# 6. MEJORA MARGINAL DE K-MEANS
# ============================================================

kmeans_delta <- kmeans_summary %>%
  
  group_by(
    matrix_name
  ) %>%
  
  arrange(
    k,
    .by_group = TRUE
  ) %>%
  
  mutate(
    
    delta_between_over_total =
      mean_between_over_total -
      lag(
        mean_between_over_total
      )
  ) %>%
  
  ungroup()



write_csv(
  
  kmeans_summary,
  
  file.path(
    out_dir,
    "01_kmeans_summary_corrected.csv"
  )
)



write_csv(
  
  kmeans_delta,
  
  file.path(
    out_dir,
    "02_kmeans_marginal_gain.csv"
  )
)



# ============================================================
# 7. K-MEANS:
#    VARIANZA EXPLICADA
# ============================================================

p_kmeans_between <- ggplot(
  
  kmeans_summary,
  
  aes(
    
    x =
      k,
    
    y =
      mean_between_over_total,
    
    color =
      matrix,
    
    group =
      matrix
  )
  
) +
  
  geom_line(
    linewidth = 1.1
  ) +
  
  geom_point(
    size = 3
  ) +
  
  geom_text(
    
    aes(
      label =
        sprintf(
          "%.3f",
          mean_between_over_total
        )
    ),
    
    vjust =
      -0.8,
    
    size =
      3.1,
    
    show.legend =
      FALSE
  ) +
  
  scale_color_manual(
    values =
      MATRIX_COLORS
  ) +
  
  scale_x_continuous(
    breaks =
      4:8
  ) +
  
  labs(
    
    title =
      "K-means: explained between-cluster variance",
    
    subtitle =
      "Higher values indicate greater separation, but improvement should diminish as K increases",
    
    x =
      "Number of clusters (K)",
    
    y =
      "Between SS / Total SS",
    
    color =
      "Matrix"
  ) +
  
  theme_paper()



save_plot(
  
  p_kmeans_between,
  
  file.path(
    fig_kmeans_dir,
    "01_kmeans_between_over_total.png"
  ),
  
  width = 10,
  height = 6.5
)



# ============================================================
# 8. K-MEANS:
#    MEJORA MARGINAL
# ============================================================

p_kmeans_delta <- kmeans_delta %>%
  
  filter(
    !is.na(
      delta_between_over_total
    )
  ) %>%
  
  ggplot(
    
    aes(
      
      x =
        factor(k),
      
      y =
        delta_between_over_total,
      
      fill =
        matrix
    )
  ) +
  
  geom_col(
    position =
      position_dodge(
        width = 0.8
      ),
    width =
      0.7
  ) +
  
  geom_text(
    
    aes(
      label =
        sprintf(
          "%.3f",
          delta_between_over_total
        )
    ),
    
    position =
      position_dodge(
        width = 0.8
      ),
    
    vjust =
      -0.4,
    
    size =
      3
  ) +
  
  scale_fill_manual(
    values =
      MATRIX_COLORS
  ) +
  
  labs(
    
    title =
      "K-means: marginal gain when adding one cluster",
    
    subtitle =
      "Useful for identifying the elbow: smaller gains suggest diminishing returns",
    
    x =
      "New K",
    
    y =
      "Increase in Between SS / Total SS",
    
    fill =
      "Matrix"
  ) +
  
  theme_paper()



save_plot(
  
  p_kmeans_delta,
  
  file.path(
    fig_kmeans_dir,
    "02_kmeans_marginal_gain.png"
  ),
  
  width = 10,
  height = 6.5
)



# ============================================================
# 9. K-MEANS:
#    DISTANCIA MÍNIMA ENTRE CLUSTERS
# ============================================================
#
# IMPORTANTE:
#
# No comparamos el valor absoluto entre matrices,
# porque cada transformación tiene una escala diferente.
#
# Miramos la evolución con K DENTRO de cada matriz.
# ============================================================

p_kmeans_distance <- ggplot(
  
  kmeans_summary,
  
  aes(
    
    x =
      k,
    
    y =
      mean_min_cluster_distance,
    
    group =
      1
  )
  
) +
  
  geom_line(
    linewidth = 1,
    color = "#4C78A8"
  ) +
  
  geom_point(
    size = 3,
    color = "#4C78A8"
  ) +
  
  geom_text(
    
    aes(
      label =
        sprintf(
          "%.2f",
          mean_min_cluster_distance
        )
    ),
    
    vjust =
      -0.7,
    
    size =
      3
  ) +
  
  facet_wrap(
    
    ~ matrix,
    
    scales =
      "free_y"
  ) +
  
  scale_x_continuous(
    breaks =
      4:8
  ) +
  
  labs(
    
    title =
      "K-means: minimum separation between cluster centroids",
    
    subtitle =
      "A decreasing minimum distance may indicate over-fragmentation as K increases",
    
    x =
      "Number of clusters (K)",
    
    y =
      "Mean minimum Euclidean distance"
  ) +
  
  theme_paper()



save_plot(
  
  p_kmeans_distance,
  
  file.path(
    fig_kmeans_dir,
    "03_kmeans_minimum_cluster_distance.png"
  ),
  
  width = 11,
  height = 7
)



# ============================================================
# 10. K-MEANS:
#     CLUSTER MÁS PEQUEÑO EN CADA BOOTSTRAP
# ============================================================
#
# Esto evita depender del cluster_rank.
#
# Para cada:
#
# bootstrap × matriz × K
#
# buscamos el cluster más pequeño.
#
# Si K es excesivo empezaremos a ver valores muy bajos.
# ============================================================

minimum_cluster_share <- kmeans_cluster_sizes %>%
  
  group_by(
    matrix_name,
    bootstrap_id,
    k
  ) %>%
  
  summarise(
    
    min_cluster_prop =
      min(
        prop_cluster,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  ) %>%
  
  mutate(
    
    matrix =
      matrix_label(
        matrix_name
      ),
    
    k =
      factor(
        k
      )
  )



write_csv(
  
  minimum_cluster_share,
  
  file.path(
    out_dir,
    "03_minimum_cluster_share_by_bootstrap.csv"
  )
)



p_min_cluster <- ggplot(
  
  minimum_cluster_share,
  
  aes(
    
    x =
      k,
    
    y =
      min_cluster_prop,
    
    fill =
      k
  )
  
) +
  
  geom_boxplot(
    
    alpha =
      0.8,
    
    outlier.alpha =
      0.25
  ) +
  
  geom_hline(
    
    yintercept =
      0.05,
    
    linetype =
      "dashed",
    
    linewidth =
      0.6,
    
    color =
      "#C44E52"
  ) +
  
  facet_wrap(
    
    ~ matrix,
    
    ncol =
      2
  ) +
  
  scale_fill_manual(
    values =
      K_COLORS
  ) +
  
  scale_y_continuous(
    
    labels =
      percent_format(
        accuracy = 1
      )
  ) +
  
  labs(
    
    title =
      "K-means: size of the smallest cluster in each bootstrap",
    
    subtitle =
      "Dashed line = 5% of the bootstrap sample; very small clusters may indicate over-segmentation",
    
    x =
      "Number of clusters (K)",
    
    y =
      "Smallest cluster share",
    
    fill =
      "K"
  ) +
  
  theme_paper()



save_plot(
  
  p_min_cluster,
  
  file.path(
    fig_kmeans_dir,
    "04_kmeans_smallest_cluster_share.png"
  ),
  
  width = 10,
  height = 8
)



# ============================================================
# 11. LEER EFA
# ============================================================

efa_summary <- read_csv(
  
  file.path(
    efa_dir,
    "02_efa_metrics_summary.csv"
  ),
  
  show_col_types =
    FALSE
  
) %>%
  
  mutate(
    
    matrix =
      matrix_label(
        matrix_name
      )
  )



# ============================================================
# 12. EFA:
#     BIC
# ============================================================
#
# Más negativo = mejor dentro de la MISMA matriz.
#
# NO usamos BIC para comparar directamente
# una transformación frente a otra.
# ============================================================

p_efa_bic <- ggplot(
  
  efa_summary,
  
  aes(
    
    x =
      n_factors,
    
    y =
      mean_BIC,
    
    group =
      1
  )
  
) +
  
  geom_line(
    
    linewidth =
      1,
    
    color =
      "#4C78A8"
  ) +
  
  geom_point(
    
    size =
      3,
    
    color =
      "#4C78A8"
  ) +
  
  geom_text(
    
    aes(
      label =
        round(
          mean_BIC
        )
    ),
    
    vjust =
      -0.7,
    
    size =
      3
  ) +
  
  facet_wrap(
    
    ~ matrix,
    
    scales =
      "free_y"
  ) +
  
  scale_x_continuous(
    breaks =
      4:8
  ) +
  
  labs(
    
    title =
      "EFA: Bayesian Information Criterion by number of factors",
    
    subtitle =
      "More negative BIC indicates a better complexity-adjusted solution within each matrix",
    
    x =
      "Number of factors",
    
    y =
      "Mean BIC"
  ) +
  
  theme_paper()



save_plot(
  
  p_efa_bic,
  
  file.path(
    fig_efa_dir,
    "05_efa_bic.png"
  ),
  
  width = 11,
  height = 7
)



# ============================================================
# 13. EFA:
#     TLI / RMSEA / RMSR
# ============================================================

efa_fit_long <- efa_summary %>%
  
  select(
    
    matrix,
    
    n_factors,
    
    mean_RMSR,
    
    mean_TLI,
    
    mean_RMSEA
  ) %>%
  
  pivot_longer(
    
    cols =
      c(
        mean_RMSR,
        mean_TLI,
        mean_RMSEA
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
        
        mean_RMSR =
          "RMSR",
        
        mean_TLI =
          "TLI",
        
        mean_RMSEA =
          "RMSEA"
      )
  )



p_efa_fit <- ggplot(
  
  efa_fit_long,
  
  aes(
    
    x =
      n_factors,
    
    y =
      value,
    
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
    size = 2.5
  ) +
  
  facet_wrap(
    
    ~ metric,
    
    scales =
      "free_y",
    
    ncol =
      1
  ) +
  
  scale_color_manual(
    values =
      MATRIX_COLORS
  ) +
  
  scale_x_continuous(
    breaks =
      4:8
  ) +
  
  labs(
    
    title =
      "EFA: model fit as the number of factors increases",
    
    subtitle =
      "TLI should increase, while RMSEA and RMSR should decrease; complexity must still be considered",
    
    x =
      "Number of factors",
    
    y =
      "Metric value",
    
    color =
      "Matrix"
  ) +
  
  theme_paper()



save_plot(
  
  p_efa_fit,
  
  file.path(
    fig_efa_dir,
    "06_efa_fit_metrics.png"
  ),
  
  width = 10,
  height = 10
)



# ============================================================
# 14. EFA:
#     CORRELACIÓN ENTRE FACTORES
# ============================================================

p_efa_cor <- ggplot(
  
  efa_summary,
  
  aes(
    
    x =
      n_factors,
    
    y =
      mean_abs_factor_correlation,
    
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
    size = 2.8
  ) +
  
  scale_color_manual(
    values =
      MATRIX_COLORS
  ) +
  
  scale_x_continuous(
    breaks =
      4:8
  ) +
  
  labs(
    
    title =
      "EFA: mean absolute correlation between factors",
    
    subtitle =
      "Higher correlations imply greater overlap between latent factors",
    
    x =
      "Number of factors",
    
    y =
      "Mean |factor correlation|",
    
    color =
      "Matrix"
  ) +
  
  theme_paper()



save_plot(
  
  p_efa_cor,
  
  file.path(
    fig_efa_dir,
    "07_efa_factor_correlations.png"
  ),
  
  width = 10,
  height = 6.5
)



# ============================================================
# 15. LEER GREEDY
# ============================================================

greedy_steps <- read_csv(
  
  file.path(
    greedy_dir,
    "03_greedy_prototype_steps.csv"
  ),
  
  show_col_types =
    FALSE
)



greedy_coverage <- read_csv(
  
  file.path(
    greedy_dir,
    "04_greedy_coverage_summary.csv"
  ),
  
  show_col_types =
    FALSE
)



greedy_prevalence <- read_csv(
  
  file.path(
    greedy_dir,
    "05_greedy_ball_determinant_prevalence.csv"
  ),
  
  show_col_types =
    FALSE
)



# ============================================================
# 16. GREEDY:
#     RADIO EQUIVALENTE A >=75% DE DETERMINANTES COMUNES
# ============================================================
#
# Para cada D_DET cogemos el radio Hamming MÁS GRANDE
# que todavía garantice:
#
#       min_common_pct >= 75
#
# Además quitamos radios impares porque son redundantes.
# ============================================================

greedy_75 <- greedy_coverage %>%
  
  filter(
    
    odd_radius_redundant ==
      FALSE,
    
    min_common_pct >=
      MIN_COMMON_PCT
  ) %>%
  
  group_by(
    
    method,
    
    matrix_name,
    
    k_candidate,
    
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
  
  mutate(
    
    matrix =
      matrix_label(
        matrix_name
      )
  )



write_csv(
  
  greedy_75,
  
  file.path(
    out_dir,
    "04_greedy_reference_75pct.csv"
  )
)



# ============================================================
# 17. TABLA DE RADIOS USADOS
# ============================================================

hamming_reference <- greedy_75 %>%
  
  distinct(
    
    d_det,
    
    d_hamming,
    
    min_common_determinants,
    
    min_common_pct
  ) %>%
  
  arrange(
    d_det
  )



write_csv(
  
  hamming_reference,
  
  file.path(
    out_dir,
    "05_hamming_reference_75pct.csv"
  )
)



# ============================================================
# 18. GREEDY:
#     HEATMAP DE COBERTURA
# ============================================================
#
# Cada celda:
#
# K × D_DET
#
# y usamos para cada D_DET el Hamming equivalente
# a >=75% de determinantes comunes.
#
# Esta es probablemente una de las figuras
# más importantes para seleccionar candidatos.
# ============================================================

p_greedy_heatmap <- ggplot(
  
  greedy_75,
  
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
      final_covered_pct
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
            final_covered_pct,
            1
          ),
          "%"
        )
    ),
    
    size =
      3.2
  ) +
  
  facet_grid(
    
    method ~ matrix
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
      "Coverage"
  ) +
  
  labs(
    
    title =
      paste0(
        "Greedy stability: coverage with at least ",
        MIN_COMMON_PCT,
        "% common determinants"
      ),
    
    subtitle =
      "Each cell reports the percentage of bootstrap-derived patterns covered by up to K representative prototypes",
    
    x =
      "K candidate",
    
    y =
      "Number of selected determinants (D)"
  ) +
  
  theme_paper(
    base_size = 11
  ) +
  
  theme(
    
    axis.text.x =
      element_text(
        face = "bold"
      )
  )



save_plot(
  
  p_greedy_heatmap,
  
  file.path(
    fig_greedy_dir,
    "08_greedy_coverage_heatmap_75pct.png"
  ),
  
  width = 14,
  height = 8
)



# ============================================================
# 19. GREEDY:
#     COBERTURA MEDIA POR K
# ============================================================
#
# Promediamos D_DET=8:15,
# siempre bajo el criterio >=75% común.
#
# Esto NO selecciona automáticamente K.
#
# Sirve para ver estabilidad general.
# ============================================================

greedy_k_summary <- greedy_75 %>%
  
  group_by(
    
    method,
    
    matrix,
    
    k_candidate
  ) %>%
  
  summarise(
    
    mean_coverage =
      mean(
        final_covered_pct,
        na.rm = TRUE
      ),
    
    sd_coverage =
      sd(
        final_covered_pct,
        na.rm = TRUE
      ),
    
    mean_last_increment =
      mean(
        last_prototype_increment_pct,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  )



write_csv(
  
  greedy_k_summary,
  
  file.path(
    out_dir,
    "06_greedy_k_summary_75pct.csv"
  )
)



p_greedy_k <- ggplot(
  
  greedy_k_summary,
  
  aes(
    
    x =
      k_candidate,
    
    y =
      mean_coverage,
    
    color =
      method,
    
    group =
      method
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
            mean_coverage,
            1
          ),
          "%"
        )
    ),
    
    vjust =
      -0.8,
    
    size =
      3
  ) +
  
  facet_wrap(
    
    ~ matrix,
    
    ncol =
      2
  ) +
  
  scale_color_manual(
    values =
      METHOD_COLORS
  ) +
  
  scale_x_continuous(
    breaks =
      4:8
  ) +
  
  scale_y_continuous(
    
    limits =
      c(
        0,
        100
      )
  ) +
  
  labs(
    
    title =
      "Greedy: mean pattern coverage by K",
    
    subtitle =
      paste0(
        "Average across D = 8–15 using Hamming radii that guarantee at least ",
        MIN_COMMON_PCT,
        "% common determinants"
      ),
    
    x =
      "K candidate",
    
    y =
      "Mean coverage (%)",
    
    color =
      "Method"
  ) +
  
  theme_paper()



save_plot(
  
  p_greedy_k,
  
  file.path(
    fig_greedy_dir,
    "09_greedy_mean_coverage_by_k.png"
  ),
  
  width = 11,
  height = 8
)



# ============================================================
# 20. GREEDY:
#     APORTE DEL ÚLTIMO PROTOTIPO
# ============================================================
#
# Si el último prototipo añade muy poco,
# puede indicar que aumentar K aporta poca información.
# ============================================================

p_last_increment <- ggplot(
  
  greedy_k_summary,
  
  aes(
    
    x =
      k_candidate,
    
    y =
      mean_last_increment,
    
    color =
      method,
    
    group =
      method
  )
  
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 3
  ) +
  
  facet_wrap(
    
    ~ matrix,
    
    ncol =
      2
  ) +
  
  scale_color_manual(
    values =
      METHOD_COLORS
  ) +
  
  scale_x_continuous(
    breaks =
      4:8
  ) +
  
  labs(
    
    title =
      "Greedy: contribution of the last selected prototype",
    
    subtitle =
      "Small increments suggest diminishing returns when adding additional profiles",
    
    x =
      "K candidate",
    
    y =
      "Coverage added by last prototype (%)",
    
    color =
      "Method"
  ) +
  
  theme_paper()



save_plot(
  
  p_last_increment,
  
  file.path(
    fig_greedy_dir,
    "10_greedy_last_prototype_increment.png"
  ),
  
  width = 11,
  height = 8
)



# ============================================================
# 21. GREEDY:
#     COBERTURA ACUMULADA PROTOTIPO A PROTOTIPO
# ============================================================
#
# Usamos:
#
# D_DET = 8
# D_HAMMING = 4
#
# porque:
#
# 6/8 determinantes comunes = 75%
#
# Este gráfico permite visualizar:
#
# P1 -> x%
# P2 -> x%
# P3 -> x%
# ...
#
# y ver dónde aparece el rendimiento decreciente.
# ============================================================

greedy_steps_reference <- greedy_steps %>%
  
  filter(
    
    d_det ==
      REFERENCE_D_DET,
    
    d_hamming ==
      REFERENCE_D_HAMMING
  ) %>%
  
  mutate(
    
    matrix =
      matrix_label(
        matrix_name
      ),
    
    k_candidate =
      factor(
        k_candidate
      )
  )



p_cumulative <- ggplot(
  
  greedy_steps_reference,
  
  aes(
    
    x =
      prototype,
    
    y =
      cumulative_covered_pct,
    
    color =
      k_candidate,
    
    group =
      k_candidate
  )
  
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 2.5
  ) +
  
  facet_grid(
    
    method ~ matrix
  ) +
  
  scale_color_manual(
    values =
      K_COLORS
  ) +
  
  scale_x_continuous(
    
    breaks =
      1:8
  ) +
  
  scale_y_continuous(
    
    limits =
      c(
        0,
        100
      )
  ) +
  
  labs(
    
    title =
      "Greedy: cumulative coverage as representative prototypes are added",
    
    subtitle =
      paste0(
        "Reference setting: D = ",
        REFERENCE_D_DET,
        ", Hamming = ",
        REFERENCE_D_HAMMING,
        " (minimum 75% common determinants)"
      ),
    
    x =
      "Number of selected prototypes",
    
    y =
      "Cumulative coverage (%)",
    
    color =
      "K"
  ) +
  
  theme_paper(
    base_size = 10
  )



save_plot(
  
  p_cumulative,
  
  file.path(
    fig_greedy_dir,
    "11_greedy_cumulative_coverage.png"
  ),
  
  width = 14,
  height = 8
)



# ============================================================
# 22. HEATMAPS DE ESTABILIDAD DE LOS DETERMINANTES
# ============================================================
#
# Para poder interpretar los perfiles.
#
# CONFIGURACIÓN DE REFERENCIA:
#
# K = 6
# D_DET = 8
# D_HAMMING = 4
#
# Se genera UN gráfico por:
#
# método × matriz
#
#
# IMPORTANTE:
#
# Esto es un diagnóstico visual.
#
# Cuando seleccionemos definitivamente:
#
# K
# D_DET
# D_HAMMING
#
# cambiaremos los parámetros de arriba
# y volveremos a generar estos gráficos.
# ============================================================

profile_reference <- greedy_prevalence %>%
  
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
      ),
    
    determinant_label =
      clean_determinant_label(
        determinant
      )
  )



for (
  
  method_current in
  unique(
    profile_reference$method
  )
  
) {
  
  
  for (
    
    matrix_current in
    unique(
      profile_reference$matrix
    )
    
  ) {
    
    
    plot_data <- profile_reference %>%
      
      filter(
        
        method ==
          method_current,
        
        matrix ==
          matrix_current
      )
    
    
    
    if (nrow(plot_data) == 0) {
      
      next
    }
    
    
    
    determinant_order <- plot_data %>%
      
      group_by(
        determinant_label
      ) %>%
      
      summarise(
        
        max_prevalence =
          max(
            pct_active_in_ball,
            na.rm = TRUE
          ),
        
        .groups =
          "drop"
      ) %>%
      
      arrange(
        max_prevalence
      ) %>%
      
      pull(
        determinant_label
      )
    
    
    
    plot_data <- plot_data %>%
      
      mutate(
        
        determinant_label =
          factor(
            
            determinant_label,
            
            levels =
              determinant_order
          )
      )
    
    
    
    p_profile <- ggplot(
      
      plot_data,
      
      aes(
        
        x =
          factor(
            prototype
          ),
        
        y =
          determinant_label,
        
        fill =
          pct_active_in_ball
      )
      
    ) +
      
      geom_tile(
        
        color =
          "white",
        
        linewidth =
          0.35
      ) +
      
      geom_text(
        
        aes(
          label =
            if_else(
              
              pct_active_in_ball >=
                50,
              
              paste0(
                round(
                  pct_active_in_ball
                ),
                "%"
              ),
              
              ""
            )
        ),
        
        size =
          2.6
      ) +
      
      scale_fill_gradientn(
        
        colours =
          c(
            
            "#FFFFFF",
            
            "#DEEBF7",
            
            "#9ECAE1",
            
            "#4292C6",
            
            "#08519C"
          ),
        
        limits =
          c(
            0,
            100
          ),
        
        name =
          "% present"
      ) +
      
      labs(
        
        title =
          paste0(
            method_current,
            " – ",
            matrix_current,
            ": determinant stability"
          ),
        
        subtitle =
          paste0(
            "Reference: K=",
            REFERENCE_K,
            ", D=",
            REFERENCE_D_DET,
            ", Hamming=",
            REFERENCE_D_HAMMING
          ),
        
        x =
          "Representative prototype",
        
        y =
          "Determinant"
      ) +
      
      theme_paper(
        base_size = 10
      )
    
    
    
    save_plot(
      
      p_profile,
      
      file.path(
        
        fig_profiles_dir,
        
        paste0(
          
          "profile_stability_",
          
          tolower(
            method_current
          ),
          
          "_",
          
          tolower(
            matrix_current
          ),
          
          ".png"
        )
      ),
      
      width =
        10,
      
      height =
        11
    )
  }
}



# ============================================================
# 23. RESUMEN GENERAL PARA CONSOLA
# ============================================================


cat(
  "\n============================================================\n"
)

cat(
  "ANÁLISIS Y GRÁFICOS COMPLETADO\n"
)

cat(
  "============================================================\n"
)



cat(
  "\nReferencia de similitud Greedy:\n"
)

cat(
  MIN_COMMON_PCT,
  "% de determinantes comunes\n"
)



cat(
  "\nRadio Hamming utilizado para cada D:\n"
)



print(
  
  hamming_reference,
  
  n =
    Inf,
  
  width =
    Inf
)



cat(
  "\n============================================================\n"
)

cat(
  "K-MEANS: MEJORA MARGINAL\n"
)

cat(
  "============================================================\n"
)



print(
  
  kmeans_delta %>%
    
    select(
      
      matrix,
      
      k,
      
      mean_between_over_total,
      
      delta_between_over_total,
      
      mean_min_cluster_distance
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
  "EFA: RESUMEN\n"
)

cat(
  "============================================================\n"
)



print(
  
  efa_summary %>%
    
    select(
      
      matrix,
      
      n_factors,
      
      mean_RMSR,
      
      mean_TLI,
      
      mean_RMSEA,
      
      mean_BIC,
      
      mean_abs_factor_correlation
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
  "GREEDY: COBERTURA MEDIA POR K\n"
)

cat(
  "============================================================\n"
)



print(
  
  greedy_k_summary %>%
    
    arrange(
      
      method,
      
      matrix,
      
      k_candidate
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
  "FIGURAS GUARDADAS EN\n"
)

cat(
  "============================================================\n"
)

cat(fig_dir,"\n")

message( "\nListo. Análisis visual generado.")








