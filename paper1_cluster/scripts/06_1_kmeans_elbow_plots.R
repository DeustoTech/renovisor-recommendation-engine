# 
# Objetivo
# Generar los gráficos del codo a partir de los resultados bootstrap
# de K-means producidos en 06, sin volver a ejecutar el clustering.
#
# Se utiliza mean_tot_withinss como criterio del codo.
#
# Configuración esperada:
# - 7 muestras: COMPLETE, EUROPE, LATAM y las 4 submuestras.
# - 4 matrices: RAW, POS, EXT y Z_ABS.
# - K = 2:8.
# - 100 ejecuciones bootstrap por combinación.
#
# Outputs:
# - kmeans_elbow_plot_COMPLETE.png:
#   gráfico principal para comparar K en COMPLETE.
# - kmeans_elbow_plot_all_samples.png:
#   comparación de los codos entre todas las muestras y matrices.

suppressPackageStartupMessages({
  library(tidyverse)
})

# Configuración
project_root <- path.expand("~/Desktop/MASTER/recommendation-engine/TFM")

kmeans_dir <- file.path(
  project_root,
  "paper1_cluster/data/processed/06_kmeans_bootstrap"
)

input_file <- file.path(
  kmeans_dir,
  "kmeans_metrics_summary_all_samples.csv"
)

output_dir <- file.path(
  kmeans_dir,
  "figures_elbow"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

sample_order <- c(
  "COMPLETE",
  "EUROPE",
  "LATAM",
  "DIEGO",
  "RENOVISOR",
  "WHY_EUROPE",
  "WHY_LATAM"
)

matrix_labels <- c(
  "matrix_32_raw_0_1" = "RAW",
  "matrix_32_pos_0_1" = "POS",
  "matrix_32_ext_0_1" = "EXT",
  "matrix_32_z_abs" = "Z_ABS"
)

expected_k <- 2:8
expected_runs <- 100L


# Lectura y comprobaciones
if (!file.exists(input_file)) {
  stop(
    "No se encuentra el archivo: ",
    input_file
  )
}

df <- read_csv(
  input_file,
  show_col_types = FALSE
)

required_cols <- c(
  "analysis_sample",
  "matrix_name",
  "k",
  "n_runs",
  "n_ok",
  "n_error",
  "mean_tot_withinss"
)

missing_cols <- setdiff(
  required_cols,
  names(df)
)

if (length(missing_cols)) {
  stop(
    "Faltan columnas necesarias: ",
    paste(
      missing_cols,
      collapse = ", "
    )
  )
}


# Comprobar que 06 contiene todas las combinaciones esperadas
expected_grid <- crossing(
  analysis_sample = sample_order,
  matrix_name = names(matrix_labels),
  k = expected_k
)

check_df <- expected_grid %>%
  left_join(
    df %>%
      select(
        analysis_sample,
        matrix_name,
        k,
        n_runs,
        n_ok,
        n_error,
        mean_tot_withinss
      ),
    by = c(
      "analysis_sample",
      "matrix_name",
      "k"
    )
  )

if (any(is.na(check_df$n_runs))) {
  stop(
    "Faltan combinaciones muestra × matriz × K. ",
    "El 06 no está completo."
  )
}

if (any(check_df$n_runs != expected_runs)) {
  stop(
    "Alguna combinación no tiene exactamente ",
    expected_runs,
    " ejecuciones."
  )
}

if (
  any(
    check_df$n_ok != expected_runs |
    check_df$n_error != 0
  )
) {
  stop(
    "Hay ejecuciones K-means con error. ",
    "Revisa el resultado del 06 antes de generar el codo."
  )
}


# Preparar datos para los gráficos
plot_df <- df %>%
  filter(
    analysis_sample %in% sample_order,
    matrix_name %in% names(matrix_labels),
    k %in% expected_k,
    is.finite(mean_tot_withinss)
  ) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = sample_order
    ),
    matrix_label = factor(
      recode(
        matrix_name,
        !!!matrix_labels
      ),
      levels = c(
        "RAW",
        "POS",
        "EXT",
        "Z_ABS"
      )
    )
  ) %>%
  arrange(
    analysis_sample,
    matrix_label,
    k
  )


# Gráfico principal de COMPLETE
complete_df <- plot_df %>%
  filter(
    analysis_sample == "COMPLETE"
  )

p_complete <- ggplot(
  complete_df,
  aes(
    x = k,
    y = mean_tot_withinss
  )
) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 2.5
  ) +
  facet_wrap(
    ~ matrix_label,
    scales = "free_y",
    ncol = 2
  ) +
  scale_x_continuous(
    breaks = expected_k
  ) +
  labs(
    title = "K-means elbow plot - COMPLETE",
    subtitle = "Mean total within-cluster sum of squares across bootstrap replicates",
    x = "Number of clusters (K)",
    y = "Mean total within-cluster sum of squares"
  ) +
  theme_minimal(
    base_size = 13
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
    output_dir,
    "kmeans_elbow_plot_COMPLETE.png"
  ),
  plot = p_complete,
  width = 10,
  height = 8,
  dpi = 300
)


# Comparación de todas las muestras y matrices.
p_all <- ggplot(
  plot_df,
  aes(
    x = k,
    y = mean_tot_withinss
  )
) +
  geom_line(
    linewidth = 0.6
  ) +
  geom_point(
    size = 1.6
  ) +
  facet_wrap(
    vars(
      analysis_sample,
      matrix_label
    ),
    scales = "free_y",
    ncol = 4
  ) +
  scale_x_continuous(
    breaks = expected_k
  ) +
  labs(
    title = "K-means elbow plots by sample and matrix",
    subtitle = "Mean total within-cluster sum of squares across bootstrap replicates",
    x = "Number of clusters (K)",
    y = "Mean total within-cluster sum of squares"
  ) +
  theme_minimal(
    base_size = 10
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
    output_dir,
    "kmeans_elbow_plot_all_samples.png"
  ),
  plot = p_all,
  width = 15,
  height = 18,
  dpi = 300
)


# Resumen
message(
  "\nGráficos generados correctamente:"
)

message(
  "  - ",
  file.path(
    output_dir,
    "kmeans_elbow_plot_COMPLETE.png"
  )
)

message(
  "  - ",
  file.path(
    output_dir,
    "kmeans_elbow_plot_all_samples.png"
  )
)