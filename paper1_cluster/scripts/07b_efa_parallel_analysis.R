
# nooooo
# script para responder a

# Sin imponer 4, 5, 6, 7 u 8, ¿cuántos factores recomiendan los datos?
# “¿Cuántos factores recomiendan realmente los datos en cada bootstrap?”

# para comparar/confirmar los factores que indica las metricas del script 07_efa.R


# ¿QUÉ HACE PARALLEL ANALYSIS?
# 1. Calcula la estructura/eigenvalues de los datos reales.
# 2. Genera muchas matrices aleatorias con el mismo:
#       N de personas
#       N de variables
# 3. Calcula qué eigenvalues obtendríamos simplemente
#    por azar.
# 4. Conservamos factores mientras la información real
#    sea mayor que la esperada por azar.

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(stringr)
  library(psych)
  library(ggplot2)
})


set.seed(123)

# RUTAS
project_root <- path.expand("~/Desktop/MASTER/recommendation-engine/TFM")
processed_root <- file.path( project_root,"paper1_cluster/data/processed")

# Bootstrap utilizado
BOOTSTRAP_SCENARIO <-
  "04_2b_propensity_bootstrap_eu_pfe_esn_renew_greens_merged_clustering_usable"

bootstrap_file <- file.path(
  processed_root,
  BOOTSTRAP_SCENARIO,
  "bootstrap_samples_index.csv"
)

# Matrices creadas por el script 05
matrix_dir <- file.path(
  processed_root,
  "05_clustering_matrices"
)

efa_dir <- file.path( processed_root, "07_efa_bootstrap")

# Output específico del Parallel Analysis
out_dir <- file.path(processed_root,"07b_efa_parallel_analysis")

fig_dir <- file.path( out_dir, "figures")

dir.create(out_dir,recursive = TRUE,showWarnings = FALSE)

dir.create(fig_dir,recursive = TRUE,showWarnings = FALSE)

# PARÁMETROS
MATRICES_TO_RUN <- c(
  "matrix_32_raw_0_1", "matrix_32_pos_0_1", "matrix_32_ext_0_1", "matrix_32_z_abs"
)

FA_METHOD <- "minres"

# Número de simulaciones aleatorias de cada Parallel Analysis#
# Para cada bootstrap se generan N_PA_ITER datasets aleatorios.
#
# 100 es una cantidad razonable para este piloto.

N_PA_ITER <- 100

# Percentil utilizado como referencia#
# 0.95 significa que comparamos los datos observados
# frente al percentil 95 de los resultados aleatorios.

PA_QUANTILE <- 0.95

# Bootstraps
# Igual que el resto del piloto:
# usamos los primeros 100
# NO utilizamos todavía los 1000.

MAX_BOOTSTRAPS <- 100

# FINAL:
# MAX_BOOTSTRAPS <- Inf

# FUNCIÓN PARA LEER LAS MATRICES
read_matrix_file <- function(
    matrix_name
) {
  file <- file.path(
    matrix_dir,
    paste0(
      matrix_name,
      ".csv"
    )
  )
  
  if (!file.exists(file)) {
    stop(
      "No encuentro la matriz: ",
      file
    )
  }
  
  mat_df <- read_csv(
    file,
    show_col_types = FALSE
  ) %>%
    mutate(
      integrated_row_id =
        as.character(
          integrated_row_id
        )
    ) %>%
    distinct(
      integrated_row_id,
      .keep_all = TRUE
    )
  
  # Detectar los 32 determinantes
  feature_cols <- names(
    mat_df
  )[
    str_detect(
      names(
        mat_df
      ),
      "^det_\\d{2}_"
    )
  ]
  
  if (
    length(
      feature_cols
    ) != 32
  ) {
    stop(
      "La matriz ",
      matrix_name,
      " debería tener 32 determinantes y tiene ",
      length(
        feature_cols
      )
    )
  }
  
  mat_df <- mat_df %>%
    select(
      integrated_row_id,
      all_of(
        feature_cols
      )
    ) %>%
    mutate(
      across(
        all_of(
          feature_cols
        ),
        as.numeric
      )
    )
  
  list(
    data =
      mat_df,
    feature_cols =
      feature_cols
  )
}

# 4. PARALLEL ANALYSIS SEGURO#
# Igual que hicimos con EFA:
#
# queremos distinguir:
#
#   - análisis correcto
#   - análisis con warning
#   - error
#
#
# psych::fa.parallel() puede imprimir en consola algo como:
#
# "Parallel analysis suggests that the number of
#  factors = 6"

safe_parallel_analysis <- function(
    x
) {
  warning_messages <- character()
  pa_result <- NULL
  invisible(
    capture.output({
      pa_result <- withCallingHandlers(
        tryCatch(
          psych::fa.parallel(
            x,
            # Queremos FACTORES,
            # no componentes principales.
            fa = "fa",
            # Mismo método de extracción
            # que en 07_efa.R.
            fm = FA_METHOD,
            # Nº de datasets aleatorios
            # generados para comparar.
            n.iter = N_PA_ITER,
            # Percentil de referencia.
            quant = PA_QUANTILE,
            # Trabajamos con correlaciones.
            cor = "cor",
            # No queremos un gráfico por bootstrap.
            plot = FALSE,
            # No necesitamos barras de error.
            error.bars = FALSE
          ),
          error = function(e) e
        ),
        # Capturar warnings
        warning = function(w) {
          warning_messages <<- c(
            warning_messages,
            conditionMessage(
              w
            )
          )
          invokeRestart(
            "muffleWarning"
          )
        }
      )
    })
  )
  
  list(
    result =
      pa_result,
    warnings =
      unique(
        warning_messages
      )
  )
}

# LEER BOOTSTRAPS
if (!file.exists(
  bootstrap_file
)) {
  stop(
    "No encuentro bootstrap: ",
    bootstrap_file
  )
}

bootstrap_index <- read_csv(
  bootstrap_file,
  show_col_types = FALSE
) %>%
  mutate(
    bootstrap_id =
      as.integer(
        bootstrap_id
      ),
    integrated_row_id =
      as.character(
        integrated_row_id
      )
  )

# Crear draw_id si no existiese
if (
  !"draw_id" %in%
  names(
    bootstrap_index
  )
) {
  bootstrap_index <- bootstrap_index %>%
    group_by(
      bootstrap_id
    ) %>%
    mutate(
      draw_id =
        row_number()
    ) %>%
    ungroup()
}

# IDs de bootstrap
boot_ids <- sort(
  unique(
    bootstrap_index$
      bootstrap_id
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

bootstrap_index <- bootstrap_index %>%
  filter(
    bootstrap_id %in%
      boot_ids
  )

cat("\nBootstraps que se utilizarán: ",
  length(
    boot_ids
  ),"\n", sep = ""
)

# EJECUTAR PARALLEL ANALYSIS
results_list <- list()
run_counter <- 0L

for (
  matrix_index in
  seq_along(
    MATRICES_TO_RUN
  )
) {
  
  matrix_name <-
    MATRICES_TO_RUN[
      matrix_index
    ]
  
  cat(
    "Parallel Analysis | Matriz: ",
    matrix_name,
    "\n",
    sep = ""
  )
  # Leer matriz
  matrix_obj <- read_matrix_file(
    matrix_name
  )
  matrix_df <-
    matrix_obj$data
  feature_cols <-
    matrix_obj$feature_cols
  # BOOTSTRAPS
  for (
    b in
    boot_ids
  ) {
    run_counter <-
      run_counter +
      1L
    # Recuperar individuos de este bootstrap
    boot_b <- bootstrap_index %>%
      filter(
        bootstrap_id == b
      ) %>%
      select(
        bootstrap_id,
        draw_id,
        integrated_row_id
      )
    
    # Añadir los 32 determinantes
    sample_df <- boot_b %>%
      left_join(
        matrix_df,
        by =
          "integrated_row_id"
      )
    n_bootstrap_draws <-
      nrow(
        sample_df
      )
    # Mantener solamente filas completas
    sample_use <- sample_df %>%
      filter(
        if_all(
          all_of(
            feature_cols
          ),
          ~ !is.na(.x)
        )
      )
    
    n_rows_used <-
      nrow(
        sample_use
      )
    
    n_rows_dropped <-
      n_bootstrap_draws -
      n_rows_used
  
    # Matriz personas × determinantes
    x_df <- sample_use %>%
      select(
        all_of(
          feature_cols
        )
      )
    
    # VARIABLES CONSTANTES
    feature_sd <- map_dbl(
      x_df,
      ~ sd(
        as.numeric(
          .x
        ),
        na.rm = TRUE
      )
    )

    variable_features <-
      feature_cols[
        is.finite(
          feature_sd
        ) &
          feature_sd >
          1e-12
      ]

    constant_features <-
      setdiff(
        feature_cols,
        variable_features
      )

    if (
      length(
        variable_features
      ) < 3
    ) {

      results_list[[
        run_counter
      ]] <- tibble(
        
        matrix_name =
          matrix_name,
        
        bootstrap_id =
          b,
        
        status =
          "too_few_variables",
        
        n_bootstrap_draws =
          n_bootstrap_draws,
        
        n_rows_used =
          n_rows_used,
        
        n_rows_dropped =
          n_rows_dropped,
        
        n_variable_features =
          length(
            variable_features
          ),
        
        n_constant_features =
          length(
            constant_features
          ),
        
        n_factors_parallel =
          NA_integer_,
        
        warning_flag =
          FALSE,
        
        warning_message =
          NA_character_,
        
        error_message =
          "Too few variable features"
      )
      
      
      next
    }
    
  
    # Matriz que entra al Parallel Analysis
    x <- x_df %>%
      
      select(
        all_of(
          variable_features
        )
      ) %>%
      
      as.matrix()
  
    
    # SEMILLA REPRODUCIBLE
    # Como Parallel Analysis genera datos aleatorios,
    # fijamos una seed distinta y reproducible para:
    #
    # matriz × bootstrap
    set.seed(
      
      3000000 +
        
        matrix_index *
        100000 +
        
        b
    )
    
    # EJECUTAR PARALLEL ANALYSIS
    pa_safe <-
      safe_parallel_analysis(
        x
      )
    
    pa_fit <-
      pa_safe$result
    
    warning_messages <-
      pa_safe$warnings
    
    
    warning_flag <-
      length(
        warning_messages
      ) > 0
    s
    warning_message <- if (
      warning_flag
    ) {
      
      paste(
        
        warning_messages,
        
        collapse =
          " | "
      )
      
    } else {
      
      NA_character_
    }
    
    
    # ERROR
    if (
      inherits(
        pa_fit,
        "error"
      )
    ) {
      
      results_list[[
        run_counter
      ]] <- tibble(
        
        matrix_name =
          matrix_name,
        
        bootstrap_id =
          b,
        
        status =
          "error",
        
        n_bootstrap_draws =
          n_bootstrap_draws,
        
        n_rows_used =
          n_rows_used,
        
        n_rows_dropped =
          n_rows_dropped,
        
        n_variable_features =
          length(
            variable_features
          ),
        
        n_constant_features =
          length(
            constant_features
          ),
        
        n_factors_parallel =
          NA_integer_,
        
        warning_flag =
          warning_flag,
        
        warning_message =
          warning_message,
        
        error_message =
          pa_fit$message
      )
      
      
      next
    }
    
    
    # EXTRAER Nº RECOMENDADO DE FACTORES
    # psych::fa.parallel() devuelve el número recomendado
    # de factores en:
    #       $nfact
    
    n_factors_parallel <- if (
      
      !is.null(
        pa_fit$nfact
      )
      
    ) {
      
      as.integer(
        pa_fit$nfact
      )
      
    } else {
      
      NA_integer_
    }
 # guardar
    results_list[[
      run_counter
    ]] <- tibble(
      
      matrix_name =
        matrix_name,
      
      bootstrap_id =
        b,
      
      status =
        if_else(
          
          warning_flag,
          
          "ok_with_warning",
          
          "ok"
        ),
      
      n_bootstrap_draws =
        n_bootstrap_draws,
      
      n_rows_used =
        n_rows_used,
      
      n_rows_dropped =
        n_rows_dropped,
      
      n_variable_features =
        length(
          variable_features
        ),
      
      n_constant_features =
        length(
          constant_features
        ),
      
      n_factors_parallel =
        n_factors_parallel,
      
      warning_flag =
        warning_flag,
      
      warning_message =
        warning_message,
      
      error_message =
        NA_character_
    )
  }
}

# UNIR RESULTADOS
parallel_by_run <-
  bind_rows(
    results_list
  )

# DISTRIBUCIÓN DEL Nº DE FACTORES
#
# Para cada matriz calculamos:
#
# nº de bootstraps que seleccionan:
#
# 3 factores
# 4 factores
# 5 factores
# 6 factores
# ...
#
# y el porcentaje correspondiente.

parallel_frequency <- parallel_by_run %>%
  
  filter(
    
    status %in%
      c(
        "ok",
        "ok_with_warning"
      ),
    
    !is.na(
      n_factors_parallel
    )
  ) %>%
  
  count(
    
    matrix_name,
    
    n_factors_parallel,
    
    name =
      "n_bootstraps"
  ) %>%
  
  group_by(
    matrix_name
  ) %>%
  
  mutate(
    
    total_valid_bootstraps =
      sum(
        n_bootstraps
      ),
    
    pct_bootstraps =
      100 *
      n_bootstraps /
      total_valid_bootstraps
  ) %>%
  
  ungroup() %>%
  
  arrange(
    
    matrix_name,
    
    n_factors_parallel
  )

# RESUMEN POR MATRIZ
parallel_summary <- parallel_by_run %>%
  
  filter(
    
    status %in%
      c(
        "ok",
        "ok_with_warning"
      ),
    
    !is.na(
      n_factors_parallel
    )
  ) %>%
  
  group_by(
    matrix_name
  ) %>%
  
  summarise(
    
    n_valid =
      n(),
    
    
    # Media del nº seleccionado
    mean_n_factors =
      mean(
        n_factors_parallel,
        na.rm = TRUE
      ),
    
    
    # Mediana
    median_n_factors =
      median(
        n_factors_parallel,
        na.rm = TRUE
      ),
    
  
    # Percentiles
    q25_n_factors =
      quantile(
        n_factors_parallel,
        0.25,
        na.rm = TRUE
      ),
    
    
    q75_n_factors =
      quantile(
        n_factors_parallel,
        0.75,
        na.rm = TRUE
      ),
    
    # Moda
    modal_n_factors =
      as.integer(
        
        names(
          
          which.max(
            
            table(
              n_factors_parallel
            )
          )
        )
      ),
    
    # % de bootstraps que seleccionan la moda
    modal_frequency =
      max(
        
        as.integer(
          
          table(
            n_factors_parallel
          )
        )
      ),
    
    
    pct_modal =
      100 *
      
      modal_frequency /
      n_valid,
    
    # Nº de warnings
    n_warning =
      sum(
        warning_flag %in% TRUE,
        na.rm = TRUE
      ),
    
    
    .groups =
      "drop"
  )


# WARNINGS / ERRORES
parallel_warnings <- parallel_by_run %>%
  
  filter(
    warning_flag %in% TRUE
  )


parallel_errors <- parallel_by_run %>%
  
  filter(
    
    !status %in%
      c(
        "ok",
        "ok_with_warning"
      )
  )

# COMPARACIÓN CON BIC DEL SCRIPT 07
# Recordatorio:
#
# para cada matriz buscamos el nº de factores con
# MENOR BIC.
#
# Luego lo comparamos con la moda del Parallel Analysis.

efa_summary_file <- file.path(
  
  efa_dir,
  
  "02_efa_metrics_summary.csv"
)


if (
  file.exists(
    efa_summary_file
  )
) {
  
  
  efa_summary <- read_csv(
    
    efa_summary_file,
    
    show_col_types = FALSE
  )
  
  
  
  bic_best <- efa_summary %>%
    
    group_by(
      matrix_name
    ) %>%
    
    slice_min(
      
      order_by =
        mean_BIC,
      
      n =
        1,
      
      with_ties =
        FALSE
    ) %>%
    
    ungroup() %>%
    
    select(
      
      matrix_name,
      
      bic_best_n_factors =
        n_factors,
      
      best_mean_BIC =
        mean_BIC
    )
  
  
  
  parallel_vs_bic <- parallel_summary %>%
    
    left_join(
      
      bic_best,
      
      by =
        "matrix_name"
    ) %>%
    
    mutate(
      
      same_number =
        modal_n_factors ==
        bic_best_n_factors
    )
  
  
} else {
  
  
  parallel_vs_bic <- parallel_summary %>%
    
    mutate(
      
      bic_best_n_factors =
        NA_integer_,
      
      best_mean_BIC =
        NA_real_,
      
      same_number =
        NA
    )
}


# NOMBRES CORTOS DE LAS MATRICES
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



parallel_frequency <- parallel_frequency %>%
  
  mutate(
    
    matrix =
      matrix_label(
        matrix_name
      )
  )



parallel_summary <- parallel_summary %>%
  
  mutate(
    
    matrix =
      matrix_label(
        matrix_name
      )
  )



parallel_vs_bic <- parallel_vs_bic %>%
  
  mutate(
    
    matrix =
      matrix_label(
        matrix_name
      )
  )



# GUARDAR CSV
write_csv(
  
  parallel_by_run,
  
  file.path(
    out_dir,
    "01_parallel_analysis_by_run.csv"
  )
)



write_csv(
  
  parallel_frequency,
  
  file.path(
    out_dir,
    "02_parallel_factor_frequency.csv"
  )
)



write_csv(
  
  parallel_summary,
  
  file.path(
    out_dir,
    "03_parallel_summary.csv"
  )
)



write_csv(
  
  parallel_vs_bic,
  
  file.path(
    out_dir,
    "04_parallel_vs_bic.csv"
  )
)



write_csv(
  
  parallel_warnings,
  
  file.path(
    out_dir,
    "05_parallel_warnings.csv"
  )
)



write_csv(
  
  parallel_errors,
  
  file.path(
    out_dir,
    "06_parallel_errors.csv"
  )
)



# FIGURA:    DISTRIBUCIÓN DEL Nº DE FACTORES
# Este será el gráfico PRINCIPAL.
#
# Eje X:
#   nº de factores sugeridos
#
# Eje Y:
#   % de bootstraps
#
# Si aparece, por ejemplo:
#
# RAW:
#
# F5 -> 15%
# F6 -> 70%
# F7 -> 15%
#
# tendremos una señal bastante clara hacia 6.
p_distribution <- ggplot(
  
  parallel_frequency,
  
  aes(
    
    x =
      factor(
        n_factors_parallel
      ),
    
    y =
      pct_bootstraps
  )
  
) +
  
  geom_col(
    
    fill =
      "#4C78A8",
    
    width =
      0.7
  ) +
  
  geom_text(
    
    aes(
      
      label =
        paste0(
          
          round(
            pct_bootstraps,
            1
          ),
          
          "%"
        )
    ),
    
    vjust =
      -0.4,
    
    size =
      3.4
  ) +
  
  facet_wrap(
    
    ~ matrix,
    
    scales =
      "free_x",
    
    ncol =
      2
  ) +
  
  scale_y_continuous(
    
    limits =
      c(
        0,
        100
      )
  ) +
  
  theme_minimal(
    base_size = 12
  ) +
  
  theme(
    
    plot.title =
      element_text(
        face = "bold"
      ),
    
    strip.text =
      element_text(
        face = "bold"
      ),
    
    panel.grid.minor =
      element_blank()
  ) +
  
  labs(
    
    title =
      "EFA Parallel Analysis across bootstrap samples",
    
    subtitle =
      paste0(
        
        length(
          boot_ids
        ),
        
        " bootstraps | ",
        
        N_PA_ITER,
        
        " random datasets per Parallel Analysis"
      ),
    
    x =
      "Number of factors suggested by Parallel Analysis",
    
    y =
      "Bootstrap samples (%)"
  )



ggsave(
  
  filename =
    file.path(
      
      fig_dir,
      
      "01_parallel_factor_distribution.png"
    ),
  
  plot =
    p_distribution,
  
  width =
    10,
  
  height =
    7,
  
  dpi =
    300,
  
  bg =
    "white"
)


# FIGURA:
#     PARALLEL ANALYSIS VS BIC#
# Dos puntos por matriz:
#
#   PA  = moda del Parallel Analysis
#   BIC = mejor nº de factores por BIC
#
# Si coinciden:
#
# tenemos dos criterios diferentes apoyando
# el mismo nº de factores.
comparison_plot_data <- parallel_vs_bic %>%
  
  select(
    
    matrix,
    
    parallel =
      modal_n_factors,
    
    BIC =
      bic_best_n_factors
  ) %>%
  
  pivot_longer(
    
    cols =
      c(
        parallel,
        BIC
      ),
    
    names_to =
      "criterion",
    
    values_to =
      "n_factors"
  )



p_comparison <- ggplot(
  
  comparison_plot_data,
  
  aes(
    
    x =
      matrix,
    
    y =
      n_factors,
    
    color =
      criterion,
    
    group =
      criterion
  )
  
) +
  
  geom_point(
    size = 4
  ) +
  
  geom_line(
    linewidth = 0.8
  ) +
  
  scale_y_continuous(
    
    breaks =
      seq(
        1,
        15,
        by = 1
      )
  ) +
  
  theme_minimal(
    base_size = 12
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
      "Number of EFA factors: Parallel Analysis vs BIC",
    
    subtitle =
      "Parallel Analysis uses the modal recommendation across bootstrap samples",
    
    x =
      "Matrix",
    
    y =
      "Number of factors",
    
    color =
      "Criterion"
  )



ggsave(
  
  filename =
    file.path(
      
      fig_dir,
      
      "02_parallel_vs_bic.png"
    ),
  
  plot =
    p_comparison,
  
  width =
    9,
  
  height =
    6,
  
  dpi =
    300,
  
  bg =
    "white"
)

# PARÁMETROS DEL ANÁLISIS
parameters <- tibble(
  
  parameter = c(
    
    "bootstrap_scenario",
    "matrices",
    "fa_method",
    "parallel_iterations",
    "parallel_quantile",
    "max_bootstraps"
  ),
  
  value = c(
    
    BOOTSTRAP_SCENARIO,
    
    paste(
      MATRICES_TO_RUN,
      collapse = ", "
    ),
    
    FA_METHOD,
    
    as.character(
      N_PA_ITER
    ),
    
    as.character(
      PA_QUANTILE
    ),
    
    as.character(
      MAX_BOOTSTRAPS
    )
  )
)

write_csv(
  
  parameters,
  
  file.path(
    out_dir,
    "07_parallel_analysis_parameters.csv"
  )
)

# CONSOLA
cat(
  "PARALLEL ANALYSIS COMPLETADO\n"
)

cat(
  "\nBootstraps utilizados: ",
  length(boot_ids),
  "\n",
  sep = ""
)


cat(
  "Simulaciones aleatorias por bootstrap: ",
  N_PA_ITER,
  "\n",
  sep = ""
)

cat(
  "DISTRIBUCIÓN DEL Nº DE FACTORES\n"
)


print(
  
  parallel_frequency %>%
    
    select(
      
      matrix,
      
      n_factors_parallel,
      
      n_bootstraps,
      
      pct_bootstraps
    ),
  
  n = Inf,
  
  width = Inf
)


cat(
  "RESUMEN POR MATRIZ\n"
)


print(
  
  parallel_summary %>%
    
    select(
      
      matrix,
      
      n_valid,
      
      mean_n_factors,
      
      median_n_factors,
      
      modal_n_factors,
      
      modal_frequency,
      
      pct_modal,
      
      n_warning
    ),
  
  n = Inf,
  
  width = Inf
)



cat(
  "PARALLEL ANALYSIS VS BIC\n"
)

print(
  
  parallel_vs_bic %>%
    
    select(
      
      matrix,
      
      modal_n_factors,
      
      pct_modal,
      
      bic_best_n_factors,
      
      same_number
    ),
  
  n = Inf,
  
  width = Inf
)


cat(
  "WARNINGS / ERRORES\n"
)


cat(
  
  "\nWarnings: ",
  
  nrow(
    parallel_warnings
  ),
  
  "\n",
  
  sep = ""
)



cat(
  
  "Errores: ",
  
  nrow(
    parallel_errors
  ),
  
  "\n",
  
  sep = ""
)



cat(
  "\nResultados guardados en:\n"
)


cat(
  out_dir,
  "\n"
)



message(
  "\nListo. Parallel Analysis completado."
)