

# EFA (Exploratory Factor Analysis) sobre las muestras
# bootstrap.
#
# Para cada:
#   - bootstrap
#   - matriz: raw / pos / ext / z_abs
#   - número de factores
#
# hacemos:
#
# 1. coger los integrated_row_id del bootstrap
# 2. buscar sus 32 determinantes en la matriz del 05
# 3. ejecutar EFA
# 4. guardar:
#      - loadings
#      - importancia de cada determinante
#      - ranking de determinantes
#      - correlación entre factores
#      - métricas de ajuste
#
# IMPORTANCIA DE UN DETERMINANTE:
#
#     importance = MAX(abs(loading))
#
# se mira cuál es su carga absoluta máxima entre todos los factores.

# metricas a tener en cuenta:
#BIC
#+
#  TLI / RMSEA / RMSR
#+
#  Parallel Analysis
#+
# interpretabilidad


suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(stringr)
  library(psych)
})

set.seed(123)

project_root <- path.expand("~/Desktop/MASTER/recommendation-engine/TFM")
processed_root <- file.path( project_root,"paper1_cluster/data/processed")

# Bootstrap usado
BOOTSTRAP_SCENARIO <- "04_2b_propensity_bootstrap_eu_pfe_esn_renew_greens_merged_clustering_usable"

bootstrap_file <- file.path( processed_root, BOOTSTRAP_SCENARIO, "bootstrap_samples_index.csv")

# Matrices creadas en el 05
matrix_dir <- file.path( processed_root, "05_clustering_matrices")

# Resultados EFA
out_dir <- file.path(processed_root,"07_efa_bootstrap")

dir.create(out_dir,recursive = TRUE,showWarnings = FALSE)

# MATRICES A ANALIZAR
MATRICES_TO_RUN <- c(
  "matrix_32_raw_0_1",  "matrix_32_pos_0_1", "matrix_32_ext_0_1", "matrix_32_z_abs"
)

# NÚMERO DE FACTORES -  Esto NO es D.
# probando distintas soluciones EFA.
# Después se ve cuál tiene más sentido por ajuste,
# estabilidad y correlación entre factores.

N_FACTORS_GRID <- 4:8

# D = NÚMERO DE DETERMINANTES IMPORTANTES
# Esto NO elimina determinantes.
#
# Solo permite estudiar:
# si me quedo con los D determinantes de mayor importancia,
# ¿cuáles aparecen de forma estable?

D_DET_GRID <- 4:15

# CONFIGURACIÓN EFA

# 1. minres:
#   método habitual y robusto para EFA.
#
# 2. oblimin:
#   permite que los factores estén correlacionados.
#
# Esto es importante porque queremos analizar la correlación entre factores.

FA_METHOD <- "minres"
FA_ROTATION <- "oblimin"

# BOOTSTRAPS
MAX_BOOTSTRAPS <- 100
# MAX_BOOTSTRAPS <- Inf

# LEER MATRIZ
read_matrix_file <- function(matrix_name) {
  
  file <- file.path( matrix_dir, paste0(matrix_name, ".csv"))
  
  if (!file.exists(file)) {
    stop("No encuentro la matriz: ", file)
  }
  
  mat_df <- read_csv(
    file,
    show_col_types = FALSE
  ) %>%
    mutate(
      integrated_row_id = as.character(integrated_row_id)
    ) %>%
    distinct(
      integrated_row_id,
      .keep_all = TRUE
    )
  
  # Detectar los 32 determinantes. Guardados en feature_cols
  feature_cols <- names(mat_df)[
    str_detect(names(mat_df), "^det_\\d{2}_")
  ]
  
  if (length(feature_cols) != 32) {
    stop( "La matriz ",  matrix_name,
      " debería tener 32 determinantes y tiene ", length(feature_cols))
  }
  
  mat_df <- mat_df %>%
    select(
      integrated_row_id,
      all_of(feature_cols)
    ) %>%
    mutate(
      across(
        all_of(feature_cols),
        as.numeric
      )
    )
  
  list(
    data = mat_df,
    feature_cols = feature_cols
  )
}

# EFA SEGURO
safe_efa <- function(
    x,
    n_factors
) {
  
  # Aquí se guardan los warnings generados
  warning_messages <- character()
  
  fit <- withCallingHandlers(
    tryCatch(
      psych::fa(
        x,
        nfactors = n_factors,
        rotate = FA_ROTATION,
        fm = FA_METHOD,
        scores = "none"
      ),
      
      # Si psych::fa falla completamente, devolvemos el objeto error
      error = function(e) e
    ),
    
    # Si aparece un warning:
    warning = function(w) {
      
      warning_messages <<- c(
        warning_messages,
        conditionMessage(w)
      )
      
      # Evitamos que el warning se imprima cientos de veces
      # en consola, pero NO lo perdemos porque ya lo hemos
      # almacenado arriba.
      invokeRestart("muffleWarning")
    }
  )
  
  # Devolvemos conjuntamente:
  # - el ajuste EFA
  # - los warnings encontrados
  list(
    fit = fit,
    warnings = unique(
      warning_messages
    )
  )
}

# CORRELACIONES ENTRE FACTORES
extract_factor_correlations <- function(
    phi,
    matrix_name,
    bootstrap_id,
    n_factors
) {
  
  # Con rotación oblimin psych guarda las correlaciones entre factores en $Phi
  if (is.null(phi)) {
    return(tibble())
  }
  
  phi <- as.matrix(phi)
  
  if (nrow(phi) < 2) {
    return(tibble())
  }
  
  # Solo parte superior: F1-F2, F1-F3, F2-F3...
  idx <- which(
    upper.tri(phi),
    arr.ind = TRUE
  )
  
  tibble(
    matrix_name = matrix_name,
    bootstrap_id = bootstrap_id,
    n_factors = n_factors,
    factor_a =
      rownames(phi)[idx[, "row"]],
    factor_b =
      colnames(phi)[idx[, "col"]],
    correlation =
      as.numeric(phi[idx])
    
  ) %>%
    mutate(
      abs_correlation =
        abs(correlation),
      # Medida análoga conceptualmente
      # a una distancia:
      # correlación alta -> distancia baja
      # correlación baja -> distancia alta
      #
      # abs() porque el signo de los
      # factores EFA puede invertirse sin
      # cambiar la solución factorial
      correlation_distance =
        1 - abs_correlation
    )
}

# EJECUTAR UN EFA
run_one_efa <- function(
    bootstrap_id_current,
    matrix_name_current,
    matrix_df,
    feature_cols,
    n_factors_current,
    bootstrap_index
) {

  # Recuperar las personas de este bootstrap
  boot_b <- bootstrap_index %>%
    filter(
      bootstrap_id ==
        bootstrap_id_current
    ) %>%
    select(
      bootstrap_id,
      draw_id,
      integrated_row_id
    )

  # Buscar sus 32 determinantes
  sample_df <- boot_b %>%
    left_join(
      matrix_df,
      by = "integrated_row_id"
    )
  
  n_bootstrap_draws <- nrow(
    sample_df
  )
  
  # Mantener filas completas
  sample_use <- sample_df %>%
    filter(
      if_all(
        all_of(feature_cols),
        ~ !is.na(.x)
      )
    )
  
  n_rows_used <- nrow(
    sample_use
  )
  
  n_rows_dropped <-
    n_bootstrap_draws -
    n_rows_used
  
  # Matriz personas × determinantes
  x_df <- sample_use %>%
    select(
      all_of(feature_cols)
    )

  # Detectar determinantes sin variación
  # Puede pasar especialmente en POS o EXT.
  # Un determinante constante no puede entrar en EFA.
  feature_sd <- map_dbl(
    x_df,
    ~ sd(
      as.numeric(.x),
      na.rm = TRUE
    )
  )
  
  variable_features <-
    feature_cols[
      is.finite(feature_sd) &
        feature_sd > 1e-12
    ]
  
  constant_features <-
    setdiff(
      feature_cols,
      variable_features
    )
  
  # Demasiadas pocas variables
  if (
    length(variable_features) <=
    n_factors_current + 1
  ) {
    return(
      list(
        metrics = tibble(
          matrix_name =
            matrix_name_current,
          bootstrap_id =
            bootstrap_id_current,
          n_factors =
            n_factors_current,
          status =
            "too_few_variables",
          n_rows_used =
            n_rows_used,
          n_rows_dropped =
            n_rows_dropped,
          n_variable_features =
            length(variable_features),
          warning_flag =
            FALSE,
          warning_message =
            NA_character_,
          error_message =
            "Too few variable features"
        ),
        loadings = tibble(),
        importance = tibble(),
        factor_correlations = tibble(),
        top_d = tibble()
      )
    )
  }

  x <- x_df %>%
    select(
      all_of(variable_features)
    ) %>%
    as.matrix()
  
  # Reproducibilidad
  set.seed(
    2000000 +
      bootstrap_id_current * 100 +
      n_factors_current
  )
  
  # Ejecutar EFA
  efa_result <- safe_efa(
    x,
    n_factors_current
  )
  
  # El ajuste EFA propiamente dicho
  fit <- efa_result$fit
  
  # Warnings generados durante el ajuste
  warning_messages <- efa_result$warnings
  
  # TRUE si apareció al menos un warning
  warning_flag <- length(
    warning_messages
  ) > 0
  
  # Guardamos todos los warnings en una única cadena
  warning_message <- if (
    warning_flag
  ) {
    paste(
      warning_messages,
      collapse = " | "
    )
  } else {
    NA_character_
  }
  
  if (inherits(fit, "error")) {
    return(
      list(
        metrics = tibble(
          matrix_name =
            matrix_name_current,
          bootstrap_id =
            bootstrap_id_current,
          n_factors =
            n_factors_current,
          status =
            "error",
          n_rows_used =
            n_rows_used,
          n_rows_dropped =
            n_rows_dropped,
          n_variable_features =
            length(variable_features),
          warning_flag =
            warning_flag,
          warning_message =
            warning_message,
          error_message =
            fit$message
        ),
        loadings = tibble(),
        importance = tibble(),
        factor_correlations = tibble(),
        top_d = tibble()
      )
    )
  }

  # LOADINGS
  loading_matrix <-
    as.matrix(
      unclass(fit$loadings)
    )
  
  loadings_long <-
    as_tibble(
      loading_matrix,
      rownames = "determinant"
    ) %>%
    pivot_longer(
      cols = -determinant,
      names_to = "factor",
      values_to = "loading"
    ) %>%
    mutate(
      matrix_name =
        matrix_name_current,
      bootstrap_id =
        bootstrap_id_current,
      n_factors =
        n_factors_current,
      abs_loading =
        abs(loading)
    ) %>%
    relocate(
      matrix_name,
      bootstrap_id,
      n_factors,
      determinant,
      factor
    )

  # IMPORTANCIA DE CADA DETERMINANTE
  # Tal como estaba en tus notas:
  #
  #       MAX(abs(x))
  #
  # Cogemos, para cada determinante, su loading absoluto máximo.
  importance_variable <-
    loadings_long %>%
    group_by(
      matrix_name,
      bootstrap_id,
      n_factors,
      determinant
    ) %>%
    
    summarise(
      importance =
        max(
          abs_loading,
          na.rm = TRUE
        ),
      mean_abs_loading =
        mean(
          abs_loading,
          na.rm = TRUE
        ),
      strongest_factor =
        factor[
          which.max(abs_loading)
        ][1],
      
      .groups = "drop"
    )
  
  # Si una variable era constante,
  # le damos importancia 0.
  importance_constant <- tibble(
    matrix_name =
      matrix_name_current,
    bootstrap_id =
      bootstrap_id_current,
    n_factors =
      n_factors_current,
    determinant =
      constant_features,
    importance = 0,
    mean_abs_loading = 0,
    strongest_factor =
      NA_character_
  )
  
  determinant_importance <-
    bind_rows(
      importance_variable,
      importance_constant
    ) %>%
    arrange(
      desc(importance),
      determinant
    ) %>%
    mutate(
      importance_rank =
        row_number(),
      above_0_20 =
        importance >= 0.20,
      above_0_30 =
        importance >= 0.30,
      above_0_40 =
        importance >= 0.40
    )

  # TOP D DETERMINANTES
  # Esto NO decide todavía cuál D es correcta.
  # Solo guarda:
  # D = 4
  # D = 5
  # ...
  # D = 15
  # para poder estudiar después qué corte tiene sentido.
  top_d <- map_dfr(
    D_DET_GRID,
    function(d_det) {
      determinant_importance %>%
        slice_head(
          n = d_det
        ) %>%
        transmute(
          matrix_name,
          bootstrap_id,
          n_factors,
          d_det =
            d_det,
          determinant,
          importance,
          importance_rank
        )
    }
  )

  # CORRELACIÓN ENTRE FACTORES
  factor_correlations <-
    extract_factor_correlations(
      phi = fit$Phi,
      matrix_name =
        matrix_name_current,
      bootstrap_id =
        bootstrap_id_current,
      n_factors =
        n_factors_current
    )
  
  # RESUMIR CORRELACIONES
  if (
    nrow(factor_correlations) > 0
  ) {
    mean_abs_cor <-
      mean(
        factor_correlations$
          abs_correlation,
        na.rm = TRUE
      )
    var_abs_cor <-
      var(
        factor_correlations$
          abs_correlation,
        na.rm = TRUE
      )
    mean_distance <-
      mean(
        factor_correlations$
          correlation_distance,
        na.rm = TRUE
      )
    var_distance <-
      var(
        factor_correlations$
          correlation_distance,
        na.rm = TRUE
      )
    
  } else {

    mean_abs_cor <- NA_real_
    var_abs_cor <- NA_real_
    mean_distance <- NA_real_
    var_distance <- NA_real_
  }
  
  # MÉTRICAS DEL EFA
  rmsea_value <- if (
    !is.null(fit$RMSEA)
  ) {
    as.numeric(
      fit$RMSEA[1]
    )
  } else {
    NA_real_
  }
  
  metrics <- tibble(
    matrix_name =
      matrix_name_current,
    bootstrap_id =
      bootstrap_id_current,
    n_factors =
      n_factors_current,
    status = if_else(
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
      length(variable_features),
    n_constant_features =
      length(constant_features),
    RMSR =
      if (!is.null(fit$rms))
        as.numeric(fit$rms)
    else NA_real_,
    TLI =
      if (!is.null(fit$TLI))
        as.numeric(fit$TLI)
    else NA_real_,
    RMSEA =
      rmsea_value,
    BIC =
      if (!is.null(fit$BIC))
        as.numeric(fit$BIC)
    else NA_real_,
    mean_abs_factor_correlation =
      mean_abs_cor,
    var_abs_factor_correlation =
      var_abs_cor,
    mean_factor_distance =
      mean_distance,
    var_factor_distance =
      var_distance,
    warning_flag =
      warning_flag,
    warning_message =
      warning_message,
    error_message =
      NA_character_
  )
  
  list(
    metrics = metrics,
    loadings =
      loadings_long,
    importance =
      determinant_importance,
    factor_correlations =
      factor_correlations,
    top_d =
      top_d
  )
}

# LEER BOOTSTRAP
if (!file.exists(bootstrap_file)) {
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
      as.integer(bootstrap_id),
    integrated_row_id =
      as.character(integrated_row_id)
  )
if (!"draw_id" %in%
    names(bootstrap_index)) {
  bootstrap_index <-
    bootstrap_index %>%
    group_by(
      bootstrap_id
    ) %>%
    mutate(
      draw_id = row_number()
    ) %>%
    ungroup()
}

boot_ids <-
  sort(
    unique(
      bootstrap_index$
        bootstrap_id
    )
  )

if (
  is.finite(MAX_BOOTSTRAPS)
) {
  boot_ids <-
    head(
      boot_ids,
      MAX_BOOTSTRAPS
    )
}

bootstrap_index <-
  bootstrap_index %>%
  filter(
    bootstrap_id %in%
      boot_ids
  )

#################################### EJECUTAR EFA
metrics_list <- list()
loadings_list <- list()
importance_list <- list()
correlations_list <- list()
top_d_list <- list()

run_counter <- 0L

for (
  matrix_name in MATRICES_TO_RUN
) {
  cat(
    "Matriz:",
    matrix_name,
    "\n"
  )
  matrix_obj <-
    read_matrix_file(
      matrix_name
    )
  matrix_df <-
    matrix_obj$data
  feature_cols <-
    matrix_obj$feature_cols
  for (
    n_factors in N_FACTORS_GRID
  ) {
    cat("  Factores =", n_factors, "\n")
    for (
      b in boot_ids
    ) {
      run_counter <-
        run_counter + 1L
      res <- run_one_efa(
        bootstrap_id_current = b,
        matrix_name_current =
          matrix_name,
        matrix_df =
          matrix_df,
        feature_cols =
          feature_cols,
        n_factors_current =
          n_factors,
        bootstrap_index =
          bootstrap_index
      )
      
      metrics_list[[run_counter]] <-
        res$metrics
      loadings_list[[run_counter]] <-
        res$loadings
      importance_list[[run_counter]] <-
        res$importance
      correlations_list[[run_counter]] <-
        res$factor_correlations
      top_d_list[[run_counter]] <-
        res$top_d
    }
  }
}

#################################### UNIR RESULTADOS
efa_metrics_by_run <- bind_rows(metrics_list)
efa_loadings_long <- bind_rows(loadings_list)
efa_determinant_importance_by_run <- bind_rows(importance_list)
efa_factor_correlations_by_run <- bind_rows(correlations_list)
efa_top_d_by_run <- bind_rows(top_d_list)

#################################### RESUMEN DE MÉTRICAS
efa_metrics_summary <-
  efa_metrics_by_run %>%
  group_by(
    matrix_name,
    n_factors
  ) %>%
  summarise(
    n_runs = n(),
    n_ok =
      sum(
        status %in%
          c(
            "ok",
            "ok_with_warning"
          )
      ),
    # Nº de ajustes que generaron algún warning
    n_warning =
      sum(
        warning_flag %in% TRUE,
        na.rm = TRUE
      ),
    # Nº de errores reales
    n_error =
      sum(
        status == "error"
      ),
    mean_RMSR =
      mean(
        RMSR,
        na.rm = TRUE
      ),
    mean_TLI =
      mean(
        TLI,
        na.rm = TRUE
      ),
    mean_RMSEA =
      mean(
        RMSEA,
        na.rm = TRUE
      ),
    mean_BIC =
      mean(
        BIC,
        na.rm = TRUE
      ),
    mean_abs_factor_correlation =
      mean(
        mean_abs_factor_correlation,
        na.rm = TRUE
      ),
    var_abs_factor_correlation =
      mean(
        var_abs_factor_correlation,
        na.rm = TRUE
      ),
    mean_factor_distance =
      mean(
        mean_factor_distance,
        na.rm = TRUE
      ),
    var_factor_distance =
      mean(
        var_factor_distance,
        na.rm = TRUE
      ),
    .groups = "drop"
  )

####################################  RESUMEN DE WARNINGS
efa_warnings <- efa_metrics_by_run %>%
  filter(
    warning_flag %in% TRUE
  ) %>%
  select(
    matrix_name,
    bootstrap_id,
    n_factors,
    status,
    warning_message
  )

####################################  RESUMEN DE IMPORTANCIA DE DETERMINANTES
efa_determinant_importance_summary <-
  efa_determinant_importance_by_run %>%
  group_by(
    matrix_name,
    n_factors,
    determinant
  ) %>%
  summarise(
    mean_importance =
      mean(
        importance,
        na.rm = TRUE
      ),
    sd_importance =
      sd(
        importance,
        na.rm = TRUE
      ),
    median_importance =
      median(
        importance,
        na.rm = TRUE
      ),
    mean_rank =
      mean(
        importance_rank,
        na.rm = TRUE
      ),
    prop_above_0_20 =
      mean(
        importance >= 0.20,
        na.rm = TRUE
      ),
    prop_above_0_30 =
      mean(
        importance >= 0.30,
        na.rm = TRUE
      ),
    prop_above_0_40 =
      mean(
        importance >= 0.40,
        na.rm = TRUE
      ),
    .groups = "drop"
  ) %>%
  arrange(
    matrix_name,
    n_factors,
    desc(mean_importance)
  )

####################################  FRECUENCIA DE ENTRADA EN TOP-D
efa_top_d_frequency <-
  efa_top_d_by_run %>%
  count(
    matrix_name,
    n_factors,
    d_det,
    determinant,
    name = "n_selected"
  ) %>%
  group_by(
    matrix_name,
    n_factors,
    d_det
  ) %>%
  mutate(
    n_bootstraps =
      length(boot_ids),
    prop_bootstraps_selected =
      n_selected /
      n_bootstraps
  ) %>%
  ungroup() %>%
  arrange(
    matrix_name,
    n_factors,
    d_det,
    desc(
      prop_bootstraps_selected
    )
  )

##################################### GUARDAR
write_csv(efa_metrics_by_run,file.path( out_dir, "01_efa_metrics_by_run.csv"))
write_csv(efa_metrics_summary,file.path(out_dir,"02_efa_metrics_summary.csv"))
write_csv(efa_loadings_long,file.path(out_dir,"03_efa_loadings_long.csv"))
write_csv(efa_determinant_importance_by_run,file.path(out_dir,  "04_efa_determinant_importance_by_run.csv"))
write_csv(efa_determinant_importance_summary,file.path(out_dir, "05_efa_determinant_importance_summary.csv"))
write_csv(efa_factor_correlations_by_run,file.path(out_dir, "06_efa_factor_correlations_by_run.csv"))
write_csv( efa_top_d_by_run,file.path( out_dir, "07_efa_top_d_by_run.csv"))
write_csv(efa_top_d_frequency,file.path(out_dir,"08_efa_top_d_frequency.csv"))
write_csv(efa_warnings,file.path(out_dir, "09_efa_warnings.csv"))

####################################  CONSOLA
cat("EFA SOBRE BOOTSTRAPS COMPLETADO\n")
cat( "\nBootstraps usados:", length(boot_ids),"\n")
cat(
  "\nMatrices:\n"
)
print(
  MATRICES_TO_RUN
)
cat(
  "\nNúmero de factores probados:\n"
)
print(
  N_FACTORS_GRID
)
cat(
  "\nResumen EFA:\n"
)
print(
  efa_metrics_summary,
  n = Inf,
  width = Inf
)
message(
  "\nResultados guardados en: ",
  out_dir
)