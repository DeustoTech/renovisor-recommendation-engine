# 07_efa.R
#
# OBJETIVO
# Explorar la estructura factorial de los 32 determinantes mediante
# Exploratory Factor Analysis (EFA) sobre las muestras bootstrap.
#
# QUÉ HACE ESTE SCRIPT
# - Usa el bootstrap combinado generado en 04_2f.
# - Analiza 7 muestras:
#   COMPLETE, EUROPE, LATAM, DIEGO, RENOVISOR, WHY_EUROPE y WHY_LATAM.
# - Usa las 4 matrices generadas en 05:
#   RAW, POS, EXT y Z_ABS.
# - Prueba soluciones EFA con 2 a 8 factores.
# - Ejecuta el análisis sobre 100 bootstraps en esta fase exploratoria.
# - Utiliza minres como método de extracción y oblimin como rotación.
# - Guarda las cargas factoriales de cada determinante.
# - Calcula la importancia de cada determinante como max(abs(loading)).
# - Resume qué determinantes aparecen de forma estable entre los más importantes.
# - Calcula correlaciones entre factores.
# - Guarda métricas de ajuste: RMSR, TLI, RMSEA y BIC.
# - Registra warnings y errores de cada ajuste.
# - Genera resultados globales y resultados separados para cada muestra.
#
# INTERPRETACIÓN
# Este script no fija todavía el número final de factores.
# Las soluciones se compararán posteriormente usando:
# - métricas de ajuste,
# - estabilidad entre bootstraps,
# - Parallel Analysis (07b, opcional),
# - interpretabilidad,
# - comparación posterior con K-means y estructura experta.
#
# IMPORTANTE
# El número de factores EFA no tiene por qué coincidir con el número
# de clusters K del K-means.
#
# CONFIGURACIÓN ACTUAL
# - Factores: 2:8
# - Bootstraps: 100
# - Método: minres
# - Rotación: oblimin


suppressPackageStartupMessages({
  library(tidyverse)
  library(psych)
})

set.seed(123)

project_root <- path.expand("~/Desktop/MASTER/recommendation-engine/TFM")
processed_root <- file.path(project_root, "paper1_cluster/data/processed")

bootstrap_file <- file.path(
  processed_root,
  "04_2f_combine_eu_latam_bootstrap",
  "bootstrap_samples_index.csv.gz"
)

matrix_dir <- file.path(
  processed_root,
  "05_clustering_matrices"
)

out_dir <- file.path(
  processed_root,
  "07_efa_bootstrap"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

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

N_FACTORS_GRID <- 2:8
D_DET_GRID <- 4:15

FA_METHOD <- "minres"
FA_ROTATION <- "oblimin"

EXPECTED_N_BOOT <- 1000L
MAX_BOOTSTRAPS <- 100L


mean_or_na <- function(x) {
  x <- x[is.finite(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  mean(x)
}


sd_or_na <- function(x) {
  x <- x[is.finite(x)]
  
  if (length(x) < 2) {
    return(NA_real_)
  }
  
  sd(x)
}


get_bootstrap_sample <- function(bootstrap_index, analysis_sample) {
  
  if (analysis_sample == "COMPLETE") {
    
    out <- bootstrap_index
    
  } else if (analysis_sample == "EUROPE") {
    
    out <- bootstrap_index %>%
      filter(comparison_region == "EUROPE")
    
  } else if (analysis_sample == "LATAM") {
    
    out <- bootstrap_index %>%
      filter(comparison_region == "LATAM")
    
  } else {
    
    out <- bootstrap_index %>%
      filter(subsample == analysis_sample)
  }
  
  if (nrow(out) == 0) {
    stop(
      "La muestra ",
      analysis_sample,
      " no tiene draws en el bootstrap."
    )
  }
  
  out
}


read_matrix_file <- function(analysis_sample, matrix_name) {
  
  file <- file.path(
    matrix_dir,
    analysis_sample,
    paste0(matrix_name, ".csv")
  )
  
  if (!file.exists(file)) {
    stop(
      "No encuentro la matriz:\n",
      file
    )
  }
  
  mat_df <- read_csv(
    file,
    show_col_types = FALSE
  ) %>%
    mutate(
      integrated_row_id = as.character(integrated_row_id)
    )
  
  if (anyDuplicated(mat_df$integrated_row_id)) {
    stop(
      "La matriz ",
      analysis_sample,
      " / ",
      matrix_name,
      " contiene integrated_row_id duplicados."
    )
  }
  
  feature_cols <- names(mat_df)[
    str_detect(
      names(mat_df),
      "^det_\\d{2}_"
    )
  ]
  
  if (length(feature_cols) != 32) {
    stop(
      "La matriz ",
      analysis_sample,
      " / ",
      matrix_name,
      " debería tener 32 determinantes y tiene ",
      length(feature_cols),
      "."
    )
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
  
  matrix_values <- mat_df %>%
    select(
      all_of(feature_cols)
    ) %>%
    as.matrix()
  
  storage.mode(matrix_values) <- "double"
  
  row_lookup <- seq_len(nrow(mat_df))
  names(row_lookup) <- mat_df$integrated_row_id
  
  list(
    data = mat_df,
    matrix = matrix_values,
    row_lookup = row_lookup,
    feature_cols = feature_cols
  )
}


prepare_bootstrap_matrix <- function(boot_b, matrix_obj) {
  
  row_idx <- unname(
    matrix_obj$row_lookup[
      boot_b$integrated_row_id
    ]
  )
  
  if (any(is.na(row_idx))) {
    stop(
      "Hay IDs del bootstrap que no existen en la matriz."
    )
  }
  
  x <- matrix_obj$matrix[
    row_idx,
    ,
    drop = FALSE
  ]
  
  complete_rows <- complete.cases(x)
  
  x <- x[
    complete_rows,
    ,
    drop = FALSE
  ]
  
  ids_used <- boot_b$integrated_row_id[
    complete_rows
  ]
  
  n_bootstrap_draws <- nrow(boot_b)
  n_rows_used <- nrow(x)
  
  feature_sd <- apply(
    x,
    2,
    sd,
    na.rm = TRUE
  )
  
  variable_features <- names(feature_sd)[
    is.finite(feature_sd) &
      feature_sd > 1e-12
  ]
  
  constant_features <- setdiff(
    matrix_obj$feature_cols,
    variable_features
  )
  
  if (length(variable_features) > 0) {
    
    x_variable <- x[
      ,
      variable_features,
      drop = FALSE
    ]
    
    correlation_matrix <- cor(
      x_variable
    )
    
  } else {
    
    x_variable <- matrix(
      numeric(),
      nrow = n_rows_used,
      ncol = 0
    )
    
    correlation_matrix <- matrix(
      numeric(),
      nrow = 0,
      ncol = 0
    )
  }
  
  list(
    x = x_variable,
    correlation_matrix = correlation_matrix,
    variable_features = variable_features,
    constant_features = constant_features,
    n_bootstrap_draws = n_bootstrap_draws,
    n_rows_used = n_rows_used,
    n_rows_dropped = n_bootstrap_draws - n_rows_used,
    n_unique_participants = n_distinct(ids_used)
  )
}


safe_efa <- function(
    correlation_matrix,
    n_obs,
    n_factors
) {
  
  warning_messages <- character()
  
  fit <- withCallingHandlers(
    tryCatch(
      psych::fa(
        r = correlation_matrix,
        nfactors = n_factors,
        n.obs = n_obs,
        rotate = FA_ROTATION,
        fm = FA_METHOD,
        scores = "none"
      ),
      error = function(e) e
    ),
    warning = function(w) {
      
      warning_messages <<- c(
        warning_messages,
        conditionMessage(w)
      )
      
      invokeRestart("muffleWarning")
    }
  )
  
  list(
    fit = fit,
    warnings = unique(warning_messages)
  )
}


extract_factor_correlations <- function(
    phi,
    analysis_sample,
    matrix_name,
    bootstrap_id,
    n_factors
) {
  
  if (is.null(phi)) {
    return(tibble())
  }
  
  phi <- as.matrix(phi)
  
  if (nrow(phi) < 2) {
    return(tibble())
  }
  
  idx <- which(
    upper.tri(phi),
    arr.ind = TRUE
  )
  
  tibble(
    analysis_sample = analysis_sample,
    matrix_name = matrix_name,
    bootstrap_id = bootstrap_id,
    n_factors = n_factors,
    factor_a = rownames(phi)[idx[, "row"]],
    factor_b = colnames(phi)[idx[, "col"]],
    correlation = as.numeric(phi[idx])
  ) %>%
    mutate(
      abs_correlation = abs(correlation),
      correlation_distance = 1 - abs_correlation
    )
}


failed_result <- function(
    analysis_sample,
    matrix_name,
    bootstrap_id,
    n_factors,
    status,
    prepared,
    error_message
) {
  
  list(
    metrics = tibble(
      analysis_sample = analysis_sample,
      matrix_name = matrix_name,
      bootstrap_id = bootstrap_id,
      n_factors = n_factors,
      status = status,
      n_bootstrap_draws = prepared$n_bootstrap_draws,
      n_rows_used = prepared$n_rows_used,
      n_rows_dropped = prepared$n_rows_dropped,
      n_unique_participants = prepared$n_unique_participants,
      n_variable_features = length(prepared$variable_features),
      n_constant_features = length(prepared$constant_features),
      RMSR = NA_real_,
      TLI = NA_real_,
      RMSEA = NA_real_,
      BIC = NA_real_,
      mean_abs_factor_correlation = NA_real_,
      var_abs_factor_correlation = NA_real_,
      mean_factor_distance = NA_real_,
      var_factor_distance = NA_real_,
      warning_flag = FALSE,
      warning_message = NA_character_,
      error_message = error_message
    ),
    loadings = tibble(),
    importance = tibble(),
    factor_correlations = tibble(),
    top_d = tibble()
  )
}


run_one_efa <- function(
    analysis_sample_current,
    analysis_sample_index,
    matrix_name_current,
    matrix_index_current,
    bootstrap_id_current,
    n_factors_current,
    prepared
) {
  
  if (
    prepared$n_rows_used <=
    n_factors_current + 1
  ) {
    return(
      failed_result(
        analysis_sample = analysis_sample_current,
        matrix_name = matrix_name_current,
        bootstrap_id = bootstrap_id_current,
        n_factors = n_factors_current,
        status = "too_few_rows",
        prepared = prepared,
        error_message = "Too few rows for requested number of factors"
      )
    )
  }
  
  if (
    length(prepared$variable_features) <=
    n_factors_current + 1
  ) {
    return(
      failed_result(
        analysis_sample = analysis_sample_current,
        matrix_name = matrix_name_current,
        bootstrap_id = bootstrap_id_current,
        n_factors = n_factors_current,
        status = "too_few_variables",
        prepared = prepared,
        error_message = "Too few variable features"
      )
    )
  }
  
  if (
    any(
      !is.finite(
        prepared$correlation_matrix
      )
    )
  ) {
    return(
      failed_result(
        analysis_sample = analysis_sample_current,
        matrix_name = matrix_name_current,
        bootstrap_id = bootstrap_id_current,
        n_factors = n_factors_current,
        status = "invalid_correlation_matrix",
        prepared = prepared,
        error_message = "Correlation matrix contains non-finite values"
      )
    )
  }
  
  set.seed(
    2000000 +
      analysis_sample_index * 100000 +
      matrix_index_current * 10000 +
      n_factors_current * 1000 +
      bootstrap_id_current
  )
  
  efa_result <- safe_efa(
    correlation_matrix = prepared$correlation_matrix,
    n_obs = prepared$n_rows_used,
    n_factors = n_factors_current
  )
  
  fit <- efa_result$fit
  warning_messages <- efa_result$warnings
  
  warning_flag <- length(
    warning_messages
  ) > 0
  
  warning_message <- if (warning_flag) {
    paste(
      warning_messages,
      collapse = " | "
    )
  } else {
    NA_character_
  }
  
  if (inherits(fit, "error")) {
    
    result <- failed_result(
      analysis_sample = analysis_sample_current,
      matrix_name = matrix_name_current,
      bootstrap_id = bootstrap_id_current,
      n_factors = n_factors_current,
      status = "error",
      prepared = prepared,
      error_message = fit$message
    )
    
    result$metrics$warning_flag <- warning_flag
    result$metrics$warning_message <- warning_message
    
    return(result)
  }
  
  loading_matrix <- as.matrix(
    unclass(
      fit$loadings
    )
  )
  
  loadings_long <- as_tibble(
    loading_matrix,
    rownames = "determinant"
  ) %>%
    pivot_longer(
      cols = -determinant,
      names_to = "factor",
      values_to = "loading"
    ) %>%
    mutate(
      analysis_sample = analysis_sample_current,
      matrix_name = matrix_name_current,
      bootstrap_id = bootstrap_id_current,
      n_factors = n_factors_current,
      abs_loading = abs(loading)
    ) %>%
    relocate(
      analysis_sample,
      matrix_name,
      bootstrap_id,
      n_factors,
      determinant,
      factor
    )
  
  importance_variable <- loadings_long %>%
    group_by(
      analysis_sample,
      matrix_name,
      bootstrap_id,
      n_factors,
      determinant
    ) %>%
    summarise(
      importance = max(
        abs_loading,
        na.rm = TRUE
      ),
      mean_abs_loading = mean(
        abs_loading,
        na.rm = TRUE
      ),
      strongest_factor = factor[
        which.max(abs_loading)
      ][1],
      .groups = "drop"
    )
  
  importance_constant <- tibble(
    analysis_sample = analysis_sample_current,
    matrix_name = matrix_name_current,
    bootstrap_id = bootstrap_id_current,
    n_factors = n_factors_current,
    determinant = prepared$constant_features,
    importance = 0,
    mean_abs_loading = 0,
    strongest_factor = NA_character_
  )
  
  determinant_importance <- bind_rows(
    importance_variable,
    importance_constant
  ) %>%
    arrange(
      desc(importance),
      determinant
    ) %>%
    mutate(
      importance_rank = row_number(),
      above_0_20 = importance >= 0.20,
      above_0_30 = importance >= 0.30,
      above_0_40 = importance >= 0.40
    )
  
  top_d <- map_dfr(
    D_DET_GRID,
    function(d_det_current) {
      
      determinant_importance %>%
        slice_head(
          n = d_det_current
        ) %>%
        transmute(
          analysis_sample,
          matrix_name,
          bootstrap_id,
          n_factors,
          d_det = d_det_current,
          determinant,
          importance,
          importance_rank
        )
    }
  )
  
  factor_correlations <- extract_factor_correlations(
    phi = fit$Phi,
    analysis_sample = analysis_sample_current,
    matrix_name = matrix_name_current,
    bootstrap_id = bootstrap_id_current,
    n_factors = n_factors_current
  )
  
  if (nrow(factor_correlations) > 0) {
    
    mean_abs_cor <- mean_or_na(
      factor_correlations$abs_correlation
    )
    
    var_abs_cor <- var(
      factor_correlations$abs_correlation,
      na.rm = TRUE
    )
    
    mean_distance <- mean_or_na(
      factor_correlations$correlation_distance
    )
    
    var_distance <- var(
      factor_correlations$correlation_distance,
      na.rm = TRUE
    )
    
  } else {
    
    mean_abs_cor <- NA_real_
    var_abs_cor <- NA_real_
    mean_distance <- NA_real_
    var_distance <- NA_real_
  }
  
  rmsea_value <- if (!is.null(fit$RMSEA)) {
    as.numeric(fit$RMSEA[1])
  } else {
    NA_real_
  }
  
  metrics <- tibble(
    analysis_sample = analysis_sample_current,
    matrix_name = matrix_name_current,
    bootstrap_id = bootstrap_id_current,
    n_factors = n_factors_current,
    status = if_else(
      warning_flag,
      "ok_with_warning",
      "ok"
    ),
    n_bootstrap_draws = prepared$n_bootstrap_draws,
    n_rows_used = prepared$n_rows_used,
    n_rows_dropped = prepared$n_rows_dropped,
    n_unique_participants = prepared$n_unique_participants,
    n_variable_features = length(
      prepared$variable_features
    ),
    n_constant_features = length(
      prepared$constant_features
    ),
    RMSR = if (!is.null(fit$rms)) {
      as.numeric(fit$rms)
    } else {
      NA_real_
    },
    TLI = if (!is.null(fit$TLI)) {
      as.numeric(fit$TLI)
    } else {
      NA_real_
    },
    RMSEA = rmsea_value,
    BIC = if (!is.null(fit$BIC)) {
      as.numeric(fit$BIC)
    } else {
      NA_real_
    },
    mean_abs_factor_correlation = mean_abs_cor,
    var_abs_factor_correlation = var_abs_cor,
    mean_factor_distance = mean_distance,
    var_factor_distance = var_distance,
    warning_flag = warning_flag,
    warning_message = warning_message,
    error_message = NA_character_
  )
  
  list(
    metrics = metrics,
    loadings = loadings_long,
    importance = determinant_importance,
    factor_correlations = factor_correlations,
    top_d = top_d
  )
}


if (!file.exists(bootstrap_file)) {
  stop(
    "No encuentro el bootstrap:\n",
    bootstrap_file
  )
}

bootstrap_colnames <- names(
  read_csv(
    bootstrap_file,
    n_max = 0,
    show_col_types = FALSE
  )
)

required_bootstrap_cols <- c(
  "bootstrap_id",
  "draw_id",
  "integrated_row_id",
  "comparison_region",
  "subsample",
  "dataset_source"
)

missing_bootstrap_cols <- setdiff(
  required_bootstrap_cols,
  bootstrap_colnames
)

if (length(missing_bootstrap_cols) > 0) {
  stop(
    "Faltan columnas en el bootstrap: ",
    paste(
      missing_bootstrap_cols,
      collapse = ", "
    )
  )
}

bootstrap_index <- read_csv(
  bootstrap_file,
  col_select = all_of(
    required_bootstrap_cols
  ),
  show_col_types = FALSE
) %>%
  mutate(
    bootstrap_id = as.integer(bootstrap_id),
    draw_id = as.integer(draw_id),
    integrated_row_id = as.character(integrated_row_id),
    comparison_region = as.character(comparison_region),
    subsample = as.character(subsample),
    dataset_source = as.character(dataset_source)
  )

all_boot_ids <- sort(
  unique(
    bootstrap_index$bootstrap_id
  )
)

if (length(all_boot_ids) != EXPECTED_N_BOOT) {
  stop(
    "Se esperaban ",
    EXPECTED_N_BOOT,
    " bootstraps y se han encontrado ",
    length(all_boot_ids),
    "."
  )
}

if (
  !identical(
    all_boot_ids,
    seq_len(EXPECTED_N_BOOT)
  )
) {
  stop(
    "Los bootstrap_id no son exactamente 1:",
    EXPECTED_N_BOOT,
    "."
  )
}

boot_ids <- all_boot_ids

if (is.finite(MAX_BOOTSTRAPS)) {
  
  boot_ids <- head(
    boot_ids,
    as.integer(MAX_BOOTSTRAPS)
  )
  
  bootstrap_index <- bootstrap_index %>%
    filter(
      bootstrap_id %in%
        boot_ids
    )
}


bootstrap_sizes <- map_dfr(
  ANALYSIS_SAMPLES,
  function(sample_name) {
    
    get_bootstrap_sample(
      bootstrap_index,
      sample_name
    ) %>%
      count(
        bootstrap_id,
        name = "n_draws"
      ) %>%
      mutate(
        analysis_sample = sample_name,
        .before = 1
      )
  }
)

missing_bootstrap_samples <- crossing(
  analysis_sample = ANALYSIS_SAMPLES,
  bootstrap_id = boot_ids
) %>%
  anti_join(
    bootstrap_sizes %>%
      select(
        analysis_sample,
        bootstrap_id
      ),
    by = c(
      "analysis_sample",
      "bootstrap_id"
    )
  )

if (nrow(missing_bootstrap_samples) > 0) {
  stop(
    "Alguna muestra no aparece en todos los bootstraps."
  )
}

fixed_expected_sizes <- c(
  COMPLETE = 1608L,
  EUROPE = 1000L,
  LATAM = 608L,
  WHY_LATAM = 608L
)

for (
  sample_name in names(
    fixed_expected_sizes
  )
) {
  
  current_sizes <- bootstrap_sizes %>%
    filter(
      analysis_sample == sample_name
    ) %>%
    pull(
      n_draws
    )
  
  if (
    any(
      current_sizes !=
      fixed_expected_sizes[[sample_name]]
    )
  ) {
    stop(
      "Tamaño inesperado en ",
      sample_name,
      ". Esperaba ",
      fixed_expected_sizes[[sample_name]],
      " draws por bootstrap."
    )
  }
}

bootstrap_size_summary <- bootstrap_sizes %>%
  group_by(
    analysis_sample
  ) %>%
  summarise(
    mean_n = mean(n_draws),
    sd_n = sd(n_draws),
    min_n = min(n_draws),
    max_n = max(n_draws),
    n_bootstraps = n_distinct(bootstrap_id),
    .groups = "drop"
  )


metrics_list <- list()
loadings_list <- list()
importance_list <- list()
correlations_list <- list()
top_d_list <- list()

run_counter <- 0L


for (
  sample_index in seq_along(
    ANALYSIS_SAMPLES
  )
) {
  
  analysis_sample <- ANALYSIS_SAMPLES[
    sample_index
  ]
  
  cat(
    "\nMUESTRA: ",
    analysis_sample,
    "\n",
    sep = ""
  )
  
  bootstrap_sample <- get_bootstrap_sample(
    bootstrap_index,
    analysis_sample
  )
  
  boot_split <- split(
    bootstrap_sample,
    bootstrap_sample$bootstrap_id
  )
  
  for (
    matrix_index in seq_along(
      MATRICES_TO_RUN
    )
  ) {
    
    matrix_name <- MATRICES_TO_RUN[
      matrix_index
    ]
    
    cat(
      "\nMatriz: ",
      matrix_name,
      "\n",
      sep = ""
    )
    
    matrix_obj <- read_matrix_file(
      analysis_sample,
      matrix_name
    )
    
    for (
      b_index in seq_along(
        boot_ids
      )
    ) {
      
      bootstrap_id <- boot_ids[
        b_index
      ]
      
      if (
        b_index == 1 ||
        b_index %% 20 == 0 ||
        b_index == length(boot_ids)
      ) {
        cat(
          "  Bootstrap ",
          b_index,
          " / ",
          length(boot_ids),
          "\n",
          sep = ""
        )
      }
      
      boot_b <- boot_split[[
        as.character(
          bootstrap_id
        )
      ]]
      
      prepared <- prepare_bootstrap_matrix(
        boot_b = boot_b,
        matrix_obj = matrix_obj
      )
      
      for (
        n_factors in N_FACTORS_GRID
      ) {
        
        run_counter <- run_counter + 1L
        
        res <- run_one_efa(
          analysis_sample_current =
            analysis_sample,
          analysis_sample_index =
            sample_index,
          matrix_name_current =
            matrix_name,
          matrix_index_current =
            matrix_index,
          bootstrap_id_current =
            bootstrap_id,
          n_factors_current =
            n_factors,
          prepared =
            prepared
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
    
    rm(matrix_obj)
    invisible(gc())
  }
}


efa_metrics_by_run <- bind_rows(
  metrics_list
)

efa_loadings_long <- bind_rows(
  loadings_list
)

efa_determinant_importance_by_run <- bind_rows(
  importance_list
)

efa_factor_correlations_by_run <- bind_rows(
  correlations_list
)

efa_top_d_by_run <- bind_rows(
  top_d_list
)


efa_metrics_summary <- efa_metrics_by_run %>%
  group_by(
    analysis_sample,
    matrix_name,
    n_factors
  ) %>%
  summarise(
    n_runs = n(),
    
    n_ok = sum(
      status %in% c(
        "ok",
        "ok_with_warning"
      )
    ),
    
    n_warning = sum(
      warning_flag %in% TRUE,
      na.rm = TRUE
    ),
    
    n_error = sum(
      !status %in% c(
        "ok",
        "ok_with_warning"
      )
    ),
    
    mean_n_rows_used =
      mean_or_na(n_rows_used),
    
    mean_n_unique_participants =
      mean_or_na(n_unique_participants),
    
    mean_RMSR =
      mean_or_na(RMSR),
    
    sd_RMSR =
      sd_or_na(RMSR),
    
    mean_TLI =
      mean_or_na(TLI),
    
    sd_TLI =
      sd_or_na(TLI),
    
    mean_RMSEA =
      mean_or_na(RMSEA),
    
    sd_RMSEA =
      sd_or_na(RMSEA),
    
    mean_BIC =
      mean_or_na(BIC),
    
    sd_BIC =
      sd_or_na(BIC),
    
    mean_abs_factor_correlation =
      mean_or_na(
        mean_abs_factor_correlation
      ),
    
    sd_abs_factor_correlation =
      sd_or_na(
        mean_abs_factor_correlation
      ),
    
    mean_factor_distance =
      mean_or_na(
        mean_factor_distance
      ),
    
    sd_factor_distance =
      sd_or_na(
        mean_factor_distance
      ),
    
    .groups = "drop"
  ) %>%
  arrange(
    analysis_sample,
    matrix_name,
    n_factors
  )


efa_warnings <- efa_metrics_by_run %>%
  filter(
    warning_flag %in% TRUE |
      !status %in% c(
        "ok",
        "ok_with_warning"
      )
  ) %>%
  select(
    analysis_sample,
    matrix_name,
    bootstrap_id,
    n_factors,
    status,
    warning_flag,
    warning_message,
    error_message
  )


efa_determinant_importance_summary <-
  efa_determinant_importance_by_run %>%
  group_by(
    analysis_sample,
    matrix_name,
    n_factors,
    determinant
  ) %>%
  summarise(
    mean_importance =
      mean_or_na(
        importance
      ),
    
    sd_importance =
      sd_or_na(
        importance
      ),
    
    median_importance =
      median(
        importance,
        na.rm = TRUE
      ),
    
    mean_rank =
      mean_or_na(
        importance_rank
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
    analysis_sample,
    matrix_name,
    n_factors,
    desc(mean_importance)
  )


# Número real de EFA válidos para cada muestra × matriz × nº factores.
# Se utiliza como denominador al calcular la estabilidad de los Top-D.
valid_efa_runs <- efa_metrics_by_run %>%
  filter(
    status %in% c(
      "ok",
      "ok_with_warning"
    )
  ) %>%
  count(
    analysis_sample,
    matrix_name,
    n_factors,
    name = "n_valid_bootstraps"
  )


efa_top_d_frequency <- efa_top_d_by_run %>%
  count(
    analysis_sample,
    matrix_name,
    n_factors,
    d_det,
    determinant,
    name = "n_selected"
  ) %>%
  left_join(
    valid_efa_runs,
    by = c(
      "analysis_sample",
      "matrix_name",
      "n_factors"
    )
  ) %>%
  mutate(
    prop_bootstraps_selected =
      n_selected /
      n_valid_bootstraps
  ) %>%
  arrange(
    analysis_sample,
    matrix_name,
    n_factors,
    d_det,
    desc(
      prop_bootstraps_selected
    )
  )


efa_run_status_summary <- efa_metrics_by_run %>%
  count(
    analysis_sample,
    matrix_name,
    n_factors,
    status,
    name = "n_runs"
  ) %>%
  arrange(
    analysis_sample,
    matrix_name,
    n_factors,
    status
  )


write_csv(
  efa_metrics_by_run,
  file.path(
    out_dir,
    "01_efa_metrics_by_run.csv"
  )
)

write_csv(
  efa_metrics_summary,
  file.path(
    out_dir,
    "02_efa_metrics_summary.csv"
  )
)

write_csv(
  efa_loadings_long,
  file.path(
    out_dir,
    "03_efa_loadings_long.csv.gz"
  )
)

write_csv(
  efa_determinant_importance_by_run,
  file.path(
    out_dir,
    "04_efa_determinant_importance_by_run.csv.gz"
  )
)

write_csv(
  efa_determinant_importance_summary,
  file.path(
    out_dir,
    "05_efa_determinant_importance_summary.csv"
  )
)

write_csv(
  efa_factor_correlations_by_run,
  file.path(
    out_dir,
    "06_efa_factor_correlations_by_run.csv.gz"
  )
)

write_csv(
  efa_top_d_by_run,
  file.path(
    out_dir,
    "07_efa_top_d_by_run.csv.gz"
  )
)

write_csv(
  efa_top_d_frequency,
  file.path(
    out_dir,
    "08_efa_top_d_frequency.csv"
  )
)

write_csv(
  efa_warnings,
  file.path(
    out_dir,
    "09_efa_warnings.csv"
  )
)

write_csv(
  efa_run_status_summary,
  file.path(
    out_dir,
    "10_efa_run_status_summary.csv"
  )
)

write_csv(
  bootstrap_size_summary,
  file.path(
    out_dir,
    "11_bootstrap_sample_sizes_summary.csv"
  )
)


parameters <- tibble(
  parameter = c(
    "bootstrap_file",
    "matrix_dir",
    "analysis_samples",
    "matrices_to_run",
    "n_factors_grid",
    "d_det_grid",
    "fa_method",
    "fa_rotation",
    "expected_n_bootstraps",
    "max_bootstraps",
    "output_dir"
  ),
  
  value = c(
    bootstrap_file,
    matrix_dir,
    
    paste(
      ANALYSIS_SAMPLES,
      collapse = ", "
    ),
    
    paste(
      MATRICES_TO_RUN,
      collapse = ", "
    ),
    
    paste(
      N_FACTORS_GRID,
      collapse = ", "
    ),
    
    paste(
      D_DET_GRID,
      collapse = ", "
    ),
    
    FA_METHOD,
    FA_ROTATION,
    
    as.character(
      EXPECTED_N_BOOT
    ),
    
    as.character(
      MAX_BOOTSTRAPS
    ),
    
    out_dir
  )
)

write_csv(
  parameters,
  file.path(
    out_dir,
    "12_efa_parameters.csv"
  )
)


for (
  sample_name in ANALYSIS_SAMPLES
) {
  
  sample_dir <- file.path(
    out_dir,
    sample_name
  )
  
  dir.create(
    sample_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  write_csv(
    efa_metrics_summary %>%
      filter(
        analysis_sample ==
          sample_name
      ),
    file.path(
      sample_dir,
      "efa_metrics_summary.csv"
    )
  )
  
  write_csv(
    efa_determinant_importance_summary %>%
      filter(
        analysis_sample ==
          sample_name
      ),
    file.path(
      sample_dir,
      "efa_determinant_importance_summary.csv"
    )
  )
  
  write_csv(
    efa_top_d_frequency %>%
      filter(
        analysis_sample ==
          sample_name
      ),
    file.path(
      sample_dir,
      "efa_top_d_frequency.csv"
    )
  )
  
  write_csv(
    efa_run_status_summary %>%
      filter(
        analysis_sample ==
          sample_name
      ),
    file.path(
      sample_dir,
      "efa_run_status_summary.csv"
    )
  )
}


cat(
  "\n============================================================\n"
)

cat(
  "07. EFA SOBRE BOOTSTRAPS COMPLETADO\n"
)

cat(
  "============================================================\n"
)

cat(
  "\nBootstrap utilizado:\n",
  bootstrap_file,
  "\n",
  sep = ""
)

cat(
  "\nNúmero de bootstraps utilizados: ",
  length(boot_ids),
  "\n",
  sep = ""
)

cat(
  "\nMuestras analizadas:\n"
)

print(
  ANALYSIS_SAMPLES
)

cat(
  "\nMatrices analizadas:\n"
)

print(
  MATRICES_TO_RUN
)

cat(
  "\nNúmero de factores explorados:\n"
)

print(
  N_FACTORS_GRID
)

cat(
  "\nTamaños de las muestras bootstrap:\n\n"
)

print(
  bootstrap_size_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nEstado de las ejecuciones:\n\n"
)

print(
  efa_run_status_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nResumen de métricas EFA:\n\n"
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