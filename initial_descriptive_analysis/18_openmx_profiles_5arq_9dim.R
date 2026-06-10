# ==============================================================================
# SCRIPT 18 - OPENMX 5 ARQUETIPOS x 3 ETAPAS x 9 DIMENSIONES
# Modelo de perfiles conocidos: macro_archetype_5 x adoption_stage
# ==============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(OpenMx)

set.seed(123)

# ==============================================================================
# 1. CARPETAS
# ==============================================================================

base_output_dir <- "initial_descriptive_analysis/output/model_5arq_9dim"

csv_dir <- file.path(base_output_dir, "csv")
bootstrap_dir <- file.path(base_output_dir, "bootstrap")
models_dir <- file.path(base_output_dir, "models_openmx")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(bootstrap_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 2. DICCIONARIOS EN CASTELLANO
# ==============================================================================

dimension_dictionary <- tibble(
  dimension = c(
    "FINANCIAL",
    "SECURITY",
    "COMPETENCE",
    "AUTONOMY",
    "PHYSIOLOGICAL",
    "RELATEDNESS",
    "STIMULATION",
    "POPULARITY",
    "MEANING"
  ),
  dimension_label = c(
    "Financiera",
    "Seguridad",
    "Competencia",
    "Autonomía",
    "Bienestar físico",
    "Relaciones sociales",
    "Estimulación",
    "Popularidad",
    "Significado"
  )
)

stage_dictionary <- tibble(
  adoption_stage = c(
    "Knowledge",
    "Considering",
    "Done"
  ),
  adoption_stage_label = c(
    "Conocimiento",
    "Consideración",
    "Implementada"
  )
)

archetype_dictionary <- tibble(
  macro_archetype_5 = c(
    "G1_Activist_Stubborn_Sentient",
    "G2_EarlyAdopter_Influencer",
    "G3_Fearful_Careful",
    "G4_HomoEconomicus",
    "G5_Uninterested"
  ),
  archetype_label = c(
    "G1 - Activista / resistente / consciente",
    "G2 - Adoptante temprano / influencer",
    "G3 - Temeroso / cuidadoso",
    "G4 - Homo economicus",
    "G5 - Desinteresado"
  )
)

write_csv(
  dimension_dictionary,
  file.path(csv_dir, "dimension_dictionary_castellano.csv")
)

write_csv(
  stage_dictionary,
  file.path(csv_dir, "stage_dictionary_castellano.csv")
)

write_csv(
  archetype_dictionary,
  file.path(csv_dir, "archetype_dictionary_castellano.csv")
)

# ==============================================================================
# 3. PARÁMETROS
# ==============================================================================

dimensions <- dimension_dictionary$dimension
macro_archetypes <- archetype_dictionary$macro_archetype_5
stage_levels <- stage_dictionary$adoption_stage

# Para probar primero, dejar FALSE.
# Cuando funcione el dataset completo, cambiar a TRUE.
RUN_BOOTSTRAPS <- FALSE

# Mínimo de filas por perfil arquetipo x etapa.
MIN_CELL_N <- 20

# Número de intentos extra para evitar mínimos locales.
MX_EXTRA_TRIES <- 10

# Optimizador OpenMx
mxOption(NULL, "Default optimizer", "SLSQP")

# ==============================================================================
# 4. CARGAR DATOS
# ==============================================================================

df_full <- read_csv(
  file.path(csv_dir, "synthetic_5arq_9dim.csv"),
  show_col_types = FALSE
)

df_bootstrap <- read_csv(
  file.path(bootstrap_dir, "bootstrap_5arq_9dim_all.csv"),
  show_col_types = FALSE
)

cat("Datos cargados correctamente\n")
cat("Filas dataset completo original:", nrow(df_full), "\n")
cat("Participantes dataset completo:", n_distinct(df_full$participant_id), "\n")
cat("Filas dataset bootstrap original:", nrow(df_bootstrap), "\n")

# ==============================================================================
# 5. VALIDACIONES
# ==============================================================================

required_full_cols <- c(
  "participant_id",
  "macro_archetype_5",
  "adoption_stage",
  dimensions
)

required_boot_cols <- c(
  "n_participants",
  "boot_id",
  required_full_cols
)

missing_full <- setdiff(required_full_cols, names(df_full))

if (length(missing_full) > 0) {
  stop(
    "Faltan columnas en synthetic_5arq_9dim.csv: ",
    paste(missing_full, collapse = ", ")
  )
}

missing_boot <- setdiff(required_boot_cols, names(df_bootstrap))

if (length(missing_boot) > 0) {
  stop(
    "Faltan columnas en bootstrap_5arq_9dim_all.csv: ",
    paste(missing_boot, collapse = ", ")
  )
}

# ==============================================================================
# 6. LIMPIAR DATASETS: SOLO COLUMNAS NECESARIAS PARA OPENMX
# ==============================================================================

df_full <- df_full %>%
  select(
    participant_id,
    macro_archetype_5,
    adoption_stage,
    all_of(dimensions)
  ) %>%
  mutate(
    across(all_of(dimensions), as.numeric)
  )

df_bootstrap <- df_bootstrap %>%
  select(
    n_participants,
    boot_id,
    participant_id,
    macro_archetype_5,
    adoption_stage,
    all_of(dimensions)
  ) %>%
  mutate(
    across(all_of(dimensions), as.numeric)
  )

cat("\nColumnas usadas en OpenMx dataset completo:\n")
print(names(df_full))

cat("\nColumnas usadas en OpenMx bootstrap:\n")
print(names(df_bootstrap))

# ==============================================================================
# 7. MAPA DE PERFILES
# ==============================================================================

profile_map <- expand_grid(
  macro_archetype_5 = macro_archetypes,
  adoption_stage = stage_levels
) %>%
  left_join(
    archetype_dictionary,
    by = "macro_archetype_5"
  ) %>%
  left_join(
    stage_dictionary,
    by = "adoption_stage"
  ) %>%
  mutate(
    profile_id = paste0("P", str_pad(row_number(), width = 2, pad = "0")),
    profile_cell = paste(macro_archetype_5, adoption_stage, sep = "__"),
    profile_label = paste(archetype_label, adoption_stage_label, sep = " | "),
    openmx_model_name = paste0("mx_", profile_id)
  )

write_csv(
  profile_map,
  file.path(csv_dir, "openmx_profile_map_5arq_3stage.csv")
)

cat("\nMapa de perfiles OpenMx:\n")
print(profile_map, n = Inf)

# ==============================================================================
# 8. PREPARAR DATOS PARA OPENMX
# ==============================================================================

prepare_openmx_data <- function(data_i) {
  
  data_i %>%
    mutate(
      macro_archetype_5 = as.character(macro_archetype_5),
      adoption_stage = as.character(adoption_stage),
      profile_cell = paste(macro_archetype_5, adoption_stage, sep = "__")
    ) %>%
    left_join(
      profile_map,
      by = c("macro_archetype_5", "adoption_stage", "profile_cell")
    ) %>%
    drop_na(all_of(dimensions)) %>%
    filter(!is.na(profile_id))
}

# ==============================================================================
# 9. CREAR UN SUBMODELO OPENMX PARA UN PERFIL
# ==============================================================================

build_profile_submodel <- function(data_profile, profile_model_name) {
  
  # OpenMx solo debe recibir las 9 dimensiones observadas.
  # Para evitar errores con nombres heredados o caracteres raros,
  # internamente las variables se renombran como D1...D9.
  
  mx_vars <- paste0("D", seq_along(dimensions))
  
  dat_i <- data_profile %>%
    select(all_of(dimensions)) %>%
    mutate(
      across(everything(), as.numeric)
    ) %>%
    as.data.frame()
  
  names(dat_i) <- mx_vars
  rownames(dat_i) <- NULL
  
  if (ncol(dat_i) != length(dimensions)) {
    stop("El dataset del perfil no tiene las 9 dimensiones esperadas.")
  }
  
  if (any(!is.finite(as.matrix(dat_i)))) {
    stop("Hay valores no finitos en las dimensiones del perfil.")
  }
  
  start_means <- colMeans(dat_i, na.rm = TRUE)
  start_vars <- apply(dat_i, 2, var, na.rm = TRUE)
  start_vars <- pmax(start_vars, 1)
  
  mxModel(
    name = profile_model_name,
    
    mxData(
      observed = dat_i,
      type = "raw"
    ),
    
    mxMatrix(
      type = "Full",
      nrow = 1,
      ncol = length(mx_vars),
      free = TRUE,
      values = start_means,
      labels = paste0("mean_", profile_model_name, "_", dimensions),
      dimnames = list(NULL, mx_vars),
      name = "Means"
    ),
    
    mxMatrix(
      type = "Diag",
      nrow = length(mx_vars),
      ncol = length(mx_vars),
      free = TRUE,
      values = start_vars,
      labels = paste0("var_", profile_model_name, "_", dimensions),
      lbound = 1e-4,
      dimnames = list(mx_vars, mx_vars),
      name = "Cov"
    ),
    
    mxExpectationNormal(
      covariance = "Cov",
      means = "Means",
      dimnames = mx_vars
    ),
    
    mxFitFunctionML()
  )
}

# ==============================================================================
# 10. AJUSTAR MODELO OPENMX PARA UN DATASET
# ==============================================================================

fit_openmx_profiles <- function(data_i, sample_label, n_participants, boot_id) {
  
  cat(
    "\nAjustando OpenMx:",
    sample_label,
    "| n =", n_participants,
    "| boot =", boot_id,
    "\n"
  )
  
  data_mx <- prepare_openmx_data(data_i)
  
  profile_counts <- data_mx %>%
    count(
      profile_id,
      openmx_model_name,
      macro_archetype_5,
      archetype_label,
      adoption_stage,
      adoption_stage_label,
      profile_cell,
      profile_label,
      name = "n_rows"
    ) %>%
    mutate(
      enough_data = n_rows >= MIN_CELL_N,
      enough_data_label = if_else(
        enough_data,
        "Muestra suficiente",
        "Muestra insuficiente"
      )
    )
  
  valid_profiles <- profile_counts %>%
    filter(enough_data)
  
  insufficient_profiles <- profile_counts %>%
    filter(!enough_data)
  
  if (nrow(insufficient_profiles) > 0) {
    cat("Perfiles excluidos por poca muestra:\n")
    print(insufficient_profiles)
  }
  
  if (nrow(valid_profiles) < 2) {
    
    summary_i <- tibble(
      sample_label = sample_label,
      sample_label_es = case_when(
        sample_label == "full" ~ "muestra completa",
        sample_label == "bootstrap" ~ "bootstrap",
        TRUE ~ sample_label
      ),
      n_participants = n_participants,
      boot_id = boot_id,
      status = "insufficient_profiles",
      status_label = "perfiles insuficientes",
      error_message = NA_character_,
      n_rows = nrow(data_mx),
      n_profiles_total = nrow(profile_counts),
      n_profiles_used = nrow(valid_profiles),
      AIC = NA_real_,
      BIC = NA_real_,
      logLik = NA_real_,
      status_code = NA_integer_
    )
    
    return(
      list(
        fit = NULL,
        summary = summary_i,
        profiles = tibble(),
        counts = profile_counts
      )
    )
  }
  
  submodels <- map(
    valid_profiles$openmx_model_name,
    function(model_name_i) {
      
      profile_id_i <- valid_profiles %>%
        filter(openmx_model_name == model_name_i) %>%
        pull(profile_id)
      
      data_profile <- data_mx %>%
        filter(profile_id == profile_id_i)
      
      build_profile_submodel(
        data_profile = data_profile,
        profile_model_name = model_name_i
      )
    }
  )
  
  names(submodels) <- valid_profiles$openmx_model_name
  
  multigroup_fitfunctions <- paste0(
    valid_profiles$openmx_model_name,
    ".fitfunction"
  )
  
  model_name <- paste0(
    "OpenMx_profiles_",
    sample_label,
    "_n",
    n_participants,
    "_boot",
    boot_id
  )
  
  mx_model <- mxModel(
    name = model_name,
    submodels,
    mxFitFunctionMultigroup(
      groups = multigroup_fitfunctions
    )
  )
  
  fit_i <- tryCatch(
    mxTryHard(
      mx_model,
      extraTries = MX_EXTRA_TRIES,
      OKstatuscodes = c(0, 1)
    ),
    error = function(e) e
  )
  
  if (inherits(fit_i, "error")) {
    
    summary_i <- tibble(
      sample_label = sample_label,
      sample_label_es = case_when(
        sample_label == "full" ~ "muestra completa",
        sample_label == "bootstrap" ~ "bootstrap",
        TRUE ~ sample_label
      ),
      n_participants = n_participants,
      boot_id = boot_id,
      status = "error",
      status_label = "error",
      error_message = conditionMessage(fit_i),
      n_rows = nrow(data_mx),
      n_profiles_total = nrow(profile_counts),
      n_profiles_used = nrow(valid_profiles),
      AIC = NA_real_,
      BIC = NA_real_,
      logLik = NA_real_,
      status_code = NA_integer_
    )
    
    return(
      list(
        fit = NULL,
        summary = summary_i,
        profiles = tibble(),
        counts = profile_counts
      )
    )
  }
  
  model_file <- file.path(
    models_dir,
    paste0(
      "openmx_profiles_",
      sample_label,
      "_n",
      n_participants,
      "_boot",
      boot_id,
      ".rds"
    )
  )
  
  saveRDS(fit_i, model_file)
  
  status_code <- tryCatch(
    fit_i$output$status$code,
    error = function(e) NA_integer_
  )
  
  profile_estimates <- map_dfr(
    seq_len(nrow(valid_profiles)),
    function(i) {
      
      model_name_i <- valid_profiles$openmx_model_name[i]
      
      means_i <- tryCatch(
        as.numeric(mxEvalByName(paste0(model_name_i, ".Means"), fit_i)),
        error = function(e) rep(NA_real_, length(dimensions))
      )
      
      cov_i <- tryCatch(
        mxEvalByName(paste0(model_name_i, ".Cov"), fit_i),
        error = function(e) {
          matrix(
            NA_real_,
            nrow = length(dimensions),
            ncol = length(dimensions)
          )
        }
      )
      
      vars_i <- diag(cov_i)
      
      tibble(
        sample_label = sample_label,
        sample_label_es = case_when(
          sample_label == "full" ~ "muestra completa",
          sample_label == "bootstrap" ~ "bootstrap",
          TRUE ~ sample_label
        ),
        n_participants = n_participants,
        boot_id = boot_id,
        profile_id = valid_profiles$profile_id[i],
        openmx_model_name = model_name_i,
        macro_archetype_5 = valid_profiles$macro_archetype_5[i],
        archetype_label = valid_profiles$archetype_label[i],
        adoption_stage = valid_profiles$adoption_stage[i],
        adoption_stage_label = valid_profiles$adoption_stage_label[i],
        profile_cell = valid_profiles$profile_cell[i],
        profile_label = valid_profiles$profile_label[i],
        n_rows_profile = valid_profiles$n_rows[i],
        dimension = dimensions,
        dimension_label = dimension_dictionary$dimension_label,
        mean_estimate = means_i,
        variance_estimate = vars_i,
        sd_estimate = sqrt(vars_i)
      )
    }
  )
  
  summary_i <- tibble(
    sample_label = sample_label,
    sample_label_es = case_when(
      sample_label == "full" ~ "muestra completa",
      sample_label == "bootstrap" ~ "bootstrap",
      TRUE ~ sample_label
    ),
    n_participants = n_participants,
    boot_id = boot_id,
    status = "ok",
    status_label = "ajustado correctamente",
    error_message = NA_character_,
    n_rows = nrow(data_mx),
    n_profiles_total = nrow(profile_counts),
    n_profiles_used = nrow(valid_profiles),
    AIC = tryCatch(AIC(fit_i), error = function(e) NA_real_),
    BIC = tryCatch(BIC(fit_i), error = function(e) NA_real_),
    logLik = tryCatch(as.numeric(logLik(fit_i)), error = function(e) NA_real_),
    status_code = status_code
  )
  
  list(
    fit = fit_i,
    summary = summary_i,
    profiles = profile_estimates,
    counts = profile_counts
  )
}

# ==============================================================================
# 11. AJUSTE EN DATASET COMPLETO
# ==============================================================================

full_result <- fit_openmx_profiles(
  data_i = df_full,
  sample_label = "full",
  n_participants = n_distinct(df_full$participant_id),
  boot_id = 0
)

write_csv(
  full_result$summary,
  file.path(csv_dir, "openmx_full_model_summary.csv")
)

write_csv(
  full_result$profiles,
  file.path(csv_dir, "openmx_full_profile_estimates.csv")
)

write_csv(
  full_result$counts,
  file.path(csv_dir, "openmx_full_profile_counts.csv")
)

cat("\nResumen OpenMx dataset completo:\n")
print(full_result$summary)

cat("\nEstimaciones de perfiles dataset completo:\n")
print(full_result$profiles, n = 50)

# ==============================================================================
# 12. AJUSTE EN BOOTSTRAPS / SUBMUESTRAS
# ==============================================================================

if (RUN_BOOTSTRAPS) {
  
  bootstrap_keys <- df_bootstrap %>%
    distinct(n_participants, boot_id) %>%
    arrange(n_participants, boot_id)
  
  fit_one_bootstrap_openmx <- function(n_i, b_i) {
    
    data_i <- df_bootstrap %>%
      filter(
        n_participants == n_i,
        boot_id == b_i
      )
    
    fit_openmx_profiles(
      data_i = data_i,
      sample_label = "bootstrap",
      n_participants = n_i,
      boot_id = b_i
    )
  }
  
  bootstrap_results <- pmap(
    list(
      bootstrap_keys$n_participants,
      bootstrap_keys$boot_id
    ),
    fit_one_bootstrap_openmx
  )
  
  bootstrap_summary <- map_dfr(bootstrap_results, "summary")
  bootstrap_profiles <- map_dfr(bootstrap_results, "profiles")
  bootstrap_counts <- map_dfr(
    bootstrap_results,
    "counts",
    .id = "bootstrap_result_id"
  )
  
  write_csv(
    bootstrap_summary,
    file.path(csv_dir, "openmx_bootstrap_model_summary.csv")
  )
  
  write_csv(
    bootstrap_profiles,
    file.path(csv_dir, "openmx_bootstrap_profile_estimates.csv")
  )
  
  write_csv(
    bootstrap_counts,
    file.path(csv_dir, "openmx_bootstrap_profile_counts.csv")
  )
  
  cat("\nResumen OpenMx bootstrap:\n")
  print(bootstrap_summary, n = Inf)
  
  profile_stability <- bootstrap_profiles %>%
    group_by(
      n_participants,
      macro_archetype_5,
      archetype_label,
      adoption_stage,
      adoption_stage_label,
      dimension,
      dimension_label
    ) %>%
    summarise(
      n_boot = n(),
      mean_of_means = mean(mean_estimate, na.rm = TRUE),
      sd_of_means = sd(mean_estimate, na.rm = TRUE),
      min_mean = min(mean_estimate, na.rm = TRUE),
      max_mean = max(mean_estimate, na.rm = TRUE),
      mean_sd_estimate = mean(sd_estimate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(
      n_participants,
      archetype_label,
      adoption_stage_label,
      dimension_label
    )
  
  write_csv(
    profile_stability,
    file.path(csv_dir, "openmx_bootstrap_profile_stability.csv")
  )
  
  cat("\nEstabilidad de perfiles OpenMx:\n")
  print(profile_stability, n = 80)
}

# ==============================================================================
# 13. TABLA TIPO HEATMAP PARA GRÁFICOS
# ==============================================================================

if (nrow(full_result$profiles) > 0) {
  
  openmx_heatmap_data <- full_result$profiles %>%
    mutate(
      profile_label = paste(archetype_label, adoption_stage_label, sep = " | ")
    ) %>%
    select(
      profile_label,
      macro_archetype_5,
      archetype_label,
      adoption_stage,
      adoption_stage_label,
      dimension,
      dimension_label,
      mean_estimate,
      variance_estimate,
      sd_estimate,
      n_rows_profile
    )
  
  write_csv(
    openmx_heatmap_data,
    file.path(csv_dir, "openmx_heatmap_profile_dimension_means.csv")
  )
}

# ==============================================================================
# 14. VERSIONES MÁS LEGIBLES EN CASTELLANO
# ==============================================================================

openmx_full_model_summary_castellano <- full_result$summary %>%
  select(
    muestra = sample_label_es,
    n_participantes = n_participants,
    boot_id,
    estado = status_label,
    mensaje_error = error_message,
    n_filas = n_rows,
    n_perfiles_totales = n_profiles_total,
    n_perfiles_usados = n_profiles_used,
    AIC,
    BIC,
    logLik,
    codigo_estado = status_code
  )

openmx_full_profile_estimates_castellano <- full_result$profiles %>%
  select(
    muestra = sample_label_es,
    n_participantes = n_participants,
    boot_id,
    perfil = profile_label,
    arquetipo = archetype_label,
    etapa = adoption_stage_label,
    dimension = dimension_label,
    n_filas_perfil = n_rows_profile,
    media_estimada = mean_estimate,
    varianza_estimada = variance_estimate,
    desviacion_tipica_estimada = sd_estimate
  )

openmx_full_profile_counts_castellano <- full_result$counts %>%
  select(
    perfil_id = profile_id,
    modelo_openmx = openmx_model_name,
    arquetipo = archetype_label,
    etapa = adoption_stage_label,
    perfil = profile_label,
    n_filas = n_rows,
    muestra_suficiente = enough_data_label
  )

write_csv(
  openmx_full_model_summary_castellano,
  file.path(csv_dir, "openmx_full_model_summary_castellano.csv")
)

write_csv(
  openmx_full_profile_estimates_castellano,
  file.path(csv_dir, "openmx_full_profile_estimates_castellano.csv")
)

write_csv(
  openmx_full_profile_counts_castellano,
  file.path(csv_dir, "openmx_full_profile_counts_castellano.csv")
)

if (RUN_BOOTSTRAPS) {
  
  openmx_bootstrap_model_summary_castellano <- bootstrap_summary %>%
    select(
      muestra = sample_label_es,
      n_participantes = n_participants,
      boot_id,
      estado = status_label,
      mensaje_error = error_message,
      n_filas = n_rows,
      n_perfiles_totales = n_profiles_total,
      n_perfiles_usados = n_profiles_used,
      AIC,
      BIC,
      logLik,
      codigo_estado = status_code
    )
  
  openmx_bootstrap_profile_estimates_castellano <- bootstrap_profiles %>%
    select(
      muestra = sample_label_es,
      n_participantes = n_participants,
      boot_id,
      perfil = profile_label,
      arquetipo = archetype_label,
      etapa = adoption_stage_label,
      dimension = dimension_label,
      n_filas_perfil = n_rows_profile,
      media_estimada = mean_estimate,
      varianza_estimada = variance_estimate,
      desviacion_tipica_estimada = sd_estimate
    )
  
  openmx_bootstrap_profile_counts_castellano <- bootstrap_counts %>%
    select(
      bootstrap_result_id,
      perfil_id = profile_id,
      modelo_openmx = openmx_model_name,
      arquetipo = archetype_label,
      etapa = adoption_stage_label,
      perfil = profile_label,
      n_filas = n_rows,
      muestra_suficiente = enough_data_label
    )
  
  openmx_bootstrap_profile_stability_castellano <- profile_stability %>%
    select(
      n_participantes = n_participants,
      arquetipo = archetype_label,
      etapa = adoption_stage_label,
      dimension = dimension_label,
      n_boot,
      media_de_medias = mean_of_means,
      desviacion_de_medias = sd_of_means,
      media_minima = min_mean,
      media_maxima = max_mean,
      desviacion_tipica_media = mean_sd_estimate
    )
  
  write_csv(
    openmx_bootstrap_model_summary_castellano,
    file.path(csv_dir, "openmx_bootstrap_model_summary_castellano.csv")
  )
  
  write_csv(
    openmx_bootstrap_profile_estimates_castellano,
    file.path(csv_dir, "openmx_bootstrap_profile_estimates_castellano.csv")
  )
  
  write_csv(
    openmx_bootstrap_profile_counts_castellano,
    file.path(csv_dir, "openmx_bootstrap_profile_counts_castellano.csv")
  )
  
  write_csv(
    openmx_bootstrap_profile_stability_castellano,
    file.path(csv_dir, "openmx_bootstrap_profile_stability_castellano.csv")
  )
}

# ==============================================================================
# 15. MENSAJE FINAL
# ==============================================================================

cat("Archivos principales creados:\n")
cat("- openmx_profile_map_5arq_3stage.csv\n")
cat("- openmx_full_model_summary.csv\n")
cat("- openmx_full_profile_estimates.csv\n")
cat("- openmx_full_profile_counts.csv\n")
cat("- openmx_heatmap_profile_dimension_means.csv\n")
cat("- openmx_full_model_summary_castellano.csv\n")
cat("- openmx_full_profile_estimates_castellano.csv\n")
cat("- openmx_full_profile_counts_castellano.csv\n")

if (RUN_BOOTSTRAPS) {
  cat("- openmx_bootstrap_model_summary.csv\n")
  cat("- openmx_bootstrap_profile_estimates.csv\n")
  cat("- openmx_bootstrap_profile_counts.csv\n")
  cat("- openmx_bootstrap_profile_stability.csv\n")
  cat("- openmx_bootstrap_model_summary_castellano.csv\n")
  cat("- openmx_bootstrap_profile_estimates_castellano.csv\n")
  cat("- openmx_bootstrap_profile_counts_castellano.csv\n")
  cat("- openmx_bootstrap_profile_stability_castellano.csv\n")
}