# SCRIPT 17 - CLMM 5 ARQUETIPOS x 9 DIMENSIONES

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(tibble)
library(ordinal)
library(broom.mixed)

set.seed(123)


# 1. CARPETAS
base_output_dir <- "initial_descriptive_analysis/output/model_5arq_9dim"

csv_dir <- file.path(base_output_dir, "csv")
bootstrap_dir <- file.path(base_output_dir, "bootstrap")
models_dir <- file.path(base_output_dir, "models_clmm")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(bootstrap_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)

# 2. PARÁMETROS
dimensions <- c(
  "FINANCIAL",
  "SECURITY",
  "COMPETENCE",
  "AUTONOMY",
  "PHYSIOLOGICAL",
  "RELATEDNESS",
  "STIMULATION",
  "POPULARITY",
  "MEANING"
)

stage_levels <- c(
  "Knowledge",
  "Considering",
  "Done"
)

# Si TRUE, añade tecnología como control.
# De momento lo dejo en FALSE para evitar meter demasiados parámetros.
INCLUDE_TECHNOLOGY <- FALSE

# Modelos que se van a probar:
# M0: solo arquetipo
# M1: arquetipo + dimensiones
# M2: arquetipo * dimensiones
MODELS_TO_RUN <- c("M0", "M1", "M2")


# 3. CARGAR DATOS
df_full <- read_csv(
  file.path(csv_dir, "synthetic_5arq_9dim.csv"),
  show_col_types = FALSE
)

df_bootstrap <- read_csv(
  file.path(bootstrap_dir, "bootstrap_5arq_9dim_all.csv"),
  show_col_types = FALSE
)

cat("Datos cargados correctamente\n")
cat("Filas dataset completo:", nrow(df_full), "\n")
cat("Filas dataset bootstrap:", nrow(df_bootstrap), "\n")


# 4. VALIDACIONES
required_cols <- c(
  "participant_id",
  "macro_archetype_5",
  "adoption_stage",
  dimensions
)

missing_full <- setdiff(required_cols, names(df_full))
missing_boot <- setdiff(
  c("n_participants", "boot_id", required_cols),
  names(df_bootstrap)
)

if (length(missing_full) > 0) {
  stop("Faltan columnas en df_full: ", paste(missing_full, collapse = ", "))
}

if (length(missing_boot) > 0) {
  stop("Faltan columnas en df_bootstrap: ", paste(missing_boot, collapse = ", "))
}

# 5. FUNCIÓN DE PREPARACIÓN
prepare_clmm_data <- function(data_i) {
  
  data_i %>%
    mutate(
      adoption_stage = ordered(
        adoption_stage,
        levels = stage_levels
      ),
      macro_archetype_5 = factor(macro_archetype_5),
      participant_id = factor(participant_id),
      technology = if ("technology" %in% names(.)) factor(technology) else NA
    ) %>%
    mutate(
      across(
        all_of(dimensions),
        ~ as.numeric(scale(.x)),
        .names = "z_{.col}"
      )
    ) %>%
    drop_na(
      adoption_stage,
      macro_archetype_5,
      participant_id,
      all_of(paste0("z_", dimensions))
    )
}

# 6. FORMULAS CLMM
build_formula <- function(model_type) {
  
  z_dims <- paste0("z_", dimensions)
  
  tech_part <- if (INCLUDE_TECHNOLOGY) {
    " + technology"
  } else {
    ""
  }
  
  if (model_type == "M0") {
    
    formula_txt <- paste0(
      "adoption_stage ~ macro_archetype_5",
      tech_part,
      " + (1 | participant_id)"
    )
    
  } else if (model_type == "M1") {
    
    formula_txt <- paste0(
      "adoption_stage ~ macro_archetype_5 + ",
      paste(z_dims, collapse = " + "),
      tech_part,
      " + (1 | participant_id)"
    )
    
  } else if (model_type == "M2") {
    
    formula_txt <- paste0(
      "adoption_stage ~ macro_archetype_5 * (",
      paste(z_dims, collapse = " + "),
      ")",
      tech_part,
      " + (1 | participant_id)"
    )
    
  } else {
    stop("Modelo no reconocido: ", model_type)
  }
  
  as.formula(formula_txt)
}

# 7. FUNCIÓN SEGURA PARA AJUSTAR CLMM
fit_clmm_safe <- function(data_i, model_type, sample_label, n_participants, boot_id) {
  
  formula_i <- build_formula(model_type)
  formula_txt <- paste(deparse(formula_i), collapse = " ")
  
  warning_messages <- character()
  
  cat("\nAjustando", model_type, "-", sample_label, "\n")
  cat("Fórmula:", deparse(formula_i), "\n")
  
  model_i <- tryCatch(
    withCallingHandlers(
      clmm(
        formula_i,
        data = data_i,
        link = "logit",
        Hess = TRUE,
        nAGQ = 1,
        control = clmm.control(
          maxIter = 500,
          gradTol = 1e-5
        )
      ),
      warning = function(w) {
        warning_messages <<- c(warning_messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      attr(e, "warning_messages") <- warning_messages
      return(e)
    }
  )
  
  if (inherits(model_i, "error")) {
    
    return(list(
      model = NULL,
      summary = tibble(
        sample_label = sample_label,
        n_participants = n_participants,
        boot_id = boot_id,
        model_type = model_type,
        formula = formula_txt,
        status = "error",
        error_message = conditionMessage(model_i),
        warnings = paste(unique(attr(model_i, "warning_messages")), collapse = " | "),
        n_rows = nrow(data_i),
        n_unique_participants = n_distinct(data_i$participant_id),
        AIC = NA_real_,
        BIC = NA_real_,
        logLik = NA_real_,
        n_terms_with_p = NA_integer_,
        n_significant_terms = NA_integer_
      ),
      terms = tibble()
    ))
  }
  
  # Guardar modelo
  model_file <- file.path(
    models_dir,
    paste0(
      "clmm_",
      model_type,
      "_",
      sample_label,
      "_n",
      n_participants,
      "_boot",
      boot_id,
      ".rds"
    )
  )
  
  saveRDS(model_i, model_file)
  
  # Extraer términos
  tidy_i <- tryCatch(
    broom.mixed::tidy(model_i, effects = "fixed"),
    error = function(e) NULL
  )
  
  if (is.null(tidy_i)) {
    
    terms_i <- tibble()
    n_terms_with_p <- NA_integer_
    n_significant_terms <- NA_integer_
    
  } else {
    
    terms_i <- tidy_i %>%
      mutate(
        sample_label = sample_label,
        n_participants = n_participants,
        boot_id = boot_id,
        model_type = model_type,
        formula = formula_txt,
        is_dimension_term = str_detect(term, "^z_"),
        is_interaction_term = str_detect(term, ":"),
        is_archetype_term = str_detect(term, "^macro_archetype_5"),
        is_significant = if_else(
          !is.na(p.value) & p.value < 0.05,
          TRUE,
          FALSE,
          missing = FALSE
        )
      )
    
    n_terms_with_p <- sum(!is.na(terms_i$p.value))
    n_significant_terms <- sum(terms_i$is_significant, na.rm = TRUE)
  }
  
  summary_i <- tibble(
    sample_label = sample_label,
    n_participants = n_participants,
    boot_id = boot_id,
    model_type = model_type,
    formula = formula_txt,
    status = "ok",
    error_message = NA_character_,
    warnings = paste(unique(warning_messages), collapse = " | "),
    n_rows = nrow(data_i),
    n_unique_participants = n_distinct(data_i$participant_id),
    AIC = AIC(model_i),
    BIC = BIC(model_i),
    logLik = as.numeric(logLik(model_i)),
    n_terms_with_p = n_terms_with_p,
    n_significant_terms = n_significant_terms
  )
  
  list(
    model = model_i,
    summary = summary_i,
    terms = terms_i
  )
}
# 8. AJUSTE EN DATASET COMPLETO
df_full_clmm <- prepare_clmm_data(df_full)

full_results <- map(
  MODELS_TO_RUN,
  function(model_type_i) {
    fit_clmm_safe(
      data_i = df_full_clmm,
      model_type = model_type_i,
      sample_label = "full",
      n_participants = n_distinct(df_full_clmm$participant_id),
      boot_id = 0
    )
  }
)

full_model_summary <- map_dfr(full_results, "summary")
full_model_terms <- map_dfr(full_results, "terms")

write_csv(
  full_model_summary,
  file.path(csv_dir, "clmm_full_model_summary.csv")
)

write_csv(
  full_model_terms,
  file.path(csv_dir, "clmm_full_model_terms.csv")
)

cat("\nResumen modelos dataset completo:\n")
print(full_model_summary)


# 9. AJUSTE EN BOOTSTRAPS / SUBMUESTRAS
bootstrap_keys <- df_bootstrap %>%
  distinct(n_participants, boot_id) %>%
  arrange(n_participants, boot_id)

fit_one_bootstrap <- function(n_i, b_i) {
  
  data_i <- df_bootstrap %>%
    filter(
      n_participants == n_i,
      boot_id == b_i
    )
  
  data_i <- prepare_clmm_data(data_i)
  
  map(
    MODELS_TO_RUN,
    function(model_type_i) {
      fit_clmm_safe(
        data_i = data_i,
        model_type = model_type_i,
        sample_label = "bootstrap",
        n_participants = n_i,
        boot_id = b_i
      )
    }
  )
}

bootstrap_results_nested <- pmap(
  list(
    bootstrap_keys$n_participants,
    bootstrap_keys$boot_id
  ),
  fit_one_bootstrap
)

bootstrap_results_flat <- flatten(bootstrap_results_nested)

bootstrap_model_summary <- map_dfr(bootstrap_results_flat, "summary")
bootstrap_model_terms <- map_dfr(bootstrap_results_flat, "terms")

write_csv(
  bootstrap_model_summary,
  file.path(csv_dir, "clmm_bootstrap_model_summary.csv")
)

write_csv(
  bootstrap_model_terms,
  file.path(csv_dir, "clmm_bootstrap_model_terms.csv")
)

cat("\nResumen modelos bootstrap:\n")
print(bootstrap_model_summary, n = Inf)


# 10. COMPARACIÓN DE MODELOS
model_comparison_full <- full_model_summary %>%
  select(
    sample_label,
    n_participants,
    boot_id,
    model_type,
    status,
    AIC,
    BIC,
    logLik,
    warnings
  ) %>%
  arrange(AIC)

write_csv(
  model_comparison_full,
  file.path(csv_dir, "clmm_full_model_comparison.csv")
)

model_comparison_bootstrap <- bootstrap_model_summary %>%
  group_by(n_participants, model_type) %>%
  summarise(
    n_boot = n(),
    n_ok = sum(status == "ok"),
    convergence_rate = mean(status == "ok"),
    mean_AIC = mean(AIC, na.rm = TRUE),
    sd_AIC = sd(AIC, na.rm = TRUE),
    mean_BIC = mean(BIC, na.rm = TRUE),
    sd_BIC = sd(BIC, na.rm = TRUE),
    mean_logLik = mean(logLik, na.rm = TRUE),
    mean_terms_with_p = mean(n_terms_with_p, na.rm = TRUE),
    mean_significant_terms = mean(n_significant_terms, na.rm = TRUE),
    n_with_warnings = sum(!is.na(warnings) & warnings != ""),
    .groups = "drop"
  ) %>%
  arrange(n_participants, mean_AIC)

write_csv(
  model_comparison_bootstrap,
  file.path(csv_dir, "clmm_bootstrap_model_comparison.csv")
)

cat("\nComparación media por bootstrap:\n")
print(model_comparison_bootstrap, n = Inf)

# 11. RESUMEN DE TÉRMINOS SIGNIFICATIVOS
significant_terms_summary <- bootstrap_model_terms %>%
  filter(!is.na(p.value)) %>%
  group_by(
    n_participants,
    model_type,
    term
  ) %>%
  summarise(
    n_boot = n(),
    n_significant = sum(p.value < 0.05, na.rm = TRUE),
    significance_rate = n_significant / n_boot,
    mean_estimate = mean(estimate, na.rm = TRUE),
    sd_estimate = sd(estimate, na.rm = TRUE),
    mean_p_value = mean(p.value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    n_participants,
    model_type,
    desc(significance_rate),
    mean_p_value
  )

write_csv(
  significant_terms_summary,
  file.path(csv_dir, "clmm_bootstrap_significant_terms_summary.csv")
)

cat("\nTérminos más estables/significativos:\n")
print(
  significant_terms_summary %>%
    filter(significance_rate > 0.5) %>%
    arrange(desc(significance_rate)),
  n = 50
)


# 12. MENSAJE FINAL
cat("Archivos principales creados:\n")
cat("- clmm_full_model_summary.csv\n")
cat("- clmm_full_model_terms.csv\n")
cat("- clmm_full_model_comparison.csv\n")
cat("- clmm_bootstrap_model_summary.csv\n")
cat("- clmm_bootstrap_model_terms.csv\n")
cat("- clmm_bootstrap_model_comparison.csv\n")
cat("- clmm_bootstrap_significant_terms_summary.csv\n")