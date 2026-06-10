# ==============================================================================
# SCRIPT 17 - CLMM 5 ARQUETIPOS x 9 DIMENSIONES
# ==============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(tibble)
library(ordinal)
library(broom.mixed)

set.seed(123)


# ==============================================================================
# 1. CARPETAS
# ==============================================================================

base_output_dir <- "initial_descriptive_analysis/output/model_5arq_9dim"

csv_dir <- file.path(base_output_dir, "csv")
bootstrap_dir <- file.path(base_output_dir, "bootstrap")
models_dir <- file.path(base_output_dir, "models_clmm")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(bootstrap_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)


# ==============================================================================
# 2. DICCIONARIOS EN CASTELLANO
# ==============================================================================

dimension_dictionary <- tibble(
  dimension_key = c(
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

model_dictionary <- tibble(
  model_type = c("M0", "M1", "M2"),
  model_label = c(
    "M0: solo arquetipo",
    "M1: arquetipo + dimensiones",
    "M2: interacción arquetipo x dimensiones"
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
  model_dictionary,
  file.path(csv_dir, "model_dictionary_castellano.csv")
)


# ==============================================================================
# 3. PARÁMETROS
# ==============================================================================

dimensions <- dimension_dictionary$dimension_key

stage_levels <- stage_dictionary$adoption_stage

# Si TRUE, añade tecnología como control.
# De momento se deja en FALSE para evitar meter demasiados parámetros.
INCLUDE_TECHNOLOGY <- FALSE

# Modelos que se van a probar:
# M0: solo arquetipo
# M1: arquetipo + dimensiones
# M2: arquetipo * dimensiones
MODELS_TO_RUN <- c("M0", "M1", "M2")


# ==============================================================================
# 4. FUNCIONES DE TRADUCCIÓN PARA SALIDAS
# ==============================================================================

get_dimension_label <- function(dimension_key) {
  label <- dimension_dictionary %>%
    filter(dimension_key == !!dimension_key) %>%
    pull(dimension_label)
  
  if (length(label) == 0) {
    return(dimension_key)
  }
  
  label
}

get_stage_label <- function(stage_key) {
  label <- stage_dictionary %>%
    filter(adoption_stage == !!stage_key) %>%
    pull(adoption_stage_label)
  
  if (length(label) == 0) {
    return(stage_key)
  }
  
  label
}

get_model_label <- function(model_type_i) {
  label <- model_dictionary %>%
    filter(model_type == !!model_type_i) %>%
    pull(model_label)
  
  if (length(label) == 0) {
    return(model_type_i)
  }
  
  label
}

translate_formula <- function(formula_txt) {
  
  formula_out <- formula_txt
  
  formula_out <- str_replace_all(formula_out, fixed("adoption_stage"), "etapa_adopcion")
  formula_out <- str_replace_all(formula_out, fixed("macro_archetype_5"), "arquetipo_macro_5")
  formula_out <- str_replace_all(formula_out, fixed("participant_id"), "participante")
  formula_out <- str_replace_all(formula_out, fixed("technology"), "tecnologia")
  
  for (i in seq_len(nrow(dimension_dictionary))) {
    key_i <- dimension_dictionary$dimension_key[i]
    label_i <- dimension_dictionary$dimension_label[i]
    
    formula_out <- str_replace_all(
      formula_out,
      fixed(paste0("z_", key_i)),
      paste0("z_", str_replace_all(str_to_lower(label_i), " ", "_"))
    )
  }
  
  formula_out
}

translate_term_component <- function(term_component) {
  
  term_component <- str_squish(term_component)
  
  if (str_detect(term_component, "^z_")) {
    
    dim_key <- str_remove(term_component, "^z_")
    return(paste0("Dimensión: ", get_dimension_label(dim_key)))
    
  } else if (str_detect(term_component, "^macro_archetype_5")) {
    
    archetype_value <- str_remove(term_component, "^macro_archetype_5")
    archetype_value <- str_replace_all(archetype_value, "_", " ")
    return(paste0("Arquetipo: ", archetype_value))
    
  } else if (str_detect(term_component, "\\|")) {
    
    stage_parts <- str_split(term_component, "\\|", simplify = TRUE)
    
    if (ncol(stage_parts) == 2) {
      return(
        paste0(
          get_stage_label(stage_parts[1]),
          " | ",
          get_stage_label(stage_parts[2])
        )
      )
    }
    
  } else if (str_detect(term_component, "^technology")) {
    
    technology_value <- str_remove(term_component, "^technology")
    technology_value <- str_replace_all(technology_value, "_", " ")
    return(paste0("Tecnología: ", technology_value))
  }
  
  term_component
}

translate_term <- function(term) {
  
  if (is.na(term)) {
    return(NA_character_)
  }
  
  components <- str_split(term, ":", simplify = FALSE)[[1]]
  
  translated_components <- map_chr(
    components,
    translate_term_component
  )
  
  paste(translated_components, collapse = " x ")
}


# ==============================================================================
# 5. CARGAR DATOS
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
cat("Filas dataset completo:", nrow(df_full), "\n")
cat("Filas dataset bootstrap:", nrow(df_bootstrap), "\n")


# ==============================================================================
# 6. VALIDACIONES
# ==============================================================================

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


# ==============================================================================
# 7. FUNCIÓN DE PREPARACIÓN
# ==============================================================================

prepare_clmm_data <- function(data_i) {
  
  data_i %>%
    mutate(
      adoption_stage = ordered(
        adoption_stage,
        levels = stage_levels
      ),
      adoption_stage_label = case_when(
        as.character(adoption_stage) == "Knowledge" ~ "Conocimiento",
        as.character(adoption_stage) == "Considering" ~ "Consideración",
        as.character(adoption_stage) == "Done" ~ "Implementada",
        TRUE ~ NA_character_
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


# ==============================================================================
# 8. FÓRMULAS CLMM
# ==============================================================================

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


# ==============================================================================
# 9. FUNCIÓN SEGURA PARA AJUSTAR CLMM
# ==============================================================================

fit_clmm_safe <- function(data_i, model_type, sample_label, n_participants, boot_id) {
  
  formula_i <- build_formula(model_type)
  formula_txt <- paste(deparse(formula_i), collapse = " ")
  formula_castellano <- translate_formula(formula_txt)
  model_label_i <- get_model_label(model_type)
  
  warning_messages <- character()
  
  cat("\nAjustando", model_type, "-", model_label_i, "-", sample_label, "\n")
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
    
    return(
      list(
        model = NULL,
        summary = tibble(
          sample_label = sample_label,
          sample_label_es = case_when(
            sample_label == "full" ~ "muestra completa",
            sample_label == "bootstrap" ~ "bootstrap",
            TRUE ~ sample_label
          ),
          n_participants = n_participants,
          boot_id = boot_id,
          model_type = model_type,
          model_label = model_label_i,
          formula = formula_txt,
          formula_castellano = formula_castellano,
          status = "error",
          estado_modelo = "error",
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
      )
    )
  }
  
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
        sample_label_es = case_when(
          sample_label == "full" ~ "muestra completa",
          sample_label == "bootstrap" ~ "bootstrap",
          TRUE ~ sample_label
        ),
        n_participants = n_participants,
        boot_id = boot_id,
        model_type = model_type,
        model_label = model_label_i,
        formula = formula_txt,
        formula_castellano = formula_castellano,
        term_label = map_chr(term, translate_term),
        is_dimension_term = str_detect(term, "^z_"),
        is_interaction_term = str_detect(term, ":"),
        is_archetype_term = str_detect(term, "^macro_archetype_5"),
        tipo_termino = case_when(
          is_interaction_term ~ "Interacción",
          is_dimension_term ~ "Dimensión",
          is_archetype_term ~ "Arquetipo",
          str_detect(term, "\\|") ~ "Umbral de etapa",
          TRUE ~ "Otro"
        ),
        is_significant = if_else(
          !is.na(p.value) & p.value < 0.05,
          TRUE,
          FALSE,
          missing = FALSE
        ),
        significacion = case_when(
          is.na(p.value) ~ "sin p-valor",
          p.value < 0.001 ~ "p < 0.001",
          p.value < 0.01 ~ "p < 0.01",
          p.value < 0.05 ~ "p < 0.05",
          TRUE ~ "no significativo"
        )
      )
    
    n_terms_with_p <- sum(!is.na(terms_i$p.value))
    n_significant_terms <- sum(terms_i$is_significant, na.rm = TRUE)
  }
  
  summary_i <- tibble(
    sample_label = sample_label,
    sample_label_es = case_when(
      sample_label == "full" ~ "muestra completa",
      sample_label == "bootstrap" ~ "bootstrap",
      TRUE ~ sample_label
    ),
    n_participants = n_participants,
    boot_id = boot_id,
    model_type = model_type,
    model_label = model_label_i,
    formula = formula_txt,
    formula_castellano = formula_castellano,
    status = "ok",
    estado_modelo = "ajustado correctamente",
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


# ==============================================================================
# 10. AJUSTE EN DATASET COMPLETO
# ==============================================================================

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


# ==============================================================================
# 11. AJUSTE EN BOOTSTRAPS / SUBMUESTRAS
# ==============================================================================

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


# ==============================================================================
# 12. COMPARACIÓN DE MODELOS
# ==============================================================================

model_comparison_full <- full_model_summary %>%
  select(
    sample_label,
    sample_label_es,
    n_participants,
    boot_id,
    model_type,
    model_label,
    status,
    estado_modelo,
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
  group_by(n_participants, model_type, model_label) %>%
  summarise(
    n_boot = n(),
    n_ok = sum(status == "ok"),
    convergence_rate = mean(status == "ok"),
    tasa_convergencia = convergence_rate,
    mean_AIC = mean(AIC, na.rm = TRUE),
    sd_AIC = sd(AIC, na.rm = TRUE),
    mean_BIC = mean(BIC, na.rm = TRUE),
    sd_BIC = sd(BIC, na.rm = TRUE),
    mean_logLik = mean(logLik, na.rm = TRUE),
    mean_terms_with_p = mean(n_terms_with_p, na.rm = TRUE),
    mean_significant_terms = mean(n_significant_terms, na.rm = TRUE),
    n_with_warnings = sum(!is.na(warnings) & warnings != ""),
    n_con_advertencias = n_with_warnings,
    .groups = "drop"
  ) %>%
  arrange(n_participants, mean_AIC)

write_csv(
  model_comparison_bootstrap,
  file.path(csv_dir, "clmm_bootstrap_model_comparison.csv")
)

cat("\nComparación media por bootstrap:\n")
print(model_comparison_bootstrap, n = Inf)


# ==============================================================================
# 13. RESUMEN DE TÉRMINOS SIGNIFICATIVOS
# ==============================================================================

significant_terms_summary <- bootstrap_model_terms %>%
  filter(!is.na(p.value)) %>%
  group_by(
    n_participants,
    model_type,
    model_label,
    term,
    term_label,
    tipo_termino
  ) %>%
  summarise(
    n_boot = n(),
    n_significant = sum(p.value < 0.05, na.rm = TRUE),
    n_significativo = n_significant,
    significance_rate = n_significant / n_boot,
    tasa_significacion = significance_rate,
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


# ==============================================================================
# 14. VERSIONES DE SALIDA MÁS LEGIBLES EN CASTELLANO
# ==============================================================================

full_model_summary_castellano <- full_model_summary %>%
  select(
    muestra = sample_label_es,
    n_participantes = n_participants,
    boot_id,
    modelo = model_label,
    formula_modelo = formula_castellano,
    estado = estado_modelo,
    mensaje_error = error_message,
    advertencias = warnings,
    n_filas = n_rows,
    n_participantes_unicos = n_unique_participants,
    AIC,
    BIC,
    logLik,
    n_terminos_con_p = n_terms_with_p,
    n_terminos_significativos = n_significant_terms
  )

bootstrap_model_summary_castellano <- bootstrap_model_summary %>%
  select(
    muestra = sample_label_es,
    n_participantes = n_participants,
    boot_id,
    modelo = model_label,
    formula_modelo = formula_castellano,
    estado = estado_modelo,
    mensaje_error = error_message,
    advertencias = warnings,
    n_filas = n_rows,
    n_participantes_unicos = n_unique_participants,
    AIC,
    BIC,
    logLik,
    n_terminos_con_p = n_terms_with_p,
    n_terminos_significativos = n_significant_terms
  )

full_model_terms_castellano <- full_model_terms %>%
  select(
    muestra = sample_label_es,
    n_participantes = n_participants,
    boot_id,
    modelo = model_label,
    termino_original = term,
    termino = term_label,
    tipo_termino,
    estimate,
    std.error,
    statistic,
    p.value,
    significacion,
    formula_modelo = formula_castellano
  )

bootstrap_model_terms_castellano <- bootstrap_model_terms %>%
  select(
    muestra = sample_label_es,
    n_participantes = n_participants,
    boot_id,
    modelo = model_label,
    termino_original = term,
    termino = term_label,
    tipo_termino,
    estimate,
    std.error,
    statistic,
    p.value,
    significacion,
    formula_modelo = formula_castellano
  )

write_csv(
  full_model_summary_castellano,
  file.path(csv_dir, "clmm_full_model_summary_castellano.csv")
)

write_csv(
  bootstrap_model_summary_castellano,
  file.path(csv_dir, "clmm_bootstrap_model_summary_castellano.csv")
)

write_csv(
  full_model_terms_castellano,
  file.path(csv_dir, "clmm_full_model_terms_castellano.csv")
)

write_csv(
  bootstrap_model_terms_castellano,
  file.path(csv_dir, "clmm_bootstrap_model_terms_castellano.csv")
)


# ==============================================================================
# 15. MENSAJE FINAL
# ==============================================================================

cat("Archivos principales creados:\n")
cat("- clmm_full_model_summary.csv\n")
cat("- clmm_full_model_terms.csv\n")
cat("- clmm_full_model_comparison.csv\n")
cat("- clmm_bootstrap_model_summary.csv\n")
cat("- clmm_bootstrap_model_terms.csv\n")
cat("- clmm_bootstrap_model_comparison.csv\n")
cat("- clmm_bootstrap_significant_terms_summary.csv\n")
cat("- clmm_full_model_summary.csv\n")
cat("- clmm_bootstrap_model_summary.csv\n")
cat("- clmm_full_model_terms.csv\n")
cat("- clmm_bootstrap_model_terms.csv\n")
