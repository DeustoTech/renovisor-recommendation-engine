# ==============================================================================
# SCRIPT 17 - CLMM DATOS SINTÉTICOS
# 5 MACROARQUETIPOS x 3 ETAPAS x 9 DIMENSIONES
# ==============================================================================
# Objetivo:
#   Ajustar modelos CLMM sobre datos sintéticos.
#
# Cambio metodológico:
#   - Las dimensiones se construyen a partir de determinantes estandarizados,
#     cuando los determinantes están disponibles.
#   - Los determinantes ausentes / no seleccionados se codifican como 0
#     en escala estandarizada.
#   - 0 significa ausencia de contribución / ausencia de señal.
#
# Modelos:
#   M0: adoption_stage ~ macro_archetype_5 + (1 | participant_id)
#   M1: adoption_stage ~ macro_archetype_5 + dimensiones + (1 | participant_id)
#   M2: adoption_stage ~ macro_archetype_5 * dimensiones + (1 | participant_id)
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
# 1. CONFIGURACIÓN
# ==============================================================================

RUN_BOOTSTRAPS <- TRUE

N_BOOTSTRAPS_PER_SIZE <- 100
# N_BOOTSTRAPS_TO_RUN <- Inf

INCLUDE_TECHNOLOGY <- FALSE
SAVE_MODEL_RDS <- FALSE

MODELS_TO_RUN <- c("M0", "M1", "M2_REDUCED", "M2_FULL")

MAX_FULL_PARTICIPANTS <- 1000

synthetic_csv_dir <- "initial_descriptive_analysis/output/model_5arq_9dim/csv"
synthetic_bootstrap_dir <- "initial_descriptive_analysis/output/model_5arq_9dim/bootstrap"

synthetic_full_path <- file.path(synthetic_csv_dir, "synthetic_5arq_9dim.csv")
synthetic_bootstrap_path <- file.path(synthetic_bootstrap_dir, "bootstrap_5arq_9dim_all.csv")

mapping_candidates <- c(
  "initial_descriptive_analysis/output/ttm_stage_analysis/csv/dimension_determinant_mapping_long.csv",
  file.path(synthetic_csv_dir, "dimension_determinant_mapping_long.csv")
)

base_output_dir <- "initial_descriptive_analysis/output/model_5arq_9dim_clmm_standardized_determinants"

csv_dir <- file.path(base_output_dir, "csv")
bootstrap_dir <- file.path(base_output_dir, "bootstrap")
models_dir <- file.path(base_output_dir, "models_clmm")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(bootstrap_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 2. DICCIONARIOS
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
    "Seguridad financiera",
    "Seguridad",
    "Competencia",
    "Autonomía",
    "Materialidad",
    "Vinculación",
    "Estímulo",
    "Popularidad",
    "Significado"
  )
)

stage_dictionary <- tibble(
  adoption_stage = c("Knowledge", "Considering", "Done"),
  adoption_stage_label = c(
    "Conocimiento / curiosidad",
    "Consideración",
    "Implementada"
  )
)

macro_archetype_dictionary <- tibble(
  macro_archetype_5 = c(
    "G1_Activist_Stubborn_Sentient",
    "G2_EarlyAdopter_Influencer",
    "G3_Fearful_Careful",
    "G4_HomoEconomicus",
    "G5_Uninterested"
  ),
  macro_archetype_label = c(
    "G1 - Activista / sensible",
    "G2 - Pionero / influyente",
    "G3 - Temeroso / cuidadoso",
    "G4 - Homo economicus",
    "G5 - Desinteresado"
  )
)

model_dictionary <- tibble(
  model_type = c("M0", "M1", "M2_REDUCED", "M2_FULL"),
  model_label = c(
    "M0: solo arquetipo",
    "M1: arquetipo + dimensiones",
    "M2 reducido: interacciones teóricas",
    "M2 completo: interacción arquetipo x todas las dimensiones"
  )
)

dimensions <- dimension_dictionary$dimension_key
stage_levels <- stage_dictionary$adoption_stage

write_csv(dimension_dictionary, file.path(csv_dir, "dimension_dictionary_castellano.csv"))
write_csv(stage_dictionary, file.path(csv_dir, "stage_dictionary_castellano.csv"))
write_csv(macro_archetype_dictionary, file.path(csv_dir, "macro_archetype_dictionary_castellano.csv"))
write_csv(model_dictionary, file.path(csv_dir, "model_dictionary_castellano.csv"))

# ==============================================================================
# 3. FUNCIONES
# ==============================================================================

safe_z_zero <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  
  if (is.na(mean_x) || is.nan(mean_x)) {
    return(rep(0, length(x)))
  }
  
  if (is.na(sd_x) || is.nan(sd_x) || sd_x == 0) {
    z <- rep(0, length(x))
  } else {
    z <- (x - mean_x) / sd_x
  }
  
  z[is.na(z)] <- 0
  z[is.nan(z)] <- 0
  z[is.infinite(z)] <- 0
  
  z
}

read_determinant_mapping <- function() {
  
  existing_mapping <- mapping_candidates[file.exists(mapping_candidates)]
  
  if (length(existing_mapping) == 0) {
    warning("No se encontró dimension_determinant_mapping_long.csv. Se usarán dimensiones existentes si están disponibles.")
    return(tibble())
  }
  
  read_csv(existing_mapping[1], show_col_types = FALSE) %>%
    filter(is_linked == 1) %>%
    select(dimension_key, determinant_id) %>%
    distinct()
}

get_dimension_label <- function(dimension_key) {
  label <- dimension_dictionary %>%
    filter(dimension_key == !!dimension_key) %>%
    pull(dimension_label)
  
  if (length(label) == 0) return(dimension_key)
  label
}

get_model_label <- function(model_type_i) {
  label <- model_dictionary %>%
    filter(model_type == !!model_type_i) %>%
    pull(model_label)
  
  if (length(label) == 0) return(model_type_i)
  label
}

translate_term <- function(term) {
  if (is.na(term)) return(NA_character_)
  
  components <- str_split(term, ":", simplify = FALSE)[[1]]
  
  translated_components <- map_chr(components, function(x) {
    x <- str_squish(x)
    
    if (str_detect(x, "^z_")) {
      dim_key <- str_remove(x, "^z_")
      return(paste0("Dimensión: ", get_dimension_label(dim_key)))
    }
    
    if (str_detect(x, "^macro_archetype_5")) {
      archetype_value <- str_remove(x, "^macro_archetype_5")
      archetype_value <- str_replace_all(archetype_value, "_", " ")
      return(paste0("Arquetipo: ", archetype_value))
    }
    
    x
  })
  
  paste(translated_components, collapse = " x ")
}

get_sample_label_es <- function(sample_label) {
  case_when(
    sample_label == "full" ~ "muestra completa",
    sample_label == "bootstrap" ~ "bootstrap sintético",
    TRUE ~ sample_label
  )
}

# ==============================================================================
# 4. RECONSTRUIR DIMENSIONES
# ==============================================================================

determinant_mapping <- read_determinant_mapping()

rebuild_dimensions_from_standardized_determinants <- function(data_i, dataset_label = "dataset") {
  
  if (nrow(determinant_mapping) == 0) {
    
    missing_dims <- setdiff(dimensions, names(data_i))
    
    if (length(missing_dims) > 0) {
      stop(
        "No hay mapeo de determinantes y faltan dimensiones en ",
        dataset_label,
        ": ",
        paste(missing_dims, collapse = ", ")
      )
    }
    
    warning(
      "No se han encontrado determinantes/mapeo. Se usan dimensiones existentes en ",
      dataset_label,
      " sustituyendo NA por 0."
    )
    
    return(
      data_i %>%
        mutate(
          across(
            all_of(dimensions),
            ~ {
              x <- suppressWarnings(as.numeric(.x))
              x[is.na(x)] <- 0
              x[is.nan(x)] <- 0
              x[is.infinite(x)] <- 0
              x
            }
          )
        )
    )
  }
  
  determinant_cols <- intersect(determinant_mapping$determinant_id, names(data_i))
  
  if (length(determinant_cols) == 0) {
    
    missing_dims <- setdiff(dimensions, names(data_i))
    
    if (length(missing_dims) > 0) {
      stop(
        "No hay columnas de determinantes ni dimensiones suficientes en ",
        dataset_label,
        ". Faltan: ",
        paste(missing_dims, collapse = ", ")
      )
    }
    
    warning(
      "No hay columnas de determinantes en ",
      dataset_label,
      ". Se usan dimensiones existentes sustituyendo NA por 0."
    )
    
    return(
      data_i %>%
        mutate(
          across(
            all_of(dimensions),
            ~ {
              x <- suppressWarnings(as.numeric(.x))
              x[is.na(x)] <- 0
              x[is.nan(x)] <- 0
              x[is.infinite(x)] <- 0
              x
            }
          )
        )
    )
  }
  
  cat("\nReconstruyendo dimensiones desde determinantes estandarizados -", dataset_label, "\n")
  cat("Determinantes encontrados:", length(determinant_cols), "\n")
  
  data_z <- data_i %>%
    mutate(
      across(
        all_of(determinant_cols),
        safe_z_zero,
        .names = "zdet_{.col}"
      )
    )
  
  dimension_scores <- map_dfc(
    dimensions,
    function(dim_i) {
      
      cols_i <- determinant_mapping %>%
        filter(dimension_key == dim_i) %>%
        pull(determinant_id) %>%
        intersect(determinant_cols)
      
      z_cols_i <- paste0("zdet_", cols_i)
      z_cols_i <- intersect(z_cols_i, names(data_z))
      
      if (length(z_cols_i) == 0) {
        score_i <- rep(0, nrow(data_z))
      } else {
        score_i <- rowMeans(
          data_z %>% select(all_of(z_cols_i)),
          na.rm = FALSE
        )
      }
      
      score_i[is.na(score_i)] <- 0
      score_i[is.nan(score_i)] <- 0
      score_i[is.infinite(score_i)] <- 0
      
      tibble(!!dim_i := score_i)
    }
  )
  
  data_z %>%
    select(-any_of(dimensions)) %>%
    bind_cols(dimension_scores)
}

# ==============================================================================
# 5. CARGAR DATOS SINTÉTICOS
# ==============================================================================

if (!file.exists(synthetic_full_path)) {
  stop("No existe synthetic_5arq_9dim.csv en: ", synthetic_full_path)
}

df_full_raw <- read_csv(synthetic_full_path, show_col_types = FALSE)

if (!is.null(MAX_FULL_PARTICIPANTS)) {
  
  participant_pool <- df_full_raw %>%
    distinct(participant_id)
  
  n_to_sample <- min(MAX_FULL_PARTICIPANTS, nrow(participant_pool))
  
  selected_full_participants <- participant_pool %>%
    slice_sample(n = n_to_sample) %>%
    pull(participant_id)
  
  df_full_raw <- df_full_raw %>%
    filter(participant_id %in% selected_full_participants)
}

df_full <- rebuild_dimensions_from_standardized_determinants(
  df_full_raw,
  dataset_label = "synthetic_full"
)

if (RUN_BOOTSTRAPS) {
  
  if (!file.exists(synthetic_bootstrap_path)) {
    stop("No existe bootstrap_5arq_9dim_all.csv en: ", synthetic_bootstrap_path)
  }
  
  df_bootstrap_raw <- read_csv(synthetic_bootstrap_path, show_col_types = FALSE)
  
} else {
  df_bootstrap_raw <- tibble()
}

cat("\nDatos sintéticos cargados correctamente\n")
cat("Filas dataset completo:", nrow(df_full), "\n")
cat("Participantes dataset completo:", n_distinct(df_full$participant_id), "\n")

# ==============================================================================
# 6. PREPARAR CLMM
# ==============================================================================

required_base_cols <- c(
  "participant_id",
  "macro_archetype_5",
  "adoption_stage"
)

missing_full_base <- setdiff(required_base_cols, names(df_full))

if (length(missing_full_base) > 0) {
  stop("Faltan columnas base en df_full: ", paste(missing_full_base, collapse = ", "))
}

missing_full_dims <- setdiff(dimensions, names(df_full))

if (length(missing_full_dims) > 0) {
  stop("Faltan dimensiones en df_full tras reconstrucción: ", paste(missing_full_dims, collapse = ", "))
}

if (RUN_BOOTSTRAPS) {
  
  missing_boot_base <- setdiff(
    c("n_participants", "boot_id", required_base_cols),
    names(df_bootstrap_raw)
  )
  
  if (length(missing_boot_base) > 0) {
    stop("Faltan columnas base en df_bootstrap_raw: ", paste(missing_boot_base, collapse = ", "))
  }
}

prepare_clmm_data <- function(data_i) {
  
  if (!"technology" %in% names(data_i)) {
    data_i <- data_i %>%
      mutate(technology = NA_character_)
  }
  
  macro_levels <- c(
    "G1_Activist_Stubborn_Sentient",
    "G2_EarlyAdopter_Influencer",
    "G3_Fearful_Careful",
    "G4_HomoEconomicus",
    "G5_Uninterested"
  )
  
  data_i %>%
    mutate(
      adoption_stage = ordered(adoption_stage, levels = stage_levels),
      macro_archetype_5 = factor(macro_archetype_5, levels = macro_levels),
      participant_id = factor(participant_id),
      technology = factor(technology)
    ) %>%
    mutate(
      across(
        all_of(dimensions),
        ~ {
          x <- suppressWarnings(as.numeric(.x))
          x[is.na(x)] <- 0
          x[is.nan(x)] <- 0
          x[is.infinite(x)] <- 0
          x
        },
        .names = "z_{.col}"
      )
    ) %>%
    drop_na(
      adoption_stage,
      macro_archetype_5,
      participant_id
    )
}

build_formula <- function(model_type) {
  
  z_dims <- paste0("z_", dimensions)
  
  z_dims_reduced <- c(
    "z_STIMULATION",
    "z_PHYSIOLOGICAL",
    "z_FINANCIAL",
    "z_AUTONOMY",
    "z_MEANING"
  )
  
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
    
  } else if (model_type == "M2_REDUCED") {
    
    formula_txt <- paste0(
      "adoption_stage ~ macro_archetype_5 + ",
      paste(z_dims, collapse = " + "),
      " + macro_archetype_5:(",
      paste(z_dims_reduced, collapse = " + "),
      ")",
      tech_part,
      " + (1 | participant_id)"
    )
    
  } else if (model_type == "M2_FULL") {
    
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

fit_clmm_safe <- function(data_i, model_type, sample_label, n_participants, boot_id) {
  
  formula_i <- build_formula(model_type)
  formula_txt <- paste(deparse(formula_i), collapse = " ")
  model_label_i <- get_model_label(model_type)
  
  warning_messages <- character()
  
  cat("\nAjustando", model_type, "-", model_label_i, "-", sample_label, "- boot", boot_id, "\n")
  cat("Filas:", nrow(data_i), "| Participantes:", n_distinct(data_i$participant_id), "\n")
  
  if (nrow(data_i) == 0 || n_distinct(data_i$adoption_stage) < 2) {
    return(
      list(
        model = NULL,
        summary = tibble(
          sample_label = sample_label,
          sample_label_es = get_sample_label_es(sample_label),
          n_participants = n_participants,
          boot_id = boot_id,
          model_type = model_type,
          model_label = model_label_i,
          formula = formula_txt,
          status = "error",
          error_message = "Dataset vacío o con menos de 2 etapas",
          warnings = NA_character_,
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
          sample_label_es = get_sample_label_es(sample_label),
          n_participants = n_participants,
          boot_id = boot_id,
          model_type = model_type,
          model_label = model_label_i,
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
      )
    )
  }
  
  if (SAVE_MODEL_RDS) {
    saveRDS(
      model_i,
      file.path(
        models_dir,
        paste0("clmm_", model_type, "_", sample_label, "_boot", boot_id, ".rds")
      )
    )
  }
  
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
        sample_label_es = get_sample_label_es(sample_label),
        n_participants = n_participants,
        boot_id = boot_id,
        model_type = model_type,
        model_label = model_label_i,
        formula = formula_txt,
        term_label = map_chr(term, translate_term),
        tipo_termino = case_when(
          str_detect(term, ":") ~ "Interacción",
          str_detect(term, "^z_") ~ "Dimensión",
          str_detect(term, "^macro_archetype_5") ~ "Arquetipo",
          str_detect(term, "\\|") ~ "Umbral de etapa",
          TRUE ~ "Otro"
        ),
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
    sample_label_es = get_sample_label_es(sample_label),
    n_participants = n_participants,
    boot_id = boot_id,
    model_type = model_type,
    model_label = model_label_i,
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

# ==============================================================================
# 7. MODELOS SOBRE DATASET COMPLETO
# ==============================================================================

df_full_clmm <- prepare_clmm_data(df_full)

write_csv(
  df_full_clmm,
  file.path(csv_dir, "clmm_synthetic_model_dataset_prepared_full.csv")
)

cat("\nDataset sintético preparado:\n")
cat("Filas:", nrow(df_full_clmm), "\n")
cat("Participantes:", n_distinct(df_full_clmm$participant_id), "\n")

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

write_csv(full_model_summary, file.path(csv_dir, "clmm_synthetic_full_model_summary.csv"))
write_csv(full_model_terms, file.path(csv_dir, "clmm_synthetic_full_model_terms.csv"))

cat("\nResumen modelos sintéticos completos:\n")
print(full_model_summary, n = Inf)

# ==============================================================================
# 8. MODELOS SOBRE BOOTSTRAPS SINTÉTICOS
# ==============================================================================

if (RUN_BOOTSTRAPS) {
  
  bootstrap_keys <- df_bootstrap_raw %>%
    distinct(n_participants, boot_id) %>%
    arrange(n_participants, boot_id)
  
  if (!is.infinite(N_BOOTSTRAPS_PER_SIZE)) {
    bootstrap_keys <- bootstrap_keys %>%
      group_by(n_participants) %>%
      slice_head(n = N_BOOTSTRAPS_PER_SIZE) %>%
      ungroup()
  }
  
  write_csv(
    bootstrap_keys,
    file.path(csv_dir, "clmm_synthetic_bootstrap_keys_to_run.csv")
  )
  
  fit_one_bootstrap <- function(n_i, b_i) {
    
    cat("\n==============================\n")
    cat("Bootstrap sintético n =", n_i, "| boot =", b_i, "\n")
    cat("==============================\n")
    
    data_i_raw <- df_bootstrap_raw %>%
      filter(
        n_participants == n_i,
        boot_id == b_i
      )
    
    data_i <- rebuild_dimensions_from_standardized_determinants(
      data_i_raw,
      dataset_label = paste0("synthetic_bootstrap_n", n_i, "_boot", b_i)
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
  
} else {
  bootstrap_model_summary <- tibble()
  bootstrap_model_terms <- tibble()
}

write_csv(
  bootstrap_model_summary,
  file.path(csv_dir, "clmm_synthetic_bootstrap_model_summary.csv")
)

write_csv(
  bootstrap_model_terms,
  file.path(csv_dir, "clmm_synthetic_bootstrap_model_terms.csv")
)

# ==============================================================================
# 9. COMPARACIONES Y TÉRMINOS
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
    AIC,
    BIC,
    logLik,
    warnings
  ) %>%
  arrange(AIC)

write_csv(
  model_comparison_full,
  file.path(csv_dir, "clmm_synthetic_full_model_comparison.csv")
)

if (nrow(bootstrap_model_summary) > 0) {
  
  model_comparison_bootstrap <- bootstrap_model_summary %>%
    group_by(n_participants, model_type, model_label) %>%
    summarise(
      n_boot = n_distinct(boot_id),
      n_ok = sum(status == "ok"),
      convergence_rate = n_ok / n_boot,
      mean_AIC = mean(AIC, na.rm = TRUE),
      sd_AIC = sd(AIC, na.rm = TRUE),
      mean_BIC = mean(BIC, na.rm = TRUE),
      sd_BIC = sd(BIC, na.rm = TRUE),
      mean_logLik = mean(logLik, na.rm = TRUE),
      sd_logLik = sd(logLik, na.rm = TRUE),
      mean_terms_with_p = mean(n_terms_with_p, na.rm = TRUE),
      mean_significant_terms = mean(n_significant_terms, na.rm = TRUE),
      n_with_warnings = sum(!is.na(warnings) & warnings != ""),
      .groups = "drop"
    ) %>%
    arrange(n_participants, mean_AIC)
  
  write_csv(
    model_comparison_bootstrap,
    file.path(csv_dir, "clmm_synthetic_bootstrap_model_comparison.csv")
  )
  
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
      n_boot_with_term = n_distinct(boot_id),
      n_significant = sum(p.value < 0.05, na.rm = TRUE),
      mean_estimate = mean(estimate, na.rm = TRUE),
      sd_estimate = sd(estimate, na.rm = TRUE),
      mean_p_value = mean(p.value, na.rm = TRUE),
      median_p_value = median(p.value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      bootstrap_model_summary %>%
        filter(status == "ok") %>%
        count(n_participants, model_type, model_label, name = "n_ok_models"),
      by = c("n_participants", "model_type", "model_label")
    ) %>%
    mutate(
      significance_rate = n_significant / n_ok_models,
      conditional_significance_rate = n_significant / n_boot_with_term,
      term_presence_rate = n_boot_with_term / n_ok_models
    ) %>%
    arrange(n_participants, model_type, desc(significance_rate), mean_p_value)
  
  write_csv(
    significant_terms_summary,
    file.path(csv_dir, "clmm_synthetic_bootstrap_significant_terms_summary.csv")
  )
}

cat("Archivos creados en:\n")
cat(csv_dir, "\n")