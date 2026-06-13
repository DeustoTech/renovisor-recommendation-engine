
# SCRIPT 17 - CLMM DATOS SINTÉTICOS 5 MACROARQUETIPOS x 3 ETAPAS x 9 DIMENSIONES

# Objetivo:
#   Ajustar modelos CLMM sobre datos sintéticos.
#
# Cambio metodológico:
#   - Las dimensiones se construyen a partir de determinantes estandarizados.
#   - Los determinantes ausentes / no seleccionados se codifican como 0
#     en escala estandarizada.
#   - Por tanto, 0 significa ausencia de contribución / ausencia de señal,
#     no intensidad media sustantiva.
#
# Modelos:
#   M0: adoption_stage ~ macro_archetype_5 + (1 | participant_id)
#   M1: adoption_stage ~ macro_archetype_5 + dimensiones + (1 | participant_id)
#   M2: adoption_stage ~ macro_archetype_5 * dimensiones + (1 | participant_id)

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(tibble)
library(ordinal)
library(broom.mixed)

set.seed(123)


# 1. CONFIGURACIÓN GENERAL
RUN_BOOTSTRAPS <- TRUE

# Primero prueba con 5 o 10.
# Luego puedes subirlo.
N_BOOTSTRAPS_TO_RUN <- 5
# N_BOOTSTRAPS_TO_RUN <- Inf

INCLUDE_TECHNOLOGY <- FALSE
SAVE_MODEL_RDS <- FALSE

MODELS_TO_RUN <- c("M0", "M1", "M2")

synthetic_csv_dir <- "initial_descriptive_analysis/output/model_5arq_9dim/csv"
synthetic_bootstrap_dir <- "initial_descriptive_analysis/output/model_5arq_9dim/bootstrap"

synthetic_full_path <- file.path(synthetic_csv_dir, "synthetic_5arq_9dim.csv")
synthetic_bootstrap_path <- file.path(synthetic_bootstrap_dir, "bootstrap_5arq_9dim_all.csv")

mapping_candidates <- c(
  "initial_descriptive_analysis/output/ttm_stage_analysis/csv/dimension_determinant_mapping_long.csv",
  file.path(synthetic_csv_dir, "dimension_determinant_mapping_long.csv")
)


# 2. CARPETAS
base_output_dir <- "initial_descriptive_analysis/output/model_5arq_9dim_clmm_standardized_determinants"

csv_dir <- file.path(base_output_dir, "csv")
bootstrap_dir <- file.path(base_output_dir, "bootstrap")
models_dir <- file.path(base_output_dir, "models_clmm")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(bootstrap_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)

# 3. DICCIONARIOS
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
  model_type = c("M0", "M1", "M2"),
  model_label = c(
    "M0: solo arquetipo",
    "M1: arquetipo + dimensiones",
    "M2: interacción arquetipo x dimensiones"
  )
)

dimensions <- dimension_dictionary$dimension_key
stage_levels <- stage_dictionary$adoption_stage

write_csv(dimension_dictionary, file.path(csv_dir, "dimension_dictionary_castellano.csv"))
write_csv(stage_dictionary, file.path(csv_dir, "stage_dictionary_castellano.csv"))
write_csv(macro_archetype_dictionary, file.path(csv_dir, "macro_archetype_dictionary_castellano.csv"))
write_csv(model_dictionary, file.path(csv_dir, "model_dictionary_castellano.csv"))


# 4. FUNCIONES AUXILIARES
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
    warning("No se encontró dimension_determinant_mapping_long.csv. Se usarán las dimensiones existentes si están disponibles.")
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

get_stage_label <- function(stage_key) {
  label <- stage_dictionary %>%
    filter(adoption_stage == !!stage_key) %>%
    pull(adoption_stage_label)
  
  if (length(label) == 0) return(stage_key)
  label
}

get_model_label <- function(model_type_i) {
  label <- model_dictionary %>%
    filter(model_type == !!model_type_i) %>%
    pull(model_label)
  
  if (length(label) == 0) return(model_type_i)
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
  }
  
  if (str_detect(term_component, "^macro_archetype_5")) {
    archetype_value <- str_remove(term_component, "^macro_archetype_5")
    archetype_value <- str_replace_all(archetype_value, "_", " ")
    return(paste0("Arquetipo: ", archetype_value))
  }
  
  if (str_detect(term_component, "\\|")) {
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
  }
  
  if (str_detect(term_component, "^technology")) {
    technology_value <- str_remove(term_component, "^technology")
    technology_value <- str_replace_all(technology_value, "_", " ")
    return(paste0("Tecnología: ", technology_value))
  }
  
  term_component
}

translate_term <- function(term) {
  if (is.na(term)) return(NA_character_)
  
  components <- str_split(term, ":", simplify = FALSE)[[1]]
  translated_components <- map_chr(components, translate_term_component)
  
  paste(translated_components, collapse = " x ")
}

get_sample_label_es <- function(sample_label) {
  case_when(
    sample_label == "full" ~ "muestra completa",
    sample_label == "bootstrap" ~ "bootstrap sintético",
    TRUE ~ sample_label
  )
}

# 5. RECONSTRUIR DIMENSIONES DESDE DETERMINANTES ESTANDARIZADOS
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
      "No se han encontrado determinantes/mapeo. Se usan las dimensiones existentes en ",
      dataset_label,
      " sustituyendo NA por 0. Esto no recompone dimensiones desde determinantes."
    )
    
    data_i <- data_i %>%
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
    
    return(data_i)
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
    
    data_i <- data_i %>%
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
    
    return(data_i)
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
  
  data_out <- data_z %>%
    select(-any_of(dimensions)) %>%
    bind_cols(dimension_scores)
  
  data_out
}

# 6. CARGAR DATOS SINTÉTICOS
if (!file.exists(synthetic_full_path)) {
  stop("No existe synthetic_5arq_9dim.csv en: ", synthetic_full_path)
}

df_full_raw <- read_csv(synthetic_full_path, show_col_types = FALSE)

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


# 7. VALIDACIONES
required_cols <- c(
  "participant_id",
  "macro_archetype_5",
  "adoption_stage",
  dimensions
)

missing_full <- setdiff(required_cols, names(df_full))

if (length(missing_full) > 0) {
  stop("Faltan columnas en df_full: ", paste(missing_full, collapse = ", "))
}

if (RUN_BOOTSTRAPS) {
  
  missing_boot <- setdiff(
    c("n_participants", "boot_id", required_cols),
    names(df_bootstrap_raw)
  )
  
  if (length(missing_boot) > 0) {
    stop("Faltan columnas en df_bootstrap_raw: ", paste(missing_boot, collapse = ", "))
  }
}


# 8. PREPARAR DATOS CLMM
prepare_clmm_data <- function(data_i) {
  
  macro_levels <- c(
    "G1_Activist_Stubborn_Sentient",
    "G2_EarlyAdopter_Influencer",
    "G3_Fearful_Careful",
    "G4_HomoEconomicus",
    "G5_Uninterested"
  )
  
  data_i %>%
    mutate(
      adoption_stage = ordered(
        adoption_stage,
        levels = stage_levels
      ),
      adoption_stage_label = case_when(
        as.character(adoption_stage) == "Knowledge" ~ "Conocimiento / curiosidad",
        as.character(adoption_stage) == "Considering" ~ "Consideración",
        as.character(adoption_stage) == "Done" ~ "Implementada",
        TRUE ~ NA_character_
      ),
      macro_archetype_5 = factor(
        macro_archetype_5,
        levels = macro_levels
      ),
      participant_id = factor(participant_id),
      technology = if ("technology" %in% names(.)) factor(technology) else NA
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


# 9. FÓRMULAS
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


# 10. AJUSTE SEGURO
fit_clmm_safe <- function(data_i, model_type, sample_label, n_participants, boot_id) {
  
  formula_i <- build_formula(model_type)
  formula_txt <- paste(deparse(formula_i), collapse = " ")
  formula_castellano <- translate_formula(formula_txt)
  model_label_i <- get_model_label(model_type)
  
  warning_messages <- character()
  
  cat("\nAjustando", model_type, "-", model_label_i, "-", sample_label, "- boot", boot_id, "\n")
  cat("Filas:", nrow(data_i), "| Participantes:", n_distinct(data_i$participant_id), "\n")
  
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
    sample_label_es = get_sample_label_es(sample_label),
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


# 11. MODELOS SOBRE DATASET COMPLETO
df_full_clmm <- prepare_clmm_data(df_full)

write_csv(
  df_full_clmm,
  file.path(csv_dir, "clmm_synthetic_model_dataset_prepared_full.csv")
)

cat("\nDataset sintético preparado:\n")
cat("Filas:", nrow(df_full_clmm), "\n")
cat("Participantes:", n_distinct(df_full_clmm$participant_id), "\n")

cat("\nDistribución macroarquetipo x etapa:\n")
print(
  df_full_clmm %>%
    count(macro_archetype_5, adoption_stage) %>%
    pivot_wider(
      names_from = adoption_stage,
      values_from = n,
      values_fill = 0
    ),
  n = Inf
)

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


# 12. MODELOS SOBRE BOOTSTRAPS SINTÉTICOS
if (RUN_BOOTSTRAPS) {
  
  bootstrap_keys <- df_bootstrap_raw %>%
    distinct(n_participants, boot_id) %>%
    arrange(n_participants, boot_id)
  
  if (!is.infinite(N_BOOTSTRAPS_TO_RUN)) {
    bootstrap_keys <- bootstrap_keys %>%
      slice_head(n = N_BOOTSTRAPS_TO_RUN)
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
  
  write_csv(
    bootstrap_model_summary,
    file.path(csv_dir, "clmm_synthetic_bootstrap_model_summary.csv")
  )
  
  write_csv(
    bootstrap_model_terms,
    file.path(csv_dir, "clmm_synthetic_bootstrap_model_terms.csv")
  )
  
} else {
  
  bootstrap_model_summary <- tibble()
  bootstrap_model_terms <- tibble()
}

# 13. COMPARACIÓN DE MODELOS
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
  file.path(csv_dir, "clmm_synthetic_full_model_comparison.csv")
)

if (nrow(bootstrap_model_summary) > 0) {
  
  model_comparison_bootstrap <- bootstrap_model_summary %>%
    group_by(n_participants, model_type, model_label) %>%
    summarise(
      n_boot = n_distinct(boot_id),
      n_ok = sum(status == "ok"),
      convergence_rate = n_ok / n_boot,
      tasa_convergencia = convergence_rate,
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
}

# 14. TÉRMINOS SIGNIFICATIVOS
significant_terms_full <- full_model_terms %>%
  filter(!is.na(p.value)) %>%
  arrange(model_type, p.value)

write_csv(
  significant_terms_full,
  file.path(csv_dir, "clmm_synthetic_full_significant_terms_ordered.csv")
)

if (nrow(bootstrap_model_terms) > 0) {
  
  bootstrap_ok_by_model <- bootstrap_model_summary %>%
    filter(status == "ok") %>%
    count(n_participants, model_type, model_label, name = "n_ok_models")
  
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
      mean_std_error = mean(std.error, na.rm = TRUE),
      mean_p_value = mean(p.value, na.rm = TRUE),
      median_p_value = median(p.value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      bootstrap_ok_by_model,
      by = c("n_participants", "model_type", "model_label")
    ) %>%
    mutate(
      significance_rate = n_significant / n_ok_models,
      tasa_significacion = significance_rate,
      term_presence_rate = n_boot_with_term / n_ok_models
    ) %>%
    arrange(
      n_participants,
      model_type,
      desc(significance_rate),
      mean_p_value
    )
  
  write_csv(
    significant_terms_summary,
    file.path(csv_dir, "clmm_synthetic_bootstrap_significant_terms_summary.csv")
  )
}

cat("Archivos en:\n")
cat(csv_dir, "\n")

cat("\nPrincipales:\n")
cat("- clmm_synthetic_full_model_summary.csv\n")
cat("- clmm_synthetic_full_model_terms.csv\n")
cat("- clmm_synthetic_full_model_comparison.csv\n")
cat("- clmm_synthetic_bootstrap_model_summary.csv\n")
cat("- clmm_synthetic_bootstrap_model_terms.csv\n")
cat("- clmm_synthetic_bootstrap_model_comparison.csv\n")
cat("- clmm_synthetic_bootstrap_significant_terms_summary.csv\n")