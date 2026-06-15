# ==============================================================================
# SCRIPT 20 - CLMM REAL SOBRE BOOTSTRAPS POLÍTICOS
# 4 MACROARQUETIPOS x 3 ETAPAS x 9 DIMENSIONES
# ==============================================================================
# Objetivo:
#   Aplicar modelos CLMM sobre datos reales y sobre las muestras bootstrap
#   políticas generadas previamente en el SCRIPT 19.
#
# Fuentes:
#   - df_clean_general.csv:
#       perfil autoclasificado, macroarquetipo, IDs limpios.
#   - ttm_stage_determinant_vector_wide.csv:
#       etapas, tecnologías y determinantes modelables.
#   - dimension_determinant_mapping_long.csv:
#       relación determinante -> dimensión.
#   - bootstrap_political_profile_draws.csv:
#       draws del bootstrap político.
#
# Cambio metodológico:
#   - Los determinantes se estandarizan antes de construir las dimensiones.
#   - Los valores ausentes / no seleccionados se codifican como 0
#     en escala estandarizada.
#   - 0 significa ausencia de contribución / ausencia de señal.
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

EXCLUDE_UNINTERESTED_REAL <- TRUE

RUN_FULL_MODEL <- TRUE
RUN_POLITICAL_BOOTSTRAPS <- TRUE

#N_BOOTSTRAPS_TO_RUN <- 5
N_BOOTSTRAPS_TO_RUN <- 100

INCLUDE_TECHNOLOGY <- FALSE
SAVE_MODEL_RDS <- FALSE

MODELS_TO_RUN <- c("M0", "M1", "M2_REDUCED", "M2_FULL")

ttm_path <- "initial_descriptive_analysis/output/ttm_stage_analysis/csv/ttm_stage_determinant_vector_wide.csv"
mapping_path <- "initial_descriptive_analysis/output/ttm_stage_analysis/csv/dimension_determinant_mapping_long.csv"
profile_path <- "initial_descriptive_analysis/output/clean_datasets/df_clean_general.csv"

political_draws_path <- "initial_descriptive_analysis/output/bootstrap_political_profile/draws/bootstrap_political_profile_draws.csv"

base_output_dir <- "initial_descriptive_analysis/output/model_4arq_9dim_real_clmm_political_bootstrap_standardized_determinants"

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

clean_text_basic <- function(x) {
  x <- str_squish(as.character(x))
  x <- na_if(x, "")
  x <- na_if(x, "NA")
  x <- na_if(x, "NaN")
  x
}

get_optional_col <- function(data, col_name) {
  if (col_name %in% names(data)) {
    as.character(data[[col_name]])
  } else {
    rep(NA_character_, nrow(data))
  }
}

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
    sample_label == "political_bootstrap" ~ "bootstrap político",
    TRUE ~ sample_label
  )
}

# ==============================================================================
# 4. CONSTRUIR DATASET REAL CLMM
# ==============================================================================

build_real_clmm_dataset <- function() {
  
  df_ttm <- read_csv(ttm_path, show_col_types = FALSE)
  determinant_mapping <- read_csv(mapping_path, show_col_types = FALSE)
  df_profile <- read_csv(profile_path, show_col_types = FALSE)
  
  cat("Datos reales cargados\n")
  cat("Filas ttm_stage_determinant_vector_wide:", nrow(df_ttm), "\n")
  cat("Filas df_clean_general:", nrow(df_profile), "\n")
  
  self_col <- "which_statement_best_describes_you_when_making_an_investment_decision_related_to_your_household_final"
  
  df_self_profile <- df_profile %>%
    mutate(
      participant_id = coalesce(
        as.character(get_optional_col(., "participant_id")),
        as.character(get_optional_col(., "join_key")),
        as.character(get_optional_col(., "prolific_id")),
        as.character(get_optional_col(., "identification_code")),
        as.character(row_number())
      ),
      self_response_raw = clean_text_basic(get_optional_col(., self_col)),
      self_profile = case_when(
        is.na(self_response_raw) ~ NA_character_,
        str_detect(self_response_raw, regex("environmental impact", ignore_case = TRUE)) ~ "Activista",
        str_detect(self_response_raw, regex("safety", ignore_case = TRUE)) ~ "Temeroso/a",
        str_detect(self_response_raw, regex("social status", ignore_case = TRUE)) ~ "Influyente",
        str_detect(self_response_raw, regex("comfort", ignore_case = TRUE)) ~ "Cuidadoso/a",
        str_detect(self_response_raw, regex("not very interested", ignore_case = TRUE)) ~ "Desinteresado/a",
        str_detect(self_response_raw, regex("early adopter", ignore_case = TRUE)) ~ "Pionero",
        str_detect(self_response_raw, regex("ethical", ignore_case = TRUE)) ~ "Sensible",
        str_detect(self_response_raw, regex("cost-effective", ignore_case = TRUE)) ~ "Homo economicus",
        str_detect(self_response_raw, regex("[NΝ]one of the above", ignore_case = TRUE)) ~ NA_character_,
        TRUE ~ NA_character_
      ),
      macro_archetype_5 = case_when(
        self_profile %in% c("Activista", "Sensible") ~ "G1_Activist_Stubborn_Sentient",
        self_profile %in% c("Pionero", "Influyente") ~ "G2_EarlyAdopter_Influencer",
        self_profile %in% c("Temeroso/a", "Cuidadoso/a") ~ "G3_Fearful_Careful",
        self_profile %in% c("Homo economicus") ~ "G4_HomoEconomicus",
        self_profile %in% c("Desinteresado/a") ~ "G5_Uninterested",
        TRUE ~ NA_character_
      )
    ) %>%
    select(participant_id, self_profile, macro_archetype_5) %>%
    distinct(participant_id, .keep_all = TRUE)
  
  determinant_mapping <- determinant_mapping %>%
    filter(is_linked == 1) %>%
    select(dimension_key, determinant_id) %>%
    distinct()
  
  determinant_cols <- intersect(determinant_mapping$determinant_id, names(df_ttm))
  
  if (length(determinant_cols) == 0) {
    stop("No se han encontrado columnas de determinantes en ttm_stage_determinant_vector_wide.csv")
  }
  
  cat("Determinantes detectados para construir dimensiones:", length(determinant_cols), "\n")
  
  required_ttm_cols <- c("participant_id", "stage")
  missing_ttm_cols <- setdiff(required_ttm_cols, names(df_ttm))
  
  if (length(missing_ttm_cols) > 0) {
    stop(
      "Faltan columnas en ttm_stage_determinant_vector_wide.csv: ",
      paste(missing_ttm_cols, collapse = ", ")
    )
  }
  
  if (!"technology" %in% names(df_ttm)) {
    df_ttm <- df_ttm %>%
      mutate(technology = NA_character_)
  }
  
  df_base <- df_ttm %>%
    mutate(
      participant_id = as.character(participant_id),
      stage = as.character(stage),
      technology = as.character(technology)
    ) %>%
    select(
      participant_id,
      stage,
      technology,
      all_of(determinant_cols)
    ) %>%
    mutate(
      across(all_of(determinant_cols), as.numeric)
    ) %>%
    group_by(participant_id, stage, technology) %>%
    summarise(
      across(
        all_of(determinant_cols),
        ~ {
          out <- mean(.x, na.rm = TRUE)
          ifelse(is.nan(out), NA_real_, out)
        }
      ),
      .groups = "drop"
    )
  
  df_base_z <- df_base %>%
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
      z_cols_i <- intersect(z_cols_i, names(df_base_z))
      
      if (length(z_cols_i) == 0) {
        score_i <- rep(0, nrow(df_base_z))
      } else {
        score_i <- rowMeans(
          df_base_z %>% select(all_of(z_cols_i)),
          na.rm = FALSE
        )
      }
      
      score_i[is.na(score_i)] <- 0
      score_i[is.nan(score_i)] <- 0
      score_i[is.infinite(score_i)] <- 0
      
      tibble(!!dim_i := score_i)
    }
  )
  
  df_dimensions <- bind_cols(
    df_base_z %>% select(participant_id, stage, technology),
    dimension_scores
  )
  
  df_real <- df_dimensions %>%
    mutate(
      adoption_stage = case_when(
        str_detect(stage, regex("Implementada|Done|Implemented", ignore_case = TRUE)) ~ "Done",
        str_detect(stage, regex("consideraría|consideraria|Considering|consider", ignore_case = TRUE)) ~ "Considering",
        str_detect(stage, regex("curiosidad|curiosity|Knowledge|No la conoce", ignore_case = TRUE)) ~ "Knowledge",
        TRUE ~ NA_character_
      )
    ) %>%
    left_join(df_self_profile, by = "participant_id") %>%
    filter(
      !is.na(adoption_stage),
      !is.na(macro_archetype_5)
    )
  
  if (EXCLUDE_UNINTERESTED_REAL) {
    df_real <- df_real %>%
      filter(macro_archetype_5 != "G5_Uninterested")
  }
  
  df_real <- df_real %>%
    select(
      participant_id,
      macro_archetype_5,
      self_profile,
      adoption_stage,
      stage,
      technology,
      all_of(dimensions)
    )
  
  write_csv(
    df_real,
    file.path(csv_dir, "clmm_real_input_4arq_9dim_standardized_determinants.csv")
  )
  
  participant_counts <- df_real %>%
    distinct(participant_id, macro_archetype_5, self_profile) %>%
    count(macro_archetype_5, self_profile, name = "n_participantes") %>%
    arrange(macro_archetype_5, desc(n_participantes))
  
  cell_counts <- df_real %>%
    count(macro_archetype_5, adoption_stage, name = "n_filas") %>%
    arrange(macro_archetype_5, adoption_stage)
  
  cell_counts_wide <- cell_counts %>%
    pivot_wider(
      names_from = adoption_stage,
      values_from = n_filas,
      values_fill = 0
    )
  
  dimension_diagnostics <- df_real %>%
    summarise(
      across(
        all_of(dimensions),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd = ~ sd(.x, na.rm = TRUE),
          min = ~ min(.x, na.rm = TRUE),
          max = ~ max(.x, na.rm = TRUE),
          n_zero = ~ sum(.x == 0, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )
    )
  
  write_csv(participant_counts, file.path(csv_dir, "clmm_real_macroarchetype_participant_counts.csv"))
  write_csv(cell_counts, file.path(csv_dir, "clmm_real_cell_counts_4arq_3stage.csv"))
  write_csv(cell_counts_wide, file.path(csv_dir, "clmm_real_cell_counts_4arq_3stage_wide.csv"))
  write_csv(dimension_diagnostics, file.path(csv_dir, "clmm_real_dimension_diagnostics_standardized_determinants.csv"))
  
  cat("\nParticipantes por macroarquetipo y perfil:\n")
  print(participant_counts, n = Inf)
  
  cat("\nFilas por macroarquetipo x etapa:\n")
  print(cell_counts_wide, n = Inf)
  
  cat("\nDataset real CLMM creado:\n")
  cat("Filas:", nrow(df_real), "\n")
  cat("Participantes:", n_distinct(df_real$participant_id), "\n")
  
  df_real
}

# ==============================================================================
# 5. CARGAR DATOS
# ==============================================================================

df_full <- build_real_clmm_dataset()

if (!file.exists(political_draws_path)) {
  stop("No existe el archivo de draws del bootstrap político: ", political_draws_path)
}

political_draws <- read_csv(
  political_draws_path,
  show_col_types = FALSE
) %>%
  mutate(
    bootstrap_id = as.integer(bootstrap_id),
    bootstrap_draw_id = as.integer(bootstrap_draw_id),
    participant_id_original = as.character(participant_id_original),
    participant_id_bootstrap = as.character(participant_id_bootstrap),
    political_profile = as.character(political_profile)
  )

cat("\nDatos cargados correctamente\n")
cat("Filas dataset real CLMM:", nrow(df_full), "\n")
cat("Participantes dataset real CLMM:", n_distinct(df_full$participant_id), "\n")
cat("Filas draws políticos:", nrow(political_draws), "\n")
cat("Bootstraps políticos disponibles:", n_distinct(political_draws$bootstrap_id), "\n")

# ==============================================================================
# 6. PREPARAR CLMM
# ==============================================================================

prepare_clmm_data <- function(data_i) {
  
  if (!"technology" %in% names(data_i)) {
    data_i <- data_i %>%
      mutate(technology = NA_character_)
  }
  
  macro_levels <- if (EXCLUDE_UNINTERESTED_REAL) {
    c(
      "G1_Activist_Stubborn_Sentient",
      "G2_EarlyAdopter_Influencer",
      "G3_Fearful_Careful",
      "G4_HomoEconomicus"
    )
  } else {
    c(
      "G1_Activist_Stubborn_Sentient",
      "G2_EarlyAdopter_Influencer",
      "G3_Fearful_Careful",
      "G4_HomoEconomicus",
      "G5_Uninterested"
    )
  }
  
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
# 7. MODELOS SOBRE MUESTRA COMPLETA
# ==============================================================================

df_full_clmm <- prepare_clmm_data(df_full)

write_csv(
  df_full_clmm,
  file.path(csv_dir, "clmm_real_model_dataset_prepared_full.csv")
)

cat("\nDataset completo preparado para CLMM:\n")
cat("Filas:", nrow(df_full_clmm), "\n")
cat("Participantes:", n_distinct(df_full_clmm$participant_id), "\n")

if (RUN_FULL_MODEL) {
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
} else {
  full_model_summary <- tibble()
  full_model_terms <- tibble()
}

write_csv(full_model_summary, file.path(csv_dir, "clmm_full_model_summary.csv"))
write_csv(full_model_terms, file.path(csv_dir, "clmm_full_model_terms.csv"))

cat("\nResumen modelos muestra completa:\n")
print(full_model_summary, n = Inf)

# ==============================================================================
# 8. MODELOS SOBRE BOOTSTRAPS POLÍTICOS
# ==============================================================================

if (RUN_POLITICAL_BOOTSTRAPS) {
  
  model_participants <- df_full %>%
    distinct(participant_id) %>%
    mutate(participant_id = as.character(participant_id))
  
  draw_match_diagnostic <- political_draws %>%
    distinct(participant_id_original) %>%
    mutate(
      in_model_dataset = participant_id_original %in% model_participants$participant_id
    ) %>%
    count(in_model_dataset, name = "n_participants")
  
  write_csv(
    draw_match_diagnostic,
    file.path(csv_dir, "clmm_political_bootstrap_match_diagnostic.csv")
  )
  
  cat("\nDiagnóstico de emparejamiento bootstrap político x dataset CLMM:\n")
  print(draw_match_diagnostic)
  
  bootstrap_ids_available <- sort(unique(political_draws$bootstrap_id))
  
  bootstrap_ids_to_run <- bootstrap_ids_available[
    seq_len(min(N_BOOTSTRAPS_TO_RUN, length(bootstrap_ids_available)))
  ]
  
  make_clmm_dataset_from_political_draws <- function(boot_id_i) {
    
    selected_draws <- political_draws %>%
      filter(bootstrap_id == boot_id_i)
    
    selected_draws %>%
      left_join(
        df_full %>% mutate(participant_id = as.character(participant_id)),
        by = c("participant_id_original" = "participant_id"),
        relationship = "many-to-many"
      ) %>%
      filter(!is.na(macro_archetype_5)) %>%
      mutate(
        participant_id = participant_id_bootstrap
      ) %>%
      select(
        bootstrap_id,
        bootstrap_draw_id,
        participant_id,
        participant_id_original,
        political_profile,
        everything(),
        -participant_id_bootstrap
      )
  }
  
  bootstrap_join_diagnostic <- map_dfr(
    bootstrap_ids_to_run,
    function(b_i) {
      make_clmm_dataset_from_political_draws(b_i) %>%
        distinct(
          bootstrap_id,
          bootstrap_draw_id,
          participant_id,
          participant_id_original,
          political_profile
        ) %>%
        count(
          bootstrap_id,
          political_profile,
          name = "n_participants_after_join"
        )
    }
  ) %>%
    group_by(bootstrap_id) %>%
    mutate(
      prop_after_join = n_participants_after_join / sum(n_participants_after_join)
    ) %>%
    ungroup()
  
  bootstrap_join_summary <- bootstrap_join_diagnostic %>%
    group_by(political_profile) %>%
    summarise(
      mean_n = mean(n_participants_after_join),
      min_n = min(n_participants_after_join),
      max_n = max(n_participants_after_join),
      mean_prop = mean(prop_after_join),
      min_prop = min(prop_after_join),
      max_prop = max(prop_after_join),
      .groups = "drop"
    )
  
  write_csv(
    bootstrap_join_diagnostic,
    file.path(csv_dir, "clmm_political_bootstrap_after_join_diagnostic.csv")
  )
  
  write_csv(
    bootstrap_join_summary,
    file.path(csv_dir, "clmm_political_bootstrap_after_join_summary.csv")
  )
  
  cat("\nComposición política tras unir bootstrap con dataset CLMM:\n")
  print(bootstrap_join_summary, n = Inf)
  
  fit_one_political_bootstrap <- function(b_i) {
    
    cat("\n==============================\n")
    cat("Bootstrap político:", b_i, "de", length(bootstrap_ids_to_run), "\n")
    cat("==============================\n")
    
    data_i <- make_clmm_dataset_from_political_draws(b_i)
    data_i <- prepare_clmm_data(data_i)
    
    write_csv(
      data_i,
      file.path(
        bootstrap_dir,
        paste0("clmm_political_bootstrap_prepared_boot_", b_i, ".csv")
      )
    )
    
    map(
      MODELS_TO_RUN,
      function(model_type_i) {
        fit_clmm_safe(
          data_i = data_i,
          model_type = model_type_i,
          sample_label = "political_bootstrap",
          n_participants = n_distinct(data_i$participant_id),
          boot_id = b_i
        )
      }
    )
  }
  
  bootstrap_results_nested <- map(bootstrap_ids_to_run, fit_one_political_bootstrap)
  bootstrap_results_flat <- flatten(bootstrap_results_nested)
  
  bootstrap_model_summary <- map_dfr(bootstrap_results_flat, "summary")
  bootstrap_model_terms <- map_dfr(bootstrap_results_flat, "terms")
  
} else {
  bootstrap_model_summary <- tibble()
  bootstrap_model_terms <- tibble()
}

write_csv(
  bootstrap_model_summary,
  file.path(csv_dir, "clmm_political_bootstrap_model_summary.csv")
)

write_csv(
  bootstrap_model_terms,
  file.path(csv_dir, "clmm_political_bootstrap_model_terms.csv")
)

cat("\nResumen modelos bootstrap político:\n")
print(bootstrap_model_summary, n = Inf)

# ==============================================================================
# 9. COMPARACIONES Y TÉRMINOS
# ==============================================================================

if (nrow(full_model_summary) > 0) {
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
    file.path(csv_dir, "clmm_full_model_comparison.csv")
  )
}

if (nrow(bootstrap_model_summary) > 0) {
  
  model_comparison_bootstrap <- bootstrap_model_summary %>%
    group_by(model_type, model_label) %>%
    summarise(
      n_boot = n_distinct(boot_id),
      n_ok = sum(status == "ok"),
      convergence_rate = n_ok / n_boot,
      mean_n_participants = mean(n_participants, na.rm = TRUE),
      min_n_participants = min(n_participants, na.rm = TRUE),
      max_n_participants = max(n_participants, na.rm = TRUE),
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
    arrange(mean_AIC)
  
  write_csv(
    model_comparison_bootstrap,
    file.path(csv_dir, "clmm_political_bootstrap_model_comparison.csv")
  )
  
  significant_terms_summary <- bootstrap_model_terms %>%
    filter(!is.na(p.value)) %>%
    group_by(
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
        count(model_type, model_label, name = "n_ok_models"),
      by = c("model_type", "model_label")
    ) %>%
    mutate(
      significance_rate = n_significant / n_ok_models,
      conditional_significance_rate = n_significant / n_boot_with_term,
      term_presence_rate = n_boot_with_term / n_ok_models
    ) %>%
    arrange(model_type, desc(significance_rate), mean_p_value)
  
  write_csv(
    significant_terms_summary,
    file.path(csv_dir, "clmm_political_bootstrap_significant_terms_summary.csv")
  )
}
cat("Archivos creados en:\n")
cat(csv_dir, "\n")