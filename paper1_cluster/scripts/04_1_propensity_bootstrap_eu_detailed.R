
# Objetivo:
# 1. Coger el dataset final integrado y enriquecido.
# 2. Entrenar un modelo de propensión al voto/abstención usando RV.
# 3. Predecir probabilidad de voto y abstención para todo el dataset.
# 4. Clasificar filas RV según la lógica del Excel de Cruz:
#    - 4.1 tipo de voto: nacional, regionalista, otros, abstención.
#    - 4.2 eje izquierda-derecha.
# 5. Generar muestras bootstrap postestratificadas:
#    - bloque abstencionista según p_abstention_predicted
#    - bloque votante según pesos electorales externos.
#

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(stringr)
})

set.seed(123)


# Parámetros editables
processed_root <- "paper1_cluster/data/processed"

in_file <- file.path(
  processed_root,
  "03_2_phase_dimension_scores",
  "all_sources_integrated_component_quality_phase_scores.csv"
)

out_dir <- file.path(
  processed_root,
  "04_1_propensity_bootstrap_eu_detailed"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Modelo de propensión
# Usamos variables comunes y razonablemente disponibles en RV, WHY y Diego.
# No usamos income ni city_size por defecto porque no están disponibles de forma transversal.
PROPENSITY_PREDICTORS <- c(
  "age_group_model",
  "gender_model",
  "country_model_grouped",
  "employment_model"
)

# Si TRUE, el modelo se entrena solo con filas RV que tienen todos los predictores completos.
# Si FALSE, se entrenan también filas con algún predictor missing, usando categoría UNKNOWN_MODEL.
TRAIN_ONLY_COMPLETE_PREDICTORS <- FALSE

# Agrupar niveles raros en el modelo para evitar problemas de sobreajuste.
MIN_LEVEL_N_MODEL <- 8

# Probabilidades mínimas/máximas para evitar pesos infinitos.
P_MIN <- 0.01
P_MAX <- 0.99

# Target electoral externo.
# Según el Excel de Cruz:
# Abstención ≈ 50.65%
# Votantes ≈ 49.35%
#
# Si quieres forzar exactamente 50/50, cambia esto a 0.50.
TARGET_ABSTENTION_SHARE <- 0.5065
TARGET_VOTER_SHARE <- 1 - TARGET_ABSTENTION_SHARE

# Bootstrap
#N_BOOT <- 200
N_BOOT <- 1000

# Tamaño de cada muestra bootstrap.
# Puedes cambiarlo a 100 si quieres muestras tipo "tabla de 100".
# Puedes cambiarlo a 3233 si quieres tamaño igual a la matriz usable.
BOOT_SAMPLE_SIZE <- 1000


# Si TRUE, el bootstrap solo usa filas usables para análisis principal.
# Si FALSE, el bootstrap político no filtra por calidad, pero conserva
# las columnas de calidad en la muestra final.
USE_QUALITY_FILTER_IN_BOOTSTRAP <- FALSE

# Recorte de pesos.
# Ningún peso de muestreo puede superar MAX_WEIGHT_MULTIPLIER veces el peso medio.
MAX_WEIGHT_MULTIPLIER <- 5

# Para el bloque abstencionista:
# - "proportional": probabilidad proporcional a p_abstention_predicted.
# - "inverse_vote": probabilidad proporcional a 1 / p_vote_predicted.
#
# En la práctica son muy parecidos si p_abstention = 1 - p_vote.
ABSTENTION_SAMPLING_MODE <- "proportional"

# Para filas 4.1 = "I usually switch my vote..."
# El Excel de Cruz no tiene categoría explícita para "switch".
# Por defecto las asignamos por eje izquierda-derecha como si fueran "national".
# Otras opciones posibles: "other", "exclude".
SWITCH_HANDLING <- "national_by_lr"

# Guardar dataset completo de propensity.
SAVE_FULL_PROPENSITY_DATASET <- TRUE

# Guardar índice de bootstrap.
# Esto guarda solo IDs y columnas compactas, no las 1400 columnas repetidas 1000 veces.
SAVE_BOOTSTRAP_INDEX <- TRUE

# Guardar dataset bootstrap completo.
# Cuidado: puede ser enorme.
SAVE_FULL_BOOTSTRAP_DATASET <- FALSE


# Lectura
if (!file.exists(in_file)) {
  stop("No encuentro el dataset final: ", in_file)
}

df <- read_csv(
  in_file,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)


# Funciones auxiliares
clean_text <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  
  x <- na_if(x, "")
  x <- na_if(x, "NA")
  x <- na_if(x, "NaN")
  x <- na_if(x, "NULL")
  x <- na_if(x, "null")
  x <- na_if(x, "None")
  x <- na_if(x, "none")
  
  x
}

as_num <- function(x) {
  suppressWarnings(readr::parse_number(as.character(x)))
}

is_true <- function(x) {
  as.character(x) %in% c("TRUE", "True", "true", "1")
}

is_valid_predictor_value <- function(x) {
  x <- clean_text(x)
  x_low <- str_to_lower(x)
  
  !is.na(x) &
    !x_low %in% c(
      "unknown",
      "conflict",
      "invalid",
      "na",
      "nan",
      "none"
    )
}

clean_model_category <- function(x) {
  x <- clean_text(x)
  x_low <- str_to_lower(x)
  
  x <- ifelse(
    is.na(x) |
      x_low %in% c("unknown", "conflict", "invalid", "na", "nan", "none"),
    "UNKNOWN_MODEL",
    x
  )
  
  x <- str_to_upper(as.character(x))
  x <- str_replace_all(x, "[^A-Z0-9_]+", "_")
  x <- str_squish(x)
  x
}

prepare_factor_mapping <- function(train_x, all_x, predictor_name, min_level_n = 8) {
  
  train_clean <- clean_model_category(train_x)
  all_clean <- clean_model_category(all_x)
  
  counts <- sort(table(train_clean), decreasing = TRUE)
  
  if (length(counts) == 0) {
    stop("No hay niveles en el predictor: ", predictor_name)
  }
  
  kept_levels <- names(counts[counts >= min_level_n])
  
  # Si el umbral deja menos de dos niveles, relajamos y dejamos todos los niveles observados.
  if (length(kept_levels) < 2) {
    kept_levels <- names(counts)
  }
  
  reference_level <- names(counts)[1]
  
  train_out <- ifelse(
    train_clean %in% kept_levels,
    train_clean,
    "OTHER_MODEL"
  )
  
  all_out <- ifelse(
    all_clean %in% kept_levels,
    all_clean,
    "OTHER_MODEL"
  )
  
  # Si OTHER_MODEL no existe en training, cualquier valor nuevo en predicción
  # se manda al nivel de referencia para evitar errores de predict().
  if (!any(train_out == "OTHER_MODEL")) {
    all_out[all_out == "OTHER_MODEL"] <- reference_level
  }
  
  levels_final <- unique(train_out)
  
  mapping <- tibble(
    predictor = predictor_name,
    raw_training_level = names(counts),
    n_training = as.integer(counts),
    kept_in_model = raw_training_level %in% kept_levels,
    model_level = ifelse(
      raw_training_level %in% kept_levels,
      raw_training_level,
      "OTHER_MODEL"
    ),
    reference_level = reference_level
  )
  
  list(
    train = factor(train_out, levels = levels_final),
    all = factor(all_out, levels = levels_final),
    mapping = mapping
  )
}

trim_weights <- function(w, max_multiplier = 5) {
  w <- as.numeric(w)
  w[is.na(w) | w < 0] <- 0
  
  if (sum(w, na.rm = TRUE) == 0) {
    return(rep(1, length(w)))
  }
  
  positive_w <- w[w > 0]
  
  if (length(positive_w) == 0) {
    return(rep(1, length(w)))
  }
  
  cap <- mean(positive_w, na.rm = TRUE) * max_multiplier
  pmin(w, cap)
}

make_sampling_prob <- function(w, max_multiplier = 5) {
  w <- trim_weights(w, max_multiplier = max_multiplier)
  
  if (sum(w, na.rm = TRUE) == 0) {
    return(rep(1 / length(w), length(w)))
  }
  
  w / sum(w, na.rm = TRUE)
}

allocate_counts <- function(shares, n_total) {
  raw <- shares * n_total
  base <- floor(raw)
  remainder <- n_total - sum(base)
  
  if (remainder > 0) {
    frac_order <- order(raw - base, decreasing = TRUE)
    base[frac_order[seq_len(remainder)]] <- base[frac_order[seq_len(remainder)]] + 1
  }
  
  as.integer(base)
}

# Comprobaciones mínimas
required_cols <- c(
  "integrated_row_id",
  "dataset_source",
  "source_survey",
  "usable_for_main_analysis",
  "voted_observed",
  "vote_status_declared",
  "vote_raw_clean",
  "political_left_right_model",
  PROPENSITY_PREDICTORS
)

missing_cols <- required_cols[!required_cols %in% names(df)]

if (length(missing_cols) > 0) {
  stop(
    "Faltan columnas necesarias:\n",
    paste(missing_cols, collapse = "\n")
  )
}

df <- df %>%
  mutate(
    .row_index = row_number(),
    voted_observed_num = as_num(voted_observed),
    usable_for_main_analysis_bool = is_true(usable_for_main_analysis)
  )

# Preparar datos para el modelo de propensión
predictor_missing_matrix <- map_dfc(
  PROPENSITY_PREDICTORS,
  function(pred) {
    tibble(
      !!paste0("missing_", pred) := !is_valid_predictor_value(df[[pred]])
    )
  }
)

df <- bind_cols(df, predictor_missing_matrix) %>%
  mutate(
    n_missing_propensity_predictors = rowSums(
      across(starts_with("missing_"), ~ .x == TRUE),
      na.rm = TRUE
    ),
    complete_propensity_predictors = n_missing_propensity_predictors == 0
  )

training_base <- df %>%
  filter(
    dataset_source == "rv",
    voted_observed_num %in% c(0, 1)
  )

if (TRAIN_ONLY_COMPLETE_PREDICTORS) {
  training_base <- training_base %>%
    filter(complete_propensity_predictors == TRUE)
}

if (nrow(training_base) < 30) {
  stop(
    "Hay muy pocas filas para entrenar el modelo de propensión: ",
    nrow(training_base)
  )
}

if (n_distinct(training_base$voted_observed_num) < 2) {
  stop("El training set no tiene votantes y abstencionistas. No se puede ajustar glm binomial.")
}

train_model_df <- tibble(
  integrated_row_id = training_base$integrated_row_id,
  voted_observed_num = training_base$voted_observed_num
)

all_model_df <- tibble(
  integrated_row_id = df$integrated_row_id
)

factor_mappings <- list()

for (pred in PROPENSITY_PREDICTORS) {
  prepared <- prepare_factor_mapping(
    train_x = training_base[[pred]],
    all_x = df[[pred]],
    predictor_name = pred,
    min_level_n = MIN_LEVEL_N_MODEL
  )
  
  new_col <- paste0("f_", pred)
  
  train_model_df[[new_col]] <- prepared$train
  all_model_df[[new_col]] <- prepared$all
  factor_mappings[[pred]] <- prepared$mapping
}

factor_mapping_table <- bind_rows(factor_mappings)

candidate_model_predictors <- names(train_model_df)[
  str_starts(names(train_model_df), "f_")
]

# Quitamos predictores sin variación en training.
model_predictors <- candidate_model_predictors[
  map_int(train_model_df[candidate_model_predictors], n_distinct) >= 2
]

if (length(model_predictors) == 0) {
  stop("No queda ningún predictor con variación suficiente para el modelo.")
}

propensity_formula <- as.formula(
  paste(
    "voted_observed_num ~",
    paste(model_predictors, collapse = " + ")
  )
)


# Ajustar modelo de propensión
propensity_fit <- glm(
  propensity_formula,
  data = train_model_df,
  family = binomial()
)

p_vote_predicted <- predict(
  propensity_fit,
  newdata = all_model_df,
  type = "response"
)

p_vote_predicted <- pmin(P_MAX, pmax(P_MIN, p_vote_predicted))
p_abstention_predicted <- 1 - p_vote_predicted

propensity_model_coefficients <- coef(summary(propensity_fit)) %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  as_tibble() %>%
  rename(
    estimate = Estimate,
    std_error = `Std. Error`,
    z_value = `z value`,
    p_value = `Pr(>|z|)`
  )


# Añadir predicciones y pesos de propensión
df_propensity <- df %>%
  mutate(
    propensity_model_formula = as.character(propensity_formula)[3],
    propensity_training_row = integrated_row_id %in% training_base$integrated_row_id,
    
    p_vote_predicted = p_vote_predicted,
    p_abstention_predicted = p_abstention_predicted,
    
    propensity_prediction_quality = case_when(
      !usable_for_main_analysis_bool ~ "not_usable_for_main_analysis",
      complete_propensity_predictors ~ "predicted_complete_predictors",
      n_missing_propensity_predictors == 1 ~ "predicted_one_missing_predictor",
      n_missing_propensity_predictors > 1 ~ "predicted_multiple_missing_predictors",
      TRUE ~ "review"
    ),
    
    observed_vote_status_binary = case_when(
      voted_observed_num == 1 ~ "observed_voter",
      voted_observed_num == 0 ~ "observed_abstainer",
      TRUE ~ NA_character_
    ),
    
    propensity_weight_raw = case_when(
      voted_observed_num == 1 ~ TARGET_VOTER_SHARE / p_vote_predicted,
      voted_observed_num == 0 ~ TARGET_ABSTENTION_SHARE / p_abstention_predicted,
      TRUE ~ NA_real_
    )
  )

propensity_weight_trimmed <- trim_weights(
  df_propensity$propensity_weight_raw,
  max_multiplier = MAX_WEIGHT_MULTIPLIER
)

df_propensity <- df_propensity %>%
  mutate(
    propensity_weight_trimmed = propensity_weight_trimmed
  )

# Clasificación electoral según RV 4.1 + 4.2
recode_vote_approach_41 <- function(vote_raw, vote_status) {
  vote_raw <- clean_text(vote_raw)
  vote_status <- clean_text(vote_status)
  
  text <- str_to_lower(paste(vote_raw, vote_status, sep = " "))
  
  case_when(
    str_detect(text, "do not vote|blank|null|abstain|abstention|abstainer|no voto|blanco|nulo") ~ "abstention",
    str_detect(text, "pro-independence|regionalist|regional|independence") ~ "regionalist",
    str_detect(text, "national parties|national party|national") ~ "national",
    str_detect(text, "other options|other option") ~ "other",
    str_detect(text, "switch|depending|depends|candidate|program|programme") ~ "switch",
    str_detect(text, "\\bvoter\\b|usually vote|always vote") ~ "national_unknown",
    TRUE ~ NA_character_
  )
}

classify_electoral_group <- function(vote_approach, lr, switch_handling = "national_by_lr") {
  
  vote_approach <- as.character(vote_approach)
  lr <- as.numeric(lr)
  
  # Tratamiento de switch
  vote_for_classification <- case_when(
    vote_approach == "switch" & switch_handling == "national_by_lr" ~ "national",
    vote_approach == "switch" & switch_handling == "other" ~ "other",
    vote_approach == "switch" & switch_handling == "exclude" ~ NA_character_,
    vote_approach == "national_unknown" ~ "national",
    TRUE ~ vote_approach
  )
  
  case_when(
    vote_for_classification == "abstention" ~ "ABSTENTION",
    
    vote_for_classification == "regionalist" & !is.na(lr) & lr < 40 ~ "GREENS_EFA",
    vote_for_classification == "regionalist" & !is.na(lr) & lr >= 40 ~ "RENEW",
    
    vote_for_classification == "other" ~ "NI",
    
    vote_for_classification == "national" & !is.na(lr) & lr < 20 ~ "GUE_NGL",
    vote_for_classification == "national" & !is.na(lr) & lr >= 20 & lr < 40 ~ "SD",
    vote_for_classification == "national" & !is.na(lr) & lr >= 40 & lr < 60 ~ "EPP",
    vote_for_classification == "national" & !is.na(lr) & lr >= 60 & lr < 80 ~ "ECR",
    vote_for_classification == "national" & !is.na(lr) & lr >= 80 & lr < 90 ~ "PFE",
    vote_for_classification == "national" & !is.na(lr) & lr >= 90 ~ "ESN",
    
    TRUE ~ NA_character_
  )
}

df_propensity <- df_propensity %>%
  mutate(
    vote_approach_41_model = recode_vote_approach_41(
      vote_raw_clean,
      vote_status_declared
    ),
    
    political_left_right_num = as_num(political_left_right_model),
    
    electoral_group_model = classify_electoral_group(
      vote_approach = vote_approach_41_model,
      lr = political_left_right_num,
      switch_handling = SWITCH_HANDLING
    ),
    
    electoral_group_quality = case_when(
      electoral_group_model == "ABSTENTION" ~ "classified_abstention",
      !is.na(electoral_group_model) ~ "classified_voter_group",
      dataset_source != "rv" ~ "not_available_outside_rv",
      is.na(vote_approach_41_model) ~ "missing_vote_approach_41",
      is.na(political_left_right_num) & vote_approach_41_model != "abstention" ~ "missing_left_right_position",
      TRUE ~ "not_classified"
    )
  )


# 9. Tabla de targets electorales de Cruz

# IMPORTANTE:
# Esta tabla está hardcodeada a partir del Excel de Cruz.
# Si Cruz cambia pesos, solo hay que modificar este bloque.
#
# external_weight_raw representa el peso relativo de cada grupo votante.
# Luego el script lo normaliza para que todos los votantes sumen TARGET_VOTER_SHARE.
#
# La abstención se fija directamente en TARGET_ABSTENTION_SHARE.

electoral_targets_voters_raw <- tribble(
  ~target_electoral_group, ~target_label, ~rv_41_rule, ~rv_42_rule, ~external_weight_raw,
  
  "GUE_NGL",
  "The Left in the European Parliament - GUE/NGL",
  "national",
  "x < 20",
  3.36,
  
  "GREENS_EFA",
  "Greens / European Free Alliance",
  "regionalist",
  "x < 40",
  3.63,
  
  "SD",
  "Progressive Alliance of Socialists and Democrats",
  "national",
  "20 <= x < 40",
  10.21,
  
  "RENEW",
  "Renew Europe",
  "regionalist",
  "x >= 40",
  5.48,
  
  "EPP",
  "European People's Party",
  "national",
  "40 <= x < 60",
  12.68,
  
  "ECR",
  "European Conservatives and Reformists",
  "national",
  "60 <= x < 80",
  5.55,
  
  "PFE",
  "Patriots for Europe",
  "national",
  "80 <= x < 90",
  5.83,
  
  "ESN",
  "Europe of Sovereign Nations",
  "national",
  "x >= 90",
  2.26,
  
  "NI",
  "Non-attached / Other options",
  "other",
  "any",
  2.40
)

electoral_targets_voters <- electoral_targets_voters_raw %>%
  mutate(
    target_sample_type = "voter",
    target_share_within_voters = external_weight_raw / sum(external_weight_raw),
    target_share_global = target_share_within_voters * TARGET_VOTER_SHARE
  )

electoral_target_abstention <- tibble(
  target_electoral_group = "ABSTENTION",
  target_label = "Abstention / blank / null / no vote",
  rv_41_rule = "abstention",
  rv_42_rule = "not used",
  external_weight_raw = NA_real_,
  target_sample_type = "abstention",
  target_share_within_voters = NA_real_,
  target_share_global = TARGET_ABSTENTION_SHARE
)

electoral_targets_final <- bind_rows(
  electoral_targets_voters,
  electoral_target_abstention
) %>%
  arrange(desc(target_sample_type), desc(target_share_global))

# Conteos objetivo por muestra bootstrap
n_abstention_target <- round(BOOT_SAMPLE_SIZE * TARGET_ABSTENTION_SHARE)
n_voter_target <- BOOT_SAMPLE_SIZE - n_abstention_target

voter_group_counts <- allocate_counts(
  electoral_targets_voters$target_share_within_voters,
  n_voter_target
)

electoral_targets_voters <- electoral_targets_voters %>%
  mutate(
    target_n_in_bootstrap = voter_group_counts
  )

electoral_target_abstention <- electoral_target_abstention %>%
  mutate(
    target_n_in_bootstrap = n_abstention_target
  )

electoral_targets_final <- bind_rows(
  electoral_targets_voters,
  electoral_target_abstention
) %>%
  arrange(desc(target_sample_type), desc(target_share_global))


# Preparar pesos de muestreo para bootstrap
df_propensity <- df_propensity %>%
  mutate(
        bootstrap_eligible = case_when(
          USE_QUALITY_FILTER_IN_BOOTSTRAP ~ usable_for_main_analysis_bool &
            !is.na(p_vote_predicted) &
            !is.na(p_abstention_predicted),
          
          !USE_QUALITY_FILTER_IN_BOOTSTRAP ~ !is.na(p_vote_predicted) &
            !is.na(p_abstention_predicted)
        ),
    
    abstention_sampling_weight_raw = case_when(
      ABSTENTION_SAMPLING_MODE == "proportional" ~ p_abstention_predicted,
      ABSTENTION_SAMPLING_MODE == "inverse_vote" ~ 1 / p_vote_predicted,
      TRUE ~ p_abstention_predicted
    ),
    
    abstention_sampling_weight_trimmed = trim_weights(
      abstention_sampling_weight_raw,
      max_multiplier = MAX_WEIGHT_MULTIPLIER
    ),
    
    voter_sampling_weight_raw = p_vote_predicted,
    
    voter_sampling_weight_trimmed = trim_weights(
      voter_sampling_weight_raw,
      max_multiplier = MAX_WEIGHT_MULTIPLIER
    ),
    
    voter_electoral_pool = bootstrap_eligible &
      !is.na(electoral_group_model) &
      electoral_group_model != "ABSTENTION"
  )


# Funciones de bootstrap
sample_pool <- function(pool, n_draw, weight_col, bootstrap_id, target_type, target_group, fallback_used = FALSE) {
  
  if (nrow(pool) == 0) {
    stop("Pool vacío para grupo: ", target_group)
  }
  
  w <- pool[[weight_col]]
  prob <- make_sampling_prob(w, max_multiplier = MAX_WEIGHT_MULTIPLIER)
  
  idx <- sample(
    seq_len(nrow(pool)),
    size = n_draw,
    replace = TRUE,
    prob = prob
  )
  
  pool[idx, ] %>%
    transmute(
      bootstrap_id = bootstrap_id,
      target_sample_type = target_type,
      target_electoral_group = target_group,
      fallback_used = fallback_used,
      integrated_row_id,
      dataset_source,
      source_survey,
      row_quality_final,
      usable_for_main_analysis,
      usable_for_clustering,
      n_det_valid,
      bootstrap_eligible,
      p_vote_predicted,
      p_abstention_predicted,
      vote_approach_41_model,
      political_left_right_num,
      electoral_group_model,
      electoral_group_quality
    )
}

draw_one_bootstrap <- function(bootstrap_id, data, targets_voters) {
  
  # 1. Bloque abstencionista
  abstention_pool <- data %>%
    filter(bootstrap_eligible == TRUE)
  
  abstention_draw <- sample_pool(
    pool = abstention_pool,
    n_draw = n_abstention_target,
    weight_col = "abstention_sampling_weight_trimmed",
    bootstrap_id = bootstrap_id,
    target_type = "abstention",
    target_group = "ABSTENTION",
    fallback_used = FALSE
  )
  
  # 2. Bloque votante por grupos electorales
  voter_pool_all <- data %>%
    filter(voter_electoral_pool == TRUE)
  
  if (nrow(voter_pool_all) == 0) {
    stop("No hay pool votante electoral. Revisa electoral_group_model.")
  }
  
  voter_draws <- map2_dfr(
    targets_voters$target_electoral_group,
    targets_voters$target_n_in_bootstrap,
    function(group, n_group) {
      
      group_pool <- voter_pool_all %>%
        filter(electoral_group_model == group)
      
      fallback_used <- FALSE
      
      # Si un grupo está vacío, usamos fallback a todo el pool votante.
      # Esto queda marcado en fallback_used.
      if (nrow(group_pool) == 0) {
        group_pool <- voter_pool_all
        fallback_used <- TRUE
      }
      
      sample_pool(
        pool = group_pool,
        n_draw = n_group,
        weight_col = "voter_sampling_weight_trimmed",
        bootstrap_id = bootstrap_id,
        target_type = "voter",
        target_group = group,
        fallback_used = fallback_used
      )
    }
  )
  
  bind_rows(
    abstention_draw,
    voter_draws
  ) %>%
    group_by(bootstrap_id) %>%
    mutate(draw_id = row_number()) %>%
    ungroup() %>%
    relocate(bootstrap_id, draw_id)
}


# Ejecutar bootstrap
bootstrap_index <- map_dfr(
  seq_len(N_BOOT),
  ~ draw_one_bootstrap(
    bootstrap_id = .x,
    data = df_propensity,
    targets_voters = electoral_targets_voters
  )
)

# Diagnósticos
diagnostics_propensity_training <- tibble(
  metric = c(
    "n_training_rows",
    "n_training_voters",
    "n_training_abstainers",
    "training_only_complete_predictors",
    "use_quality_filter_in_bootstrap",
    "n_model_predictors",
    "target_abstention_share",
    "target_voter_share",
    "n_boot",
    "boot_sample_size",
    "n_abstention_target_per_bootstrap",
    "n_voter_target_per_bootstrap"
  ),
  value = c(
    nrow(training_base),
    sum(training_base$voted_observed_num == 1, na.rm = TRUE),
    sum(training_base$voted_observed_num == 0, na.rm = TRUE),
    as.character(TRAIN_ONLY_COMPLETE_PREDICTORS),
    as.character(USE_QUALITY_FILTER_IN_BOOTSTRAP),
    length(model_predictors),
    TARGET_ABSTENTION_SHARE,
    TARGET_VOTER_SHARE,
    N_BOOT,
    BOOT_SAMPLE_SIZE,
    n_abstention_target,
    n_voter_target
  )
)

diagnostics_propensity_predictions_by_source <- df_propensity %>%
  group_by(dataset_source) %>%
  summarise(
    n_rows = n(),
    mean_p_vote = mean(p_vote_predicted, na.rm = TRUE),
    sd_p_vote = sd(p_vote_predicted, na.rm = TRUE),
    min_p_vote = min(p_vote_predicted, na.rm = TRUE),
    max_p_vote = max(p_vote_predicted, na.rm = TRUE),
    mean_p_abstention = mean(p_abstention_predicted, na.rm = TRUE),
    n_predicted_complete = sum(propensity_prediction_quality == "predicted_complete_predictors", na.rm = TRUE),
    n_predicted_partial = sum(str_detect(propensity_prediction_quality, "missing"), na.rm = TRUE),
    n_not_usable = sum(propensity_prediction_quality == "not_usable_for_main_analysis", na.rm = TRUE),
    .groups = "drop"
  )

diagnostics_electoral_group_counts <- df_propensity %>%
  count(
    dataset_source,
    vote_approach_41_model,
    electoral_group_model,
    electoral_group_quality,
    name = "n"
  ) %>%
  group_by(dataset_source) %>%
  mutate(prop_source = n / sum(n)) %>%
  ungroup() %>%
  arrange(dataset_source, desc(n))

diagnostics_voter_pool_by_group <- df_propensity %>%
  filter(voter_electoral_pool == TRUE) %>%
  count(electoral_group_model, name = "n_pool") %>%
  right_join(
    electoral_targets_voters %>%
      select(target_electoral_group, target_n_in_bootstrap, target_share_global),
    by = c("electoral_group_model" = "target_electoral_group")
  ) %>%
  mutate(
    n_pool = replace_na(n_pool, 0L),
    pool_empty = n_pool == 0,
    
    # Ratio > 1 significa que hay más casos originales que elementos requeridos.
    # Ratio < 1 significa que habrá repetición con reemplazo.
    pool_to_target_ratio = if_else(
      target_n_in_bootstrap > 0,
      n_pool / target_n_in_bootstrap,
      NA_real_
    ),
    
    # Número aproximado de veces que cada fila tendría que reutilizarse por bootstrap.
    # Más alto = peor.
    target_to_pool_ratio = if_else(
      n_pool > 0,
      target_n_in_bootstrap / n_pool,
      Inf
    ),
    
    pool_diagnostic = case_when(
      pool_empty ~ "empty_pool_fallback_needed",
      pool_to_target_ratio >= 1 ~ "good_pool",
      pool_to_target_ratio >= 0.5 ~ "acceptable_some_repetition",
      pool_to_target_ratio >= 0.25 ~ "weak_high_repetition",
      TRUE ~ "very_weak_high_repetition"
    )
  ) %>%
  arrange(desc(target_share_global))

bootstrap_samples_summary <- bootstrap_index %>%
  count(
    bootstrap_id,
    target_sample_type,
    target_electoral_group,
    name = "n"
  ) %>%
  group_by(bootstrap_id) %>%
  mutate(prop_bootstrap = n / sum(n)) %>%
  ungroup()

bootstrap_source_distribution <- bootstrap_index %>%
  count(bootstrap_id, target_sample_type, dataset_source, name = "n") %>%
  group_by(bootstrap_id, target_sample_type) %>%
  mutate(prop_within_type = n / sum(n)) %>%
  ungroup()

bootstrap_quality_distribution <- bootstrap_index %>%
  count(bootstrap_id, target_sample_type, row_quality_final, name = "n") %>%
  group_by(bootstrap_id, target_sample_type) %>%
  mutate(prop_within_type = n / sum(n)) %>%
  ungroup()

bootstrap_quality_flags_summary <- bootstrap_index %>%
  count(
    bootstrap_id,
    target_sample_type,
    usable_for_main_analysis,
    usable_for_clustering,
    row_quality_final,
    name = "n"
  ) %>%
  group_by(bootstrap_id, target_sample_type) %>%
  mutate(prop_within_type = n / sum(n)) %>%
  ungroup()

bootstrap_fallback_summary <- bootstrap_index %>%
  count(target_sample_type, target_electoral_group, fallback_used, name = "n") %>%
  group_by(target_sample_type, target_electoral_group) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# Guardado
if (SAVE_FULL_PROPENSITY_DATASET) {
  write_csv(
    df_propensity,
    file.path(out_dir, "dataset_with_propensity_and_electoral_groups.csv")
  )
}

write_csv(
  diagnostics_propensity_training,
  file.path(out_dir, "diagnostics_propensity_training.csv")
)

write_csv(
  propensity_model_coefficients,
  file.path(out_dir, "propensity_model_coefficients.csv")
)

write_csv(
  factor_mapping_table,
  file.path(out_dir, "propensity_factor_level_mapping.csv")
)

writeLines(
  capture.output(summary(propensity_fit)),
  file.path(out_dir, "propensity_model_summary.txt")
)

write_csv(
  diagnostics_propensity_predictions_by_source,
  file.path(out_dir, "diagnostics_propensity_predictions_by_source.csv")
)

write_csv(
  diagnostics_electoral_group_counts,
  file.path(out_dir, "diagnostics_electoral_group_counts.csv")
)

write_csv(
  diagnostics_voter_pool_by_group,
  file.path(out_dir, "diagnostics_voter_pool_by_group.csv")
)

write_csv(
  electoral_targets_final,
  file.path(out_dir, "bootstrap_targets_final.csv")
)

if (SAVE_BOOTSTRAP_INDEX) {
  write_csv(
    bootstrap_index,
    file.path(out_dir, "bootstrap_samples_index.csv")
  )
}

write_csv(
  bootstrap_samples_summary,
  file.path(out_dir, "bootstrap_samples_summary.csv")
)

write_csv(
  bootstrap_source_distribution,
  file.path(out_dir, "bootstrap_source_distribution.csv")
)

write_csv(
  bootstrap_quality_distribution,
  file.path(out_dir, "bootstrap_quality_distribution.csv")
)

write_csv(
  bootstrap_fallback_summary,
  file.path(out_dir, "bootstrap_fallback_summary.csv")
)

if (SAVE_FULL_BOOTSTRAP_DATASET) {
  bootstrap_full <- bootstrap_index %>%
    left_join(
      df_propensity,
      by = "integrated_row_id",
      suffix = c("_bootstrap", "_original")
    )
  
  write_csv(
    bootstrap_full,
    file.path(out_dir, "bootstrap_samples_full_dataset.csv")
  )
}

write_csv(
  bootstrap_quality_flags_summary,
  file.path(out_dir, "bootstrap_quality_flags_summary.csv")
)

# Resumen en consola
cat("\n============================================================\n")
cat("MODELO DE PROPENSIÓN\n")
cat("============================================================\n")
print(diagnostics_propensity_training, n = Inf, width = Inf)

cat("\nFórmula usada:\n")
print(propensity_formula)

cat("\nCoeficientes del modelo:\n")
print(propensity_model_coefficients, n = Inf, width = Inf)

cat("\n============================================================\n")
cat("PREDICCIONES POR FUENTE\n")
cat("============================================================\n")
print(diagnostics_propensity_predictions_by_source, n = Inf, width = Inf)

cat("\n============================================================\n")
cat("TARGETS BOOTSTRAP\n")
cat("============================================================\n")
print(electoral_targets_final, n = Inf, width = Inf)

cat("\n============================================================\n")
cat("POOL VOTANTE POR GRUPO ELECTORAL\n")
cat("============================================================\n")
print(diagnostics_voter_pool_by_group, n = Inf, width = Inf)

cat("\n============================================================\n")
cat("CLASIFICACIÓN ELECTORAL OBSERVADA\n")
cat("============================================================\n")
print(diagnostics_electoral_group_counts, n = Inf, width = Inf)

cat("\n============================================================\n")
cat("RESUMEN BOOTSTRAP: primeras filas\n")
cat("============================================================\n")
print(
  bootstrap_samples_summary %>%
    group_by(target_sample_type, target_electoral_group) %>%
    summarise(
      mean_n = mean(n),
      sd_n = sd(n),
      mean_prop = mean(prop_bootstrap),
      .groups = "drop"
    ) %>%
    arrange(target_sample_type, desc(mean_n)),
  n = Inf,
  width = Inf
)

cat("\n============================================================\n")
cat("FALLBACKS EN BOOTSTRAP\n")
cat("============================================================\n")
print(bootstrap_fallback_summary, n = Inf, width = Inf)

message("\nListo.")
message("Resultados guardados en: ", out_dir)
message("Dataset con propensity: ", file.path(out_dir, "dataset_with_propensity_and_electoral_groups.csv"))
message("Índice de muestras bootstrap: ", file.path(out_dir, "bootstrap_samples_index.csv"))
message("Targets bootstrap: ", file.path(out_dir, "bootstrap_targets_final.csv"))
message("Diagnóstico training propensity: ", file.path(out_dir, "diagnostics_propensity_training.csv"))
message("Diagnóstico pool votante: ", file.path(out_dir, "diagnostics_voter_pool_by_group.csv"))