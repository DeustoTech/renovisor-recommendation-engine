
# SCRIPT 13 - ARQUETIPOS SINTÉTICOS POR ETAPA + BOOTSTRAP CLMM

# librerias
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(ordinal)
library(broom.mixed)
library(tibble)
library(broom)

set.seed(123)

# Carpetas de salida
base_output_dir <- "initial_descriptive_analysis/output/synthetic_archetypes_stage_bootstrap"

csv_dir <- file.path(base_output_dir, "csv")
models_dir <- file.path(base_output_dir, "models")
logs_dir <- file.path(base_output_dir, "logs")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)


# Parámetros iniciales
N_SYNTHETIC_PARTICIPANTS <- 5000 # num participantes indiv
DECISIONS_PER_PARTICIPANT <- 3 # decisiones por particip

# BOOTSTRAP_SIZES <- c(10, 25, 50, 100, 150, 200, 300, 500, 750, 1000)
BOOTSTRAP_SIZES <- c( 100, 200, 500, 1000)
# BOOTSTRAP_SIZES <- c(1000)
N_BOOT <- 2
#N_BOOT <- 5

# Si TRUE prueba determinantes * arquetipo.
# Si da problemas o tarda mucho, poner FALSE.
# USE_INTERACTIONS <- FALSE


# 32 Determinantes del modelo - variab predictoras del modelo
determinants <- c(
  "profits",
  "credit_score_access_to_funding",
  "risk_profile",
  "added_value",
  "frugality",
  "climate_protection",
  "legal",
  "trust",
  "safety",
  "cost_efficiency",
  "knowledge",
  "own_competence",
  "technical_fit",
  "environmental_concerns",
  "self_satisfaction",
  "commitment",
  "adherence",
  "autarky",
  "wellbeing",
  "coziness",
  "rights_and_duties",
  "peer_pressure",
  "support",
  "socialising",
  "agreement",
  "novelty",
  "fun",
  "recognition",
  "trends",
  "authority",
  "approval",
  "own_significance"
)

# Mapa experto por arquetipo y etapa
# 3 Etapas usadas:
# - Early = Precontemplation + Contemplation
# - Preparation = Preparation
# - Action = Action
# Se ignora la ultima --> Maintenance

# posseur --> approval
# brag --> recognition

stage_map <- list(
  
  Early_Adopter = list(
    Early = c("environmental_concerns", "trends","novelty"),
    Preparation = c("knowledge"),
    Action = c("adherence")
  ),
  
  Uninterested = list(
    Early = c("technical_fit","peer_pressure","coziness","legal",
              "wellbeing"),
    Preparation = c("authority"),
    Action = c()
  ),
  
  Homo_Economicus = list(
    Early = c("knowledge","profits", "cost_efficiency",
              "added_value", "legal"),
    Preparation = c("technical_fit","trust","safety"),
    Action = c("self_satisfaction")
  ),
  
  Fearful = list(
    Early = c("cost_efficiency", "safety", "risk_profile", "legal",
              "trust","wellbeing"),
    Preparation = c("knowledge","technical_fit"),
    Action = c()
  ),
  
  Stubborn = list(
    Early = c(),
    Preparation = c("knowledge"),
    Action = c("self_satisfaction")
  ),
  
  Influencer = list(
    Early = c("peer_pressure"),
    Preparation = c("knowledge", "socialising"),
    Action = c("approval","recognition")
  ),
  
  Careful = list(
    Early = c("environmental_concerns", "risk_profile", "safety", 
              "climate_protection", "wellbeing"),
    Preparation = c("cost_efficiency","own_competence"),
    Action = c("adherence")
  ),
  
  Activist = list(
    Early = c("environmental_concerns", "trust"),
    Preparation = c("technical_fit"),
    Action = c("self_satisfaction")
  )
)

archetypes <- names(stage_map)


# Generar participantes sintéticos

# se crea una tabla. El arqutipo se asigna aleator.
participants <- tibble(
  participant_id = paste0("synthetic_", seq_len(N_SYNTHETIC_PARTICIPANTS)),
  arquetipo = sample(
    archetypes,
    size = N_SYNTHETIC_PARTICIPANTS,
    replace = TRUE
  )
)

# Varias decisiones por participante. Duplica cada partic 3 veces
synthetic_long <- participants %>%
  tidyr::crossing(decision_id = seq_len(DECISIONS_PER_PARTICIPANT)) %>%
  mutate(
    row_id = row_number(),
    Estado_Mental = sample(
      c("Early", "Preparation", "Action"),
      size = n(),
      replace = TRUE,
      prob = c(0.33, 0.33, 0.34)
    ),
    Estado_Mental = ordered(
      Estado_Mental,
      levels = c("Early", "Preparation", "Action")
    )
  )


# Asignar valores de determinantes según arquetipo + etapa
generate_determinant_value <- function(arquetipo, etapa, determinant) {
  
  active_dets <- stage_map[[arquetipo]][[as.character(etapa)]] # mira determinantes activos
  
  if (determinant %in% active_dets) { # si está activo --> valor alto
    val <- rnorm(1, mean = 85, sd = 8)
  } else {
    val <- rnorm(1, mean = 35, sd = 15)
  }
  
  val <- pmax(0, pmin(100, val)) # limita el valor entre 0 y 100
  round(val, 1) # redondea a 1 decimal
}

# crear la matriz de determinantes
determinant_matrix <- map_dfc(determinants, function(det) {
  tibble(
    !!det := pmap_dbl(
      list(
        synthetic_long$arquetipo,
        synthetic_long$Estado_Mental
      ),
      ~ generate_determinant_value(..1, ..2, det)
    )
  )
})

# crear dataset sintetico final
synthetic_stage_data <- bind_cols(
  synthetic_long,
  determinant_matrix
) # une datos del particip, decisión, etapa, valores de los 32 det

write_csv(
  synthetic_stage_data,
  file.path(csv_dir, "synthetic_stage_archetypes_participants.csv")
)

cat("Dataset sintético generado\n")
cat("Participantes:", n_distinct(synthetic_stage_data$participant_id), "\n")
cat("Filas:", nrow(synthetic_stage_data), "\n")

####################################################

# Función para ajustar CLMM en una muestra
fit_bootstrap_model <- function(data, n_participants, boot_id) {
  
  # selecciona participantes
  sampled_ids <- sample(
    unique(data$participant_id),
    size = n_participants,
    replace = FALSE
  )
  # filtra las filas de esos participantes
  sample_data <- data %>%
    filter(participant_id %in% sampled_ids) %>%
    mutate(
      arquetipo = factor(arquetipo),
      Estado_Mental = ordered(
        Estado_Mental,
        levels = c("Early", "Preparation", "Action")
      )
    )
  
  # media 0 dv 1
  sample_scaled <- sample_data %>%
    mutate(
      across(
        all_of(determinants),
        ~ as.numeric(scale(.x)),
        .names = "z_{.col}"
      )
    )
  
  z_dets <- paste0("z_", determinants)
  
  # formula del modelo
  formula_i <- as.formula(
    paste(
      "Estado_Mental ~",
      paste(paste0(z_dets, " * arquetipo"), collapse = " + "),
      "+ (1 | participant_id)"
    )
  )
  
  # sin interacciones
  # formula_i <- as.formula(
  #   paste(
  #     "Estado_Mental ~ arquetipo +",
  #     paste(z_dets, collapse = " + "),
  #     "+ (1 | participant_id)"
  #   )
  # )
  
  # ajuste de modelo
  model_i <- tryCatch(
    suppressWarnings(
      clmm(
        formula_i,
        data = sample_scaled,
        link = "logit", # modelo logistico ordinal
        Hess = TRUE, # calcular errores setandar y pvalues
        nAGQ = 1
      )
    ),
    error = function(e) NULL
  )
  
  if (is.null(model_i)) { # si falla:
    return(tibble(
      n_participants = n_participants,
      boot_id = boot_id,
      converged = FALSE,
      n_terms = NA_integer_,
      n_terms_with_p = NA_integer_,
      recovery_rate = NA_real_
    ))
  }
  saveRDS(
    model_i,
    file.path(
      models_dir,
      paste0("modelo_clmm_boot_", boot_id, "_n_", n_participants, ".rds")
    )
  )
  
  tidy_i <- tryCatch(
    broom::tidy(model_i),
    error = function(e) NULL
  )
  
  if (is.null(tidy_i) || !"p.value" %in% names(tidy_i)) {
    return(tibble(
      n_participants = n_participants,
      boot_id = boot_id,
      converged = TRUE,
      n_terms = NA_integer_,
      n_terms_with_p = 0,
      recovery_rate = NA_real_
    ))
  }
  
  det_terms <- tidy_i %>%
    filter(str_detect(term, "^z_")) %>%
    mutate(
      determinant = str_remove(term, "^z_"),
    )
  # tabla experta en formato largo
  expert_table <- imap_dfr(stage_map, function(stage_list, archetype_name) {
    imap_dfr(stage_list, function(dets, stage_name) {
      tibble(
        arquetipo_expert = archetype_name,
        Estado_Mental_expert = stage_name,
        determinant = dets,
        expert_defined = TRUE
      )
    })
  })
  
  # resultados completos: modelo + experto
  model_terms_full <- det_terms %>%
    mutate(
      term_clean = term,
      model_archetype = str_extract(term, "arquetipo.*$"),
      model_archetype = str_remove(model_archetype, "^arquetipo"),
      model_archetype = if_else(is.na(model_archetype), "main_effect", model_archetype),
      is_significant = !is.na(p.value) & p.value < 0.05
    ) %>%
    left_join(
      expert_table,
      by = "determinant",
      relationship = "many-to-many"
    ) %>%
    mutate(
      recovered = expert_defined & is_significant,
      n_participants = n_participants,
      boot_id = boot_id
    )
  
  write_csv(
    model_terms_full,
    file.path(
      csv_dir,
      paste0("model_terms_full_boot_", boot_id, "_n_", n_participants, ".csv")
    )
  )
  # determinantes definidos por expertos
  expert_determinants <- stage_map %>%
    unlist() %>%
    unique()
  
  # recovery:
  # ¿los determinantes expertos salen significativos?
  recovery <- det_terms %>%
    mutate(
      expert_defined = determinant %in% expert_determinants,
      recovered = expert_defined &
        !is.na(p.value) &
        p.value < 0.05
    )
  
  tibble(
    n_participants = n_participants,
    boot_id = boot_id,
    converged = TRUE,
    n_terms = nrow(det_terms),
    n_terms_with_p = sum(!is.na(det_terms$p.value)),
    n_significant_terms = sum(det_terms$p.value < 0.05, na.rm = TRUE),
    recovery_rate = mean(model_terms_full$recovered, na.rm = TRUE)
  )
}

# Ejecutar bootstrap por tamaño muestral
bootstrap_results <- crossing(
  n_participants = BOOTSTRAP_SIZES,
  boot_id = seq_len(N_BOOT)
) %>%
  mutate(
    result = map2(
      n_participants,
      boot_id,
      ~ fit_bootstrap_model(
        data = synthetic_stage_data,
        n_participants = .x,
        boot_id = .y
      )
    )
  ) %>%
  select(result) %>%
  unnest(result)

write_csv(
  bootstrap_results,
  file.path(csv_dir, "bootstrap_clmm_recovery_results.csv")
)


# Resumen final
bootstrap_summary <- bootstrap_results %>%
  group_by(n_participants) %>%
  summarise(
    n_boot = n(),
    convergence_rate = mean(converged, na.rm = TRUE),
    mean_terms_with_p = mean(n_terms_with_p, na.rm = TRUE),
    
    # % de determinantes expertos recuperados
    mean_recovery_rate = mean(recovery_rate, na.rm = TRUE),
    
    .groups = "drop"
  )

write_csv(
  bootstrap_summary,
  file.path(csv_dir, "bootstrap_clmm_recovery_summary.csv")
)

print(bootstrap_summary)

cat("Bootstrap CLMM terminado\n")
