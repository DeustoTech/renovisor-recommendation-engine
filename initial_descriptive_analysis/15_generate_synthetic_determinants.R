
# SCRIPT 15 - GENERAR MUESTRA SINTÉTICA
# 32 DETERMINANTES + 8 ARQUETIPOS ORIGINALES + 5 MACRO-ARQUETIPOS + 3 ETAPAS

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)

set.seed(123)


# 1. CARPETAS
base_output_dir <- "initial_descriptive_analysis/output/model_5arq_9dim"

csv_dir <- file.path(base_output_dir, "csv")
bootstrap_dir <- file.path(base_output_dir, "bootstrap")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(bootstrap_dir, recursive = TRUE, showWarnings = FALSE)

# 2. PARÁMETROS GENERALES
N_SYNTHETIC_PARTICIPANTS <- 5000
DECISIONS_PER_PARTICIPANT <- 3

# Esto genera una población sintética grande.
# Después se generan índices bootstrap sobre participantes.
#BOOTSTRAP_SIZES <- c(300, 600, 1000, 2000, 3000, 5000)
BOOTSTRAP_SIZES <- c(200, 300, 600, 1000, 2000)
N_BOOT <- 5

# TRUE = bootstrap clásico, con reemplazo.
# FALSE = submuestreo, sin reemplazo.
BOOTSTRAP_WITH_REPLACEMENT <- FALSE


# 3. DETERMINANTES
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
  "recognition", # brag
  "trends",
  "authority",
  "approval", # poseur
  "own_significance"
)


# 4. DICCIONARIO DETERMINANTE -> DIMENSIÓN
dimension_dictionary <- tribble(
  ~determinant,                         ~dimension,
  "profits",                            "FINANCIAL",
  "credit_score_access_to_funding",     "FINANCIAL",
  "risk_profile",                       "FINANCIAL",
  "added_value",                        "FINANCIAL",
  "frugality",                          "FINANCIAL",
  
  "legal",                              "SECURITY",
  "trust",                              "SECURITY",
  "safety",                             "SECURITY",
  "climate_protection",                 "SECURITY",
  
  "cost_efficiency",                    "COMPETENCE",
  "knowledge",                          "COMPETENCE",
  "own_competence",                     "COMPETENCE",
  "technical_fit",                      "COMPETENCE",
  "environmental_concerns",             "COMPETENCE",
  
  "self_satisfaction",                  "AUTONOMY",
  "commitment",                         "AUTONOMY",
  "adherence",                          "AUTONOMY",
  "autarky",                            "AUTONOMY",
  
  "wellbeing",                          "PHYSIOLOGICAL",
  "coziness",                           "PHYSIOLOGICAL",
  
  "rights_and_duties",                  "RELATEDNESS",
  "peer_pressure",                      "RELATEDNESS",
  "support",                            "RELATEDNESS",
  "socialising",                        "RELATEDNESS",
  "agreement",                          "RELATEDNESS",
  
  "novelty",                            "STIMULATION",
  "fun",                                "STIMULATION",
  "recognition",                        "STIMULATION",
  
  "trends",                             "POPULARITY",
  "authority",                          "POPULARITY",
  
  "own_significance",                   "MEANING",
  "approval",                           "MEANING"
  
)

dimensions <- unique(dimension_dictionary$dimension)

write_csv(
  dimension_dictionary,
  file.path(csv_dir, "determinant_dimension_dictionary.csv")
)


# 5. MAPA DE 8 ARQUETIPOS ORIGINALES A 5 MACRO-ARQUETIPOS
original_archetypes <- c(
  "Activist",
  "Stubborn",
  "Early_Adopter",
  "Influencer",
  "Fearful",
  "Careful",
  "Homo_Economicus",
  "Uninterested"
)

map_to_macro_archetype <- function(original_archetype) {
  case_when(
    original_archetype %in% c("Activist", "Stubborn") ~ "G1_Activist_Stubborn_Sentient",
    original_archetype %in% c("Early_Adopter", "Influencer") ~ "G2_EarlyAdopter_Influencer",
    original_archetype %in% c("Fearful", "Careful") ~ "G3_Fearful_Careful",
    original_archetype == "Homo_Economicus" ~ "G4_HomoEconomicus",
    original_archetype == "Uninterested" ~ "G5_Uninterested",
    TRUE ~ NA_character_
  )
}

macro_archetypes <- c(
  "G1_Activist_Stubborn_Sentient",
  "G2_EarlyAdopter_Influencer",
  "G3_Fearful_Careful",
  "G4_HomoEconomicus",
  "G5_Uninterested"
)

# Sobrerrepresentamos Early Adopter + Influencer porque tu jefe dijo que
# si hacía falta generar muestra, especialmente para ese grupo.
original_archetype_probs <- c(
  Activist = 0.11,
  Stubborn = 0.10,
  Early_Adopter = 0.22,
  Influencer = 0.14,
  Fearful = 0.12,
  Careful = 0.12,
  Homo_Economicus = 0.10,
  Uninterested = 0.09
)

original_archetype_probs <- original_archetype_probs / sum(original_archetype_probs)


# 6. ETAPAS
stage_levels <- c(
  "Knowledge",
  "Considering",
  "Done"
)

# Probabilidad de etapa según macro-arquetipo.
# Esto NO son resultados reales: solo sirve para generar una muestra sintética
# coherente con la hipótesis del modelo
stage_probs_by_macro <- tribble(
  ~macro_archetype_5,                    ~Knowledge, ~Considering, ~Done,
  "G1_Activist_Stubborn_Sentient",        0.25,       0.40,          0.35,
  "G2_EarlyAdopter_Influencer",           0.15,       0.35,          0.50,
  "G3_Fearful_Careful",                   0.45,       0.40,          0.15,
  "G4_HomoEconomicus",                    0.30,       0.45,          0.25,
  "G5_Uninterested",                      0.65,       0.25,          0.10
)

sample_stage <- function(macro_archetype_i) {
  probs_i <- stage_probs_by_macro %>%
    filter(macro_archetype_5 == macro_archetype_i) %>%
    select(all_of(stage_levels)) %>%
    unlist(use.names = FALSE)
  
  sample(
    stage_levels,
    size = 1,
    replace = TRUE,
    prob = probs_i
  )
}

# 7. MAPA EXPERTO: ARQUETIPO ORIGINAL x ETAPA -> DETERMINANTES ACTIVOS

# Se adapta la lógica previa:
# Early       -> Knowledge
# Preparation -> Considering
# Action      -> Done

stage_map <- list(
  
  Early_Adopter = list(
    Knowledge = c("environmental_concerns", "trends","novelty"),
    Considering = c("knowledge"),
    Done = c("adherence")
  ),
  
  Uninterested = list(
    Knowledge = c("technical_fit","peer_pressure","coziness","legal",
              "wellbeing"),
    Considering = c("authority"),
    Done = c()
  ),
  
  Homo_Economicus = list(
    Knowledge = c("knowledge","profits", "cost_efficiency",
              "added_value", "legal"),
    Considering = c("technical_fit","trust","safety"),
    Done = c("self_satisfaction")
  ),
  
  Fearful = list(
    Knowledge = c("cost_efficiency", "safety", "risk_profile", "legal",
              "trust","wellbeing"),
    Considering = c("knowledge","technical_fit"),
    Done = c()
  ),
  
  Stubborn = list(
    Knowledge = c(),
    Considering = c("knowledge"),
    Done = c("self_satisfaction")
  ),
  
  Influencer = list(
    Knowledge = c("peer_pressure"),
    Considering = c("knowledge", "socialising"),
    Done = c("approval","recognition")
  ),
  
  Careful = list(
    Knowledge = c("environmental_concerns", "risk_profile", "safety", 
              "climate_protection", "wellbeing"),
    Considering = c("cost_efficiency","own_competence"),
    Done = c("adherence")
  ),
  
  Activist = list(
    Knowledge = c("environmental_concerns", "trust"),
    Considering = c("technical_fit"),
    Done = c("self_satisfaction")
  )
)

expert_table <- imap_dfr(stage_map, function(stage_list, archetype_name) {
  imap_dfr(stage_list, function(dets, stage_name) {
    tibble(
      original_archetype = archetype_name,
      macro_archetype_5 = map_to_macro_archetype(archetype_name),
      adoption_stage = stage_name,
      determinant = dets,
      expert_active = TRUE
    )
  })
})

write_csv(
  expert_table,
  file.path(csv_dir, "expert_active_determinants_by_archetype_stage.csv")
)


# 8. CREAR PARTICIPANTES SINTÉTICOS
participants <- tibble(
  participant_id = paste0("synthetic_", seq_len(N_SYNTHETIC_PARTICIPANTS)),
  original_archetype = sample(
    original_archetypes,
    size = N_SYNTHETIC_PARTICIPANTS,
    replace = TRUE,
    prob = original_archetype_probs
  )
) %>%
  mutate(
    macro_archetype_5 = map_to_macro_archetype(original_archetype),
    
    # Rasgo latente individual de estilo de respuesta.
    # Hace que algunas personas puntúen sistemáticamente algo más alto/bajo.
    response_style_latent = rnorm(n(), mean = 0, sd = 4)
  )


# 9. CREAR DECISIONES POR PARTICIPANTE
technologies <- c(
  "balcony_solar_kits",
  "electricity_tariff_change",
  "cooling_system",
  "domestic_hot_water_system",
  "electric_vehicle",
  "elevator",
  "energy_storage_systems",
  "energy_efficient_appliances",
  "envelope_renovation",
  "fossil_fuel_or_biomass_heating_system",
  "heat_pump_heating_system",
  "heat_recovery_mechanical_ventilation",
  "energy_community",
  "micro_efficiency_measures",
  "rooftop_solar_pv",
  "smart_home_devices"
  #"none"
)

synthetic_long <- participants %>%
  crossing(decision_id = seq_len(DECISIONS_PER_PARTICIPANT)) %>%
  mutate(
    technology = sample(
      technologies,
      size = n(),
      replace = TRUE
    ),
    adoption_stage = map_chr(macro_archetype_5, sample_stage),
    adoption_stage = ordered(
      adoption_stage,
      levels = stage_levels
    )
  )


# 10. EFECTOS DE ARQUETIPO, ETAPA E INTERACCIÓN SOBRE DIMENSIONES

# Efectos por macro-arquetipo sobre las 9 dimensiones.
# Son supuestos sintéticos, no resultados reales.

macro_dimension_effects <- tribble(
  ~macro_archetype_5,                    ~FINANCIAL, ~SECURITY, ~COMPETENCE, ~AUTONOMY, ~PHYSIOLOGICAL, ~RELATEDNESS, ~STIMULATION, ~POPULARITY, ~MEANING,
  "G1_Activist_Stubborn_Sentient",        -2,         2,         4,           7,         3,              6,            5,            2,           9,
  "G2_EarlyAdopter_Influencer",            1,         1,         8,           4,         4,              5,            9,            8,           6,
  "G3_Fearful_Careful",                    5,         9,         5,          -3,         3,              2,           -4,           -2,           2,
  "G4_HomoEconomicus",                    10,         6,         2,           1,         1,             -3,           -2,           -1,          -3,
  "G5_Uninterested",                      -5,         3,        -4,          -4,         2,             -3,           -5,           -4,          -5
)

macro_dim_mat <- as.matrix(macro_dimension_effects[, dimensions])
rownames(macro_dim_mat) <- macro_dimension_effects$macro_archetype_5

# Efectos por etapa.
stage_dimension_effects <- tribble(
  ~adoption_stage, ~FINANCIAL, ~SECURITY, ~COMPETENCE, ~AUTONOMY, ~PHYSIOLOGICAL, ~RELATEDNESS, ~STIMULATION, ~POPULARITY, ~MEANING,
  "Knowledge",       0,         5,         2,          -2,         1,              0,            1,            0,           2,
  "Considering",     6,         4,         6,           2,         2,              2,            3,            2,           4,
  "Done",            4,         2,         7,           7,         6,              4,            5,            4,           7
)

stage_dim_mat <- as.matrix(stage_dimension_effects[, dimensions])
rownames(stage_dim_mat) <- stage_dimension_effects$adoption_stage

# Interacción aleatoria macro-arquetipo x etapa x dimensión.
# Esto evita que todas las combinaciones sean demasiado limpias o iguales.
interaction_effects <- array(
  rnorm(
    length(macro_archetypes) * length(stage_levels) * length(dimensions),
    mean = 0,
    sd = 3
  ),
  dim = c(length(macro_archetypes), length(stage_levels), length(dimensions)),
  dimnames = list(macro_archetypes, stage_levels, dimensions)
)


# 11. PARÁMETROS PROPIOS DE CADA DETERMINANTE

# Aquí evitamos el problema de "si activo siempre 85, si no activo siempre 35".
# Cada determinante tiene su baseline, su lift y su desviación típica.

determinant_parameters <- dimension_dictionary %>%
  mutate(
    base_mean = runif(n(), min = 35, max = 52),
    active_lift = runif(n(), min = 16, max = 32),
    inactive_shift = runif(n(), min = -5, max = 5),
    residual_sd = runif(n(), min = 8, max = 15)
  )

write_csv(
  determinant_parameters,
  file.path(csv_dir, "synthetic_determinant_generation_parameters.csv")
)

det_to_dim <- setNames(
  determinant_parameters$dimension,
  determinant_parameters$determinant
)

det_base <- setNames(
  determinant_parameters$base_mean,
  determinant_parameters$determinant
)

det_active_lift <- setNames(
  determinant_parameters$active_lift,
  determinant_parameters$determinant
)

det_inactive_shift <- setNames(
  determinant_parameters$inactive_shift,
  determinant_parameters$determinant
)

det_sd <- setNames(
  determinant_parameters$residual_sd,
  determinant_parameters$determinant
)


# 12. FUNCIÓN PARA GENERAR VALORES DE DETERMINANTES
generate_determinant_value <- function(
    original_archetype,
    macro_archetype_5,
    adoption_stage,
    response_style_latent,
    determinant
) {
  
  adoption_stage_chr <- as.character(adoption_stage)
  
  det_dimension <- det_to_dim[[determinant]]
  
  active_dets <- stage_map[[original_archetype]][[adoption_stage_chr]]
  
  is_active <- determinant %in% active_dets
  
  base <- det_base[[determinant]]
  
  determinant_part <- if (is_active) {
    det_active_lift[[determinant]]
  } else {
    det_inactive_shift[[determinant]]
  }
  
  mu <- base +
    determinant_part +
    macro_dim_mat[macro_archetype_5, det_dimension] +
    stage_dim_mat[adoption_stage_chr, det_dimension] +
    interaction_effects[macro_archetype_5, adoption_stage_chr, det_dimension] +
    response_style_latent
  
  value <- rnorm(
    n = 1,
    mean = mu,
    sd = det_sd[[determinant]]
  )
  
  value <- pmax(0, pmin(100, value))
  
  round(value, 1)
}


# 13. GENERAR MATRIZ DE 32 DETERMINANTES
cat("Generando matriz de 32 determinantes...\n")

determinant_matrix <- map_dfc(determinants, function(det_i) {
  
  tibble(
    !!det_i := pmap_dbl(
      list(
        synthetic_long$original_archetype,
        synthetic_long$macro_archetype_5,
        synthetic_long$adoption_stage,
        synthetic_long$response_style_latent
      ),
      function(original_archetype, macro_archetype_5, adoption_stage, response_style_latent) {
        generate_determinant_value(
          original_archetype = original_archetype,
          macro_archetype_5 = macro_archetype_5,
          adoption_stage = adoption_stage,
          response_style_latent = response_style_latent,
          determinant = det_i
        )
      }
    )
  )
})

synthetic_32det <- bind_cols(
  synthetic_long,
  determinant_matrix
) %>%
  mutate(
    row_id = row_number()
  ) %>%
  select(
    row_id,
    participant_id,
    decision_id,
    technology,
    original_archetype,
    macro_archetype_5,
    adoption_stage,
    response_style_latent,
    all_of(determinants)
  )


# 14. GUARDAR DATASET SINTÉTICO COMPLETO
write_csv(
  synthetic_32det,
  file.path(csv_dir, "synthetic_32det_5arq.csv")
)

cat("Dataset sintético guardado en:\n")
cat(file.path(csv_dir, "synthetic_32det_5arq.csv"), "\n")


# 15. RESÚMENES DE CONTROL
participant_distribution <- synthetic_32det %>%
  distinct(participant_id, original_archetype, macro_archetype_5) %>%
  count(original_archetype, macro_archetype_5, name = "n_participants") %>%
  mutate(percentage = n_participants / sum(n_participants) * 100)

stage_distribution <- synthetic_32det %>%
  count(macro_archetype_5, adoption_stage, name = "n_rows") %>%
  group_by(macro_archetype_5) %>%
  mutate(percentage_within_macro = n_rows / sum(n_rows) * 100) %>%
  ungroup()

write_csv(
  participant_distribution,
  file.path(csv_dir, "synthetic_participant_distribution.csv")
)

write_csv(
  stage_distribution,
  file.path(csv_dir, "synthetic_stage_distribution_by_macro_archetype.csv")
)

print(participant_distribution, n = Inf)
print(stage_distribution, n = Inf)


# 16. GENERAR ÍNDICES BOOTSTRAP SOBRE PARTICIPANTES

# La unidad de remuestreo es el participante, no la fila.
# Como cada participante tiene varias decisiones, al remuestrear participantes
# se conservan sus decisiones completas.

cat("Generando índices bootstrap...\n")

bootstrap_index <- crossing(
  n_participants = BOOTSTRAP_SIZES,
  boot_id = seq_len(N_BOOT)
) %>%
  mutate(
    sampled_participants = map(
      n_participants,
      function(n_i) {
        sample(
          participants$participant_id,
          size = n_i,
          replace = BOOTSTRAP_WITH_REPLACEMENT
        )
      }
    )
  ) %>%
  unnest_longer(
    sampled_participants,
    values_to = "original_participant_id"
  ) %>%
  group_by(n_participants, boot_id) %>%
  mutate(
    bootstrap_position = row_number(),
    bootstrap_participant_id = paste0(
      "boot_n", n_participants,
      "_b", boot_id,
      "_p", bootstrap_position
    )
  ) %>%
  ungroup()

write_csv(
  bootstrap_index,
  file.path(bootstrap_dir, "bootstrap_participant_index.csv")
)

# Resumen del bootstrap por macro-arquetipo.
bootstrap_summary <- bootstrap_index %>%
  left_join(
    participants %>%
      select(
        original_participant_id = participant_id,
        original_archetype,
        macro_archetype_5
      ),
    by = "original_participant_id"
  ) %>%
  count(n_participants, boot_id, macro_archetype_5, name = "n_boot_participants") %>%
  group_by(n_participants, boot_id) %>%
  mutate(
    percentage = n_boot_participants / sum(n_boot_participants) * 100
  ) %>%
  ungroup()

write_csv(
  bootstrap_summary,
  file.path(bootstrap_dir, "bootstrap_macro_archetype_summary.csv")
)

print(bootstrap_summary, n = 50)

cat("Archivos principales creados:\n")
cat("- synthetic_32det_5arq.csv\n")
cat("- determinant_dimension_dictionary.csv\n")
cat("- expert_active_determinants_by_archetype_stage.csv\n")
cat("- bootstrap_participant_index.csv\n")
cat("- bootstrap_macro_archetype_summary.csv\n")