
# SCRIPT 14 - CLMM SINTÉTICO POR ARQUETIPO

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(ordinal)
library(broom.mixed)
library(tibble)

set.seed(123)

# 1. Carpetas
base_output_dir <- "initial_descriptive_analysis/output/synthetic_clmm_by_archetype"

csv_dir <- file.path(base_output_dir, "csv")
models_dir <- file.path(base_output_dir, "models")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)


# 2. Parámetros
N_SYNTHETIC_PARTICIPANTS <- 10000
DECISIONS_PER_PARTICIPANT <- 3

#BOOTSTRAP_SIZES <- c(2000, 3000, 5000, 10000)
BOOTSTRAP_SIZES <- c(1000, 2000, 3000)

N_BOOT <- 10


# 3. Determinantes

determinants <- c(
  "profits", "credit_score_access_to_funding", "risk_profile", "added_value",
  "frugality", "climate_protection", "legal", "trust", "safety",
  "cost_efficiency", "knowledge", "own_competence", "technical_fit",
  "environmental_concerns", "self_satisfaction", "commitment", "adherence",
  "autarky", "wellbeing", "coziness", "rights_and_duties", "peer_pressure",
  "support", "socialising", "agreement", "novelty", "fun", "recognition",
  "trends", "authority", "approval", "own_significance"
)

# 4. Mapa experto

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


# 5. Tabla experta larga
expert_table <- imap_dfr(stage_map, function(stage_list, archetype_name) {
  imap_dfr(stage_list, function(dets, stage_name) {
    tibble(
      arquetipo = archetype_name,
      Estado_Mental_expert = stage_name,
      determinant = dets,
      expert_defined = TRUE
    )
  })
})

write_csv(
  expert_table,
  file.path(csv_dir, "expert_archetype_stage_determinants.csv")
)

# 6. Generar dataset sintético
participants <- tibble(
  participant_id = paste0("synthetic_", seq_len(N_SYNTHETIC_PARTICIPANTS)),
  arquetipo = sample(
    archetypes,
    size = N_SYNTHETIC_PARTICIPANTS,
    replace = TRUE
  )
)

synthetic_long <- participants %>%
  crossing(decision_id = seq_len(DECISIONS_PER_PARTICIPANT)) %>%
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

generate_determinant_value <- function(arquetipo, etapa, determinant) {
  
  active_dets <- stage_map[[arquetipo]][[as.character(etapa)]]
  
  if (determinant %in% active_dets) {
    val <- rnorm(1, mean = 70, sd = 15)
  } else {
    val <- rnorm(1, mean = 45, sd = 18)
  }
  
  round(pmax(0, pmin(100, val)), 1)
}

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

synthetic_stage_data <- bind_cols(
  synthetic_long,
  determinant_matrix
)

write_csv(
  synthetic_stage_data,
  file.path(csv_dir, "synthetic_stage_archetypes_data.csv")
)

cat("Dataset sintético generado\n")
cat("Participantes:", n_distinct(synthetic_stage_data$participant_id), "\n")
cat("Filas:", nrow(synthetic_stage_data), "\n")


# 7. Modelo CLMM por arquetipo
fit_one_archetype <- function(sample_scaled, archetype_i, n_participants, boot_id) {
  
  data_i <- sample_scaled %>%
    filter(arquetipo == archetype_i) %>%
    droplevels()
  
  if (
    n_distinct(data_i$participant_id) < 30 ||
    n_distinct(data_i$Estado_Mental) < 2
  ) {
    return(tibble(
      arquetipo = archetype_i,
      boot_id = boot_id,
      n_participants = n_participants,
      model_status = "insufficient_data"
    ))
  }
  
  z_dets <- paste0("z_", determinants)
  
  # formula_i <- as.formula(
  #   paste(
  #     "Estado_Mental ~",
  #     paste(z_dets, collapse = " + "),
  #     "+ (1 | participant_id)"
  #   )
  # )
  
  formula_i <- as.formula(
    paste(
      "Estado_Mental ~",
      "z_profits",
      "+ (1 | participant_id)"
    )
  )
  
  model_i <- tryCatch(
    suppressWarnings(
      clmm(
        formula_i,
        data = data_i,
        link = "logit",
        Hess = TRUE,
        nAGQ = 1
      )
    ),
    error = function(e) NULL
  )
  
  if (is.null(model_i)) {
    return(tibble(
      arquetipo = archetype_i,
      boot_id = boot_id,
      n_participants = n_participants,
      model_status = "model_failed"
    ))
  }
  
  saveRDS(
    model_i,
    file.path(
      models_dir,
      paste0("clmm_", archetype_i, "_boot_", boot_id, "_n_", n_participants, ".rds")
    )
  )
  
  tidy_i <- tryCatch(
    broom.mixed::tidy(model_i, effects = "fixed"),
    error = function(e) NULL
  )
  
  if (is.null(tidy_i) || !"p.value" %in% names(tidy_i)) {
    return(tibble(
      arquetipo = archetype_i,
      boot_id = boot_id,
      n_participants = n_participants,
      model_status = "no_p_values"
    ))
  }
  
  tidy_i %>%
    filter(str_detect(term, "^z_")) %>%
    mutate(
      arquetipo = archetype_i,
      determinant = str_remove(term, "^z_"),
      is_significant = !is.na(p.value) & p.value < 0.05,
      boot_id = boot_id,
      n_participants = n_participants,
      model_status = "ok"
    )
}


# 8. Bootstrap
fit_bootstrap_model <- function(data, n_participants, boot_id) {
  
  sampled_ids <- sample(
    unique(data$participant_id),
    size = n_participants,
    replace = FALSE
  )
  
  sample_data <- data %>%
    filter(participant_id %in% sampled_ids) %>%
    mutate(
      arquetipo = factor(arquetipo),
      Estado_Mental = ordered(
        Estado_Mental,
        levels = c("Early", "Preparation", "Action")
      )
    )
  
  sample_scaled <- sample_data %>%
    mutate(
      across(
        all_of(determinants),
        ~ as.numeric(scale(.x)),
        .names = "z_{.col}"
      )
    )
  
  all_terms <- map_dfr(
    unique(sample_scaled$arquetipo),
    ~ fit_one_archetype(
      sample_scaled = sample_scaled,
      archetype_i = .x,
      n_participants = n_participants,
      boot_id = boot_id
    )
  )
  if (!"determinant" %in% names(all_terms)) {
    
    all_terms_full <- tibble()
    
  } else {
    
    all_terms_ok <- all_terms %>%
      filter(model_status == "ok") %>%
      filter(!is.na(determinant))
    
    all_terms_full <- all_terms_ok %>%
      left_join(
        expert_table,
        by = c("arquetipo", "determinant"),
        relationship = "many-to-many"
      ) %>%
      mutate(
        expert_defined = replace_na(expert_defined, FALSE),
        recovered = expert_defined & is_significant
      )
    
    write_csv(
      all_terms_full,
      file.path(
        csv_dir,
        paste0(
          "model_terms_by_archetype_boot_",
          boot_id,
          "_n_",
          n_participants,
          ".csv"
        )
      )
    )
  }
  
  tibble(
    n_participants = n_participants,
    boot_id = boot_id,
    n_archetypes_ok = ifelse(exists("all_terms_ok"), n_distinct(all_terms_ok$arquetipo), 0),
    n_terms = ifelse(exists("all_terms_ok"), nrow(all_terms_ok), 0),
    n_terms_with_p = ifelse(exists("all_terms_ok"), sum(!is.na(all_terms_ok$p.value)), 0),
    n_significant_terms = ifelse(exists("all_terms_ok"), sum(all_terms_ok$is_significant, na.rm = TRUE), 0),
    recovery_rate = ifelse(nrow(all_terms_full) > 0, mean(all_terms_full$recovered, na.rm = TRUE), NA_real_)
  )
}

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
  file.path(csv_dir, "bootstrap_clmm_by_archetype_results.csv")
)


# 9. Resumen
bootstrap_summary <- bootstrap_results %>%
  group_by(n_participants) %>%
  summarise(
    n_boot = n(),
    mean_archetypes_ok = mean(n_archetypes_ok, na.rm = TRUE),
    mean_terms_with_p = mean(n_terms_with_p, na.rm = TRUE),
    mean_significant_terms = mean(n_significant_terms, na.rm = TRUE),
    mean_recovery_rate = mean(recovery_rate, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  bootstrap_summary,
  file.path(csv_dir, "bootstrap_clmm_by_archetype_summary.csv")
)

print(bootstrap_summary)

cat("Bootstrap CLMM por arquetipo terminado\n")