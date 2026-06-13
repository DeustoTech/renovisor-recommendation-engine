
# SCRIPT 12 - MODELO ORDINAL TTM CON DETERMINANTES PONDERADOS POR DIMENSIÓN

#
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(purrr)
library(ordinal)
library(broom.mixed)


# 1. Carpetas
base_input_ttm <- "initial_descriptive_analysis/output/ttm_stage_analysis/csv"
base_input_data <- "initial_descriptive_analysis/output/data_preparation/csv"

base_output_dir <- "initial_descriptive_analysis/output/ordinal_model_weighted_determinants"

csv_dir <- file.path(base_output_dir, "csv")
models_dir <- file.path(base_output_dir, "models")
logs_dir <- file.path(base_output_dir, "logs")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)


# 2. Parámetros
SELECTED_WEIGHT <- 1
UNSELECTED_WEIGHT <- 0.25
NEUTRAL_VALUE <- 50

# 3. Cargar datos
df_det <- read_csv(
  file.path(base_input_ttm, "ttm_determinants_wide.csv"),
  show_col_types = FALSE
)

df_stage_dim <- read_csv(
  file.path(base_input_ttm, "ttm_stage_dimension_long.csv"),
  show_col_types = FALSE
)

mapping_long <- read_csv(
  file.path(base_input_ttm, "dimension_determinant_mapping_long.csv"),
  show_col_types = FALSE
) %>%
  filter(is_linked == 1) %>%
  select(dimension_key, determinant_id)

df_profile <- read_csv(
  file.path(base_input_data, "df_analysis_ready.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    participant_id = coalesce(
      as.character(join_key),
      as.character(prolific_id),
      as.character(identification_code),
      as.character(row_number())
    )
  )


# 4. Crear arquetipo
self_col <- "which_statement_best_describes_you_when_making_an_investment_decision_related_to_your_household_final"

df_archetype <- df_profile %>%
  mutate(
    self_response_raw = str_squish(as.character(.data[[self_col]])),
    self_response_raw = na_if(self_response_raw, ""),
    arquetipo = case_when(
      is.na(self_response_raw) ~ "Missing",
      str_detect(self_response_raw, regex("environmental impact", ignore_case = TRUE)) ~ "Activist",
      str_detect(self_response_raw, regex("safety", ignore_case = TRUE)) ~ "Fearful",
      str_detect(self_response_raw, regex("social status", ignore_case = TRUE)) ~ "Influencer",
      str_detect(self_response_raw, regex("comfort", ignore_case = TRUE)) ~ "Careful",
      str_detect(self_response_raw, regex("not very interested", ignore_case = TRUE)) ~ "Uninterested",
      str_detect(self_response_raw, regex("early adopter", ignore_case = TRUE)) ~ "Early Adopter",
      str_detect(self_response_raw, regex("ethical", ignore_case = TRUE)) ~ "Sentient",
      str_detect(self_response_raw, regex("cost-effective", ignore_case = TRUE)) ~ "Homo Economicus",
      str_detect(self_response_raw, regex("[NΝ]one of the above", ignore_case = TRUE)) ~ "Unclassified",
      TRUE ~ "Other"
    )
  ) %>%
  select(participant_id, arquetipo)


# 5. Preparar observaciones persona-etapa-tecnología
determinant_ids <- setdiff(names(df_det), "participant_id")

stage_obs <- df_stage_dim %>%
  filter(
    stage %in% c(
      "No la conoce, pero le genera curiosidad",
      "La conoce / la consideraría",
      "Implementada"
    ),
    !is.na(dimension_key)
  ) %>%
  mutate(
    dimension_key = str_to_upper(dimension_key)
  ) %>%
  group_by(participant_id, stage, technology) %>%
  summarise(
    selected_dimensions = list(unique(dimension_key)),
    n_selected_dimensions = n_distinct(dimension_key),
    .groups = "drop"
  ) %>%
  mutate(
    obs_id = row_number(),
    Estado_Mental = case_when(
      stage == "No la conoce, pero le genera curiosidad" ~ "Fase 1 - Curiosidad",
      stage == "La conoce / la consideraría" ~ "Fase 2 - Consideración",
      stage == "Implementada" ~ "Fase 3 - Implementada"
    ),
    Estado_Mental = ordered(
      Estado_Mental,
      levels = c(
        "Fase 1 - Curiosidad",
        "Fase 2 - Consideración",
        "Fase 3 - Implementada"
      )
    )
  )

# 6. Determinantes activos según dimensiones seleccionadas
selected_dims_long <- stage_obs %>%
  select(obs_id, selected_dimensions) %>%
  unnest_longer(selected_dimensions, values_to = "dimension_key") %>%
  mutate(dimension_key = str_to_upper(dimension_key))

active_determinants <- selected_dims_long %>%
  inner_join(mapping_long, by = "dimension_key") %>%
  distinct(obs_id, determinant_id) %>%
  mutate(is_selected_dimension_determinant = TRUE)


# 7. Construir dataset ponderado
model_base <- stage_obs %>%
  left_join(df_det, by = "participant_id") %>%
  left_join(df_archetype, by = "participant_id") %>%
  filter(
    !is.na(arquetipo),
    !arquetipo %in% c("Missing", "Other", "Unclassified")
  ) %>%
  mutate(
    arquetipo = factor(arquetipo),
    across(all_of(determinant_ids), as.numeric)
  )

model_long <- model_base %>%
  pivot_longer(
    cols = all_of(determinant_ids),
    names_to = "determinant_id",
    values_to = "determinant_value_raw"
  ) %>%
  left_join(
    active_determinants,
    by = c("obs_id", "determinant_id")
  ) %>%
  mutate(
    is_selected_dimension_determinant = replace_na(is_selected_dimension_determinant, FALSE),
    determinant_value_imputed = if_else(
      is.na(determinant_value_raw),
      NEUTRAL_VALUE,
      determinant_value_raw
    ),
    determinant_weight = if_else(
      is_selected_dimension_determinant,
      SELECTED_WEIGHT,
      UNSELECTED_WEIGHT
    ),
    determinant_value_weighted = determinant_value_imputed * determinant_weight
  )

write_csv(
  model_long,
  file.path(csv_dir, "model_long_weighted_determinants.csv")
)

model_wide <- model_long %>%
  select(
    obs_id,
    participant_id,
    Estado_Mental,
    stage,
    technology,
    arquetipo,
    n_selected_dimensions,
    determinant_id,
    determinant_value_weighted
  ) %>%
  pivot_wider(
    names_from = determinant_id,
    values_from = determinant_value_weighted,
    names_prefix = "w_"
  )

weighted_determinants <- paste0("w_", determinant_ids)

# Escalar determinantes ponderados
model_wide_scaled <- model_wide %>%
  mutate(
    across(
      all_of(weighted_determinants),
      ~ as.numeric(scale(.x)),
      .names = "z_{.col}"
    )
  )

z_weighted_determinants <- paste0("z_", weighted_determinants)

write_csv(
  model_wide_scaled,
  file.path(csv_dir, "model_input_weighted_determinants_scaled.csv")
)

cat("Filas modelo:", nrow(model_wide_scaled), "\n")
cat("Participantes:", n_distinct(model_wide_scaled$participant_id), "\n")
cat("Determinantes ponderados:", length(weighted_determinants), "\n")

# 8. Modelo principal CLMM
# formula_main <- as.formula(
#   paste(
#     "Estado_Mental ~ arquetipo +",
#     paste(z_weighted_determinants, collapse = " + "),
#     "+ (1 | participant_id)"
#   )
# )

# formula_main <- as.formula(
#   paste(
#     "Estado_Mental ~",
#     paste(z_weighted_determinants, collapse = " + "),
#     "+ (1 | participant_id)"
#   )
# )
formula_main <- as.formula(
  paste(
    "Estado_Mental ~",
    paste(z_weighted_determinants, collapse = " + ")
  )
)

# modelo_main <- clmm(
#   formula_main,
#   data = model_wide_scaled,
#   link = "logit",
#   Hess = TRUE,
#   nAGQ = 1,
#   control = clmm.control(
#     maxIter = 200,
#     gradTol = 1e-4
#   )
# )
modelo_main <- clm(
   formula_main,
   data = model_wide_scaled,
   link = "logit",
   Hess = TRUE
 )

summary(modelo_main)

saveRDS(
  modelo_main,
  file.path(models_dir, "modelo_clmm_weighted_32det_main.rds")
)

results_main <- broom.mixed::tidy(
  modelo_main,
  effects = "fixed"
) %>%
  mutate(
    odds_ratio = exp(estimate),
    p_adj_BH = p.adjust(p.value, method = "BH"),
    significance = case_when(
      p_adj_BH < 0.001 ~ "***",
      p_adj_BH < 0.01 ~ "**",
      p_adj_BH < 0.05 ~ "*",
      p_adj_BH < 0.1 ~ ".",
      TRUE ~ "ns"
    )
  )

write_csv(
  results_main,
  file.path(csv_dir, "modelo_clmm_weighted_32det_main_results.csv")
)


# 9. Modelo con interacciones ponderadas × arquetipo
# formula_interactions <- as.formula(
#   paste(
#     "Estado_Mental ~",
#     paste(paste0(z_weighted_determinants, " + arquetipo"), collapse = " + "),
#     "+ (1 | participant_id)"
#   )
# )
# 
# modelo_interactions <- tryCatch(
#   clmm(
#     formula_interactions,
#     data = model_wide_scaled,
#     link = "logit",
#     Hess = TRUE,
#     nAGQ = 1,
#     control = clmm.control(
#       maxIter = 200,
#       gradTol = 1e-4
#     )
#   ),
#   error = function(e) {
#     message("El modelo completo con interacciones no ha convergido: ", e$message)
#     NULL
#   }
# )
# 
# if (!is.null(modelo_interactions)) {
#   
#   saveRDS(
#     modelo_interactions,
#     file.path(models_dir, "modelo_clmm_weighted_32det_interactions.rds")
#   )
#   
#   results_interactions <- broom.mixed::tidy(
#     modelo_interactions,
#     effects = "fixed"
#   ) %>%
#     mutate(
#       odds_ratio = exp(estimate),
#       p_adj_BH = p.adjust(p.value, method = "BH"),
#       significance = case_when(
#         p_adj_BH < 0.001 ~ "***",
#         p_adj_BH < 0.01 ~ "**",
#         p_adj_BH < 0.05 ~ "*",
#         p_adj_BH < 0.1 ~ ".",
#         TRUE ~ "ns"
#       )
#     )
#   
#   write_csv(
#     results_interactions,
#     file.path(csv_dir, "modelo_clmm_weighted_32det_interactions_results.csv")
#   )
# }


# 10. Log final
summary_log <- tibble(
  n_rows_model = nrow(model_wide_scaled),
  n_participants = n_distinct(model_wide_scaled$participant_id),
  n_archetypes = n_distinct(model_wide_scaled$arquetipo),
  n_determinants = length(determinant_ids),
  selected_weight = SELECTED_WEIGHT,
  unselected_weight = UNSELECTED_WEIGHT,
  neutral_value_for_na = NEUTRAL_VALUE,
  main_model_estimated = TRUE
)

write_csv(
  summary_log,
  file.path(logs_dir, "weighted_ordinal_model_summary_log.csv")
)

cat("Modelo ordinal ponderado guardado en:", base_output_dir, "\n")

cat("df_profile participantes:", n_distinct(df_profile$participant_id), "\n")
cat("df_stage_dim participantes:", n_distinct(df_stage_dim$participant_id), "\n")
cat("stage_obs participantes:", n_distinct(stage_obs$participant_id), "\n")
cat("model_base participantes:", n_distinct(model_base$participant_id), "\n")
cat("model_wide_scaled participantes:", n_distinct(model_wide_scaled$participant_id), "\n")

