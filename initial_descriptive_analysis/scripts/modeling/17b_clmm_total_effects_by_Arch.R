
# SCRIPT 17B - EFECTOS TOTALES CLMM POR ARQUETIPO Y DIMENSIÓN

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(tibble)


# 1. CARPETAS
base_output_dir <- "initial_descriptive_analysis/output/model_5arq_9dim"
csv_dir <- file.path(base_output_dir, "csv")


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

macro_archetypes <- c(
  "G1_Activist_Stubborn_Sentient",
  "G2_EarlyAdopter_Influencer",
  "G3_Fearful_Careful",
  "G4_HomoEconomicus",
  "G5_Uninterested"
)

reference_archetype <- "G1_Activist_Stubborn_Sentient"


# 3. CARGAR TÉRMINOS DEL CLMM
full_terms <- read_csv(
  file.path(csv_dir, "clmm_full_model_terms.csv"),
  show_col_types = FALSE
)

bootstrap_terms <- read_csv(
  file.path(csv_dir, "clmm_bootstrap_model_terms.csv"),
  show_col_types = FALSE
)

# 4. FUNCIÓN PARA CALCULAR EFECTOS TOTALES EN M2
compute_total_effects_m2 <- function(terms_df) {
  
  m2_terms <- terms_df %>%
    filter(model_type == "M2")
  
  key_cols <- c("sample_label", "n_participants", "boot_id")
  
  # Efectos principales de las dimensiones.
  # Estos son los efectos para el grupo de referencia:
  # G1_Activist_Stubborn_Sentient.
  main_effects <- m2_terms %>%
    filter(term %in% paste0("z_", dimensions)) %>%
    transmute(
      across(all_of(key_cols)),
      dimension = str_remove(term, "^z_"),
      main_effect = estimate,
      main_p_value = p.value
    )
  
  # Interacciones arquetipo x dimensión.
  # Estas indican cuánto cambia el efecto respecto al grupo de referencia.
  interaction_effects <- m2_terms %>%
    filter(str_detect(term, ":z_")) %>%
    mutate(
      dimension = str_match(term, ":z_([A-Z_]+)$")[, 2],
      macro_archetype_5 = term %>%
        str_remove("^macro_archetype_5") %>%
        str_remove(":z_[A-Z_]+$")
    ) %>%
    transmute(
      across(all_of(key_cols)),
      macro_archetype_5,
      dimension,
      interaction_effect = estimate,
      interaction_p_value = p.value
    )
  
  # Grid completo: cada muestra x cada arquetipo x cada dimensión
  sample_keys <- m2_terms %>%
    distinct(across(all_of(key_cols)))
  
  total_effects <- sample_keys %>%
    crossing(
      macro_archetype_5 = macro_archetypes,
      dimension = dimensions
    ) %>%
    left_join(
      main_effects,
      by = c(key_cols, "dimension")
    ) %>%
    left_join(
      interaction_effects,
      by = c(key_cols, "macro_archetype_5", "dimension")
    ) %>%
    mutate(
      interaction_effect = if_else(
        macro_archetype_5 == reference_archetype,
        0,
        interaction_effect
      ),
      total_effect = main_effect + interaction_effect,
      direction = case_when(
        total_effect > 0 ~ "positive",
        total_effect < 0 ~ "negative",
        TRUE ~ "zero_or_missing"
      ),
      interpretation = case_when(
        direction == "positive" ~ "Higher values in this dimension are associated with more advanced adoption stages",
        direction == "negative" ~ "Higher values in this dimension are associated with less advanced adoption stages",
        TRUE ~ "No clear interpretation"
      )
    ) %>%
    arrange(
      sample_label,
      n_participants,
      boot_id,
      macro_archetype_5,
      dimension
    )
  
  total_effects
}

# 5. EFECTOS TOTALES EN FULL
full_total_effects <- compute_total_effects_m2(full_terms)

write_csv(
  full_total_effects,
  file.path(csv_dir, "clmm_m2_full_total_effects_by_archetype_dimension.csv")
)

# Versión ancha para leerlo como tabla/heatmap
full_total_effects_wide <- full_total_effects %>%
  select(
    macro_archetype_5,
    dimension,
    total_effect
  ) %>%
  pivot_wider(
    names_from = dimension,
    values_from = total_effect
  )

write_csv(
  full_total_effects_wide,
  file.path(csv_dir, "clmm_m2_full_total_effects_wide.csv")
)


# 6. EFECTOS TOTALES EN BOOTSTRAP
bootstrap_total_effects <- compute_total_effects_m2(bootstrap_terms)

write_csv(
  bootstrap_total_effects,
  file.path(csv_dir, "clmm_m2_bootstrap_total_effects_by_archetype_dimension.csv")
)

# 7. RESUMEN DE ESTABILIDAD POR BOOTSTRAP
bootstrap_total_effects_summary <- bootstrap_total_effects %>%
  group_by(
    n_participants,
    macro_archetype_5,
    dimension
  ) %>%
  summarise(
    n_boot = n(),
    mean_total_effect = mean(total_effect, na.rm = TRUE),
    sd_total_effect = sd(total_effect, na.rm = TRUE),
    min_total_effect = min(total_effect, na.rm = TRUE),
    max_total_effect = max(total_effect, na.rm = TRUE),
    prop_positive = mean(total_effect > 0, na.rm = TRUE),
    prop_negative = mean(total_effect < 0, na.rm = TRUE),
    stable_direction = case_when(
      prop_positive >= 0.8 ~ "positive",
      prop_negative >= 0.8 ~ "negative",
      TRUE ~ "unstable"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    abs_mean_total_effect = abs(mean_total_effect)
  ) %>%
  arrange(
    n_participants,
    macro_archetype_5,
    desc(abs_mean_total_effect)
  )

write_csv(
  bootstrap_total_effects_summary,
  file.path(csv_dir, "clmm_m2_bootstrap_total_effects_summary.csv")
)


# 8. TOP DIMENSIONES POR ARQUETIPO
top_dimensions_by_archetype <- bootstrap_total_effects_summary %>%
  filter(
    stable_direction != "unstable"
  ) %>%
  group_by(
    n_participants,
    macro_archetype_5
  ) %>%
  slice_max(
    order_by = abs_mean_total_effect,
    n = 3,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  arrange(
    n_participants,
    macro_archetype_5,
    desc(abs_mean_total_effect)
  )

write_csv(
  top_dimensions_by_archetype,
  file.path(csv_dir, "clmm_m2_top_dimensions_by_archetype.csv")
)

# 9. RESUMEN M1: EFECTOS GENERALES DE DIMENSIONES
m1_dimension_effects_summary <- bootstrap_terms %>%
  filter(
    model_type == "M1",
    term %in% paste0("z_", dimensions)
  ) %>%
  mutate(
    dimension = str_remove(term, "^z_")
  ) %>%
  group_by(
    n_participants,
    dimension
  ) %>%
  summarise(
    n_boot = n(),
    mean_effect = mean(estimate, na.rm = TRUE),
    sd_effect = sd(estimate, na.rm = TRUE),
    mean_p_value = mean(p.value, na.rm = TRUE),
    significance_rate = mean(p.value < 0.05, na.rm = TRUE),
    prop_positive = mean(estimate > 0, na.rm = TRUE),
    prop_negative = mean(estimate < 0, na.rm = TRUE),
    stable_direction = case_when(
      prop_positive >= 0.8 ~ "positive",
      prop_negative >= 0.8 ~ "negative",
      TRUE ~ "unstable"
    ),
    .groups = "drop"
  ) %>%
  arrange(
    n_participants,
    desc(abs(mean_effect))
  )

write_csv(
  m1_dimension_effects_summary,
  file.path(csv_dir, "clmm_m1_dimension_effects_summary.csv")
)


# 10. PRINTS DE CONTROL
cat("\nEfectos totales M2 - FULL:\n")
print(full_total_effects_wide, n = Inf, width = Inf)

cat("\nResumen bootstrap efectos totales M2:\n")
print(bootstrap_total_effects_summary, n = 100, width = Inf)

cat("\nTop dimensiones por arquetipo:\n")
print(top_dimensions_by_archetype, n = Inf, width = Inf)

cat("Archivos creados:\n")
cat("- clmm_m2_full_total_effects_by_archetype_dimension.csv\n")
cat("- clmm_m2_full_total_effects_wide.csv\n")
cat("- clmm_m2_bootstrap_total_effects_by_archetype_dimension.csv\n")
cat("- clmm_m2_bootstrap_total_effects_summary.csv\n")
cat("- clmm_m2_top_dimensions_by_archetype.csv\n")
cat("- clmm_m1_dimension_effects_summary.csv\n")