# ==============================================================================
# SCRIPT 03.2 - CUOTAS DE MUESTRA POR PAÍS Y REGIÓN
# ESCENARIO REAL: MUESTRA ACTUAL + 181 NUEVAS RESPUESTAS
# ==============================================================================

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(tibble)

# ==============================================================================
# RUTAS
# ==============================================================================

base_input_dir <- "initial_descriptive_analysis/output/clean_datasets"
quota_output_dir <- "initial_descriptive_analysis/output/sample_quotas"

dir.create(quota_output_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# CARGAR DATASET LIMPIO
# ==============================================================================

df_quota <- read_csv(
  file.path(base_input_dir, "df_clean_sociodemographic.csv"),
  show_col_types = FALSE
)

cat("Dataset cargado: df_clean_sociodemographic.csv\n")
cat("Filas:", nrow(df_quota), "\n")
cat("Columnas:", ncol(df_quota), "\n")

# ==============================================================================
# FUNCIONES AUXILIARES
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

clean_country_quota <- function(x) {
  x <- clean_text_basic(x)
  x <- str_remove(x, "\\s*\\(ID[0-9]+\\)$")
  x <- str_remove(x, "^[A-Z]{2}\\s*[–-]\\s*")
  
  x <- case_when(
    is.na(x) | x == "" ~ NA_character_,
    x == "United Kingdom *" ~ "United Kingdom",
    x == "UK" ~ "United Kingdom",
    x == "Great Britain" ~ "United Kingdom",
    x == "Czechia" ~ "Czech Republic",
    TRUE ~ x
  )
  
  x
}

clean_residence_region <- function(country) {
  case_when(
    country %in% c(
      "Denmark", "Estonia", "Finland", "Ireland", "Iceland",
      "Latvia", "Lithuania", "Norway", "United Kingdom", "Sweden"
    ) ~ "Europa del Norte",
    
    country %in% c(
      "Germany", "Austria", "Belgium", "France", "Liechtenstein",
      "Luxembourg", "Monaco", "Netherlands", "Switzerland"
    ) ~ "Europa Occidental",
    
    country %in% c(
      "Albania", "Andorra", "Bosnia and Herzegovina", "Croatia",
      "Slovenia", "Spain", "Greece", "Italy", "Malta",
      "Montenegro", "Portugal", "North Macedonia", "San Marino",
      "Serbia", "Cyprus"
    ) ~ "Europa del Sur",
    
    country %in% c(
      "Belarus", "Bulgaria", "Slovakia", "Hungary",
      "Moldova", "Moldova (Republic of Moldova)",
      "Poland", "Czech Republic", "Czechia",
      "Romania", "Russia", "Russian Federation", "Ukraine"
    ) ~ "Europa del Este",
    
    is.na(country) ~ NA_character_,
    TRUE ~ "Otra región"
  )
}

# ==============================================================================
# PREPARAR PAÍS Y REGIÓN
# ==============================================================================

country_col <- "in_which_country_do_you_currently_live_final"

df_quota_input <- df_quota

df_quota <- df_quota %>%
  mutate(
    participant_id = coalesce(
      as.character(get_optional_col(., "participant_id")),
      as.character(get_optional_col(., "join_key")),
      as.character(get_optional_col(., "prolific_id")),
      as.character(get_optional_col(., "identification_code")),
      as.character(row_number())
    ),
    country_raw = get_optional_col(., country_col),
    country_clean_existing = get_optional_col(., "country_clean"),
    country_clean = coalesce(
      clean_country_quota(country_clean_existing),
      clean_country_quota(country_raw)
    ),
    residence_region_existing = get_optional_col(., "residence_region"),
    residence_region = coalesce(
      clean_text_basic(residence_region_existing),
      clean_residence_region(country_clean)
    )
  ) %>%
  distinct(participant_id, .keep_all = TRUE)

quota_country_filter_summary <- tibble(
  dataset = c(
    "df_clean_sociodemographic_before_country_filter",
    "usable_for_country_quotas",
    "excluded_from_country_quotas_no_country"
  ),
  n_rows = c(
    nrow(df_quota),
    sum(!is.na(df_quota$country_clean)),
    sum(is.na(df_quota$country_clean))
  )
)

print(quota_country_filter_summary)

df_quota <- df_quota %>%
  filter(!is.na(country_clean))

cat("Filas usables para cuotas:", nrow(df_quota), "\n")

# ==============================================================================
# DISTRIBUCIÓN ACTUAL
# ==============================================================================

country_distribution_clean <- df_quota %>%
  count(country_clean, name = "current_n") %>%
  mutate(current_pct = round(current_n / sum(current_n) * 100, 2)) %>%
  arrange(desc(current_n))

region_distribution_clean <- df_quota %>%
  count(residence_region, name = "current_n") %>%
  mutate(current_pct = round(current_n / sum(current_n) * 100, 2)) %>%
  arrange(desc(current_n))

print(country_distribution_clean, n = Inf)
print(region_distribution_clean, n = Inf)

# ==============================================================================
# DISTRIBUCIÓN TEÓRICA OBJETIVO POR PAÍS
# ==============================================================================

target_country <- tribble(
  ~country, ~target_n,
  "Germany", 20,
  "France", 20,
  "United Kingdom", 20,
  "Italy", 20,
  "Spain", 15,
  "Poland", 15,
  "Romania", 5,
  "Netherlands", 5,
  "Belgium", 5,
  "Czech Republic", 5,
  "Sweden", 5,
  "Portugal", 5,
  "Greece", 5,
  "Hungary", 3,
  "Austria", 3,
  "Bulgaria", 3,
  "Denmark", 3,
  "Finland", 3,
  "Slovakia", 3,
  "Ireland", 3,
  "Croatia", 3,
  "Lithuania", 3,
  "Slovenia", 3,
  "Latvia", 3,
  "Estonia", 3,
  "Cyprus", 0,
  "Luxembourg", 0,
  "Malta", 0
)

target_total_original <- sum(target_country$target_n)

cat("Total teórico original:", target_total_original, "\n")

# ==============================================================================
# ESCENARIO REAL: MUESTRA ACTUAL + 181 NUEVAS RESPUESTAS
# ==============================================================================

additional_n_available <- 181
current_total_n <- nrow(df_quota)
selected_total_n <- current_total_n + additional_n_available

cat("\nMuestra actual usable:", current_total_n, "\n")
cat("Nuevas respuestas disponibles:", additional_n_available, "\n")
cat("Tamaño final objetivo:", selected_total_n, "\n")

desired_sample_sizes <- c(
  selected_total_n,
  300,
  500,
  1000
) %>%
  unique() %>%
  sort()

# ==============================================================================
# FUNCIÓN PARA ESCALAR CUOTAS
# ==============================================================================

scale_quotas <- function(target_country, desired_n) {
  
  base <- target_country %>%
    mutate(
      original_total_n = sum(target_n),
      desired_total_n = desired_n,
      target_pct = target_n / original_total_n,
      target_n_raw = target_pct * desired_total_n,
      target_n_floor = floor(target_n_raw),
      remainder = target_n_raw - target_n_floor
    )
  
  remaining_n <- desired_n - sum(base$target_n_floor)
  
  base <- base %>%
    arrange(desc(remainder), desc(target_n), country) %>%
    mutate(
      extra_unit = if_else(row_number() <= remaining_n, 1L, 0L),
      target_n_scaled = target_n_floor + extra_unit
    ) %>%
    arrange(country) %>%
    select(
      desired_total_n,
      country,
      target_pct,
      target_n_original = target_n,
      target_n_scaled
    )
  
  base
}

quota_targets_scaled <- bind_rows(
  lapply(desired_sample_sizes, function(n) {
    scale_quotas(target_country, n)
  })
)

print(quota_targets_scaled, n = Inf)

# ==============================================================================
# COMPARACIÓN ACTUAL VS OBJETIVO POR PAÍS
# ==============================================================================

quota_check_scenarios <- quota_targets_scaled %>%
  left_join(
    country_distribution_clean %>%
      select(country_clean, current_n),
    by = c("country" = "country_clean")
  ) %>%
  mutate(
    current_n = replace_na(current_n, 0),
    current_pct_for_desired_n = round(current_n / desired_total_n * 100, 2),
    difference_n = target_n_scaled - current_n,
    recruit_next_n = pmax(difference_n, 0),
    overrepresented_n = pmax(current_n - target_n_scaled, 0),
    keep_for_balanced_sample_n = pmin(current_n, target_n_scaled),
    status = case_when(
      current_n < target_n_scaled ~ "Faltan participantes",
      current_n == target_n_scaled ~ "Cuota cubierta",
      current_n > target_n_scaled ~ "Sobrerrepresentado"
    )
  ) %>%
  arrange(desired_total_n, desc(recruit_next_n), country)

quota_check <- quota_check_scenarios %>%
  filter(desired_total_n == selected_total_n)

quota_check_scenarios_wide <- quota_check_scenarios %>%
  select(
    country,
    desired_total_n,
    target_n_scaled,
    current_n,
    recruit_next_n,
    overrepresented_n,
    keep_for_balanced_sample_n,
    status
  ) %>%
  pivot_wider(
    names_from = desired_total_n,
    values_from = c(
      target_n_scaled,
      current_n,
      recruit_next_n,
      overrepresented_n,
      keep_for_balanced_sample_n,
      status
    ),
    names_glue = "{.value}_{desired_total_n}"
  )

countries_outside_target <- country_distribution_clean %>%
  anti_join(target_country, by = c("country_clean" = "country")) %>%
  arrange(desc(current_n))

# ==============================================================================
# DISTRIBUCIÓN OBJETIVO POR REGIÓN
# ==============================================================================

region_targets_scaled <- quota_targets_scaled %>%
  mutate(residence_region = clean_residence_region(country)) %>%
  group_by(desired_total_n, residence_region) %>%
  summarise(
    target_n_original = sum(target_n_original),
    target_n_scaled = sum(target_n_scaled),
    .groups = "drop"
  ) %>%
  mutate(target_pct = round(target_n_scaled / desired_total_n * 100, 2)) %>%
  arrange(desired_total_n, residence_region)

# ==============================================================================
# COMPARACIÓN ACTUAL VS OBJETIVO POR REGIÓN
# ==============================================================================

region_quota_check_scenarios <- region_targets_scaled %>%
  left_join(
    region_distribution_clean %>%
      select(residence_region, current_n),
    by = "residence_region"
  ) %>%
  mutate(
    current_n = replace_na(current_n, 0),
    current_pct_for_desired_n = round(current_n / desired_total_n * 100, 2),
    difference_n = target_n_scaled - current_n,
    recruit_next_n = pmax(difference_n, 0),
    overrepresented_n = pmax(current_n - target_n_scaled, 0),
    keep_for_balanced_sample_n = pmin(current_n, target_n_scaled),
    status = case_when(
      current_n < target_n_scaled ~ "Faltan participantes",
      current_n == target_n_scaled ~ "Cuota cubierta",
      current_n > target_n_scaled ~ "Sobrerrepresentado"
    )
  ) %>%
  arrange(desired_total_n, desc(recruit_next_n), residence_region)

region_quota_check <- region_quota_check_scenarios %>%
  filter(desired_total_n == selected_total_n)

region_quota_check_scenarios_wide <- region_quota_check_scenarios %>%
  select(
    residence_region,
    desired_total_n,
    target_n_scaled,
    current_n,
    recruit_next_n,
    overrepresented_n,
    keep_for_balanced_sample_n,
    status
  ) %>%
  pivot_wider(
    names_from = desired_total_n,
    values_from = c(
      target_n_scaled,
      current_n,
      recruit_next_n,
      overrepresented_n,
      keep_for_balanced_sample_n,
      status
    ),
    names_glue = "{.value}_{desired_total_n}"
  )

# ==============================================================================
# REPARTO REALISTA DE LAS 181 NUEVAS RESPUESTAS POR REGIÓN
# ==============================================================================

region_recruitment_plan_181 <- region_quota_check %>%
  filter(recruit_next_n > 0) %>%
  mutate(
    deficit_weight = recruit_next_n / sum(recruit_next_n),
    recruit_raw_181 = deficit_weight * additional_n_available,
    recruit_floor_181 = floor(recruit_raw_181),
    remainder = recruit_raw_181 - recruit_floor_181
  ) %>%
  arrange(desc(remainder)) %>%
  mutate(
    extra_unit = if_else(
      row_number() <= additional_n_available - sum(recruit_floor_181),
      1L,
      0L
    ),
    recruit_planned_181 = recruit_floor_181 + extra_unit
  ) %>%
  arrange(desc(recruit_planned_181)) %>%
  select(
    residence_region,
    current_n,
    target_n_scaled,
    recruit_next_n,
    recruit_planned_181
  )

print(region_recruitment_plan_181)

# ==============================================================================
# REPARTO REALISTA DE LAS 181 NUEVAS RESPUESTAS POR PAÍS
# ==============================================================================

country_recruitment_plan_181 <- quota_check %>%
  filter(recruit_next_n > 0) %>%
  mutate(
    deficit_weight = recruit_next_n / sum(recruit_next_n),
    recruit_raw_181 = deficit_weight * additional_n_available,
    recruit_floor_181 = floor(recruit_raw_181),
    remainder = recruit_raw_181 - recruit_floor_181
  ) %>%
  arrange(desc(remainder)) %>%
  mutate(
    extra_unit = if_else(
      row_number() <= additional_n_available - sum(recruit_floor_181),
      1L,
      0L
    ),
    recruit_planned_181 = recruit_floor_181 + extra_unit
  ) %>%
  arrange(desc(recruit_planned_181)) %>%
  select(
    country,
    current_n,
    target_n_scaled,
    recruit_next_n,
    recruit_planned_181
  )

print(country_recruitment_plan_181)

# ==============================================================================
# RESUMEN EJECUTIVO
# ==============================================================================

quota_summary <- tibble(
  selected_total_n = selected_total_n,
  current_total_usable = current_total_n,
  additional_n_available = additional_n_available,
  rows_in_clean_sociodemographic = nrow(df_quota_input),
  usable_rows_for_country_quota = nrow(df_quota),
  excluded_rows_without_country = nrow(df_quota_input) - nrow(df_quota),
  current_total_in_target_countries = sum(quota_check$current_n),
  current_total_outside_target_countries = sum(countries_outside_target$current_n),
  total_to_recruit_by_country_to_cover_deficits = sum(quota_check$recruit_next_n),
  total_planned_recruitment_by_country = sum(country_recruitment_plan_181$recruit_planned_181),
  total_overrepresented_by_country_if_balanced = sum(quota_check$overrepresented_n),
  total_to_recruit_by_region_to_cover_deficits = sum(region_quota_check$recruit_next_n),
  total_planned_recruitment_by_region = sum(region_recruitment_plan_181$recruit_planned_181),
  total_overrepresented_by_region_if_balanced = sum(region_quota_check$overrepresented_n)
)

print(quota_summary)

# ==============================================================================
# GUARDAR CSVs
# ==============================================================================

write_csv(df_quota, file.path(quota_output_dir, "df_quota_base_usable.csv"))

write_csv(country_distribution_clean, file.path(quota_output_dir, "country_distribution_current_clean.csv"))
write_csv(region_distribution_clean, file.path(quota_output_dir, "region_distribution_current_clean.csv"))

write_csv(quota_targets_scaled, file.path(quota_output_dir, "target_distribution_scaled_by_country_scenarios.csv"))
write_csv(region_targets_scaled, file.path(quota_output_dir, "target_distribution_scaled_by_region_scenarios.csv"))

write_csv(quota_check, file.path(quota_output_dir, "country_quota_check_selected_total.csv"))
write_csv(quota_check_scenarios, file.path(quota_output_dir, "country_quota_check_scenarios.csv"))
write_csv(quota_check_scenarios_wide, file.path(quota_output_dir, "country_quota_check_scenarios_wide.csv"))

write_csv(region_quota_check, file.path(quota_output_dir, "region_quota_check_selected_total.csv"))
write_csv(region_quota_check_scenarios, file.path(quota_output_dir, "region_quota_check_scenarios.csv"))
write_csv(region_quota_check_scenarios_wide, file.path(quota_output_dir, "region_quota_check_scenarios_wide.csv"))

write_csv(region_recruitment_plan_181, file.path(quota_output_dir, "region_recruitment_plan_181_new_responses.csv"))
write_csv(country_recruitment_plan_181, file.path(quota_output_dir, "country_recruitment_plan_181_new_responses.csv"))

write_csv(countries_outside_target, file.path(quota_output_dir, "countries_outside_target.csv"))
write_csv(quota_summary, file.path(quota_output_dir, "quota_summary.csv"))
write_csv(quota_country_filter_summary, file.path(quota_output_dir, "quota_country_filter_summary.csv"))

# ==============================================================================
# GRÁFICOS
# ==============================================================================

plot_region_recruitment_plan_181 <- region_recruitment_plan_181 %>%
  mutate(residence_region = reorder(residence_region, recruit_planned_181)) %>%
  ggplot(aes(x = residence_region, y = recruit_planned_181)) +
  geom_col(fill = "#56B4E9", color = "#2C3E50") +
  coord_flip() +
  labs(
    title = "Reparto recomendado de las 181 nuevas respuestas por región",
    subtitle = paste0("Escenario final objetivo: N = ", selected_total_n),
    x = NULL,
    y = "Participantes a reclutar"
  ) +
  theme_minimal(base_size = 12)

print(plot_region_recruitment_plan_181)

ggsave(
  file.path(quota_output_dir, "region_recruitment_plan_181_new_responses.png"),
  plot_region_recruitment_plan_181,
  width = 8,
  height = 5,
  dpi = 300
)

plot_country_recruitment_plan_181 <- country_recruitment_plan_181 %>%
  filter(recruit_planned_181 > 0) %>%
  mutate(country = reorder(country, recruit_planned_181)) %>%
  ggplot(aes(x = country, y = recruit_planned_181)) +
  geom_col(fill = "#56B4E9", color = "#2C3E50") +
  coord_flip() +
  labs(
    title = "Reparto recomendado de las 181 nuevas respuestas por país",
    subtitle = paste0("Escenario final objetivo: N = ", selected_total_n),
    x = NULL,
    y = "Participantes a reclutar"
  ) +
  theme_minimal(base_size = 12)

print(plot_country_recruitment_plan_181)

ggsave(
  file.path(quota_output_dir, "country_recruitment_plan_181_new_responses.png"),
  plot_country_recruitment_plan_181,
  width = 9,
  height = 7,
  dpi = 300
)

plot_region_overrepresented <- region_quota_check %>%
  filter(overrepresented_n > 0) %>%
  mutate(residence_region = reorder(residence_region, overrepresented_n)) %>%
  ggplot(aes(x = residence_region, y = overrepresented_n)) +
  geom_col(fill = "#E69F00", color = "#2C3E50") +
  coord_flip() +
  labs(
    title = paste0("Regiones sobrerrepresentadas respecto a la cuota para N = ", selected_total_n),
    subtitle = "Participantes que exceden la distribución objetivo",
    x = NULL,
    y = "Participantes por encima de la cuota"
  ) +
  theme_minimal(base_size = 12)

print(plot_region_overrepresented)

ggsave(
  file.path(quota_output_dir, "region_overrepresented_selected_total.png"),
  plot_region_overrepresented,
  width = 8,
  height = 5,
  dpi = 300
)

# ==============================================================================
# COMPROBACIONES FINALES
# ==============================================================================

cat("\n============================================================\n")
cat("RESUMEN FINAL\n")
cat("============================================================\n")

cat("\nMuestra actual usable:", current_total_n, "\n")
cat("Nuevas respuestas disponibles:", additional_n_available, "\n")
cat("Tamaño final objetivo:", selected_total_n, "\n")

cat("\nDistribución actual por región:\n")
print(region_distribution_clean, n = Inf)

cat("\nPlan recomendado por región para las 181 nuevas respuestas:\n")
print(region_recruitment_plan_181, n = Inf)

cat("\nPlan recomendado por país para las 181 nuevas respuestas:\n")
print(country_recruitment_plan_181, n = Inf)

cat("\nArchivos principales generados en:\n")
cat(quota_output_dir, "\n")

cat("\nArchivos clave para enviar/revisar:\n")
cat("- region_recruitment_plan_181_new_responses.csv\n")
cat("- country_recruitment_plan_181_new_responses.csv\n")
cat("- quota_summary.csv\n")