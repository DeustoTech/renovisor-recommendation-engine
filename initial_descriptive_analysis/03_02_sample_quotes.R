
# SCRIPT 03.2 - CUOTAS DE MUESTRA POR PAÍS Y REGIÓN

# Este script calcula la distribución actual de la muestra limpia frente a la
# distribución teórica objetivo.
#
# Parte del dataset limpio sociodemográfico generado en el Script 03:
#   initial_descriptive_analysis/output/clean_datasets/df_clean_sociodemographic.csv
#
# Genera:
# 1. Distribución actual por país.
# 2. Distribución actual por región europea.
# 3. Distribución objetivo escalada para distintos tamaños de muestra:
#    181, 300, 500 y 1000.
# 4. Comparación actual vs objetivo por país.
# 5. Comparación actual vs objetivo por región.
# 6. CSVs en formato largo y ancho.
# 7. Gráficos de participantes a reclutar y sobrerrepresentación.


# LIBRERÍAS
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(tibble)

# RUTAS
base_input_dir <- "initial_descriptive_analysis/output/clean_datasets"
quota_output_dir <- "initial_descriptive_analysis/output/sample_quotas"

dir.create(quota_output_dir, recursive = TRUE, showWarnings = FALSE)

# CARGAR DATASET LIMPIO
df_quota <- read_csv(
  file.path(base_input_dir, "df_clean_sociodemographic.csv"),
  show_col_types = FALSE
)

cat("Dataset cargado: df_clean_sociodemographic.csv\n")
cat("Filas:", nrow(df_quota), "\n")
cat("Columnas:", ncol(df_quota), "\n")

# FUNCIONES AUXILIARES
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
  
  # Quitar sufijo tipo "(ID860)"
  x <- str_remove(x, "\\s*\\(ID[0-9]+\\)$")
  
  # Quitar prefijo tipo "ES – ", "GB - ", "DE – "
  x <- str_remove(x, "^[A-Z]{2}\\s*[–-]\\s*")
  
  # Normalizaciones
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

clean_filename <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[áàäâ]", "a") %>%
    str_replace_all("[éèëê]", "e") %>%
    str_replace_all("[íìïî]", "i") %>%
    str_replace_all("[óòöô]", "o") %>%
    str_replace_all("[úùüû]", "u") %>%
    str_replace_all("ñ", "n") %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

# PREPARAR PAÍS Y REGIÓN
country_col <- "in_which_country_do_you_currently_live_final"

# Mantener una copia antes de filtrar país
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
    
    # Si country_clean ya viene del Script 03, se usa.
    # Si no, se reconstruye desde la columna original de país.
    country_raw = get_optional_col(., country_col),
    country_clean_existing = get_optional_col(., "country_clean"),
    
    country_clean = coalesce(
      clean_country_quota(country_clean_existing),
      clean_country_quota(country_raw)
    ),
    
    # Si residence_region ya viene del Script 03, se usa.
    # Si no, se reconstruye desde country_clean.
    residence_region_existing = get_optional_col(., "residence_region"),
    
    residence_region = coalesce(
      clean_text_basic(residence_region_existing),
      clean_residence_region(country_clean)
    )
  ) %>%
  distinct(participant_id, .keep_all = TRUE)

# Filas con y sin país dentro del dataset sociodemográfico
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

# Para cuotas por país/región solo pueden entrar personas con país
df_quota <- df_quota %>%
  filter(!is.na(country_clean))

cat("Filas usables para cuotas:", nrow(df_quota), "\n")

# DISTRIBUCIÓN ACTUAL POR PAÍS
country_distribution_clean <- df_quota %>%
  count(country_clean, name = "current_n") %>%
  mutate(
    current_pct = round(current_n / sum(current_n) * 100, 2)
  ) %>%
  arrange(desc(current_n))

print(country_distribution_clean, n = Inf)

# DISTRIBUCIÓN ACTUAL POR REGIÓN
region_distribution_clean <- df_quota %>%
  count(residence_region, name = "current_n") %>%
  mutate(
    current_pct = round(current_n / sum(current_n) * 100, 2)
  ) %>%
  arrange(desc(current_n))

print(region_distribution_clean, n = Inf)


# DISTRIBUCIÓN TEÓRICA OBJETIVO POR PAÍS
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

# ESCALAR CUOTAS A DISTINTOS TAMAÑOS DE MUESTRA
# Cambia este valor para elegir el escenario principal
selected_total_n <- 500

# Escenarios que se calculan en paralelo
desired_sample_sizes <- c(181, 300, 500, 1000)

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
  
  return(base)
}

quota_targets_scaled <- bind_rows(
  lapply(desired_sample_sizes, function(n) {
    scale_quotas(target_country, n)
  })
)

print(quota_targets_scaled, n = Inf)


# COMPARACIÓN ACTUAL VS OBJETIVO POR PAÍS
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

print(quota_check_scenarios, n = Inf)
print(quota_check_scenarios_wide, n = Inf)

# PAÍSES PRESENTES EN DATOS PERO FUERA DE LA DISTRIBUCIÓN OBJETIVO
countries_outside_target <- country_distribution_clean %>%
  anti_join(
    target_country,
    by = c("country_clean" = "country")
  ) %>%
  arrange(desc(current_n))

print(countries_outside_target, n = Inf)


# DISTRIBUCIÓN OBJETIVO POR REGIÓN
region_targets_scaled <- quota_targets_scaled %>%
  mutate(
    residence_region = clean_residence_region(country)
  ) %>%
  group_by(desired_total_n, residence_region) %>%
  summarise(
    target_n_original = sum(target_n_original),
    target_n_scaled = sum(target_n_scaled),
    .groups = "drop"
  ) %>%
  mutate(
    target_pct = round(target_n_scaled / desired_total_n * 100, 2)
  ) %>%
  arrange(desired_total_n, residence_region)

print(region_targets_scaled, n = Inf)


# COMPARACIÓN ACTUAL VS OBJETIVO POR REGIÓN
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

print(region_quota_check_scenarios, n = Inf)
print(region_quota_check_scenarios_wide, n = Inf)

# RESUMEN EJECUTIVO
quota_summary <- tibble(
  selected_total_n = selected_total_n,
  rows_in_clean_sociodemographic = nrow(df_quota_input),
  usable_rows_for_country_quota = nrow(df_quota),
  excluded_rows_without_country = nrow(df_quota_input) - nrow(df_quota),
  current_total_in_target_countries = sum(quota_check$current_n),
  current_total_outside_target_countries = sum(countries_outside_target$current_n),
  total_to_recruit_by_country_to_cover_deficits = sum(quota_check$recruit_next_n),
  total_overrepresented_by_country_if_balanced = sum(quota_check$overrepresented_n),
  total_to_recruit_by_region_to_cover_deficits = sum(region_quota_check$recruit_next_n),
  total_overrepresented_by_region_if_balanced = sum(region_quota_check$overrepresented_n)
)

print(quota_summary)

# GUARDAR CSVs
# Dataset base usado para cuotas
write_csv(
  df_quota,
  file.path(quota_output_dir, "df_quota_base_usable.csv")
)

# Distribución actual
write_csv(
  country_distribution_clean,
  file.path(quota_output_dir, "country_distribution_current_clean.csv")
)

write_csv(
  region_distribution_clean,
  file.path(quota_output_dir, "region_distribution_current_clean.csv")
)

# Distribución objetivo escalada
write_csv(
  quota_targets_scaled,
  file.path(quota_output_dir, "target_distribution_scaled_by_country_181_300_500_1000.csv")
)

write_csv(
  region_targets_scaled,
  file.path(quota_output_dir, "target_distribution_scaled_by_region_181_300_500_1000.csv")
)

# Comparación país
write_csv(
  quota_check,
  file.path(quota_output_dir, "country_quota_check.csv")
)

write_csv(
  quota_check_scenarios,
  file.path(quota_output_dir, "country_quota_check_scenarios_181_300_500_1000.csv")
)

write_csv(
  quota_check_scenarios_wide,
  file.path(quota_output_dir, "country_quota_check_scenarios_wide.csv")
)

write_csv(
  countries_outside_target,
  file.path(quota_output_dir, "countries_outside_target.csv")
)

# Comparación región
write_csv(
  region_quota_check,
  file.path(quota_output_dir, "region_quota_check.csv")
)

write_csv(
  region_quota_check_scenarios,
  file.path(quota_output_dir, "region_quota_check_scenarios_181_300_500_1000.csv")
)

write_csv(
  region_quota_check_scenarios_wide,
  file.path(quota_output_dir, "region_quota_check_scenarios_wide.csv")
)

# Resumen
write_csv(
  quota_summary,
  file.path(quota_output_dir, "quota_summary.csv")
)

write_csv(
  quota_country_filter_summary,
  file.path(quota_output_dir, "quota_country_filter_summary.csv")
)

# GRÁFICOS POR PAÍS
plot_country_recruitment_needed <- quota_check %>%
  filter(recruit_next_n > 0) %>%
  mutate(country = reorder(country, recruit_next_n)) %>%
  ggplot(aes(x = country, y = recruit_next_n)) +
  geom_col(fill = "#56B4E9", color = "#2C3E50") +
  coord_flip() +
  labs(
    title = paste0("Participantes adicionales necesarios por país para N = ", selected_total_n),
    subtitle = "Comparación entre muestra actual usable y distribución teórica objetivo",
    x = NULL,
    y = "Participantes a reclutar"
  ) +
  theme_minimal(base_size = 12)

print(plot_country_recruitment_needed)

ggsave(
  file.path(quota_output_dir, "country_recruitment_needed.png"),
  plot_country_recruitment_needed,
  width = 9,
  height = 7,
  dpi = 300
)

plot_country_overrepresented <- quota_check %>%
  filter(overrepresented_n > 0) %>%
  mutate(country = reorder(country, overrepresented_n)) %>%
  ggplot(aes(x = country, y = overrepresented_n)) +
  geom_col(fill = "#E69F00", color = "#2C3E50") +
  coord_flip() +
  labs(
    title = paste0("Países sobrerrepresentados respecto a la cuota para N = ", selected_total_n),
    subtitle = "Participantes que exceden la distribución objetivo",
    x = NULL,
    y = "Participantes por encima de la cuota"
  ) +
  theme_minimal(base_size = 12)

print(plot_country_overrepresented)

ggsave(
  file.path(quota_output_dir, "country_overrepresented.png"),
  plot_country_overrepresented,
  width = 9,
  height = 5,
  dpi = 300
)


# GRÁFICOS POR REGIÓN
plot_region_recruitment_needed <- region_quota_check %>%
  filter(recruit_next_n > 0) %>%
  mutate(residence_region = reorder(residence_region, recruit_next_n)) %>%
  ggplot(aes(x = residence_region, y = recruit_next_n)) +
  geom_col(fill = "#56B4E9", color = "#2C3E50") +
  coord_flip() +
  labs(
    title = paste0("Participantes adicionales necesarios por región para N = ", selected_total_n),
    subtitle = "Comparación entre muestra actual usable y distribución teórica objetivo",
    x = NULL,
    y = "Participantes a reclutar"
  ) +
  theme_minimal(base_size = 12)

print(plot_region_recruitment_needed)

ggsave(
  file.path(quota_output_dir, "region_recruitment_needed.png"),
  plot_region_recruitment_needed,
  width = 8,
  height = 5,
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
  file.path(quota_output_dir, "region_overrepresented.png"),
  plot_region_overrepresented,
  width = 8,
  height = 5,
  dpi = 300
)


# COMPROBACIONES FINALES
cat("\nEscenario seleccionado:", selected_total_n, "\n")

cat("\nFilas usables para cuotas:\n")
print(nrow(df_quota))

cat("\nDistribución actual por país:\n")
print(country_distribution_clean, n = Inf)

cat("\nDistribución actual por región:\n")
print(region_distribution_clean, n = Inf)

cat("\nResumen de cuotas:\n")
print(quota_summary)

cat("\nArchivos principales generados en:\n")
cat(quota_output_dir, "\n")

cat("\nPara revisar:\n")
cat("- country_quota_check_scenarios_wide.csv\n")
cat("- region_quota_check_scenarios_wide.csv\n")
cat("- quota_summary.csv\n")