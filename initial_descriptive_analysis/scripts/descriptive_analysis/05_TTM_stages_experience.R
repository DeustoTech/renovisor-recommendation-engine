# SCRIPT 05 - EXPERIENCE Y ETAPAS TTM


# ==============================================================================
# LIBRERÍAS
# ==============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tibble)
library(ggpattern)


# ==============================================================================
# RUTAS
# ==============================================================================

base_input_dir <- "initial_descriptive_analysis/output/clean_datasets"

base_output_dir <- "initial_descriptive_analysis/output/experience"

csv_dir <- file.path(base_output_dir, "csv")
plots_dir <- file.path(base_output_dir, "plots")
pdf_dir <- file.path(base_output_dir, "pdf")
logs_dir <- file.path(base_output_dir, "logs")
plots_technology_dir <- file.path(plots_dir, "by_technology")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_technology_dir, recursive = TRUE, showWarnings = FALSE)


# ==============================================================================
# CARGAR DATOS
# ==============================================================================

df <- read_csv(
  file.path(base_input_dir, "df_clean_general.csv"),
  show_col_types = FALSE
)

cat("Filas:", nrow(df), "\n")
cat("Columnas:", ncol(df), "\n")


# ==============================================================================
# INSPECCIÓN INICIAL DE COLUMNAS
# ==============================================================================

cols_df <- tibble(
  index = seq_along(names(df)),
  column_name = names(df)
)

write_csv(
  cols_df,
  file.path(csv_dir, "column_names_df_clean_general.csv")
)

print(cols_df, n = Inf)


# ==============================================================================
# DEFINIR COLUMNAS RELEVANTES
# ==============================================================================

df <- df %>%
  mutate(
    participant_id = coalesce(
      as.character(join_key),
      as.character(prolific_id),
      as.character(identification_code),
      as.character(row_number())
    )
  )

experience_tech_cols <- names(df)[
  str_detect(
    names(df),
    "^for_each_of_the_following_technologies_decisions_or_behaviours_.*_final$"
  )
]

renovation_age_cols <- names(df)[
  str_detect(
    names(df),
    "^please_indicate_when_the_technology_decision_was_implemented_or_contracted_.*_final$"
  )
]

year_of_birth_col <- "year_of_birth"
gender_col <- "what_is_your_gender_final"
country_col <- "in_which_country_do_you_currently_live_final"
size_city_col <- "what_is_the_approximate_population_size_of_the_city_where_you_live_final"
climate_zone_col <- "in_which_climate_zone_do_you_live_please_select_the_option_that_corresponds_to_your_location_if_you_re_not_sure_which_climate_zone_you_live_in_please_refer_to_the_map_below_or_select_the_option_that_best_matches_your_region_final"
employment_col <- "what_is_your_current_employment_status_please_indicate_your_main_contractual_status_if_you_are_temporarily_on_leave_such_as_sick_leave_parental_leave_or_a_temporary_reduction_in_working_hours_please_report_your_usual_employment_status_final"
education_level_col <- "what_is_the_highest_level_of_education_that_you_have_completed_if_you_are_currently_studying_and_have_not_yet_completed_a_level_please_select_the_last_level_you_have_finished_final"
work_home_col <- "do_you_currently_work_or_study_from_home_final"
type_house_col <- "what_type_of_household_do_you_live_in_please_select_the_option_that_best_describes_your_household_final"
tenure_col <- "what_is_the_current_tenure_status_of_your_home_final"
political_col <- "on_a_scale_from_0_to_100_where_0_means_most_left_and_100_means_most_right_where_would_you_place_yourself_politically_final"

cat("Tecnologías Experience:", length(experience_tech_cols), "\n")
cat("Edad renovación:", length(renovation_age_cols), "\n")


# ==============================================================================
# COMPROBAR OPCIONES ORIGINALES DE EXPERIENCE
# ==============================================================================

experience_options_raw <- df %>%
  select(all_of(experience_tech_cols)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "technology_col",
    values_to = "experience_raw"
  ) %>%
  mutate(
    experience_raw = str_squish(as.character(experience_raw)),
    experience_raw = na_if(experience_raw, "")
  ) %>%
  filter(!is.na(experience_raw)) %>%
  count(experience_raw, sort = TRUE)

write_csv(
  experience_options_raw,
  file.path(csv_dir, "experience_options_raw.csv")
)

print(experience_options_raw, n = Inf)


# ==============================================================================
# RECODIFICAR OPCIONES DE EXPERIENCE
# ==============================================================================

experience_levels <- c(
  "Already present when I moved in",
  "I have installed / implemented / am currently doing this myself",
  "I have already actively looked for information about it",
  "I am unaware of this technology, decision, or behaviour",
  "I am aware of this and could consider it in the future",
  "I would never apply / use it"
)

recode_experience <- function(x) {
  x <- str_squish(as.character(x))
  
  case_when(
    x == "Already present when I moved in" ~
      "Already present when I moved in",
    
    x == "I have installed / implemented / am currently doing this myself" |
      x == "I installed / implemented this myself" |
      x == "Technologies implemented by me" ~
      "I have installed / implemented / am currently doing this myself",
    
    x == "I have already actively looked for information about it" ~
      "I have already actively looked for information about it",
    
    x == "I am unaware of this technology, decision, or behaviour" |
      x == "I don't know this technology" |
      x == "Technologies you are not aware" ~
      "I am unaware of this technology, decision, or behaviour",
    
    x == "I am aware of this and could consider it in the future" |
      x == "Technologies you are interested on" ~
      "I am aware of this and could consider it in the future",
    
    x == "I would never apply / use it" |
      x == "Technologies you will never do" ~
      "I would never apply / use it",
    
    TRUE ~ NA_character_
  )
}


# ==============================================================================
# COMPROBAR OPCIONES LIMPIAS
# ==============================================================================

experience_options_clean <- df %>%
  select(all_of(experience_tech_cols)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "technology_col",
    values_to = "experience_raw"
  ) %>%
  mutate(
    experience_raw = str_squish(as.character(experience_raw)),
    experience_raw = na_if(experience_raw, ""),
    experience_clean = recode_experience(experience_raw)
  ) %>%
  filter(!is.na(experience_clean)) %>%
  count(experience_clean, sort = TRUE)

write_csv(
  experience_options_clean,
  file.path(csv_dir, "experience_options_clean.csv")
)

print(experience_options_clean, n = Inf)


# ==============================================================================
# REVISAR RESPUESTAS SIN CLASIFICAR
# ==============================================================================

experience_unclassified <- df %>%
  select(all_of(experience_tech_cols)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "technology_col",
    values_to = "experience_raw"
  ) %>%
  mutate(
    experience_raw = str_squish(as.character(experience_raw)),
    experience_raw = na_if(experience_raw, ""),
    experience_clean = recode_experience(experience_raw)
  ) %>%
  filter(
    !is.na(experience_raw),
    is.na(experience_clean)
  ) %>%
  count(experience_raw, sort = TRUE)

write_csv(
  experience_unclassified,
  file.path(csv_dir, "experience_unclassified.csv")
)

print(experience_unclassified, n = Inf)


# ==============================================================================
# LIMPIAR AÑO DE NACIMIENTO
# ==============================================================================

project_year <- 2026

df <- df %>%
  mutate(
    year_of_birth_raw = suppressWarnings(as.numeric(.data[[year_of_birth_col]])),
    
    year_of_birth_clean = case_when(
      year_of_birth_raw >= 1900 & year_of_birth_raw <= 2007 ~ year_of_birth_raw,
      year_of_birth_raw >= 18 & year_of_birth_raw <= 100 ~ project_year - year_of_birth_raw,
      TRUE ~ NA_real_
    )
  )


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


# ==============================================================================
# FUNCIONES DE LIMPIEZA SOCIOECONÓMICA
# ==============================================================================

clean_gender <- function(x) {
  x <- str_squish(as.character(x))
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    str_detect(x, regex("^male", ignore_case = TRUE)) ~ "Hombre",
    str_detect(x, regex("^female", ignore_case = TRUE)) ~ "Mujer",
    TRUE ~ "Otro / Prefiere no decirlo"
  )
}

clean_country <- function(x) {
  x <- str_squish(as.character(x))
  x <- str_remove(x, "\\s*\\(ID[0-9]+\\)$")
  x <- str_remove(x, "^[A-Z]{2}\\s*[–-]\\s*")
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    TRUE ~ x
  )
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

clean_city_size <- function(x) {
  x <- str_squish(as.character(x))
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    str_detect(x, regex("^Town", ignore_case = TRUE)) ~ "Municipio pequeño",
    str_detect(x, regex("^Small city", ignore_case = TRUE)) ~ "Ciudad pequeña",
    str_detect(x, regex("^Medium city", ignore_case = TRUE)) ~ "Ciudad mediana",
    str_detect(x, regex("^Large city", ignore_case = TRUE)) ~ "Ciudad grande",
    str_detect(x, regex("^Global city", ignore_case = TRUE)) ~ "Metrópolis",
    TRUE ~ NA_character_
  )
}

clean_climate_zone <- function(x) {
  x <- str_squish(as.character(x))
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    str_detect(x, regex("Subtropical", ignore_case = TRUE)) ~ "Clima subtropical",
    str_detect(x, regex("Temperate", ignore_case = TRUE)) ~ "Clima templado",
    str_detect(x, regex("Cold", ignore_case = TRUE)) ~ "Clima frío",
    TRUE ~ NA_character_
  )
}

clean_employment <- function(x) {
  x <- str_squish(as.character(x))
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    str_detect(x, regex("Full time employed|Part-time employed|Self-employed", ignore_case = TRUE)) ~ "Empleado/a",
    str_detect(x, regex("Student", ignore_case = TRUE)) ~ "Estudiante",
    str_detect(x, regex("Unemployed", ignore_case = TRUE)) ~ "Desempleado/a",
    str_detect(x, regex("Stay-at-home", ignore_case = TRUE)) ~ "Otra situación inactiva",
    TRUE ~ "Otra situación"
  )
}

clean_education <- function(x) {
  x <- str_squish(as.character(x))
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    str_detect(x, regex("University", ignore_case = TRUE)) ~ "Universitaria",
    str_detect(x, regex("Primary|Secondary|Vocational", ignore_case = TRUE)) ~ "No universitaria",
    TRUE ~ NA_character_
  )
}

clean_work_home <- function(x) {
  x <- str_squish(as.character(x))
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    str_detect(x, regex("^Yes", ignore_case = TRUE)) ~ "Sí",
    str_detect(x, regex("^No", ignore_case = TRUE)) ~ "No",
    TRUE ~ NA_character_
  )
}

clean_type_house <- function(x) {
  x <- str_squish(as.character(x))
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    str_detect(x, regex("Uni-personal", ignore_case = TRUE)) ~ "Vive solo/a",
    str_detect(x, regex("Dual Income No Kids|Ageing family", ignore_case = TRUE)) ~ "Pareja sin hijos",
    str_detect(x, regex("Traditional family|Large family|Single parenthood", ignore_case = TRUE)) ~ "Hogar con hijos",
    str_detect(x, regex("Poly-nuclear", ignore_case = TRUE)) ~ "Vivienda compartida",
    TRUE ~ "Otro tipo de hogar"
  )
}

clean_tenure <- function(x) {
  x <- str_squish(as.character(x))
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    str_detect(x, regex("own the home outright|fully paid-off", ignore_case = TRUE)) ~ "Propiedad sin hipoteca",
    str_detect(x, regex("mortgage|outstanding payments", ignore_case = TRUE)) ~ "Propiedad con hipoteca",
    str_detect(x, regex("rent|rental", ignore_case = TRUE)) ~ "Alquiler",
    TRUE ~ "Otro régimen"
  )
}

clean_political_orientation <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  
  case_when(
    is.na(x) ~ NA_character_,
    x < 20 ~ "Extrema izquierda",
    x < 40 ~ "Izquierda",
    x >= 40 & x <= 60 ~ "Centro",
    x > 60 & x <= 80 ~ "Derecha",
    x > 80 ~ "Extrema derecha",
    TRUE ~ NA_character_
  )
}


# ==============================================================================
# LIMPIAR TECNOLOGÍA
# ==============================================================================

clean_technology <- function(x) {
  case_when(
    str_detect(x, "balcony_solar_kits") ~ "Kits solares de balcón",
    str_detect(x, "change_of_electricity_tariff") ~ "Cambio de tarifa eléctrica",
    str_detect(x, "cooling_system") ~ "Sistema de refrigeración",
    str_detect(x, "domestic_hot_water_system") ~ "Agua caliente sanitaria",
    str_detect(x, "electric_vehicle") ~ "Vehículo eléctrico",
    str_detect(x, "elevator") ~ "Ascensor",
    str_detect(x, "energy_efficient_appliances") ~ "Electrodomésticos eficientes",
    str_detect(x, "energy_storage_systems") ~ "Almacenamiento energético",
    str_detect(x, "envelope_renovation") ~ "Renovación de envolvente",
    str_detect(x, "fosil_fuel_or_biomass_based_heating_system") ~ "Calefacción fósil o biomasa",
    str_detect(x, "heat_pump_based_heating_system") ~ "Bomba de calor",
    str_detect(x, "heat_recovery_mechanical_ventilation") ~ "Ventilación con recuperador",
    str_detect(x, "join_an_energy_community") ~ "Comunidad energética",
    str_detect(x, "micro_efficiency_measures") ~ "Medidas de microeficiencia",
    str_detect(x, "rooftop_photovoltaic_system") ~ "Fotovoltaica en cubierta",
    str_detect(x, "smart_home_systems") ~ "Sistemas inteligentes del hogar",
    TRUE ~ x
  )
}


# ==============================================================================
# CREAR TABLA LARGA DE EXPERIENCE
# ==============================================================================

experience_long <- df %>%
  mutate(
    country_clean_final = coalesce(
      clean_text_basic(get_optional_col(., "country_clean")),
      clean_country(get_optional_col(., country_col))
    ),
    residence_region_final = coalesce(
      clean_text_basic(get_optional_col(., "residence_region")),
      clean_residence_region(country_clean_final)
    )
  ) %>%
  select(
    participant_id,
    year_of_birth = year_of_birth_clean,
    gender = all_of(gender_col),
    country = country_clean_final,
    residence_region = residence_region_final,
    city_size = all_of(size_city_col),
    climate_zone = all_of(climate_zone_col),
    employment = all_of(employment_col),
    education_level = all_of(education_level_col),
    work_home = all_of(work_home_col),
    type_house = all_of(type_house_col),
    tenure = all_of(tenure_col),
    political_orientation_raw = all_of(political_col),
    all_of(experience_tech_cols)
  ) %>%
  pivot_longer(
    cols = all_of(experience_tech_cols),
    names_to = "technology_col",
    values_to = "experience_raw"
  ) %>%
  mutate(
    technology = clean_technology(technology_col),
    
    experience_raw = str_squish(as.character(experience_raw)),
    experience_raw = na_if(experience_raw, ""),
    experience_clean = recode_experience(experience_raw),
    
    gender = clean_gender(gender),
    city_size = clean_city_size(city_size),
    climate_zone = clean_climate_zone(climate_zone),
    employment = clean_employment(employment),
    education_group = clean_education(education_level),
    work_home = clean_work_home(work_home),
    type_house = clean_type_house(type_house),
    tenure = clean_tenure(tenure),
    political_orientation = clean_political_orientation(political_orientation_raw),
    
    age_group = case_when(
      is.na(year_of_birth) ~ NA_character_,
      year_of_birth >= 2001 & year_of_birth <= 2007 ~ "Generación Z",
      year_of_birth >= 1986 & year_of_birth <= 2000 ~ "Millennials",
      year_of_birth >= 1971 & year_of_birth <= 1985 ~ "Generación X",
      year_of_birth >= 1932 & year_of_birth <= 1970 ~ "Boomers + generación silenciosa",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(experience_clean))

write_csv(
  experience_long,
  file.path(csv_dir, "experience_long.csv")
)

glimpse(experience_long)


# ==============================================================================
# PREPARAR ETIQUETAS CORTAS DE EXPERIENCE
# ==============================================================================

experience_short_levels <- c(
  "Ya estaba presente",
  "Implementada",
  "Buscó información",
  "La conoce / la consideraría",
  "No la conoce",
  "Nunca la usaría"
)

awareness_order_levels <- c(
  "No la conoce",
  "La conoce / la consideraría"
)

experience_long <- experience_long %>%
  mutate(
    experience_clean = factor(experience_clean, levels = experience_levels),
    
    experience_short = case_when(
      experience_clean == "Already present when I moved in" ~ "Ya estaba presente",
      experience_clean == "I have installed / implemented / am currently doing this myself" ~ "Implementada",
      experience_clean == "I have already actively looked for information about it" ~ "Buscó información",
      experience_clean == "I am unaware of this technology, decision, or behaviour" ~ "No la conoce",
      experience_clean == "I am aware of this and could consider it in the future" ~ "La conoce / la consideraría",
      experience_clean == "I would never apply / use it" ~ "Nunca la usaría",
      TRUE ~ NA_character_
    ),
    
    experience_short = factor(experience_short, levels = experience_short_levels)
  )

write_csv(
  experience_long,
  file.path(csv_dir, "experience_long.csv")
)


# ==============================================================================
# DISTRIBUCIÓN GENERAL DE EXPERIENCE
# ==============================================================================

experience_distribution <- experience_long %>%
  count(experience_short, sort = FALSE) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  arrange(desc(n)) %>%
  mutate(
    experience_short = factor(
      experience_short,
      levels = rev(as.character(experience_short))
    )
  )

write_csv(
  experience_distribution,
  file.path(csv_dir, "experience_distribution.csv")
)

print(experience_distribution, n = Inf)


# ==============================================================================
# DISTRIBUCIÓN DE EXPERIENCE POR TECNOLOGÍA
# ==============================================================================

experience_distribution_by_technology <- experience_long %>%
  count(technology, experience_short, sort = FALSE) %>%
  group_by(technology) %>%
  complete(
    experience_short = factor(experience_short_levels, levels = experience_short_levels),
    fill = list(n = 0)
  ) %>%
  mutate(
    total_technology = sum(n),
    percentage = if_else(total_technology > 0, n / total_technology * 100, 0)
  ) %>%
  ungroup()

technology_order <- experience_distribution_by_technology %>%
  filter(experience_short %in% awareness_order_levels) %>%
  group_by(technology) %>%
  summarise(
    score_awareness = sum(n),
    .groups = "drop"
  ) %>%
  arrange(score_awareness) %>%
  pull(technology)

experience_distribution_by_technology <- experience_distribution_by_technology %>%
  mutate(
    technology = factor(technology, levels = technology_order)
  )

write_csv(
  experience_distribution_by_technology,
  file.path(csv_dir, "experience_distribution_by_technology.csv")
)

print(experience_distribution_by_technology, n = Inf)


# ==============================================================================
# COMPROBACIONES
# ==============================================================================

n_participants_total <- n_distinct(df$participant_id)
n_participants_valid <- n_distinct(experience_long$participant_id)
n_technologies <- length(experience_tech_cols)
n_possible <- n_participants_total * n_technologies
n_valid <- nrow(experience_long)

subtitle_sample <- paste0(
  "n encuesta = ", n_participants_total,
  "; n con Experience válido = ", n_participants_valid,
  "; observaciones persona-tecnología válidas = ", n_valid
)

cat("Participantes totales:", n_participants_total, "\n")
cat("Participantes con Experience válido:", n_participants_valid, "\n")
cat("Tecnologías:", n_technologies, "\n")
cat("Observaciones posibles persona-tecnología:", n_possible, "\n")
cat("Observaciones válidas:", n_valid, "\n")
cat("Observaciones vacías/NA:", n_possible - n_valid, "\n")

responses_per_participant <- experience_long %>%
  count(participant_id, name = "n_technologies_answered") %>%
  arrange(n_technologies_answered)

write_csv(
  responses_per_participant,
  file.path(csv_dir, "responses_per_participant_experience.csv")
)

print(responses_per_participant, n = Inf)


# ==============================================================================
# COMPROBAR VARIABLES LIMPIAS
# ==============================================================================

variables_check <- experience_long %>%
  distinct(
    participant_id,
    year_of_birth,
    age_group,
    gender,
    country,
    residence_region,
    city_size,
    climate_zone,
    employment,
    education_group,
    work_home,
    type_house,
    tenure,
    political_orientation
  )

write_csv(
  variables_check,
  file.path(csv_dir, "socioeconomic_variables_check.csv")
)


# ==============================================================================
# FUNCIONES PARA GUARDAR GRÁFICOS
# ==============================================================================

save_plot_png <- function(plot, filename, width = 9, height = 5) {
  ggsave(
    filename = file.path(plots_dir, paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}

save_plot_png_technology <- function(plot, filename, width = 9, height = 5) {
  ggsave(
    filename = file.path(plots_technology_dir, paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}


# ==============================================================================
# ESTILO DE GRÁFICOS
# ==============================================================================

experience_colors <- c(
  "Ya estaba presente" = "#0072B2",
  "Implementada" = "#009E73",
  "Buscó información" = "#E69F00",
  "La conoce / la consideraría" = "#56B4E9",
  "No la conoce" = "#D55E00",
  "Nunca la usaría" = "#CC79A7"
)

experience_patterns <- c(
  "Ya estaba presente" = "none",
  "Implementada" = "stripe",
  "Buscó información" = "crosshatch",
  "La conoce / la consideraría" = "circle",
  "No la conoce" = "stripe",
  "Nunca la usaría" = "crosshatch"
)

experience_pattern_angles <- c(
  "Ya estaba presente" = 0,
  "Implementada" = 45,
  "Buscó información" = 0,
  "La conoce / la consideraría" = 0,
  "No la conoce" = 135,
  "Nunca la usaría" = 90
)

plot_base_size <- 15
plot_title_size <- 18
plot_subtitle_size <- 13
plot_axis_title_size <- 15
plot_axis_text_size <- 15
plot_legend_text_size <- 13
plot_legend_title_size <- 14

plot_label_size <- 4.2
plot_stack_label_size <- 3.4
plot_heatmap_label_size <- 3.5

theme_experience <- theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(face = "bold", size = plot_title_size),
    plot.subtitle = element_text(size = plot_subtitle_size),
    axis.title.x = element_text(size = plot_axis_title_size, margin = margin(t = 8)),
    axis.title.y = element_text(size = plot_axis_title_size, margin = margin(r = 8)),
    axis.text.x = element_text(size = plot_axis_text_size),
    axis.text.y = element_text(size = plot_axis_text_size),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = plot_legend_title_size),
    legend.text = element_text(size = plot_legend_text_size),
    panel.grid.minor = element_blank(),
    plot.margin = margin(12, 45, 12, 12)
  )

theme_experience_heatmap <- theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(face = "bold", size = plot_title_size),
    plot.subtitle = element_text(size = plot_subtitle_size),
    axis.title.x = element_text(size = plot_axis_title_size, margin = margin(t = 8)),
    axis.title.y = element_text(size = plot_axis_title_size, margin = margin(r = 8)),
    axis.text.x = element_text(size = plot_axis_text_size, angle = 35, hjust = 1),
    axis.text.y = element_text(size = plot_axis_text_size),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = plot_legend_title_size),
    legend.text = element_text(size = plot_legend_text_size),
    panel.grid.minor = element_blank(),
    plot.margin = margin(12, 12, 12, 12)
  )


# ==============================================================================
# FUNCIÓN PARA ELEGIR COLOR DEL TEXTO DENTRO DE LAS BARRAS
# ==============================================================================

label_color_experience <- function(x) {
  case_when(
    x %in% c("Ya estaba presente", "Implementada", "No la conoce") ~ "white",
    TRUE ~ "black"
  )
}


# ==============================================================================
# GRÁFICO TOTAL DE EXPERIENCE
# ==============================================================================

experience_distribution_plot <- experience_distribution %>%
  mutate(
    label = paste0(round(percentage, 1), "%\n(n=", n, ")")
  )

plot_experience_total <- ggplot(
  experience_distribution_plot,
  aes(
    x = experience_short,
    y = percentage,
    fill = experience_short,
    pattern = experience_short,
    pattern_angle = experience_short
  )
) +
  geom_col_pattern(
    color = "black",
    linewidth = 0.25,
    pattern_fill = "black",
    pattern_colour = "black",
    pattern_density = 0.08,
    pattern_spacing = 0.04,
    show.legend = FALSE
  ) +
  geom_text(
    aes(label = label),
    hjust = -0.10,
    size = plot_label_size,
    lineheight = 0.9
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = experience_colors, drop = FALSE) +
  scale_pattern_manual(values = experience_patterns, drop = FALSE, guide = "none") +
  scale_pattern_angle_manual(values = experience_pattern_angles, drop = FALSE, guide = "none") +
  scale_y_continuous(
    limits = c(0, max(experience_distribution_plot$percentage) + 10),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Distribución general de respuestas de experiencia",
    subtitle = subtitle_sample,
    x = NULL,
    y = "Porcentaje"
  ) +
  theme_experience

print(plot_experience_total)

save_plot_png(
  plot = plot_experience_total,
  filename = "experience_distribution_total",
  width = 11,
  height = 6
)


# ==============================================================================
# BARRAS APILADAS POR TECNOLOGÍA EN NÚMEROS ABSOLUTOS
# ==============================================================================

experience_distribution_by_technology_plot <- experience_distribution_by_technology %>%
  mutate(
    technology_label = str_wrap(as.character(technology), width = 32),
    technology_label = factor(
      technology_label,
      levels = str_wrap(as.character(technology_order), width = 32)
    ),
    label_n = if_else(
      n > 0 & percentage >= 1,
      as.character(n),
      ""
    ),
    label_pct = if_else(
      n > 0 & percentage >= 1,
      paste0(round(percentage, 0), "%\n(n=", n, ")"),
      ""
    ),
    label_color = label_color_experience(experience_short)
  )

plot_experience_by_technology <- ggplot(
  experience_distribution_by_technology_plot,
  aes(
    x = technology_label,
    y = n,
    fill = experience_short,
    pattern = experience_short,
    pattern_angle = experience_short
  )
) +
  geom_col_pattern(
    color = "black",
    linewidth = 0.15,
    pattern_fill = "black",
    pattern_colour = "black",
    pattern_density = 0.06,
    pattern_spacing = 0.045
  ) +
  geom_text(
    aes(
      label = label_n,
      color = label_color
    ),
    position = position_stack(vjust = 0.5),
    size = plot_stack_label_size,
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_fill_manual(values = experience_colors, drop = FALSE) +
  scale_pattern_manual(values = experience_patterns, drop = FALSE, guide = "none") +
  scale_pattern_angle_manual(values = experience_pattern_angles, drop = FALSE, guide = "none") +
  scale_color_identity() +
  labs(
    title = "Respuestas de experiencia por tecnología",
    subtitle = paste0(
      subtitle_sample,
      ". Tecnologías ordenadas por la suma de 'No la conoce' y 'La conoce / la consideraría'"
    ),
    x = NULL,
    y = "Número de respuestas persona-tecnología",
    fill = "Categoría de experiencia"
  ) +
  theme_experience

print(plot_experience_by_technology)

save_plot_png(
  plot = plot_experience_by_technology,
  filename = "experience_distribution_by_technology_absolute",
  width = 14,
  height = 9
)


# ==============================================================================
# BARRAS APILADAS POR TECNOLOGÍA AL 100%
# ==============================================================================

plot_experience_by_technology_percentage <- ggplot(
  experience_distribution_by_technology_plot,
  aes(
    x = technology_label,
    y = percentage,
    fill = experience_short,
    pattern = experience_short,
    pattern_angle = experience_short
  )
) +
  geom_col_pattern(
    color = "black",
    linewidth = 0.15,
    pattern_fill = "black",
    pattern_colour = "black",
    pattern_density = 0.06,
    pattern_spacing = 0.045
  ) +
  geom_text(
    aes(
      label = label_pct,
      color = label_color
    ),
    position = position_stack(vjust = 0.5),
    size = plot_stack_label_size,
    lineheight = 0.9,
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_fill_manual(values = experience_colors, drop = FALSE) +
  scale_pattern_manual(values = experience_patterns, drop = FALSE, guide = "none") +
  scale_pattern_angle_manual(values = experience_pattern_angles, drop = FALSE, guide = "none") +
  scale_color_identity() +
  scale_y_continuous(
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Distribución porcentual de experiencia por tecnología",
    subtitle = paste0(
      subtitle_sample,
      ". Porcentajes calculados dentro de cada tecnología"
    ),
    x = NULL,
    y = "Porcentaje",
    fill = "Categoría de experiencia"
  ) +
  theme_experience

print(plot_experience_by_technology_percentage)

save_plot_png(
  plot = plot_experience_by_technology_percentage,
  filename = "experience_distribution_by_technology_percentage",
  width = 14,
  height = 9
)


# ==============================================================================
# HEATMAP POR TECNOLOGÍA
# ==============================================================================

experience_distribution_by_technology_heatmap <- experience_distribution_by_technology %>%
  mutate(
    technology_label = str_wrap(as.character(technology), width = 32),
    technology_label = factor(
      technology_label,
      levels = str_wrap(as.character(technology_order), width = 32)
    ),
    label = paste0(round(percentage, 0), "%\n(n=", n, ")")
  )

plot_experience_heatmap <- ggplot(
  experience_distribution_by_technology_heatmap,
  aes(
    x = experience_short,
    y = technology_label,
    fill = percentage
  )
) +
  geom_tile(color = "black", linewidth = 0.25) +
  geom_text(
    aes(label = label),
    size = plot_heatmap_label_size,
    lineheight = 0.9
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#0072B2"
  ) +
  labs(
    title = "Distribución de experiencia por tecnología",
    subtitle = paste0(
      subtitle_sample,
      ". Porcentajes calculados dentro de cada tecnología"
    ),
    x = "Categoría de experiencia",
    y = NULL,
    fill = "Porcentaje"
  ) +
  theme_experience_heatmap

print(plot_experience_heatmap)

save_plot_png(
  plot = plot_experience_heatmap,
  filename = "experience_distribution_heatmap_by_technology",
  width = 14,
  height = 9
)


# ==============================================================================
# GRÁFICOS INDIVIDUALES POR TECNOLOGÍA
# ==============================================================================

plot_single_technology_absolute <- function(technology_name) {
  
  plot_data <- experience_distribution_by_technology %>%
    filter(technology == technology_name) %>%
    mutate(
      experience_short = factor(
        experience_short,
        levels = rev(experience_short_levels)
      ),
      label_color = label_color_experience(experience_short),
      label_n = if_else(n > 0, as.character(n), ""),
      label_y = case_when(
        n == 0 ~ 0,
        n < 2 ~ n + 0.25,
        TRUE ~ n / 2
      ),
      label_hjust = case_when(
        n == 0 ~ 0,
        n < 2 ~ 0,
        TRUE ~ 0.5
      )
    )
  
  total_technology <- unique(plot_data$total_technology)
  
  p <- ggplot(
    plot_data,
    aes(
      x = experience_short,
      y = n,
      fill = experience_short,
      pattern = experience_short,
      pattern_angle = experience_short
    )
  ) +
    geom_col_pattern(
      color = "black",
      linewidth = 0.25,
      pattern_fill = "black",
      pattern_colour = "black",
      pattern_density = 0.08,
      pattern_spacing = 0.04,
      show.legend = FALSE
    ) +
    geom_text(
      aes(
        y = label_y,
        label = label_n,
        color = label_color,
        hjust = label_hjust
      ),
      size = plot_label_size,
      show.legend = FALSE
    ) +
    coord_flip(clip = "off") +
    scale_fill_manual(values = experience_colors, drop = FALSE) +
    scale_pattern_manual(values = experience_patterns, drop = FALSE, guide = "none") +
    scale_pattern_angle_manual(values = experience_pattern_angles, drop = FALSE, guide = "none") +
    scale_color_identity() +
    scale_y_continuous(
      limits = c(0, max(plot_data$n, na.rm = TRUE) + 3),
      breaks = scales::pretty_breaks()
    ) +
    labs(
      title = paste0("Experiencia declarada: ", technology_name),
      subtitle = paste0(
        "n encuesta = ", n_participants_total,
        "; respuestas persona-tecnología para esta intervención = ", total_technology
      ),
      x = NULL,
      y = "Número de respuestas"
    ) +
    theme_experience
  
  print(p)
  
  save_plot_png_technology(
    plot = p,
    filename = paste0("experience_absolute_", clean_filename(technology_name)),
    width = 10,
    height = 6
  )
  
  return(p)
}

plot_single_technology_percentage <- function(technology_name) {
  
  plot_data <- experience_distribution_by_technology %>%
    filter(technology == technology_name) %>%
    mutate(
      experience_short = factor(
        experience_short,
        levels = rev(experience_short_levels)
      ),
      label_color = label_color_experience(experience_short),
      label_pct = if_else(
        n > 0,
        paste0(round(percentage, 1), "%\n(n=", n, ")"),
        ""
      ),
      label_y = case_when(
        percentage == 0 ~ 0,
        percentage < 8 ~ percentage + 2,
        TRUE ~ percentage / 2
      ),
      label_hjust = case_when(
        percentage == 0 ~ 0,
        percentage < 8 ~ 0,
        TRUE ~ 0.5
      )
    )
  
  total_technology <- unique(plot_data$total_technology)
  
  p <- ggplot(
    plot_data,
    aes(
      x = experience_short,
      y = percentage,
      fill = experience_short,
      pattern = experience_short,
      pattern_angle = experience_short
    )
  ) +
    geom_col_pattern(
      color = "black",
      linewidth = 0.25,
      pattern_fill = "black",
      pattern_colour = "black",
      pattern_density = 0.08,
      pattern_spacing = 0.04,
      show.legend = FALSE
    ) +
    geom_text(
      aes(
        y = label_y,
        label = label_pct,
        color = label_color,
        hjust = label_hjust
      ),
      size = plot_label_size,
      lineheight = 0.9,
      show.legend = FALSE
    ) +
    coord_flip(clip = "off") +
    scale_fill_manual(values = experience_colors, drop = FALSE) +
    scale_pattern_manual(values = experience_patterns, drop = FALSE, guide = "none") +
    scale_pattern_angle_manual(values = experience_pattern_angles, drop = FALSE, guide = "none") +
    scale_color_identity() +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 10),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = paste0("Distribución porcentual: ", technology_name),
      subtitle = paste0(
        "n encuesta = ", n_participants_total,
        "; respuestas persona-tecnología para esta intervención = ", total_technology
      ),
      x = NULL,
      y = "Porcentaje"
    ) +
    theme_experience
  
  print(p)
  
  save_plot_png_technology(
    plot = p,
    filename = paste0("experience_percentage_", clean_filename(technology_name)),
    width = 10,
    height = 6
  )
  
  return(p)
}


# ==============================================================================
# GENERAR UN GRÁFICO POR CADA TECNOLOGÍA
# ==============================================================================

technology_names <- as.character(technology_order)

plot_experience_individual_absolute_by_technology <- lapply(
  technology_names,
  plot_single_technology_absolute
)

names(plot_experience_individual_absolute_by_technology) <- technology_names

plot_experience_individual_percentage_by_technology <- lapply(
  technology_names,
  plot_single_technology_percentage
)

names(plot_experience_individual_percentage_by_technology) <- technology_names


# ==============================================================================
# FUNCIONES PARA VARIABLES SOCIODEMOGRÁFICAS
# ==============================================================================

calculate_experience_by_group <- function(data, group_col) {
  data %>%
    filter(
      !is.na(.data[[group_col]]),
      !is.na(experience_short)
    ) %>%
    count(
      group = .data[[group_col]],
      experience_short,
      sort = FALSE
    ) %>%
    group_by(group) %>%
    complete(
      experience_short = factor(experience_short_levels, levels = experience_short_levels),
      fill = list(n = 0)
    ) %>%
    mutate(
      total_group = sum(n),
      percentage = if_else(total_group > 0, n / total_group * 100, 0)
    ) %>%
    ungroup()
}

order_groups_by_awareness <- function(distribution_data) {
  distribution_data %>%
    filter(experience_short %in% awareness_order_levels) %>%
    group_by(group) %>%
    summarise(
      score_awareness = sum(n),
      .groups = "drop"
    ) %>%
    arrange(score_awareness) %>%
    pull(group)
}

get_natural_group_order <- function(distribution_data, variable_name = NULL) {
  
  natural_orders <- list(
    age_group = c(
      "Generación Z",
      "Millennials",
      "Generación X",
      "Boomers + generación silenciosa"
    ),
    city_size = c(
      "Municipio pequeño",
      "Ciudad pequeña",
      "Ciudad mediana",
      "Ciudad grande",
      "Metrópolis"
    ),
    political_orientation = c(
      "Extrema izquierda",
      "Izquierda",
      "Centro",
      "Derecha",
      "Extrema derecha"
    )
  )
  
  if (!is.null(variable_name) && variable_name %in% names(natural_orders)) {
    return(natural_orders[[variable_name]])
  }
  
  distribution_data %>%
    distinct(group) %>%
    pull(group) %>%
    as.character()
}


# ==============================================================================
# FUNCIONES DE GRÁFICOS POR GRUPO
# ==============================================================================

plot_experience_by_group <- function(distribution_data, title, subtitle, filename,
                                     width = 11, height = 6) {
  
  group_order <- order_groups_by_awareness(distribution_data)
  
  plot_data <- distribution_data %>%
    mutate(
      group_label = str_wrap(as.character(group), width = 28),
      group_label = factor(
        group_label,
        levels = str_wrap(as.character(group_order), width = 28)
      ),
      label_n = if_else(n > 0, as.character(n), ""),
      label_color = label_color_experience(experience_short)
    )
  
  p <- ggplot(
    plot_data,
    aes(
      x = group_label,
      y = n,
      fill = experience_short,
      pattern = experience_short,
      pattern_angle = experience_short
    )
  ) +
    geom_col_pattern(
      color = "black",
      linewidth = 0.15,
      pattern_fill = "black",
      pattern_colour = "black",
      pattern_density = 0.06,
      pattern_spacing = 0.045
    ) +
    geom_text(
      aes(
        label = label_n,
        color = label_color
      ),
      position = position_stack(vjust = 0.5),
      size = plot_stack_label_size,
      show.legend = FALSE
    ) +
    coord_flip() +
    scale_fill_manual(values = experience_colors, drop = FALSE) +
    scale_pattern_manual(values = experience_patterns, drop = FALSE, guide = "none") +
    scale_pattern_angle_manual(values = experience_pattern_angles, drop = FALSE, guide = "none") +
    scale_color_identity() +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Número de respuestas persona-tecnología",
      fill = "Categoría de experiencia"
    ) +
    theme_experience
  
  print(p)
  
  save_plot_png(
    plot = p,
    filename = filename,
    width = width,
    height = height
  )
  
  return(p)
}

plot_experience_percentage_by_group <- function(distribution_data, title, subtitle, filename,
                                                width = 11, height = 6) {
  
  group_order <- order_groups_by_awareness(distribution_data)
  
  plot_data <- distribution_data %>%
    mutate(
      group_label = str_wrap(as.character(group), width = 28),
      group_label = factor(
        group_label,
        levels = str_wrap(as.character(group_order), width = 28)
      ),
      label = if_else(
        n > 0,
        paste0(round(percentage, 0), "%\n(n=", n, ")"),
        ""
      ),
      label_color = label_color_experience(experience_short)
    )
  
  p <- ggplot(
    plot_data,
    aes(
      x = group_label,
      y = percentage,
      fill = experience_short,
      pattern = experience_short,
      pattern_angle = experience_short
    )
  ) +
    geom_col_pattern(
      color = "black",
      linewidth = 0.15,
      pattern_fill = "black",
      pattern_colour = "black",
      pattern_density = 0.06,
      pattern_spacing = 0.045
    ) +
    geom_text(
      aes(
        label = label,
        color = label_color
      ),
      position = position_stack(vjust = 0.5),
      size = plot_stack_label_size,
      lineheight = 0.9,
      show.legend = FALSE
    ) +
    coord_flip(clip = "off") +
    scale_fill_manual(values = experience_colors, drop = FALSE) +
    scale_pattern_manual(values = experience_patterns, drop = FALSE, guide = "none") +
    scale_pattern_angle_manual(values = experience_pattern_angles, drop = FALSE, guide = "none") +
    scale_color_identity() +
    scale_y_continuous(
      limits = c(0, 102),
      breaks = seq(0, 100, 25),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Porcentaje",
      fill = "Categoría de experiencia"
    ) +
    theme_experience
  
  print(p)
  
  save_plot_png(
    plot = p,
    filename = filename,
    width = width,
    height = height
  )
  
  return(p)
}

plot_experience_heatmap_by_group <- function(distribution_data, title, subtitle, filename,
                                             width = 11, height = 6) {
  
  group_order <- order_groups_by_awareness(distribution_data)
  
  plot_data <- distribution_data %>%
    mutate(
      group_label = str_wrap(as.character(group), width = 28),
      group_label = factor(
        group_label,
        levels = str_wrap(as.character(group_order), width = 28)
      ),
      label = paste0(round(percentage, 0), "%\n(n=", n, ")")
    )
  
  p <- ggplot(
    plot_data,
    aes(
      x = experience_short,
      y = group_label,
      fill = percentage
    )
  ) +
    geom_tile(color = "black", linewidth = 0.25) +
    geom_text(
      aes(label = label),
      size = plot_heatmap_label_size,
      lineheight = 0.9
    ) +
    scale_fill_gradient(
      low = "white",
      high = "#0072B2"
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Categoría de experiencia",
      y = NULL,
      fill = "Porcentaje"
    ) +
    theme_experience_heatmap
  
  print(p)
  
  save_plot_png(
    plot = p,
    filename = filename,
    width = width,
    height = height
  )
  
  return(p)
}


# ==============================================================================
# FUNCIONES DE GRÁFICOS POR GRUPO SIN ORDENAR POR AWARENESS
# ==============================================================================

plot_experience_by_group_no_order <- function(distribution_data, title, subtitle, filename,
                                              variable_name = NULL,
                                              width = 11, height = 6) {
  
  group_order <- get_natural_group_order(distribution_data, variable_name)
  
  plot_data <- distribution_data %>%
    mutate(
      group_label = str_wrap(as.character(group), width = 28),
      group_label = factor(
        group_label,
        levels = str_wrap(group_order, width = 28)
      ),
      label_n = if_else(n > 0, as.character(n), ""),
      label_color = label_color_experience(experience_short)
    )
  
  p <- ggplot(
    plot_data,
    aes(
      x = group_label,
      y = n,
      fill = experience_short,
      pattern = experience_short,
      pattern_angle = experience_short
    )
  ) +
    geom_col_pattern(
      color = "black",
      linewidth = 0.15,
      pattern_fill = "black",
      pattern_colour = "black",
      pattern_density = 0.06,
      pattern_spacing = 0.045
    ) +
    geom_text(
      aes(label = label_n, color = label_color),
      position = position_stack(vjust = 0.5),
      size = plot_stack_label_size,
      show.legend = FALSE
    ) +
    coord_flip() +
    scale_fill_manual(values = experience_colors, drop = FALSE) +
    scale_pattern_manual(values = experience_patterns, drop = FALSE, guide = "none") +
    scale_pattern_angle_manual(values = experience_pattern_angles, drop = FALSE, guide = "none") +
    scale_color_identity() +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Número de respuestas persona-tecnología",
      fill = "Categoría de experiencia"
    ) +
    theme_experience
  
  print(p)
  save_plot_png(p, filename, width, height)
  return(p)
}

plot_experience_percentage_by_group_no_order <- function(distribution_data, title, subtitle, filename,
                                                         variable_name = NULL,
                                                         width = 11, height = 6) {
  
  group_order <- get_natural_group_order(distribution_data, variable_name)
  
  plot_data <- distribution_data %>%
    mutate(
      group_label = str_wrap(as.character(group), width = 28),
      group_label = factor(
        group_label,
        levels = str_wrap(group_order, width = 28)
      ),
      label = if_else(
        n > 0,
        paste0(round(percentage, 0), "%\n(n=", n, ")"),
        ""
      ),
      label_color = label_color_experience(experience_short)
    )
  
  p <- ggplot(
    plot_data,
    aes(
      x = group_label,
      y = percentage,
      fill = experience_short,
      pattern = experience_short,
      pattern_angle = experience_short
    )
  ) +
    geom_col_pattern(
      color = "black",
      linewidth = 0.15,
      pattern_fill = "black",
      pattern_colour = "black",
      pattern_density = 0.06,
      pattern_spacing = 0.045
    ) +
    geom_text(
      aes(label = label, color = label_color),
      position = position_stack(vjust = 0.5),
      size = plot_stack_label_size,
      lineheight = 0.9,
      show.legend = FALSE
    ) +
    coord_flip(clip = "off") +
    scale_fill_manual(values = experience_colors, drop = FALSE) +
    scale_pattern_manual(values = experience_patterns, drop = FALSE, guide = "none") +
    scale_pattern_angle_manual(values = experience_pattern_angles, drop = FALSE, guide = "none") +
    scale_color_identity() +
    scale_y_continuous(
      limits = c(0, 102),
      breaks = seq(0, 100, 25),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Porcentaje",
      fill = "Categoría de experiencia"
    ) +
    theme_experience
  
  print(p)
  save_plot_png(p, filename, width, height)
  return(p)
}

plot_experience_heatmap_by_group_no_order <- function(distribution_data, title, subtitle, filename,
                                                      variable_name = NULL,
                                                      width = 11, height = 6) {
  
  group_order <- get_natural_group_order(distribution_data, variable_name)
  
  plot_data <- distribution_data %>%
    mutate(
      group_label = str_wrap(as.character(group), width = 28),
      group_label = factor(
        group_label,
        levels = str_wrap(group_order, width = 28)
      ),
      label = paste0(round(percentage, 0), "%\n(n=", n, ")")
    )
  
  p <- ggplot(
    plot_data,
    aes(
      x = experience_short,
      y = group_label,
      fill = percentage
    )
  ) +
    geom_tile(color = "black", linewidth = 0.25) +
    geom_text(
      aes(label = label),
      size = plot_heatmap_label_size,
      lineheight = 0.9
    ) +
    scale_fill_gradient(
      low = "white",
      high = "#0072B2"
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Categoría de experiencia",
      y = NULL,
      fill = "Porcentaje"
    ) +
    theme_experience_heatmap
  
  print(p)
  save_plot_png(p, filename, width, height)
  return(p)
}


# ==============================================================================
# EXPERIENCE POR GENERACIÓN
# ==============================================================================

experience_by_age_group <- calculate_experience_by_group(
  experience_long,
  "age_group"
)

write_csv(
  experience_by_age_group,
  file.path(csv_dir, "experience_distribution_by_age_group.csv")
)

plot_experience_by_age_group <- plot_experience_by_group(
  distribution_data = experience_by_age_group,
  title = "Respuestas de experiencia por generación",
  subtitle = paste0(
    subtitle_sample,
    ". Grupos ordenados por la suma de 'No la conoce' y 'La conoce / la consideraría'"
  ),
  filename = "experience_distribution_by_age_group_absolute",
  width = 12,
  height = 7
)

plot_experience_percentage_by_age_group <- plot_experience_percentage_by_group(
  distribution_data = experience_by_age_group,
  title = "Distribución porcentual de experiencia por generación",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada generación"
  ),
  filename = "experience_distribution_by_age_group_percentage",
  width = 12,
  height = 7
)

plot_heatmap_age_group <- plot_experience_heatmap_by_group(
  distribution_data = experience_by_age_group,
  title = "Respuestas de experiencia por generación",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada generación"
  ),
  filename = "heatmap_experience_by_age_group",
  width = 12,
  height = 7
)


# ==============================================================================
# EXPERIENCE POR GÉNERO
# ==============================================================================

experience_by_gender <- calculate_experience_by_group(
  experience_long,
  "gender"
)

write_csv(
  experience_by_gender,
  file.path(csv_dir, "experience_distribution_by_gender.csv")
)

plot_experience_by_gender <- plot_experience_by_group(
  distribution_data = experience_by_gender,
  title = "Respuestas de experiencia por género",
  subtitle = paste0(
    subtitle_sample,
    ". Grupos ordenados por la suma de 'No la conoce' y 'La conoce / la consideraría'"
  ),
  filename = "experience_distribution_by_gender_absolute",
  width = 11,
  height = 6
)

plot_experience_percentage_by_gender <- plot_experience_percentage_by_group(
  distribution_data = experience_by_gender,
  title = "Distribución porcentual de experiencia por género",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada grupo de género"
  ),
  filename = "experience_distribution_by_gender_percentage",
  width = 11,
  height = 6
)

plot_heatmap_gender <- plot_experience_heatmap_by_group(
  distribution_data = experience_by_gender,
  title = "Respuestas de experiencia por género",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada grupo de género"
  ),
  filename = "heatmap_experience_by_gender",
  width = 11,
  height = 6
)


# ==============================================================================
# EXPERIENCE POR PAÍS
# ==============================================================================

experience_by_country <- calculate_experience_by_group(
  experience_long,
  "country"
)

write_csv(
  experience_by_country,
  file.path(csv_dir, "experience_distribution_by_country.csv")
)

plot_experience_by_country <- plot_experience_by_group(
  distribution_data = experience_by_country,
  title = "Respuestas de experiencia por país",
  subtitle = paste0(
    subtitle_sample,
    ". Países ordenados por la suma de 'No la conoce' y 'La conoce / la consideraría'"
  ),
  filename = "experience_distribution_by_country_absolute",
  width = 14,
  height = 9
)

plot_experience_percentage_by_country <- plot_experience_percentage_by_group(
  distribution_data = experience_by_country,
  title = "Distribución porcentual de experiencia por país",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada país"
  ),
  filename = "experience_distribution_by_country_percentage",
  width = 14,
  height = 9
)

plot_heatmap_country <- plot_experience_heatmap_by_group(
  distribution_data = experience_by_country,
  title = "Respuestas de experiencia por país",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada país"
  ),
  filename = "heatmap_experience_by_country",
  width = 14,
  height = 9
)


# ==============================================================================
# EXPERIENCE POR REGIÓN EUROPEA
# ==============================================================================

experience_by_residence_region <- calculate_experience_by_group(
  experience_long,
  "residence_region"
)

write_csv(
  experience_by_residence_region,
  file.path(csv_dir, "experience_distribution_by_residence_region.csv")
)

plot_experience_by_residence_region <- plot_experience_by_group(
  distribution_data = experience_by_residence_region,
  title = "Respuestas de experiencia por región europea",
  subtitle = paste0(
    subtitle_sample,
    ". Regiones ordenadas por la suma de 'No la conoce' y 'La conoce / la consideraría'"
  ),
  filename = "experience_distribution_by_residence_region_absolute",
  width = 11,
  height = 6
)

plot_experience_percentage_by_residence_region <- plot_experience_percentage_by_group(
  distribution_data = experience_by_residence_region,
  title = "Distribución porcentual de experiencia por región europea",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada región europea"
  ),
  filename = "experience_distribution_by_residence_region_percentage",
  width = 11,
  height = 6
)

plot_heatmap_residence_region <- plot_experience_heatmap_by_group(
  distribution_data = experience_by_residence_region,
  title = "Respuestas de experiencia por región europea",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada región europea"
  ),
  filename = "heatmap_experience_by_residence_region",
  width = 11,
  height = 6
)


# ==============================================================================
# EXPERIENCE POR TAMAÑO DE CIUDAD
# ==============================================================================

experience_by_city_size <- calculate_experience_by_group(
  experience_long,
  "city_size"
)

write_csv(
  experience_by_city_size,
  file.path(csv_dir, "experience_distribution_by_city_size.csv")
)

plot_experience_by_city_size <- plot_experience_by_group(
  distribution_data = experience_by_city_size,
  title = "Respuestas de experiencia por tamaño de ciudad",
  subtitle = paste0(
    subtitle_sample,
    ". Grupos ordenados por la suma de 'No la conoce' y 'La conoce / la consideraría'"
  ),
  filename = "experience_distribution_by_city_size_absolute",
  width = 12,
  height = 7
)

plot_experience_percentage_by_city_size <- plot_experience_percentage_by_group(
  distribution_data = experience_by_city_size,
  title = "Distribución porcentual de experiencia por tamaño de ciudad",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada tamaño de ciudad"
  ),
  filename = "experience_distribution_by_city_size_percentage",
  width = 12,
  height = 7
)

plot_heatmap_city_size <- plot_experience_heatmap_by_group(
  distribution_data = experience_by_city_size,
  title = "Respuestas de experiencia por tamaño de ciudad",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada tamaño de ciudad"
  ),
  filename = "heatmap_experience_by_city_size",
  width = 12,
  height = 7
)


# ==============================================================================
# EXPERIENCE POR ZONA CLIMÁTICA
# ==============================================================================

experience_by_climate_zone <- calculate_experience_by_group(
  experience_long,
  "climate_zone"
)

write_csv(
  experience_by_climate_zone,
  file.path(csv_dir, "experience_distribution_by_climate_zone.csv")
)

plot_experience_by_climate_zone <- plot_experience_by_group(
  distribution_data = experience_by_climate_zone,
  title = "Respuestas de experiencia por zona climática",
  subtitle = paste0(
    subtitle_sample,
    ". Grupos ordenados por la suma de 'No la conoce' y 'La conoce / la consideraría'"
  ),
  filename = "experience_distribution_by_climate_zone_absolute",
  width = 12,
  height = 7
)

plot_experience_percentage_by_climate_zone <- plot_experience_percentage_by_group(
  distribution_data = experience_by_climate_zone,
  title = "Distribución porcentual de experiencia por zona climática",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada zona climática"
  ),
  filename = "experience_distribution_by_climate_zone_percentage",
  width = 12,
  height = 7
)

plot_heatmap_climate_zone <- plot_experience_heatmap_by_group(
  distribution_data = experience_by_climate_zone,
  title = "Respuestas de experiencia por zona climática",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada zona climática"
  ),
  filename = "heatmap_experience_by_climate_zone",
  width = 12,
  height = 7
)


# ==============================================================================
# EXPERIENCE POR SITUACIÓN LABORAL
# ==============================================================================

experience_by_employment <- calculate_experience_by_group(
  experience_long,
  "employment"
)

write_csv(
  experience_by_employment,
  file.path(csv_dir, "experience_distribution_by_employment.csv")
)

plot_experience_by_employment <- plot_experience_by_group(
  distribution_data = experience_by_employment,
  title = "Respuestas de experiencia por situación laboral",
  subtitle = paste0(
    subtitle_sample,
    ". Grupos ordenados por la suma de 'No la conoce' y 'La conoce / la consideraría'"
  ),
  filename = "experience_distribution_by_employment_absolute",
  width = 12,
  height = 7
)

plot_experience_percentage_by_employment <- plot_experience_percentage_by_group(
  distribution_data = experience_by_employment,
  title = "Distribución porcentual de experiencia por situación laboral",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada situación laboral"
  ),
  filename = "experience_distribution_by_employment_percentage",
  width = 12,
  height = 7
)

plot_heatmap_employment <- plot_experience_heatmap_by_group(
  distribution_data = experience_by_employment,
  title = "Respuestas de experiencia por situación laboral",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada situación laboral"
  ),
  filename = "heatmap_experience_by_employment",
  width = 12,
  height = 7
)


# ==============================================================================
# EXPERIENCE POR NIVEL EDUCATIVO
# ==============================================================================

experience_by_education <- calculate_experience_by_group(
  experience_long,
  "education_group"
)

write_csv(
  experience_by_education,
  file.path(csv_dir, "experience_distribution_by_education.csv")
)

plot_experience_by_education <- plot_experience_by_group(
  distribution_data = experience_by_education,
  title = "Respuestas de experiencia por nivel educativo",
  subtitle = paste0(
    subtitle_sample,
    ". Grupos ordenados por la suma de 'No la conoce' y 'La conoce / la consideraría'"
  ),
  filename = "experience_distribution_by_education_absolute",
  width = 11,
  height = 6
)

plot_experience_percentage_by_education <- plot_experience_percentage_by_group(
  distribution_data = experience_by_education,
  title = "Distribución porcentual de experiencia por nivel educativo",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada nivel educativo"
  ),
  filename = "experience_distribution_by_education_percentage",
  width = 11,
  height = 6
)

plot_heatmap_education <- plot_experience_heatmap_by_group(
  distribution_data = experience_by_education,
  title = "Respuestas de experiencia por nivel educativo",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada nivel educativo"
  ),
  filename = "heatmap_experience_by_education",
  width = 11,
  height = 6
)


# ==============================================================================
# EXPERIENCE POR TRABAJO/ESTUDIO DESDE CASA
# ==============================================================================

experience_by_work_home <- calculate_experience_by_group(
  experience_long,
  "work_home"
)

write_csv(
  experience_by_work_home,
  file.path(csv_dir, "experience_distribution_by_work_home.csv")
)

plot_experience_by_work_home <- plot_experience_by_group(
  distribution_data = experience_by_work_home,
  title = "Respuestas de experiencia según trabajo/estudio desde casa",
  subtitle = paste0(
    subtitle_sample,
    ". Grupos ordenados por la suma de 'No la conoce' y 'La conoce / la consideraría'"
  ),
  filename = "experience_distribution_by_work_home_absolute",
  width = 11,
  height = 6
)

plot_experience_percentage_by_work_home <- plot_experience_percentage_by_group(
  distribution_data = experience_by_work_home,
  title = "Distribución porcentual de experiencia según trabajo/estudio desde casa",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada grupo"
  ),
  filename = "experience_distribution_by_work_home_percentage",
  width = 11,
  height = 6
)

plot_heatmap_work_home <- plot_experience_heatmap_by_group(
  distribution_data = experience_by_work_home,
  title = "Respuestas de experiencia según trabajo/estudio desde casa",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada grupo"
  ),
  filename = "heatmap_experience_by_work_home",
  width = 11,
  height = 6
)


# ==============================================================================
# EXPERIENCE POR TIPO DE HOGAR
# ==============================================================================

experience_by_type_house <- calculate_experience_by_group(
  experience_long,
  "type_house"
)

write_csv(
  experience_by_type_house,
  file.path(csv_dir, "experience_distribution_by_type_house.csv")
)

plot_experience_by_type_house <- plot_experience_by_group(
  distribution_data = experience_by_type_house,
  title = "Respuestas de experiencia por tipo de hogar",
  subtitle = paste0(
    subtitle_sample,
    ". Grupos ordenados por la suma de 'No la conoce' y 'La conoce / la consideraría'"
  ),
  filename = "experience_distribution_by_type_house_absolute",
  width = 12,
  height = 7
)

plot_experience_percentage_by_type_house <- plot_experience_percentage_by_group(
  distribution_data = experience_by_type_house,
  title = "Distribución porcentual de experiencia por tipo de hogar",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada tipo de hogar"
  ),
  filename = "experience_distribution_by_type_house_percentage",
  width = 12,
  height = 7
)

plot_heatmap_type_house <- plot_experience_heatmap_by_group(
  distribution_data = experience_by_type_house,
  title = "Respuestas de experiencia por tipo de hogar",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada tipo de hogar"
  ),
  filename = "heatmap_experience_by_type_house",
  width = 12,
  height = 7
)


# ==============================================================================
# EXPERIENCE POR RÉGIMEN DE TENENCIA
# ==============================================================================

experience_by_tenure <- calculate_experience_by_group(
  experience_long,
  "tenure"
)

write_csv(
  experience_by_tenure,
  file.path(csv_dir, "experience_distribution_by_tenure.csv")
)

plot_experience_by_tenure <- plot_experience_by_group(
  distribution_data = experience_by_tenure,
  title = "Respuestas de experiencia por régimen de tenencia",
  subtitle = paste0(
    subtitle_sample,
    ". Grupos ordenados por la suma de 'No la conoce' y 'La conoce / la consideraría'"
  ),
  filename = "experience_distribution_by_tenure_absolute",
  width = 12,
  height = 7
)

plot_experience_percentage_by_tenure <- plot_experience_percentage_by_group(
  distribution_data = experience_by_tenure,
  title = "Distribución porcentual de experiencia por régimen de tenencia",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada régimen de tenencia"
  ),
  filename = "experience_distribution_by_tenure_percentage",
  width = 12,
  height = 7
)

plot_heatmap_tenure <- plot_experience_heatmap_by_group(
  distribution_data = experience_by_tenure,
  title = "Respuestas de experiencia por régimen de tenencia",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada régimen de tenencia"
  ),
  filename = "heatmap_experience_by_tenure",
  width = 12,
  height = 7
)


# ==============================================================================
# EXPERIENCE POR ORIENTACIÓN POLÍTICA
# ==============================================================================

experience_by_political_orientation <- calculate_experience_by_group(
  experience_long,
  "political_orientation"
)

write_csv(
  experience_by_political_orientation,
  file.path(csv_dir, "experience_distribution_by_political_orientation.csv")
)

plot_experience_by_political_orientation <- plot_experience_by_group(
  distribution_data = experience_by_political_orientation,
  title = "Respuestas de experiencia por orientación política",
  subtitle = paste0(
    subtitle_sample,
    ". Grupos ordenados por la suma de 'No la conoce' y 'La conoce / la consideraría'"
  ),
  filename = "experience_distribution_by_political_orientation_absolute",
  width = 12,
  height = 7
)

plot_experience_percentage_by_political_orientation <- plot_experience_percentage_by_group(
  distribution_data = experience_by_political_orientation,
  title = "Distribución porcentual de experiencia por orientación política",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada grupo de orientación política"
  ),
  filename = "experience_distribution_by_political_orientation_percentage",
  width = 12,
  height = 7
)

plot_heatmap_political_orientation <- plot_experience_heatmap_by_group(
  distribution_data = experience_by_political_orientation,
  title = "Respuestas de experiencia por orientación política",
  subtitle = paste0(
    subtitle_sample,
    ". Porcentajes calculados dentro de cada grupo de orientación política"
  ),
  filename = "heatmap_experience_by_political_orientation",
  width = 12,
  height = 7
)


# ==============================================================================
# MISMOS GRÁFICOS SIN ORDENAR POR AWARENESS
# ==============================================================================

variables_no_order <- list(
  age_group = list(data = experience_by_age_group, label = "generación", width = 12, height = 7),
  gender = list(data = experience_by_gender, label = "género", width = 11, height = 6),
  country = list(data = experience_by_country, label = "país", width = 14, height = 9),
  residence_region = list(data = experience_by_residence_region, label = "región europea", width = 11, height = 6),
  city_size = list(data = experience_by_city_size, label = "tamaño de ciudad", width = 12, height = 7),
  climate_zone = list(data = experience_by_climate_zone, label = "zona climática", width = 12, height = 7),
  employment = list(data = experience_by_employment, label = "situación laboral", width = 12, height = 7),
  education_group = list(data = experience_by_education, label = "nivel educativo", width = 11, height = 6),
  work_home = list(data = experience_by_work_home, label = "trabajo/estudio desde casa", width = 11, height = 6),
  type_house = list(data = experience_by_type_house, label = "tipo de hogar", width = 12, height = 7),
  tenure = list(data = experience_by_tenure, label = "régimen de tenencia", width = 12, height = 7),
  political_orientation = list(data = experience_by_political_orientation, label = "orientación política", width = 12, height = 7)
)

plots_no_order <- list()

for (var_name in names(variables_no_order)) {
  
  var_data <- variables_no_order[[var_name]]$data
  var_label <- variables_no_order[[var_name]]$label
  var_width <- variables_no_order[[var_name]]$width
  var_height <- variables_no_order[[var_name]]$height
  
  plots_no_order[[paste0(var_name, "_absolute_no_order")]] <-
    plot_experience_by_group_no_order(
      distribution_data = var_data,
      title = paste("Respuestas de experiencia por", var_label),
      subtitle = paste0(subtitle_sample, ". Grupos sin ordenar por awareness"),
      filename = paste0("experience_distribution_by_", var_name, "_absolute_no_order"),
      variable_name = var_name,
      width = var_width,
      height = var_height
    )
  
  plots_no_order[[paste0(var_name, "_percentage_no_order")]] <-
    plot_experience_percentage_by_group_no_order(
      distribution_data = var_data,
      title = paste("Distribución porcentual de experiencia por", var_label),
      subtitle = paste0(subtitle_sample, ". Grupos sin ordenar por awareness"),
      filename = paste0("experience_distribution_by_", var_name, "_percentage_no_order"),
      variable_name = var_name,
      width = var_width,
      height = var_height
    )
  
  plots_no_order[[paste0(var_name, "_heatmap_no_order")]] <-
    plot_experience_heatmap_by_group_no_order(
      distribution_data = var_data,
      title = paste("Respuestas de experiencia por", var_label),
      subtitle = paste0(subtitle_sample, ". Grupos sin ordenar por awareness"),
      filename = paste0("heatmap_experience_by_", var_name, "_no_order"),
      variable_name = var_name,
      width = var_width,
      height = var_height
    )
}


# ==============================================================================
# TABLA RESUMEN DE PORCENTAJES POR VARIABLE
# ==============================================================================

create_summary_table_by_group <- function(data, group_col, variable_label) {
  
  distribution_data <- calculate_experience_by_group(data, group_col)
  
  participants_by_group <- data %>%
    filter(!is.na(.data[[group_col]])) %>%
    distinct(participant_id, group = .data[[group_col]]) %>%
    count(group, name = "n_participants")
  
  distribution_data %>%
    left_join(participants_by_group, by = "group") %>%
    mutate(
      variable = variable_label
    ) %>%
    select(
      variable,
      group,
      n_participants,
      experience_short,
      n_pairs = n,
      total_pairs = total_group,
      percentage
    )
}

summary_experience_by_variables <- bind_rows(
  create_summary_table_by_group(experience_long, "age_group", "Generación"),
  create_summary_table_by_group(experience_long, "gender", "Género"),
  create_summary_table_by_group(experience_long, "country", "País"),
  create_summary_table_by_group(experience_long, "residence_region", "Región europea"),
  create_summary_table_by_group(experience_long, "city_size", "Tamaño de ciudad"),
  create_summary_table_by_group(experience_long, "climate_zone", "Zona climática"),
  create_summary_table_by_group(experience_long, "employment", "Situación laboral"),
  create_summary_table_by_group(experience_long, "education_group", "Nivel educativo"),
  create_summary_table_by_group(experience_long, "work_home", "Trabajo/estudio desde casa"),
  create_summary_table_by_group(experience_long, "type_house", "Tipo de hogar"),
  create_summary_table_by_group(experience_long, "tenure", "Régimen de tenencia"),
  create_summary_table_by_group(experience_long, "political_orientation", "Orientación política")
)

write_csv(
  summary_experience_by_variables,
  file.path(csv_dir, "summary_experience_by_variables.csv")
)

print(summary_experience_by_variables, n = Inf)


# ==============================================================================
# EDAD DE RENOVACIONES YA EXISTENTES O IMPLEMENTADAS
# ==============================================================================

implemented_experience_levels <- c(
  "Already present when I moved in",
  "I have installed / implemented / am currently doing this myself"
)

renovation_age_options_raw <- df %>%
  select(all_of(renovation_age_cols)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "renovation_age_col",
    values_to = "renovation_age_raw"
  ) %>%
  mutate(
    renovation_age_raw = str_squish(as.character(renovation_age_raw)),
    renovation_age_raw = na_if(renovation_age_raw, "")
  ) %>%
  filter(!is.na(renovation_age_raw)) %>%
  count(renovation_age_raw, sort = TRUE)

write_csv(
  renovation_age_options_raw,
  file.path(csv_dir, "renovation_age_options_raw.csv")
)

print(renovation_age_options_raw, n = Inf)


renovation_age_levels <- c(
  "Hace menos de 5 años",
  "Hace 5-15 años",
  "Hace 15-25 años",
  "Hace más de 25 años / nunca actualizada",
  "No lo sé"
)

clean_renovation_age <- function(x) {
  x <- str_squish(as.character(x))
  x <- str_remove(x, "\\s*\\(ID[0-9]+\\)$")
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    x == "Yes, less than 5 years ago" ~ "Hace menos de 5 años",
    x == "Yes, between 15 and 5 years ago" ~ "Hace 5-15 años",
    x == "Yes, between 25 and 15 years ago" ~ "Hace 15-25 años",
    x == "Never or more than 25 years ago" ~ "Hace más de 25 años / nunca actualizada",
    x == "I do not know" ~ "No lo sé",
    TRUE ~ NA_character_
  )
}


# ==============================================================================
# TABLA LARGA CON LA ETAPA DE EXPERIENCE
# ==============================================================================

experience_stage_for_age <- df %>%
  select(
    participant_id,
    all_of(experience_tech_cols)
  ) %>%
  pivot_longer(
    cols = all_of(experience_tech_cols),
    names_to = "technology_col",
    values_to = "experience_raw"
  ) %>%
  mutate(
    technology_index = match(technology_col, experience_tech_cols),
    technology = clean_technology(technology_col),
    
    experience_raw = str_squish(as.character(experience_raw)),
    experience_raw = na_if(experience_raw, ""),
    experience_clean = recode_experience(experience_raw)
  )


# ==============================================================================
# TABLA LARGA CON EDAD DE RENOVACIÓN
# ==============================================================================

renovation_age_long <- df %>%
  select(
    participant_id,
    all_of(renovation_age_cols)
  ) %>%
  pivot_longer(
    cols = all_of(renovation_age_cols),
    names_to = "renovation_age_col",
    values_to = "renovation_age_raw"
  ) %>%
  mutate(
    technology_index = match(renovation_age_col, renovation_age_cols),
    
    renovation_age_raw = str_squish(as.character(renovation_age_raw)),
    renovation_age_raw = na_if(renovation_age_raw, ""),
    renovation_age_clean = clean_renovation_age(renovation_age_raw)
  ) %>%
  left_join(
    experience_stage_for_age %>%
      select(
        participant_id,
        technology_index,
        technology,
        experience_clean
      ),
    by = c("participant_id", "technology_index")
  ) %>%
  filter(
    experience_clean %in% implemented_experience_levels,
    !is.na(renovation_age_clean)
  ) %>%
  mutate(
    renovation_age_clean = factor(
      renovation_age_clean,
      levels = renovation_age_levels
    )
  )

write_csv(
  renovation_age_long,
  file.path(csv_dir, "renovation_age_long.csv")
)

glimpse(renovation_age_long)


n_renovation_participants <- n_distinct(renovation_age_long$participant_id)
n_renovation_pairs <- nrow(renovation_age_long)

subtitle_renovation_age <- paste0(
  "Solo tecnologías ya presentes o implementadas. n participantes = ",
  n_renovation_participants,
  "; observaciones persona-tecnología = ",
  n_renovation_pairs
)


# ==============================================================================
# DISTRIBUCIÓN DE EDAD DE RENOVACIÓN POR TECNOLOGÍA
# ==============================================================================

renovation_age_distribution_by_technology <- renovation_age_long %>%
  count(technology, renovation_age_clean, sort = FALSE) %>%
  group_by(technology) %>%
  complete(
    renovation_age_clean = factor(renovation_age_levels, levels = renovation_age_levels),
    fill = list(n = 0)
  ) %>%
  mutate(
    total_technology = sum(n),
    percentage = if_else(total_technology > 0, n / total_technology * 100, 0),
    label = if_else(
      n > 0,
      paste0(round(percentage, 0), "%\n(n=", n, ")"),
      ""
    )
  ) %>%
  ungroup()

write_csv(
  renovation_age_distribution_by_technology,
  file.path(csv_dir, "renovation_age_distribution_by_technology.csv")
)

print(renovation_age_distribution_by_technology, n = Inf)


renovation_age_distribution_by_technology_plot <- renovation_age_distribution_by_technology %>%
  mutate(
    technology = factor(technology, levels = technology_order),
    technology_label = str_wrap(as.character(technology), width = 32),
    technology_label = factor(
      technology_label,
      levels = str_wrap(as.character(technology_order), width = 32)
    )
  )


# ==============================================================================
# BARRAS APILADAS DE EDAD DE RENOVACIÓN
# ==============================================================================

plot_renovation_age_by_technology <- ggplot(
  renovation_age_distribution_by_technology_plot,
  aes(
    x = technology_label,
    y = n,
    fill = renovation_age_clean
  )
) +
  geom_col(color = "black", linewidth = 0.15) +
  coord_flip() +
  labs(
    title = "Antigüedad de las renovaciones existentes o implementadas por tecnología",
    subtitle = subtitle_renovation_age,
    x = NULL,
    y = "Número de respuestas persona-tecnología",
    fill = "Antigüedad"
  ) +
  theme_experience

print(plot_renovation_age_by_technology)

save_plot_png(
  plot = plot_renovation_age_by_technology,
  filename = "renovation_age_distribution_by_technology_absolute",
  width = 14,
  height = 9
)


# ==============================================================================
# BARRAS APILADAS DE EDAD DE RENOVACIÓN EN PORCENTAJE
# ==============================================================================

plot_renovation_age_by_technology_percentage <- ggplot(
  renovation_age_distribution_by_technology_plot,
  aes(
    x = technology_label,
    y = percentage,
    fill = renovation_age_clean
  )
) +
  geom_col(color = "black", linewidth = 0.15) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = plot_stack_label_size,
    lineheight = 0.9
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    limits = c(0, 102),
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Distribución porcentual de antigüedad de renovaciones por tecnología",
    subtitle = paste0(
      subtitle_renovation_age,
      ". Porcentajes calculados dentro de cada tecnología"
    ),
    x = NULL,
    y = "Porcentaje",
    fill = "Antigüedad"
  ) +
  theme_experience

print(plot_renovation_age_by_technology_percentage)

save_plot_png(
  plot = plot_renovation_age_by_technology_percentage,
  filename = "renovation_age_distribution_by_technology_percentage",
  width = 14,
  height = 9
)


# ==============================================================================
# HEATMAP DE EDAD DE RENOVACIÓN
# ==============================================================================

plot_renovation_age_heatmap <- ggplot(
  renovation_age_distribution_by_technology_plot,
  aes(
    x = renovation_age_clean,
    y = technology_label,
    fill = percentage
  )
) +
  geom_tile(color = "black", linewidth = 0.25) +
  geom_text(
    aes(label = label),
    size = plot_heatmap_label_size,
    lineheight = 0.9
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#0072B2"
  ) +
  labs(
    title = "Antigüedad de las renovaciones existentes o implementadas por tecnología",
    subtitle = paste0(
      subtitle_renovation_age,
      ". Porcentajes calculados dentro de cada tecnología"
    ),
    x = "Antigüedad de la renovación",
    y = NULL,
    fill = "Porcentaje"
  ) +
  theme_experience_heatmap

print(plot_renovation_age_heatmap)

save_plot_png(
  plot = plot_renovation_age_heatmap,
  filename = "renovation_age_heatmap_by_technology",
  width = 14,
  height = 9
)


# ==============================================================================
# GUARDAR TODOS LOS GRÁFICOS EN UN ÚNICO PDF
# ==============================================================================

save_plots_pdf <- function(plot_list, filename, width = 14, height = 9) {
  pdf(
    file = file.path(pdf_dir, filename),
    width = width,
    height = height,
    onefile = TRUE
  )
  
  for (p in plot_list) {
    print(p)
  }
  
  dev.off()
}

all_plots <- c(
  list(
    plot_experience_total,
    
    plot_experience_by_technology,
    plot_experience_by_technology_percentage,
    plot_experience_heatmap
  ),
  
  plot_experience_individual_absolute_by_technology,
  plot_experience_individual_percentage_by_technology,
  
  list(
    plot_experience_by_age_group,
    plot_experience_percentage_by_age_group,
    plot_heatmap_age_group,
    plots_no_order[["age_group_absolute_no_order"]],
    plots_no_order[["age_group_percentage_no_order"]],
    plots_no_order[["age_group_heatmap_no_order"]],
    
    plot_experience_by_gender,
    plot_experience_percentage_by_gender,
    plot_heatmap_gender,
    plots_no_order[["gender_absolute_no_order"]],
    plots_no_order[["gender_percentage_no_order"]],
    plots_no_order[["gender_heatmap_no_order"]],
    
    plot_experience_by_country,
    plot_experience_percentage_by_country,
    plot_heatmap_country,
    plots_no_order[["country_absolute_no_order"]],
    plots_no_order[["country_percentage_no_order"]],
    plots_no_order[["country_heatmap_no_order"]],
    
    plot_experience_by_residence_region,
    plot_experience_percentage_by_residence_region,
    plot_heatmap_residence_region,
    plots_no_order[["residence_region_absolute_no_order"]],
    plots_no_order[["residence_region_percentage_no_order"]],
    plots_no_order[["residence_region_heatmap_no_order"]],
    
    plot_experience_by_city_size,
    plot_experience_percentage_by_city_size,
    plot_heatmap_city_size,
    plots_no_order[["city_size_absolute_no_order"]],
    plots_no_order[["city_size_percentage_no_order"]],
    plots_no_order[["city_size_heatmap_no_order"]],
    
    plot_experience_by_climate_zone,
    plot_experience_percentage_by_climate_zone,
    plot_heatmap_climate_zone,
    plots_no_order[["climate_zone_absolute_no_order"]],
    plots_no_order[["climate_zone_percentage_no_order"]],
    plots_no_order[["climate_zone_heatmap_no_order"]],
    
    plot_experience_by_employment,
    plot_experience_percentage_by_employment,
    plot_heatmap_employment,
    plots_no_order[["employment_absolute_no_order"]],
    plots_no_order[["employment_percentage_no_order"]],
    plots_no_order[["employment_heatmap_no_order"]],
    
    plot_experience_by_education,
    plot_experience_percentage_by_education,
    plot_heatmap_education,
    plots_no_order[["education_group_absolute_no_order"]],
    plots_no_order[["education_group_percentage_no_order"]],
    plots_no_order[["education_group_heatmap_no_order"]],
    
    plot_experience_by_work_home,
    plot_experience_percentage_by_work_home,
    plot_heatmap_work_home,
    plots_no_order[["work_home_absolute_no_order"]],
    plots_no_order[["work_home_percentage_no_order"]],
    plots_no_order[["work_home_heatmap_no_order"]],
    
    plot_experience_by_type_house,
    plot_experience_percentage_by_type_house,
    plot_heatmap_type_house,
    plots_no_order[["type_house_absolute_no_order"]],
    plots_no_order[["type_house_percentage_no_order"]],
    plots_no_order[["type_house_heatmap_no_order"]],
    
    plot_experience_by_tenure,
    plot_experience_percentage_by_tenure,
    plot_heatmap_tenure,
    plots_no_order[["tenure_absolute_no_order"]],
    plots_no_order[["tenure_percentage_no_order"]],
    plots_no_order[["tenure_heatmap_no_order"]],
    
    plot_experience_by_political_orientation,
    plot_experience_percentage_by_political_orientation,
    plot_heatmap_political_orientation,
    plots_no_order[["political_orientation_absolute_no_order"]],
    plots_no_order[["political_orientation_percentage_no_order"]],
    plots_no_order[["political_orientation_heatmap_no_order"]],
    
    plot_renovation_age_by_technology,
    plot_renovation_age_by_technology_percentage,
    plot_renovation_age_heatmap
  )
)

all_plots <- all_plots[!sapply(all_plots, is.null)]

save_plots_pdf(
  plot_list = all_plots,
  filename = "experience_all_plots.pdf",
  width = 14,
  height = 9
)

cat("Gráficos guardados en:\n")
cat(plots_dir, "\n")
cat("PDF conjunto guardado en:\n")
cat(file.path(pdf_dir, "experience_all_plots.pdf"), "\n")