
# SCRIPT 03.1 - DESCRIPCIÓN DE LA MUESTRA

# Este script realiza la descripción exploratoria de la muestra.
#
# Parte del dataset limpio sociodemográfico generado en el Script 03:
#   initial_descriptive_analysis/output/clean_datasets/df_clean_sociodemographic.csv
#
# 1. Cargar el dataset limpio para análisis de muestra.
# 2. Crear variables sociodemográficas limpias y agrupadas.
# 3. Generar tablas resumen.
# 4. Generar gráficos en PNG y PDF.
# 5. Generar un PDF conjunto con todos los gráficos.

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(tibble)


# RUTAS
base_input_dir <- "initial_descriptive_analysis/output/clean_datasets"
base_output_dir <- "initial_descriptive_analysis/output/sample_description"

csv_dir <- file.path(base_output_dir, "csv")
plots_dir <- file.path(base_output_dir, "plots")
pdf_dir <- file.path(base_output_dir, "pdf")
logs_dir <- file.path(base_output_dir, "logs")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)


# CARGAR DATOS
df <- read_csv(
  file.path(base_input_dir, "df_clean_sociodemographic.csv"),
  show_col_types = FALSE
)

cat("Dataset cargado: df_clean_sociodemographic.csv\n")
cat("Filas:", nrow(df), "\n")
cat("Columnas:", ncol(df), "\n")

# FUNCIONES AUXILIARES GENERALES
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

coalesce_optional_cols <- function(data, candidates) {
  existing <- candidates[candidates %in% names(data)]
  
  if (length(existing) == 0) {
    return(rep(NA_character_, nrow(data)))
  }
  
  values <- lapply(existing, function(col) as.character(data[[col]]))
  do.call(coalesce, values)
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

# CREAR PARTICIPANT_ID
df <- df %>%
  mutate(
    participant_id = coalesce(
      as.character(get_optional_col(., "participant_id")),
      as.character(get_optional_col(., "join_key")),
      as.character(get_optional_col(., "prolific_id")),
      as.character(get_optional_col(., "identification_code")),
      as.character(row_number())
    )
  )

# DEFINIR COLUMNAS
year_of_birth_candidates <- c(
  "year_of_birth_clean",
  "please_enter_your_year_of_birth_final",
  "year_of_birth"
)

age_candidates <- c(
  "age_clean",
  "age"
)

gender_col <- "what_is_your_gender_final"
country_col <- "in_which_country_do_you_currently_live_final"
country_clean_col <- "country_clean"
residence_region_col <- "residence_region"
size_city_col <- "what_is_the_approximate_population_size_of_the_city_where_you_live_final"
climate_zone_col <- "in_which_climate_zone_do_you_live_please_select_the_option_that_corresponds_to_your_location_if_you_re_not_sure_which_climate_zone_you_live_in_please_refer_to_the_map_below_or_select_the_option_that_best_matches_your_region_final"
employment_col <- "what_is_your_current_employment_status_please_indicate_your_main_contractual_status_if_you_are_temporarily_on_leave_such_as_sick_leave_parental_leave_or_a_temporary_reduction_in_working_hours_please_report_your_usual_employment_status_final"
education_level_col <- "what_is_the_highest_level_of_education_that_you_have_completed_if_you_are_currently_studying_and_have_not_yet_completed_a_level_please_select_the_last_level_you_have_finished_final"
work_home_col <- "do_you_currently_work_or_study_from_home_final"
type_house_col <- "what_type_of_household_do_you_live_in_please_select_the_option_that_best_describes_your_household_final"
tenure_col <- "what_is_the_current_tenure_status_of_your_home_final"
political_col <- "on_a_scale_from_0_to_100_where_0_means_most_left_and_100_means_most_right_where_would_you_place_yourself_politically_final"
vote_col <- "which_of_the_following_best_describes_your_general_approach_to_voting_in_elections_final"

# FUNCIONES DE LIMPIEZA SOCIODEMOGRÁFICA
clean_gender <- function(x) {
  x <- clean_text_basic(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("^male", ignore_case = TRUE)) ~ "Hombre",
    str_detect(x, regex("^female", ignore_case = TRUE)) ~ "Mujer",
    TRUE ~ "Otro / Prefiere no decirlo"
  )
}

clean_country <- function(x) {
  x <- clean_text_basic(x)
  x <- str_remove(x, "\\s*\\(ID[0-9]+\\)$")
  x <- str_remove(x, "^[A-Z]{2}\\s*[–-]\\s*")
  
  x <- case_when(
    is.na(x) | x == "" ~ NA_character_,
    x %in% c("United Kingdom *", "UK", "Great Britain", "United Kingdom") ~ "United Kingdom",
    x %in% c("Czechia", "Czech Republic") ~ "Czech Republic",
    x %in% c("Moldova (Republic of Moldova)", "Moldova") ~ "Moldova",
    x %in% c("Russia", "Russian Federation") ~ "Russia",
    TRUE ~ x
  )
  
  country_map <- c(
    "Albania" = "Albania",
    "Andorra" = "Andorra",
    "Austria" = "Austria",
    "Belarus" = "Bielorrusia",
    "Belgium" = "Bélgica",
    "Bosnia and Herzegovina" = "Bosnia y Herzegovina",
    "Bulgaria" = "Bulgaria",
    "Croatia" = "Croacia",
    "Cyprus" = "Chipre",
    "Czech Republic" = "República Checa",
    "Denmark" = "Dinamarca",
    "Estonia" = "Estonia",
    "Finland" = "Finlandia",
    "France" = "Francia",
    "Germany" = "Alemania",
    "Greece" = "Grecia",
    "Hungary" = "Hungría",
    "Iceland" = "Islandia",
    "Ireland" = "Irlanda",
    "Italy" = "Italia",
    "Latvia" = "Letonia",
    "Liechtenstein" = "Liechtenstein",
    "Lithuania" = "Lituania",
    "Luxembourg" = "Luxemburgo",
    "Malta" = "Malta",
    "Moldova" = "Moldavia",
    "Monaco" = "Mónaco",
    "Montenegro" = "Montenegro",
    "Netherlands" = "Países Bajos",
    "North Macedonia" = "Macedonia del Norte",
    "Norway" = "Noruega",
    "Poland" = "Polonia",
    "Portugal" = "Portugal",
    "Romania" = "Rumanía",
    "Russia" = "Rusia",
    "San Marino" = "San Marino",
    "Serbia" = "Serbia",
    "Slovakia" = "Eslovaquia",
    "Slovenia" = "Eslovenia",
    "Spain" = "España",
    "Sweden" = "Suecia",
    "Switzerland" = "Suiza",
    "Ukraine" = "Ucrania",
    "United Kingdom" = "Reino Unido"
  )
  
  recode(x, !!!country_map, .default = x)
}

clean_residence_region <- function(country) {
  country <- clean_country(country)
  
  case_when(
    country %in% c(
      "Dinamarca", "Estonia", "Finlandia", "Irlanda", "Islandia",
      "Letonia", "Lituania", "Noruega", "Reino Unido", "Suecia"
    ) ~ "Europa del Norte",
    
    country %in% c(
      "Alemania", "Austria", "Bélgica", "Francia", "Liechtenstein",
      "Luxemburgo", "Mónaco", "Países Bajos", "Suiza"
    ) ~ "Europa Occidental",
    
    country %in% c(
      "Albania", "Andorra", "Bosnia y Herzegovina", "Croacia",
      "Eslovenia", "España", "Grecia", "Italia", "Malta",
      "Montenegro", "Portugal", "Macedonia del Norte", "San Marino",
      "Serbia", "Chipre"
    ) ~ "Europa del Sur",
    
    country %in% c(
      "Bielorrusia", "Bulgaria", "Eslovaquia", "Hungría",
      "Moldavia", "Polonia", "República Checa", "Rumanía",
      "Rusia", "Ucrania"
    ) ~ "Europa del Este",
    
    is.na(country) ~ NA_character_,
    TRUE ~ "Otra región"
  )
}

clean_city_size <- function(x) {
  x <- clean_text_basic(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("^Village", ignore_case = TRUE)) ~ "Aldea o núcleo rural (<1.000 hab.)",
    str_detect(x, regex("^Small town", ignore_case = TRUE)) ~ "Municipio pequeño (1.000-10.000 hab.)",
    str_detect(x, regex("^Town", ignore_case = TRUE)) ~ "Municipio mediano (10.000-50.000 hab.)",
    str_detect(x, regex("^Small city", ignore_case = TRUE)) ~ "Ciudad pequeña (50.000-250.000 hab.)",
    str_detect(x, regex("^Medium city", ignore_case = TRUE)) ~ "Ciudad mediana (250.000-500.000 hab.)",
    str_detect(x, regex("^Large city", ignore_case = TRUE)) ~ "Ciudad grande (500.000-1.000.000 hab.)",
    str_detect(x, regex("^Global city", ignore_case = TRUE)) ~ "Metrópolis (>1.000.000 hab.)",
    TRUE ~ NA_character_
  )
}

clean_climate_zone <- function(x) {
  x <- clean_text_basic(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    
    str_detect(x, regex("Subtropical", ignore_case = TRUE)) ~ 
      "Clima cálido/mediterráneo",
    str_detect(x, regex("Temperate Climates - Maritime|Temperate Climates - Transitional|Temperate Climates - Intermediate|Temperate Climates - Continental", ignore_case = TRUE)) ~ 
      "Clima templado",
    str_detect(x, regex("Cold Climates|Circumpolar", ignore_case = TRUE)) ~ 
      "Clima frío/polar",
    TRUE ~ NA_character_
  )
}

clean_employment <- function(x) {
  x <- clean_text_basic(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("Full time employed|Part-time employed|Self-employed", ignore_case = TRUE)) ~ 
      "Empleado/a",
    str_detect(x, regex("Student", ignore_case = TRUE)) ~ 
      "Estudiante",
    str_detect(x, regex("Unemployed|Retired|Stay-at-home", ignore_case = TRUE)) ~ 
      "Otra situación laboral",
    TRUE ~ "Otra situación laboral"
  )
}

clean_education <- function(x) {
  x <- clean_text_basic(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("University", ignore_case = TRUE)) ~ "Universitaria",
    str_detect(x, regex("Primary|Secondary|Vocational", ignore_case = TRUE)) ~ "No universitaria",
    TRUE ~ NA_character_
  )
}

clean_work_home <- function(x) {
  x <- clean_text_basic(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("^Yes", ignore_case = TRUE)) ~ "Sí",
    str_detect(x, regex("^No", ignore_case = TRUE)) ~ "No",
    TRUE ~ NA_character_
  )
}

clean_type_house <- function(x) {
  x <- clean_text_basic(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("Uni-personal", ignore_case = TRUE)) ~ "Vive solo/a",
    str_detect(x, regex("Dual Income No Kids|Ageing family", ignore_case = TRUE)) ~ "Pareja sin hijos",
    str_detect(x, regex("Traditional family|Large family|Single parenthood", ignore_case = TRUE)) ~ "Hogar con hijos",
    str_detect(x, regex("Poly-nuclear", ignore_case = TRUE)) ~ "Vivienda compartida",
    TRUE ~ "Otro tipo de hogar"
  )
}

clean_tenure <- function(x) {
  x <- clean_text_basic(x)
  
  case_when(
    is.na(x) ~ NA_character_,
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


clean_vote <- function(x) {
  x <- clean_text_basic(x)
  x <- str_remove(x, "\\s*\\(ID[0-9]+\\)$")
  
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("pro-independence|regionalist", ignore_case = TRUE)) ~ "Partidos regionalistas/independentistas",
    str_detect(x, regex("candidate/program|type of election", ignore_case = TRUE)) ~ "Voto variable según candidato/programa",
    str_detect(x, regex("do not vote|blank|null|abstain", ignore_case = TRUE)) ~ "No vota / blanco / nulo",
    str_detect(x, regex("national parties", ignore_case = TRUE)) ~ "Partidos nacionales",
    str_detect(x, regex("other options", ignore_case = TRUE)) ~ "Otras opciones",
    TRUE ~ "Otra respuesta"
  )
}


# LIMPIAR EDAD Y GENERACIÓN
project_year <- 2026

year_of_birth_raw <- suppressWarnings(
  as.numeric(coalesce_optional_cols(df, year_of_birth_candidates))
)

age_raw <- suppressWarnings(
  as.numeric(coalesce_optional_cols(df, age_candidates))
)

df <- df %>%
  mutate(
    year_of_birth_raw = year_of_birth_raw,
    age_raw = age_raw,
    
    year_of_birth_clean = case_when(
      !is.na(year_of_birth_raw) & year_of_birth_raw >= 1900 & year_of_birth_raw <= 2007 ~ year_of_birth_raw,
      !is.na(year_of_birth_raw) & year_of_birth_raw >= 18 & year_of_birth_raw <= 100 ~ project_year - year_of_birth_raw,
      !is.na(age_raw) & age_raw >= 18 & age_raw <= 100 ~ project_year - age_raw,
      TRUE ~ NA_real_
    ),
    
    age_clean = case_when(
      !is.na(year_of_birth_clean) ~ project_year - year_of_birth_clean,
      !is.na(age_raw) & age_raw >= 18 & age_raw <= 100 ~ age_raw,
      TRUE ~ NA_real_
    ),
    
    age_group = case_when(
      year_of_birth_clean >= 2001 & year_of_birth_clean <= 2007 ~ "Generación Z",
      year_of_birth_clean >= 1986 & year_of_birth_clean <= 2000 ~ "Millennials",
      year_of_birth_clean >= 1971 & year_of_birth_clean <= 1985 ~ "Generación X",
      year_of_birth_clean >= 1932 & year_of_birth_clean <= 1970 ~ "Boomers + generación silenciosa",
      TRUE ~ NA_character_
    )
  )


# CREAR TABLA DE DESCRIPCIÓN DE MUESTRA
country_raw <- get_optional_col(df, country_col)
country_clean_from_dataset <- get_optional_col(df, country_clean_col)
region_from_dataset <- get_optional_col(df, residence_region_col)

sample_description <- df %>%
  transmute(
    participant_id,
    
    year_of_birth = year_of_birth_clean,
    age = age_clean,
    age_group,
    
    gender = clean_gender(get_optional_col(., gender_col)),
    
    country = coalesce(
      clean_country(country_clean_from_dataset),
      clean_country(country_raw)
    ),
    
    residence_region = clean_residence_region(country),
    
    city_size = clean_city_size(get_optional_col(., size_city_col)),
    climate_zone = clean_climate_zone(get_optional_col(., climate_zone_col)),
    employment = clean_employment(get_optional_col(., employment_col)),
    education_group = clean_education(get_optional_col(., education_level_col)),
    work_home = clean_work_home(get_optional_col(., work_home_col)),
    type_house = clean_type_house(get_optional_col(., type_house_col)),
    tenure = clean_tenure(get_optional_col(., tenure_col)),
    political_orientation = clean_political_orientation(get_optional_col(., political_col)),
    vote_type = clean_vote(get_optional_col(., vote_col)),
    
    source_survey = get_optional_col(., "source_survey"),
    identifier_type = get_optional_col(., "identifier_type"),
    row_quality = get_optional_col(., "row_quality")
  ) %>%
  distinct(participant_id, .keep_all = TRUE)

write_csv(
  sample_description,
  file.path(csv_dir, "sample_description.csv")
)

cat("Participantes únicos en sample_description:", nrow(sample_description), "\n")


# ÓRDENES NATURALES
natural_orders <- list(
  age_group = c(
    "Generación Z",
    "Millennials",
    "Generación X",
    "Boomers + generación silenciosa"
  ),
  gender = c(
    "Mujer",
    "Hombre",
    "Otro / Prefiere no decirlo"
  ),
  residence_region = c(
    "Europa del Norte",
    "Europa Occidental",
    "Europa del Sur",
    "Europa del Este",
    "Otra región"
  ),
  city_size = c(
    "Aldea o núcleo rural (<1.000 hab.)",
    "Municipio pequeño (1.000-10.000 hab.)",
    "Municipio mediano (10.000-50.000 hab.)",
    "Ciudad pequeña (50.000-250.000 hab.)",
    "Ciudad mediana (250.000-500.000 hab.)",
    "Ciudad grande (500.000-1.000.000 hab.)",
    "Metrópolis (>1.000.000 hab.)"
  ),
  climate_zone = c(
    "Clima cálido/mediterráneo",
    "Clima templado",
    "Clima frío/polar"
  ),
  employment = c(
    "Empleado/a",
    "Estudiante",
    "Otra situación laboral"
  ),
  education_group = c(
    "No universitaria",
    "Universitaria"
  ),
  work_home = c(
    "Sí",
    "No"
  ),
  type_house = c(
    "Vive solo/a",
    "Pareja sin hijos",
    "Hogar con hijos",
    "Vivienda compartida",
    "Otro tipo de hogar"
  ),
  tenure = c(
    "Propiedad sin hipoteca",
    "Propiedad con hipoteca",
    "Alquiler",
    "Otro régimen"
  ),
  political_orientation = c(
    "Extrema izquierda",
    "Izquierda",
    "Centro",
    "Derecha",
    "Extrema derecha"
  ),
  vote_type = c(
    "Partidos regionalistas/independentistas",
    "Voto variable según candidato/programa",
    "Partidos nacionales",
    "Otras opciones",
    "No vota / blanco / nulo",
    "Otra respuesta"
  ),
  identifier_type = c(
    "prolific",
    "codigo",
    "prolific_y_codigo",
    "sin_identificador"
  ),
  source_survey = c(
    "decision",
    "concerns1",
    "rvs"
  )
)

# PALETAS
sample_colors <- list(
  age_group = c(
    "Generación Z" = "#56B4E9",
    "Millennials" = "#009E73",
    "Generación X" = "#E69F00",
    "Boomers + generación silenciosa" = "#CC79A7"
  ),
  gender = c(
    "Mujer" = "#009E73",
    "Hombre" = "#0072B2",
    "Otro / Prefiere no decirlo" = "#999999"
  ),
  residence_region = c(
    "Europa del Norte" = "#56B4E9",
    "Europa Occidental" = "#009E73",
    "Europa del Sur" = "#E69F00",
    "Europa del Este" = "#D55E00",
    "Otra región" = "#999999"
  )
)

default_sample_colors <- c(
  "#0072B2", "#56B4E9", "#009E73", "#E69F00",
  "#D55E00", "#CC79A7", "#F0E442", "#999999"
)

get_sample_colors <- function(variable_name, categories) {
  categories <- as.character(categories)
  
  if (variable_name %in% names(sample_colors)) {
    colors <- sample_colors[[variable_name]]
    colors <- colors[names(colors) %in% categories]
    
    missing_categories <- setdiff(categories, names(colors))
    
    if (length(missing_categories) > 0) {
      extra_colors <- rep(default_sample_colors, length.out = length(missing_categories))
      names(extra_colors) <- missing_categories
      colors <- c(colors, extra_colors)
    }
    
    return(colors)
  }
  
  colors <- rep(default_sample_colors, length.out = length(categories))
  names(colors) <- categories
  colors
}

# CONFIGURACIÓN VISUAL PARA GRÁFICOS DEL TFM
plot_base_size <- 16
plot_title_size <- 19
plot_subtitle_size <- 13
plot_axis_title_size <- 15
plot_axis_text_x_size <- 15
plot_axis_text_y_size <- 15
plot_label_size <- 4.5

theme_sample_tfm <- function() {
  theme_minimal(base_size = plot_base_size) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = plot_title_size
      ),
      plot.subtitle = element_text(
        size = plot_subtitle_size
      ),
      axis.title.x = element_text(
        size = plot_axis_title_size,
        margin = margin(t = 8)
      ),
      axis.title.y = element_text(
        size = plot_axis_title_size,
        margin = margin(r = 8)
      ),
      axis.text.x = element_text(
        size = plot_axis_text_x_size
      ),
      axis.text.y = element_text(
        size = plot_axis_text_y_size
      ),
      panel.grid.minor = element_blank(),
      plot.margin = margin(12, 80, 12, 12)
    )
}

# FUNCIONES DE TABLAS Y GRÁFICOS
get_variable_order <- function(variable_name, data_summary) {
  if (variable_name %in% names(natural_orders)) {
    return(natural_orders[[variable_name]])
  }
  
  data_summary %>%
    arrange(desc(n)) %>%
    pull(category) %>%
    as.character()
}

create_summary_table <- function(data, variable_name, variable_label) {
  data %>%
    filter(!is.na(.data[[variable_name]])) %>%
    count(category = .data[[variable_name]], name = "n") %>%
    mutate(
      variable = variable_label,
      percentage = round(n / sum(n) * 100, 1)
    ) %>%
    select(variable, category, n, percentage)
}

plot_sample_variable <- function(data, variable_name, variable_label, width = 9, height = 5) {
  summary_data <- create_summary_table(data, variable_name, variable_label)
  
  if (nrow(summary_data) == 0) {
    message("Sin datos para variable: ", variable_name)
    return(NULL)
  }
  
  n_valid <- sum(summary_data$n)
  n_total <- nrow(data)
  n_missing <- n_total - n_valid
  
  variable_order <- get_variable_order(variable_name, summary_data)
  variable_order <- variable_order[variable_order %in% summary_data$category]
  
  colors <- get_sample_colors(variable_name, summary_data$category)
  
  plot_data <- summary_data %>%
    mutate(
      category = factor(category, levels = rev(variable_order)),
      label = paste0(n, " (", percentage, "%)")
    )
  
  p <- ggplot(plot_data, aes(x = category, y = n, fill = category)) +
    geom_col(color = "#2C3E50") +
    geom_text(
      aes(label = label),
      hjust = -0.1,
      size = plot_label_size
    ) +
    coord_flip(clip = "off") +
    scale_x_discrete(
      labels = function(x) str_wrap(x, width = 32)
    ) +
    scale_fill_manual(values = colors, drop = FALSE) +
    guides(fill = "none") +
    scale_y_continuous(
      limits = c(0, max(plot_data$n) * 1.30)
    ) +
    labs(
      title = paste("Distribución de la muestra por", str_to_lower(variable_label)),
      subtitle = paste0(
        "n válido = ", n_valid,
        "; n missing = ", n_missing,
        "; n total = ", n_total
      ),
      x = NULL,
      y = "Número de participantes"
    ) +
    theme_sample_tfm()
  
  print(p)
  
  filename <- paste0("sample_description_", clean_filename(variable_name))
  
  ggsave(
    file.path(plots_dir, paste0(filename, ".png")),
    p,
    width = width,
    height = height,
    dpi = 300
  )
  
  ggsave(
    file.path(pdf_dir, paste0(filename, ".pdf")),
    p,
    width = width,
    height = height
  )
  
  write_csv(
    summary_data,
    file.path(csv_dir, paste0(filename, ".csv"))
  )
  
  return(p)
}

plot_sample_variable_percentage <- function(data, variable_name, variable_label, width = 9, height = 5) {
  summary_data <- create_summary_table(data, variable_name, variable_label)
  
  if (nrow(summary_data) == 0) {
    message("Sin datos para variable: ", variable_name)
    return(NULL)
  }
  
  n_valid <- sum(summary_data$n)
  n_total <- nrow(data)
  n_missing <- n_total - n_valid
  
  variable_order <- get_variable_order(variable_name, summary_data)
  variable_order <- variable_order[variable_order %in% summary_data$category]
  
  colors <- get_sample_colors(variable_name, summary_data$category)
  
  plot_data <- summary_data %>%
    mutate(
      category = factor(category, levels = rev(variable_order)),
      label = paste0(percentage, "% (n=", n, ")")
    )
  
  p <- ggplot(plot_data, aes(x = category, y = percentage, fill = category)) +
    geom_col(color = "#2C3E50") +
    geom_text(
      aes(label = label),
      hjust = -0.1,
      size = plot_label_size
    ) +
    coord_flip(clip = "off") +
    scale_x_discrete(
      labels = function(x) str_wrap(x, width = 32)
    ) +
    scale_fill_manual(values = colors, drop = FALSE) +
    guides(fill = "none") +
    scale_y_continuous(
      limits = c(0, max(plot_data$percentage) * 1.30),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = paste("Distribución porcentual de la muestra por", str_to_lower(variable_label)),
      subtitle = paste0(
        "n válido = ", n_valid,
        "; n missing = ", n_missing,
        "; n total = ", n_total
      ),
      x = NULL,
      y = "Porcentaje de participantes"
    ) +
    theme_sample_tfm()
  
  print(p)
  
  filename <- paste0("sample_description_", clean_filename(variable_name), "_percentage")
  
  ggsave(
    file.path(plots_dir, paste0(filename, ".png")),
    p,
    width = width,
    height = height,
    dpi = 300
  )
  
  ggsave(
    file.path(pdf_dir, paste0(filename, ".pdf")),
    p,
    width = width,
    height = height
  )
  
  return(p)
}

plot_sample_variable_ordered <- function(data, variable_name, variable_label, width = 9, height = 5) {
  summary_data <- create_summary_table(data, variable_name, variable_label)
  
  if (nrow(summary_data) == 0) {
    message("Sin datos para variable: ", variable_name)
    return(NULL)
  }
  
  n_valid <- sum(summary_data$n)
  n_total <- nrow(data)
  n_missing <- n_total - n_valid
  
  colors <- get_sample_colors(variable_name, summary_data$category)
  
  plot_data <- summary_data %>%
    arrange(n) %>%
    mutate(
      category = factor(category, levels = category),
      label = paste0(n, " (", percentage, "%)")
    )
  
  p <- ggplot(plot_data, aes(x = category, y = n, fill = category)) +
    geom_col(color = "#2C3E50") +
    geom_text(
      aes(label = label),
      hjust = -0.1,
      size = plot_label_size
    ) +
    coord_flip(clip = "off") +
    scale_x_discrete(
      labels = function(x) str_wrap(x, width = 32)
    ) +
    scale_fill_manual(values = colors, drop = FALSE) +
    guides(fill = "none") +
    scale_y_continuous(
      limits = c(0, max(plot_data$n) * 1.30)
    ) +
    labs(
      title = paste("Distribución de la muestra por", str_to_lower(variable_label), "- ordenada"),
      subtitle = paste0(
        "n válido = ", n_valid,
        "; n missing = ", n_missing,
        "; n total = ", n_total
      ),
      x = NULL,
      y = "Número de participantes"
    ) +
    theme_sample_tfm()
  
  print(p)
  
  filename <- paste0("sample_description_", clean_filename(variable_name), "_ordered")
  
  ggsave(
    file.path(plots_dir, paste0(filename, ".png")),
    p,
    width = width,
    height = height,
    dpi = 300
  )
  
  ggsave(
    file.path(pdf_dir, paste0(filename, ".pdf")),
    p,
    width = width,
    height = height
  )
  
  return(p)
}

# VARIABLES A DESCRIBIR
sample_variables <- tibble(
  variable_name = c(
    "age_group",
    "gender",
    "country",
    "residence_region",
    "city_size",
    "climate_zone",
    "employment",
    "education_group",
    "work_home",
    "type_house",
    "tenure",
    "political_orientation",
    "vote_type",
    "source_survey",
    "identifier_type"
  ),
  variable_label = c(
    "generación",
    "género",
    "país",
    "región europea",
    "tamaño de ciudad",
    "zona climática",
    "situación laboral",
    "nivel educativo",
    "trabajo/estudio desde casa",
    "tipo de hogar",
    "régimen de tenencia",
    "orientación política",
    "tipo de voto",
    "encuesta de origen",
    "tipo de identificador"
  ),
  width = c(
    9, 8, 11, 9, 9,
    9, 9, 8, 8, 9,
    9, 9, 11, 8, 8
  ),
  height = c(
    5.5, 5.5, 7.5, 5.5, 5.5,
    5.5, 5.5, 5.5, 5.5, 5.5,
    5.5, 5.5, 6, 5.5, 5.5
  )
)

write_csv(
  sample_variables,
  file.path(csv_dir, "sample_description_variables_used.csv")
)

# TABLAS RESUMEN
summary_sample_all <- sample_variables %>%
  rowwise() %>%
  do(
    create_summary_table(
      data = sample_description,
      variable_name = .$variable_name,
      variable_label = .$variable_label
    )
  ) %>%
  ungroup()

write_csv(
  summary_sample_all,
  file.path(csv_dir, "summary_sample_description_all_variables.csv")
)

print(summary_sample_all, n = Inf)

# GRÁFICOS CATEGÓRICOS
sample_plots <- list()

for (i in seq_len(nrow(sample_variables))) {
  
  variable_name_i <- sample_variables$variable_name[i]
  variable_label_i <- sample_variables$variable_label[i]
  width_i <- sample_variables$width[i]
  height_i <- sample_variables$height[i]
  
  sample_plots[[paste0(variable_name_i, "_n")]] <- plot_sample_variable(
    sample_description,
    variable_name_i,
    variable_label_i,
    width_i,
    height_i
  )
  
  sample_plots[[paste0(variable_name_i, "_percentage")]] <- plot_sample_variable_percentage(
    sample_description,
    variable_name_i,
    variable_label_i,
    width_i,
    height_i
  )
  
  sample_plots[[paste0(variable_name_i, "_ordered")]] <- plot_sample_variable_ordered(
    sample_description,
    variable_name_i,
    variable_label_i,
    width_i,
    height_i
  )
}

sample_plots <- sample_plots[!sapply(sample_plots, is.null)]

# EDAD NUMÉRICA
age_summary <- sample_description %>%
  summarise(
    n_valid = sum(!is.na(age)),
    mean_age = round(mean(age, na.rm = TRUE), 2),
    median_age = round(median(age, na.rm = TRUE), 2),
    sd_age = round(sd(age, na.rm = TRUE), 2),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE)
  )

write_csv(
  age_summary,
  file.path(csv_dir, "sample_age_numeric_summary.csv")
)

print(age_summary)

if (sum(!is.na(sample_description$age)) > 0) {
  
  plot_age_numeric <- sample_description %>%
    filter(!is.na(age)) %>%
    ggplot(aes(x = age)) +
    geom_histogram(
      bins = 15,
      fill = "#56B4E9",
      color = "#2C3E50"
    ) +
    labs(
      title = "Distribución de edad de la muestra",
      subtitle = paste0(
        "n válido = ", sum(!is.na(sample_description$age)),
        "; n total = ", nrow(sample_description)
      ),
      x = "Edad",
      y = "Número de participantes"
    ) +
    theme_sample_tfm()
  
  print(plot_age_numeric)
  
  ggsave(
    file.path(plots_dir, "sample_description_age_numeric.png"),
    plot_age_numeric,
    width = 9,
    height = 5.5,
    dpi = 300
  )
  
  ggsave(
    file.path(pdf_dir, "sample_description_age_numeric.pdf"),
    plot_age_numeric,
    width = 9,
    height = 5.5
  )
  
  sample_plots[["age_numeric"]] <- plot_age_numeric
}

# MISSING POR VARIABLE
missing_sample_description <- sample_description %>%
  summarise(across(-participant_id, ~ sum(is.na(.x)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "n_missing"
  ) %>%
  mutate(
    n_total = nrow(sample_description),
    n_valid = n_total - n_missing,
    percentage_missing = round(n_missing / n_total * 100, 1)
  ) %>%
  arrange(desc(n_missing))

write_csv(
  missing_sample_description,
  file.path(csv_dir, "sample_description_missing_by_variable.csv")
)

print(missing_sample_description, n = Inf)

# PDF FINAL CON TODOS LOS GRÁFICOS
pdf(
  file = file.path(pdf_dir, "sample_description_all_plots.pdf"),
  width = 12,
  height = 7,
  onefile = TRUE
)

for (p in sample_plots) {
  print(p)
}

dev.off()

# COMPROBACIONES FINALES
cat("\nDescripción de muestra generada en:\n")
cat(base_output_dir, "\n")

cat("\nFilas en sample_description:\n")
print(nrow(sample_description))

cat("\nMissing por variable:\n")
print(missing_sample_description, n = Inf)

cat("\nArchivos principales:\n")
cat("- sample_description.csv\n")
cat("- summary_sample_description_all_variables.csv\n")
cat("- sample_age_numeric_summary.csv\n")
cat("- sample_description_missing_by_variable.csv\n")
cat("- sample_description_all_plots.pdf\n")


# ==============================================================================
# TABLA SOCIODEMOGRÁFICA TIPO PAPER POR ARQUETIPO AUTOCLASIFICADO
# ==============================================================================

# Esta tabla resume la muestra total y la composición sociodemográfica
# por los 8 arquetipos de autoclasificación.
#
# Salidas:
# - CSV largo
# - CSV ancho, listo para revisar
# - DOCX tipo paper, si tienes instalados flextable y officer

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)

# ------------------------------------------------------------------------------
# 1. Leer dataset general para recuperar autoclasificación
# ------------------------------------------------------------------------------

df_general_self <- read_csv(
  file.path(base_input_dir, "df_clean_general.csv"),
  show_col_types = FALSE
)

add_participant_id_safe <- function(data) {
  data %>%
    mutate(
      participant_id = coalesce(
        if ("participant_id" %in% names(.)) as.character(.data[["participant_id"]]) else NA_character_,
        if ("join_key" %in% names(.)) as.character(.data[["join_key"]]) else NA_character_,
        if ("prolific_id" %in% names(.)) as.character(.data[["prolific_id"]]) else NA_character_,
        if ("identification_code" %in% names(.)) as.character(.data[["identification_code"]]) else NA_character_,
        as.character(row_number())
      )
    )
}

df_general_self <- add_participant_id_safe(df_general_self)

self_col_candidates <- c(
  "which_statement_best_describes_you_when_making_an_investment_decision_related_to_your_household_final",
  "which_statement_best_describes_you_when_making_an_investment_decision_related_to_your_household"
)

self_col <- intersect(self_col_candidates, names(df_general_self))[1]

if (is.na(self_col)) {
  stop("No se encuentra la columna de autoclasificación en df_clean_general.csv")
}

self_profiles <- df_general_self %>%
  transmute(
    participant_id = as.character(participant_id),
    self_response_raw = str_squish(as.character(.data[[self_col]])),
    self_response_raw = na_if(self_response_raw, ""),
    self_profile = case_when(
      is.na(self_response_raw) ~ "Faltante",
      str_detect(self_response_raw, regex("environmental impact", ignore_case = TRUE)) ~ "Activista",
      str_detect(self_response_raw, regex("safety", ignore_case = TRUE)) ~ "Temeroso/a",
      str_detect(self_response_raw, regex("social status", ignore_case = TRUE)) ~ "Influyente",
      str_detect(self_response_raw, regex("comfort", ignore_case = TRUE)) ~ "Cuidadoso/a",
      str_detect(self_response_raw, regex("not very interested", ignore_case = TRUE)) ~ "Desinteresado/a",
      str_detect(self_response_raw, regex("early adopter", ignore_case = TRUE)) ~ "Pionero",
      str_detect(self_response_raw, regex("ethical", ignore_case = TRUE)) ~ "Sensible",
      str_detect(self_response_raw, regex("cost-effective", ignore_case = TRUE)) ~ "Homo economicus",
      str_detect(self_response_raw, regex("[NΝ]one of the above", ignore_case = TRUE)) ~ "Sin clasificar",
      TRUE ~ "Otro"
    )
  ) %>%
  filter(!self_profile %in% c("Faltante", "Sin clasificar", "Otro")) %>%
  distinct(participant_id, .keep_all = TRUE)

# ------------------------------------------------------------------------------
# 2. Unir autoclasificación con sample_description
# ------------------------------------------------------------------------------

archetype_order <- c(
  "Cuidadoso/a",
  "Activista",
  "Temeroso/a",
  "Homo economicus",
  "Sensible",
  "Influyente",
  "Desinteresado/a",
  "Pionero"
)

sample_description_archetype <- sample_description %>%
  mutate(participant_id = as.character(participant_id)) %>%
  left_join(self_profiles, by = "participant_id") %>%
  mutate(
    self_profile = factor(self_profile, levels = archetype_order),
    
    # Variable compacta de país para que la tabla no sea enorme
    country_group = case_when(
      is.na(country) ~ NA_character_,
      country == "Spain" ~ "España",
      TRUE ~ "Otro país europeo"
    ),
    
    # Agrupación climática en 3 grupos
    climate_zone_3 = case_when(
      is.na(climate_zone) ~ NA_character_,
      climate_zone %in% c("Clima mediterráneo") ~ "Clima cálido/mediterráneo",
      climate_zone %in% c("Clima oceánico", "Clima continental", "Clima de transición") ~ "Clima templado",
      climate_zone %in% c("Clima frío") ~ "Clima frío/polar",
      TRUE ~ "Otro clima"
    ),
    
    # Situación laboral en 3 grupos
    employment_3 = case_when(
      is.na(employment) ~ NA_character_,
      employment == "Empleado/a" ~ "Empleado/a",
      employment == "Estudiante" ~ "Estudiante",
      TRUE ~ "Otra situación laboral"
    )
  )

write_csv(
  sample_description_archetype,
  file.path(csv_dir, "sample_description_with_self_profile.csv")
)

cat("Tabla base con arquetipos creada.\n")
cat("Filas:", nrow(sample_description_archetype), "\n")
cat("Con arquetipo válido:", sum(!is.na(sample_description_archetype$self_profile)), "\n")

# ------------------------------------------------------------------------------
# 3. Funciones para tabla tipo paper
# ------------------------------------------------------------------------------

format_n_pct <- function(n, denom) {
  n <- ifelse(is.na(n), 0, n)
  
  if (is.na(denom) || denom == 0) {
    return("")
  }
  
  paste0(n, " (", sprintf("%.1f", 100 * n / denom), "%)")
}

format_mean_sd <- function(mean_value, sd_value, n_valid) {
  if (is.na(n_valid) || n_valid == 0 || is.na(mean_value)) {
    return("")
  }
  
  paste0(sprintf("%.1f", mean_value), " (", sprintf("%.1f", sd_value), ")")
}

profile_columns <- c("Total", archetype_order)

# Duplicamos los datos:
# - una copia para Total
# - una copia para cada arquetipo
table_data_long <- bind_rows(
  sample_description_archetype %>%
    mutate(profile_group = "Total"),
  
  sample_description_archetype %>%
    filter(!is.na(self_profile)) %>%
    mutate(profile_group = as.character(self_profile))
) %>%
  mutate(
    profile_group = factor(profile_group, levels = profile_columns)
  )

# ------------------------------------------------------------------------------
# 4. Fila de N por columna
# ------------------------------------------------------------------------------

make_n_row <- function(data) {
  data %>%
    distinct(profile_group, participant_id) %>%
    count(profile_group, name = "n") %>%
    complete(profile_group = profile_columns, fill = list(n = 0)) %>%
    mutate(
      row_id = 1,
      row_type = "data",
      characteristic = "N participantes",
      value = as.character(n)
    ) %>%
    select(row_id, row_type, characteristic, profile_group, value)
}

# ------------------------------------------------------------------------------
# 5. Fila edad: media y desviación típica
# ------------------------------------------------------------------------------

make_age_row <- function(data) {
  data %>%
    group_by(profile_group) %>%
    summarise(
      n_valid = sum(!is.na(age)),
      mean_age = mean(age, na.rm = TRUE),
      sd_age = sd(age, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    complete(profile_group = profile_columns) %>%
    mutate(
      row_id = 2,
      row_type = "data",
      characteristic = "Edad, años (media (DE))",
      value = mapply(format_mean_sd, mean_age, sd_age, n_valid)
    ) %>%
    select(row_id, row_type, characteristic, profile_group, value)
}

# ------------------------------------------------------------------------------
# 6. Bloques categóricos
# ------------------------------------------------------------------------------

make_categorical_block <- function(data, var_name, label, levels_vec, start_row_id) {
  
  denom <- data %>%
    filter(!is.na(.data[[var_name]])) %>%
    count(profile_group, name = "denom")
  
  counts <- data %>%
    filter(!is.na(.data[[var_name]])) %>%
    mutate(category = as.character(.data[[var_name]])) %>%
    count(profile_group, category, name = "n")
  
  header <- tibble(
    row_id = start_row_id,
    row_type = "header",
    characteristic = label,
    profile_group = factor(profile_columns, levels = profile_columns),
    value = ""
  )
  
  rows <- expand_grid(
    profile_group = factor(profile_columns, levels = profile_columns),
    category = levels_vec
  ) %>%
    left_join(counts, by = c("profile_group", "category")) %>%
    left_join(denom, by = "profile_group") %>%
    mutate(
      n = ifelse(is.na(n), 0, n),
      row_id = start_row_id + row_number(),
      row_type = "data",
      characteristic = paste0("  ", category),
      value = mapply(format_n_pct, n, denom)
    ) %>%
    select(row_id, row_type, characteristic, profile_group, value)
  
  bind_rows(header, rows)
}

# ------------------------------------------------------------------------------
# 7. Variables que van a la tabla
# ------------------------------------------------------------------------------

table_blocks <- list(
  list(
    var = "gender",
    label = "Género",
    levels = c("Mujer", "Hombre", "Otro / Prefiere no decirlo")
  ),
  list(
    var = "country_group",
    label = "País de residencia",
    levels = c("España", "Otro país europeo")
  ),
  list(
    var = "residence_region",
    label = "Región europea",
    levels = c("Europa del Norte", "Europa Occidental", "Europa del Sur", "Europa del Este", "Otra región")
  ),
  list(
    var = "education_group",
    label = "Nivel educativo",
    levels = c("No universitaria", "Universitaria")
  ),
  list(
    var = "employment_3",
    label = "Situación laboral",
    levels = c("Empleado/a", "Estudiante", "Otra situación laboral")
  ),
  list(
    var = "work_home",
    label = "Trabajo/estudio desde casa",
    levels = c("Sí", "No")
  ),
  list(
    var = "type_house",
    label = "Tipo de hogar",
    levels = c("Vive solo/a", "Pareja sin hijos", "Hogar con hijos", "Vivienda compartida", "Otro tipo de hogar")
  ),
  list(
    var = "tenure",
    label = "Régimen de tenencia",
    levels = c("Propiedad sin hipoteca", "Propiedad con hipoteca", "Alquiler", "Otro régimen")
  ),
  list(
    var = "climate_zone_3",
    label = "Zona climática",
    levels = c("Clima cálido/mediterráneo", "Clima templado", "Clima frío/polar", "Otro clima")
  ),
  list(
    var = "political_orientation",
    label = "Orientación política",
    levels = c("Extrema izquierda", "Izquierda", "Centro", "Derecha", "Extrema derecha")
  )
)

# ------------------------------------------------------------------------------
# 8. Construir tabla larga
# ------------------------------------------------------------------------------

table_long_parts <- list(
  make_n_row(table_data_long),
  make_age_row(table_data_long)
)

current_row_id <- 10

for (block in table_blocks) {
  table_long_parts[[length(table_long_parts) + 1]] <- make_categorical_block(
    data = table_data_long,
    var_name = block$var,
    label = block$label,
    levels_vec = block$levels,
    start_row_id = current_row_id
  )
  
  current_row_id <- current_row_id + length(block$levels) + 10
}

sociodemographic_by_archetype_long <- bind_rows(table_long_parts) %>%
  mutate(profile_group = as.character(profile_group)) %>%
  arrange(row_id, profile_group)

# ------------------------------------------------------------------------------
# 9. Pasar a formato ancho tipo paper
# ------------------------------------------------------------------------------

sociodemographic_by_archetype_wide <- sociodemographic_by_archetype_long %>%
  select(row_id, row_type, characteristic, profile_group, value) %>%
  pivot_wider(
    names_from = profile_group,
    values_from = value
  ) %>%
  arrange(row_id) %>%
  select(row_id, row_type, characteristic, all_of(profile_columns))

# Guardar versión completa con columnas auxiliares
write_csv(
  sociodemographic_by_archetype_long,
  file.path(csv_dir, "sociodemographic_by_archetype_long.csv")
)

write_csv(
  sociodemographic_by_archetype_wide,
  file.path(csv_dir, "sociodemographic_by_archetype_wide.csv")
)

# Guardar versión limpia para copiar en Word
sociodemographic_by_archetype_for_word <- sociodemographic_by_archetype_wide %>%
  select(-row_id, -row_type)

write_csv(
  sociodemographic_by_archetype_for_word,
  file.path(csv_dir, "sociodemographic_by_archetype_for_word.csv")
)

cat("Tabla por arquetipo exportada en CSV.\n")
cat("Archivo principal:\n")
cat(file.path(csv_dir, "sociodemographic_by_archetype_for_word.csv"), "\n")

# ------------------------------------------------------------------------------
# 10. Exportar a XLSX si openxlsx está instalado
# ------------------------------------------------------------------------------

if (requireNamespace("openxlsx", quietly = TRUE)) {
  xlsx_path <- file.path(csv_dir, "sociodemographic_by_archetype_for_word.xlsx")
  
  openxlsx::write.xlsx(
    sociodemographic_by_archetype_for_word,
    file = xlsx_path,
    overwrite = TRUE
  )
  
  cat("Tabla exportada también en XLSX:\n")
  cat(xlsx_path, "\n")
} else {
  cat("Paquete openxlsx no instalado. Se omite exportación XLSX.\n")
}

# ------------------------------------------------------------------------------
# 11. Exportar a DOCX tipo paper si flextable y officer están instalados
# ------------------------------------------------------------------------------

if (
  requireNamespace("flextable", quietly = TRUE) &&
  requireNamespace("officer", quietly = TRUE)
) {
  
  table_for_docx <- sociodemographic_by_archetype_wide %>%
    select(-row_id)
  
  header_rows <- which(table_for_docx$row_type == "header")
  
  table_for_docx_clean <- table_for_docx %>%
    select(-row_type)
  
  ft <- flextable::flextable(table_for_docx_clean)
  
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::fontsize(ft, size = 7, part = "all")
  ft <- flextable::fontsize(ft, size = 8, part = "header")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::bold(ft, i = header_rows, bold = TRUE, part = "body")
  ft <- flextable::align(ft, j = 2:ncol(table_for_docx_clean), align = "center", part = "all")
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  ft <- flextable::width(ft, j = 1, width = 2.4)
  ft <- flextable::width(ft, j = 2:ncol(table_for_docx_clean), width = 0.9)
  
  ft <- flextable::add_footer_lines(
    ft,
    values = "Nota. Los porcentajes se calculan dentro de cada columna excluyendo valores faltantes. Los perfiles con tamaños muy reducidos, especialmente Pionero, deben interpretarse con cautela."
  )
  
  section_landscape <- officer::prop_section(
    page_size = officer::page_size(orient = "landscape"),
    page_margins = officer::page_mar(
      bottom = 0.5,
      top = 0.5,
      right = 0.5,
      left = 0.5
    )
  )
  
  docx_path <- file.path(csv_dir, "sociodemographic_by_archetype_for_word.docx")
  
  flextable::save_as_docx(
    "Tabla. Características sociodemográficas de la muestra total y por arquetipo autoclasificado" = ft,
    path = docx_path,
    pr_section = section_landscape
  )
  
  cat("Tabla exportada también en DOCX:\n")
  cat(docx_path, "\n")
  
} else {
  cat("Paquetes flextable/officer no instalados. Se omite exportación DOCX.\n")
}

# ==============================================================================
# PIRÁMIDE DE POBLACIÓN POR GÉNERO Y GENERACIÓN
# Eje Y = generaciones, para que cada barra tenga un solo color
# ==============================================================================

generation_levels_pyramid <- c(
  "Boomers + generación silenciosa\n(56+)",
  "Generación X\n(41-55)",
  "Millennials\n(26-40)",
  "Generación Z\n(19-25)",
  
)

generation_colors_pyramid <- c(
  "Generación Z\n(19-25)" = "#8EC1B8",
  "Millennials\n(26-40)" = "#E8E6A0",
  "Generación X\n(41-55)" = "#B4B0D0",
  "Boomers + generación silenciosa\n(56+)" = "#E58373"
)

pyramid_data <- sample_description %>%
  filter(
    !is.na(age),
    !is.na(gender),
    gender %in% c("Hombre", "Mujer")
  ) %>%
  mutate(
    generation_pyramid = case_when(
      age >= 19 & age <= 25 ~ "Generación Z\n(19-25)",
      age >= 26 & age <= 40 ~ "Millennials\n(26-40)",
      age >= 41 & age <= 55 ~ "Generación X\n(41-55)",
      age >= 56             ~ "Boomers + generación silenciosa\n(56+)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(generation_pyramid)) %>%
  count(generation_pyramid, gender, name = "n") %>%
  complete(
    generation_pyramid = generation_levels_pyramid,
    gender = c("Hombre", "Mujer"),
    fill = list(n = 0)
  ) %>%
  mutate(
    generation_pyramid = factor(
      generation_pyramid,
      levels = generation_levels_pyramid
    ),
    total_valid = sum(n),
    percentage = round(100 * n / total_valid, 1),
    percentage_plot = if_else(gender == "Hombre", -percentage, percentage)
  )

max_axis <- ceiling(max(abs(pyramid_data$percentage_plot), na.rm = TRUE) / 5) * 5

plot_population_pyramid <- ggplot(
  pyramid_data,
  aes(
    x = percentage_plot,
    y = generation_pyramid,
    fill = generation_pyramid
  )
) +
  geom_col(width = 0.75, color = "white", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "#333333", linewidth = 0.7) +
  geom_text(
    data = pyramid_data %>% filter(n > 0),
    aes(
      label = paste0(percentage, "%"),
      x = if_else(
        gender == "Hombre",
        percentage_plot - max_axis * 0.04,
        percentage_plot + max_axis * 0.04
      )
    ),
    size = 4.2,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = -max_axis * 0.55,
    y = length(generation_levels_pyramid) + 0.45,
    label = "Hombres",
    size = 5.5,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = max_axis * 0.55,
    y = length(generation_levels_pyramid) + 0.45,
    label = "Mujeres",
    size = 5.5,
    fontface = "bold"
  ) +
  scale_x_continuous(
    limits = c(-max_axis * 1.15, max_axis * 1.15),
    breaks = seq(-max_axis, max_axis, by = 5),
    labels = function(x) paste0(abs(x), "%")
  ) +
  scale_y_discrete(
    limits = rev(generation_levels_pyramid)
  ) +
  scale_fill_manual(
    values = generation_colors_pyramid,
    name = "Generación"
  ) +
  labs(
    title = "Pirámide de edad y género de la muestra",
    subtitle = "Porcentaje sobre participantes con edad y género válidos",
    x = "Porcentaje de la muestra",
    y = "Grupo generacional"
  ) +
  theme_sample_tfm() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 14),
    plot.margin = margin(12, 25, 12, 25)
  )

print(plot_population_pyramid)

ggsave(
  file.path(plots_dir, "sample_description_population_pyramid_generation.png"),
  plot_population_pyramid,
  width = 11,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(pdf_dir, "sample_description_population_pyramid_generation.pdf"),
  plot_population_pyramid,
  width = 11,
  height = 6
)

# Sobrescribe también el nombre antiguo para no confundirte al abrir archivos
ggsave(
  file.path(plots_dir, "population_pyramid_age_gender_generation.png"),
  plot_population_pyramid,
  width = 11,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(pdf_dir, "population_pyramid_age_gender_generation.pdf"),
  plot_population_pyramid,
  width = 11,
  height = 6
)

write_csv(
  pyramid_data %>%
    select(generation_pyramid, gender, n, percentage),
  file.path(csv_dir, "sample_description_population_pyramid_generation.csv")
)

sample_plots[["population_pyramid_generation"]] <- plot_population_pyramid