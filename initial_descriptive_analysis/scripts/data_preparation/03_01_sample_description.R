
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
    str_detect(x, regex("Subtropical", ignore_case = TRUE)) ~ "Clima mediterráneo/subtropical",
    str_detect(x, regex("Temperate Climates - Maritime", ignore_case = TRUE)) ~ "Clima oceánico",
    str_detect(x, regex("Temperate Climates - Transitional", ignore_case = TRUE)) ~ "Clima de transición",
    str_detect(x, regex("Temperate Climates - Intermediate|Temperate Climates - Continental", ignore_case = TRUE)) ~ "Clima continental templado",
    str_detect(x, regex("Cold Climates", ignore_case = TRUE)) ~ "Clima frío",
    str_detect(x, regex("Circumpolar", ignore_case = TRUE)) ~ "Clima polar/subpolar",
    TRUE ~ "Otro clima"
  )
}

clean_employment <- function(x) {
  x <- clean_text_basic(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("Full time employed|Part-time employed|Self-employed", ignore_case = TRUE)) ~ "Empleado/a",
    str_detect(x, regex("Student", ignore_case = TRUE)) ~ "Estudiante",
    str_detect(x, regex("Unemployed", ignore_case = TRUE)) ~ "Desempleado/a",
    str_detect(x, regex("Retired|Stay-at-home", ignore_case = TRUE)) ~ "Otra situación inactiva",
    TRUE ~ NA_character_
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
    "Clima mediterráneo/subtropical",
    "Clima oceánico",
    "Clima de transición",
    "Clima continental templado",
    "Clima frío",
    "Clima polar/subpolar",
    "Otro clima"
  ),
  employment = c(
    "Empleado/a",
    "Estudiante",
    "Desempleado/a",
    "Otra situación inactiva",
    "Otra situación"
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