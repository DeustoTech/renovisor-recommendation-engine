# ==============================================================================
# SCRIPT 07 - ETAPAS TTM, DIMENSIONES Y VECTOR DE DETERMINANTES
# ==============================================================================

# Descripción de dimensiones:
#
# FINANCIAL:
# Incluye los aspectos económicos de la decisión, como el coste de la inversión,
# el ahorro esperado, el acceso a financiación, los riesgos económicos y los
# posibles beneficios o pérdidas monetarias.
#
# SECURITY:
# Recoge factores relacionados con la seguridad y la fiabilidad de la decisión.
# Incluye la confianza en la tecnología, en las instituciones o empresas implicadas,
# la seguridad personal, la certidumbre legal y la reducción de riesgos.
#
# COMPETENCE:
# Hace referencia a la percepción de capacidad para tomar la decisión.
# Incluye el conocimiento disponible, la comprensión técnica, la adecuación de la
# tecnología al hogar y la sensación de competencia personal para gestionar la inversión.
#
# AUTONOMY:
# Agrupa factores vinculados al control personal, la independencia y la capacidad
# de mantener la decisión en el tiempo. Incluye el compromiso, la persistencia,
# la autosuficiencia y el esfuerzo que implica adoptar la medida.
#
# PHYSIOLOGICAL:
# Se refiere al impacto de la tecnología o renovación sobre el bienestar físico y
# cotidiano del hogar. Incluye la mejora del confort, la salud, la calidad de vida
# y el bienestar de la persona o de su familia.
#
# RELATEDNESS:
# Incluye los factores sociales y relacionales asociados a la decisión. Considera
# cómo la medida afecta a la familia, vecinos, comunidad o entorno social, así como
# el acuerdo con otras personas implicadas.
#
# STIMULATION:
# Representa el grado en que la decisión resulta interesante, novedosa, motivadora
# o atractiva para la persona. Incluye la curiosidad, la innovación, el aprendizaje
# y el componente de disfrute asociado a la tecnología.
#
# POPULARITY:
# Agrupa factores relacionados con la influencia social, las tendencias, el
# reconocimiento externo y la opinión de los demás. Incluye la presión social, la
# aprobación, el prestigio o el deseo de seguir comportamientos populares.
#
# MEANING:
# Recoge los factores vinculados al sentido personal, los valores y el propósito de
# la decisión. Incluye la contribución social, el valor añadido y la percepción de
# que la acción tiene un significado más allá del beneficio económico.


# 0. Librerías

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(purrr)
library(ggplot2)


# Cargar datos
df <- read_csv(
  "initial_descriptive_analysis/output/clean_datasets/df_clean_general.csv",
  show_col_types = FALSE
)

# Carpetas de salida
base_output_dir <- "initial_descriptive_analysis/output/ttm_stage_analysis"

csv_dir <- file.path(base_output_dir, "csv")
plots_dir <- file.path(base_output_dir, "plots")
pdf_dir <- file.path(base_output_dir, "pdf")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)

cat("Filas:", nrow(df), "\n")
cat("Columnas:", ncol(df), "\n")

df <- df %>%
  mutate(
    participant_id = coalesce(
      as.character(join_key),
      as.character(prolific_id),
      as.character(identification_code),
      as.character(row_number())
    )
  )

# Función para guardar gráficos
save_plot_png <- function(plot, filename, width = 10, height = 6) {
  ggsave(
    filename = file.path(plots_dir, paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}

# Diccionario de determinantes
determinant_ids <- c(
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
  "autonomy",
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

determinant_labels <- c(
  "Beneficio económico",
  "Acceso a financiación",
  "Perfil de riesgo",
  "Valor añadido",
  "Frugalidad",
  "Protección climática",
  "Cumplimiento legal",
  "Confianza",
  "Seguridad",
  "Eficiencia de costes",
  "Conocimiento",
  "Competencia propia",
  "Adecuación técnica",
  "Preocupación ambiental",
  "Satisfacción personal",
  "Compromiso",
  "Persistencia",
  "Autosuficiencia",
  "Bienestar",
  "Confort",
  "Derechos y deberes",
  "Presión social",
  "Apoyo social",
  "Sociabilidad",
  "Acuerdo",
  "Novedad",
  "Diversión",
  "Reconocimiento",
  "Tendencias",
  "Autoridad",
  "Aprobación",
  "Significado personal"
)

# Buscar automáticamente la columna real de cada determinante en df
determinant_cols <- map_chr(determinant_ids, function(id) {
  
  matches <- names(df)[str_detect(names(df), paste0("^", id, "_"))]
  
  if (length(matches) == 0) {
    stop(paste("No se ha encontrado columna para el determinante:", id))
  }
  
  if (length(matches) > 1) {
    stop(paste(
      "Hay más de una columna posible para el determinante:",
      id,
      "\nColumnas:",
      paste(matches, collapse = ", ")
    ))
  }
  
  matches
})

determinant_dictionary <- tibble(
  determinant_col = determinant_cols,
  determinant_id = determinant_ids,
  determinant_label = determinant_labels
)

write_csv(
  determinant_dictionary,
  file.path(csv_dir, "determinant_dictionary.csv")
)

print(determinant_dictionary, n = Inf)


# 3. Correspondencia dimensión -> determinantes
dimension_determinant_mapping <- tibble(
  dimension = c(
    "FINANCIAL",
    "SECURITY",
    "COMPETENCE",
    "AUTONOMY",
    "PHYSIOLOGICAL",
    "RELATEDNESS",
    "STIMULATION",
    "POPULARITY",
    "MEANING"
  ),
  
  # FINANCIAL
  profits = c(1, 0, 0, 0, 0, 0, 0, 0, 0),
  credit_score_access_to_funding = c(1, 0, 0, 0, 0, 0, 0, 0, 0),
  risk_profile = c(1, 0, 0, 0, 0, 0, 0, 0, 0),
  added_value = c(1, 0, 0, 0, 0, 0, 0, 0, 0),
  frugality = c(1, 0, 0, 0, 0, 0, 0, 0, 0),
  
  # SECURITY
  legal = c(0, 1, 0, 0, 0, 0, 0, 0, 0),
  trust = c(0, 1, 0, 0, 0, 0, 0, 0, 0),
  safety = c(0, 1, 0, 0, 0, 0, 0, 0, 0),
  climate_protection = c(0, 1, 0, 0, 0, 0, 0, 0, 0),
  
  # COMPETENCE
  cost_efficiency = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
  knowledge = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
  own_competence = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
  technical_fit = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
  environmental_concerns = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
  
  # AUTONOMY
  self_satisfaction = c(0, 0, 0, 1, 0, 0, 0, 0, 0),
  commitment = c(0, 0, 0, 1, 0, 0, 0, 0, 0),
  adherence = c(0, 0, 0, 1, 0, 0, 0, 0, 0),
  autonomy = c(0, 0, 0, 1, 0, 0, 0, 0, 0),
  
  # PHYSIOLOGICAL
  wellbeing = c(0, 0, 0, 0, 1, 0, 0, 0, 0),
  coziness = c(0, 0, 0, 0, 1, 0, 0, 0, 0),
  
  # RELATEDNESS
  rights_and_duties = c(0, 0, 0, 0, 0, 1, 0, 0, 0),
  peer_pressure = c(0, 0, 0, 0, 0, 1, 0, 0, 0),
  support = c(0, 0, 0, 0, 0, 1, 0, 0, 0),
  socialising = c(0, 0, 0, 0, 0, 1, 0, 0, 0),
  agreement = c(0, 0, 0, 0, 0, 1, 0, 0, 0),
  
  # STIMULATION
  novelty = c(0, 0, 0, 0, 0, 0, 1, 0, 0),
  fun = c(0, 0, 0, 0, 0, 0, 1, 0, 0),
  recognition = c(0, 0, 0, 0, 0, 0, 1, 0, 0),
  
  # POPULARITY
  trends = c(0, 0, 0, 0, 0, 0, 0, 1, 0),
  authority = c(0, 0, 0, 0, 0, 0, 0, 1, 0),
  
  # MEANING
  own_significance = c(0, 0, 0, 0, 0, 0, 0, 0, 1),
  approval = c(0, 0, 0, 0, 0, 0, 0, 0, 1)
)

write_csv(
  dimension_determinant_mapping,
  file.path(csv_dir, "dimension_determinant_mapping.csv")
)

# Funciones auxiliares
clean_determinant_score <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  
  case_when(
    is.na(x) ~ NA_real_,
    x >= 0 & x <= 100 ~ x,
    TRUE ~ NA_real_
  )
}

clean_selected_technology <- function(x) {
  x <- str_squish(as.character(x))
  x <- str_replace_all(x, "\u039D", "N")
  x <- str_replace_all(x, "\u00A0", " ")
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    
    str_detect(x, regex("^none\\.?$|none option|prefer not to answer", ignore_case = TRUE)) ~ NA_character_,
    
    str_detect(x, regex("balcony|kit", ignore_case = TRUE)) ~
      "Kits solares de balcón",
    
    str_detect(x, regex("tariff|electricity tariff|time-of-use", ignore_case = TRUE)) ~
      "Cambio de tarifa eléctrica",
    
    str_detect(x, regex("cooling", ignore_case = TRUE)) ~
      "Sistema de refrigeración",
    
    str_detect(x, regex("hot water|domestic hot water|boiler|water heater", ignore_case = TRUE)) ~
      "Agua caliente sanitaria",
    
    str_detect(x, regex("electric vehicle", ignore_case = TRUE)) ~
      "Vehículo eléctrico",
    
    str_detect(x, regex("elevator|lift", ignore_case = TRUE)) ~
      "Ascensor",
    
    str_detect(x, regex("appliance", ignore_case = TRUE)) ~
      "Electrodomésticos eficientes",
    
    str_detect(x, regex("storage", ignore_case = TRUE)) ~
      "Almacenamiento energético",
    
    str_detect(x, regex("envelope|insulation|windows|roof|wall", ignore_case = TRUE)) ~
      "Renovación de envolvente",
    
    str_detect(x, regex("fossil|biomass", ignore_case = TRUE)) ~
      "Calefacción fósil o biomasa",
    
    str_detect(x, regex("heat pump", ignore_case = TRUE)) ~
      "Bomba de calor",
    
    str_detect(x, regex("ventilation|heat recovery", ignore_case = TRUE)) ~
      "Ventilación con recuperador",
    
    str_detect(x, regex("energy community", ignore_case = TRUE)) ~
      "Comunidad energética",
    
    str_detect(x, regex("micro", ignore_case = TRUE)) ~
      "Medidas de microeficiencia",
    
    str_detect(x, regex("photovoltaic|pv|solar pv|rooftop", ignore_case = TRUE)) ~
      "Fotovoltaica en cubierta",
    
    str_detect(x, regex("smart home", ignore_case = TRUE)) ~
      "Sistemas inteligentes del hogar",
    
    TRUE ~ x
  )
}

dimension_levels <- c(
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

dimension_dictionary <- tibble(
  dimension_key = dimension_levels,
  dimension_label = c(
    "Financiero",
    "Seguridad",
    "Competencia",
    "Autonomía",
    "Fisiología",
    "Relación",
    "Estímulo",
    "Popularidad",
    "Sentido"
  )
)

extract_dimensions <- function(x) {
  x <- str_squish(as.character(x))
  
  if (is.na(x) || x == "") {
    return(NA_character_)
  }
  
  detected <- dimension_levels[
    str_detect(x, fixed(dimension_levels, ignore_case = TRUE))
  ]
  
  if (length(detected) == 0) {
    return(NA_character_)
  }
  
  detected
}

# Matriz de determinantes por participante
determinants_wide <- df %>%
  select(participant_id, all_of(determinant_cols)) %>%
  pivot_longer(
    cols = all_of(determinant_cols),
    names_to = "determinant_col",
    values_to = "response_raw"
  ) %>%
  mutate(
    response_numeric = clean_determinant_score(response_raw)
  ) %>%
  left_join(
    determinant_dictionary,
    by = "determinant_col"
  ) %>%
  select(
    participant_id,
    determinant_id,
    response_numeric
  ) %>%
  pivot_wider(
    names_from = determinant_id,
    values_from = response_numeric
  )

write_csv(
  determinants_wide,
  file.path(csv_dir, "ttm_determinants_wide.csv")
)


# Localizar columnas de etapa/dimensión
find_unique_col <- function(pattern, label, exclude_pattern = NULL) {
  
  matches <- names(df)[
    str_detect(
      names(df),
      regex(pattern, ignore_case = TRUE)
    )
  ]
  
  if (!is.null(exclude_pattern)) {
    matches <- matches[
      !str_detect(
        matches,
        regex(exclude_pattern, ignore_case = TRUE)
      )
    ]
  }
  
  if (length(matches) == 0) {
    stop(
      paste0(
        "No se ha encontrado columna para: ", label,
        "\nPatrón usado: ", pattern
      )
    )
  }
  
  if (length(matches) > 1) {
    stop(
      paste0(
        "Hay más de una columna posible para: ", label,
        "\nColumnas encontradas:\n",
        paste(matches, collapse = "\n")
      )
    )
  }
  
  matches
}

implemented_technology_col <- find_unique_col(
  pattern = "^from_the_following_list_please_select_the_technology_or_energy_related_measure_you_have_implemented_at_home",
  label = "tecnología implementada"
)

implemented_dimensions_col <- find_unique_col(
  pattern = "^what_were_the_reasons_that_led_you_to_implement_or_contract_the_selected_technology_or_energy_related_measure",
  label = "dimensiones implementada"
)

interested_technology_col <- find_unique_col(
  pattern = "^which_of_the_following_technologies_or_energy_related_measures_are_you_most_interested_in_implementing_in_your_home",
  label = "tecnología interesada"
)

interested_dimensions_col <- find_unique_col(
  pattern = "^what_would_make_you_more_likely_to_implement_or_contract_this_technology_or_measure_please_select_the_3_most_important_for_you$",
  label = "dimensiones interesada"
)

curious_technology_col <- find_unique_col(
  pattern = "^is_there_a_technology_or_energy_related_measures_you_don_t_know_much_about_but_that_sparks_your_curiosity",
  label = "tecnología curiosidad"
)

curious_dimensions_col <- find_unique_col(
  pattern = "^what_would_make_you_more_likely_to_implement_or_contract_this_technology_or_measure_please_select_the_3_most_important_for_you_1$",
  label = "dimensiones curiosidad"
)

never_technology_col <- find_unique_col(
  pattern = "^is_there_any_technology_or_energy_realted_measure_on_this_list_that_you_would_never_install_in_your_home",
  label = "tecnología nunca"
)

cat("Tecnología implementada:", implemented_technology_col, "\n")
cat("Dimensiones implementada:", implemented_dimensions_col, "\n")
cat("Tecnología interesada:", interested_technology_col, "\n")
cat("Dimensiones interesada:", interested_dimensions_col, "\n")
cat("Tecnología curiosidad:", curious_technology_col, "\n")
cat("Dimensiones curiosidad:", curious_dimensions_col, "\n")
cat("Tecnología nunca:", never_technology_col, "\n")


# Crear tabla etapa - tecnología - dimensión
implemented_stage <- df %>%
  transmute(
    participant_id,
    stage = "Implementada",
    technology_raw = .data[[implemented_technology_col]],
    dimensions_raw = .data[[implemented_dimensions_col]]
  )

interested_stage <- df %>%
  transmute(
    participant_id,
    stage = "La conoce / la consideraría",
    technology_raw = .data[[interested_technology_col]],
    dimensions_raw = .data[[interested_dimensions_col]]
  )

curious_stage <- df %>%
  transmute(
    participant_id,
    stage = "No la conoce, pero le genera curiosidad",
    technology_raw = .data[[curious_technology_col]],
    dimensions_raw = .data[[curious_dimensions_col]]
  )

never_stage <- df %>%
  transmute(
    participant_id,
    stage = "Nunca la usaría",
    technology_raw = .data[[never_technology_col]],
    dimensions_raw = NA_character_
  )

ttm_stage_raw <- bind_rows(
  implemented_stage,
  interested_stage,
  curious_stage,
  never_stage
) %>%
  mutate(
    technology = clean_selected_technology(technology_raw),
    dimensions_list = lapply(dimensions_raw, extract_dimensions)
  ) %>%
  filter(
    !is.na(technology),
    technology != ""
  )

write_csv(
  ttm_stage_raw %>% select(-dimensions_list),
  file.path(csv_dir, "ttm_stage_technology_raw.csv")
)


# Expandir dimensiones seleccionadas
ttm_stage_dimension_long <- ttm_stage_raw %>%
  unnest_longer(
    dimensions_list,
    values_to = "dimension_key",
    keep_empty = TRUE
  ) %>%
  mutate(
    dimension_key = str_to_upper(as.character(dimension_key))
  ) %>%
  left_join(
    dimension_dictionary,
    by = "dimension_key"
  ) %>%
  mutate(
    dimension = dimension_label
  ) %>%
  select(
    participant_id,
    stage,
    technology,
    technology_raw,
    dimension_key,
    dimension,
    dimensions_raw
  )

write_csv(
  ttm_stage_dimension_long,
  file.path(csv_dir, "ttm_stage_dimension_long.csv")
)

print(ttm_stage_dimension_long, n = 100)


# Resumen de tecnologías por etapa
summary_technology_by_stage <- ttm_stage_dimension_long %>%
  distinct(participant_id, stage, technology) %>%
  count(
    stage,
    technology,
    sort = TRUE,
    name = "n_participants"
  ) %>%
  group_by(stage) %>%
  mutate(
    percentage = n_participants / sum(n_participants) * 100
  ) %>%
  ungroup()

write_csv(
  summary_technology_by_stage,
  file.path(csv_dir, "summary_technology_by_stage.csv")
)

print(summary_technology_by_stage, n = Inf)


# Resumen de dimensiones por etapa
summary_dimension_by_stage <- ttm_stage_dimension_long %>%
  filter(!is.na(dimension)) %>%
  count(
    stage,
    dimension,
    sort = TRUE,
    name = "n_mentions"
  ) %>%
  group_by(stage) %>%
  mutate(
    percentage = n_mentions / sum(n_mentions) * 100
  ) %>%
  ungroup()

write_csv(
  summary_dimension_by_stage,
  file.path(csv_dir, "summary_dimension_by_stage.csv")
)

print(summary_dimension_by_stage, n = Inf)


# Preparar mapping dimensión -> determinantes
dimension_determinant_mapping_fixed <- dimension_determinant_mapping %>%
  rename(dimension_key = dimension) %>%
  mutate(
    dimension_key = str_to_upper(dimension_key)
  )

write_csv(
  dimension_determinant_mapping_fixed,
  file.path(csv_dir, "dimension_determinant_mapping.csv")
)

mapping_long <- dimension_determinant_mapping_fixed %>%
  pivot_longer(
    cols = all_of(determinant_ids),
    names_to = "determinant_id",
    values_to = "is_linked"
  ) %>%
  mutate(
    is_linked = as.numeric(is_linked)
  )

write_csv(
  mapping_long,
  file.path(csv_dir, "dimension_determinant_mapping_long.csv")
)

# Comprobación rápida del mapping

print(
  mapping_long %>%
    filter(is_linked == 1) %>%
    count(dimension_key),
  n = Inf
)


# Construir vector de 32 determinantes por etapa/dimensión
determinants_long_scores <- determinants_wide %>%
  pivot_longer(
    cols = all_of(determinant_ids),
    names_to = "determinant_id",
    values_to = "determinant_score"
  )

ttm_stage_determinant_vector_long <- ttm_stage_dimension_long %>%
  filter(!is.na(dimension_key)) %>%
  mutate(
    stage_dimension_id = row_number()
  ) %>%
  crossing(
    determinant_id = determinant_ids
  ) %>%
  left_join(
    mapping_long,
    by = c("dimension_key", "determinant_id")
  ) %>%
  left_join(
    determinants_long_scores,
    by = c("participant_id", "determinant_id")
  ) %>%
  mutate(
    is_linked = replace_na(is_linked, 0),
    determinant_score_stage = if_else(
      is_linked == 1,
      determinant_score,
      NA_real_
    )
  ) %>%
  left_join(
    determinant_dictionary %>%
      select(determinant_id, determinant_label),
    by = "determinant_id"
  )

write_csv(
  ttm_stage_determinant_vector_long,
  file.path(csv_dir, "ttm_stage_determinant_vector_long.csv")
)

check_vector_filling <- ttm_stage_determinant_vector_long %>%
  group_by(
    stage_dimension_id,
    participant_id,
    stage,
    technology,
    dimension_key,
    dimension
  ) %>%
  summarise(
    n_determinants_linked = sum(is_linked == 1, na.rm = TRUE),
    n_determinants_filled = sum(is_linked == 1 & !is.na(determinant_score_stage)),
    n_determinants_missing = n_determinants_linked - n_determinants_filled,
    .groups = "drop"
  )

write_csv(
  check_vector_filling,
  file.path(csv_dir, "check_vector_filling.csv")
)

print(
  check_vector_filling %>%
    count(dimension, n_determinants_linked, n_determinants_filled),
  n = Inf
)

check_vector_filling %>%
  filter(n_determinants_filled == 0) %>%
  arrange(dimension, stage, participant_id) %>%
  print(n = Inf)

write_csv(
  check_vector_filling %>%
    filter(n_determinants_filled == 0),
  file.path(csv_dir, "check_vector_filling_empty_rows.csv")
)

ttm_stage_determinant_vector_wide <- ttm_stage_determinant_vector_long %>%
  select(
    stage_dimension_id,
    participant_id,
    stage,
    technology,
    dimension_key,
    dimension,
    determinant_id,
    determinant_score_stage
  ) %>%
  pivot_wider(
    names_from = determinant_id,
    values_from = determinant_score_stage
  )

write_csv(
  ttm_stage_determinant_vector_wide,
  file.path(csv_dir, "ttm_stage_determinant_vector_wide.csv")
)

ttm_stage_determinant_vector_wide_valid <- ttm_stage_determinant_vector_wide %>%
  left_join(
    check_vector_filling %>%
      select(stage_dimension_id, n_determinants_filled),
    by = "stage_dimension_id"
  ) %>%
  filter(n_determinants_filled > 0)

write_csv(
  ttm_stage_determinant_vector_wide_valid,
  file.path(csv_dir, "ttm_stage_determinant_vector_wide_valid.csv")
)


# Resumen de determinantes por etapa
summary_determinants_by_stage <- ttm_stage_determinant_vector_long %>%
  filter(
    is_linked == 1,
    !is.na(determinant_score_stage)
  ) %>%
  group_by(
    stage,
    determinant_id,
    determinant_label
  ) %>%
  summarise(
    n_mentions = n(),
    mean_score = mean(determinant_score_stage, na.rm = TRUE),
    median_score = median(determinant_score_stage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    stage,
    desc(n_mentions),
    desc(mean_score)
  )

write_csv(
  summary_determinants_by_stage,
  file.path(csv_dir, "summary_determinants_by_stage.csv")
)

print(summary_determinants_by_stage, n = Inf)


# Gráfico de la tabla final etapa/intervención/dimensión/vector

# Seleccionar tecnologías más frecuentes por etapa para que el gráfico no sea ilegible

top_technologies_by_stage <- ttm_stage_dimension_long %>%
  filter(
    !is.na(dimension),
    technology != "Ninguna"
  ) %>%
  distinct(participant_id, stage, technology) %>%
  count(stage, technology, sort = TRUE) %>%
  group_by(stage) %>%
  slice_max(n, n = 4, with_ties = FALSE) %>%
  ungroup()

vector_final_plot_data <- ttm_stage_determinant_vector_long %>%
  filter(
    is_linked == 1,
    !is.na(determinant_score_stage)
  ) %>%
  semi_join(
    top_technologies_by_stage,
    by = c("stage", "technology")
  ) %>%
  group_by(
    stage,
    technology,
    dimension,
    determinant_label
  ) %>%
  summarise(
    mean_score = mean(determinant_score_stage, na.rm = TRUE),
    n_values = n(),
    .groups = "drop"
  ) %>%
  mutate(
    determinant_label = factor(
      determinant_label,
      levels = rev(determinant_dictionary$determinant_label)
    )
  )

plot_final_stage_technology_dimension_vector <- ggplot(
  vector_final_plot_data,
  aes(
    x = dimension,
    y = determinant_label,
    fill = mean_score
  )
) +
  geom_tile(color = "black", linewidth = 0.2) +
  geom_text(
    aes(label = round(mean_score, 0)),
    size = 2.5
  ) +
  facet_grid(
    rows = vars(stage),
    cols = vars(technology),
    scales = "free_x",
    space = "free_x"
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#0072B2",
    limits = c(0, 100)
  ) +
  labs(
    title = "Vector medio de determinantes por etapa, tecnología y dimensión",
    subtitle = "Solo se muestran las tecnologías más frecuentes de cada etapa. Valores medios en escala 0-100",
    x = "Dimensión seleccionada",
    y = "Determinante",
    fill = "Media"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    axis.text.x = element_text(angle = 35, hjust = 1),
    axis.text.y = element_text(size = 7),
    strip.text = element_text(face = "bold", size = 8),
    legend.position = "right"
  )

print(plot_final_stage_technology_dimension_vector)

save_plot_png(
  plot_final_stage_technology_dimension_vector,
  "final_stage_technology_dimension_vector_heatmap",
  width = 16,
  height = 12
)


# Gráficos de resultados TTM
theme_ttm <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(10, 20, 10, 10)
  )


# Tecnologías por etapa
plot_technology_by_stage <- summary_technology_by_stage %>%
  group_by(stage) %>%
  slice_max(n_participants, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    technology = str_wrap(technology, width = 28),
    technology = reorder(technology, n_participants)
  ) %>%
  ggplot(
    aes(
      x = technology,
      y = n_participants,
      fill = stage
    )
  ) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = n_participants),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip(clip = "off") +
  facet_wrap(~ stage, scales = "free_y") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Tecnologías más frecuentes por etapa TTM",
    subtitle = "Top 10 tecnologías/intervenciones dentro de cada etapa",
    x = NULL,
    y = "Número de participantes"
  ) +
  theme_ttm

print(plot_technology_by_stage)

save_plot_png(
  plot_technology_by_stage,
  "ttm_top_technologies_by_stage",
  width = 13,
  height = 8
)

# Dimensiones por etapa
plot_dimensions_by_stage_percentage <- summary_dimension_by_stage %>%
  ggplot(
    aes(
      x = stage,
      y = percentage,
      fill = dimension
    )
  ) +
  geom_col(color = "black", linewidth = 0.2) +
  geom_text(
    aes(
      label = if_else(
        percentage >= 5,
        paste0(round(percentage, 0), "%"),
        ""
      )
    ),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +
  coord_flip() +
  labs(
    title = "Distribución de dimensiones por etapa TTM",
    subtitle = "Porcentajes calculados dentro de cada etapa",
    x = NULL,
    y = "Porcentaje",
    fill = "Dimensión"
  ) +
  theme_ttm

print(plot_dimensions_by_stage_percentage)

save_plot_png(
  plot_dimensions_by_stage_percentage,
  "ttm_dimensions_by_stage_percentage",
  width = 11,
  height = 6
)

# Heatmap etapa x dimensión
plot_heatmap_dimensions_by_stage <- summary_dimension_by_stage %>%
  ggplot(
    aes(
      x = dimension,
      y = stage,
      fill = percentage
    )
  ) +
  geom_tile(color = "black", linewidth = 0.25) +
  geom_text(
    aes(
      label = paste0(
        round(percentage, 0),
        "%\n(n=",
        n_mentions,
        ")"
      )
    ),
    size = 3,
    lineheight = 0.9
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#0072B2"
  ) +
  labs(
    title = "Dimensiones seleccionadas por etapa TTM",
    subtitle = "Porcentaje y número de menciones dentro de cada etapa",
    x = "Dimensión",
    y = NULL,
    fill = "Porcentaje"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "right"
  )

print(plot_heatmap_dimensions_by_stage)

save_plot_png(
  plot_heatmap_dimensions_by_stage,
  "ttm_heatmap_dimensions_by_stage",
  width = 12,
  height = 6
)


# Top determinantes por etapa
plot_top_determinants_by_stage <- summary_determinants_by_stage %>%
  group_by(stage) %>%
  slice_max(n_mentions, n = 8, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    determinant_label = str_wrap(determinant_label, width = 24),
    determinant_label = reorder(determinant_label, n_mentions)
  ) %>%
  ggplot(
    aes(
      x = determinant_label,
      y = n_mentions,
      fill = stage
    )
  ) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = n_mentions),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip(clip = "off") +
  facet_wrap(~ stage, scales = "free_y") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Determinantes más frecuentes por etapa TTM",
    subtitle = "Top 8 determinantes asociados a las dimensiones seleccionadas",
    x = NULL,
    y = "Número de menciones"
  ) +
  theme_ttm

print(plot_top_determinants_by_stage)

save_plot_png(
  plot_top_determinants_by_stage,
  "ttm_top_determinants_by_stage",
  width = 13,
  height = 8
)

# Heatmap de score medio de determinantes por etapa
plot_heatmap_determinants_mean_score <- summary_determinants_by_stage %>%
  group_by(determinant_label) %>%
  mutate(
    total_mentions = sum(n_mentions)
  ) %>%
  ungroup() %>%
  filter(total_mentions >= 5) %>%
  mutate(
    determinant_label = factor(
      determinant_label,
      levels = rev(unique(determinant_label[order(total_mentions)]))
    )
  ) %>%
  ggplot(
    aes(
      x = stage,
      y = determinant_label,
      fill = mean_score
    )
  ) +
  geom_tile(color = "black", linewidth = 0.2) +
  geom_text(
    aes(label = round(mean_score, 1)),
    size = 2.8
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#0072B2",
    limits = c(0, 100)
  ) +
  labs(
    title = "Valor medio de los determinantes por etapa TTM",
    subtitle = "Solo determinantes con al menos 5 menciones totales",
    x = NULL,
    y = NULL,
    fill = "Media"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 35, hjust = 1),
    axis.text.y = element_text(size = 8),
    legend.position = "right"
  )

print(plot_heatmap_determinants_mean_score)

save_plot_png(
  plot_heatmap_determinants_mean_score,
  "ttm_heatmap_determinants_mean_score_by_stage",
  width = 11,
  height = 10
)


# Guardar todos los gráficos en un único PDF
save_plots_pdf <- function(plot_list, filename, width = 12, height = 8) {
  
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

all_ttm_plots <- list(
  plot_technology_by_stage,
  plot_dimensions_by_stage_percentage,
  plot_heatmap_dimensions_by_stage,
  plot_top_determinants_by_stage,
  plot_heatmap_determinants_mean_score,
  plot_final_stage_technology_dimension_vector
)

save_plots_pdf(
  plot_list = all_ttm_plots,
  filename = "ttm_stage_dimension_determinants_all_plots.pdf",
  width = 12,
  height = 8
)

cat("Script 07 finalizado correctamente.\n")
cat("Resultados guardados en:", base_output_dir, "\n")