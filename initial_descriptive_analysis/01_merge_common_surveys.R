
# SCRIPT 01 - PREPARACIÓN E INTEGRACIÓN DE DATOS
#
# Este script forma parte de la fase de *Data Preparation* del proyecto y tiene
# como objetivo preparar y consolidar los datos procedentes de varias encuestas
# del proyecto (EUSurvey).
#
# Se integran las respuestas de la encuesta RV-Decision con las encuestas
# principales que comparten secciones comunes:
# - RV-Concerns2
# - RV-Poverty
# - RV-Energy Crisis
#
# El objetivo es reconstruir, para cada participante de RV-Decision, la información
# común que pudo haber sido respondida previamente en otra encuesta del proyecto.
# Estas secciones comunes son:
# - Socio-economic
# - Personal Profile
# - Experience
#
# La integración se realiza mediante una clave única (`join_key`), construida a
# partir del Prolific ID o, en su defecto, del código identificativo introducido
# por el participante.
#
# Procedimiento:
# 1. Lectura de los datos exportados desde EUSurvey.
# 2. Limpieza de encabezados y normalización de nombres de variables.
# 3. Creación de una clave de unión entre encuestas.
# 4. Identificación de variables comunes entre encuestas.
# 5. Integración de información externa en RV-Decision.
# 6. Construcción de variables finales (`*_final`), priorizando siempre la
#    respuesta de RV-Decision y usando otras encuestas solo para completar vacíos.
# 7. Detección y registro de inconsistencias (conflictos) entre respuestas.
# 8. Identificación de casos potencialmente problemáticos.
# 9. Generación de un dataset consolidado provisional y ficheros auxiliares.
#
# Nota:
# Las encuestas RV-Concerns1 y RVS presentan una estructura diferente y se
# integrarán en una fase posterior a partir del dataset consolidado generado aquí.
#

# librerias
library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(ggplot2)

# RUTAS DE ARCHIVOS
files <- list(
  #decision = "initial_descriptive_analysis/data/Content_Export_RV-Decision_Decision.xlsx",
  decision = "initial_descriptive_analysis/data/Content_Export_RV-Decision__423.xlsx",
  concerns2 = "initial_descriptive_analysis/data/Content_Export_RV-Concerns_2__161.xlsx",
  poverty = "initial_descriptive_analysis/data/Content_Export_RV-Poverty__96.xlsx",
  energy_crisis = "initial_descriptive_analysis/data/Content_Export_RV-Energy_Crisis__156.xlsx"
)

# LIMPIAR TEXTO DE PREGUNTA PARA CREAR NOMBRE DE VARIABLE
clean_question_name <- function(x) {
  x %>%
    str_remove("\\s*\\(ID\\d+\\)\\s*$") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

# FUNCIÓN PARA LEER CADA ENCUESTA
read_survey <- function(path, survey_name) {
  
  raw <- read_excel(path, col_names = FALSE)
  
  # fila 4 = preguntas reales
  original_names <- as.character(raw[4, ])
  
  # datos desde fila 5
  df <- raw[-c(1:4), ]
  
  # nombres basados en texto de pregunta, no en ID
  logical_names <- clean_question_name(original_names)
  logical_names <- make.unique(logical_names, sep = "_")
  
  names(df) <- logical_names
  
  dictionary <- data.frame(
    survey = survey_name,
    variable = logical_names,
    question = original_names,
    stringsAsFactors = FALSE
  )
  
  return(list(data = df, dictionary = dictionary))
}


# LEER ENCUESTAS
surveys <- imap(files, read_survey)

df_decision <- surveys$decision$data
df_concerns2 <- surveys$concerns2$data
df_poverty <- surveys$poverty$data
df_energy <- surveys$energy_crisis$data

dictionary_all <- bind_rows(
  surveys$decision$dictionary,
  surveys$concerns2$dictionary,
  surveys$poverty$dictionary,
  surveys$energy_crisis$dictionary
)

View(dictionary_all)

# HELPER PARA BUSCAR VARIABLES
get_q <- function(text) {
  dictionary_all %>%
    filter(grepl(text, question, ignore.case = TRUE))
}

get_q("Prolific ID")
get_q("identification code")
get_q("previous survey")
get_q("year of birth")
get_q("gender")

# NOMBRES LÓGICOS IMPORTANTES
var_prolific <- "please_provide_your_prolific_id"
var_identification_code <- "please_provide_your_identification_code"
var_previous_survey <- "have_you_ever_taken_a_survey_about_this_project_before"
var_has_code_or_prolific <- "do_you_have_a_prolific_id_or_an_identification_code_from_a_previous_survey"

# CREAR CLAVE DE UNIÓN
create_join_key <- function(df) {
  
  df %>%
    mutate(
      prolific_id = if (var_prolific %in% names(.)) .[[var_prolific]] else NA_character_,
      identification_code = if (var_identification_code %in% names(.)) .[[var_identification_code]] else NA_character_,
      
      prolific_id = str_trim(as.character(prolific_id)),
      identification_code = str_trim(as.character(identification_code)),
      
      join_key = case_when(
        !is.na(prolific_id) & prolific_id != "" ~ paste0("PROLIFIC_", prolific_id),
        !is.na(identification_code) & identification_code != "" ~ paste0("CODE_", identification_code),
        TRUE ~ NA_character_
      )
    )
}

df_decision <- create_join_key(df_decision)
df_concerns2 <- create_join_key(df_concerns2)
df_poverty <- create_join_key(df_poverty)
df_energy <- create_join_key(df_energy)

# IDENTIFICAR COLUMNAS COMUNES POR NOMBRE (identificar partes comunes)
common_vars <- dictionary_all %>%
  count(variable) %>%
  filter(n > 1) %>%
  pull(variable)

# quitar columnas de control
common_vars <- setdiff(
  common_vars,
  c(var_has_code_or_prolific, var_prolific, var_previous_survey, var_identification_code)
)

common_vars

# CREAR BASE COMÚN DESDE OTRAS ENCUESTAS
common_from_other_surveys <- bind_rows(
  df_concerns2 %>% select(join_key, any_of(common_vars)),
  df_poverty %>% select(join_key, any_of(common_vars)),
  df_energy %>% select(join_key, any_of(common_vars))
) %>%
  filter(!is.na(join_key)) %>%
  group_by(join_key) %>%
  summarise(
    across(everything(), ~ first(na.omit(.x))),
    .groups = "drop"
  )

# 10. DETECTAR QUIÉNES NECESITAN COMPLETAR DATOS
df_decision <- df_decision %>%
  mutate(
    has_join_key = !is.na(join_key),
    has_previous_survey = if (var_previous_survey %in% names(.)) .[[var_previous_survey]] else NA_character_,
    
    needs_external_data = case_when(
      has_join_key & grepl("Yes", has_previous_survey, ignore.case = TRUE) ~ TRUE,
      TRUE ~ FALSE
    )
  )

table(df_decision$needs_external_data, useNA = "ifany")

# MERGE
df_final <- df_decision %>%
  left_join(
    common_from_other_surveys,
    by = "join_key",
    suffix = c("", "_external")
  )
View(df_final)

# CREAR VARIABLES FINALES UNIFICADAS
# Se prioriza el valor de RV-Decision.
# Si está vacío, se usa el valor externo.

for (var in common_vars) {
  
  ext_var <- paste0(var, "_external")
  final_var <- paste0(var, "_final")
  
  if (var %in% names(df_final) && ext_var %in% names(df_final)) {
    
    df_final[[final_var]] <- coalesce(
      df_final[[var]],
      df_final[[ext_var]]
    )
  }
}

# CREAR FLAGS DE CONFLICTO POR VARIABLE
# Limpia respuestas para comparar bien:
# - elimina IDs internos de EUSurvey: (ID123)
# - minúsculas
# - espacios extra
# - punto final

clean_answer <- function(x) {
  x %>%
    as.character() %>%
    str_remove("\\s*\\(ID\\d+\\)\\s*$") %>%
    str_to_lower() %>%
    str_trim() %>%
    str_replace_all("\\.$", "") %>%
    str_replace_all("\\s+", " ")
}

for (var in common_vars) {
  
  ext_var <- paste0(var, "_external")
  conflict_var <- paste0(var, "_conflict")
  
  if (var %in% names(df_final) && ext_var %in% names(df_final)) {
    
    df_final[[conflict_var]] <- !is.na(df_final[[var]]) &
      !is.na(df_final[[ext_var]]) &
      clean_answer(df_final[[var]]) != clean_answer(df_final[[ext_var]])
  }
}

######################### COMPROBAR CALIDAD DEL DATASET ########################

# COMPROBAR COLUMNAS CREADAS
final_vars <- grep("_final$", names(df_final), value = TRUE)

final_vars
View(df_final[, c("join_key", final_vars)])

# DETECTAR CONFLICTOS ENTRE VALORES ORIGINALES Y EXTERNOS
conflict_summary <- data.frame()

for (var in common_vars) {
  
  ext_var <- paste0(var, "_external")
  
  if (var %in% names(df_final) && ext_var %in% names(df_final)) {
    
    conflicts <- sum(
      !is.na(df_final[[var]]) &
        !is.na(df_final[[ext_var]]) &
        clean_answer(df_final[[var]]) != clean_answer(df_final[[ext_var]]),
      na.rm = TRUE
    )
    
    conflict_summary <- rbind(
      conflict_summary,
      data.frame(variable = var, conflicts = conflicts)
    )
  }
}

View(conflict_summary)

#MIRAR QUÉ FILAS FALLAN
conflict_rows <- data.frame()

for (var in common_vars) {
  
  ext_var <- paste0(var, "_external")
  
  if (var %in% names(df_final) && ext_var %in% names(df_final)) {
    
    temp <- df_final %>%
      filter(
        !is.na(.data[[var]]),
        !is.na(.data[[ext_var]]),
        clean_answer(.data[[var]]) != clean_answer(.data[[ext_var]])
      ) %>%
      select(join_key, all_of(var), all_of(ext_var)) %>%
      mutate(variable = var)
    
    conflict_rows <- bind_rows(conflict_rows, temp)
  }
}

conflict_rows %>%
  arrange(variable) %>%
  View()

conflict_rows %>%
  count(join_key, sort = TRUE)


#### este caso ha rellenado doss veces la parte de la encuesta comun!!!!!!!! 
# las diferencias en alguna tecnologia si que hay(diferente fase)
# lo demas es diferencia de 9 vs 10 o cosas asi
conflict_rows %>%
  filter(join_key == "PROLIFIC_5cb4adc019ee7300189e8547") %>%
  View()


# MARCAR CASOS PROBLEMÁTICOS
problematic_ids <- conflict_rows %>%
  count(join_key, sort = TRUE) %>%
  filter(n > 5) %>%
  pull(join_key)

df_final <- df_final %>%
  mutate(problematic_merge = join_key %in% problematic_ids)


######################### GUARDAR DATASETS #########################
base_output_dir <- "initial_descriptive_analysis/output/data_preparation"

csv_dir <- file.path(base_output_dir, "csv")
plots_dir <- file.path(base_output_dir, "plots")
logs_dir <- file.path(base_output_dir, "logs")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

write.csv(
  df_final,
  file.path(csv_dir, "df_decision_consolidated_provisional.csv"), 
)

write.csv(
  dictionary_all,
  file.path(csv_dir, "dictionary_consolidated_provisional.csv")
)

write.csv(
  conflict_rows,
  file.path(logs_dir, "conflict_rows_provisional.csv")
)

write.csv(
  conflict_summary,
  file.path(logs_dir, "conflict_summary_provisional.csv")
)

