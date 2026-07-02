# SCRIPT 00 - UNIFICACIÓN EXPORTS EUSURVEY CON IDIOMA, FECHA Y GRÁFICOS

library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(tibble)
library(ggplot2)


# 1. RUTAS
data_dir <- "initial_descriptive_analysis/data"

files <- tribble(
  ~survey,          ~path,
  "decision",      file.path(data_dir, "Content_Export_RV-Decision_421.xlsx"),
  "concerns2",     file.path(data_dir, "Content_Export_RV-Concerns_2_162.xlsx"),
  "concerns1",     file.path(data_dir, "Content_Export_RV-Concerns_12.xlsx"),
  "poverty",       file.path(data_dir, "Content_Export_RV-Poverty__94.xlsx"),
  "energy_crisis", file.path(data_dir, "Content_Export_RV-Energy_Crisis_157.xlsx"),
  "rvs",           file.path(data_dir, "Content_Export_RVS_8.xlsx")
)

base_output_dir <- "initial_descriptive_analysis/output/data_preparation"

csv_dir   <- file.path(base_output_dir, "csv")
logs_dir  <- file.path(base_output_dir, "logs")
plots_dir <- file.path(base_output_dir, "plots")
dict_dir  <- file.path(base_output_dir, "dictionaries")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dict_dir, recursive = TRUE, showWarnings = FALSE)



# 2. FUNCIONES AUXILIARES

clean_question_name <- function(x) {
  x <- as.character(x)
  x <- if_else(is.na(x) | str_squish(x) == "", "unnamed_column", x)
  
  x %>%
    str_remove("\\s*\\(ID\\d+\\)\\s*$") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

clean_text_basic <- function(x) {
  x <- str_squish(as.character(x))
  x <- na_if(x, "")
  x <- na_if(x, "NA")
  x <- na_if(x, "NaN")
  x
}

parse_eusurvey_datetime <- function(x) {
  x <- clean_text_basic(x)
  x_num <- suppressWarnings(as.numeric(x))
  
  out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC")
  
  idx_num <- !is.na(x_num)
  out[idx_num] <- as.POSIXct(
    (x_num[idx_num] - 25569) * 86400,
    origin = "1970-01-01",
    tz = "UTC"
  )
  
  idx_txt <- !idx_num & !is.na(x)
  out[idx_txt] <- suppressWarnings(as.POSIXct(x[idx_txt], tz = "UTC"))
  
  out
}

get_optional_col <- function(data, col_name) {
  if (col_name %in% names(data)) {
    clean_text_basic(data[[col_name]])
  } else {
    rep(NA_character_, nrow(data))
  }
}

coalesce_optional_cols <- function(data, candidates) {
  existing <- candidates[candidates %in% names(data)]
  
  if (length(existing) == 0) {
    return(rep(NA_character_, nrow(data)))
  }
  
  values <- lapply(existing, function(col) clean_text_basic(data[[col]]))
  do.call(coalesce, values)
}

first_non_empty <- function(x) {
  x <- clean_text_basic(x)
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  x[1]
}

clean_answer <- function(x) {
  x %>%
    as.character() %>%
    str_remove("\\s*\\(ID\\d+\\)\\s*$") %>%
    str_to_lower() %>%
    str_trim() %>%
    str_replace_all("\\.$", "") %>%
    str_replace_all("\\s+", " ")
}

pad_to_n <- function(df, n) {
  if (nrow(df) >= n) return(df)
  
  extra <- as_tibble(
    matrix(
      NA_character_,
      nrow = n - nrow(df),
      ncol = ncol(df),
      dimnames = list(NULL, names(df))
    )
  )
  
  bind_rows(df, extra)
}

align_to_cols <- function(df, all_cols) {
  df <- df %>%
    mutate(across(everything(), as.character))
  
  missing_cols <- setdiff(all_cols, names(df))
  
  for (col in missing_cols) {
    df[[col]] <- NA_character_
  }
  
  df %>%
    select(all_of(all_cols))
}



# 3. LECTURA ROBUSTA DE EXPORTS EUSURVEY
#    También lee ficheros con varias hojas: Content + Content1
read_eusurvey_export <- function(path, survey_name) {
  
  sheet_names <- excel_sheets(path)
  
  sheet_objects <- map(sheet_names, function(sh) {
    
    raw <- read_excel(
      path,
      sheet = sh,
      col_names = FALSE,
      col_types = "text"
    )
    
    alias <- as.character(raw[[2]][1])
    export_date_raw <- as.character(raw[[2]][2])
    export_datetime <- parse_eusurvey_datetime(export_date_raw)
    
    original_names <- raw[4, ] %>%
      unlist(use.names = FALSE) %>%
      as.character()
    
    df <- raw[-c(1:4), , drop = FALSE]
    
    logical_names <- clean_question_name(original_names)
    logical_names <- make.unique(logical_names, sep = "_")
    
    names(df) <- logical_names
    
    dictionary <- tibble(
      survey = survey_name,
      source_file = basename(path),
      sheet = sh,
      variable = logical_names,
      question = original_names
    )
    
    list(
      data = df,
      dictionary = dictionary,
      alias = alias,
      export_datetime = export_datetime
    )
  })
  
  max_rows <- max(map_int(sheet_objects, ~ nrow(.x$data)))
  
  data_combined <- sheet_objects %>%
    map(~ pad_to_n(.x$data, max_rows)) %>%
    bind_cols(.name_repair = "unique")
  
  dictionary_combined <- bind_rows(map(sheet_objects, "dictionary"))
  
  alias <- first_non_empty(map_chr(sheet_objects, "alias"))
  export_datetime <- sheet_objects[[1]]$export_datetime
  
  # Columnas nuevas solicitadas
  creation_dt <- parse_eusurvey_datetime(get_optional_col(data_combined, "creation_date"))
  last_update_dt <- parse_eusurvey_datetime(get_optional_col(data_combined, "last_update"))
  export_dt_vec <- rep(export_datetime, nrow(data_combined))
  
  fecha_datetime <- coalesce(creation_dt, last_update_dt, export_dt_vec)
  
  data_combined <- data_combined %>%
    mutate(
      source_survey = survey_name,
      source_file = basename(path),
      export_alias = alias,
      export_date = as.Date(export_datetime),
      fecha_datetime = fecha_datetime,
      fecha = as.Date(fecha_datetime),
      idioma = coalesce(
        get_optional_col(., "languages"),
        rep(NA_character_, nrow(.))
      ),
      .before = 1
    )
  
  list(
    data = data_combined,
    dictionary = dictionary_combined
  )
}

# 4. LEER TODOS LOS FICHEROS
exports <- pmap(
  files,
  function(survey, path) {
    read_eusurvey_export(path, survey)
  }
)

df_all_raw <- bind_rows(map(exports, "data"))
dictionary_all <- bind_rows(map(exports, "dictionary"))

write_csv(
  dictionary_all,
  file.path(dict_dir, "dictionary_all_exports.csv")
)



# 5. CREAR join_key
create_join_key <- function(df) {
  
  prolific_candidates <- names(df)[
    str_detect(names(df), "provide_your_prolific_id|provide_your_prolific")
  ]
  
  code_candidates <- names(df)[
    str_detect(names(df), "identification_code|5_characters") &
      !str_detect(names(df), "^do_you_have|have_you_ever")
  ]
  
  df %>%
    mutate(
      prolific_id = coalesce_optional_cols(., prolific_candidates),
      identification_code = coalesce_optional_cols(., code_candidates),
      
      prolific_id = str_to_lower(str_trim(prolific_id)),
      identification_code = str_to_upper(str_trim(identification_code)),
      
      prolific_id = na_if(prolific_id, ""),
      identification_code = na_if(identification_code, ""),
      
      join_key = case_when(
        !is.na(prolific_id) ~ paste0("PROLIFIC_", prolific_id),
        !is.na(identification_code) ~ paste0("CODE_", identification_code),
        TRUE ~ NA_character_
      )
    )
}

df_all_raw <- create_join_key(df_all_raw)

df_decision <- df_all_raw %>% filter(source_survey == "decision")

df_other_main <- df_all_raw %>%
  filter(source_survey %in% c("concerns2", "poverty", "energy_crisis"))

df_extra_append <- df_all_raw %>%
  filter(source_survey %in% c("concerns1", "rvs"))


# 6. MERGE PRINCIPAL:
#    Decision + Concerns2 + Poverty + Energy Crisis
metadata_cols <- c(
  "source_survey",
  "source_file",
  "export_alias",
  "export_date",
  "fecha",
  "fecha_datetime",
  "idioma",
  "creation_date",
  "last_update",
  "languages",
  "prolific_id",
  "identification_code",
  "join_key"
)

control_cols <- c(
  "do_you_have_a_prolific_id_or_an_identification_code_from_a_previous_survey",
  "please_provide_your_prolific_id",
  "have_you_ever_taken_a_survey_about_this_project_before",
  "please_provide_your_identification_code"
)

decision_vars <- setdiff(names(df_decision), c(metadata_cols, control_cols))
other_main_vars <- setdiff(names(df_other_main), c(metadata_cols, control_cols))

common_vars <- intersect(decision_vars, other_main_vars)

common_from_other_surveys <- df_other_main %>%
  filter(!is.na(join_key)) %>%
  group_by(join_key) %>%
  summarise(
    external_surveys_used = paste(unique(source_survey), collapse = "; "),
    external_idiomas_used = paste(unique(na.omit(idioma)), collapse = "; "),
    external_fechas_used = paste(unique(na.omit(as.character(fecha))), collapse = "; "),
    across(all_of(common_vars), first_non_empty),
    .groups = "drop"
  )

previous_survey_col <- "have_you_ever_taken_a_survey_about_this_project_before"

df_merged <- df_decision %>%
  mutate(
    has_join_key = !is.na(join_key),
    has_previous_survey = if (previous_survey_col %in% names(.)) {
      .data[[previous_survey_col]]
    } else {
      NA_character_
    },
    needs_external_data = case_when(
      has_join_key & str_detect(has_previous_survey, regex("Yes", ignore_case = TRUE)) ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  left_join(
    common_from_other_surveys,
    by = "join_key",
    suffix = c("", "_external")
  ) %>%
  mutate(
    merge_status = case_when(
      !is.na(external_surveys_used) ~ "decision_with_external_match",
      TRUE ~ "decision_only_no_external_match"
    )
  )

# Crear variables *_final priorizando Decision
for (var in common_vars) {
  
  ext_var <- paste0(var, "_external")
  final_var <- paste0(var, "_final")
  
  if (var %in% names(df_merged) && ext_var %in% names(df_merged)) {
    df_merged[[final_var]] <- coalesce(
      clean_text_basic(df_merged[[var]]),
      clean_text_basic(df_merged[[ext_var]])
    )
  }
}

# Detectar conflictos
conflict_rows <- tibble()

for (var in common_vars) {
  
  ext_var <- paste0(var, "_external")
  
  if (var %in% names(df_merged) && ext_var %in% names(df_merged)) {
    
    temp <- df_merged %>%
      filter(
        !is.na(.data[[var]]),
        !is.na(.data[[ext_var]]),
        clean_answer(.data[[var]]) != clean_answer(.data[[ext_var]])
      ) %>%
      select(
        join_key,
        source_survey,
        idioma,
        fecha,
        all_of(var),
        all_of(ext_var)
      ) %>%
      mutate(variable = var)
    
    conflict_rows <- bind_rows(conflict_rows, temp)
  }
}

conflict_summary <- conflict_rows %>%
  count(variable, name = "conflicts") %>%
  arrange(desc(conflicts))

problematic_ids <- conflict_rows %>%
  count(join_key, sort = TRUE) %>%
  filter(n > 5) %>%
  pull(join_key)

df_merged <- df_merged %>%
  mutate(problematic_merge = join_key %in% problematic_ids)


# 7. AÑADIR ABAJO LO QUE NO SE PUEDE UNIR
decision_keys <- df_decision %>%
  filter(!is.na(join_key)) %>%
  pull(join_key) %>%
  unique()

df_other_main_unmatched <- df_other_main %>%
  filter(is.na(join_key) | !join_key %in% decision_keys) %>%
  mutate(
    external_surveys_used = NA_character_,
    external_idiomas_used = NA_character_,
    external_fechas_used = NA_character_,
    needs_external_data = NA,
    has_join_key = !is.na(join_key),
    has_previous_survey = NA_character_,
    problematic_merge = FALSE,
    merge_status = "external_unmatched_appended"
  )

# Para filas externas no unidas, crear *_final cuando exista la variable
for (var in common_vars) {
  final_var <- paste0(var, "_final")
  
  if (var %in% names(df_other_main_unmatched)) {
    df_other_main_unmatched[[final_var]] <- df_other_main_unmatched[[var]]
  }
}

# 8. AÑADIR CONCERNS1 Y RVS ABAJO
df_extra_append <- df_extra_append %>%
  mutate(
    external_surveys_used = NA_character_,
    external_idiomas_used = NA_character_,
    external_fechas_used = NA_character_,
    needs_external_data = NA,
    has_join_key = !is.na(join_key),
    has_previous_survey = NA_character_,
    problematic_merge = FALSE,
    merge_status = "extra_survey_appended"
  )

for (var in common_vars) {
  final_var <- paste0(var, "_final")
  
  if (var %in% names(df_extra_append)) {
    df_extra_append[[final_var]] <- df_extra_append[[var]]
  }
}


# 9. DATASET COMPLETO FINAL
all_cols <- reduce(
  list(
    names(df_merged),
    names(df_other_main_unmatched),
    names(df_extra_append)
  ),
  union
)

df_complete <- bind_rows(
  align_to_cols(df_merged, all_cols),
  align_to_cols(df_other_main_unmatched, all_cols),
  align_to_cols(df_extra_append, all_cols)
) %>%
  mutate(
    row_id_global = row_number(),
    year_of_birth = suppressWarnings(as.numeric(
      coalesce_optional_cols(
        .,
        c(
          "please_enter_your_year_of_birth_final",
          "please_enter_your_year_of_birth"
        )
      )
    )),
    age = if_else(
      !is.na(year_of_birth),
      as.integer(format(Sys.Date(), "%Y")) - year_of_birth,
      NA_real_
    )
  ) %>%
  relocate(row_id_global, source_survey, merge_status, idioma, fecha, join_key)


# 10. RESÚMENES DESCRIPTIVOS
summary_by_survey <- df_complete %>%
  count(source_survey, merge_status, name = "n") %>%
  arrange(source_survey, merge_status)

summary_by_language <- df_complete %>%
  count(idioma, name = "n") %>%
  mutate(percentage = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

summary_by_date <- df_complete %>%
  mutate(fecha_date = as.Date(fecha)) %>%
  count(fecha_date, source_survey, name = "n") %>%
  arrange(fecha_date, source_survey)

summary_merge <- df_complete %>%
  count(merge_status, name = "n") %>%
  mutate(percentage = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

identifier_summary <- df_complete %>%
  mutate(
    identifier_type = case_when(
      str_detect(coalesce(join_key, ""), "^PROLIFIC_") ~ "prolific",
      str_detect(coalesce(join_key, ""), "^CODE_") ~ "codigo",
      TRUE ~ "sin_identificador"
    )
  ) %>%
  count(source_survey, identifier_type, name = "n") %>%
  group_by(source_survey) %>%
  mutate(percentage = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  arrange(source_survey, desc(n))



# 11. GRÁFICOS DESCRIPTIVOS
plot_by_survey <- df_complete %>%
  count(source_survey, name = "n") %>%
  ggplot(aes(x = reorder(source_survey, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Number of responses by survey",
    x = "Survey",
    y = "Number of responses"
  ) +
  theme_minimal()

plot_by_language <- df_complete %>%
  count(idioma, name = "n") %>%
  filter(!is.na(idioma)) %>%
  ggplot(aes(x = reorder(idioma, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Number of responses by language",
    x = "Language",
    y = "Number of responses"
  ) +
  theme_minimal()

plot_by_date <- df_complete %>%
  mutate(fecha_date = as.Date(fecha)) %>%
  filter(!is.na(fecha_date)) %>%
  count(fecha_date, source_survey, name = "n") %>%
  ggplot(aes(x = fecha_date, y = n, fill = source_survey)) +
  geom_col() +
  labs(
    title = "Responses by date and survey",
    x = "Response date",
    y = "Number of responses",
    fill = "Survey"
  ) +
  theme_minimal()

plot_merge_status <- df_complete %>%
  count(merge_status, name = "n") %>%
  ggplot(aes(x = reorder(merge_status, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Merge status",
    x = "Merge status",
    y = "Number of rows"
  ) +
  theme_minimal()

ggsave(file.path(plots_dir, "responses_by_survey.png"), plot_by_survey, width = 8, height = 5, dpi = 300)
ggsave(file.path(plots_dir, "responses_by_language.png"), plot_by_language, width = 8, height = 5, dpi = 300)
ggsave(file.path(plots_dir, "responses_by_date.png"), plot_by_date, width = 9, height = 5, dpi = 300)
ggsave(file.path(plots_dir, "merge_status.png"), plot_merge_status, width = 8, height = 5, dpi = 300)

ggsave(file.path(plots_dir, "responses_by_survey.pdf"), plot_by_survey, width = 8, height = 5)
ggsave(file.path(plots_dir, "responses_by_language.pdf"), plot_by_language, width = 8, height = 5)
ggsave(file.path(plots_dir, "responses_by_date.pdf"), plot_by_date, width = 9, height = 5)
ggsave(file.path(plots_dir, "merge_status.pdf"), plot_merge_status, width = 8, height = 5)


# 12. GUARDAR RESULTADOS
write_csv(
  df_all_raw,
  file.path(csv_dir, "df_all_surveys_raw_with_language_date.csv")
)

write_csv(
  df_merged,
  file.path(csv_dir, "df_decision_consolidated_with_language_date.csv")
)

write_csv(
  df_complete,
  file.path(csv_dir, "df_all_surveys_complete_with_language_date.csv")
)

write_csv(
  summary_by_survey,
  file.path(logs_dir, "summary_by_survey.csv")
)

write_csv(
  summary_by_language,
  file.path(logs_dir, "summary_by_language.csv")
)

write_csv(
  summary_by_date,
  file.path(logs_dir, "summary_by_date.csv")
)

write_csv(
  summary_merge,
  file.path(logs_dir, "summary_merge_status.csv")
)

write_csv(
  identifier_summary,
  file.path(logs_dir, "identifier_summary_by_source.csv")
)

write_csv(
  conflict_rows,
  file.path(logs_dir, "conflict_rows_with_language_date.csv")
)

write_csv(
  conflict_summary,
  file.path(logs_dir, "conflict_summary_with_language_date.csv")
)



# 13. COMPROBACIONES EN CONSOLA
cat("\nDataset RAW leído:\n")
print(dim(df_all_raw))

cat("\nDataset Decision consolidado:\n")
print(dim(df_merged))

cat("\nDataset completo final:\n")
print(dim(df_complete))

cat("\nResumen por encuesta:\n")
print(summary_by_survey)

cat("\nResumen por idioma:\n")
print(summary_by_language)

cat("\nResumen merge:\n")
print(summary_merge)

cat("\nConflictos por variable:\n")
print(conflict_summary)

cat("\nArchivos principales guardados:\n")
cat("- ", file.path(csv_dir, "df_all_surveys_raw_with_language_date.csv"), "\n")
cat("- ", file.path(csv_dir, "df_decision_consolidated_with_language_date.csv"), "\n")
cat("- ", file.path(csv_dir, "df_all_surveys_complete_with_language_date.csv"), "\n")
cat("- Gráficos en: ", plots_dir, "\n")