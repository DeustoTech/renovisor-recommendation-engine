
# SCRIPT 16 - CONSTRUIR 9 DIMENSIONES A PARTIR DE 32 DETERMINANTES

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)

set.seed(123)

# 1. CARPETAS

base_output_dir <- "initial_descriptive_analysis/output/model_5arq_9dim"

csv_dir <- file.path(base_output_dir, "csv")
bootstrap_dir <- file.path(base_output_dir, "bootstrap")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(bootstrap_dir, recursive = TRUE, showWarnings = FALSE)

# 2. CARGAR DATOS DEL SCRIPT 15
synthetic_path <- file.path(csv_dir, "synthetic_32det_5arq.csv")
dictionary_path <- file.path(csv_dir, "determinant_dimension_dictionary.csv")
bootstrap_index_path <- file.path(bootstrap_dir, "bootstrap_participant_index.csv")

df_32det <- read_csv(
  synthetic_path,
  show_col_types = FALSE
)

dimension_dictionary <- read_csv(
  dictionary_path,
  show_col_types = FALSE
)

bootstrap_index <- read_csv(
  bootstrap_index_path,
  show_col_types = FALSE
)

cat("Datos cargados correctamente\n")
cat("Filas dataset 32 determinantes:", nrow(df_32det), "\n")
cat("Participantes:", n_distinct(df_32det$participant_id), "\n")


# 3. VALIDACIONES BÁSICAS

required_metadata_cols <- c(
  "row_id",
  "participant_id",
  "decision_id",
  "technology",
  "original_archetype",
  "macro_archetype_5",
  "adoption_stage",
  "response_style_latent"
)

missing_metadata_cols <- setdiff(required_metadata_cols, names(df_32det))

if (length(missing_metadata_cols) > 0) {
  stop(
    "Faltan columnas de metadatos en synthetic_32det_5arq.csv: ",
    paste(missing_metadata_cols, collapse = ", ")
  )
}

determinants <- dimension_dictionary$determinant
dimensions <- unique(dimension_dictionary$dimension)

missing_determinants <- setdiff(determinants, names(df_32det))

if (length(missing_determinants) > 0) {
  stop(
    "Faltan determinantes en synthetic_32det_5arq.csv: ",
    paste(missing_determinants, collapse = ", ")
  )
}

if (length(dimensions) != 9) {
  warning(
    "El diccionario no contiene exactamente 9 dimensiones. Dimensiones detectadas: ",
    paste(dimensions, collapse = ", ")
  )
}

cat("Determinantes detectados:", length(determinants), "\n")
cat("Dimensiones detectadas:", length(dimensions), "\n")
print(dimensions)

# 4. PASAR A FORMATO LARGO: UNA FILA POR DETERMINANTE
df_det_long <- df_32det %>%
  select(
    all_of(required_metadata_cols),
    all_of(determinants)
  ) %>%
  pivot_longer(
    cols = all_of(determinants),
    names_to = "determinant",
    values_to = "determinant_value"
  ) %>%
  left_join(
    dimension_dictionary,
    by = "determinant"
  )

# Comprobar que todos los determinantes tienen dimensión
missing_dimension_rows <- df_det_long %>%
  filter(is.na(dimension))

if (nrow(missing_dimension_rows) > 0) {
  stop("Hay determinantes sin dimensión asignada en el diccionario.")
}

write_csv(
  df_det_long,
  file.path(csv_dir, "synthetic_32det_long_with_dimensions.csv")
)

cat("Dataset largo de determinantes guardado\n")


# 5. CALCULAR LAS 9 DIMENSIONES
df_dim_long <- df_det_long %>%
  group_by(
    across(all_of(required_metadata_cols)),
    dimension
  ) %>%
  summarise(
    dimension_score = mean(determinant_value, na.rm = TRUE),
    n_determinants_in_dimension = n(),
    .groups = "drop"
  )

write_csv(
  df_dim_long,
  file.path(csv_dir, "synthetic_9dim_long.csv")
)

df_9dim <- df_dim_long %>%
  select(
    all_of(required_metadata_cols),
    dimension,
    dimension_score
  ) %>%
  pivot_wider(
    names_from = dimension,
    values_from = dimension_score
  ) %>%
  arrange(row_id)

# 6. REORDENAR Y VALIDAR DATASET FINAL
stage_levels <- c("Knowledge", "Considering", "Done")

df_9dim <- df_9dim %>%
  mutate(
    adoption_stage = ordered(
      adoption_stage,
      levels = stage_levels
    ),
    macro_archetype_5 = factor(macro_archetype_5),
    original_archetype = factor(original_archetype),
    technology = factor(technology)
  ) %>%
  select(
    row_id,
    participant_id,
    decision_id,
    technology,
    original_archetype,
    macro_archetype_5,
    adoption_stage,
    response_style_latent,
    all_of(dimensions)
  )

# Comprobaciones
if (nrow(df_9dim) != nrow(df_32det)) {
  stop("El número de filas del dataset con dimensiones no coincide con el original.")
}

missing_dimension_cols <- setdiff(dimensions, names(df_9dim))

if (length(missing_dimension_cols) > 0) {
  stop(
    "Faltan dimensiones en df_9dim: ",
    paste(missing_dimension_cols, collapse = ", ")
  )
}

na_dimension_values <- df_9dim %>%
  select(all_of(dimensions)) %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(
    everything(),
    names_to = "dimension",
    values_to = "n_na"
  )

if (any(na_dimension_values$n_na > 0)) {
  warning("Hay valores NA en alguna dimensión.")
  print(na_dimension_values)
}

# Guardar dataset final
write_csv(
  df_9dim,
  file.path(csv_dir, "synthetic_5arq_9dim.csv")
)

cat("Dataset 5 arquetipos x 9 dimensiones guardado\n")


# 7. RESÚMENES DE CONTROL
dimension_summary_global <- df_9dim %>%
  summarise(
    across(
      all_of(dimensions),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "metric",
    values_to = "value"
  )

write_csv(
  dimension_summary_global,
  file.path(csv_dir, "dimension_summary_global.csv")
)

dimension_summary_by_group <- df_9dim %>%
  group_by(macro_archetype_5, adoption_stage) %>%
  summarise(
    across(
      all_of(dimensions),
      ~ mean(.x, na.rm = TRUE),
      .names = "mean_{.col}"
    ),
    n_rows = n(),
    n_participants = n_distinct(participant_id),
    .groups = "drop"
  )

write_csv(
  dimension_summary_by_group,
  file.path(csv_dir, "dimension_summary_by_macro_archetype_stage.csv")
)

stage_distribution <- df_9dim %>%
  count(macro_archetype_5, adoption_stage, name = "n_rows") %>%
  group_by(macro_archetype_5) %>%
  mutate(
    percentage_within_macro = n_rows / sum(n_rows) * 100
  ) %>%
  ungroup()

write_csv(
  stage_distribution,
  file.path(csv_dir, "stage_distribution_9dim.csv")
)

print(stage_distribution, n = Inf)


# 8. CREAR DATASET BOOTSTRAP CON 9 DIMENSIONES
# El índice bootstrap viene del Script 15.
# Cada participante remuestreado se expande a todas sus decisiones.
# Si un participante aparece varias veces, se le asigna un nuevo bootstrap_participant_id.

df_bootstrap_9dim <- bootstrap_index %>%
  left_join(
    df_9dim,
    by = c("original_participant_id" = "participant_id"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    original_participant_id = original_participant_id,
    participant_id = bootstrap_participant_id
  ) %>%
  select(
    n_participants,
    boot_id,
    bootstrap_position,
    participant_id,
    original_participant_id,
    row_id,
    decision_id,
    technology,
    original_archetype,
    macro_archetype_5,
    adoption_stage,
    response_style_latent,
    all_of(dimensions)
  ) %>%
  arrange(
    n_participants,
    boot_id,
    bootstrap_position,
    decision_id
  )

write_csv(
  df_bootstrap_9dim,
  file.path(bootstrap_dir, "bootstrap_5arq_9dim_all.csv")
)

cat("Dataset bootstrap con 9 dimensiones guardado\n")


# 9. VALIDAR BOOTSTRAP
bootstrap_validation <- df_bootstrap_9dim %>%
  group_by(n_participants, boot_id) %>%
  summarise(
    n_rows = n(),
    n_bootstrap_participants = n_distinct(participant_id),
    n_original_participants = n_distinct(original_participant_id),
    n_decisions = n_distinct(decision_id),
    expected_rows = first(n_participants) * n_distinct(decision_id),
    ok_rows = n_rows == expected_rows,
    .groups = "drop"
  )

write_csv(
  bootstrap_validation,
  file.path(bootstrap_dir, "bootstrap_5arq_9dim_validation.csv")
)

print(bootstrap_validation, n = Inf)

if (any(!bootstrap_validation$ok_rows)) {
  warning("Alguna muestra bootstrap no tiene el número esperado de filas.")
}

bootstrap_group_summary <- df_bootstrap_9dim %>%
  count(
    n_participants,
    boot_id,
    macro_archetype_5,
    adoption_stage,
    name = "n_rows"
  ) %>%
  group_by(n_participants, boot_id, macro_archetype_5) %>%
  mutate(
    percentage_within_macro = n_rows / sum(n_rows) * 100
  ) %>%
  ungroup()

write_csv(
  bootstrap_group_summary,
  file.path(bootstrap_dir, "bootstrap_5arq_9dim_group_summary.csv")
)


# 10. COMPROBACIÓN FINAL DE RANGOS
dimension_ranges <- df_9dim %>%
  summarise(
    across(
      all_of(dimensions),
      list(
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "metric",
    values_to = "value"
  )

write_csv(
  dimension_ranges,
  file.path(csv_dir, "dimension_ranges.csv")
)

print(dimension_ranges, n = Inf)

cat("Archivos principales creados:\n")
cat("- synthetic_5arq_9dim.csv\n")
cat("- synthetic_9dim_long.csv\n")
cat("- synthetic_32det_long_with_dimensions.csv\n")
cat("- dimension_summary_global.csv\n")
cat("- dimension_summary_by_macro_archetype_stage.csv\n")
cat("- bootstrap_5arq_9dim_all.csv\n")
cat("- bootstrap_5arq_9dim_validation.csv\n")
cat("- bootstrap_5arq_9dim_group_summary.csv\n")