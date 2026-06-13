
# SCRIPT 19 - BOOTSTRAP POLITICAL PROFILE 3 BLOCKS

# Objetivo:
#   Generar 1000 muestras bootstrap estratificadas por perfil político:
#     Left     = 13.76%
#     Moderate = 34.16%
#     Right    = 52.08%
#
# Cambio respecto a la versión anterior:
#   - Se unen Conservative + Authoritarian en un único bloque Right.
#   - Esto evita sobrerrepresentar artificialmente el grupo Authoritarian,
#     que tenía muy pocos casos reales.
#
# Importante:
#   - Se usa SOLO la escala política 0-100.
#   - No se usa abstención como estrato.
#   - El remuestreo se hace a nivel participante.


library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(tibble)

set.seed(123)


# 1. CONFIGURACIÓN
input_file <- "initial_descriptive_analysis/output/clean_datasets/df_clean_general.csv"

output_dir <- "initial_descriptive_analysis/output/bootstrap_political_profile"
diagnostics_dir <- file.path(output_dir, "diagnostics")
draws_dir <- file.path(output_dir, "draws")
examples_dir <- file.path(output_dir, "examples")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(draws_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(examples_dir, recursive = TRUE, showWarnings = FALSE)

n_boot <- 1000

spectrum_col <- "on_a_scale_from_0_to_100_where_0_means_most_left_and_100_means_most_right_where_would_you_place_yourself_politically_final"

id_candidates <- c(
  "participant_id",
  "join_key",
  "prolific_id",
  "Prolific_ID",
  "identifier",
  "response_id"
)

# 2. FUNCIONES
clean_political_bootstrap <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  
  case_when(
    is.na(x) ~ NA_character_,
    x < 40 ~ "Left",
    x >= 40 & x <= 60 ~ "Moderate",
    x > 60 ~ "Right",
    TRUE ~ NA_character_
  )
}


# 3. CARGAR DATOS
df <- read_csv(input_file, show_col_types = FALSE)

cat("Datos cargados\n")
cat("Filas:", nrow(df), "\n")
cat("Columnas:", ncol(df), "\n\n")

if (!spectrum_col %in% names(df)) {
  stop("No existe la columna de escala política. Revisa spectrum_col.")
}

existing_ids <- id_candidates[id_candidates %in% names(df)]

if (length(existing_ids) > 0) {
  id_col <- existing_ids[1]
  df <- df %>%
    mutate(participant_id = as.character(.data[[id_col]]))
  cat("ID usado:", id_col, "\n\n")
} else {
  df <- df %>%
    mutate(participant_id = as.character(row_number()))
  cat("No se encontró ID. Se ha creado participant_id con row_number().\n\n")
}


# 4. CREAR BLOQUE POLÍTICO
df <- df %>%
  mutate(
    political_spectrum = suppressWarnings(as.numeric(.data[[spectrum_col]])),
    political_profile = clean_political_bootstrap(political_spectrum)
  )

participant_political <- df %>%
  distinct(participant_id, political_spectrum, political_profile) %>%
  filter(!is.na(political_profile))

cat("Participantes con escala política válida:", nrow(participant_political), "\n\n")


# 5. DISTRIBUCIÓN OBSERVADA
observed_distribution <- participant_political %>%
  count(political_profile, name = "n_observed") %>%
  mutate(
    prop_observed = n_observed / sum(n_observed)
  ) %>%
  arrange(political_profile)

cat("Distribución observada:\n")
print(observed_distribution)

write_csv(
  observed_distribution,
  file.path(diagnostics_dir, "observed_political_distribution.csv")
)

# 6. DISTRIBUCIÓN OBJETIVO
target_distribution <- tibble(
  political_profile = c(
    "Left",
    "Moderate",
    "Right"
  ),
  target_prop = c(
    0.1376,
    0.3416,
    0.5208
  )
)

if (abs(sum(target_distribution$target_prop) - 1) > 0.001) {
  stop("Los pesos objetivo no suman 1.")
}

target_vs_observed <- target_distribution %>%
  left_join(observed_distribution, by = "political_profile") %>%
  mutate(
    n_observed = replace_na(n_observed, 0),
    prop_observed = replace_na(prop_observed, 0),
    difference_target_minus_observed = target_prop - prop_observed
  )

cat("\nDistribución objetivo vs observada:\n")
print(target_vs_observed)

write_csv(
  target_vs_observed,
  file.path(diagnostics_dir, "target_vs_observed_political_distribution.csv")
)

# 7. CALCULAR N OBJETIVO POR GRUPO
n_target <- nrow(participant_political)

target_counts <- target_distribution %>%
  mutate(
    n_target_raw = target_prop * n_target,
    n_target = floor(n_target_raw)
  )

remaining <- n_target - sum(target_counts$n_target)

if (remaining > 0) {
  target_counts <- target_counts %>%
    mutate(decimal_part = n_target_raw - n_target) %>%
    arrange(desc(decimal_part)) %>%
    mutate(
      n_target = n_target + if_else(row_number() <= remaining, 1L, 0L)
    ) %>%
    arrange(political_profile) %>%
    select(-decimal_part)
}

cat("\nN objetivo por muestra bootstrap:\n")
print(target_counts)

write_csv(
  target_counts,
  file.path(diagnostics_dir, "target_counts_political_profile.csv")
)


# 8. FUNCIÓN BOOTSTRAP
make_bootstrap_draws <- function(boot_id) {
  
  target_counts %>%
    select(political_profile, n_target) %>%
    pmap_dfr(function(political_profile, n_target) {
      
      available_ids <- participant_political %>%
        filter(political_profile == !!political_profile) %>%
        pull(participant_id)
      
      if (length(available_ids) == 0) {
        stop(paste("No hay participantes para:", political_profile))
      }
      
      tibble(
        bootstrap_id = boot_id,
        political_profile  = political_profile,
        participant_id_original = sample(
          available_ids,
          size = n_target,
          replace = TRUE
        )
      )
    }) %>%
    mutate(
      bootstrap_draw_id = row_number(),
      participant_id_bootstrap = paste0(
        participant_id_original,
        "_boot",
        bootstrap_id,
        "_draw",
        bootstrap_draw_id
      )
    )
}


# 9. GENERAR BOOTSTRAPS
all_draws <- map_dfr(seq_len(n_boot), function(b) {
  cat("Generando bootstrap", b, "de", n_boot, "\n")
  make_bootstrap_draws(b)
})

write_csv(
  all_draws,
  file.path(draws_dir, "bootstrap_political_profile_draws.csv")
)


# 10. DIAGNÓSTICOS
bootstrap_diagnostics <- all_draws %>%
  count(bootstrap_id, political_profile, name = "n") %>%
  group_by(bootstrap_id) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

write_csv(
  bootstrap_diagnostics,
  file.path(diagnostics_dir, "bootstrap_political_profile_diagnostics.csv")
)

bootstrap_summary <- bootstrap_diagnostics %>%
  group_by(political_profile) %>%
  summarise(
    mean_n = mean(n),
    sd_n = sd(n),
    mean_prop = mean(prop),
    sd_prop = sd(prop),
    .groups = "drop"
  ) %>%
  left_join(target_distribution, by = "political_profile") %>%
  mutate(
    difference_mean_target = mean_prop - target_prop
  )

cat("\nResumen final bootstrap:\n")
print(bootstrap_summary)

write_csv(
  bootstrap_summary,
  file.path(diagnostics_dir, "bootstrap_political_profile_summary.csv")
)

# 11. FUNCIÓN PARA MATERIALIZAR UNA MUESTRA COMPLETA
get_bootstrap_dataset <- function(boot_id, original_df = df, draws_df = all_draws) {
  
  selected_draws <- draws_df %>%
    filter(bootstrap_id == boot_id)
  
  boot_df <- selected_draws %>%
    left_join(
      original_df,
      by = c(
        "participant_id_original" = "participant_id",
        "political_profile" = "political_profile"
      )
    ) %>%
    mutate(
      participant_id_original = participant_id_original,
      participant_id = participant_id_bootstrap
    ) %>%
    select(
      bootstrap_id,
      bootstrap_draw_id,
      participant_id,
      participant_id_original,
      everything(),
      -participant_id_bootstrap
    )
  
  return(boot_df)
}

boot_1 <- get_bootstrap_dataset(1)

write_csv(
  boot_1,
  file.path(examples_dir, "example_bootstrap_sample_1.csv")
)

# 12. DIAGNÓSTICO DE PARTICIPANTES ÚNICOS POR BOOTSTRAP
unique_participants_summary <- all_draws %>%
  group_by(bootstrap_id, political_profile) %>%
  summarise(
    n_draws = n(),
    n_unique_original = n_distinct(participant_id_original),
    .groups = "drop"
  ) %>%
  group_by(political_profile) %>%
  summarise(
    mean_unique_original = mean(n_unique_original),
    min_unique_original = min(n_unique_original),
    max_unique_original = max(n_unique_original),
    .groups = "drop"
  )

cat("\nResumen participantes únicos por bloque político:\n")
print(unique_participants_summary)

write_csv(
  unique_participants_summary,
  file.path(diagnostics_dir, "unique_original_participants_by_bootstrap.csv")
)

cat("Draws guardados en:", draws_dir, "\n")
cat("Diagnósticos guardados en:", diagnostics_dir, "\n")
cat("Ejemplo guardado en:", examples_dir, "\n")