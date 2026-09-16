
# Objetivo:
# Preparar la variable de ingreso anual de WHY_LATAM y diagnosticar
# su distribución por país.
#
# Se generan distribuciones:
# - sobre todas las respuestas,
# - solo sobre ingresos válidos,
# - solo sobre ingresos válidos de personas usables para clustering.

suppressPackageStartupMessages({
  library(tidyverse)
})

# Configuración
project_root <- path.expand(
  "~/Desktop/MASTER/recommendation-engine/TFM"
)

processed_root <- file.path(
  project_root,
  "paper1_cluster/data/processed"
)

# Dataset integrado procedente del 01.
integrated_file <- file.path(
  processed_root,
  "01_mergeData",
  "all_sources_integrated.csv"
)

# IDs que han superado los criterios de calidad para clustering.
clustering_file <- file.path(
  processed_root,
  "03_component_quality",
  "matrix_32det_for_clustering.csv"
)

out_dir <- file.path(
  processed_root,
  "04_latam_income_bootstrap"
)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# Lectura
if (!file.exists(integrated_file)) {
  stop(
    "No encuentro el dataset integrado: ",
    integrated_file
  )
}

if (!file.exists(clustering_file)) {
  stop(
    "No encuentro la matriz de clustering: ",
    clustering_file
  )
}

df <- read_csv(
  integrated_file,
  show_col_types = FALSE
)

clustering_ids <- read_csv(
  clustering_file,
  show_col_types = FALSE
) %>%
  transmute(
    integrated_row_id = as.character(
      integrated_row_id
    )
  ) %>%
  distinct()

if (!nrow(clustering_ids)) {
  stop(
    "La matriz de clustering no contiene IDs."
  )
}


# Variable original de ingreso anual de WHY.
income_col <-
  "why__what_is_your_approximate_individual_yearly_income_neat_id220"

if (!income_col %in% names(df)) {
  stop(
    "No encuentro la variable de ingreso WHY."
  )
}

# Tramos que se consideran respuestas válidas de ingreso.
INCOME_BANDS <- c(
  "<15000",
  "15000_30000",
  "30000_50000",
  "50000_100000",
  ">100000"
)


# Preparar WHY_LATAM
latam_income <- df %>%
  mutate(
    integrated_row_id = as.character(
      integrated_row_id
    )
  ) %>%
  filter(
    subsample == "WHY_LATAM"
  ) %>%
  mutate(
    # Conservamos la respuesta original de la encuesta.
    income_yearly_raw =
      .data[[income_col]],
    
    # País de residencia.
    country = case_when(
      country_code == "MX" ~ "Mexico",
      country_code == "CL" ~ "Chile",
      country_code == "CO" ~ "Colombia",
      TRUE ~ country_code
    ),
    
    # Recodificación a los cinco tramos exactos utilizados
    # posteriormente en el bootstrap económico.
    income_band = case_when(
      str_detect(
        income_yearly_raw,
        fixed("Less than 15.000")
      ) ~
        "<15000",
      
      str_detect(
        income_yearly_raw,
        fixed("Between 15.000")
      ) ~
        "15000_30000",
      
      str_detect(
        income_yearly_raw,
        fixed("Between 30.000")
      ) ~
        "30000_50000",
      
      str_detect(
        income_yearly_raw,
        fixed("Between 50.000")
      ) ~
        "50000_100000",
      
      str_detect(
        income_yearly_raw,
        fixed("More than 100.000")
      ) ~
        ">100000",
      
      str_detect(
        income_yearly_raw,
        regex(
          "prefer not",
          ignore_case = TRUE
        )
      ) ~
        "NO_RESPONSE",
      
      is.na(
        income_yearly_raw
      ) ~
        "MISSING",
      
      TRUE ~
        "OTHER"
    )
  ) %>%
  left_join(
    clustering_ids %>%
      mutate(
        usable_for_clustering = TRUE
      ),
    by = "integrated_row_id"
  ) %>%
  mutate(
    usable_for_clustering = replace_na(
      usable_for_clustering,
      FALSE
    )
  )


# Distribución completa de ingreso por país.
# Incluye también NO_RESPONSE, MISSING y OTHER.
income_distribution <- latam_income %>%
  count(
    country_code,
    country,
    income_band,
    name = "n"
  ) %>%
  group_by(
    country_code,
    country
  ) %>%
  mutate(
    n_country = sum(n),
    
    prop_total_country =
      n /
      n_country
  ) %>%
  ungroup()


# Distribución únicamente entre respuestas válidas de ingreso.
income_valid_distribution <- latam_income %>%
  filter(
    income_band %in%
      INCOME_BANDS
  ) %>%
  count(
    country_code,
    country,
    income_band,
    name = "n_sample"
  ) %>%
  group_by(
    country_code,
    country
  ) %>%
  mutate(
    n_valid_income = sum(
      n_sample
    ),
    
    sample_prop =
      n_sample /
      n_valid_income
  ) %>%
  ungroup()


# La misma distribución restringida a personas que pueden utilizarse
# posteriormente en K-means/EFA.
income_valid_clustering <- latam_income %>%
  filter(
    usable_for_clustering,
    income_band %in%
      INCOME_BANDS
  ) %>%
  count(
    country_code,
    country,
    income_band,
    name = "n_sample_clustering"
  ) %>%
  group_by(
    country_code,
    country
  ) %>%
  mutate(
    n_valid_income_clustering = sum(
      n_sample_clustering
    ),
    
    sample_prop_clustering =
      n_sample_clustering /
      n_valid_income_clustering
  ) %>%
  ungroup()


# Guardado
outputs <- list(
  "latam_income_individual_level.csv" =
    latam_income,
  
  "latam_income_distribution_all.csv" =
    income_distribution,
  
  "latam_income_distribution_valid.csv" =
    income_valid_distribution,
  
  "latam_income_distribution_clustering_usable.csv" =
    income_valid_clustering
)

iwalk(
  outputs,
  ~ write_csv(
    .x,
    file.path(
      out_dir,
      .y
    )
  )
)


# Resumen en consola
cat("\nWHY_LATAM: INGRESO ANUAL POR PAÍS\n\n")

print(
  income_distribution,
  n = Inf,
  width = Inf
)

cat("\nSOLO INGRESOS VÁLIDOS\n\n")

print(
  income_valid_distribution,
  n = Inf,
  width = Inf
)

cat("\nINGRESOS VÁLIDOS + USABLE CLUSTERING\n\n")

print(
  income_valid_clustering,
  n = Inf,
  width = Inf
)

message(
  "\nListo. Outputs guardados en: ",
  out_dir
)