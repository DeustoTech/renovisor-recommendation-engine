#
# Objetivo
#
# Combinar el bootstrap político/electoral de EUROPE (04_2b)
# con el bootstrap económico de LATAM (04_2e).
#
# La combinación se realiza por bootstrap_id:
# bootstrap 1 = EUROPE 1 + LATAM 1
# bootstrap 2 = EUROPE 2 + LATAM 2
# ...
#
# IMPORTANTE:
# - No se vuelve a remuestrear.
# - Se conservan exactamente los draws de cada región.
# - EUROPE mantiene su corrección política.
# - LATAM mantiene su corrección económica.
# - COMPLETE se obtiene apilando ambos bootstraps.

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

# Bootstrap político europeo generado en 04_2b.
eu_file <- file.path(
  processed_root,
  "04_2b_propensity_bootstrap_eu_pfe_esn_renew_greens_merged_clustering_usable",
  "bootstrap_samples_index.csv"
)

# Bootstrap económico LATAM generado en 04_2e.
latam_file <- file.path(
  processed_root,
  "04_2e_latam_income_bootstrap",
  "bootstrap_samples_index_latam_income.csv"
)

# Matriz maestra de personas utilizables para clustering.
# Se utiliza también para recuperar región, submuestra y fuente.
clustering_meta_file <- file.path(
  processed_root,
  "03_component_quality",
  "matrix_32det_for_clustering.csv"
)

out_dir <- file.path(
  processed_root,
  "04_2f_combine_eu_latam_bootstrap"
)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# El índice combinado se guarda comprimido porque puede superar
# ampliamente el millón de filas.
output_file <- file.path(
  out_dir,
  "bootstrap_samples_index.csv.gz"
)

EXPECTED_N_BOOT <- 1000L
EXPECTED_EU_SIZE <- 1000L

COMBINATION_MODE <-
  "stack_europe_and_latam_preserving_region_bootstrap_sizes"


# Comprobar archivos
required_files <- c(
  eu_file,
  latam_file,
  clustering_meta_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files)) {
  stop(
    "Faltan archivos necesarios:\n",
    paste(
      missing_files,
      collapse = "\n"
    )
  )
}


# Funciones auxiliares
optional_chr_col <- function(
    data,
    col
) {
  if (col %in% names(data)) {
    as.character(data[[col]])
  } else {
    rep(
      NA_character_,
      nrow(data)
    )
  }
}


# Leer bootstrap EUROPE
eu_raw <- read_csv(
  eu_file,
  col_select = any_of(
    c(
      "bootstrap_id",
      "draw_id",
      "integrated_row_id",
      "target_sample_type",
      "target_electoral_group"
    )
  ),
  show_col_types = FALSE
)

required_eu_cols <- c(
  "bootstrap_id",
  "draw_id",
  "integrated_row_id"
)

missing_eu_cols <- setdiff(
  required_eu_cols,
  names(eu_raw)
)

if (length(missing_eu_cols)) {
  stop(
    "Faltan columnas en bootstrap Europa: ",
    paste(
      missing_eu_cols,
      collapse = ", "
    )
  )
}


# Leer bootstrap LATAM
latam_raw <- read_csv(
  latam_file,
  col_select = any_of(
    c(
      "bootstrap_id",
      "draw_id",
      "integrated_row_id",
      "country_code",
      "country",
      "income_band"
    )
  ),
  show_col_types = FALSE
)

required_latam_cols <- c(
  "bootstrap_id",
  "draw_id",
  "integrated_row_id"
)

missing_latam_cols <- setdiff(
  required_latam_cols,
  names(latam_raw)
)

if (length(missing_latam_cols)) {
  stop(
    "Faltan columnas en bootstrap LATAM: ",
    paste(
      missing_latam_cols,
      collapse = ", "
    )
  )
}


# Leer metadata de clustering
metadata <- read_csv(
  clustering_meta_file,
  col_select = any_of(
    c(
      "integrated_row_id",
      "comparison_region",
      "subsample",
      "dataset_source",
      "source_survey"
    )
  ),
  show_col_types = FALSE
) %>%
  mutate(
    integrated_row_id = as.character(
      integrated_row_id
    )
  )

required_meta_cols <- c(
  "integrated_row_id",
  "comparison_region",
  "subsample",
  "dataset_source"
)

missing_meta_cols <- setdiff(
  required_meta_cols,
  names(metadata)
)

if (length(missing_meta_cols)) {
  stop(
    "Faltan columnas en matrix_32det_for_clustering.csv: ",
    paste(
      missing_meta_cols,
      collapse = ", "
    )
  )
}

metadata_duplicates <- metadata %>%
  count(
    integrated_row_id,
    name = "n"
  ) %>%
  filter(
    n > 1
  )

if (nrow(metadata_duplicates)) {
  print(
    metadata_duplicates,
    n = Inf
  )
  
  stop(
    "Hay integrated_row_id duplicados en la matriz de clustering."
  )
}


# Preparar EUROPE
eu <- eu_raw %>%
  transmute(
    bootstrap_id = as.integer(
      bootstrap_id
    ),
    
    region_draw_id = as.integer(
      draw_id
    ),
    
    integrated_row_id = as.character(
      integrated_row_id
    ),
    
    target_sample_type = optional_chr_col(
      eu_raw,
      "target_sample_type"
    ),
    
    target_electoral_group = optional_chr_col(
      eu_raw,
      "target_electoral_group"
    )
  ) %>%
  left_join(
    metadata,
    by = "integrated_row_id"
  ) %>%
  mutate(
    bootstrap_source =
      "EU_POLITICAL",
    
    country_code =
      NA_character_,
    
    country =
      NA_character_,
    
    income_band =
      NA_character_
  )


# Preparar LATAM
latam <- latam_raw %>%
  transmute(
    bootstrap_id = as.integer(
      bootstrap_id
    ),
    
    region_draw_id = as.integer(
      draw_id
    ),
    
    integrated_row_id = as.character(
      integrated_row_id
    ),
    
    country_code = optional_chr_col(
      latam_raw,
      "country_code"
    ),
    
    country = optional_chr_col(
      latam_raw,
      "country"
    ),
    
    income_band = optional_chr_col(
      latam_raw,
      "income_band"
    )
  ) %>%
  left_join(
    metadata,
    by = "integrated_row_id"
  ) %>%
  mutate(
    bootstrap_source =
      "LATAM_INCOME",
    
    target_sample_type =
      "LATAM_INCOME_BOOTSTRAP",
    
    target_electoral_group =
      NA_character_
  )


# Validar que cada bootstrap contiene exclusivamente la región correcta.
eu_wrong_region <- eu %>%
  filter(
    is.na(comparison_region) |
      comparison_region != "EUROPE"
  )

if (nrow(eu_wrong_region)) {
  print(
    eu_wrong_region %>%
      count(
        comparison_region,
        subsample,
        dataset_source
      ),
    n = Inf
  )
  
  stop(
    "ERROR: el bootstrap europeo contiene IDs que no pertenecen a EUROPE."
  )
}

latam_wrong_region <- latam %>%
  filter(
    is.na(comparison_region) |
      comparison_region != "LATAM"
  )

if (nrow(latam_wrong_region)) {
  print(
    latam_wrong_region %>%
      count(
        comparison_region,
        subsample,
        dataset_source
      ),
    n = Inf
  )
  
  stop(
    "ERROR: el bootstrap LATAM contiene IDs que no pertenecen a LATAM."
  )
}

# En la arquitectura actual LATAM debe contener exclusivamente WHY_LATAM.
latam_wrong_subsample <- latam %>%
  filter(
    is.na(subsample) |
      subsample != "WHY_LATAM"
  )

if (nrow(latam_wrong_subsample)) {
  stop(
    "ERROR: el bootstrap LATAM contiene filas que no son WHY_LATAM."
  )
}


# Comprobar bootstrap_id
eu_boot_ids <- sort(
  unique(
    eu$bootstrap_id
  )
)

latam_boot_ids <- sort(
  unique(
    latam$bootstrap_id
  )
)

if (
  !identical(
    eu_boot_ids,
    latam_boot_ids
  )
) {
  stop(
    "Europa y LATAM no tienen exactamente los mismos bootstrap_id."
  )
}

if (
  length(eu_boot_ids) !=
  EXPECTED_N_BOOT
) {
  stop(
    "Se esperaban ",
    EXPECTED_N_BOOT,
    " bootstraps y hay ",
    length(eu_boot_ids),
    "."
  )
}


# Comprobar tamaños regionales.
#
# EUROPE debe mantener el tamaño fijado en 04_2b.
# LATAM debe mantener un tamaño constante entre sus réplicas.
eu_sizes <- eu %>%
  count(
    bootstrap_id,
    name = "n_europe"
  )

if (
  n_distinct(
    eu_sizes$n_europe
  ) != 1
) {
  stop(
    "El tamaño del bootstrap europeo no es constante."
  )
}

EU_SIZE <- unique(
  eu_sizes$n_europe
)

if (EU_SIZE != EXPECTED_EU_SIZE) {
  stop(
    "Europa debería tener ",
    EXPECTED_EU_SIZE,
    " draws por bootstrap y tiene ",
    EU_SIZE,
    "."
  )
}

latam_sizes <- latam %>%
  count(
    bootstrap_id,
    name = "n_latam"
  )

if (
  n_distinct(
    latam_sizes$n_latam
  ) != 1
) {
  stop(
    "El tamaño del bootstrap LATAM no es constante."
  )
}

LATAM_SIZE <- unique(
  latam_sizes$n_latam
)

EXPECTED_TOTAL_SIZE <-
  EU_SIZE +
  LATAM_SIZE


# Combinar EUROPE + LATAM.
#
# bind_rows() no genera ningún draw nuevo.
# Las filas con el mismo bootstrap_id forman conjuntamente una
# réplica COMPLETE.
#
# Se conserva region_draw_id, que identifica la posición original
# dentro del bootstrap regional, y se crea un nuevo draw_id para COMPLETE.
combined <- bind_rows(
  eu,
  latam
) %>%
  arrange(
    bootstrap_id,
    factor(
      comparison_region,
      levels = c(
        "EUROPE",
        "LATAM"
      )
    ),
    region_draw_id
  ) %>%
  group_by(
    bootstrap_id
  ) %>%
  mutate(
    draw_id = row_number()
  ) %>%
  ungroup() %>%
  select(
    bootstrap_id,
    draw_id,
    region_draw_id,
    integrated_row_id,
    comparison_region,
    subsample,
    dataset_source,
    any_of("source_survey"),
    bootstrap_source,
    target_sample_type,
    target_electoral_group,
    country_code,
    country,
    income_band
  )


# Validar tamaño de cada bootstrap COMPLETE.
combined_size_check <- combined %>%
  count(
    bootstrap_id,
    name = "n_total"
  ) %>%
  mutate(
    expected =
      EXPECTED_TOTAL_SIZE,
    
    ok =
      n_total ==
      expected
  )

if (any(!combined_size_check$ok)) {
  print(
    combined_size_check %>%
      filter(
        !ok
      ),
    n = Inf
  )
  
  stop(
    "Algún bootstrap combinado no tiene el tamaño esperado."
  )
}


# Confirmar que todos los draws pertenecen a personas utilizables
# para clustering.
clustering_ids <- metadata$integrated_row_id

id_check <- combined %>%
  mutate(
    exists_in_clustering_matrix =
      integrated_row_id %in%
      clustering_ids
  ) %>%
  group_by(
    bootstrap_id
  ) %>%
  summarise(
    n_draws = n(),
    
    n_in_matrix = sum(
      exists_in_clustering_matrix
    ),
    
    n_missing =
      n_draws -
      n_in_matrix,
    
    prop_in_matrix =
      n_in_matrix /
      n_draws,
    
    .groups = "drop"
  )

if (any(id_check$n_missing > 0)) {
  print(
    id_check %>%
      filter(
        n_missing > 0
      ),
    n = Inf
  )
  
  stop(
    "Hay draws del bootstrap combinado que no existen en ",
    "matrix_32det_for_clustering.csv."
  )
}


# Distribución regional dentro de COMPLETE.
region_by_bootstrap <- combined %>%
  count(
    bootstrap_id,
    comparison_region,
    name = "n_draws"
  ) %>%
  group_by(
    bootstrap_id
  ) %>%
  mutate(
    prop_complete =
      n_draws /
      sum(n_draws)
  ) %>%
  ungroup()

region_summary <- region_by_bootstrap %>%
  group_by(
    comparison_region
  ) %>%
  summarise(
    mean_n = mean(
      n_draws
    ),
    
    min_n = min(
      n_draws
    ),
    
    max_n = max(
      n_draws
    ),
    
    mean_prop_complete = mean(
      prop_complete
    ),
    
    .groups = "drop"
  )


# Distribución por submuestra.
subsample_by_bootstrap <- combined %>%
  count(
    bootstrap_id,
    comparison_region,
    subsample,
    dataset_source,
    name = "n_draws"
  )

subsample_summary <- subsample_by_bootstrap %>%
  group_by(
    comparison_region,
    subsample,
    dataset_source
  ) %>%
  summarise(
    mean_n = mean(
      n_draws
    ),
    
    sd_n = sd(
      n_draws
    ),
    
    min_n = min(
      n_draws
    ),
    
    max_n = max(
      n_draws
    ),
    
    .groups = "drop"
  )


# Personas únicas dentro de cada réplica.
#
# Como ambos bootstraps usan muestreo con reemplazo, una misma persona
# puede aparecer varias veces dentro de la misma réplica.
unique_people_by_bootstrap <- combined %>%
  group_by(
    bootstrap_id,
    comparison_region,
    subsample
  ) %>%
  summarise(
    n_draws = n(),
    
    n_unique_people = n_distinct(
      integrated_row_id
    ),
    
    unique_share =
      n_unique_people /
      n_draws,
    
    .groups = "drop"
  )

unique_people_summary <- unique_people_by_bootstrap %>%
  group_by(
    comparison_region,
    subsample
  ) %>%
  summarise(
    mean_n_draws = mean(
      n_draws
    ),
    
    mean_n_unique = mean(
      n_unique_people
    ),
    
    mean_unique_share = mean(
      unique_share
    ),
    
    min_n_unique = min(
      n_unique_people
    ),
    
    max_n_unique = max(
      n_unique_people
    ),
    
    .groups = "drop"
  )


# Parámetros de la combinación.
parameters <- tibble(
  parameter = c(
    "n_boot",
    "eu_bootstrap_file",
    "latam_bootstrap_file",
    "clustering_metadata_file",
    "combination_mode",
    "europe_draws_per_bootstrap",
    "latam_draws_per_bootstrap",
    "complete_draws_per_bootstrap",
    "europe_share_complete",
    "latam_share_complete"
  ),
  
  value = c(
    as.character(
      EXPECTED_N_BOOT
    ),
    
    eu_file,
    latam_file,
    clustering_meta_file,
    COMBINATION_MODE,
    
    as.character(
      EU_SIZE
    ),
    
    as.character(
      LATAM_SIZE
    ),
    
    as.character(
      EXPECTED_TOTAL_SIZE
    ),
    
    as.character(
      EU_SIZE /
        EXPECTED_TOTAL_SIZE
    ),
    
    as.character(
      LATAM_SIZE /
        EXPECTED_TOTAL_SIZE
    )
  )
)


# Guardar outputs.
#
# El índice principal se guarda comprimido para reducir tamaño en disco.
write_csv(
  combined,
  output_file
)

outputs <- list(
  "bootstrap_combined_size_check.csv" =
    combined_size_check,
  
  "bootstrap_combined_clustering_id_check.csv" =
    id_check,
  
  "bootstrap_region_distribution_by_run.csv" =
    region_by_bootstrap,
  
  "bootstrap_region_distribution_summary.csv" =
    region_summary,
  
  "bootstrap_subsample_distribution_by_run.csv" =
    subsample_by_bootstrap,
  
  "bootstrap_subsample_distribution_summary.csv" =
    subsample_summary,
  
  "bootstrap_unique_people_by_run.csv" =
    unique_people_by_bootstrap,
  
  "bootstrap_unique_people_summary.csv" =
    unique_people_summary,
  
  "bootstrap_combination_parameters.csv" =
    parameters
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
cat("\nBOOTSTRAP EUROPA + LATAM COMBINADO\n")

cat(
  "\nNúmero de bootstraps: ",
  EXPECTED_N_BOOT,
  "\n",
  sep = ""
)

cat(
  "Europa por bootstrap: ",
  EU_SIZE,
  "\n",
  sep = ""
)

cat(
  "LATAM por bootstrap: ",
  LATAM_SIZE,
  "\n",
  sep = ""
)

cat(
  "TOTAL por bootstrap: ",
  EXPECTED_TOTAL_SIZE,
  "\n",
  sep = ""
)

cat(
  "\nPeso de Europa en COMPLETE: ",
  round(
    100 *
      EU_SIZE /
      EXPECTED_TOTAL_SIZE,
    2
  ),
  "%\n",
  sep = ""
)

cat(
  "Peso de LATAM en COMPLETE: ",
  round(
    100 *
      LATAM_SIZE /
      EXPECTED_TOTAL_SIZE,
    2
  ),
  "%\n",
  sep = ""
)

cat("\nRESUMEN POR REGIÓN\n")

print(
  region_summary,
  n = Inf,
  width = Inf
)

cat("\nRESUMEN POR SUBMUESTRA\n")

print(
  subsample_summary,
  n = Inf,
  width = Inf
)

cat("\nPERSONAS ÚNICAS POR SUBMUESTRA\n")

print(
  unique_people_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nArchivo principal:\n",
  output_file,
  "\n",
  sep = ""
)

cat(
  "\nOutputs guardados en:\n",
  out_dir,
  "\n",
  sep = ""
)

message(
  "\nListo. Bootstrap Europa + LATAM combinado correctamente."
)