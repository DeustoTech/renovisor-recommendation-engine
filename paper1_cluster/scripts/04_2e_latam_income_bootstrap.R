
# Objetivo:
# Generar bootstraps de WHY_LATAM corrigiendo, dentro de cada país,
# la distribución observada de ingreso hacia los targets poblacionales
# definidos en 04_2d.
#
# Países:
# - México
# - Chile
# - Colombia
#
# Reglas metodológicas:
# 1. La postestratificación se realiza por país.
# 2. Solo entran personas usables para clustering.
# 3. Los pesos se estiman únicamente con personas con ingreso válido.
# 4. El tamaño final de cada país se mantiene igual al número TOTAL
#    de personas usables para clustering, aunque algunas no declarasen ingreso.
# 5. El muestreo se realiza con reemplazo.
# 6. Si un tramo de ingreso no tiene ninguna persona observada, no se
#    inventan individuos. Su masa objetivo se redistribuye proporcionalmente
#    entre los tramos disponibles de ese mismo país.
#
# Output principal:
# bootstrap_samples_index_latam_income.csv
#
# Este índice se combinará posteriormente con el bootstrap europeo.

suppressPackageStartupMessages({
  library(tidyverse)
})

set.seed(123)

# Configuración
project_root <- path.expand(
  "~/Desktop/MASTER/recommendation-engine/TFM"
)

processed_root <- file.path(
  project_root,
  "paper1_cluster/data/processed"
)

# Dataset individual preparado en 04_2c.
latam_file <- file.path(
  processed_root,
  "04_latam_income_bootstrap",
  "latam_income_individual_level.csv"
)

# Targets poblacionales generados en 04_2d.
targets_file <- file.path(
  processed_root,
  "04_latam_income_bootstrap",
  "latam_income_population_targets.csv"
)

out_dir <- file.path(
  processed_root,
  "04_2e_latam_income_bootstrap"
)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

N_BOOT <- 1000
BASE_SEED <- 123

COUNTRY_ORDER <- c(
  "MX",
  "CL",
  "CO"
)

INCOME_BANDS <- c(
  "<15000",
  "15000_30000",
  "30000_50000",
  "50000_100000",
  ">100000"
)

# Las personas sin ingreso válido no participan en el cálculo de pesos,
# pero el tamaño final por país se restaura al total usable para clustering.
MISSING_INCOME_POLICY <-
  "exclude_from_weight_pool_restore_country_sample_size"


# Comprobar inputs
if (!file.exists(latam_file)) {
  stop(
    "No encuentro el archivo LATAM individual:\n",
    latam_file
  )
}

if (!file.exists(targets_file)) {
  stop(
    "No encuentro el archivo de targets:\n",
    targets_file
  )
}


# Lectura
latam <- read_csv(
  latam_file,
  show_col_types = FALSE
)

targets <- read_csv(
  targets_file,
  show_col_types = FALSE
)


# Comprobar columnas necesarias
required_latam_cols <- c(
  "integrated_row_id",
  "country_code",
  "income_band",
  "usable_for_clustering"
)

missing_latam_cols <- setdiff(
  required_latam_cols,
  names(latam)
)

if (length(missing_latam_cols)) {
  stop(
    "Faltan columnas en latam_income_individual_level.csv: ",
    paste(
      missing_latam_cols,
      collapse = ", "
    )
  )
}

required_target_cols <- c(
  "country_code",
  "country",
  "income_band",
  "target_pct",
  "target_prop"
)

missing_target_cols <- setdiff(
  required_target_cols,
  names(targets)
)

if (length(missing_target_cols)) {
  stop(
    "Faltan columnas en latam_income_population_targets.csv: ",
    paste(
      missing_target_cols,
      collapse = ", "
    )
  )
}


# Normalizar tipos
to_logical <- function(x) {
  if (is.logical(x)) {
    return(x)
  }
  
  str_to_lower(
    str_trim(
      as.character(x)
    )
  ) %in% c(
    "true",
    "1",
    "yes",
    "si"
  )
}

latam <- latam %>%
  mutate(
    integrated_row_id = as.character(
      integrated_row_id
    ),
    
    country_code = str_to_upper(
      as.character(country_code)
    ),
    
    income_band = as.character(
      income_band
    ),
    
    usable_for_clustering = to_logical(
      usable_for_clustering
    )
  )

targets <- targets %>%
  mutate(
    country_code = str_to_upper(
      as.character(country_code)
    ),
    
    income_band = as.character(
      income_band
    ),
    
    target_prop = as.numeric(
      target_prop
    ),
    
    target_pct = as.numeric(
      target_pct
    )
  )


# Comprobar targets poblacionales
target_check <- targets %>%
  filter(
    country_code %in% COUNTRY_ORDER,
    income_band %in% INCOME_BANDS
  ) %>%
  group_by(
    country_code
  ) %>%
  summarise(
    n_bands = n(),
    
    target_sum = sum(
      target_prop,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

if (any(target_check$n_bands != length(INCOME_BANDS))) {
  print(
    target_check,
    n = Inf
  )
  
  stop(
    "Algún país no tiene exactamente 5 tramos de ingreso."
  )
}

if (
  any(
    abs(
      target_check$target_sum - 1
    ) > 1e-8
  )
) {
  print(
    target_check,
    n = Inf
  )
  
  stop(
    "Los targets de algún país no suman 1."
  )
}


# WHY_LATAM usable para clustering
latam_usable <- latam %>%
  filter(
    usable_for_clustering,
    country_code %in% COUNTRY_ORDER
  )

if (anyDuplicated(latam_usable$integrated_row_id)) {
  stop(
    "Hay integrated_row_id duplicados en WHY_LATAM usable."
  )
}


# Tamaño que debe conservar cada país en cada réplica.
#
# Se utiliza el número TOTAL de personas usables para clustering,
# no solo aquellas que han declarado un ingreso válido.
country_draw_sizes <- latam_usable %>%
  count(
    country_code,
    name = "n_draw_country"
  ) %>%
  arrange(
    match(
      country_code,
      COUNTRY_ORDER
    )
  )

cat("\nTAMAÑO BOOTSTRAP POR PAÍS\n\n")

print(
  country_draw_sizes,
  n = Inf
)

cat(
  "\nTOTAL WHY_LATAM POR BOOTSTRAP: ",
  sum(
    country_draw_sizes$n_draw_country
  ),
  "\n",
  sep = ""
)


# Pool utilizado para estimar los pesos.
#
# Solo las personas con uno de los cinco tramos de ingreso válidos
# pueden representar la distribución económica objetivo.
income_pool <- latam_usable %>%
  filter(
    income_band %in% INCOME_BANDS
  )

income_pool_sizes <- income_pool %>%
  count(
    country_code,
    name = "n_valid_income"
  )

cat("\nCASOS CON INGRESO VÁLIDO PARA PONDERAR\n\n")

print(
  income_pool_sizes,
  n = Inf
)

# Cada país necesita al menos alguna persona con ingreso conocido.
missing_country_pool <- setdiff(
  COUNTRY_ORDER,
  unique(
    income_pool$country_code
  )
)

if (length(missing_country_pool)) {
  stop(
    "No hay ningún caso con ingreso válido para: ",
    paste(
      missing_country_pool,
      collapse = ", "
    )
  )
}


# Distribución observada país × ingreso.
#
# expand_grid() garantiza que aparezcan también las combinaciones
# país-tramo con cero observaciones.
sample_cells <- expand_grid(
  country_code = COUNTRY_ORDER,
  income_band = INCOME_BANDS
) %>%
  left_join(
    income_pool %>%
      count(
        country_code,
        income_band,
        name = "n_sample"
      ),
    by = c(
      "country_code",
      "income_band"
    )
  ) %>%
  mutate(
    n_sample = replace_na(
      n_sample,
      0L
    )
  ) %>%
  group_by(
    country_code
  ) %>%
  mutate(
    n_valid_income = sum(
      n_sample
    ),
    
    sample_prop = if_else(
      n_valid_income > 0,
      n_sample / n_valid_income,
      NA_real_
    )
  ) %>%
  ungroup()


# Comparar distribución observada y target poblacional.
target_vs_sample <- sample_cells %>%
  left_join(
    targets %>%
      select(
        country_code,
        country,
        income_band,
        target_pct,
        target_prop,
        any_of("target_type")
      ),
    by = c(
      "country_code",
      "income_band"
    )
  )

if (any(is.na(target_vs_sample$target_prop))) {
  print(
    target_vs_sample %>%
      filter(
        is.na(target_prop)
      ),
    n = Inf
  )
  
  stop(
    "Hay combinaciones país-ingreso sin target."
  )
}

# Tramos imposibles de representar.
#
# Si un tramo tiene target > 0 pero n_sample = 0, el bootstrap no puede
# generar personas pertenecientes a ese tramo.
#
# No se inventan individuos. La masa objetivo correspondiente se
# redistribuye proporcionalmente entre los tramos que sí están
# representados dentro del mismo país.
target_vs_sample <- target_vs_sample %>%
  group_by(
    country_code
  ) %>%
  mutate(
    available_band =
      n_sample > 0,
    
    target_prop_available_raw = if_else(
      available_band,
      target_prop,
      0
    ),
    
    available_target_mass = sum(
      target_prop_available_raw,
      na.rm = TRUE
    ),
    
    unavailable_target_mass = sum(
      if_else(
        !available_band,
        target_prop,
        0
      ),
      na.rm = TRUE
    ),
    
    target_prop_effective = if_else(
      available_band &
        available_target_mass > 0,
      
      target_prop /
        available_target_mass,
      
      0
    )
  ) %>%
  ungroup()


# Pesos de postestratificación.
#
# El ratio clásico es:
#
#   target / observado
#
# Para realizar directamente el muestreo, la probabilidad individual
# se define como:
#
#   target_prop_effective / n_personas_del_tramo
#
# De esta forma, la suma de probabilidades individuales de un tramo
# coincide exactamente con su target efectivo.
target_vs_sample <- target_vs_sample %>%
  mutate(
    poststrat_ratio = case_when(
      n_sample > 0 &
        sample_prop > 0 ~
        target_prop_effective /
        sample_prop,
      
      TRUE ~
        NA_real_
    ),
    
    probability_per_person = case_when(
      n_sample > 0 ~
        target_prop_effective /
        n_sample,
      
      TRUE ~
        0
    )
  )


# Asignar probabilidad de selección a cada persona
income_pool_weighted <- income_pool %>%
  left_join(
    target_vs_sample %>%
      select(
        country_code,
        income_band,
        n_sample,
        sample_prop,
        target_prop,
        target_prop_effective,
        poststrat_ratio,
        probability_per_person
      ),
    by = c(
      "country_code",
      "income_band"
    )
  ) %>%
  group_by(
    country_code
  ) %>%
  mutate(
    # Normalización defensiva. La suma ya debería ser 1 por país.
    selection_prob =
      probability_per_person /
      sum(
        probability_per_person,
        na.rm = TRUE
      )
  ) %>%
  ungroup()


# Comprobar las probabilidades de selección.
probability_check <- income_pool_weighted %>%
  group_by(
    country_code
  ) %>%
  summarise(
    n_people = n(),
    
    sum_selection_prob = sum(
      selection_prob,
      na.rm = TRUE
    ),
    
    min_selection_prob = min(
      selection_prob,
      na.rm = TRUE
    ),
    
    max_selection_prob = max(
      selection_prob,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

if (
  any(
    abs(
      probability_check$sum_selection_prob - 1
    ) > 1e-8
  )
) {
  print(
    probability_check,
    n = Inf
  )
  
  stop(
    "Las probabilidades de selección no suman 1 por país."
  )
}


# Diagnóstico previo al bootstrap.
cat("\nTARGET VS MUESTRA\n\n")

print(
  target_vs_sample %>%
    select(
      country_code,
      country,
      income_band,
      n_sample,
      sample_prop,
      target_prop,
      target_prop_effective,
      unavailable_target_mass,
      poststrat_ratio
    ),
  n = Inf,
  width = Inf
)

cat("\nCOMPROBACIÓN DE PROBABILIDADES\n\n")

print(
  probability_check,
  n = Inf,
  width = Inf
)


# Identificar targets sin ninguna persona observada.
unavailable_cells <- target_vs_sample %>%
  filter(
    target_prop > 0,
    n_sample == 0
  )

if (nrow(unavailable_cells)) {
  cat(
    "\nAVISO: TRAMOS OBJETIVO SIN PERSONAS OBSERVADAS\n\n"
  )
  
  print(
    unavailable_cells %>%
      select(
        country_code,
        country,
        income_band,
        target_prop
      ),
    n = Inf
  )
  
  cat(
    "\nEsos tramos no se inventan. ",
    "Su masa objetivo se redistribuye entre los tramos ",
    "disponibles del mismo país.\n",
    sep = ""
  )
}


# Generar una réplica bootstrap.
make_one_bootstrap <- function(
    bootstrap_id_current
) {
  # Cada réplica utiliza una semilla diferente pero reproducible.
  set.seed(
    BASE_SEED +
      bootstrap_id_current
  )
  
  country_results <- map(
    COUNTRY_ORDER,
    function(country_current) {
      pool_country <- income_pool_weighted %>%
        filter(
          country_code ==
            country_current
        )
      
      n_draw_current <- country_draw_sizes %>%
        filter(
          country_code ==
            country_current
        ) %>%
        pull(
          n_draw_country
        )
      
      if (length(n_draw_current) != 1) {
        stop(
          "No puedo determinar n_draw para ",
          country_current
        )
      }
      
      if (!nrow(pool_country)) {
        stop(
          "No hay pool de ingreso válido para ",
          country_current
        )
      }
      
      sampled_rows <- sample(
        seq_len(
          nrow(pool_country)
        ),
        size = n_draw_current,
        replace = TRUE,
        prob = pool_country$selection_prob
      )
      
      sampled <- pool_country[
        sampled_rows,
        ,
        drop = FALSE
      ]
      
      # Mantener el nombre de país incluso si el input individual
      # no incluyera explícitamente la columna country.
      if (!"country" %in% names(sampled)) {
        sampled <- sampled %>%
          mutate(
            country = case_when(
              country_code == "MX" ~ "Mexico",
              country_code == "CL" ~ "Chile",
              country_code == "CO" ~ "Colombia",
              TRUE ~ country_code
            )
          )
      }
      
      sampled %>%
        transmute(
          bootstrap_id =
            bootstrap_id_current,
          
          country_draw_id =
            row_number(),
          
          integrated_row_id,
          country_code,
          country,
          income_band,
          sample_prop,
          target_prop,
          target_prop_effective,
          poststrat_ratio,
          selection_prob
        )
    }
  )
  
  bind_rows(
    country_results
  ) %>%
    mutate(
      draw_id = row_number(),
      
      bootstrap_source =
        "LATAM_INCOME"
    ) %>%
    select(
      bootstrap_id,
      draw_id,
      country_draw_id,
      integrated_row_id,
      country_code,
      country,
      income_band,
      sample_prop,
      target_prop,
      target_prop_effective,
      poststrat_ratio,
      selection_prob,
      bootstrap_source
    )
}


# Generar las 1000 réplicas bootstrap.
cat("\nGENERANDO BOOTSTRAPS LATAM\n\n")

bootstrap_list <- vector(
  "list",
  N_BOOT
)

for (b in seq_len(N_BOOT)) {
  if (
    b == 1 ||
    b %% 100 == 0
  ) {
    cat(
      "Bootstrap ",
      b,
      " / ",
      N_BOOT,
      "\n",
      sep = ""
    )
  }
  
  bootstrap_list[[b]] <- make_one_bootstrap(
    bootstrap_id_current = b
  )
}

bootstrap_index <- bind_rows(
  bootstrap_list
)

rm(
  bootstrap_list
)


# Comprobar tamaño total de cada réplica.
expected_n_per_boot <- sum(
  country_draw_sizes$n_draw_country
)

bootstrap_sizes <- bootstrap_index %>%
  count(
    bootstrap_id,
    name = "n_draws"
  )

if (
  any(
    bootstrap_sizes$n_draws !=
    expected_n_per_boot
  )
) {
  stop(
    "Algún bootstrap no tiene el tamaño esperado."
  )
}


# Comprobar que cada país mantiene su tamaño original usable.
country_counts_by_run <- bootstrap_index %>%
  count(
    bootstrap_id,
    country_code,
    name = "n_draws_country"
  ) %>%
  left_join(
    country_draw_sizes,
    by = "country_code"
  ) %>%
  mutate(
    correct_country_size =
      n_draws_country ==
      n_draw_country
  )

if (
  any(
    !country_counts_by_run$correct_country_size
  )
) {
  stop(
    "Algún bootstrap no mantiene el tamaño por país."
  )
}


# Distribución de ingreso obtenida en cada réplica.
#
# complete() mantiene también los tramos que aparezcan cero veces
# en una réplica concreta.
bootstrap_distribution_by_run <- bootstrap_index %>%
  count(
    bootstrap_id,
    country_code,
    income_band,
    name = "n"
  ) %>%
  complete(
    bootstrap_id =
      seq_len(N_BOOT),
    
    country_code =
      COUNTRY_ORDER,
    
    income_band =
      INCOME_BANDS,
    
    fill = list(
      n = 0L
    )
  ) %>%
  left_join(
    country_draw_sizes,
    by = "country_code"
  ) %>%
  mutate(
    prop =
      n /
      n_draw_country
  ) %>%
  left_join(
    target_vs_sample %>%
      select(
        country_code,
        income_band,
        target_prop,
        target_prop_effective
      ),
    by = c(
      "country_code",
      "income_band"
    )
  )

bootstrap_distribution_summary <- bootstrap_distribution_by_run %>%
  group_by(
    country_code,
    income_band,
    target_prop,
    target_prop_effective
  ) %>%
  summarise(
    mean_boot_prop = mean(
      prop,
      na.rm = TRUE
    ),
    
    sd_boot_prop = sd(
      prop,
      na.rm = TRUE
    ),
    
    min_boot_prop = min(
      prop,
      na.rm = TRUE
    ),
    
    q25_boot_prop = as.numeric(
      quantile(
        prop,
        0.25,
        na.rm = TRUE
      )
    ),
    
    median_boot_prop = median(
      prop,
      na.rm = TRUE
    ),
    
    q75_boot_prop = as.numeric(
      quantile(
        prop,
        0.75,
        na.rm = TRUE
      )
    ),
    
    max_boot_prop = max(
      prop,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  arrange(
    match(
      country_code,
      COUNTRY_ORDER
    ),
    match(
      income_band,
      INCOME_BANDS
    )
  )


# Diversidad de personas dentro de cada réplica.
#
# Al muestrear con reemplazo una misma persona puede aparecer varias veces.
# Si el pool de ingreso conocido es pequeño, como puede ocurrir en Colombia,
# el porcentaje de personas únicas será menor.
bootstrap_unique_people <- bootstrap_index %>%
  group_by(
    bootstrap_id,
    country_code
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

bootstrap_unique_people_summary <- bootstrap_unique_people %>%
  group_by(
    country_code
  ) %>%
  summarise(
    mean_n_unique = mean(
      n_unique_people
    ),
    
    min_n_unique = min(
      n_unique_people
    ),
    
    max_n_unique = max(
      n_unique_people
    ),
    
    mean_unique_share = mean(
      unique_share
    ),
    
    .groups = "drop"
  )


# Parámetros utilizados.
parameters <- tibble(
  parameter = c(
    "n_boot",
    "base_seed",
    "countries",
    "income_bands",
    "missing_income_policy",
    "expected_draws_per_bootstrap",
    "bootstrap_method"
  ),
  
  value = c(
    as.character(N_BOOT),
    as.character(BASE_SEED),
    
    paste(
      COUNTRY_ORDER,
      collapse = ", "
    ),
    
    paste(
      INCOME_BANDS,
      collapse = ", "
    ),
    
    MISSING_INCOME_POLICY,
    
    as.character(
      expected_n_per_boot
    ),
    
    "country_stratified_income_poststratified_sampling_with_replacement"
  )
)


# Guardar outputs.
outputs <- list(
  "bootstrap_samples_index_latam_income.csv" =
    bootstrap_index,
  
  "latam_income_target_vs_sample.csv" =
    target_vs_sample,
  
  "latam_income_individual_weights.csv" =
    income_pool_weighted,
  
  "latam_income_probability_check.csv" =
    probability_check,
  
  "latam_income_unavailable_target_cells.csv" =
    unavailable_cells,
  
  "latam_country_draw_sizes.csv" =
    country_draw_sizes,
  
  "latam_bootstrap_sizes.csv" =
    bootstrap_sizes,
  
  "latam_country_counts_by_bootstrap.csv" =
    country_counts_by_run,
  
  "latam_income_distribution_by_bootstrap.csv" =
    bootstrap_distribution_by_run,
  
  "latam_income_distribution_bootstrap_summary.csv" =
    bootstrap_distribution_summary,
  
  "latam_unique_people_by_bootstrap.csv" =
    bootstrap_unique_people,
  
  "latam_unique_people_summary.csv" =
    bootstrap_unique_people_summary,
  
  "latam_income_bootstrap_parameters.csv" =
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


# Resumen final.
cat("\nBOOTSTRAP WHY_LATAM COMPLETADO\n")

cat(
  "\nNúmero de bootstraps: ",
  N_BOOT,
  "\n",
  sep = ""
)

cat(
  "Tamaño de cada bootstrap: ",
  expected_n_per_boot,
  "\n",
  sep = ""
)

cat("\nTamaño por país:\n")

print(
  country_draw_sizes,
  n = Inf
)

cat("\nDistribución media de ingreso obtenida:\n")

print(
  bootstrap_distribution_summary %>%
    select(
      country_code,
      income_band,
      target_prop,
      target_prop_effective,
      mean_boot_prop,
      sd_boot_prop
    ),
  n = Inf,
  width = Inf
)

cat("\nPersonas únicas por país dentro de los bootstraps:\n")

print(
  bootstrap_unique_people_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nArchivo principal:\n",
  file.path(
    out_dir,
    "bootstrap_samples_index_latam_income.csv"
  ),
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
  "\nListo. Bootstrap LATAM por ingreso generado correctamente."
)