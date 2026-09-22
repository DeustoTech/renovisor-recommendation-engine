
# Objetivo

# Crear la distribución poblacional objetivo de ingreso anual
# para WHY_LATAM, que será utilizada posteriormente en 04_2e_latam_income_bootstrap.R
#
# Países: México, Chile, Colombia
#
# Tramos de ingreso WHY: 
#  < 15.000
#   15.000 - 30.000
#   30.000 - 50.000
#   50.000 - 100.000
#   > 100.000
#
# Los porcentajes representan la distribución poblacional objetivo dentro de cada país.

suppressPackageStartupMessages({
  library(tidyverse)
})


# Configuración
processed_root <- file.path("paper1_cluster/data/processed")

out_dir <- file.path(
  processed_root,
  "04_latam_income_bootstrap"
)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

output_file <- file.path(
  out_dir,
  "latam_income_population_targets.csv"
)


# Orden de los tramos de ingreso
INCOME_BANDS <- c(
  "<15000",
  "15000_30000",
  "30000_50000",
  "50000_100000",
  ">100000"
)
# Targets poblacionales
#
# México:   84.8, 11.2, 2.8, 0.8, 0.4
# Chile:    75.4, 16.1, 5.2, 2.4, 0.9
# Colombia: 88.9,  8.1, 1.9, 0.8, 0.3
#
# Cada país suma exactamente 100%.

targets <- tribble(
  ~country_code, ~country,   ~income_band,    ~target_pct,
  
  "MX",          "Mexico",   "<15000",         84.8,
  "MX",          "Mexico",   "15000_30000",    11.2,
  "MX",          "Mexico",   "30000_50000",     2.8,
  "MX",          "Mexico",   "50000_100000",    0.8,
  "MX",          "Mexico",   ">100000",          0.4,
  
  "CL",          "Chile",    "<15000",         75.4,
  "CL",          "Chile",    "15000_30000",    16.1,
  "CL",          "Chile",    "30000_50000",     5.2,
  "CL",          "Chile",    "50000_100000",    2.4,
  "CL",          "Chile",    ">100000",          0.9,
  
  "CO",          "Colombia", "<15000",         88.9,
  "CO",          "Colombia", "15000_30000",     8.1,
  "CO",          "Colombia", "30000_50000",     1.9,
  "CO",          "Colombia", "50000_100000",    0.8,
  "CO",          "Colombia", ">100000",          0.3
) %>%
  mutate(
    # Proporción 0-1 utilizada posteriormente por el bootstrap.
    target_prop = target_pct / 100,
    
    target_type = "population_income_distribution"
  )


# Validaciones
target_check <- targets %>%
  group_by(
    country_code,
    country
  ) %>%
  summarise(
    n_bands = n(),
    total_pct = sum(target_pct),
    total_prop = sum(target_prop),
    .groups = "drop"
  )

# Cada país debe tener exactamente cinco tramos.
if (any(target_check$n_bands != 5)) {
  print(
    target_check,
    n = Inf
  )
  
  stop(
    "ERROR: algún país no tiene exactamente 5 tramos."
  )
}

# Los porcentajes deben sumar 100.
if (
  any(
    abs(
      target_check$total_pct - 100
    ) > 1e-8
  )
) {
  print(
    target_check,
    n = Inf
  )
  
  stop(
    "ERROR: los porcentajes de algún país no suman 100."
  )
}

# Las proporciones deben sumar 1.
if (
  any(
    abs(
      target_check$total_prop - 1
    ) > 1e-8
  )
) {
  print(
    target_check,
    n = Inf
  )
  
  stop(
    "ERROR: las proporciones de algún país no suman 1."
  )
}

# No puede haber porcentajes negativos.
if (any(targets$target_pct < 0)) {
  stop(
    "ERROR: existen porcentajes negativos."
  )
}


# Ordenar países y tramos
targets <- targets %>%
  mutate(
    country_code = factor(
      country_code,
      levels = c(
        "MX",
        "CL",
        "CO"
      )
    ),
    
    income_band = factor(
      income_band,
      levels = INCOME_BANDS
    )
  ) %>%
  arrange(
    country_code,
    income_band
  ) %>%
  mutate(
    country_code = as.character(
      country_code
    ),
    
    income_band = as.character(
      income_band
    )
  )


# Guardar
write_csv(
  targets,
  output_file
)

# Resumen en consola
cat("\nTARGETS DE INGRESO WHY_LATAM\n\n")

print(
  targets,
  n = Inf,
  width = Inf
)

cat("\nCOMPROBACIÓN DE SUMAS\n\n")

print(
  target_check,
  n = Inf,
  width = Inf
)

cat(
  "\nArchivo generado:\n",
  output_file,
  "\n",
  sep = ""
)

message(
  "Listo. Targets poblacionales de ingreso LATAM guardados correctamente."
)
