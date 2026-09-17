#
# Objetivo
# Construye las transformaciones de los 32 determinantes que utilizarán
# posteriormente K-means, EFA y Greedy.
#
# Muestras de referencia:
#   COMPLETE
#   EUROPE
#   LATAM
#   DIEGO
#   RENOVISOR
#   WHY_EUROPE
#   WHY_LATAM
#
# En este script COMPLETE representa todos los participantes únicos
# utilizables para clustering. No es todavía la muestra bootstrap de 04_2f.
#
# LATAM y WHY_LATAM contienen actualmente los mismos participantes,
# pero se mantienen separados porque representan:
#   LATAM     -> comparación regional
#   WHY_LATAM -> comparación por submuestra
#

suppressPackageStartupMessages({
  library(tidyverse)
})


# Configuración
processed_root <- "paper1_cluster/data/processed"

in_file <- file.path(
  processed_root,
  "03_component_quality",
  "matrix_32det_for_clustering.csv"
)

mapping_file <- file.path(
  processed_root,
  "03_2_phase_dimension_scores",
  "dimension_determinant_mapping.csv"
)

out_dir <- file.path(
  processed_root,
  "05_clustering_matrices"
)

MAX_POS <- 50
EXT_THRESHOLD <- 25

ANALYSIS_SAMPLES <- c(
  "COMPLETE",
  "EUROPE",
  "LATAM",
  "DIEGO",
  "RENOVISOR",
  "WHY_EUROPE",
  "WHY_LATAM"
)

EXPECTED_REGIONS <- c(
  "EUROPE",
  "LATAM"
)

EXPECTED_SUBSAMPLES <- c(
  "DIEGO",
  "RENOVISOR",
  "WHY_EUROPE",
  "WHY_LATAM"
)

if (!file.exists(in_file)) {
  stop(
    "No encuentro el archivo de entrada: ",
    in_file
  )
}

if (!file.exists(mapping_file)) {
  stop(
    "No encuentro el mapeo dimensión-determinantes: ",
    mapping_file,
    "\nEjecuta primero 03_2_add_phase_dimension_scores.R"
  )
}

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# Lectura
df_all <- read_csv(
  in_file,
  show_col_types = FALSE,
  col_types = cols(
    .default = col_character()
  )
)

dimension_determinant_map <- read_csv(
  mapping_file,
  show_col_types = FALSE
)


# Comprobaciones
required_cols <- c(
  "integrated_row_id",
  "subsample",
  "comparison_region",
  "dataset_source"
)

missing_required_cols <- setdiff(
  required_cols,
  names(df_all)
)

if (length(missing_required_cols)) {
  stop(
    "Faltan columnas necesarias: ",
    paste(
      missing_required_cols,
      collapse = ", "
    )
  )
}

if (anyDuplicated(df_all$integrated_row_id)) {
  stop(
    "integrated_row_id contiene duplicados en la matriz de clustering."
  )
}

missing_subsamples <- setdiff(
  EXPECTED_SUBSAMPLES,
  unique(df_all$subsample)
)

if (length(missing_subsamples)) {
  stop(
    "Faltan submuestras esperadas: ",
    paste(
      missing_subsamples,
      collapse = ", "
    )
  )
}

missing_regions <- setdiff(
  EXPECTED_REGIONS,
  unique(df_all$comparison_region)
)

if (length(missing_regions)) {
  stop(
    "Faltan regiones esperadas: ",
    paste(
      missing_regions,
      collapse = ", "
    )
  )
}

unexpected_regions <- setdiff(
  unique(
    na.omit(
      df_all$comparison_region
    )
  ),
  EXPECTED_REGIONS
)

if (length(unexpected_regions)) {
  stop(
    "Hay regiones inesperadas: ",
    paste(
      unexpected_regions,
      collapse = ", "
    )
  )
}


# Determinantes
det_cols <- names(df_all)[
  str_detect(
    names(df_all),
    "^det_\\d{2}_"
  )
]

if (length(det_cols) != 32) {
  stop(
    "Se esperaban 32 determinantes y se han encontrado ",
    length(det_cols),
    "."
  )
}

required_mapping_cols <- c(
  "dimension",
  "det_col"
)

if (
  any(
    !required_mapping_cols %in%
    names(dimension_determinant_map)
  )
) {
  stop(
    "El archivo de mapeo debe contener dimension y det_col."
  )
}

if (
  n_distinct(
    dimension_determinant_map$det_col
  ) != 32
) {
  stop(
    "El mapeo no contiene exactamente 32 determinantes únicos."
  )
}

if (
  n_distinct(
    dimension_determinant_map$dimension
  ) != 9
) {
  stop(
    "El mapeo no contiene exactamente 9 dimensiones."
  )
}

missing_in_mapping <- setdiff(
  det_cols,
  dimension_determinant_map$det_col
)

extra_in_mapping <- setdiff(
  dimension_determinant_map$det_col,
  det_cols
)

if (length(missing_in_mapping)) {
  stop(
    "Faltan determinantes en el mapeo: ",
    paste(
      missing_in_mapping,
      collapse = ", "
    )
  )
}

if (length(extra_in_mapping)) {
  stop(
    "El mapeo contiene determinantes no presentes en el dataset: ",
    paste(
      extra_in_mapping,
      collapse = ", "
    )
  )
}

dimension_determinants <- split(
  dimension_determinant_map$det_col,
  dimension_determinant_map$dimension
)


# Metadata que acompañará a las matrices
id_cols <- c(
  "analysis_sample",
  "integrated_row_id",
  "subsample",
  "comparison_region",
  "subsample_row_id",
  "global_participant_key",
  "dataset_source",
  "source_survey",
  "source_file",
  "participant_key",
  "prolific_id",
  "identification_code",
  "country_model",
  "country_model_grouped",
  "country_region_model",
  "age_group_model",
  "gender_model",
  "employment_model",
  "row_quality_final",
  "usable_for_clustering",
  "usable_for_main_analysis",
  "n_det_valid"
)


# Funciones auxiliares
as_num <- function(x) {
  suppressWarnings(
    parse_number(
      as.character(x),
      locale = locale(
        decimal_mark = ".",
        grouping_mark = ","
      )
    )
  )
}


get_analysis_sample <- function(
    data,
    sample_name
) {
  if (sample_name == "COMPLETE") {
    out <- data
    
  } else if (sample_name == "EUROPE") {
    out <- data %>%
      filter(
        comparison_region == "EUROPE"
      )
    
  } else if (sample_name == "LATAM") {
    out <- data %>%
      filter(
        comparison_region == "LATAM"
      )
    
  } else {
    out <- data %>%
      filter(
        subsample == sample_name
      )
  }
  
  if (!nrow(out)) {
    stop(
      "La muestra ",
      sample_name,
      " tiene 0 filas."
    )
  }
  
  out %>%
    mutate(
      analysis_sample = sample_name
    ) %>%
    relocate(
      analysis_sample
    )
}


write_matrix <- function(
    sample_df,
    mat,
    matrix_name,
    sample_dir
) {
  if (nrow(sample_df) != nrow(mat)) {
    stop(
      "Número de filas distinto entre metadata y ",
      matrix_name,
      "."
    )
  }
  
  current_id_cols <- intersect(
    id_cols,
    names(sample_df)
  )
  
  out <- bind_cols(
    sample_df %>%
      select(
        all_of(
          current_id_cols
        )
      ),
    as_tibble(mat)
  )
  
  write_csv(
    out,
    file.path(
      sample_dir,
      paste0(
        matrix_name,
        ".csv"
      )
    )
  )
  
  invisible(out)
}


summarise_matrix <- function(
    mat,
    matrix_name,
    sample_name
) {
  values <- as.numeric(
    as.matrix(mat)
  )
  
  tibble(
    analysis_sample = sample_name,
    matrix_name = matrix_name,
    n_rows = nrow(mat),
    n_determinants = ncol(mat),
    n_values = length(values),
    n_missing = sum(
      is.na(values)
    ),
    prop_missing =
      n_missing /
      n_values,
    mean_value = mean(
      values,
      na.rm = TRUE
    ),
    sd_value = sd(
      values,
      na.rm = TRUE
    ),
    min_value = min(
      values,
      na.rm = TRUE
    ),
    q25_value = as.numeric(
      quantile(
        values,
        0.25,
        na.rm = TRUE
      )
    ),
    median_value = median(
      values,
      na.rm = TRUE
    ),
    q75_value = as.numeric(
      quantile(
        values,
        0.75,
        na.rm = TRUE
      )
    ),
    max_value = max(
      values,
      na.rm = TRUE
    )
  )
}


aggregate_to_dimensions <- function(mat) {
  map_dfc(
    names(
      dimension_determinants
    ),
    function(dim) {cols_dim <- dimension_determinants[[dim]]
      
      values <- rowMeans(
        as.matrix(
          mat[
            ,
            cols_dim,
            drop = FALSE
          ]
        ),
        na.rm = TRUE
      )
      
      values[
        is.nan(values)
      ] <- NA_real_
      
      tibble(
        !!paste0(
          "dim_",
          str_to_lower(dim)
        ) := values
      )
    }
  )
}


save_plot <- function(
    plot,
    filename,
    fig_dir,
    width = 10,
    height = 6
) {
  ggsave(
    file.path(
      fig_dir,
      filename
    ),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}


# Construcción de matrices
build_sample_matrices <- function(sample_name) {
  
  cat(
    "\nGENERANDO MATRICES: ",
    sample_name,
    "\n",
    sep = ""
  )
  
  sample_df <- get_analysis_sample(
    df_all,
    sample_name
  )
  
  sample_dir <- file.path(
    out_dir,
    sample_name
  )
  
  fig_dir <- file.path(
    sample_dir,
    "figures"
  )
  
  walk(
    c(
      sample_dir,
      fig_dir
    ),
    ~ dir.create(
      .x,
      recursive = TRUE,
      showWarnings = FALSE
    )
  )
  
  
  # Valores originales válidos 0-100
  det_0_100_clean <- sample_df %>%
    select(
      all_of(
        det_cols
      )
    ) %>%
    mutate(
      across(
        everything(),
        as_num
      )
    ) %>%
    mutate(
      across(
        everything(),
        ~ if_else(
          !is.na(.x) &
            .x >= 0 &
            .x <= 100,
          .x,
          NA_real_
        )
      )
    )
  
  
  # Imputación neutral para las matrices de análisis
  det_0_100_imputed50 <- det_0_100_clean %>%
    mutate(
      across(
        everything(),
        ~ replace_na(
          .x,
          50
        )
      )
    )
  
  
  # RAW
  #
  # Escala original 0-100 transformada a 0-1.
  # Missing -> 50 -> 0.5.
  
  matrix_raw_0_1 <-
    det_0_100_imputed50 /
    100
  
  names(
    matrix_raw_0_1
  ) <- det_cols
  
  
  # POS
  #
  # Valores >= 50 conservan x/100.
  # Valores < 50 y missing se representan como 0.5 en la matriz analítica.
  #
  # La máscara Greedy conserva los missing originales como NA.
  pos_mask_for_greedy <- det_0_100_clean %>%
    mutate(
      across(
        everything(),
        ~ if_else(
          !is.na(.x) &
            .x >= MAX_POS,
          .x,
          NA_real_
        )
      )
    )
  
  matrix_pos_0_1 <- det_0_100_imputed50 %>%
    mutate(
      across(
        everything(),
        ~ if_else(
          .x >= MAX_POS,
          .x / 100,
          0.5
        )
      )
    )
  
  names(
    matrix_pos_0_1
  ) <- det_cols
  
  names(
    pos_mask_for_greedy
  ) <- det_cols
  
  
  # EXT
  #
  # Distancia absoluta respecto al punto neutral 50.
  # Se conservan únicamente distancias >= 25.
  #
  # Missing y valores no extremos -> 0 en la matriz analítica.
  # La máscara Greedy conserva missing/no extremos como NA.
  ext_distance_imputed <-
    abs(
      det_0_100_imputed50 -
        50
    )
  
  ext_distance_observed <-
    abs(
      det_0_100_clean -
        50
    )
  
  ext_mask_for_greedy <- ext_distance_observed %>%
    mutate(
      across(
        everything(),
        ~ if_else(
          !is.na(.x) &
            .x >= EXT_THRESHOLD,
          .x,
          NA_real_
        )
      )
    )
  
  matrix_ext_0_1 <- ext_distance_imputed %>%
    mutate(
      across(
        everything(),
        ~ if_else(
          .x >= EXT_THRESHOLD,
          .x / 50,
          0
        )
      )
    )
  
  names(
    matrix_ext_0_1
  ) <- det_cols
  
  names(
    ext_mask_for_greedy
  ) <- det_cols
  
  
  # Z_ABS
  #
  # abs((x - media) / sd)
  #
  # Media y SD se calculan separadamente dentro de cada muestra
  # de referencia. Missing -> 0 después de la transformación.
  det_means <- map_dbl(
    det_0_100_clean,
    ~ mean(
      .x,
      na.rm = TRUE
    )
  )
  
  det_sds_original <- map_dbl(
    det_0_100_clean,
    ~ sd(
      .x,
      na.rm = TRUE
    )
  )
  
  det_sds_used <- det_sds_original
  
  det_sds_used[
    is.na(det_sds_used) |
      det_sds_used == 0
  ] <- 1
  
  matrix_z_abs <- map2_dfc(
    det_0_100_clean,
    seq_along(
      det_cols
    ),
    function(x, j) {
      z_abs <- abs(
        (
          x -
            det_means[j]
        ) /
          det_sds_used[j]
      )
      
      z_abs[
        is.na(z_abs)
      ] <- 0
      
      tibble(
        !!det_cols[j] :=
          z_abs
      )
    }
  )
  
  
  # Matrices agregadas a 9 dimensiones
  matrix_dim9_raw_0_1 <-
    aggregate_to_dimensions(
      matrix_raw_0_1
    )
  
  matrix_dim9_pos_0_1 <-
    aggregate_to_dimensions(
      matrix_pos_0_1
    )
  
  matrix_dim9_ext_0_1 <-
    aggregate_to_dimensions(
      matrix_ext_0_1
    )
  
  matrix_dim9_z_abs <-
    aggregate_to_dimensions(
      matrix_z_abs
    )
  
  
  # Registro de matrices
  matrices <- list(
    matrix_32_raw_0_100_imputed50 =
      det_0_100_imputed50,
    
    matrix_32_raw_0_1 =
      matrix_raw_0_1,
    
    matrix_32_pos_0_1 =
      matrix_pos_0_1,
    
    matrix_32_ext_0_1 =
      matrix_ext_0_1,
    
    matrix_32_z_abs =
      matrix_z_abs,
    
    matrix_32_pos_mask_for_greedy =
      pos_mask_for_greedy,
    
    matrix_32_ext_mask_for_greedy =
      ext_mask_for_greedy,
    
    matrix_9dim_raw_0_1 =
      matrix_dim9_raw_0_1,
    
    matrix_9dim_pos_0_1 =
      matrix_dim9_pos_0_1,
    
    matrix_9dim_ext_0_1 =
      matrix_dim9_ext_0_1,
    
    matrix_9dim_z_abs =
      matrix_dim9_z_abs
  )
  
  iwalk(
    matrices,
    ~ write_matrix(
      sample_df = sample_df,
      mat = .x,
      matrix_name = .y,
      sample_dir = sample_dir
    )
  )
  
  
  # Diagnósticos
  diagnostics_parameters <- tibble(
    analysis_sample = sample_name,
    
    parameter = c(
      "n_rows",
      "MAX_POS",
      "EXT_THRESHOLD",
      "z_abs_reference",
      "input_file",
      "dimension_mapping_file",
      "output_dir"
    ),
    
    value = c(
      as.character(
        nrow(sample_df)
      ),
      as.character(
        MAX_POS
      ),
      as.character(
        EXT_THRESHOLD
      ),
      paste0(
        "sample_specific_",
        sample_name
      ),
      in_file,
      mapping_file,
      sample_dir
    )
  )
  
  
  diagnostics_input_missing <- det_0_100_clean %>%
    pivot_longer(
      all_of(
        det_cols
      ),
      names_to = "determinant",
      values_to = "value"
    ) %>%
    group_by(
      determinant
    ) %>%
    summarise(
      analysis_sample =
        sample_name,
      
      n_rows = n(),
      
      n_missing = sum(
        is.na(value)
      ),
      
      prop_missing =
        n_missing /
        n_rows,
      
      mean_observed = mean(
        value,
        na.rm = TRUE
      ),
      
      sd_observed = sd(
        value,
        na.rm = TRUE
      ),
      
      min_observed = suppressWarnings(
        min(
          value,
          na.rm = TRUE
        )
      ),
      
      max_observed = suppressWarnings(
        max(
          value,
          na.rm = TRUE
        )
      ),
      
      .groups = "drop"
    ) %>%
    mutate(
      min_observed = if_else(
        is.infinite(
          min_observed
        ),
        NA_real_,
        min_observed
      ),
      
      max_observed = if_else(
        is.infinite(
          max_observed
        ),
        NA_real_,
        max_observed
      )
    ) %>%
    relocate(
      analysis_sample
    ) %>%
    arrange(
      desc(
        prop_missing
      ),
      determinant
    )
  
  
  diagnostics_z_abs_reference <- tibble(
    analysis_sample =
      sample_name,
    
    determinant =
      det_cols,
    
    sample_mean =
      as.numeric(
        det_means
      ),
    
    sample_sd_original =
      as.numeric(
        det_sds_original
      ),
    
    sample_sd_used =
      as.numeric(
        det_sds_used
      ),
    
    sd_replaced =
      is.na(
        det_sds_original
      ) |
      det_sds_original == 0
  )
  
  
  matrix_summary_inputs <- list(
    matrix_32_raw_0_1 =
      matrix_raw_0_1,
    
    matrix_32_pos_0_1 =
      matrix_pos_0_1,
    
    matrix_32_ext_0_1 =
      matrix_ext_0_1,
    
    matrix_32_z_abs =
      matrix_z_abs,
    
    matrix_9dim_raw_0_1 =
      matrix_dim9_raw_0_1,
    
    matrix_9dim_pos_0_1 =
      matrix_dim9_pos_0_1,
    
    matrix_9dim_ext_0_1 =
      matrix_dim9_ext_0_1,
    
    matrix_9dim_z_abs =
      matrix_dim9_z_abs
  )
  
  diagnostics_matrix_summary <- imap_dfr(
    matrix_summary_inputs,
    ~ summarise_matrix(
      mat = .x,
      matrix_name = .y,
      sample_name = sample_name
    )
  )
  
  
  # Los recuentos de umbral se calculan sobre valores observados.
  # Los missing no se convierten artificialmente en positivos.
  diagnostics_threshold_counts <- tibble(
    analysis_sample =
      sample_name,
    
    determinant =
      det_cols,
    
    n_pos_ge_MAX = map_int(
      det_0_100_clean[
        det_cols
      ],
      ~ sum(
        .x >= MAX_POS,
        na.rm = TRUE
      )
    ),
    
    n_ext_ge_threshold = map_int(
      ext_distance_observed[
        det_cols
      ],
      ~ sum(
        .x >= EXT_THRESHOLD,
        na.rm = TRUE
      )
    ),
    
    mean_raw_0_100 = map_dbl(
      det_0_100_imputed50[
        det_cols
      ],
      ~ mean(
        .x,
        na.rm = TRUE
      )
    ),
    
    sd_raw_0_100 = map_dbl(
      det_0_100_imputed50[
        det_cols
      ],
      ~ sd(
        .x,
        na.rm = TRUE
      )
    )
  ) %>%
    mutate(
      prop_pos_ge_MAX =
        n_pos_ge_MAX /
        nrow(
          det_0_100_clean
        ),
      
      prop_ext_ge_threshold =
        n_ext_ge_threshold /
        nrow(
          det_0_100_clean
        )
    )
  
  
  matrix_registry <- tibble(
    analysis_sample =
      sample_name,
    
    matrix_name =
      names(matrices),
    
    intended_use = c(
      "diagnostic_original_scale",
      "kmeans_efa",
      "kmeans_efa_positive_values",
      "kmeans_efa_extreme_values",
      "kmeans_efa_relative_deviation",
      "greedy_binarisation",
      "greedy_binarisation",
      "dimension_level_analysis",
      "dimension_level_analysis",
      "dimension_level_analysis",
      "dimension_level_analysis"
    ),
    
    description = c(
      "Original 0-100 values with missing values imputed to 50.",
      
      "Original values divided by 100; missing values become 0.5.",
      
      paste0(
        "Values >= ",
        MAX_POS,
        " retain x/100; lower or missing values become neutral 0.5."
      ),
      
      paste0(
        "Absolute distance from 50; distances >= ",
        EXT_THRESHOLD,
        " retain distance/50 and the rest become 0."
      ),
      
      paste0(
        "Absolute sample-specific z-score within ",
        sample_name,
        "; missing values become 0."
      ),
      
      paste0(
        "Observed values >= ",
        MAX_POS,
        " are retained; lower and missing values are NA."
      ),
      
      paste0(
        "Observed |x-50| >= ",
        EXT_THRESHOLD,
        " are retained; non-extreme and missing values are NA."
      ),
      
      "Mean of RAW determinants within each theoretical dimension.",
      
      "Mean of POS determinants within each theoretical dimension.",
      
      "Mean of EXT determinants within each theoretical dimension.",
      
      paste0(
        "Mean of sample-specific Z_ABS determinants within each theoretical dimension for ",
        sample_name,
        "."
      )
    )
  ) %>%
    mutate(
      file = file.path(
        analysis_sample,
        paste0(
          matrix_name,
          ".csv"
        )
      )
    )
  
  
  sample_summary <- sample_df %>%
    summarise(
      analysis_sample =
        sample_name,
      
      n_rows = n(),
      
      n_unique_integrated_ids =
        n_distinct(
          integrated_row_id
        ),
      
      n_subsamples =
        n_distinct(
          subsample
        ),
      
      n_sources =
        n_distinct(
          dataset_source
        ),
      
      n_europe = sum(
        comparison_region ==
          "EUROPE"
      ),
      
      n_latam = sum(
        comparison_region ==
          "LATAM"
      ),
      
      n_diego = sum(
        subsample ==
          "DIEGO"
      ),
      
      n_renovisor = sum(
        subsample ==
          "RENOVISOR"
      ),
      
      n_why_europe = sum(
        subsample ==
          "WHY_EUROPE"
      ),
      
      n_why_latam = sum(
        subsample ==
          "WHY_LATAM"
      )
    )
  
  
  diagnostics_outputs <- list(
    "diagnostics_parameters.csv" =
      diagnostics_parameters,
    
    "diagnostics_input_missing_by_determinant.csv" =
      diagnostics_input_missing,
    
    "diagnostics_z_abs_reference.csv" =
      diagnostics_z_abs_reference,
    
    "diagnostics_matrix_summary.csv" =
      diagnostics_matrix_summary,
    
    "diagnostics_threshold_counts.csv" =
      diagnostics_threshold_counts,
    
    "matrix_registry.csv" =
      matrix_registry,
    
    "diagnostics_sample_summary.csv" =
      sample_summary
  )
  
  iwalk(
    diagnostics_outputs,
    ~ write_csv(
      .x,
      file.path(
        sample_dir,
        .y
      )
    )
  )
  
  
  # Figuras diagnósticas
  plot_matrix_summary <- diagnostics_matrix_summary %>%
    mutate(
      matrix_name = factor(
        matrix_name,
        levels = matrix_name
      )
    ) %>%
    ggplot(
      aes(
        x = matrix_name,
        y = mean_value
      )
    ) +
    geom_col() +
    geom_errorbar(
      aes(
        ymin =
          mean_value -
          sd_value,
        
        ymax =
          mean_value +
          sd_value
      ),
      width = 0.2
    ) +
    coord_flip() +
    theme_minimal(
      base_size = 12
    ) +
    labs(
      title = paste0(
        "Mean and SD by matrix - ",
        sample_name
      ),
      x = "Matrix",
      y = "Mean value ± SD"
    )
  
  save_plot(
    plot_matrix_summary,
    "01_matrix_summary_mean_sd.png",
    fig_dir
  )
  
  
  plot_thresholds <- diagnostics_threshold_counts %>%
    select(
      determinant,
      prop_pos_ge_MAX,
      prop_ext_ge_threshold
    ) %>%
    pivot_longer(
      c(
        prop_pos_ge_MAX,
        prop_ext_ge_threshold
      ),
      names_to =
        "threshold_type",
      values_to =
        "prop"
    ) %>%
    mutate(
      threshold_type = recode(
        threshold_type,
        
        prop_pos_ge_MAX =
          paste0(
            "Positive >= ",
            MAX_POS
          ),
        
        prop_ext_ge_threshold =
          paste0(
            "Extreme |x-50| >= ",
            EXT_THRESHOLD
          )
      ),
      
      determinant = factor(
        determinant,
        levels = rev(
          det_cols
        )
      )
    ) %>%
    ggplot(
      aes(
        x = determinant,
        y = prop,
        fill = threshold_type
      )
    ) +
    geom_col(
      position = "dodge"
    ) +
    coord_flip() +
    theme_minimal(
      base_size = 10
    ) +
    labs(
      title = paste0(
        "Positive/extreme thresholds - ",
        sample_name
      ),
      x = "Determinant",
      y = "Share of rows",
      fill = "Threshold"
    )
  
  save_plot(
    plot_thresholds,
    "02_threshold_counts_by_determinant.png",
    fig_dir,
    width = 12,
    height = 9
  )
  
  
  matrix_values_long <- bind_rows(
    matrix_raw_0_1 %>%
      mutate(
        matrix_name =
          "matrix_32_raw_0_1"
      ),
    
    matrix_pos_0_1 %>%
      mutate(
        matrix_name =
          "matrix_32_pos_0_1"
      ),
    
    matrix_ext_0_1 %>%
      mutate(
        matrix_name =
          "matrix_32_ext_0_1"
      ),
    
    matrix_z_abs %>%
      mutate(
        matrix_name =
          "matrix_32_z_abs"
      )
  ) %>%
    pivot_longer(
      all_of(
        det_cols
      ),
      names_to =
        "determinant",
      values_to =
        "value"
    )
  
  plot_matrix_boxplot <- ggplot(
    matrix_values_long,
    aes(
      x = matrix_name,
      y = value,
      fill = matrix_name
    )
  ) +
    geom_boxplot(
      outlier.alpha = 0.15
    ) +
    coord_flip() +
    theme_minimal(
      base_size = 12
    ) +
    guides(
      fill = "none"
    ) +
    labs(
      title = paste0(
        "Distribution of values by matrix - ",
        sample_name
      ),
      x = "Matrix",
      y = "Value"
    )
  
  save_plot(
    plot_matrix_boxplot,
    "03_matrix_value_boxplot_32det.png",
    fig_dir
  )
  
  
  cat(
    "\nMuestra: ",
    sample_name,
    "\nN: ",
    nrow(sample_df),
    "\n",
    sep = ""
  )
  
  if (
    sample_name %in%
    c(
      "COMPLETE",
      "EUROPE",
      "LATAM"
    )
  ) {
    cat(
      "\nComposición por submuestra:\n"
    )
    
    print(
      sample_df %>%
        count(
          comparison_region,
          subsample,
          dataset_source,
          name = "n"
        ) %>%
        mutate(
          prop =
            n /
            sum(n)
        ),
      n = Inf,
      width = Inf
    )
  }
  
  list(
    sample_summary =
      sample_summary,
    
    matrix_summary =
      diagnostics_matrix_summary,
    
    z_abs_reference =
      diagnostics_z_abs_reference,
    
    threshold_counts =
      diagnostics_threshold_counts,
    
    matrix_registry =
      matrix_registry
  )
}


# Ejecutar las siete muestras
results <- map(
  ANALYSIS_SAMPLES,
  build_sample_matrices
) %>%
  set_names(
    ANALYSIS_SAMPLES
  )


# Resúmenes globales
diagnostics_analysis_samples <- bind_rows(
  map(
    results,
    "sample_summary"
  )
)

diagnostics_matrix_summary_all_samples <- bind_rows(
  map(
    results,
    "matrix_summary"
  )
)

diagnostics_z_abs_reference_all_samples <- bind_rows(
  map(
    results,
    "z_abs_reference"
  )
)

diagnostics_threshold_counts_all_samples <- bind_rows(
  map(
    results,
    "threshold_counts"
  )
)

matrix_registry_all_samples <- bind_rows(
  map(
    results,
    "matrix_registry"
  )
)

diagnostics_sample_size_check <- diagnostics_analysis_samples %>%
  select(
    analysis_sample,
    n_rows,
    n_unique_integrated_ids,
    n_subsamples,
    n_sources,
    n_europe,
    n_latam,
    n_diego,
    n_renovisor,
    n_why_europe,
    n_why_latam
  )


# Checks de arquitectura
analysis_ids <- map(
  ANALYSIS_SAMPLES,
  ~ get_analysis_sample(
    df_all,
    .x
  ) %>%
    pull(
      integrated_row_id
    )
) %>%
  set_names(
    ANALYSIS_SAMPLES
  )

check_complete <- setequal(
  analysis_ids$COMPLETE,
  union(
    analysis_ids$EUROPE,
    analysis_ids$LATAM
  )
)

check_europe <- setequal(
  analysis_ids$EUROPE,
  unique(
    c(
      analysis_ids$DIEGO,
      analysis_ids$RENOVISOR,
      analysis_ids$WHY_EUROPE
    )
  )
)

check_latam <- setequal(
  analysis_ids$LATAM,
  analysis_ids$WHY_LATAM
)

check_no_overlap <-
  !length(
    intersect(
      analysis_ids$EUROPE,
      analysis_ids$LATAM
    )
  )

diagnostics_structure_checks <- tibble(
  check = c(
    "COMPLETE_equals_EUROPE_plus_LATAM",
    "EUROPE_equals_DIEGO_plus_RENOVISOR_plus_WHY_EUROPE",
    "LATAM_equals_WHY_LATAM",
    "EUROPE_LATAM_no_overlap"
  ),
  
  passed = c(
    check_complete,
    check_europe,
    check_latam,
    check_no_overlap
  )
)

if (
  any(
    !diagnostics_structure_checks$passed
  )
) {
  print(
    diagnostics_structure_checks,
    n = Inf
  )
  
  stop(
    "La estructura de las muestras de referencia no es coherente."
  )
}


# Guardado global
global_outputs <- list(
  "diagnostics_analysis_samples.csv" =
    diagnostics_analysis_samples,
  
  "diagnostics_sample_size_check.csv" =
    diagnostics_sample_size_check,
  
  "diagnostics_structure_checks.csv" =
    diagnostics_structure_checks,
  
  "diagnostics_matrix_summary_all_samples.csv" =
    diagnostics_matrix_summary_all_samples,
  
  "diagnostics_z_abs_reference_all_samples.csv" =
    diagnostics_z_abs_reference_all_samples,
  
  "diagnostics_threshold_counts_all_samples.csv" =
    diagnostics_threshold_counts_all_samples,
  
  "matrix_registry_all_samples.csv" =
    matrix_registry_all_samples,
  
  "dimension_determinant_map_used.csv" =
    dimension_determinant_map
)

iwalk(
  global_outputs,
  ~ write_csv(
    .x,
    file.path(
      out_dir,
      .y
    )
  )
)


# Figura global
plot_sample_sizes <- diagnostics_analysis_samples %>%
  ggplot(
    aes(
      x = reorder(
        analysis_sample,
        n_rows
      ),
      y = n_rows
    )
  ) +
  geom_col() +
  geom_text(
    aes(
      label = n_rows
    ),
    hjust = -0.15,
    size = 4
  ) +
  coord_flip() +
  theme_minimal(
    base_size = 13
  ) +
  labs(
    title = "Reference sample sizes for clustering analyses",
    subtitle = "Unique usable participants before applying the bootstrap indices",
    x = "Analysis sample",
    y = "Number of respondents"
  )

save_plot(
  plot_sample_sizes,
  "00_analysis_sample_sizes.png",
  out_dir,
  width = 9
)


# Resumen
cat(
  "\n05. MATRICES PARA CLUSTERING COMPLETADAS\n"
)

cat(
  "\nArchivo de entrada: ",
  in_file,
  "\n",
  sep = ""
)

cat(
  "\nMUESTRAS DE REFERENCIA\n\n"
)

print(
  diagnostics_analysis_samples,
  n = Inf,
  width = Inf
)

cat(
  "\nCOMPROBACIONES DE ESTRUCTURA\n\n"
)

print(
  diagnostics_structure_checks,
  n = Inf,
  width = Inf
)

cat(
  "\nZ_ABS: medias y SD calculadas separadamente para cada muestra de referencia.\n"
)

cat(
  "\nMatrices guardadas en: ",
  out_dir,
  "\n",
  sep = ""
)

message(
  "\nListo. Matrices generadas para COMPLETE, regiones y submuestras."
)