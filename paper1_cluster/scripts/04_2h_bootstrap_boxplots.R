# 
# Objetivo
# Representar la variabilidad de las distribuciones categóricas
# a través de las réplicas del bootstrap final de 04_2f.
#
# Muestras:
# - COMPLETE
# - EUROPE
# - LATAM
# - DIEGO
# - RENOVISOR
# - WHY_EUROPE
# - WHY_LATAM
#
# Cada boxplot representa la proporción de una categoría en las
# distintas réplicas bootstrap. Este script no vuelve a remuestrear.

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

# Configuración
project_root <- path.expand("~/Desktop/MASTER/recommendation-engine/TFM")
processed_root <- file.path(project_root, "paper1_cluster/data/processed")

input_file <- file.path(
  processed_root,
  "03_2_phase_dimension_scores",
  "all_sources_integrated_component_quality_phase_scores.csv"
)

bootstrap_index_file <- file.path(
  processed_root,
  "04_2f_combine_eu_latam_bootstrap",
  "bootstrap_samples_index.csv.gz"
)

out_dir <- file.path(processed_root, "04_2h_bootstrap_boxplots")
fig_dir <- file.path(out_dir, "figures")

walk(c(out_dir, fig_dir), ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))

EXPECTED_N_BOOT <- 1000L
MAX_BOOTSTRAPS <- Inf

ANALYSIS_SAMPLES <- c(
  "COMPLETE",
  "EUROPE",
  "LATAM",
  "DIEGO",
  "RENOVISOR",
  "WHY_EUROPE",
  "WHY_LATAM"
)

candidate_vars <- c(
  "dataset_source",
  "subsample",
  "comparison_region",
  "source_survey",
  "country_model",
  "country_model_grouped",
  "country_region_model",
  "age_group_model",
  "gender_model",
  "education_model",
  "education_tfm_model",
  "employment_model",
  "city_size_model",
  "housing_tenure_model",
  "tenure_model",
  "tenure_tfm_model",
  "income_model",
  "vote_status_declared",
  "voted_observed",
  "political_left_right_model",
  "political_block_model",
  "electoral_group_model",
  "row_quality_final",
  "usable_for_main_analysis",
  "usable_for_clustering"
)

bootstrap_specific_vars <- c(
  "bootstrap_source",
  "target_sample_type",
  "target_electoral_group",
  "country_code",
  "country",
  "income_band"
)

archetype_var <- "self_classification_archetype_model"

# Funciones auxiliares
clean_category <- function(x) {
  x <- str_squish(as.character(x))
  x[is.na(x) | x == ""] <- "Missing"
  x
}

safe_filename <- function(x) {
  x %>%
    str_replace_all("[^A-Za-z0-9_]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_remove_all("^_|_$")
}

main_palette <- c(
  "#0072B2",
  "#56B4E9",
  "#009E73",
  "#E69F00",
  "#F0E442",
  "#D55E00",
  "#CC79A7",
  "#999999",
  "#332288",
  "#88CCEE",
  "#44AA99",
  "#117733",
  "#DDCC77",
  "#CC6677",
  "#882255",
  "#AA4499"
)

category_palette <- function(categories) {
  categories <- unique(as.character(categories))
  categories <- categories[!is.na(categories)]
  
  palette <- rep(main_palette, length.out = length(categories))
  names(palette) <- categories
  palette
}

find_self_classification_col <- function(data_names) {
  priority_cols <- c(
    "self_classification_raw_clean",
    "self_classification_raw"
  )
  
  available_priority <- priority_cols[priority_cols %in% data_names]
  
  if (length(available_priority)) {
    return(available_priority[1])
  }
  
  candidates <- data_names[
    str_detect(
      data_names,
      regex(
        paste(
          c(
            "4\\.3",
            "statement.*describes",
            "investment.*decision",
            "household.*decision",
            "autoclas"
          ),
          collapse = "|"
        ),
        ignore_case = TRUE
      )
    )
  ]
  
  candidates <- setdiff(candidates, archetype_var)
  
  if (length(candidates)) {
    candidates[1]
  } else {
    NA_character_
  }
}

add_archetype_classification <- function(data, self_classification_col) {
  if (is.na(self_classification_col) || !self_classification_col %in% names(data)) {
    if (!"self_classification_raw_clean" %in% names(data)) {
      data$self_classification_raw_clean <- NA_character_
    }
    
    if (!archetype_var %in% names(data)) {
      data[[archetype_var]] <- NA_character_
    }
    
    return(data)
  }
  
  data %>%
    mutate(
      self_classification_raw_clean = str_squish(as.character(.data[[self_classification_col]])),
      self_classification_archetype_model = case_when(
        str_detect(
          self_classification_raw_clean,
          regex("environmental impact|impacto ambiental|reduce.*environmental", ignore_case = TRUE)
        ) ~ "Activist",
        
        str_detect(
          self_classification_raw_clean,
          regex("safety|seguridad", ignore_case = TRUE)
        ) ~ "Fearful",
        
        str_detect(
          self_classification_raw_clean,
          regex("social status|colleagues|friends|family|estatus social|norms|normas", ignore_case = TRUE)
        ) ~ "Influencer",
        
        str_detect(
          self_classification_raw_clean,
          regex("comfort|well-being|bienestar|confort", ignore_case = TRUE)
        ) ~ "Careful",
        
        str_detect(
          self_classification_raw_clean,
          regex("not very interested|stick to what|no.*interes|no muy interesado|used to", ignore_case = TRUE)
        ) ~ "Uninterested",
        
        str_detect(
          self_classification_raw_clean,
          regex("early adopter|latest trends|tendencias|pioner", ignore_case = TRUE)
        ) ~ "Early adopter",
        
        str_detect(
          self_classification_raw_clean,
          regex("ethical|social commitment|compromiso.*social|environmental protection|protecci", ignore_case = TRUE)
        ) ~ "Stubborn",
        
        str_detect(
          self_classification_raw_clean,
          regex("cost-effective|well informed|costs and benefits|costes y beneficios|costos y beneficios|rentable", ignore_case = TRUE)
        ) ~ "Homo economicus",
        
        str_detect(
          self_classification_raw_clean,
          regex("none of the above|Νone of the above|none|ninguna", ignore_case = TRUE)
        ) ~ "None",
        
        is.na(self_classification_raw_clean) | self_classification_raw_clean == "" ~ NA_character_,
        TRUE ~ "Other_unclassified"
      )
    )
}

compute_bootstrap_distribution <- function(boot_df, var, drop_missing = FALSE) {
  boot_ids <- sort(unique(boot_df$bootstrap_id))
  
  base <- boot_df %>%
    transmute(
      bootstrap_id,
      comparison_region,
      subsample,
      category = clean_category(.data[[var]])
    )
  
  if (drop_missing) {
    base <- base %>% filter(category != "Missing")
  }
  
  if (!nrow(base)) {
    return(tibble())
  }
  
  counts_complete <- base %>%
    count(bootstrap_id, category, name = "n") %>%
    mutate(analysis_sample = "COMPLETE")
  
  counts_region <- base %>%
    count(bootstrap_id, comparison_region, category, name = "n") %>%
    transmute(
      bootstrap_id,
      analysis_sample = comparison_region,
      category,
      n
    )
  
  counts_subsample <- base %>%
    count(bootstrap_id, subsample, category, name = "n") %>%
    transmute(
      bootstrap_id,
      analysis_sample = subsample,
      category,
      n
    )
  
  counts <- bind_rows(
    counts_complete,
    counts_region,
    counts_subsample
  ) %>%
    filter(analysis_sample %in% ANALYSIS_SAMPLES)
  
  if (!nrow(counts)) {
    return(tibble())
  }
  
  counts %>%
    group_by(analysis_sample) %>%
    group_modify(
      ~ {
        categories_current <- sort(unique(.x$category))
        
        complete(
          .x,
          bootstrap_id = boot_ids,
          category = categories_current,
          fill = list(n = 0L)
        )
      }
    ) %>%
    ungroup() %>%
    group_by(analysis_sample, bootstrap_id) %>%
    mutate(
      total_n = sum(n),
      prop = if_else(total_n > 0, n / total_n, NA_real_)
    ) %>%
    ungroup() %>%
    mutate(
      variable = var,
      analysis_sample = factor(analysis_sample, levels = ANALYSIS_SAMPLES)
    ) %>%
    select(
      analysis_sample,
      bootstrap_id,
      variable,
      category,
      n,
      total_n,
      prop
    )
}

summarise_bootstrap_distribution <- function(distribution_df) {
  if (!nrow(distribution_df)) {
    return(tibble())
  }
  
  distribution_df %>%
    group_by(analysis_sample, variable, category) %>%
    summarise(
      n_bootstraps = n_distinct(bootstrap_id),
      mean_n = mean(n, na.rm = TRUE),
      sd_n = sd(n, na.rm = TRUE),
      mean_total_n = mean(total_n, na.rm = TRUE),
      min_total_n = min(total_n, na.rm = TRUE),
      max_total_n = max(total_n, na.rm = TRUE),
      mean_prop = mean(prop, na.rm = TRUE),
      sd_prop = sd(prop, na.rm = TRUE),
      min_prop = min(prop, na.rm = TRUE),
      q25_prop = as.numeric(quantile(prop, 0.25, na.rm = TRUE)),
      median_prop = median(prop, na.rm = TRUE),
      q75_prop = as.numeric(quantile(prop, 0.75, na.rm = TRUE)),
      max_prop = max(prop, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(variable, analysis_sample, desc(median_prop), category)
}

make_boxplot <- function(distribution_df, var, filename_prefix = "bootstrap_boxplot") {
  p_data <- distribution_df %>%
    filter(variable == var, !is.na(prop)) %>%
    mutate(
      analysis_sample = factor(analysis_sample, levels = ANALYSIS_SAMPLES),
      category = fct_reorder(category, prop, .fun = median, .desc = FALSE)
    )
  
  if (!nrow(p_data)) {
    return(invisible(NULL))
  }
  
  n_boot <- n_distinct(p_data$bootstrap_id)
  n_categories <- n_distinct(p_data$category)
  
  plot_height <- max(8, min(16, 5 + 0.35 * n_categories))
  
  p <- ggplot(
    p_data,
    aes(
      x = category,
      y = prop,
      fill = category
    )
  ) +
    geom_boxplot(
      outlier.alpha = 0.20,
      width = 0.65,
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values = category_palette(p_data$category),
      guide = "none"
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 1)
    ) +
    coord_flip() +
    facet_wrap(
      ~ analysis_sample,
      ncol = 2,
      scales = "free_x"
    ) +
    theme_minimal(base_size = 10) +
    labs(
      title = paste0("Bootstrap distribution of ", var),
      subtitle = paste0(
        "Percentage of each category across ",
        n_boot,
        " bootstrap replicates"
      ),
      x = NULL,
      y = "Percentage in each bootstrap sample"
    )
  
  ggsave(
    file.path(
      fig_dir,
      paste0(filename_prefix, "_", safe_filename(var), ".png")
    ),
    plot = p,
    width = 14,
    height = plot_height,
    dpi = 300
  )
}

# Dataset descriptivo
if (!file.exists(input_file)) {
  stop("No encuentro el archivo de entrada:\n", input_file)
}

if (!file.exists(bootstrap_index_file)) {
  stop("No encuentro el bootstrap final de 04_2f:\n", bootstrap_index_file)
}

all_colnames <- names(
  read_csv(
    input_file,
    n_max = 0,
    show_col_types = FALSE
  )
)

if (!"integrated_row_id" %in% all_colnames) {
  stop("El dataset descriptivo no contiene integrated_row_id.")
}

self_classification_col <- find_self_classification_col(all_colnames)

self_classification_candidates <- all_colnames[
  str_detect(
    all_colnames,
    regex(
      paste(
        c(
          "4\\.3",
          "statement.*describes",
          "investment.*decision",
          "household.*decision",
          "self.*classification",
          "autoclas"
        ),
        collapse = "|"
      ),
      ignore_case = TRUE
    )
  )
]

needed_cols <- unique(
  c(
    "integrated_row_id",
    candidate_vars,
    archetype_var,
    self_classification_col,
    self_classification_candidates
  )
)

needed_cols <- needed_cols[!is.na(needed_cols)]
needed_cols <- intersect(needed_cols, all_colnames)

df <- read_csv(
  input_file,
  col_select = all_of(needed_cols),
  show_col_types = FALSE
) %>%
  mutate(
    integrated_row_id = as.character(integrated_row_id)
  )

if (anyDuplicated(df$integrated_row_id)) {
  stop("integrated_row_id contiene duplicados en el dataset descriptivo.")
}

df <- add_archetype_classification(
  data = df,
  self_classification_col = self_classification_col
)

if ("political_left_right_model" %in% names(df)) {
  df <- df %>%
    mutate(
      political_left_right_num = suppressWarnings(as.numeric(political_left_right_model)),
      political_lr_group = case_when(
        is.na(political_left_right_num) ~ NA_character_,
        political_left_right_num < 20 ~ "00_19_extreme_left",
        political_left_right_num < 40 ~ "20_39_left",
        political_left_right_num < 60 ~ "40_59_centre",
        political_left_right_num < 80 ~ "60_79_right",
        political_left_right_num <= 100 ~ "80_100_extreme_right",
        TRUE ~ NA_character_
      )
    )
}

# Esta base sí es individual: una fila por participante.
# Las repeticiones del bootstrap se conservan después del join.
df_boot_base <- df %>%
  distinct(
    integrated_row_id,
    .keep_all = TRUE
  )

# Bootstrap final
boot_index_colnames <- names(
  read_csv(
    bootstrap_index_file,
    n_max = 0,
    show_col_types = FALSE
  )
)

required_boot_cols <- c(
  "bootstrap_id",
  "draw_id",
  "integrated_row_id",
  "comparison_region",
  "subsample",
  "dataset_source"
)

missing_boot_cols <- setdiff(required_boot_cols, boot_index_colnames)

if (length(missing_boot_cols)) {
  stop(
    "Faltan columnas necesarias en el bootstrap final: ",
    paste(missing_boot_cols, collapse = ", ")
  )
}

boot_index_cols <- intersect(
  c(
    "bootstrap_id",
    "draw_id",
    "region_draw_id",
    "integrated_row_id",
    "comparison_region",
    "subsample",
    "dataset_source",
    "source_survey",
    "bootstrap_source",
    "target_sample_type",
    "target_electoral_group",
    "country_code",
    "country",
    "income_band"
  ),
  boot_index_colnames
)

boot_index <- read_csv(
  bootstrap_index_file,
  col_select = all_of(boot_index_cols),
  show_col_types = FALSE
) %>%
  mutate(
    bootstrap_id = as.integer(bootstrap_id),
    integrated_row_id = as.character(integrated_row_id),
    comparison_region = as.character(comparison_region),
    subsample = as.character(subsample),
    dataset_source = as.character(dataset_source)
  )

all_boot_ids <- sort(unique(boot_index$bootstrap_id))

if (length(all_boot_ids) != EXPECTED_N_BOOT) {
  stop(
    "Se esperaban ",
    EXPECTED_N_BOOT,
    " bootstraps en 04_2f y se han encontrado ",
    length(all_boot_ids),
    "."
  )
}

if (!identical(all_boot_ids, seq_len(EXPECTED_N_BOOT))) {
  stop(
    "Los bootstrap_id de 04_2f no son exactamente 1:",
    EXPECTED_N_BOOT,
    "."
  )
}

if (is.finite(MAX_BOOTSTRAPS)) {
  if (MAX_BOOTSTRAPS < 1) {
    stop("MAX_BOOTSTRAPS debe ser al menos 1.")
  }
  
  keep_bootstraps <- head(all_boot_ids, as.integer(MAX_BOOTSTRAPS))
  
  boot_index <- boot_index %>%
    filter(bootstrap_id %in% keep_bootstraps)
}

boot_ids <- sort(unique(boot_index$bootstrap_id))

wrong_regions <- setdiff(
  unique(na.omit(boot_index$comparison_region)),
  c("EUROPE", "LATAM")
)

if (length(wrong_regions)) {
  stop(
    "Hay regiones inesperadas en el bootstrap: ",
    paste(wrong_regions, collapse = ", ")
  )
}

wrong_subsamples <- setdiff(
  unique(na.omit(boot_index$subsample)),
  c("DIEGO", "RENOVISOR", "WHY_EUROPE", "WHY_LATAM")
)

if (length(wrong_subsamples)) {
  stop(
    "Hay submuestras inesperadas en el bootstrap: ",
    paste(wrong_subsamples, collapse = ", ")
  )
}

# Unir draws con variables descriptivas
missing_boot_ids <- boot_index %>%
  distinct(integrated_row_id) %>%
  anti_join(
    df_boot_base %>% distinct(integrated_row_id),
    by = "integrated_row_id"
  )

if (nrow(missing_boot_ids)) {
  stop(
    "Hay ",
    nrow(missing_boot_ids),
    " IDs del bootstrap que no aparecen en el dataset descriptivo."
  )
}

extra_df_cols <- setdiff(names(df_boot_base), names(boot_index))

boot_df <- boot_index %>%
  left_join(
    df_boot_base %>%
      select(
        integrated_row_id,
        all_of(extra_df_cols)
      ),
    by = "integrated_row_id"
  )

# Comprobar tamaños de las siete muestras
bootstrap_sample_sizes <- bind_rows(
  boot_df %>%
    count(bootstrap_id, name = "n_draws") %>%
    mutate(analysis_sample = "COMPLETE"),
  
  boot_df %>%
    count(bootstrap_id, comparison_region, name = "n_draws") %>%
    transmute(
      bootstrap_id,
      analysis_sample = comparison_region,
      n_draws
    ),
  
  boot_df %>%
    count(bootstrap_id, subsample, name = "n_draws") %>%
    transmute(
      bootstrap_id,
      analysis_sample = subsample,
      n_draws
    )
) %>%
  complete(
    bootstrap_id = boot_ids,
    analysis_sample = ANALYSIS_SAMPLES,
    fill = list(n_draws = 0L)
  ) %>%
  mutate(
    analysis_sample = factor(analysis_sample, levels = ANALYSIS_SAMPLES)
  ) %>%
  arrange(bootstrap_id, analysis_sample)

bootstrap_sample_sizes_wide <- bootstrap_sample_sizes %>%
  mutate(
    analysis_sample = as.character(analysis_sample)
  ) %>%
  pivot_wider(
    names_from = analysis_sample,
    values_from = n_draws,
    values_fill = 0
  )

bootstrap_structure_checks <- tibble(
  check = c(
    "COMPLETE_equals_EUROPE_plus_LATAM",
    "EUROPE_equals_DIEGO_plus_RENOVISOR_plus_WHY_EUROPE",
    "LATAM_equals_WHY_LATAM",
    "EUROPE_size_constant",
    "LATAM_size_constant",
    "COMPLETE_size_constant"
  ),
  passed = c(
    all(
      bootstrap_sample_sizes_wide$COMPLETE ==
        bootstrap_sample_sizes_wide$EUROPE +
        bootstrap_sample_sizes_wide$LATAM
    ),
    all(
      bootstrap_sample_sizes_wide$EUROPE ==
        bootstrap_sample_sizes_wide$DIEGO +
        bootstrap_sample_sizes_wide$RENOVISOR +
        bootstrap_sample_sizes_wide$WHY_EUROPE
    ),
    all(
      bootstrap_sample_sizes_wide$LATAM ==
        bootstrap_sample_sizes_wide$WHY_LATAM
    ),
    n_distinct(bootstrap_sample_sizes_wide$EUROPE) == 1,
    n_distinct(bootstrap_sample_sizes_wide$LATAM) == 1,
    n_distinct(bootstrap_sample_sizes_wide$COMPLETE) == 1
  )
)

if (any(!bootstrap_structure_checks$passed)) {
  print(bootstrap_structure_checks, n = Inf)
  stop("La estructura del bootstrap combinado no es coherente.")
}

bootstrap_sample_sizes_summary <- bootstrap_sample_sizes %>%
  group_by(analysis_sample) %>%
  summarise(
    mean_n = mean(n_draws),
    sd_n = sd(n_draws),
    min_n = min(n_draws),
    max_n = max(n_draws),
    n_bootstraps = n_distinct(bootstrap_id),
    .groups = "drop"
  )

write_csv(
  bootstrap_sample_sizes,
  file.path(out_dir, "bootstrap_sample_sizes_by_run.csv")
)

write_csv(
  bootstrap_sample_sizes_summary,
  file.path(out_dir, "bootstrap_sample_sizes_summary.csv")
)

write_csv(
  bootstrap_structure_checks,
  file.path(out_dir, "bootstrap_structure_checks.csv")
)

# Variables finales
boxplot_vars <- unique(
  c(
    candidate_vars,
    bootstrap_specific_vars,
    "political_lr_group",
    archetype_var
  )
)

boxplot_vars <- intersect(boxplot_vars, names(boot_df))

write_csv(
  tibble(variable = boxplot_vars),
  file.path(out_dir, "bootstrap_boxplot_variables_used.csv")
)

# Distribuciones bootstrap
bootstrap_boxplot_distribution_all <- map_dfr(
  boxplot_vars,
  ~ compute_bootstrap_distribution(
    boot_df = boot_df,
    var = .x,
    drop_missing = FALSE
  )
)

bootstrap_boxplot_summary_all <- summarise_bootstrap_distribution(
  bootstrap_boxplot_distribution_all
)

write_csv(
  bootstrap_boxplot_distribution_all,
  file.path(out_dir, "bootstrap_boxplot_distribution_by_run_all_variables.csv")
)

write_csv(
  bootstrap_boxplot_summary_all,
  file.path(out_dir, "bootstrap_boxplot_summary_all_variables.csv")
)

# Boxplots generales

for (var in boxplot_vars) {
  make_boxplot(
    distribution_df = bootstrap_boxplot_distribution_all,
    var = var
  )
}

# Arquetipos sin Missing
#
# El denominador en esta salida son únicamente las personas con
# autoclasificación disponible. Como esta variable procede de RV,
# no todas las muestras tendrán un panel informativo.
bootstrap_archetype_distribution_non_missing <- tibble()
bootstrap_archetype_summary_non_missing <- tibble()

if (archetype_var %in% names(boot_df)) {
  bootstrap_archetype_distribution_non_missing <- compute_bootstrap_distribution(
    boot_df = boot_df,
    var = archetype_var,
    drop_missing = TRUE
  )
  
  bootstrap_archetype_summary_non_missing <- summarise_bootstrap_distribution(
    bootstrap_archetype_distribution_non_missing
  )
  
  write_csv(
    bootstrap_archetype_distribution_non_missing,
    file.path(out_dir, "bootstrap_archetype_distribution_by_run_non_missing.csv")
  )
  
  write_csv(
    bootstrap_archetype_summary_non_missing,
    file.path(out_dir, "bootstrap_archetype_boxplot_summary_non_missing.csv")
  )
  
  make_boxplot(
    distribution_df = bootstrap_archetype_distribution_non_missing,
    var = archetype_var,
    filename_prefix = "bootstrap_boxplot_archetypes_non_missing"
  )
}

# Parámetros
parameters <- tibble(
  parameter = c(
    "input_file",
    "bootstrap_index_file",
    "output_dir",
    "figures_dir",
    "expected_n_bootstraps",
    "max_bootstraps",
    "n_bootstraps_used",
    "n_bootstrap_rows_used",
    "analysis_samples",
    "self_classification_col",
    "boxplot_vars"
  ),
  value = c(
    input_file,
    bootstrap_index_file,
    out_dir,
    fig_dir,
    as.character(EXPECTED_N_BOOT),
    as.character(MAX_BOOTSTRAPS),
    as.character(n_distinct(boot_df$bootstrap_id)),
    as.character(nrow(boot_df)),
    paste(ANALYSIS_SAMPLES, collapse = ", "),
    as.character(self_classification_col),
    paste(boxplot_vars, collapse = ", ")
  )
)

write_csv(
  parameters,
  file.path(out_dir, "bootstrap_boxplot_parameters.csv")
)

# Resumen
cat("\n04_2h. BOXPLOTS DEL BOOTSTRAP FINAL COMPLETADOS\n")

cat(
  "\nBootstrap: ",
  bootstrap_index_file,
  "\nBootstraps usados: ",
  n_distinct(boot_df$bootstrap_id),
  "\nDraws usados: ",
  nrow(boot_df),
  "\n",
  sep = ""
)

cat("\nTAMAÑOS POR MUESTRA\n\n")

print(
  bootstrap_sample_sizes_summary,
  n = Inf,
  width = Inf
)

cat("\nCHECKS DE ESTRUCTURA\n\n")

print(
  bootstrap_structure_checks,
  n = Inf,
  width = Inf
)

cat("\nVARIABLES CON BOXPLOT\n\n")
print(boxplot_vars)

cat(
  "\nAutoclasificación detectada en: ",
  self_classification_col,
  "\n",
  sep = ""
)

cat(
  "\nOutputs: ",
  out_dir,
  "\nFiguras: ",
  fig_dir,
  "\n",
  sep = ""
)

message(
  "\nListo. 04_2h generado con el bootstrap final EUROPE + LATAM."
)