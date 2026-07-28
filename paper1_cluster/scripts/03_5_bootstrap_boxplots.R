
# Objetivo:
# Sacar boxplots del porcentaje de cada categoría en cada muestra bootstrap.
#
# Especialmente:
# - Boxplot de arquetipos de autoclasificación RV.
# - Boxplots para todas las variables sociodemográficas/políticas/calidad.
#
#
# Salidas:
# - CSV/tablas -> paper1_cluster/outputs/03_5_bootstrap_boxplots
# - Figuras    -> paper1_cluster/figures/03_5_bootstrap_boxplots

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(forcats)
})


# Paths
project_root <- path.expand("~/Desktop/MASTER/recommendation-engine/TFM")
paper_root <- file.path(project_root, "paper1_cluster")

processed_root <- file.path(paper_root, "data", "processed")
outputs_root <- file.path(paper_root, "outputs")
figures_root <- file.path(paper_root, "figures")

input_file <- file.path(
  processed_root,
  "03_2_phase_dimension_scores",
  "all_sources_integrated_component_quality_phase_scores.csv"
)

bootstrap_scenario <- "04_2_propensity_bootstrap_eu_pfe_esn_renew_greens_merged"

bootstrap_index_file <- file.path(
  processed_root,
  bootstrap_scenario,
  "bootstrap_samples_index.csv"
)

bootstrap_dataset_file <- file.path(
  processed_root,
  bootstrap_scenario,
  "dataset_with_propensity_and_electoral_groups.csv"
)

out_dir <- file.path(
  outputs_root,
  "03_5_bootstrap_boxplots"
)

fig_dir <- file.path(
  figures_root,
  "03_5_bootstrap_boxplots"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# Si quieres probar solo con pocos bootstraps, pon por ejemplo 20.
# Para correr todo, déjalo en Inf.
MAX_BOOTSTRAPS <- Inf


# variables para boxplot
candidate_vars <- c(
  "dataset_source",
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
  "target_electoral_group",
  "target_sample_type",
  "row_quality_final",
  "usable_for_main_analysis",
  "usable_for_clustering"
)

# Para autoclasificación, usamos el arquetipo ya recodificado.
# No meto self_classification_raw en los boxplots porque son textos largos y no sirve bien como figura.
archetype_var <- "self_classification_archetype_model"


# Funciones auxiliares
clean_category <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x[x == "" | is.na(x)] <- "Missing"
  x
}

safe_filename <- function(x) {
  x %>%
    str_replace_all("[^A-Za-z0-9_]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_remove_all("^_|_$")
}

main_palette <- c(
  "#0072B2", "#56B4E9", "#009E73", "#E69F00",
  "#F0E442", "#D55E00", "#CC79A7", "#999999",
  "#332288", "#88CCEE", "#44AA99", "#117733",
  "#DDCC77", "#CC6677", "#882255", "#AA4499"
)

category_palette <- function(categories) {
  
  cats <- if (is.factor(categories)) {
    levels(categories)
  } else {
    unique(as.character(categories))
  }
  
  cats <- cats[!is.na(cats)]
  
  pal <- rep(main_palette, length.out = length(cats))
  names(pal) <- cats
  
  pal
}

find_self_classification_col <- function(data_names) {
  
  priority_cols <- c(
    "self_classification_raw_clean",
    "self_classification_raw"
  )
  
  existing_priority_cols <- priority_cols[priority_cols %in% data_names]
  
  if (length(existing_priority_cols) > 0) {
    return(existing_priority_cols[1])
  }
  
  candidates_by_name <- data_names[
    str_detect(
      data_names,
      regex(
        "4\\.3|statement.*describes|investment.*decision|household.*decision|self.*classification|autoclas",
        ignore_case = TRUE
      )
    )
  ]
  
  if (length(candidates_by_name) > 0) {
    return(candidates_by_name[1])
  }
  
  NA_character_
}

add_archetype_classification <- function(data, self_classification_col) {
  
  if (is.na(self_classification_col) || !self_classification_col %in% names(data)) {
    return(
      data %>%
        mutate(
          self_classification_raw_clean = NA_character_,
          self_classification_archetype_model = NA_character_
        )
    )
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
  
  data_var <- boot_df %>%
    transmute(
      bootstrap_id = bootstrap_id,
      category = clean_category(.data[[var]])
    )
  
  if (drop_missing) {
    data_var <- data_var %>%
      filter(category != "Missing")
  }
  
  if (nrow(data_var) == 0) {
    return(tibble())
  }
  
  all_bootstraps <- sort(unique(data_var$bootstrap_id))
  all_categories <- sort(unique(data_var$category))
  
  data_var %>%
    count(bootstrap_id, category, name = "n") %>%
    tidyr::complete(
      bootstrap_id = all_bootstraps,
      category = all_categories,
      fill = list(n = 0)
    ) %>%
    group_by(bootstrap_id) %>%
    mutate(
      total_n = sum(n),
      prop = if_else(total_n > 0, n / total_n, NA_real_)
    ) %>%
    ungroup() %>%
    mutate(variable = var) %>%
    select(bootstrap_id, variable, category, n, total_n, prop)
}

make_boxplot <- function(distribution_df, var, fig_dir, filename_prefix = "06_bootstrap_boxplot") {
  
  p_data <- distribution_df %>%
    filter(variable == var) %>%
    filter(!is.na(prop)) %>%
    mutate(
      category = fct_reorder(category, prop, .fun = median, .desc = FALSE)
    )
  
  if (nrow(p_data) == 0) return(invisible(NULL))
  
  n_boot <- n_distinct(p_data$bootstrap_id)
  n_sample <- round(mean(unique(p_data$total_n), na.rm = TRUE), 0)
  n_categories <- n_distinct(p_data$category)
  
  plot_height <- max(6, min(12, 2.5 + 0.45 * n_categories))
  
  p <- ggplot(
    p_data,
    aes(x = category, y = prop, fill = category)
  ) +
    geom_boxplot(
      outlier.alpha = 0.25,
      width = 0.65,
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values = category_palette(p_data$category),
      guide = "none"
    ) +
    coord_flip() +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    theme_minimal(base_size = 12) +
    labs(
      title = paste0("Bootstrap distribution of ", var),
      subtitle = paste0(
        "Percentage of each category across bootstrap samples",
        " | n muestra bootstrap = ", n_sample,
        " | n bootstraps = ", n_boot
      ),
      x = NULL,
      y = "Percentage in each bootstrap sample"
    )
  
  ggsave(
    filename = file.path(
      fig_dir,
      paste0(filename_prefix, "_", safe_filename(var), ".png")
    ),
    plot = p,
    width = 10,
    height = plot_height,
    dpi = 300
  )
}


# Lectura ligera del dataset integrado
if (!file.exists(input_file)) {
  stop("No encuentro archivo de entrada: ", input_file)
}

if (!file.exists(bootstrap_index_file)) {
  stop("No encuentro bootstrap index: ", bootstrap_index_file)
}

all_colnames <- names(
  read_csv(
    input_file,
    n_max = 0,
    show_col_types = FALSE
  )
)

self_classification_col <- find_self_classification_col(all_colnames)

needed_cols <- unique(c(
  "integrated_row_id",
  candidate_vars,
  self_classification_col
))

needed_cols <- needed_cols[needed_cols %in% all_colnames]

df <- read_csv(
  input_file,
  col_select = any_of(needed_cols),
  show_col_types = FALSE
) %>%
  mutate(
    integrated_row_id = as.character(integrated_row_id)
  )

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

df_boot_base <- df %>%
  distinct(integrated_row_id, .keep_all = TRUE)

cat("\n============================================================\n")
cat("BOOTSTRAP BOXPLOTS\n")
cat("============================================================\n")
cat("Archivo integrado:\n", input_file, "\n")
cat("Filas leídas:", nrow(df), "\n")
cat("Columnas leídas:", ncol(df), "\n")
cat("Columna autoclasificación:", self_classification_col, "\n")


#  Lectura ligera del bootstrap inde
boot_index_colnames <- names(
  read_csv(
    bootstrap_index_file,
    n_max = 0,
    show_col_types = FALSE
  )
)

boot_index_cols <- c(
  "bootstrap_id",
  "integrated_row_id",
  "target_electoral_group",
  "target_sample_type"
)

boot_index_cols <- boot_index_cols[boot_index_cols %in% boot_index_colnames]

boot_index <- read_csv(
  bootstrap_index_file,
  col_select = any_of(boot_index_cols),
  show_col_types = FALSE
) %>%
  mutate(
    bootstrap_id = as.integer(bootstrap_id),
    integrated_row_id = as.character(integrated_row_id)
  )

if (is.finite(MAX_BOOTSTRAPS)) {
  keep_bootstraps <- sort(unique(boot_index$bootstrap_id))[1:MAX_BOOTSTRAPS]
  
  boot_index <- boot_index %>%
    filter(bootstrap_id %in% keep_bootstraps)
}

boot_index <- boot_index %>%
  rename_with(
    .fn = ~ paste0(.x, "_boot"),
    .cols = -c(bootstrap_id, integrated_row_id)
  )

cat("Bootstrap index:\n", bootstrap_index_file, "\n")
cat("Filas bootstrap index:", nrow(boot_index), "\n")
cat("N bootstraps:", n_distinct(boot_index$bootstrap_id), "\n")


# Añadir variables extra del dataset con propensión, si existen
if (file.exists(bootstrap_dataset_file)) {
  
  boot_dataset_colnames <- names(
    read_csv(
      bootstrap_dataset_file,
      n_max = 0,
      show_col_types = FALSE
    )
  )
  
  boot_dataset_cols <- c(
    "integrated_row_id",
    "target_electoral_group",
    "target_sample_type"
  )
  
  boot_dataset_cols <- boot_dataset_cols[boot_dataset_cols %in% boot_dataset_colnames]
  
  if (length(boot_dataset_cols) > 1) {
    
    boot_dataset <- read_csv(
      bootstrap_dataset_file,
      col_select = any_of(boot_dataset_cols),
      show_col_types = FALSE
    ) %>%
      mutate(
        integrated_row_id = as.character(integrated_row_id)
      ) %>%
      distinct(integrated_row_id, .keep_all = TRUE)
    
    extra_cols <- setdiff(names(boot_dataset), names(df_boot_base))
    
    if (length(extra_cols) > 0) {
      df_boot_base <- df_boot_base %>%
        left_join(
          boot_dataset %>%
            select(integrated_row_id, all_of(extra_cols)),
          by = "integrated_row_id"
        )
    }
  }
}

# Unir bootstrap con variables
boot_df <- boot_index %>%
  left_join(
    df_boot_base,
    by = "integrated_row_id"
  )

if ("target_electoral_group_boot" %in% names(boot_df)) {
  if ("target_electoral_group" %in% names(boot_df)) {
    boot_df <- boot_df %>%
      mutate(
        target_electoral_group = coalesce(
          as.character(target_electoral_group_boot),
          as.character(target_electoral_group)
        )
      )
  } else {
    boot_df <- boot_df %>%
      mutate(target_electoral_group = as.character(target_electoral_group_boot))
  }
}

if ("target_sample_type_boot" %in% names(boot_df)) {
  if ("target_sample_type" %in% names(boot_df)) {
    boot_df <- boot_df %>%
      mutate(
        target_sample_type = coalesce(
          as.character(target_sample_type_boot),
          as.character(target_sample_type)
        )
      )
  } else {
    boot_df <- boot_df %>%
      mutate(target_sample_type = as.character(target_sample_type_boot))
  }
}


# Variables finales para boxplot
boxplot_vars <- unique(c(
  candidate_vars,
  "political_lr_group",
  archetype_var
))

boxplot_vars <- boxplot_vars[boxplot_vars %in% names(boot_df)]

cat("\nVariables para boxplot:\n")
print(boxplot_vars)

write_csv(
  tibble(variable = boxplot_vars),
  file.path(out_dir, "bootstrap_boxplot_variables_used.csv")
)


# Distribución bootstrap para todas las variables
bootstrap_boxplot_distribution_all <- map_dfr(
  boxplot_vars,
  function(var) {
    compute_bootstrap_distribution(
      boot_df = boot_df,
      var = var,
      drop_missing = FALSE
    )
  }
)

bootstrap_boxplot_summary_all <- bootstrap_boxplot_distribution_all %>%
  group_by(variable, category) %>%
  summarise(
    n_bootstraps = n_distinct(bootstrap_id),
    mean_n = mean(n, na.rm = TRUE),
    sd_n = sd(n, na.rm = TRUE),
    mean_total_n = mean(total_n, na.rm = TRUE),
    mean_prop = mean(prop, na.rm = TRUE),
    sd_prop = sd(prop, na.rm = TRUE),
    min_prop = min(prop, na.rm = TRUE),
    q25_prop = quantile(prop, 0.25, na.rm = TRUE),
    median_prop = median(prop, na.rm = TRUE),
    q75_prop = quantile(prop, 0.75, na.rm = TRUE),
    max_prop = max(prop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(variable, desc(median_prop), category)

write_csv(
  bootstrap_boxplot_distribution_all,
  file.path(out_dir, "bootstrap_boxplot_distribution_by_run_all_variables.csv")
)

write_csv(
  bootstrap_boxplot_summary_all,
  file.path(out_dir, "bootstrap_boxplot_summary_all_variables.csv")
)


# Boxplots para todas las variables
for (var in boxplot_vars) {
  make_boxplot(
    distribution_df = bootstrap_boxplot_distribution_all,
    var = var,
    fig_dir = fig_dir,
    filename_prefix = "06_bootstrap_boxplot_all_variables"
  )
}


# Boxplot especial de arquetipos sin Missing

# Importante:
# Para arquetipos, quitamos Missing porque la autoclasificación solo aplica a RV.
# Así el porcentaje se interpreta entre participantes que sí tienen arquetipo.

bootstrap_archetype_distribution_non_missing <- tibble()
bootstrap_archetype_summary_non_missing <- tibble()

if (archetype_var %in% names(boot_df)) {
  
  bootstrap_archetype_distribution_non_missing <- compute_bootstrap_distribution(
    boot_df = boot_df,
    var = archetype_var,
    drop_missing = TRUE
  )
  
  bootstrap_archetype_summary_non_missing <- bootstrap_archetype_distribution_non_missing %>%
    group_by(variable, category) %>%
    summarise(
      n_bootstraps = n_distinct(bootstrap_id),
      mean_n = mean(n, na.rm = TRUE),
      sd_n = sd(n, na.rm = TRUE),
      mean_total_n = mean(total_n, na.rm = TRUE),
      mean_prop = mean(prop, na.rm = TRUE),
      sd_prop = sd(prop, na.rm = TRUE),
      min_prop = min(prop, na.rm = TRUE),
      q25_prop = quantile(prop, 0.25, na.rm = TRUE),
      median_prop = median(prop, na.rm = TRUE),
      q75_prop = quantile(prop, 0.75, na.rm = TRUE),
      max_prop = max(prop, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(median_prop), category)
  
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
    fig_dir = fig_dir,
    filename_prefix = "06_MAIN_bootstrap_boxplot_archetypes_non_missing"
  )
}


# Parámetros
parameters <- tibble(
  parameter = c(
    "input_file",
    "bootstrap_scenario",
    "bootstrap_index_file",
    "bootstrap_dataset_file",
    "out_dir",
    "fig_dir",
    "max_bootstraps",
    "n_bootstraps_used",
    "n_bootstrap_rows_used",
    "self_classification_col",
    "boxplot_vars"
  ),
  value = c(
    input_file,
    bootstrap_scenario,
    bootstrap_index_file,
    bootstrap_dataset_file,
    out_dir,
    fig_dir,
    as.character(MAX_BOOTSTRAPS),
    as.character(n_distinct(boot_df$bootstrap_id)),
    as.character(nrow(boot_df)),
    as.character(self_classification_col),
    paste(boxplot_vars, collapse = ", ")
  )
)

write_csv(
  parameters,
  file.path(out_dir, "bootstrap_boxplot_parameters.csv")
)


# Consola

cat("\n============================================================\n")
cat("BOOTSTRAP BOXPLOTS COMPLETADO\n")
cat("============================================================\n")

cat("\nN bootstraps usados:", n_distinct(boot_df$bootstrap_id), "\n")
cat("Filas bootstrap usadas:", nrow(boot_df), "\n")

cat("\nVariables con boxplot:\n")
print(boxplot_vars)

cat("\nArchivos principales en outputs:\n")
cat("- bootstrap_boxplot_distribution_by_run_all_variables.csv\n")
cat("- bootstrap_boxplot_summary_all_variables.csv\n")
cat("- bootstrap_archetype_distribution_by_run_non_missing.csv\n")
cat("- bootstrap_archetype_boxplot_summary_non_missing.csv\n")
cat("- bootstrap_boxplot_variables_used.csv\n")
cat("- bootstrap_boxplot_parameters.csv\n")

cat("\nFiguras principales:\n")
cat("- 06_bootstrap_boxplot_all_variables_*.png\n")
cat("- 06_MAIN_bootstrap_boxplot_archetypes_non_missing_self_classification_archetype_model.png\n")

cat("\nCarpeta outputs:\n")
cat(out_dir, "\n")

cat("\nCarpeta figures:\n")
cat(fig_dir, "\n")

message("\nListo. Boxplots bootstrap guardados.")