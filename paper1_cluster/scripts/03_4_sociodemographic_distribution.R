
#
# Objetivo:
# Sacar distribución sociodemográfica y de calidad de la muestra:
# 1. Muestra integrada completa.
# 2. Muestra usable para análisis principal.
# 3. Muestra usable para clustering.
# 4. Distribución por fuente: RV / WHY / Diego.
# 5. Distribución post-bootstrap usando el escenario político elegido.
# 6. Autoclasificación de arquetipo RV, como en el TFM.
#
# Este script NO modifica datos.
# Solo genera tablas y gráficos descriptivos.
#
# Salidas:
# - CSV/tablas -> paper1_cluster/outputs/03_4_sociodemographic_distribution
# - Figuras    -> paper1_cluster/figures/03_4_sociodemographic_distribution

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

data_root <- file.path(paper_root, "data")
processed_root <- file.path(data_root, "processed")

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
  "03_4_sociodemographic_distribution"
)

fig_dir <- file.path(
  figures_root,
  "03_4_sociodemographic_distribution"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

#  Variables esperadas
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
  "self_classification_raw",
  "self_classification_raw_clean",
  "self_classification_archetype_model",
  "row_quality_final",
  "usable_for_main_analysis",
  "usable_for_clustering"
)

# Lectura ligera
if (!file.exists(input_file)) {
  stop("No encuentro archivo de entrada: ", input_file)
}

all_colnames <- names(
  read_csv(
    input_file,
    n_max = 0,
    show_col_types = FALSE
  )
)

self_classification_candidates_by_name <- all_colnames[
  str_detect(
    all_colnames,
    regex(
      "4\\.3|statement.*describes|investment.*decision|household.*decision|self.*classification|autoclas",
      ignore_case = TRUE
    )
  )
]

needed_cols <- unique(c(
  "integrated_row_id",
  candidate_vars,
  self_classification_candidates_by_name
))

needed_cols <- needed_cols[needed_cols %in% all_colnames]

df <- read_csv(
  input_file,
  col_select = any_of(needed_cols),
  show_col_types = FALSE
)

cat("\n============================================================\n")
cat("DISTRIBUCIÓN SOCIODEMOGRÁFICA\n")
cat("============================================================\n")
cat("Archivo usado:\n", input_file, "\n")
cat("Filas:", nrow(df), "\n")
cat("Columnas leídas:", ncol(df), "\n")
cat("Columnas disponibles en archivo original:", length(all_colnames), "\n")


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

as_logical_flag <- function(x) {
  x_chr <- str_to_lower(str_squish(as.character(x)))
  
  case_when(
    x_chr %in% c("true", "1", "yes", "sí", "si") ~ TRUE,
    x_chr %in% c("false", "0", "no") ~ FALSE,
    TRUE ~ NA
  )
}

make_label_n_pct <- function(n, prop, accuracy = 1) {
  paste0(n, " (", percent(prop, accuracy = accuracy), ")")
}

make_label_mean_n_pct <- function(mean_n, mean_prop, accuracy = 1) {
  paste0(round(mean_n, 1), " (", percent(mean_prop, accuracy = accuracy), ")")
}

safe_y_limit <- function(x, multiplier = 1.30) {
  max_x <- suppressWarnings(max(x, na.rm = TRUE))
  
  if (!is.finite(max_x) || max_x <= 0) {
    return(1)
  }
  
  max_x * multiplier
}


# Autoclasificación de arquetipo RV
find_self_classification_col <- function(data) {
  
  priority_cols <- c(
    "self_classification_raw_clean",
    "self_classification_raw"
  )
  
  existing_priority_cols <- priority_cols[priority_cols %in% names(data)]
  
  if (length(existing_priority_cols) > 0) {
    return(existing_priority_cols[1])
  }
  
  candidates_by_name <- names(data)[
    str_detect(
      names(data),
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

self_classification_col <- find_self_classification_col(df)

cat("\nColumna detectada para autoclasificación:\n")
print(self_classification_col)

if (!is.na(self_classification_col)) {
  
  df <- df %>%
    mutate(
      self_classification_raw = .data[[self_classification_col]],
      self_classification_raw_clean = str_squish(as.character(self_classification_raw)),
      
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
  
} else {
  
  warning("No se ha podido detectar automáticamente la columna de autoclasificación.")
  
  df <- df %>%
    mutate(
      self_classification_raw = NA_character_,
      self_classification_raw_clean = NA_character_,
      self_classification_archetype_model = NA_character_
    )
}

self_classification_mapping <- tribble(
  ~answer_pattern, ~archetype,
  "reduce environmental impact", "Activist",
  "improves my safety or that of my household", "Fearful",
  "improves my social status or aligns with norms", "Influencer",
  "maximizes my comfort or well-being", "Careful",
  "not very interested / stick to what I am used to", "Uninterested",
  "early adopter / latest trends", "Early adopter",
  "ethical or social commitment to environmental protection", "Stubborn",
  "cost-effective / informed / costs and benefits", "Homo economicus",
  "None of the above", "None"
)

write_csv(
  self_classification_mapping,
  file.path(out_dir, "self_classification_mapping_used.csv")
)

write_lines(
  paste0("self_classification_col_detected: ", self_classification_col),
  file.path(out_dir, "self_classification_column_detected.txt")
)

# Variables a resumir
vars_to_summarise <- candidate_vars[candidate_vars %in% names(df)]

vars_to_summarise <- unique(c(
  vars_to_summarise,
  "self_classification_archetype_model"
))

vars_to_summarise <- vars_to_summarise[vars_to_summarise %in% names(df)]

if (length(vars_to_summarise) == 0) {
  stop("No encuentro ninguna variable sociodemográfica esperada.")
}

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
  
  vars_to_summarise <- unique(c(vars_to_summarise, "political_lr_group"))
}

# Funciones de resumen y gráficos
summarise_one_var <- function(data, var, context_name) {
  
  data %>%
    mutate(
      category = clean_category(.data[[var]])
    ) %>%
    count(category, name = "n") %>%
    mutate(
      variable = var,
      context = context_name,
      total_n = sum(n),
      prop = n / total_n
    ) %>%
    select(context, variable, category, n, total_n, prop) %>%
    arrange(context, variable, desc(n), category)
}

summarise_one_var_by_source <- function(data, var, context_name) {
  
  if (!"dataset_source" %in% names(data)) {
    return(tibble())
  }
  
  data %>%
    mutate(
      dataset_source = clean_category(dataset_source),
      category = clean_category(.data[[var]])
    ) %>%
    count(dataset_source, category, name = "n") %>%
    group_by(dataset_source) %>%
    mutate(
      variable = var,
      context = context_name,
      total_n = sum(n),
      prop = n / total_n
    ) %>%
    ungroup() %>%
    select(context, variable, dataset_source, category, n, total_n, prop) %>%
    arrange(context, variable, dataset_source, desc(n), category)
}

plot_overall <- function(summary_df, var, context_name, filename_prefix) {
  
  p_data <- summary_df %>%
    filter(
      variable == var,
      context == context_name
    ) %>%
    mutate(
      category = fct_reorder(category, prop),
      label = make_label_n_pct(n, prop)
    )
  
  if (nrow(p_data) == 0) return(invisible(NULL))
  
  n_participants <- unique(p_data$total_n)
  n_participants <- n_participants[!is.na(n_participants)][1]
  
  y_max <- safe_y_limit(p_data$prop)
  
  p <- ggplot(p_data, aes(x = category, y = prop, fill = category)) +
    geom_col(show.legend = FALSE) +
    geom_text(
      aes(label = label),
      hjust = -0.06,
      size = 3.2
    ) +
    scale_fill_manual(
      values = category_palette(p_data$category),
      guide = "none"
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, y_max)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.margin = margin(10, 55, 10, 10)
    ) +
    labs(
      title = paste0("Distribution of ", var),
      subtitle = paste0(context_name, " | n participantes = ", n_participants),
      x = NULL,
      y = "Percentage"
    )
  
  ggsave(
    filename = file.path(
      fig_dir,
      paste0(filename_prefix, "_", safe_filename(context_name), "_", safe_filename(var), ".png")
    ),
    plot = p,
    width = 10,
    height = 6,
    dpi = 300
  )
}

plot_by_source <- function(summary_df, var, context_name, filename_prefix) {
  
  p_data <- summary_df %>%
    filter(
      variable == var,
      context == context_name
    ) %>%
    mutate(
      dataset_source_panel = paste0(dataset_source, "\n", "n participantes = ", total_n),
      category = fct_reorder(category, prop),
      label = make_label_n_pct(n, prop)
    )
  
  if (nrow(p_data) == 0) return(invisible(NULL))
  
  y_max <- safe_y_limit(p_data$prop)
  
  p <- ggplot(p_data, aes(x = category, y = prop, fill = category)) +
    geom_col(show.legend = FALSE) +
    geom_text(
      aes(label = label),
      hjust = -0.06,
      size = 2.8
    ) +
    scale_fill_manual(
      values = category_palette(p_data$category),
      guide = "none"
    ) +
    coord_flip(clip = "off") +
    facet_wrap(~ dataset_source_panel, scales = "free_y") +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, y_max)
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.margin = margin(10, 55, 10, 10)
    ) +
    labs(
      title = paste0("Distribution of ", var, " by source"),
      subtitle = context_name,
      x = NULL,
      y = "Percentage"
    )
  
  ggsave(
    filename = file.path(
      fig_dir,
      paste0(filename_prefix, "_by_source_", safe_filename(context_name), "_", safe_filename(var), ".png")
    ),
    plot = p,
    width = 12,
    height = 7,
    dpi = 300
  )
}

# Crear datasets de análisis
if ("usable_for_main_analysis" %in% names(df)) {
  df <- df %>%
    mutate(usable_for_main_analysis_flag = as_logical_flag(usable_for_main_analysis))
} else {
  df <- df %>%
    mutate(usable_for_main_analysis_flag = TRUE)
}

if ("usable_for_clustering" %in% names(df)) {
  df <- df %>%
    mutate(usable_for_clustering_flag = as_logical_flag(usable_for_clustering))
} else {
  df <- df %>%
    mutate(usable_for_clustering_flag = TRUE)
}

df_all <- df %>%
  mutate(sample_context = "01_all_integrated_rows")

df_main <- df %>%
  filter(usable_for_main_analysis_flag == TRUE) %>%
  mutate(sample_context = "02_usable_for_main_analysis")

df_cluster <- df %>%
  filter(usable_for_clustering_flag == TRUE) %>%
  mutate(sample_context = "03_usable_for_clustering")

context_list <- list(
  "01_all_integrated_rows" = df_all,
  "02_usable_for_main_analysis" = df_main,
  "03_usable_for_clustering" = df_cluster
)


# Tablas descriptivas originales
overall_distribution <- map_dfr(
  names(context_list),
  function(context_name) {
    data_context <- context_list[[context_name]]
    
    map_dfr(
      vars_to_summarise,
      ~ summarise_one_var(data_context, .x, context_name)
    )
  }
)

by_source_distribution <- map_dfr(
  names(context_list),
  function(context_name) {
    data_context <- context_list[[context_name]]
    
    map_dfr(
      vars_to_summarise,
      ~ summarise_one_var_by_source(data_context, .x, context_name)
    )
  }
)

write_csv(
  overall_distribution,
  file.path(out_dir, "sociodemographic_distribution_overall.csv")
)

write_csv(
  by_source_distribution,
  file.path(out_dir, "sociodemographic_distribution_by_source.csv")
)

sample_flow <- bind_rows(
  df_all,
  df_main,
  df_cluster
) %>%
  mutate(
    dataset_source = if ("dataset_source" %in% names(.)) clean_category(dataset_source) else "all",
    row_quality_final = if ("row_quality_final" %in% names(.)) clean_category(row_quality_final) else "not_available"
  ) %>%
  count(sample_context, dataset_source, row_quality_final, name = "n") %>%
  group_by(sample_context, dataset_source) %>%
  mutate(
    total_source_n = sum(n),
    prop_source = n / total_source_n
  ) %>%
  ungroup()

write_csv(
  sample_flow,
  file.path(out_dir, "sample_flow_by_context_source_quality.csv")
)

self_classification_rv_distribution <- df %>%
  filter(dataset_source == "rv") %>%
  mutate(
    self_classification_archetype_model = clean_category(self_classification_archetype_model)
  ) %>%
  count(self_classification_archetype_model, name = "n") %>%
  mutate(
    variable = "self_classification_archetype_model",
    total_n = sum(n),
    prop = n / total_n
  ) %>%
  select(variable, category = self_classification_archetype_model, n, total_n, prop) %>%
  arrange(desc(n), category)

write_csv(
  self_classification_rv_distribution,
  file.path(out_dir, "self_classification_rv_distribution.csv")
)
# Gráficos originales
vars_for_plots <- vars_to_summarise

for (context_name in names(context_list)) {
  for (var in vars_for_plots) {
    
    plot_overall(
      summary_df = overall_distribution,
      var = var,
      context_name = context_name,
      filename_prefix = "01_overall"
    )
    
    plot_by_source(
      summary_df = by_source_distribution,
      var = var,
      context_name = context_name,
      filename_prefix = "02"
    )
  }
}

# Gráfico de calidad por fuente
if ("row_quality_final" %in% names(df) && "dataset_source" %in% names(df)) {
  
  quality_plot_data <- df %>%
    mutate(
      dataset_source = clean_category(dataset_source),
      row_quality_final = clean_category(row_quality_final)
    ) %>%
    count(dataset_source, row_quality_final, name = "n") %>%
    group_by(dataset_source) %>%
    mutate(
      prop = n / sum(n),
      total_n = sum(n),
      label = make_label_n_pct(n, prop),
      dataset_source_panel = paste0(dataset_source, "\n", "n participantes = ", total_n)
    ) %>%
    ungroup()
  
  y_max_quality <- safe_y_limit(quality_plot_data$prop)
  
  p_quality <- ggplot(
    quality_plot_data,
    aes(x = row_quality_final, y = prop, fill = row_quality_final)
  ) +
    geom_col(show.legend = FALSE) +
    geom_text(
      aes(label = label),
      hjust = -0.06,
      size = 2.8
    ) +
    scale_fill_manual(
      values = category_palette(quality_plot_data$row_quality_final),
      guide = "none"
    ) +
    coord_flip(clip = "off") +
    facet_wrap(~ dataset_source_panel, scales = "free_y") +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, y_max_quality)
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.margin = margin(10, 55, 10, 10)
    ) +
    labs(
      title = "Response quality by dataset source",
      x = NULL,
      y = "Percentage"
    )
  
  ggsave(
    filename = file.path(fig_dir, "03_quality_by_source.png"),
    plot = p_quality,
    width = 12,
    height = 7,
    dpi = 300
  )
}

# Gráfico específico de autoclasificación RV
if (nrow(self_classification_rv_distribution) > 0) {
  
  n_self <- unique(self_classification_rv_distribution$total_n)
  n_self <- n_self[!is.na(n_self)][1]
  
  p_self_data <- self_classification_rv_distribution %>%
    mutate(
      category = fct_reorder(category, prop),
      label = make_label_n_pct(n, prop)
    )
  
  y_max_self <- safe_y_limit(p_self_data$prop)
  
  p_self <- ggplot(
    p_self_data,
    aes(x = category, y = prop, fill = category)
  ) +
    geom_col(show.legend = FALSE) +
    geom_text(
      aes(label = label),
      hjust = -0.08,
      size = 3.7
    ) +
    scale_fill_manual(
      values = category_palette(p_self_data$category),
      guide = "none"
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, y_max_self)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.margin = margin(10, 55, 10, 10)
    ) +
    labs(
      title = "RV self-classification archetype",
      subtitle = paste0("RV survey only | n participantes = ", n_self),
      x = NULL,
      y = "Percentage"
    )
  
  ggsave(
    filename = file.path(
      fig_dir,
      "05_rv_self_classification_archetype_model.png"
    ),
    plot = p_self,
    width = 10,
    height = 6,
    dpi = 300
  )
}


# Distribución post-bootstrap
bootstrap_distribution <- tibble()
bootstrap_distribution_summary <- tibble()

if (file.exists(bootstrap_index_file)) {
  
  boot_index <- read_csv(
    bootstrap_index_file,
    col_select = any_of(c("bootstrap_id", "integrated_row_id")),
    show_col_types = FALSE
  ) %>%
    mutate(
      bootstrap_id = as.integer(bootstrap_id),
      integrated_row_id = as.character(integrated_row_id)
    )
  
  df_boot_base <- df %>%
    mutate(
      integrated_row_id = as.character(integrated_row_id)
    ) %>%
    distinct(integrated_row_id, .keep_all = TRUE)
  
  if (file.exists(bootstrap_dataset_file)) {
    
    boot_dataset <- read_csv(
      bootstrap_dataset_file,
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
  
  boot_df <- boot_index %>%
    left_join(
      df_boot_base,
      by = "integrated_row_id",
      suffix = c("_boot", "")
    )
  
  if ("target_electoral_group_boot" %in% names(boot_df) && "target_electoral_group" %in% names(boot_df)) {
    boot_df <- boot_df %>%
      mutate(
        target_electoral_group = coalesce(
          as.character(target_electoral_group_boot),
          as.character(target_electoral_group)
        )
      )
  }
  
  if ("target_sample_type_boot" %in% names(boot_df) && "target_sample_type" %in% names(boot_df)) {
    boot_df <- boot_df %>%
      mutate(
        target_sample_type = coalesce(
          as.character(target_sample_type_boot),
          as.character(target_sample_type)
        )
      )
  }
  
  boot_vars <- unique(c(
    vars_to_summarise,
    "target_sample_type",
    "target_electoral_group"
  ))
  
  boot_vars <- boot_vars[boot_vars %in% names(boot_df)]
  
  bootstrap_distribution <- map_dfr(
    boot_vars,
    function(var) {
      boot_df %>%
        mutate(
          category = clean_category(.data[[var]])
        ) %>%
        count(bootstrap_id, category, name = "n") %>%
        group_by(bootstrap_id) %>%
        mutate(
          total_n = sum(n),
          prop = n / total_n
        ) %>%
        ungroup() %>%
        mutate(variable = var) %>%
        select(bootstrap_id, variable, category, n, total_n, prop)
    }
  )
  
  bootstrap_distribution_summary <- bootstrap_distribution %>%
    group_by(variable, category) %>%
    summarise(
      mean_n = mean(n, na.rm = TRUE),
      sd_n = sd(n, na.rm = TRUE),
      mean_total_n = mean(total_n, na.rm = TRUE),
      sd_total_n = sd(total_n, na.rm = TRUE),
      mean_prop = mean(prop, na.rm = TRUE),
      sd_prop = sd(prop, na.rm = TRUE),
      n_bootstraps = n_distinct(bootstrap_id),
      .groups = "drop"
    ) %>%
    arrange(variable, desc(mean_prop), category)
  
  write_csv(
    bootstrap_distribution,
    file.path(out_dir, "sociodemographic_distribution_bootstrap_by_run.csv")
  )
  
  write_csv(
    bootstrap_distribution_summary,
    file.path(out_dir, "sociodemographic_distribution_bootstrap_summary.csv")
  )
  
  for (var in boot_vars) {
    
    p_data <- bootstrap_distribution_summary %>%
      filter(variable == var) %>%
      mutate(
        category = fct_reorder(category, mean_prop),
        label = make_label_mean_n_pct(mean_n, mean_prop)
      )
    
    if (nrow(p_data) == 0) next
    
    n_boot <- unique(p_data$n_bootstraps)
    n_boot <- n_boot[!is.na(n_boot)][1]
    
    n_boot_sample <- round(mean(p_data$mean_total_n, na.rm = TRUE), 0)
    
    y_max_boot <- safe_y_limit(p_data$mean_prop)
    
    p <- ggplot(p_data, aes(x = category, y = mean_prop, fill = category)) +
      geom_col(show.legend = FALSE) +
      geom_errorbar(
        aes(
          ymin = pmax(mean_prop - sd_prop, 0),
          ymax = pmin(mean_prop + sd_prop, 1)
        ),
        width = 0.2
      ) +
      geom_text(
        aes(label = label),
        hjust = -0.06,
        size = 3.0
      ) +
      scale_fill_manual(
        values = category_palette(p_data$category),
        guide = "none"
      ) +
      coord_flip(clip = "off") +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        limits = c(0, y_max_boot)
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.margin = margin(10, 55, 10, 10)
      ) +
      labs(
        title = paste0("Bootstrap-adjusted distribution of ", var),
        subtitle = paste0(
          "Scenario: ", bootstrap_scenario,
          " | n muestra bootstrap = ", n_boot_sample,
          " | n bootstraps = ", n_boot
        ),
        x = NULL,
        y = "Mean percentage across bootstraps ± SD"
      )
    
    ggsave(
      filename = file.path(
        fig_dir,
        paste0("04_bootstrap_adjusted_", safe_filename(var), ".png")
      ),
      plot = p,
      width = 10,
      height = 6,
      dpi = 300
    )
  }
  
} else {
  message("No se ha encontrado bootstrap index. Se omite distribución post-bootstrap.")
}


# Registro de variables y parámetros
parameters <- tibble(
  parameter = c(
    "input_file",
    "output_dir",
    "figures_dir",
    "variables_summarised",
    "self_classification_col_detected",
    "bootstrap_scenario",
    "bootstrap_index_file",
    "bootstrap_dataset_file",
    "n_rows_all",
    "n_rows_main",
    "n_rows_clustering"
  ),
  value = c(
    input_file,
    out_dir,
    fig_dir,
    paste(vars_to_summarise, collapse = ", "),
    as.character(self_classification_col),
    bootstrap_scenario,
    bootstrap_index_file,
    bootstrap_dataset_file,
    as.character(nrow(df_all)),
    as.character(nrow(df_main)),
    as.character(nrow(df_cluster))
  )
)

write_csv(
  parameters,
  file.path(out_dir, "sociodemographic_distribution_parameters.csv")
)


# Consola
cat("\n============================================================\n")
cat("SOCIODEMOGRAPHIC DISTRIBUTION COMPLETADA\n")
cat("============================================================\n")

cat("\nFilas:\n")
cat("All integrated:", nrow(df_all), "\n")
cat("Usable main:", nrow(df_main), "\n")
cat("Usable clustering:", nrow(df_cluster), "\n")

cat("\nColumna autoclasificación detectada:\n")
print(self_classification_col)

cat("\nVariables resumidas:\n")
print(vars_to_summarise)

cat("\nDistribución autoclasificación RV:\n")
print(self_classification_rv_distribution, n = Inf, width = Inf)

cat("\nArchivos principales en outputs:\n")
cat("- sociodemographic_distribution_overall.csv\n")
cat("- sociodemographic_distribution_by_source.csv\n")
cat("- sample_flow_by_context_source_quality.csv\n")
cat("- sociodemographic_distribution_bootstrap_by_run.csv\n")
cat("- sociodemographic_distribution_bootstrap_summary.csv\n")
cat("- self_classification_rv_distribution.csv\n")
cat("- self_classification_mapping_used.csv\n")
cat("- self_classification_column_detected.txt\n")
cat("- sociodemographic_distribution_parameters.csv\n")

cat("\nCarpeta outputs:\n")
cat(out_dir, "\n")

cat("\nCarpeta figures:\n")
cat(fig_dir, "\n")

message("\nListo. Distribución sociodemográfica guardada.")