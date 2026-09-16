#
# Compara las distribuciones descriptivas antes y después del bootstrap.
#
# Pre-bootstrap:
#   - muestra integrada
#   - usable para análisis principal
#   - usable para clustering
#
# Post-bootstrap:
#   COMPLETE
#   EUROPE
#   LATAM
#   DIEGO
#   RENOVISOR
#   WHY_EUROPE
#   WHY_LATAM
#
# El bootstrap final procede de 04_2f:
#   EUROPE   -> bootstrap político/electoral
#   LATAM    -> bootstrap económico por ingreso
#   COMPLETE -> EUROPE + LATAM
#
# Este script no modifica ni vuelve a remuestrear los datos.


suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})


# Configuración

project_root <- path.expand(
  "~/Desktop/MASTER/recommendation-engine/TFM"
)

processed_root <- file.path(
  project_root,
  "paper1_cluster/data/processed"
)

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

out_dir <- file.path(
  processed_root,
  "04_2g_sociodemographic_distribution"
)

fig_dir <- file.path(
  out_dir,
  "figures"
)

walk(
  c(out_dir, fig_dir),
  ~ dir.create(
    .x,
    recursive = TRUE,
    showWarnings = FALSE
  )
)

EXPECTED_N_BOOT <- 1000L

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
  "self_classification_raw",
  "self_classification_raw_clean",
  "self_classification_archetype_model",
  "row_quality_final",
  "usable_for_main_analysis",
  "usable_for_clustering"
)


# Funciones auxiliares

clean_category <- function(x) {
  x <- str_squish(as.character(x))
  x[is.na(x) | x == ""] <- "Missing"
  x
}


as_logical_flag <- function(x) {
  x <- str_to_lower(
    str_squish(
      as.character(x)
    )
  )
  
  case_when(
    x %in% c(
      "true",
      "1",
      "yes",
      "sí",
      "si"
    ) ~ TRUE,
    
    x %in% c(
      "false",
      "0",
      "no"
    ) ~ FALSE,
    
    TRUE ~ NA
  )
}


safe_filename <- function(x) {
  x %>%
    str_replace_all(
      "[^A-Za-z0-9_]+",
      "_"
    ) %>%
    str_replace_all(
      "_+",
      "_"
    ) %>%
    str_remove_all(
      "^_|_$"
    )
}


make_label_n_pct <- function(n, prop) {
  paste0(
    n,
    " (",
    percent(
      prop,
      accuracy = 1
    ),
    ")"
  )
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
  categories <- unique(
    as.character(categories)
  )
  
  categories <- categories[
    !is.na(categories)
  ]
  
  palette <- rep(
    main_palette,
    length.out = length(categories)
  )
  
  names(palette) <- categories
  palette
}


find_self_classification_col <- function(data_names) {
  priority_cols <- c(
    "self_classification_raw_clean",
    "self_classification_raw"
  )
  
  available_priority <- priority_cols[
    priority_cols %in% data_names
  ]
  
  if (length(available_priority)) {
    return(
      available_priority[1]
    )
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
  
  candidates <- setdiff(
    candidates,
    "self_classification_archetype_model"
  )
  
  if (length(candidates)) {
    candidates[1]
  } else {
    NA_character_
  }
}


summarise_one_var <- function(
    data,
    var,
    context_name
) {
  data %>%
    transmute(
      category =
        clean_category(
          .data[[var]]
        )
    ) %>%
    count(
      category,
      name = "n"
    ) %>%
    mutate(
      context = context_name,
      variable = var,
      total_n = sum(n),
      prop = n / total_n
    ) %>%
    select(
      context,
      variable,
      category,
      n,
      total_n,
      prop
    ) %>%
    arrange(
      desc(prop),
      category
    )
}


summarise_one_var_by_group <- function(
    data,
    var,
    group_var,
    context_name
) {
  if (!group_var %in% names(data)) {
    return(
      tibble()
    )
  }
  
  data %>%
    transmute(
      group =
        clean_category(
          .data[[group_var]]
        ),
      
      category =
        clean_category(
          .data[[var]]
        )
    ) %>%
    count(
      group,
      category,
      name = "n"
    ) %>%
    group_by(
      group
    ) %>%
    mutate(
      context = context_name,
      variable = var,
      total_n = sum(n),
      prop = n / total_n
    ) %>%
    ungroup() %>%
    select(
      context,
      variable,
      group,
      category,
      n,
      total_n,
      prop
    ) %>%
    arrange(
      group,
      desc(prop),
      category
    )
}


plot_overall <- function(
    summary_df,
    var,
    context_name
) {
  p_data <- summary_df %>%
    filter(
      variable == var,
      context == context_name
    ) %>%
    mutate(
      category = fct_reorder(
        category,
        prop
      ),
      
      label = make_label_n_pct(
        n,
        prop
      )
    )
  
  if (!nrow(p_data)) {
    return(
      invisible(NULL)
    )
  }
  
  n_current <- unique(
    p_data$total_n
  )[1]
  
  p <- ggplot(
    p_data,
    aes(
      x = category,
      y = prop,
      fill = category
    )
  ) +
    geom_col(
      show.legend = FALSE
    ) +
    geom_text(
      aes(
        label = label
      ),
      hjust = -0.05,
      size = 3
    ) +
    scale_fill_manual(
      values = category_palette(
        p_data$category
      ),
      guide = "none"
    ) +
    scale_y_continuous(
      labels = percent_format(
        accuracy = 1
      ),
      expand = expansion(
        mult = c(
          0,
          0.25
        )
      )
    ) +
    coord_flip(
      clip = "off"
    ) +
    theme_minimal(
      base_size = 12
    ) +
    theme(
      plot.margin = margin(
        10,
        60,
        10,
        10
      )
    ) +
    labs(
      title = paste0(
        "Distribution of ",
        var
      ),
      subtitle = paste0(
        context_name,
        " | n = ",
        n_current
      ),
      x = NULL,
      y = "Percentage"
    )
  
  ggsave(
    file.path(
      fig_dir,
      paste0(
        "prebootstrap_overall_",
        safe_filename(
          context_name
        ),
        "_",
        safe_filename(
          var
        ),
        ".png"
      )
    ),
    plot = p,
    width = 10,
    height = 6,
    dpi = 300
  )
}


plot_by_group <- function(
    summary_df,
    var,
    context_name,
    group_label,
    filename_prefix
) {
  p_data <- summary_df %>%
    filter(
      variable == var,
      context == context_name
    ) %>%
    mutate(
      panel = paste0(
        group,
        "\nn = ",
        total_n
      ),
      
      category = fct_reorder(
        category,
        prop
      ),
      
      label = make_label_n_pct(
        n,
        prop
      )
    )
  
  if (!nrow(p_data)) {
    return(
      invisible(NULL)
    )
  }
  
  p <- ggplot(
    p_data,
    aes(
      x = category,
      y = prop,
      fill = category
    )
  ) +
    geom_col(
      show.legend = FALSE
    ) +
    geom_text(
      aes(
        label = label
      ),
      hjust = -0.05,
      size = 2.7
    ) +
    scale_fill_manual(
      values = category_palette(
        p_data$category
      ),
      guide = "none"
    ) +
    scale_y_continuous(
      labels = percent_format(
        accuracy = 1
      ),
      expand = expansion(
        mult = c(
          0,
          0.25
        )
      )
    ) +
    coord_flip(
      clip = "off"
    ) +
    facet_wrap(
      ~ panel,
      scales = "free_y"
    ) +
    theme_minimal(
      base_size = 11
    ) +
    theme(
      plot.margin = margin(
        10,
        60,
        10,
        10
      )
    ) +
    labs(
      title = paste0(
        "Distribution of ",
        var,
        " by ",
        group_label
      ),
      subtitle = context_name,
      x = NULL,
      y = "Percentage"
    )
  
  ggsave(
    file.path(
      fig_dir,
      paste0(
        filename_prefix,
        "_",
        safe_filename(
          context_name
        ),
        "_",
        safe_filename(
          var
        ),
        ".png"
      )
    ),
    plot = p,
    width = 13,
    height = 8,
    dpi = 300
  )
}


compute_bootstrap_distribution <- function(
    data,
    var,
    boot_ids
) {
  base <- data %>%
    transmute(
      bootstrap_id,
      comparison_region,
      subsample,
      category =
        clean_category(
          .data[[var]]
        )
    )
  
  counts_complete <- base %>%
    count(
      bootstrap_id,
      category,
      name = "n"
    ) %>%
    mutate(
      analysis_sample = "COMPLETE"
    )
  
  counts_region <- base %>%
    count(
      bootstrap_id,
      comparison_region,
      category,
      name = "n"
    ) %>%
    transmute(
      bootstrap_id,
      analysis_sample =
        comparison_region,
      category,
      n
    )
  
  counts_subsample <- base %>%
    count(
      bootstrap_id,
      subsample,
      category,
      name = "n"
    ) %>%
    transmute(
      bootstrap_id,
      analysis_sample =
        subsample,
      category,
      n
    )
  
  bind_rows(
    counts_complete,
    counts_region,
    counts_subsample
  ) %>%
    filter(
      analysis_sample %in%
        ANALYSIS_SAMPLES
    ) %>%
    group_by(
      analysis_sample
    ) %>%
    group_modify(
      ~ {
        categories_current <- sort(
          unique(
            .x$category
          )
        )
        
        complete(
          .x,
          bootstrap_id = boot_ids,
          category = categories_current,
          fill = list(
            n = 0L
          )
        )
      }
    ) %>%
    ungroup() %>%
    group_by(
      analysis_sample,
      bootstrap_id
    ) %>%
    mutate(
      total_n = sum(n),
      
      prop = if_else(
        total_n > 0,
        n / total_n,
        NA_real_
      )
    ) %>%
    ungroup() %>%
    mutate(
      variable = var,
      
      analysis_sample = factor(
        analysis_sample,
        levels = ANALYSIS_SAMPLES
      )
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


# Leer dataset descriptivo

if (!file.exists(input_file)) {
  stop(
    "No encuentro el archivo de entrada:\n",
    input_file
  )
}

if (!file.exists(bootstrap_index_file)) {
  stop(
    "No encuentro el bootstrap final de 04_2f:\n",
    bootstrap_index_file
  )
}

all_colnames <- names(
  read_csv(
    input_file,
    n_max = 0,
    show_col_types = FALSE
  )
)

if (!"integrated_row_id" %in% all_colnames) {
  stop(
    "El dataset descriptivo no contiene integrated_row_id."
  )
}

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
    self_classification_candidates
  )
)

needed_cols <- intersect(
  needed_cols,
  all_colnames
)

df <- read_csv(
  input_file,
  col_select = all_of(
    needed_cols
  ),
  show_col_types = FALSE
) %>%
  mutate(
    integrated_row_id =
      as.character(
        integrated_row_id
      )
  )

if (anyDuplicated(df$integrated_row_id)) {
  stop(
    "integrated_row_id contiene duplicados en el dataset descriptivo."
  )
}


# Autoclasificación RV

self_classification_col <- find_self_classification_col(
  names(df)
)

if (!is.na(self_classification_col)) {
  df <- df %>%
    mutate(
      self_classification_raw =
        .data[
          [self_classification_col]
        ],
      
      self_classification_raw_clean =
        str_squish(
          as.character(
            self_classification_raw
          )
        ),
      
      self_classification_archetype_model = case_when(
        str_detect(
          self_classification_raw_clean,
          regex(
            "environmental impact|impacto ambiental|reduce.*environmental",
            ignore_case = TRUE
          )
        ) ~ "Activist",
        
        str_detect(
          self_classification_raw_clean,
          regex(
            "safety|seguridad",
            ignore_case = TRUE
          )
        ) ~ "Fearful",
        
        str_detect(
          self_classification_raw_clean,
          regex(
            "social status|colleagues|friends|family|estatus social|norms|normas",
            ignore_case = TRUE
          )
        ) ~ "Influencer",
        
        str_detect(
          self_classification_raw_clean,
          regex(
            "comfort|well-being|bienestar|confort",
            ignore_case = TRUE
          )
        ) ~ "Careful",
        
        str_detect(
          self_classification_raw_clean,
          regex(
            "not very interested|stick to what|no.*interes|no muy interesado|used to",
            ignore_case = TRUE
          )
        ) ~ "Uninterested",
        
        str_detect(
          self_classification_raw_clean,
          regex(
            "early adopter|latest trends|tendencias|pioner",
            ignore_case = TRUE
          )
        ) ~ "Early adopter",
        
        str_detect(
          self_classification_raw_clean,
          regex(
            "ethical|social commitment|compromiso.*social|environmental protection|protecci",
            ignore_case = TRUE
          )
        ) ~ "Stubborn",
        
        str_detect(
          self_classification_raw_clean,
          regex(
            "cost-effective|well informed|costs and benefits|costes y beneficios|costos y beneficios|rentable",
            ignore_case = TRUE
          )
        ) ~ "Homo economicus",
        
        str_detect(
          self_classification_raw_clean,
          regex(
            "none of the above|Νone of the above|none|ninguna",
            ignore_case = TRUE
          )
        ) ~ "None",
        
        is.na(
          self_classification_raw_clean
        ) |
          self_classification_raw_clean == "" ~
          NA_character_,
        
        TRUE ~
          "Other_unclassified"
      )
    )
} else {
  for (
    col in c(
      "self_classification_raw",
      "self_classification_raw_clean",
      "self_classification_archetype_model"
    )
  ) {
    if (!col %in% names(df)) {
      df[[col]] <- NA_character_
    }
  }
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
  file.path(
    out_dir,
    "self_classification_mapping_used.csv"
  )
)

write_lines(
  paste0(
    "self_classification_col_detected: ",
    self_classification_col
  ),
  file.path(
    out_dir,
    "self_classification_column_detected.txt"
  )
)


# Variables descriptivas disponibles

vars_to_summarise <- candidate_vars[
  candidate_vars %in%
    names(df)
]

vars_to_summarise <- unique(
  c(
    vars_to_summarise,
    "self_classification_archetype_model"
  )
)

vars_to_summarise <- vars_to_summarise[
  vars_to_summarise %in%
    names(df)
]

if (
  "political_left_right_model" %in%
  names(df)
) {
  df <- df %>%
    mutate(
      political_left_right_num =
        suppressWarnings(
          as.numeric(
            political_left_right_model
          )
        ),
      
      political_lr_group = case_when(
        is.na(
          political_left_right_num
        ) ~ NA_character_,
        
        political_left_right_num < 20 ~
          "00_19_extreme_left",
        
        political_left_right_num < 40 ~
          "20_39_left",
        
        political_left_right_num < 60 ~
          "40_59_centre",
        
        political_left_right_num < 80 ~
          "60_79_right",
        
        political_left_right_num <= 100 ~
          "80_100_extreme_right",
        
        TRUE ~
          NA_character_
      )
    )
  
  vars_to_summarise <- unique(
    c(
      vars_to_summarise,
      "political_lr_group"
    )
  )
}


# Muestras pre-bootstrap

if (
  "usable_for_main_analysis" %in%
  names(df)
) {
  df <- df %>%
    mutate(
      usable_for_main_analysis_flag =
        as_logical_flag(
          usable_for_main_analysis
        )
    )
} else {
  df$usable_for_main_analysis_flag <- TRUE
}

if (
  "usable_for_clustering" %in%
  names(df)
) {
  df <- df %>%
    mutate(
      usable_for_clustering_flag =
        as_logical_flag(
          usable_for_clustering
        )
    )
} else {
  df$usable_for_clustering_flag <- TRUE
}

df_all <- df %>%
  mutate(
    sample_context =
      "01_all_integrated_rows"
  )

df_main <- df %>%
  filter(
    usable_for_main_analysis_flag ==
      TRUE
  ) %>%
  mutate(
    sample_context =
      "02_usable_for_main_analysis"
  )

df_cluster <- df %>%
  filter(
    usable_for_clustering_flag ==
      TRUE
  ) %>%
  mutate(
    sample_context =
      "03_usable_for_clustering"
  )

context_list <- list(
  "01_all_integrated_rows" =
    df_all,
  
  "02_usable_for_main_analysis" =
    df_main,
  
  "03_usable_for_clustering" =
    df_cluster
)


# Descriptivos pre-bootstrap

overall_distribution <- map_dfr(
  names(context_list),
  function(context_name) {
    map_dfr(
      vars_to_summarise,
      ~ summarise_one_var(
        data =
          context_list[
            [context_name]
          ],
        var = .x,
        context_name =
          context_name
      )
    )
  }
)

by_source_distribution <- map_dfr(
  names(context_list),
  function(context_name) {
    map_dfr(
      vars_to_summarise,
      ~ summarise_one_var_by_group(
        data =
          context_list[
            [context_name]
          ],
        var = .x,
        group_var =
          "dataset_source",
        context_name =
          context_name
      )
    )
  }
)

by_subsample_distribution <- map_dfr(
  names(context_list),
  function(context_name) {
    map_dfr(
      vars_to_summarise,
      ~ summarise_one_var_by_group(
        data =
          context_list[
            [context_name]
          ],
        var = .x,
        group_var =
          "subsample",
        context_name =
          context_name
      )
    )
  }
)

write_csv(
  overall_distribution,
  file.path(
    out_dir,
    "sociodemographic_distribution_overall.csv"
  )
)

write_csv(
  by_source_distribution,
  file.path(
    out_dir,
    "sociodemographic_distribution_by_source.csv"
  )
)

write_csv(
  by_subsample_distribution,
  file.path(
    out_dir,
    "sociodemographic_distribution_by_subsample.csv"
  )
)


sample_flow <- bind_rows(
  df_all,
  df_main,
  df_cluster
) %>%
  mutate(
    comparison_region =
      clean_category(
        comparison_region
      ),
    
    subsample =
      clean_category(
        subsample
      ),
    
    dataset_source =
      clean_category(
        dataset_source
      ),
    
    row_quality_final =
      clean_category(
        row_quality_final
      )
  ) %>%
  count(
    sample_context,
    comparison_region,
    subsample,
    dataset_source,
    row_quality_final,
    name = "n"
  ) %>%
  group_by(
    sample_context,
    comparison_region,
    subsample,
    dataset_source
  ) %>%
  mutate(
    total_n = sum(n),
    prop = n / total_n
  ) %>%
  ungroup()

write_csv(
  sample_flow,
  file.path(
    out_dir,
    "sample_flow_by_context_subsample_quality.csv"
  )
)


# Autoclasificación RV

self_classification_rv_distribution <- df %>%
  filter(
    dataset_source == "rv"
  ) %>%
  mutate(
    self_classification_archetype_model =
      clean_category(
        self_classification_archetype_model
      )
  ) %>%
  count(
    self_classification_archetype_model,
    name = "n"
  ) %>%
  mutate(
    variable =
      "self_classification_archetype_model",
    total_n = sum(n),
    prop = n / total_n
  ) %>%
  transmute(
    variable,
    category =
      self_classification_archetype_model,
    n,
    total_n,
    prop
  ) %>%
  arrange(
    desc(prop)
  )

write_csv(
  self_classification_rv_distribution,
  file.path(
    out_dir,
    "self_classification_rv_distribution.csv"
  )
)


# Figuras pre-bootstrap

for (
  context_name in
  names(context_list)
) {
  for (
    var in
    vars_to_summarise
  ) {
    plot_overall(
      overall_distribution,
      var,
      context_name
    )
    
    plot_by_group(
      by_source_distribution,
      var,
      context_name,
      "dataset source",
      "prebootstrap_by_source"
    )
    
    plot_by_group(
      by_subsample_distribution,
      var,
      context_name,
      "subsample",
      "prebootstrap_by_subsample"
    )
  }
}


# Bootstrap final 04_2f

bootstrap_colnames <- names(
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

missing_boot_cols <- setdiff(
  required_boot_cols,
  bootstrap_colnames
)

if (length(missing_boot_cols)) {
  stop(
    "Faltan columnas necesarias en el bootstrap combinado: ",
    paste(
      missing_boot_cols,
      collapse = ", "
    )
  )
}

bootstrap_cols_to_read <- intersect(
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
  bootstrap_colnames
)

boot_index <- read_csv(
  bootstrap_index_file,
  col_select = all_of(
    bootstrap_cols_to_read
  ),
  show_col_types = FALSE
) %>%
  mutate(
    bootstrap_id =
      as.integer(
        bootstrap_id
      ),
    
    integrated_row_id =
      as.character(
        integrated_row_id
      ),
    
    comparison_region =
      as.character(
        comparison_region
      ),
    
    subsample =
      as.character(
        subsample
      ),
    
    dataset_source =
      as.character(
        dataset_source
      )
  )

boot_ids <- sort(
  unique(
    boot_index$bootstrap_id
  )
)

if (
  length(boot_ids) !=
  EXPECTED_N_BOOT
) {
  stop(
    "Se esperaban ",
    EXPECTED_N_BOOT,
    " bootstraps y se han encontrado ",
    length(boot_ids),
    "."
  )
}

if (
  !identical(
    boot_ids,
    seq_len(
      EXPECTED_N_BOOT
    )
  )
) {
  stop(
    "Los bootstrap_id no son exactamente 1:",
    EXPECTED_N_BOOT,
    "."
  )
}

wrong_regions <- setdiff(
  unique(
    na.omit(
      boot_index$comparison_region
    )
  ),
  c(
    "EUROPE",
    "LATAM"
  )
)

if (length(wrong_regions)) {
  stop(
    "Hay regiones inesperadas en el bootstrap final: ",
    paste(
      wrong_regions,
      collapse = ", "
    )
  )
}

wrong_subsamples <- setdiff(
  unique(
    na.omit(
      boot_index$subsample
    )
  ),
  c(
    "DIEGO",
    "RENOVISOR",
    "WHY_EUROPE",
    "WHY_LATAM"
  )
)

if (length(wrong_subsamples)) {
  stop(
    "Hay submuestras inesperadas en el bootstrap final: ",
    paste(
      wrong_subsamples,
      collapse = ", "
    )
  )
}


# Unir draws con variables originales
#
# df_boot_base es individual y tiene una fila por persona.
# boot_index NO se deduplica porque las repeticiones forman parte
# del muestreo bootstrap con reemplazo.

df_boot_base <- df %>%
  distinct(
    integrated_row_id,
    .keep_all = TRUE
  )

missing_boot_ids <- boot_index %>%
  distinct(
    integrated_row_id
  ) %>%
  anti_join(
    df_boot_base %>%
      distinct(
        integrated_row_id
      ),
    by = "integrated_row_id"
  )

if (nrow(missing_boot_ids)) {
  stop(
    "Hay ",
    nrow(missing_boot_ids),
    " IDs del bootstrap que no aparecen en el dataset descriptivo."
  )
}

extra_df_cols <- setdiff(
  names(df_boot_base),
  names(boot_index)
)

boot_df <- boot_index %>%
  left_join(
    df_boot_base %>%
      select(
        integrated_row_id,
        all_of(
          extra_df_cols
        )
      ),
    by = "integrated_row_id"
  )


# Tamaños bootstrap

bootstrap_sample_sizes <- bind_rows(
  boot_df %>%
    count(
      bootstrap_id,
      name = "n_draws"
    ) %>%
    mutate(
      analysis_sample =
        "COMPLETE"
    ),
  
  boot_df %>%
    count(
      bootstrap_id,
      comparison_region,
      name = "n_draws"
    ) %>%
    transmute(
      bootstrap_id,
      analysis_sample =
        comparison_region,
      n_draws
    ),
  
  boot_df %>%
    count(
      bootstrap_id,
      subsample,
      name = "n_draws"
    ) %>%
    transmute(
      bootstrap_id,
      analysis_sample =
        subsample,
      n_draws
    )
) %>%
  complete(
    bootstrap_id = boot_ids,
    analysis_sample = ANALYSIS_SAMPLES,
    fill = list(
      n_draws = 0L
    )
  ) %>%
  mutate(
    analysis_sample = factor(
      analysis_sample,
      levels = ANALYSIS_SAMPLES
    )
  ) %>%
  arrange(
    bootstrap_id,
    analysis_sample
  )

bootstrap_sample_sizes_wide <- bootstrap_sample_sizes %>%
  mutate(
    analysis_sample =
      as.character(
        analysis_sample
      )
  ) %>%
  pivot_wider(
    names_from =
      analysis_sample,
    values_from =
      n_draws,
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
    
    n_distinct(
      bootstrap_sample_sizes_wide$EUROPE
    ) == 1,
    
    n_distinct(
      bootstrap_sample_sizes_wide$LATAM
    ) == 1,
    
    n_distinct(
      bootstrap_sample_sizes_wide$COMPLETE
    ) == 1
  )
)

if (
  any(
    !bootstrap_structure_checks$passed
  )
) {
  print(
    bootstrap_structure_checks,
    n = Inf
  )
  
  stop(
    "La estructura del bootstrap combinado no es coherente."
  )
}

bootstrap_sample_sizes_summary <- bootstrap_sample_sizes %>%
  group_by(
    analysis_sample
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
    n_bootstraps = n_distinct(
      bootstrap_id
    ),
    .groups = "drop"
  )

write_csv(
  bootstrap_sample_sizes,
  file.path(
    out_dir,
    "bootstrap_sample_sizes_by_run.csv"
  )
)

write_csv(
  bootstrap_sample_sizes_summary,
  file.path(
    out_dir,
    "bootstrap_sample_sizes_summary.csv"
  )
)

write_csv(
  bootstrap_structure_checks,
  file.path(
    out_dir,
    "bootstrap_structure_checks.csv"
  )
)


# Distribuciones post-bootstrap

bootstrap_specific_vars <- c(
  "bootstrap_source",
  "target_sample_type",
  "target_electoral_group",
  "country_code",
  "country",
  "income_band"
)

boot_vars <- unique(
  c(
    vars_to_summarise,
    bootstrap_specific_vars
  )
)

boot_vars <- intersect(
  boot_vars,
  names(boot_df)
)

bootstrap_distribution <- map_dfr(
  boot_vars,
  ~ compute_bootstrap_distribution(
    data = boot_df,
    var = .x,
    boot_ids = boot_ids
  )
)

bootstrap_distribution_summary <- bootstrap_distribution %>%
  group_by(
    analysis_sample,
    variable,
    category
  ) %>%
  summarise(
    mean_n = mean(
      n,
      na.rm = TRUE
    ),
    
    sd_n = sd(
      n,
      na.rm = TRUE
    ),
    
    mean_total_n = mean(
      total_n,
      na.rm = TRUE
    ),
    
    sd_total_n = sd(
      total_n,
      na.rm = TRUE
    ),
    
    mean_prop = mean(
      prop,
      na.rm = TRUE
    ),
    
    sd_prop = sd(
      prop,
      na.rm = TRUE
    ),
    
    n_bootstraps = n_distinct(
      bootstrap_id
    ),
    
    .groups = "drop"
  ) %>%
  arrange(
    variable,
    analysis_sample,
    desc(
      mean_prop
    ),
    category
  )

write_csv(
  bootstrap_distribution,
  file.path(
    out_dir,
    "sociodemographic_distribution_bootstrap_by_run.csv"
  )
)

write_csv(
  bootstrap_distribution_summary,
  file.path(
    out_dir,
    "sociodemographic_distribution_bootstrap_summary.csv"
  )
)


# Figuras post-bootstrap

for (
  var in
  boot_vars
) {
  p_data <- bootstrap_distribution_summary %>%
    filter(
      variable == var
    ) %>%
    mutate(
      analysis_sample = factor(
        analysis_sample,
        levels = ANALYSIS_SAMPLES
      ),
      
      category = fct_reorder(
        category,
        mean_prop
      ),
      
      label = percent(
        mean_prop,
        accuracy = 1
      )
    )
  
  if (!nrow(p_data)) {
    next
  }
  
  n_categories <- n_distinct(
    p_data$category
  )
  
  plot_height <- max(
    8,
    min(
      16,
      5 +
        0.35 *
        n_categories
    )
  )
  
  p <- ggplot(
    p_data,
    aes(
      x = category,
      y = mean_prop,
      fill = category
    )
  ) +
    geom_col(
      show.legend = FALSE
    ) +
    geom_errorbar(
      aes(
        ymin = pmax(
          mean_prop -
            sd_prop,
          0
        ),
        
        ymax = pmin(
          mean_prop +
            sd_prop,
          1
        )
      ),
      width = 0.2
    ) +
    geom_text(
      aes(
        label = label
      ),
      hjust = -0.05,
      size = 2.7
    ) +
    scale_fill_manual(
      values = category_palette(
        p_data$category
      ),
      guide = "none"
    ) +
    scale_y_continuous(
      labels = percent_format(
        accuracy = 1
      ),
      expand = expansion(
        mult = c(
          0,
          0.25
        )
      )
    ) +
    coord_flip(
      clip = "off"
    ) +
    facet_wrap(
      ~ analysis_sample,
      ncol = 2,
      scales = "free_y"
    ) +
    theme_minimal(
      base_size = 10
    ) +
    theme(
      plot.margin = margin(
        10,
        60,
        10,
        10
      )
    ) +
    labs(
      title = paste0(
        "Bootstrap-adjusted distribution of ",
        var
      ),
      
      subtitle = paste0(
        "Final bootstrap 04_2f | ",
        EXPECTED_N_BOOT,
        " bootstrap replicates"
      ),
      
      x = NULL,
      y = "Mean percentage across bootstrap samples ± SD"
    )
  
  ggsave(
    file.path(
      fig_dir,
      paste0(
        "postbootstrap_",
        safe_filename(
          var
        ),
        ".png"
      )
    ),
    plot = p,
    width = 14,
    height = plot_height,
    dpi = 300
  )
}


# Parámetros

parameters <- tibble(
  parameter = c(
    "input_file",
    "bootstrap_index_file",
    "output_dir",
    "figures_dir",
    "n_rows_all",
    "n_rows_main",
    "n_rows_clustering",
    "n_bootstraps",
    "analysis_samples",
    "variables_prebootstrap",
    "variables_postbootstrap"
  ),
  
  value = c(
    input_file,
    bootstrap_index_file,
    out_dir,
    fig_dir,
    as.character(
      nrow(df_all)
    ),
    as.character(
      nrow(df_main)
    ),
    as.character(
      nrow(df_cluster)
    ),
    as.character(
      length(boot_ids)
    ),
    paste(
      ANALYSIS_SAMPLES,
      collapse = ", "
    ),
    paste(
      vars_to_summarise,
      collapse = ", "
    ),
    paste(
      boot_vars,
      collapse = ", "
    )
  )
)

write_csv(
  parameters,
  file.path(
    out_dir,
    "sociodemographic_distribution_parameters.csv"
  )
)


# Resumen

cat("\n04_2g. DISTRIBUCIÓN SOCIODEMOGRÁFICA COMPLETADA\n")

cat(
  "\nPRE-BOOTSTRAP",
  "\nMuestra integrada: ",
  nrow(df_all),
  "\nUsable análisis principal: ",
  nrow(df_main),
  "\nUsable clustering: ",
  nrow(df_cluster),
  "\n",
  sep = ""
)

cat(
  "\nPOST-BOOTSTRAP",
  "\nNúmero de bootstraps: ",
  length(boot_ids),
  "\n\n",
  sep = ""
)

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

cat(
  "\nAutoclasificación RV detectada en: ",
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
  "\nListo. 04_2g generado con el bootstrap final EUROPE + LATAM."
)