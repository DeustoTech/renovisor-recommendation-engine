# SCRIPT 08 - BOXPLOTS OF DETERMINANTS BY TTM STAGE - EN

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tibble)
library(purrr)
library(gridExtra)
library(grid)

# ==============================================================================
# 1. LOAD TABLE
# ==============================================================================

df <- read_csv(
  file.path(
    "initial_descriptive_analysis/output/ttm_stage_analysis/csv",
    "ttm_stage_determinant_vector_wide.csv"
  ),
  show_col_types = FALSE
)

glimpse(df)

# ==============================================================================
# 2. DEFINE KEY COLUMNS
# ==============================================================================

id_col <- "participant_id"
stage_col <- "stage"
technology_col <- "technology"
dimension_col <- "dimension"

df <- df %>%
  mutate(
    participant_id = as.character(.data[[id_col]])
  )

# ==============================================================================
# 3. DICTIONARIES IN ENGLISH
# ==============================================================================

determinant_dictionary <- tibble(
  determinant_id = c(
    "profits",
    "credit_score_access_to_funding",
    "risk_profile",
    "added_value",
    "frugality",
    "climate_protection",
    "legal",
    "trust",
    "safety",
    "cost_efficiency",
    "knowledge",
    "own_competence",
    "technical_fit",
    "environmental_concerns",
    "self_satisfaction",
    "commitment",
    "adherence",
    "autonomy",
    "wellbeing",
    "coziness",
    "rights_and_duties",
    "peer_pressure",
    "support",
    "socialising",
    "agreement",
    "novelty",
    "fun",
    "recognition",
    "trends",
    "authority",
    "approval",
    "own_significance"
  ),
  determinant_label = c(
    "Economic profit",
    "Access to funding",
    "Risk profile",
    "Added value",
    "Frugality",
    "Climate protection",
    "Legal compliance",
    "Trust",
    "Safety",
    "Cost efficiency",
    "Knowledge",
    "Own competence",
    "Technical fit",
    "Environmental concern",
    "Self-satisfaction",
    "Commitment",
    "Adherence",
    "Self-sufficiency",
    "Wellbeing",
    "Comfort",
    "Rights and duties",
    "Peer pressure",
    "Social support",
    "Socialising",
    "Agreement",
    "Novelty",
    "Fun",
    "Recognition",
    "Trends",
    "Authority",
    "Approval",
    "Personal significance"
  )
)

dimension_dictionary <- tibble(
  dimension_key = c(
    "FINANCIAL",
    "SECURITY",
    "COMPETENCE",
    "AUTONOMY",
    "PHYSIOLOGICAL",
    "RELATEDNESS",
    "STIMULATION",
    "POPULARITY",
    "MEANING"
  ),
  dimension_label = c(
    "Financial security",
    "Security",
    "Competence",
    "Autonomy",
    "Materiality",
    "Relatedness",
    "Stimulation",
    "Popularity",
    "Meaning"
  )
)

determinant_label_levels <- determinant_dictionary$determinant_label
determinant_id_levels <- determinant_dictionary$determinant_id
dimension_label_levels <- dimension_dictionary$dimension_label

stage_levels <- c(
  "General",
  "Implemented",
  "Aware / would consider",
  "Unaware but curious"
)

stage_order_profile <- c(
  "Implemented",
  "Aware / would consider",
  "Unaware but curious"
)

profile_order <- c(
  "Careful",
  "Activist",
  "Fearful",
  "Homo economicus",
  "Stubborn",
  "Influencer",
  "Uninterested",
  "Early adopter"
)

rank_levels <- c(
  "1st",
  "2nd",
  "3rd",
  "4th",
  "5th",
  "6th-10th",
  "11th-15th",
  "Rest"
)

recode_dimension_label <- function(x) {
  x <- as.character(x)
  
  case_when(
    x %in% c("FINANCIAL", "Financial", "Financiera", "Financiero", "Dimensión financiera", "Seguridad financiera", "Seguridad Financiera", "Financial security") ~ "Financial security",
    x %in% c("SECURITY", "Security", "Seguridad") ~ "Security",
    x %in% c("COMPETENCE", "Competence", "Competencia") ~ "Competence",
    x %in% c("AUTONOMY", "Autonomy", "Autonomía") ~ "Autonomy",
    x %in% c("PHYSIOLOGICAL", "Physiological", "Fisiología", "Bienestar físico", "Materialidad", "Materiality") ~ "Materiality",
    x %in% c("RELATEDNESS", "Relatedness", "Relación", "Relaciones sociales", "Vinculación", "Vinculación sociales") ~ "Relatedness",
    x %in% c("STIMULATION", "Stimulation", "Estimulación", "Estímulo", "Estimulo") ~ "Stimulation",
    x %in% c("POPULARITY", "Popularity", "Popularidad") ~ "Popularity",
    x %in% c("MEANING", "Meaning", "Sentido", "Significado") ~ "Meaning",
    TRUE ~ NA_character_
  )
}

recode_stage_label <- function(x) {
  x <- as.character(x)
  
  case_when(
    x %in% c("General") ~ "General",
    x %in% c("Implementada", "Implemented") ~ "Implemented",
    x %in% c("La conoce / la consideraría", "Aware / would consider") ~ "Aware / would consider",
    x %in% c(
      "No la conoce, pero le genera curiosidad",
      "No la conoce",
      "Unaware but curious",
      "Unaware"
    ) ~ "Unaware but curious",
    TRUE ~ x
  )
}

clean_filename <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[áàäâ]", "a") %>%
    str_replace_all("[éèëê]", "e") %>%
    str_replace_all("[íìïî]", "i") %>%
    str_replace_all("[óòöô]", "o") %>%
    str_replace_all("[úùüû]", "u") %>%
    str_replace_all("ñ", "n") %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

# ==============================================================================
# 4. DEFINE DETERMINANT COLUMNS
# ==============================================================================

non_determinant_cols <- c(
  id_col,
  stage_col,
  technology_col,
  dimension_col,
  "dimension_key",
  "dimension_label",
  "stage_dimension_id",
  "n_determinants_filled"
)

determinant_cols <- setdiff(names(df), non_determinant_cols)

determinant_cols <- determinant_cols[
  sapply(df[determinant_cols], is.numeric)
]

cat("Number of determinants detected:", length(determinant_cols), "\n")
print(determinant_cols)

# ==============================================================================
# 5. OUTPUT FOLDERS
# ==============================================================================

base_output_dir <- "initial_descriptive_analysis/output/boxplots_ttm_determinants"

csv_dir <- file.path(base_output_dir, "csv")
plots_dir <- file.path(base_output_dir, "plots")
pdf_dir <- file.path(base_output_dir, "pdf")
profile_rank_dir <- file.path(plots_dir, "by_profile_stage_rank")

dir.create(csv_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(pdf_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(profile_rank_dir, showWarnings = FALSE, recursive = TRUE)

# Prefix for all English output files
output_prefix <- "en_"

out_file <- function(directory, filename) {
  file.path(directory, paste0(output_prefix, filename))
}

# ==============================================================================
# 6. VISUAL CONFIGURATION
# ==============================================================================

# Common palette used across all scripts.
# Rule:
# category 1 = colour 1
# category 2 = colour 2
# category 3 = colour 3
# etc.

main_palette <- c(
  "#0072B2", "#56B4E9", "#009E73", "#E69F00",
  "#D55E00", "#CC79A7", "#F0E442", "#999999",
  "#332288", "#88CCEE", "#44AA99", "#DDCC77",
  "#117733", "#882255", "#AA4499", "#661100",
  "#6699CC", "#AA4466", "#4477AA", "#228833",
  "#CC6677", "#AA3377", "#BBBBBB", "#000000",
  "#66CCEE", "#CCBB44", "#EE6677", "#EE7733",
  "#0077BB", "#33BBEE", "#009988", "#EE3377"
)

make_named_palette <- function(levels_vec) {
  levels_vec <- as.character(levels_vec)
  colors <- rep(main_palette, length.out = length(levels_vec))
  names(colors) <- levels_vec
  colors
}

stage_colors <- make_named_palette(stage_levels)
stage_profile_colors <- make_named_palette(stage_order_profile)
determinant_colors <- make_named_palette(determinant_label_levels)
dimension_colors <- make_named_palette(dimension_label_levels)
profile_colors <- make_named_palette(profile_order)
rank_colors <- make_named_palette(rank_levels)

box_color <- "#2C3E50"
outlier_color <- "#4F4F4F"
other_fill_color <- "#D9D9D9"

plot_base_size <- 15
plot_title_size <- 18
plot_subtitle_size <- 13
plot_axis_title_size <- 15
plot_axis_text_size <- 15
plot_strip_text_size <- 13
plot_legend_title_size <- 14
plot_legend_text_size <- 13

plot_label_size <- 4
plot_dense_label_size <- 3.8

theme_boxplot <- theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(face = "bold", size = plot_title_size),
    plot.subtitle = element_text(size = plot_subtitle_size),
    axis.title.x = element_text(size = plot_axis_title_size, margin = margin(t = 8)),
    axis.title.y = element_text(size = plot_axis_title_size, margin = margin(r = 8)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = plot_axis_text_size),
    axis.text.y = element_text(size = plot_axis_text_size),
    strip.text = element_text(face = "bold", size = plot_strip_text_size),
    legend.title = element_text(face = "bold", size = plot_legend_title_size),
    legend.text = element_text(size = plot_legend_text_size),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.margin = margin(12, 45, 12, 12)
  )

theme_boxplot_facets <- theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(face = "bold", size = plot_title_size),
    plot.subtitle = element_text(size = plot_subtitle_size),
    axis.title.x = element_text(size = plot_axis_title_size, margin = margin(t = 8)),
    axis.title.y = element_text(size = plot_axis_title_size, margin = margin(r = 8)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = plot_axis_text_size),
    axis.text.y = element_text(size = plot_axis_text_size),
    strip.text = element_text(face = "bold", size = plot_strip_text_size),
    legend.title = element_text(face = "bold", size = plot_legend_title_size),
    legend.text = element_text(size = plot_legend_text_size),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.margin = margin(12, 45, 12, 12)
  )

# ==============================================================================
# 7. HELPER FUNCTIONS
# ==============================================================================

save_plot <- function(plot, filename, width = 12, height = 7) {
  ggsave(
    filename = out_file(plots_dir, paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}

make_subtitle <- function(data) {
  paste0(
    "n participants = ", n_distinct(data$participant_id),
    "; valid participant-determinant observations = ", nrow(data)
  )
}

# ==============================================================================
# 8. LONG FORMAT AND TRANSLATION OF DETERMINANTS / DIMENSIONS / STAGES
# ==============================================================================

df_long <- df %>%
  pivot_longer(
    cols = all_of(determinant_cols),
    names_to = "determinant_id",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  mutate(
    participant_id = as.character(participant_id),
    value = as.numeric(value),
    stage = recode_stage_label(.data[[stage_col]]),
    dimension = recode_dimension_label(.data[[dimension_col]])
  ) %>%
  filter(value >= 0, value <= 100) %>%
  left_join(
    determinant_dictionary,
    by = "determinant_id"
  ) %>%
  mutate(
    determinant = coalesce(determinant_label, determinant_id),
    determinant = factor(determinant, levels = determinant_label_levels),
    dimension = factor(dimension, levels = dimension_label_levels)
  )

write_csv(
  df_long,
  out_file(csv_dir, "ttm_determinants_long_for_boxplots.csv")
)

glimpse(df_long)

# ==============================================================================
# 9. CREATE GENERAL DATASET
# ==============================================================================

df_general_long <- df_long %>%
  mutate(stage = "General")

df_all_long <- bind_rows(
  df_general_long,
  df_long
) %>%
  mutate(
    stage = factor(
      stage,
      levels = stage_levels
    ),
    determinant = factor(determinant, levels = determinant_label_levels),
    dimension = factor(dimension, levels = dimension_label_levels)
  )

# ==============================================================================
# 10. BOXPLOTS OF 32 DETERMINANTS BY STAGE
# ==============================================================================

create_32det_boxplot <- function(data, selected_stage, title) {
  
  data_filtered <- data %>%
    filter(stage == selected_stage) %>%
    mutate(
      determinant = reorder(determinant, value, median)
    )
  
  ggplot(
    data_filtered,
    aes(
      x = determinant,
      y = value,
      fill = determinant
    )
  ) +
    geom_boxplot(
      color = box_color,
      outlier.color = outlier_color,
      outlier.alpha = 0.45
    ) +
    scale_fill_manual(values = determinant_colors, drop = FALSE) +
    guides(fill = "none") +
    labs(
      title = title,
      subtitle = make_subtitle(data_filtered),
      x = "Determinant",
      y = "Value"
    ) +
    theme_boxplot
}

plot_32_general <- create_32det_boxplot(
  df_all_long,
  "General",
  "Distribution of the 32 determinants - General"
)

plot_32_implemented <- create_32det_boxplot(
  df_all_long,
  "Implemented",
  "Distribution of the 32 determinants - Implemented"
)

plot_32_consider <- create_32det_boxplot(
  df_all_long,
  "Aware / would consider",
  "Distribution of the 32 determinants - Aware / would consider"
)

plot_32_curious <- create_32det_boxplot(
  df_all_long,
  "Unaware but curious",
  "Distribution of the 32 determinants - Unaware but curious"
)

plots_32 <- list(
  general = plot_32_general,
  implemented = plot_32_implemented,
  aware_consider = plot_32_consider,
  unaware_curious = plot_32_curious
)

walk2(
  plots_32,
  names(plots_32),
  ~ save_plot(.x, paste0("boxplot_32det_", .y), width = 16, height = 9)
)

pdf(
  file = out_file(pdf_dir, "boxplots_32det_individual.pdf"),
  width = 16,
  height = 9
)

for (p in plots_32) {
  print(p)
}

dev.off()

# ==============================================================================
# 11. 2x2 COMPARISON
# ==============================================================================

plot_32_comparison_2x2 <- grid.arrange(
  plot_32_general,
  plot_32_implemented,
  plot_32_consider,
  plot_32_curious,
  ncol = 2,
  nrow = 2,
  top = textGrob(
    "Comparison of the 32 determinants by stage",
    gp = gpar(fontsize = plot_title_size, fontface = "bold")
  )
)

ggsave(
  filename = out_file(plots_dir, "boxplots_32det_comparison_2x2.png"),
  plot = plot_32_comparison_2x2,
  width = 24,
  height = 17,
  dpi = 300
)

ggsave(
  filename = out_file(plots_dir, "boxplots_32det_comparison_2x2.pdf"),
  plot = plot_32_comparison_2x2,
  width = 24,
  height = 17
)

# ==============================================================================
# 12. COMPARISON BY DETERMINANT
# ==============================================================================

plot_determinants_by_stage <- df_all_long %>%
  ggplot(
    aes(
      x = stage,
      y = value,
      fill = stage
    )
  ) +
  geom_boxplot(
    color = box_color,
    outlier.color = outlier_color,
    outlier.alpha = 0.3
  ) +
  scale_fill_manual(values = stage_colors, drop = FALSE) +
  facet_wrap(~ determinant, scales = "free_y") +
  labs(
    title = "Comparison of determinants by stage",
    subtitle = make_subtitle(df_all_long),
    x = "Stage",
    y = "Value",
    fill = "Stage"
  ) +
  theme_boxplot_facets

print(plot_determinants_by_stage)

save_plot(
  plot_determinants_by_stage,
  "boxplot_comparison_by_determinant",
  width = 20,
  height = 15
)

ggsave(
  filename = out_file(plots_dir, "boxplot_comparison_by_determinant.pdf"),
  plot = plot_determinants_by_stage,
  width = 20,
  height = 15
)

# ==============================================================================
# 13. BOXPLOTS BY PROFILE: DETERMINANTS BY STAGE WITH RANKING COLOUR
# ==============================================================================

df_profile <- read_csv(
  file.path(
    "initial_descriptive_analysis/output/data_preparation/csv",
    "df_analysis_ready.csv"
  ),
  show_col_types = FALSE
) %>%
  mutate(
    participant_id = coalesce(
      as.character(join_key),
      as.character(prolific_id),
      as.character(identification_code),
      as.character(row_number())
    )
  )

self_col <- "which_statement_best_describes_you_when_making_an_investment_decision_related_to_your_household_final"

df_self_profile <- df_profile %>%
  mutate(
    self_response_raw = as.character(.data[[self_col]]),
    self_response_raw = str_squish(str_trim(self_response_raw)),
    self_response_raw = na_if(self_response_raw, ""),
    
    self_profile = case_when(
      is.na(self_response_raw) ~ "Missing",
      str_detect(self_response_raw, regex("comfort", ignore_case = TRUE)) ~ "Careful",
      str_detect(self_response_raw, regex("environmental impact", ignore_case = TRUE)) ~ "Activist",
      str_detect(self_response_raw, regex("safety", ignore_case = TRUE)) ~ "Fearful",
      str_detect(self_response_raw, regex("cost-effective", ignore_case = TRUE)) ~ "Homo economicus",
      str_detect(self_response_raw, regex("ethical", ignore_case = TRUE)) ~ "Stubborn",
      str_detect(self_response_raw, regex("social status", ignore_case = TRUE)) ~ "Influencer",
      str_detect(self_response_raw, regex("not very interested", ignore_case = TRUE)) ~ "Uninterested",
      str_detect(self_response_raw, regex("early adopter", ignore_case = TRUE)) ~ "Early adopter",
      str_detect(self_response_raw, regex("[NΝ]one of the above", ignore_case = TRUE)) ~ "Unclassified",
      TRUE ~ "Other"
    )
  ) %>%
  select(participant_id, self_profile)

df_all_long_profile <- df_all_long %>%
  left_join(df_self_profile, by = "participant_id") %>%
  filter(
    stage != "General",
    !is.na(self_profile),
    !self_profile %in% c("Missing", "Other", "Unclassified")
  ) %>%
  mutate(
    self_profile = factor(self_profile, levels = profile_order),
    stage = factor(stage, levels = stage_order_profile),
    determinant = as.character(determinant)
  )

ranking_by_profile_stage <- df_all_long_profile %>%
  group_by(self_profile, stage, determinant) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(self_profile, stage) %>%
  arrange(desc(mean_value), .by_group = TRUE) %>%
  mutate(
    rank_stage = row_number(),
    rank_group = case_when(
      rank_stage == 1 ~ "1st",
      rank_stage == 2 ~ "2nd",
      rank_stage == 3 ~ "3rd",
      rank_stage == 4 ~ "4th",
      rank_stage == 5 ~ "5th",
      rank_stage <= 10 ~ "6th-10th",
      rank_stage <= 15 ~ "11th-15th",
      TRUE ~ "Rest"
    )
  ) %>%
  ungroup()

df_all_long_profile_ranked <- df_all_long_profile %>%
  left_join(
    ranking_by_profile_stage %>%
      select(self_profile, stage, determinant, mean_value, rank_stage, rank_group),
    by = c("self_profile", "stage", "determinant")
  ) %>%
  mutate(
    rank_group = factor(rank_group, levels = rank_levels),
    determinant = factor(determinant, levels = determinant_label_levels)
  )

plots_profile_stage_rank <- list()

profiles_to_plot <- df_all_long_profile_ranked %>%
  pull(self_profile) %>%
  as.character() %>%
  unique()

profiles_to_plot <- profile_order[profile_order %in% profiles_to_plot]

cat("Profiles detected for ranking plots:\n")
print(profiles_to_plot)

cat("Number of rows in df_all_long_profile_ranked:", nrow(df_all_long_profile_ranked), "\n")

for (prof in profiles_to_plot) {
  
  data_prof <- df_all_long_profile_ranked %>%
    filter(as.character(self_profile) == prof) %>%
    mutate(
      determinant = factor(
        determinant,
        levels = determinant_label_levels
      )
    )
  
  plot_prof <- data_prof %>%
    ggplot(
      aes(
        x = stage,
        y = value,
        fill = rank_group
      )
    ) +
    geom_boxplot(
      color = box_color,
      outlier.color = outlier_color,
      outlier.alpha = 0.30
    ) +
    facet_wrap(
      ~ determinant,
      scales = "free_y",
      drop = FALSE
    ) +
    scale_fill_manual(values = rank_colors, drop = FALSE) +
    labs(
      title = paste("Determinants by stage - Profile:", prof),
      subtitle = paste0(
        "Colour indicates determinant ranking within each stage and profile. ",
        "n participants = ", n_distinct(data_prof$participant_id),
        "; valid observations = ", nrow(data_prof)
      ),
      x = "Stage",
      y = "Value",
      fill = "Ranking"
    ) +
    theme_boxplot_facets
  
  print(plot_prof)
  
  filename <- paste0(
    "boxplot_profile_stage_rank_",
    clean_filename(prof)
  )
  
  ggsave(
    out_file(profile_rank_dir, paste0(filename, ".png")),
    plot_prof,
    width = 22,
    height = 16,
    dpi = 300
  )
  
  ggsave(
    out_file(profile_rank_dir, paste0(filename, ".pdf")),
    plot_prof,
    width = 22,
    height = 16
  )
  
  plots_profile_stage_rank[[prof]] <- plot_prof
}

pdf(
  file = out_file(profile_rank_dir, "boxplots_by_profile_stage_rank_ALL.pdf"),
  width = 22,
  height = 16,
  onefile = TRUE
)

for (p in plots_profile_stage_rank) {
  print(p)
}

dev.off()

# ==============================================================================
# 14. AGGREGATED BOXPLOT BY DIMENSION
# ==============================================================================

plot_dimensions_by_stage <- df_all_long %>%
  ggplot(
    aes(
      x = stage,
      y = value,
      fill = stage
    )
  ) +
  geom_boxplot(
    color = box_color,
    outlier.color = outlier_color,
    outlier.alpha = 0.3
  ) +
  scale_fill_manual(values = stage_colors, drop = FALSE) +
  facet_wrap(~ dimension, scales = "free_y") +
  labs(
    title = "Distribution of values by dimension and stage",
    subtitle = make_subtitle(df_all_long),
    x = "Stage",
    y = "Value",
    fill = "Stage"
  ) +
  theme_boxplot_facets

print(plot_dimensions_by_stage)

save_plot(
  plot_dimensions_by_stage,
  "boxplot_dimensions_by_stage",
  width = 16,
  height = 10
)

ggsave(
  filename = out_file(plots_dir, "boxplot_dimensions_by_stage.pdf"),
  plot = plot_dimensions_by_stage,
  width = 16,
  height = 10
)

# ==============================================================================
# 15. 32 DETERMINANTS HIGHLIGHTING EACH DIMENSION
# ==============================================================================

determinant_dimension_map <- read_csv(
  file.path(
    "initial_descriptive_analysis/output/ttm_stage_analysis/csv",
    "dimension_determinant_mapping_long.csv"
  ),
  show_col_types = FALSE
) %>%
  filter(is_linked == 1) %>%
  left_join(dimension_dictionary, by = "dimension_key") %>%
  left_join(determinant_dictionary, by = "determinant_id") %>%
  transmute(
    determinant = determinant_label,
    dimension = dimension_label
  )

dimensions <- determinant_dimension_map %>%
  pull(dimension) %>%
  unique()

plots_32det_by_dimension <- list()

for (dim_i in dimensions) {
  
  determinants_dim_i <- determinant_dimension_map %>%
    filter(dimension == dim_i) %>%
    pull(determinant) %>%
    unique()
  
  data_dim_highlight <- df_all_long %>%
    mutate(
      determinant_group = if_else(
        as.character(determinant) %in% determinants_dim_i,
        paste0("Belongs to dimension: ", dim_i),
        "Other determinants"
      ),
      determinant = reorder(determinant, value, median)
    )
  
  highlight_values <- setNames(
    c(main_palette[1], other_fill_color),
    c(
      paste0("Belongs to dimension: ", dim_i),
      "Other determinants"
    )
  )
  
  plot_dim_32det <- ggplot(
    data_dim_highlight,
    aes(
      x = determinant,
      y = value,
      fill = determinant_group
    )
  ) +
    geom_boxplot(
      color = box_color,
      outlier.color = outlier_color,
      outlier.alpha = 0.30
    ) +
    facet_wrap(~ stage, ncol = 2) +
    scale_fill_manual(
      values = highlight_values,
      drop = FALSE
    ) +
    labs(
      title = paste("32 determinants highlighting dimension:", dim_i),
      subtitle = paste0(
        make_subtitle(data_dim_highlight),
        "; highlighted determinants = ", length(determinants_dim_i)
      ),
      x = "Determinant",
      y = "Value",
      fill = NULL
    ) +
    theme_boxplot_facets +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, size = plot_axis_text_size)
    )
  
  print(plot_dim_32det)
  
  filename <- paste0(
    "boxplot_32det_highlight_dimension_",
    clean_filename(dim_i)
  )
  
  save_plot(plot_dim_32det, filename, width = 20, height = 12)
  
  ggsave(
    filename = out_file(plots_dir, paste0(filename, ".pdf")),
    plot = plot_dim_32det,
    width = 20,
    height = 12
  )
  
  plots_32det_by_dimension[[dim_i]] <- plot_dim_32det
}

pdf(
  file = out_file(pdf_dir, "boxplots_32det_highlighting_dimensions.pdf"),
  width = 20,
  height = 12
)

for (p in plots_32det_by_dimension) {
  print(p)
}

dev.off()

# ==============================================================================
# 16. FINAL PDF WITH ALL FIGURES
# ==============================================================================

pdf(
  file = out_file(pdf_dir, "boxplots_ttm_determinants_ALL.pdf"),
  width = 16,
  height = 10
)

for (p in plots_32) {
  print(p)
}

grid.arrange(
  plot_32_general,
  plot_32_implemented,
  plot_32_consider,
  plot_32_curious,
  ncol = 2,
  nrow = 2,
  top = textGrob(
    "Comparison of the 32 determinants by stage",
    gp = gpar(fontsize = plot_title_size, fontface = "bold")
  )
)

print(plot_determinants_by_stage)

for (p in plots_profile_stage_rank) {
  print(p)
}

print(plot_dimensions_by_stage)

for (p in plots_32det_by_dimension) {
  print(p)
}

dev.off()

# ==============================================================================
# 17. NUMERICAL SUMMARIES
# ==============================================================================

summary_by_stage_determinant <- df_all_long %>%
  group_by(stage, determinant) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  summary_by_stage_determinant,
  out_file(csv_dir, "summary_boxplots_by_stage_determinant.csv")
)

summary_by_stage_dimension <- df_all_long %>%
  group_by(stage, dimension) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  summary_by_stage_dimension,
  out_file(csv_dir, "summary_boxplots_by_stage_dimension.csv")
)

summary_by_stage_dimension_determinant <- df_all_long %>%
  group_by(stage, dimension, determinant) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  summary_by_stage_dimension_determinant,
  out_file(csv_dir, "summary_boxplots_by_stage_dimension_determinant.csv")
)

cat("Boxplots generated in:", plots_dir, "\n")

cat("\nMain English output files:\n")
cat("- en_ttm_determinants_long_for_boxplots.csv\n")
cat("- en_boxplots_32det_individual.pdf\n")
cat("- en_boxplots_32det_comparison_2x2.png\n")
cat("- en_boxplot_comparison_by_determinant.pdf\n")
cat("- en_boxplot_dimensions_by_stage.pdf\n")
cat("- en_boxplots_32det_highlighting_dimensions.pdf\n")
cat("- en_boxplots_ttm_determinants_ALL.pdf\n")
cat("- en_summary_boxplots_by_stage_determinant.csv\n")
cat("- en_summary_boxplots_by_stage_dimension.csv\n")
cat("- en_summary_boxplots_by_stage_dimension_determinant.csv\n")