# SCRIPT 07 - TTM STAGES, DIMENSIONS AND DETERMINANT VECTOR - EN

# Dimension descriptions:
#
# FINANCIAL:
# Includes the economic aspects of the decision, such as investment cost,
# expected savings, access to funding, economic risks and possible monetary gains
# or losses.
#
# SECURITY:
# Covers factors related to safety, security and reliability.
# It includes trust in the technology, institutions or companies involved,
# personal safety, legal certainty and risk reduction.
#
# COMPETENCE:
# Refers to the perceived ability to make the decision.
# It includes available knowledge, technical understanding, the technical fit of
# the technology with the home and the perceived competence to manage the investment.
#
# AUTONOMY:
# Groups factors linked to personal control, independence and the ability to
# maintain the decision over time. It includes commitment, persistence,
# self-sufficiency and the effort involved in adopting the measure.
#
# PHYSIOLOGICAL:
# Refers to the impact of the technology or renovation on the physical and
# everyday wellbeing of the household. It includes comfort, health, quality of life
# and wellbeing for the person or their family.
#
# RELATEDNESS:
# Includes social and relational factors associated with the decision.
# It considers how the measure affects family, neighbours, community or the social
# environment, as well as agreement with other people involved.
#
# STIMULATION:
# Represents the extent to which the decision is interesting, novel, motivating
# or attractive to the person. It includes curiosity, innovation, learning and
# enjoyment associated with the technology.
#
# POPULARITY:
# Groups factors related to social influence, trends, external recognition and
# the opinion of others. It includes social pressure, approval, prestige or the
# desire to follow popular behaviours.
#
# MEANING:
# Covers factors linked to personal meaning, values and purpose.
# It includes social contribution, added value and the perception that the action
# has meaning beyond economic benefit.

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(purrr)
library(ggplot2)

# 1. LOAD DATA AND DEFINE OUTPUT FOLDERS

df <- read_csv(
  "initial_descriptive_analysis/output/clean_datasets/df_clean_general.csv",
  show_col_types = FALSE
)

base_output_dir <- "initial_descriptive_analysis/output/ttm_stage_analysis"

csv_dir <- file.path(base_output_dir, "csv")
plots_dir <- file.path(base_output_dir, "plots")
pdf_dir <- file.path(base_output_dir, "pdf")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)

# Prefix for all English output files
output_prefix <- "en_"

out_file <- function(directory, filename) {
  file.path(directory, paste0(output_prefix, filename))
}

cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n")

df <- df %>%
  mutate(
    participant_id = coalesce(
      as.character(join_key),
      as.character(prolific_id),
      as.character(identification_code),
      as.character(row_number())
    )
  )


# 2. SAVE PLOT FUNCTION

save_plot_png <- function(plot, filename, width = 10, height = 6) {
  ggsave(
    filename = out_file(plots_dir, paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}


# 3. VISUAL CONFIGURATION

plot_base_size <- 15
plot_title_size <- 18
plot_subtitle_size <- 13
plot_axis_title_size <- 15
plot_axis_text_size <- 15
plot_strip_text_size <- 13
plot_legend_title_size <- 14
plot_legend_text_size <- 13

plot_label_size <- 4
plot_heatmap_label_size <- 3.8

theme_ttm <- theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(face = "bold", size = plot_title_size),
    plot.subtitle = element_text(size = plot_subtitle_size),
    axis.title.x = element_text(size = plot_axis_title_size, margin = margin(t = 8)),
    axis.title.y = element_text(size = plot_axis_title_size, margin = margin(r = 8)),
    axis.text.x = element_text(size = plot_axis_text_size),
    axis.text.y = element_text(size = plot_axis_text_size),
    strip.text = element_text(face = "bold", size = plot_strip_text_size),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = plot_legend_title_size),
    legend.text = element_text(size = plot_legend_text_size),
    panel.grid.minor = element_blank(),
    plot.margin = margin(12, 45, 12, 12)
  )

theme_ttm_heatmap <- theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(face = "bold", size = plot_title_size),
    plot.subtitle = element_text(size = plot_subtitle_size),
    axis.title.x = element_text(size = plot_axis_title_size, margin = margin(t = 8)),
    axis.title.y = element_text(size = plot_axis_title_size, margin = margin(r = 8)),
    axis.text.x = element_text(size = plot_axis_text_size, angle = 35, hjust = 1),
    axis.text.y = element_text(size = plot_axis_text_size),
    strip.text = element_text(face = "bold", size = plot_strip_text_size),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = plot_legend_title_size),
    legend.text = element_text(size = plot_legend_text_size),
    panel.grid.minor = element_blank(),
    plot.margin = margin(12, 20, 12, 12)
  )


# 4. DETERMINANT DICTIONARY

determinant_ids <- c(
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
)

determinant_labels <- c(
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

determinant_cols <- map_chr(determinant_ids, function(id) {
  
  matches <- names(df)[str_detect(names(df), paste0("^", id, "_"))]
  
  if (length(matches) == 0) {
    stop(paste("No column found for determinant:", id))
  }
  
  if (length(matches) > 1) {
    stop(paste(
      "More than one possible column found for determinant:",
      id,
      "\nColumns:",
      paste(matches, collapse = ", ")
    ))
  }
  
  matches
})

determinant_dictionary <- tibble(
  determinant_col = determinant_cols,
  determinant_id = determinant_ids,
  determinant_label = determinant_labels
)

write_csv(
  determinant_dictionary,
  out_file(csv_dir, "determinant_dictionary.csv")
)

print(determinant_dictionary, n = Inf)


# 5. DIMENSION -> DETERMINANT MAPPING

dimension_determinant_mapping <- tibble(
  dimension = c(
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
  
  profits = c(1, 0, 0, 0, 0, 0, 0, 0, 0),
  credit_score_access_to_funding = c(1, 0, 0, 0, 0, 0, 0, 0, 0),
  risk_profile = c(1, 0, 0, 0, 0, 0, 0, 0, 0),
  added_value = c(1, 0, 0, 0, 0, 0, 0, 0, 0),
  frugality = c(1, 0, 0, 0, 0, 0, 0, 0, 0),
  
  legal = c(0, 1, 0, 0, 0, 0, 0, 0, 0),
  trust = c(0, 1, 0, 0, 0, 0, 0, 0, 0),
  safety = c(0, 1, 0, 0, 0, 0, 0, 0, 0),
  climate_protection = c(0, 1, 0, 0, 0, 0, 0, 0, 0),
  
  cost_efficiency = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
  knowledge = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
  own_competence = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
  technical_fit = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
  environmental_concerns = c(0, 0, 1, 0, 0, 0, 0, 0, 0),
  
  self_satisfaction = c(0, 0, 0, 1, 0, 0, 0, 0, 0),
  commitment = c(0, 0, 0, 1, 0, 0, 0, 0, 0),
  adherence = c(0, 0, 0, 1, 0, 0, 0, 0, 0),
  autonomy = c(0, 0, 0, 1, 0, 0, 0, 0, 0),
  
  wellbeing = c(0, 0, 0, 0, 1, 0, 0, 0, 0),
  coziness = c(0, 0, 0, 0, 1, 0, 0, 0, 0),
  
  rights_and_duties = c(0, 0, 0, 0, 0, 1, 0, 0, 0),
  peer_pressure = c(0, 0, 0, 0, 0, 1, 0, 0, 0),
  support = c(0, 0, 0, 0, 0, 1, 0, 0, 0),
  socialising = c(0, 0, 0, 0, 0, 1, 0, 0, 0),
  agreement = c(0, 0, 0, 0, 0, 1, 0, 0, 0),
  
  novelty = c(0, 0, 0, 0, 0, 0, 1, 0, 0),
  fun = c(0, 0, 0, 0, 0, 0, 1, 0, 0),
  recognition = c(0, 0, 0, 0, 0, 0, 1, 0, 0),
  
  trends = c(0, 0, 0, 0, 0, 0, 0, 1, 0),
  authority = c(0, 0, 0, 0, 0, 0, 0, 1, 0),
  
  own_significance = c(0, 0, 0, 0, 0, 0, 0, 0, 1),
  approval = c(0, 0, 0, 0, 0, 0, 0, 0, 1)
)

write_csv(
  dimension_determinant_mapping,
  out_file(csv_dir, "dimension_determinant_mapping.csv")
)


# 6. HELPER FUNCTIONS


clean_determinant_score <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  
  case_when(
    is.na(x) ~ NA_real_,
    x >= 0 & x <= 100 ~ x,
    TRUE ~ NA_real_
  )
}

clean_selected_technology <- function(x) {
  x <- str_squish(as.character(x))
  x <- str_replace_all(x, "\u039D", "N")
  x <- str_replace_all(x, "\u00A0", " ")
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    str_detect(x, regex("^none\\.?$|none option|prefer not to answer", ignore_case = TRUE)) ~ NA_character_,
    str_detect(x, regex("balcony|kit", ignore_case = TRUE)) ~ "Balcony solar kits",
    str_detect(x, regex("tariff|electricity tariff|time-of-use", ignore_case = TRUE)) ~ "Electricity tariff change",
    str_detect(x, regex("cooling", ignore_case = TRUE)) ~ "Cooling system",
    str_detect(x, regex("hot water|domestic hot water|boiler|water heater", ignore_case = TRUE)) ~ "Domestic hot water system",
    str_detect(x, regex("electric vehicle", ignore_case = TRUE)) ~ "Electric vehicle",
    str_detect(x, regex("elevator|lift", ignore_case = TRUE)) ~ "Elevator",
    str_detect(x, regex("appliance", ignore_case = TRUE)) ~ "Energy-efficient appliances",
    str_detect(x, regex("storage", ignore_case = TRUE)) ~ "Energy storage systems",
    str_detect(x, regex("envelope|insulation|windows|roof|wall", ignore_case = TRUE)) ~ "Envelope renovation",
    str_detect(x, regex("fossil|biomass", ignore_case = TRUE)) ~ "Fossil fuel or biomass heating",
    str_detect(x, regex("heat pump", ignore_case = TRUE)) ~ "Heat pump",
    str_detect(x, regex("ventilation|heat recovery", ignore_case = TRUE)) ~ "Heat recovery ventilation",
    str_detect(x, regex("energy community", ignore_case = TRUE)) ~ "Energy community",
    str_detect(x, regex("micro", ignore_case = TRUE)) ~ "Micro-efficiency measures",
    str_detect(x, regex("photovoltaic|pv|solar pv|rooftop", ignore_case = TRUE)) ~ "Rooftop photovoltaic system",
    str_detect(x, regex("smart home", ignore_case = TRUE)) ~ "Smart home systems",
    TRUE ~ x
  )
}

dimension_levels <- c(
  "FINANCIAL",
  "SECURITY",
  "COMPETENCE",
  "AUTONOMY",
  "PHYSIOLOGICAL",
  "RELATEDNESS",
  "STIMULATION",
  "POPULARITY",
  "MEANING"
)

dimension_dictionary <- tibble(
  dimension_key = dimension_levels,
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

extract_dimensions <- function(x) {
  x <- str_squish(as.character(x))
  
  if (is.na(x) || x == "") {
    return(NA_character_)
  }
  
  detected <- dimension_levels[
    str_detect(x, fixed(dimension_levels, ignore_case = TRUE))
  ]
  
  if (length(detected) == 0) {
    return(NA_character_)
  }
  
  detected
}


# 7. DETERMINANT MATRIX BY PARTICIPANT


determinants_wide <- df %>%
  select(participant_id, all_of(determinant_cols)) %>%
  pivot_longer(
    cols = all_of(determinant_cols),
    names_to = "determinant_col",
    values_to = "response_raw"
  ) %>%
  mutate(
    response_numeric = clean_determinant_score(response_raw)
  ) %>%
  left_join(
    determinant_dictionary,
    by = "determinant_col"
  ) %>%
  select(
    participant_id,
    determinant_id,
    response_numeric
  ) %>%
  pivot_wider(
    names_from = determinant_id,
    values_from = response_numeric
  )

write_csv(
  determinants_wide,
  out_file(csv_dir, "ttm_determinants_wide.csv")
)


# 8. LOCATE STAGE / DIMENSION COLUMNS


find_unique_col <- function(pattern, label, exclude_pattern = NULL) {
  
  matches <- names(df)[
    str_detect(
      names(df),
      regex(pattern, ignore_case = TRUE)
    )
  ]
  
  if (!is.null(exclude_pattern)) {
    matches <- matches[
      !str_detect(
        matches,
        regex(exclude_pattern, ignore_case = TRUE)
      )
    ]
  }
  
  if (length(matches) == 0) {
    stop(
      paste0(
        "No column found for: ", label,
        "\nPattern used: ", pattern
      )
    )
  }
  
  if (length(matches) > 1) {
    stop(
      paste0(
        "More than one possible column found for: ", label,
        "\nColumns found:\n",
        paste(matches, collapse = "\n")
      )
    )
  }
  
  matches
}

implemented_technology_col <- find_unique_col(
  pattern = "^from_the_following_list_please_select_the_technology_or_energy_related_measure_you_have_implemented_at_home",
  label = "implemented technology"
)

implemented_dimensions_col <- find_unique_col(
  pattern = "^what_were_the_reasons_that_led_you_to_implement_or_contract_the_selected_technology_or_energy_related_measure",
  label = "implemented dimensions"
)

interested_technology_col <- find_unique_col(
  pattern = "^which_of_the_following_technologies_or_energy_related_measures_are_you_most_interested_in_implementing_in_your_home",
  label = "interested technology"
)

interested_dimensions_col <- find_unique_col(
  pattern = "^what_would_make_you_more_likely_to_implement_or_contract_this_technology_or_measure_please_select_the_3_most_important_for_you$",
  label = "interested dimensions"
)

curious_technology_col <- find_unique_col(
  pattern = "^is_there_a_technology_or_energy_related_measures_you_don_t_know_much_about_but_that_sparks_your_curiosity",
  label = "curiosity technology"
)

curious_dimensions_col <- find_unique_col(
  pattern = "^what_would_make_you_more_likely_to_implement_or_contract_this_technology_or_measure_please_select_the_3_most_important_for_you_1$",
  label = "curiosity dimensions"
)

never_technology_col <- find_unique_col(
  pattern = "^is_there_any_technology_or_energy_realted_measure_on_this_list_that_you_would_never_install_in_your_home",
  label = "never-use technology"
)

cat("Implemented technology:", implemented_technology_col, "\n")
cat("Implemented dimensions:", implemented_dimensions_col, "\n")
cat("Interested technology:", interested_technology_col, "\n")
cat("Interested dimensions:", interested_dimensions_col, "\n")
cat("Curiosity technology:", curious_technology_col, "\n")
cat("Curiosity dimensions:", curious_dimensions_col, "\n")
cat("Never-use technology:", never_technology_col, "\n")


# 9. CREATE STAGE - TECHNOLOGY - DIMENSION TABLE


implemented_stage <- df %>%
  transmute(
    participant_id,
    stage = "Implemented",
    technology_raw = .data[[implemented_technology_col]],
    dimensions_raw = .data[[implemented_dimensions_col]]
  )

interested_stage <- df %>%
  transmute(
    participant_id,
    stage = "Aware / would consider",
    technology_raw = .data[[interested_technology_col]],
    dimensions_raw = .data[[interested_dimensions_col]]
  )

curious_stage <- df %>%
  transmute(
    participant_id,
    stage = "Unaware but curious",
    technology_raw = .data[[curious_technology_col]],
    dimensions_raw = .data[[curious_dimensions_col]]
  )

never_stage <- df %>%
  transmute(
    participant_id,
    stage = "Would never use it",
    technology_raw = .data[[never_technology_col]],
    dimensions_raw = NA_character_
  )

ttm_stage_raw <- bind_rows(
  implemented_stage,
  interested_stage,
  curious_stage,
  never_stage
) %>%
  mutate(
    technology = clean_selected_technology(technology_raw),
    dimensions_list = lapply(dimensions_raw, extract_dimensions)
  ) %>%
  filter(
    !is.na(technology),
    technology != ""
  )

write_csv(
  ttm_stage_raw %>% select(-dimensions_list),
  out_file(csv_dir, "ttm_stage_technology_raw.csv")
)


# 10. EXPAND SELECTED DIMENSIONS

ttm_stage_dimension_long <- ttm_stage_raw %>%
  unnest_longer(
    dimensions_list,
    values_to = "dimension_key",
    keep_empty = TRUE
  ) %>%
  mutate(
    dimension_key = str_to_upper(as.character(dimension_key))
  ) %>%
  left_join(
    dimension_dictionary,
    by = "dimension_key"
  ) %>%
  mutate(
    dimension = dimension_label
  ) %>%
  select(
    participant_id,
    stage,
    technology,
    technology_raw,
    dimension_key,
    dimension,
    dimensions_raw
  )

write_csv(
  ttm_stage_dimension_long,
  out_file(csv_dir, "ttm_stage_dimension_long.csv")
)

print(ttm_stage_dimension_long, n = 100)


# 11. SUMMARY OF TECHNOLOGIES BY STAGE


summary_technology_by_stage <- ttm_stage_dimension_long %>%
  distinct(participant_id, stage, technology) %>%
  count(
    stage,
    technology,
    sort = TRUE,
    name = "n_participants"
  ) %>%
  group_by(stage) %>%
  mutate(
    percentage = n_participants / sum(n_participants) * 100
  ) %>%
  ungroup()

write_csv(
  summary_technology_by_stage,
  out_file(csv_dir, "summary_technology_by_stage.csv")
)

print(summary_technology_by_stage, n = Inf)


# 12. SUMMARY OF DIMENSIONS BY STAGE

summary_dimension_by_stage <- ttm_stage_dimension_long %>%
  filter(!is.na(dimension)) %>%
  count(
    stage,
    dimension,
    sort = TRUE,
    name = "n_mentions"
  ) %>%
  group_by(stage) %>%
  mutate(
    percentage = n_mentions / sum(n_mentions) * 100
  ) %>%
  ungroup()

write_csv(
  summary_dimension_by_stage,
  out_file(csv_dir, "summary_dimension_by_stage.csv")
)

print(summary_dimension_by_stage, n = Inf)


# 13. PREPARE DIMENSION -> DETERMINANTS MAPPING
dimension_determinant_mapping_fixed <- dimension_determinant_mapping %>%
  rename(dimension_key = dimension) %>%
  mutate(
    dimension_key = str_to_upper(dimension_key)
  )

write_csv(
  dimension_determinant_mapping_fixed,
  out_file(csv_dir, "dimension_determinant_mapping.csv")
)

mapping_long <- dimension_determinant_mapping_fixed %>%
  pivot_longer(
    cols = all_of(determinant_ids),
    names_to = "determinant_id",
    values_to = "is_linked"
  ) %>%
  mutate(
    is_linked = as.numeric(is_linked)
  )

write_csv(
  mapping_long,
  out_file(csv_dir, "dimension_determinant_mapping_long.csv")
)

print(
  mapping_long %>%
    filter(is_linked == 1) %>%
    count(dimension_key),
  n = Inf
)


# 14. BUILD 32-DETERMINANT VECTOR BY STAGE / DIMENSION
determinants_long_scores <- determinants_wide %>%
  pivot_longer(
    cols = all_of(determinant_ids),
    names_to = "determinant_id",
    values_to = "determinant_score"
  )

ttm_stage_determinant_vector_long <- ttm_stage_dimension_long %>%
  filter(!is.na(dimension_key)) %>%
  mutate(
    stage_dimension_id = row_number()
  ) %>%
  crossing(
    determinant_id = determinant_ids
  ) %>%
  left_join(
    mapping_long,
    by = c("dimension_key", "determinant_id")
  ) %>%
  left_join(
    determinants_long_scores,
    by = c("participant_id", "determinant_id")
  ) %>%
  mutate(
    is_linked = replace_na(is_linked, 0),
    determinant_score_stage = if_else(
      is_linked == 1,
      determinant_score,
      NA_real_
    )
  ) %>%
  left_join(
    determinant_dictionary %>%
      select(determinant_id, determinant_label),
    by = "determinant_id"
  )

write_csv(
  ttm_stage_determinant_vector_long,
  out_file(csv_dir, "ttm_stage_determinant_vector_long.csv")
)

check_vector_filling <- ttm_stage_determinant_vector_long %>%
  group_by(
    stage_dimension_id,
    participant_id,
    stage,
    technology,
    dimension_key,
    dimension
  ) %>%
  summarise(
    n_determinants_linked = sum(is_linked == 1, na.rm = TRUE),
    n_determinants_filled = sum(is_linked == 1 & !is.na(determinant_score_stage)),
    n_determinants_missing = n_determinants_linked - n_determinants_filled,
    .groups = "drop"
  )

write_csv(
  check_vector_filling,
  out_file(csv_dir, "check_vector_filling.csv")
)

print(
  check_vector_filling %>%
    count(dimension, n_determinants_linked, n_determinants_filled),
  n = Inf
)

check_vector_filling %>%
  filter(n_determinants_filled == 0) %>%
  arrange(dimension, stage, participant_id) %>%
  print(n = Inf)

write_csv(
  check_vector_filling %>%
    filter(n_determinants_filled == 0),
  out_file(csv_dir, "check_vector_filling_empty_rows.csv")
)

ttm_stage_determinant_vector_wide <- ttm_stage_determinant_vector_long %>%
  select(
    stage_dimension_id,
    participant_id,
    stage,
    technology,
    dimension_key,
    dimension,
    determinant_id,
    determinant_score_stage
  ) %>%
  pivot_wider(
    names_from = determinant_id,
    values_from = determinant_score_stage
  )

write_csv(
  ttm_stage_determinant_vector_wide,
  out_file(csv_dir, "ttm_stage_determinant_vector_wide.csv")
)

ttm_stage_determinant_vector_wide_valid <- ttm_stage_determinant_vector_wide %>%
  left_join(
    check_vector_filling %>%
      select(stage_dimension_id, n_determinants_filled),
    by = "stage_dimension_id"
  ) %>%
  filter(n_determinants_filled > 0)

write_csv(
  ttm_stage_determinant_vector_wide_valid,
  out_file(csv_dir, "ttm_stage_determinant_vector_wide_valid.csv")
)


# 15. SUMMARY OF DETERMINANTS BY STAG

summary_determinants_by_stage <- ttm_stage_determinant_vector_long %>%
  filter(
    is_linked == 1,
    !is.na(determinant_score_stage)
  ) %>%
  group_by(
    stage,
    determinant_id,
    determinant_label
  ) %>%
  summarise(
    n_mentions = n(),
    mean_score = mean(determinant_score_stage, na.rm = TRUE),
    median_score = median(determinant_score_stage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    stage,
    desc(n_mentions),
    desc(mean_score)
  )

write_csv(
  summary_determinants_by_stage,
  out_file(csv_dir, "summary_determinants_by_stage.csv")
)

print(summary_determinants_by_stage, n = Inf)


# 16. FINAL STAGE / INTERVENTION / DIMENSION / VECTOR PLOT

stage_levels <- c(
  "Implemented",
  "Aware / would consider",
  "Unaware but curious"
)

top_technologies_by_stage <- ttm_stage_dimension_long %>%
  filter(
    !is.na(dimension)
  ) %>%
  distinct(participant_id, stage, technology) %>%
  count(stage, technology, sort = TRUE) %>%
  group_by(stage) %>%
  slice_max(n, n = 4, with_ties = FALSE) %>%
  ungroup()

vector_final_plot_data <- ttm_stage_determinant_vector_long %>%
  mutate(
    stage = factor(stage, levels = stage_levels)
  ) %>%
  filter(
    is_linked == 1,
    !is.na(determinant_score_stage)
  ) %>%
  semi_join(
    top_technologies_by_stage,
    by = c("stage", "technology")
  ) %>%
  group_by(
    stage,
    technology,
    dimension,
    determinant_label
  ) %>%
  summarise(
    mean_score = mean(determinant_score_stage, na.rm = TRUE),
    n_values = n(),
    .groups = "drop"
  ) %>%
  mutate(
    determinant_label = factor(
      determinant_label,
      levels = rev(determinant_dictionary$determinant_label)
    )
  )

plot_final_stage_technology_dimension_vector <- ggplot(
  vector_final_plot_data,
  aes(
    x = dimension,
    y = determinant_label,
    fill = mean_score
  )
) +
  geom_tile(color = "black", linewidth = 0.2) +
  geom_text(
    aes(label = round(mean_score, 0)),
    size = plot_heatmap_label_size
  ) +
  facet_grid(
    rows = vars(stage),
    cols = vars(technology),
    scales = "free_x",
    space = "free_x"
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#0072B2",
    limits = c(0, 100)
  ) +
  labs(
    title = "Mean determinant vector by stage, technology and dimension",
    subtitle = "Only the most frequent technologies within each stage are shown. Mean values on a 0-100 scale",
    x = "Selected dimension",
    y = "Determinant",
    fill = "Mean"
  ) +
  theme_ttm_heatmap

print(plot_final_stage_technology_dimension_vector)

save_plot_png(
  plot_final_stage_technology_dimension_vector,
  "final_stage_technology_dimension_vector_heatmap",
  width = 18,
  height = 14
)


# 17. TECHNOLOGIES BY STAGE


plot_technology_by_stage <- summary_technology_by_stage %>%
  group_by(stage) %>%
  slice_max(n_participants, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    stage = factor(stage, levels = c(stage_levels, "Would never use it")),
    technology = str_wrap(technology, width = 28),
    technology = reorder(technology, n_participants)
  ) %>%
  ggplot(
    aes(
      x = technology,
      y = n_participants,
      fill = stage
    )
  ) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = n_participants),
    hjust = -0.1,
    size = plot_label_size
  ) +
  coord_flip(clip = "off") +
  facet_wrap(~ stage, scales = "free_y") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Most frequent technologies by TTM stage",
    subtitle = "Top 10 technologies/interventions within each stage",
    x = NULL,
    y = "Number of participants"
  ) +
  theme_ttm

print(plot_technology_by_stage)

save_plot_png(
  plot_technology_by_stage,
  "ttm_top_technologies_by_stage",
  width = 14,
  height = 9
)


# 18. DIMENSIONS BY STAGE


plot_dimensions_by_stage_percentage <- summary_dimension_by_stage %>%
  mutate(
    stage = factor(stage, levels = stage_levels),
    dimension = factor(dimension, levels = dimension_dictionary$dimension_label)
  ) %>%
  ggplot(
    aes(
      x = stage,
      y = percentage,
      fill = dimension
    )
  ) +
  geom_col(color = "black", linewidth = 0.2) +
  geom_text(
    aes(
      label = if_else(
        percentage >= 5,
        paste0(round(percentage, 0), "%"),
        ""
      )
    ),
    position = position_stack(vjust = 0.5),
    size = plot_label_size
  ) +
  scale_x_discrete(limits = rev(stage_levels)) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +
  coord_flip() +
  labs(
    title = "Distribution of dimensions by TTM stage",
    subtitle = "Percentages calculated within each stage",
    x = NULL,
    y = "Percentage",
    fill = "Dimension"
  ) +
  theme_ttm

print(plot_dimensions_by_stage_percentage)

save_plot_png(
  plot_dimensions_by_stage_percentage,
  "ttm_dimensions_by_stage_percentage",
  width = 12,
  height = 7
)


# 19. STAGE X DIMENSION HEATMAP


plot_heatmap_dimensions_by_stage <- summary_dimension_by_stage %>%
  mutate(
    stage = factor(stage, levels = stage_levels),
    dimension = factor(dimension, levels = dimension_dictionary$dimension_label)
  ) %>%
  ggplot(
    aes(
      x = dimension,
      y = stage,
      fill = percentage
    )
  ) +
  geom_tile(color = "black", linewidth = 0.25) +
  geom_text(
    aes(
      label = paste0(
        round(percentage, 0),
        "%\n(n=",
        n_mentions,
        ")"
      )
    ),
    size = plot_heatmap_label_size,
    lineheight = 0.9
  ) +
  scale_y_discrete(limits = rev(stage_levels)) +
  scale_fill_gradient(
    low = "white",
    high = "#0072B2"
  ) +
  labs(
    title = "Selected dimensions by TTM stage",
    subtitle = "Percentage and number of mentions within each stage",
    x = "Dimension",
    y = NULL,
    fill = "Percentage"
  ) +
  theme_ttm_heatmap

print(plot_heatmap_dimensions_by_stage)

save_plot_png(
  plot_heatmap_dimensions_by_stage,
  "ttm_heatmap_dimensions_by_stage",
  width = 13,
  height = 7
)


# 20. TOP DETERMINANTS BY STAGE


plot_top_determinants_by_stage <- summary_determinants_by_stage %>%
  group_by(stage) %>%
  slice_max(n_mentions, n = 8, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    determinant_label = str_wrap(determinant_label, width = 24),
    determinant_label = reorder(determinant_label, n_mentions)
  ) %>%
  ggplot(
    aes(
      x = determinant_label,
      y = n_mentions,
      fill = stage
    )
  ) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = n_mentions),
    hjust = -0.1,
    size = plot_label_size
  ) +
  coord_flip(clip = "off") +
  facet_wrap(~ stage, scales = "free_y") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Most frequent determinants by TTM stage",
    subtitle = "Top 8 determinants associated with the selected dimensions",
    x = NULL,
    y = "Number of mentions"
  ) +
  theme_ttm

print(plot_top_determinants_by_stage)

save_plot_png(
  plot_top_determinants_by_stage,
  "ttm_top_determinants_by_stage",
  width = 14,
  height = 9
)


# 21. HEATMAP OF MEAN DETERMINANT SCORES BY STAGE


plot_heatmap_determinants_mean_score <- summary_determinants_by_stage %>%
  group_by(determinant_label) %>%
  mutate(
    total_mentions = sum(n_mentions)
  ) %>%
  ungroup() %>%
  filter(total_mentions >= 5) %>%
  mutate(
    determinant_label = factor(
      determinant_label,
      levels = rev(unique(determinant_label[order(total_mentions)]))
    )
  ) %>%
  ggplot(
    aes(
      x = stage,
      y = determinant_label,
      fill = mean_score
    )
  ) +
  geom_tile(color = "black", linewidth = 0.2) +
  geom_text(
    aes(label = round(mean_score, 1)),
    size = plot_heatmap_label_size
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#0072B2",
    limits = c(0, 100)
  ) +
  labs(
    title = "Mean determinant value by TTM stage",
    subtitle = "Only determinants with at least 5 total mentions",
    x = NULL,
    y = NULL,
    fill = "Mean"
  ) +
  theme_ttm_heatmap

print(plot_heatmap_determinants_mean_score)

save_plot_png(
  plot_heatmap_determinants_mean_score,
  "ttm_heatmap_determinants_mean_score_by_stage",
  width = 13,
  height = 11
)


# 22. SAVE ALL PLOTS IN A SINGLE PDF

save_plots_pdf <- function(plot_list, filename, width = 14, height = 9) {
  
  pdf(
    file = out_file(pdf_dir, filename),
    width = width,
    height = height,
    onefile = TRUE
  )
  
  for (p in plot_list) {
    print(p)
  }
  
  dev.off()
}

all_ttm_plots <- list(
  plot_technology_by_stage,
  plot_dimensions_by_stage_percentage,
  plot_heatmap_dimensions_by_stage,
  plot_top_determinants_by_stage,
  plot_heatmap_determinants_mean_score,
  plot_final_stage_technology_dimension_vector
)

save_plots_pdf(
  plot_list = all_ttm_plots,
  filename = "ttm_stage_dimension_determinants_all_plots.pdf",
  width = 14,
  height = 9
)

cat("Results saved in:", base_output_dir, "\n")

cat("\nMain English output files:\n")
cat("- en_determinant_dictionary.csv\n")
cat("- en_dimension_determinant_mapping.csv\n")
cat("- en_ttm_determinants_wide.csv\n")
cat("- en_ttm_stage_technology_raw.csv\n")
cat("- en_ttm_stage_dimension_long.csv\n")
cat("- en_summary_technology_by_stage.csv\n")
cat("- en_summary_dimension_by_stage.csv\n")
cat("- en_dimension_determinant_mapping_long.csv\n")
cat("- en_ttm_stage_determinant_vector_long.csv\n")
cat("- en_check_vector_filling.csv\n")
cat("- en_ttm_stage_determinant_vector_wide.csv\n")
cat("- en_ttm_stage_determinant_vector_wide_valid.csv\n")
cat("- en_summary_determinants_by_stage.csv\n")
cat("- en_ttm_stage_dimension_determinants_all_plots.pdf\n")