# SCRIPT 09 - SELF-CLASSIFICATION 4.3 AND DETERMINANT PROFILE - EN

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tibble)
library(purrr)
library(gridExtra)
library(grid)

# 1. LOAD CLEAN DATA
df <- read_csv(
  "initial_descriptive_analysis/output/clean_datasets/df_clean_general.csv",
  show_col_types = FALSE
)

glimpse(df)


# 2. ENSURE PARTICIPANT_ID
if (!"participant_id" %in% names(df)) {
  df <- df %>%
    mutate(
      participant_id = coalesce(
        if ("join_key" %in% names(.)) as.character(join_key) else NA_character_,
        if ("prolific_id" %in% names(.)) as.character(prolific_id) else NA_character_,
        if ("identification_code" %in% names(.)) as.character(identification_code) else NA_character_,
        as.character(row_number())
      )
    )
} else {
  df <- df %>%
    mutate(participant_id = as.character(participant_id))
}

cat("Rows used in Script 09:", nrow(df), "\n")

# 3. ROBUST DETERMINANT DICTIONARY IN ENGLISH
determinant_dictionary <- tribble(
  ~determinant_id, ~determinant_label, ~determinant_prefix,
  "profits", "Economic profit", "profits",
  "credit_score_access_to_funding", "Access to funding", "credit_score_access_to_funding",
  "risk_profile", "Risk profile", "risk_profile",
  "added_value", "Added value", "added_value",
  "frugality", "Frugality", "frugality",
  "climate_protection", "Climate protection", "climate_protection",
  "legal", "Legal compliance", "legal",
  "trust", "Trust", "trust",
  "safety", "Safety", "safety",
  "cost_efficiency", "Cost efficiency", "cost_efficiency",
  "knowledge", "Knowledge", "knowledge",
  "own_competence", "Own competence", "own_competence",
  "technical_fit", "Technical fit", "technical_fit",
  "environmental_concerns", "Environmental concerns", "environmental_concerns",
  "self_satisfaction", "Self-satisfaction", "self_satisfaction",
  "commitment", "Commitment", "commitment",
  "adherence", "Adherence", "adherence",
  "autarky", "Self-sufficiency", "autonomy",
  "wellbeing", "Wellbeing", "wellbeing",
  "coziness", "Comfort", "coziness",
  "rights_and_duties", "Rights and duties", "rights_and_duties",
  "peer_pressure", "Peer pressure", "peer_pressure",
  "support", "Social support", "support",
  "socialising", "Socialising", "socialising",
  "agreement", "Agreement", "agreement",
  "novelty", "Novelty", "novelty",
  "fun", "Fun", "fun",
  "recognition", "Recognition", "recognition",
  "trends", "Trends", "trends",
  "authority", "Authority", "authority",
  "approval", "Approval", "approval",
  "own_significance", "Personal significance", "own_significance"
) %>%
  mutate(
    determinant_col = map_chr(
      determinant_prefix,
      ~ {
        hits <- names(df)[str_detect(names(df), paste0("^", .x, "_"))]
        if (length(hits) == 0) NA_character_ else hits[1]
      }
    )
  )

missing_determinants <- determinant_dictionary %>%
  filter(is.na(determinant_col))

if (nrow(missing_determinants) > 0) {
  stop(
    paste0(
      "Missing determinant columns in the dataset: ",
      paste(missing_determinants$determinant_id, collapse = ", ")
    )
  )
}

determinant_cols <- determinant_dictionary$determinant_col
determinant_label_levels <- determinant_dictionary$determinant_label

cat("Number of determinants detected:", length(determinant_cols), "\n")
print(determinant_dictionary %>% select(determinant_id, determinant_label, determinant_col))

# 4. DETECT SELF-CLASSIFICATION COLUMN 4.3
self_col_candidates <- c(
  "which_statement_best_describes_you_when_making_an_investment_decision_related_to_your_household_final",
  "which_statement_best_describes_you_when_making_an_investment_decision_related_to_your_household"
)

self_col <- intersect(self_col_candidates, names(df))[1]

if (is.na(self_col)) {
  stop("Self-classification column 4.3 not found in the dataset.")
}

cat("Column used for self-classification:", self_col, "\n")


# 5. OUTPUT FOLDERS

base_output_dir <- "initial_descriptive_analysis/output/self_classification"

csv_dir <- file.path(base_output_dir, "csv")
plots_dir <- file.path(base_output_dir, "plots")
pdf_dir <- file.path(base_output_dir, "pdf")

dir.create(csv_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(pdf_dir, showWarnings = FALSE, recursive = TRUE)

# Prefix for all English output files
output_prefix <- "en_"

out_file <- function(directory, filename) {
  file.path(directory, paste0(output_prefix, filename))
}

excluded_plot_profiles <- c("Missing", "Unclassified", "Other")


# 6. VISUAL CONFIGURATION
box_fill <- "#BDE3FF"
box_color <- "#2C3E50"
outlier_color <- "#4F4F4F"
bar_fill <- "#BDE3FF"
bar_color <- "#2C3E50"

plot_base_size <- 15
plot_title_size <- 18
plot_subtitle_size <- 13
plot_axis_title_size <- 15
plot_axis_text_size <- 15
plot_strip_text_size <- 13
plot_legend_title_size <- 14
plot_legend_text_size <- 13

plot_label_size <- 4.5
plot_heatmap_label_size <- 3.8

theme_self <- theme_minimal(base_size = plot_base_size) +
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

theme_self_heatmap <- theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(face = "bold", size = plot_title_size),
    plot.subtitle = element_text(size = plot_subtitle_size),
    axis.title.x = element_text(size = plot_axis_title_size, margin = margin(t = 8)),
    axis.title.y = element_text(size = plot_axis_title_size, margin = margin(r = 8)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = plot_axis_text_size),
    axis.text.y = element_text(size = plot_axis_text_size),
    legend.title = element_text(face = "bold", size = plot_legend_title_size),
    legend.text = element_text(size = plot_legend_text_size),
    legend.position = "right",
    panel.grid = element_blank(),
    plot.margin = margin(12, 20, 12, 12)
  )


# 7. HELPER FUNCTIONS
make_subtitle_self <- function(data) {
  paste0(
    "n participants = ", n_distinct(data$participant_id),
    "; valid participant-determinant observations = ", nrow(data)
  )
}

save_plot <- function(plot, filename, width = 12, height = 7) {
  ggsave(
    filename = out_file(plots_dir, paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
  
  ggsave(
    filename = out_file(pdf_dir, paste0(filename, ".pdf")),
    plot = plot,
    width = width,
    height = height
  )
}

clean_determinant_score <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  
  case_when(
    is.na(x) ~ NA_real_,
    x >= 0 & x <= 100 ~ x,
    TRUE ~ NA_real_
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

# 8. RECODE SELF-CLASSIFICATION 4.3 IN ENGLISH

df_self <- df %>%
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
      str_detect(self_response_raw, regex("not very interested", ignore_case = TRUE)) ~ "Disinterested",
      str_detect(self_response_raw, regex("early adopter", ignore_case = TRUE)) ~ "Early adopter",
      str_detect(self_response_raw, regex("[NΝ]one of the above", ignore_case = TRUE)) ~ "Unclassified",
      TRUE ~ "Other"
    )
  )

profile_order <- c(
  "Careful",
  "Activist",
  "Fearful",
  "Homo economicus",
  "Stubborn",
  "Influencer",
  "Disinterested",
  "Early adopter"
)

# ==============================================================================
# EXTRA: SELF-CLASSIFICATION DISTRIBUTION BY COUNTRY GROUP
# Spain vs Rest of sample
# ==============================================================================

# 1. Detect country column
country_col_candidates <- c(
  "country_clean",
  "in_which_country_do_you_currently_live_final"
)

country_col <- intersect(country_col_candidates, names(df_self))[1]

if (is.na(country_col)) {
  stop("No country column found. Expected one of: country_clean or in_which_country_do_you_currently_live_final")
}

cat("Column used for Spain / Rest grouping:", country_col, "\n")

# 2. Create Spain / Rest variable
df_self <- df_self %>%
  mutate(
    country_raw = as.character(.data[[country_col]]),
    country_raw = str_squish(str_trim(country_raw)),
    country_raw = str_remove(country_raw, "\\s*\\(ID[0-9]+\\)$"),
    country_raw = str_remove(country_raw, "^[A-Z]{2}\\s*[–-]\\s*"),
    country_raw = na_if(country_raw, ""),
    
    country_group = case_when(
      str_detect(country_raw, regex("^Spain$|^España$", ignore_case = TRUE)) ~ "Spain",
      !is.na(country_raw) ~ "Rest of sample",
      TRUE ~ NA_character_
    )
  )

# 3. Check number of participants by group
country_group_counts <- df_self %>%
  count(country_group, name = "n_participants") %>%
  arrange(country_group)

write_csv(
  country_group_counts,
  out_file(csv_dir, "country_group_counts_spain_rest.csv")
)

print(country_group_counts)

# 4. Function to create self-classification plot by country group
create_self_counts_country_group_plot <- function(data, selected_country_group, filename_suffix) {
  
  self_profile_counts_group <- data %>%
    filter(country_group == selected_country_group) %>%
    mutate(
      self_profile = factor(
        self_profile,
        levels = c(profile_order, "Missing", "Unclassified", "Other")
      )
    ) %>%
    count(self_profile, name = "n_participants") %>%
    arrange(self_profile)
  
  write_csv(
    self_profile_counts_group,
    out_file(csv_dir, paste0("self_classification_counts_", filename_suffix, ".csv"))
  )
  
  self_profile_counts_group_plot <- self_profile_counts_group %>%
    filter(!self_profile %in% excluded_plot_profiles)
  
  plot_self_counts_group <- self_profile_counts_group_plot %>%
    mutate(
      self_profile = factor(self_profile, levels = rev(profile_order))
    ) %>%
    ggplot(aes(x = self_profile, y = n_participants)) +
    geom_col(fill = bar_fill, color = bar_color) +
    geom_text(
      aes(label = n_participants),
      hjust = -0.2,
      size = plot_label_size
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.15))
    ) +
    labs(
      title = paste0("Distribution of self-classified profiles - ", selected_country_group),
      subtitle = paste0(
        "n classified participants = ",
        sum(self_profile_counts_group_plot$n_participants)
      ),
      x = "Self-classified profile",
      y = "Number of participants"
    ) +
    theme_self
  
  print(plot_self_counts_group)
  
  save_plot(
    plot_self_counts_group,
    paste0("self_classification_counts_", filename_suffix),
    width = 10,
    height = 7
  )
  
  return(plot_self_counts_group)
}

# 5. Generate Spain plot
plot_self_counts_spain <- create_self_counts_country_group_plot(
  data = df_self,
  selected_country_group = "Spain",
  filename_suffix = "spain"
)

# 6. Generate Rest of sample plot
plot_self_counts_rest <- create_self_counts_country_group_plot(
  data = df_self,
  selected_country_group = "Rest of sample",
  filename_suffix = "rest_of_sample"
)

# 9. LONG TABLE OF DETERMINANTS + SELF-CLASSIFICATION
determinants_self_long <- df_self %>%
  select(
    participant_id,
    self_response_raw,
    self_profile,
    all_of(determinant_cols)
  ) %>%
  mutate(
    across(all_of(determinant_cols), as.character)
  ) %>%
  pivot_longer(
    cols = all_of(determinant_cols),
    names_to = "determinant_col",
    values_to = "response_raw"
  ) %>%
  mutate(
    response_numeric = clean_determinant_score(response_raw)
  ) %>%
  left_join(
    determinant_dictionary %>%
      select(determinant_col, determinant_id, determinant_label),
    by = "determinant_col"
  ) %>%
  filter(!is.na(response_numeric)) %>%
  mutate(
    determinant_label = factor(
      determinant_label,
      levels = determinant_label_levels
    ),
    self_profile = factor(
      self_profile,
      levels = c(profile_order, "Missing", "Unclassified", "Other")
    )
  )

write_csv(
  determinants_self_long,
  out_file(csv_dir, "determinants_by_self_classification_long.csv")
)

determinants_self_long_plot <- determinants_self_long %>%
  filter(!self_profile %in% excluded_plot_profiles)


# 10. SELF-CLASSIFICATION DISTRIBUTION

self_profile_counts <- df_self %>%
  mutate(
    self_profile = factor(
      self_profile,
      levels = c(profile_order, "Missing", "Unclassified", "Other")
    )
  ) %>%
  count(self_profile, name = "n_participants") %>%
  arrange(self_profile)

write_csv(
  self_profile_counts,
  out_file(csv_dir, "self_classification_counts.csv")
)

self_profile_counts_plot <- self_profile_counts %>%
  filter(!self_profile %in% excluded_plot_profiles)

plot_self_counts <- self_profile_counts_plot %>%
  mutate(
    self_profile = factor(self_profile, levels = rev(profile_order))
  ) %>%
  ggplot(aes(x = self_profile, y = n_participants)) +
  geom_col(fill = bar_fill, color = bar_color) +
  geom_text(
    aes(label = n_participants),
    hjust = -0.2,
    size = plot_label_size
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Distribution of self-classified profiles",
    subtitle = paste0(
      "n classified participants = ",
      sum(self_profile_counts_plot$n_participants)
    ),
    x = "Self-classified profile",
    y = "Number of participants"
  ) +
  theme_self

print(plot_self_counts)

save_plot(
  plot_self_counts,
  "self_classification_counts",
  width = 10,
  height = 7
)

# 11. MEAN DETERMINANTS BY PROFILE
summary_self_determinants <- determinants_self_long %>%
  group_by(self_profile, determinant_id, determinant_label) %>%
  summarise(
    n = n(),
    mean = mean(response_numeric, na.rm = TRUE),
    median = median(response_numeric, na.rm = TRUE),
    sd = sd(response_numeric, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  summary_self_determinants,
  out_file(csv_dir, "summary_self_profile_determinants.csv")
)

summary_self_determinants_plot <- summary_self_determinants %>%
  filter(!self_profile %in% excluded_plot_profiles)


# 12. HEATMAP PROFILE X DETERMINANTS
plot_heatmap_self <- summary_self_determinants_plot %>%
  mutate(
    self_profile = factor(
      self_profile,
      levels = profile_order
    ),
    determinant_label = factor(
      determinant_label,
      levels = rev(determinant_label_levels)
    )
  ) %>%
  ggplot(aes(x = self_profile, y = determinant_label, fill = mean)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient(
    low = "white",
    high = "#4DADE8",
    limits = c(0, 100)
  ) +
  labs(
    title = "Mean determinant profile by self-classification",
    subtitle = make_subtitle_self(determinants_self_long_plot),
    x = "Self-classified profile",
    y = "Determinant",
    fill = "Mean"
  ) +
  theme_self_heatmap

print(plot_heatmap_self)

save_plot(
  plot_heatmap_self,
  "heatmap_self_classification_determinants",
  width = 14,
  height = 12
)


# 13. RANKED HEATMAP WITHIN EACH PROFILE
rank_colors <- c(
  "1st" = "#8B0000",
  "2nd" = "#D7191C",
  "3rd" = "#F46D43",
  "4th" = "#FDAE61",
  "5th" = "#FEE08B",
  "6th-10th" = "#D9EF8B",
  "11th-15th" = "#A6D96A",
  "Rest" = "#1A9641"
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

plot_heatmap_self_ranked <- summary_self_determinants_plot %>%
  group_by(self_profile) %>%
  arrange(desc(mean), desc(n), determinant_id, .by_group = TRUE) %>%
  mutate(
    rank_in_profile = row_number(),
    rank_group = case_when(
      rank_in_profile == 1 ~ "1st",
      rank_in_profile == 2 ~ "2nd",
      rank_in_profile == 3 ~ "3rd",
      rank_in_profile == 4 ~ "4th",
      rank_in_profile == 5 ~ "5th",
      rank_in_profile <= 10 ~ "6th-10th",
      rank_in_profile <= 15 ~ "11th-15th",
      TRUE ~ "Rest"
    )
  ) %>%
  ungroup() %>%
  mutate(
    self_profile = factor(
      self_profile,
      levels = profile_order
    ),
    determinant_label = factor(
      determinant_label,
      levels = rev(determinant_label_levels)
    ),
    rank_group = factor(rank_group, levels = rank_levels)
  ) %>%
  ggplot(aes(x = self_profile, y = determinant_label, fill = rank_group)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(
    aes(label = round(mean, 0)),
    size = plot_heatmap_label_size
  ) +
  scale_fill_manual(
    values = rank_colors,
    drop = FALSE,
    name = "Position within profile"
  ) +
  labs(
    title = "Ranking of determinants within each profile",
    subtitle = paste0(
      "Top positions shown individually; remaining determinants grouped by rank. ",
      make_subtitle_self(determinants_self_long_plot)
    ),
    x = "Self-classified profile",
    y = "Determinant"
  ) +
  theme_self_heatmap

print(plot_heatmap_self_ranked)

save_plot(
  plot_heatmap_self_ranked,
  "heatmap_self_classification_determinants_ranked",
  width = 14,
  height = 12
)


# 14. BOXPLOTS OF ALL DETERMINANTS BY SELF-CLASSIFICATION
plot_boxplots_self_all <- determinants_self_long_plot %>%
  mutate(
    determinant_label = factor(
      determinant_label,
      levels = determinant_label_levels
    ),
    self_profile = factor(
      self_profile,
      levels = profile_order
    )
  ) %>%
  ggplot(aes(x = self_profile, y = response_numeric)) +
  geom_boxplot(
    fill = box_fill,
    color = box_color,
    outlier.color = outlier_color,
    outlier.alpha = 0.35
  ) +
  facet_wrap(~ determinant_label, scales = "free_y") +
  labs(
    title = "Distribution of determinants by self-classification",
    subtitle = make_subtitle_self(determinants_self_long_plot),
    x = "Self-classified profile",
    y = "Value"
  ) +
  theme_self

print(plot_boxplots_self_all)

save_plot(
  plot_boxplots_self_all,
  "boxplots_all_determinants_by_self_classification",
  width = 22,
  height = 16
)


# 15. BOXPLOTS OF KEY DETERMINANTS
key_determinants <- c(
  "novelty",
  "trends",
  "risk_profile",
  "cost_efficiency",
  "knowledge",
  "technical_fit",
  "trust",
  "climate_protection",
  "environmental_concerns",
  "safety",
  "wellbeing",
  "recognition",
  "approval"
)

plot_key_determinants <- determinants_self_long_plot %>%
  filter(determinant_id %in% key_determinants) %>%
  mutate(
    determinant_label = factor(
      determinant_label,
      levels = determinant_label_levels
    ),
    self_profile = factor(
      self_profile,
      levels = profile_order
    )
  ) %>%
  ggplot(aes(x = self_profile, y = response_numeric)) +
  geom_boxplot(
    fill = box_fill,
    color = box_color,
    outlier.color = outlier_color,
    outlier.alpha = 0.4
  ) +
  facet_wrap(~ determinant_label, scales = "free_y") +
  labs(
    title = "Key determinants by self-classification",
    subtitle = make_subtitle_self(
      determinants_self_long_plot %>%
        filter(determinant_id %in% key_determinants)
    ),
    x = "Self-classified profile",
    y = "Value"
  ) +
  theme_self

print(plot_key_determinants)

save_plot(
  plot_key_determinants,
  "boxplots_key_determinants_by_self_classification",
  width = 18,
  height = 12
)


# 16. INDIVIDUAL BOXPLOTS BY SELF-CLASSIFIED PROFILE
profiles_boxplots <- determinants_self_long_plot %>%
  pull(self_profile) %>%
  as.character() %>%
  unique()

profiles_boxplots <- profile_order[profile_order %in% profiles_boxplots]

profile_boxplot_list <- list()

for (prof in profiles_boxplots) {
  
  top_dets_prof <- summary_self_determinants_plot %>%
    filter(as.character(self_profile) == prof) %>%
    slice_max(order_by = mean, n = 10, with_ties = FALSE) %>%
    pull(determinant_id)
  
  profile_data_box <- determinants_self_long_plot %>%
    filter(
      as.character(self_profile) == prof,
      determinant_id %in% top_dets_prof
    )
  
  filename_clean <- clean_filename(prof)
  
  plot_profile_box <- profile_data_box %>%
    mutate(
      determinant_label = reorder(
        determinant_label,
        response_numeric,
        median,
        na.rm = TRUE
      )
    ) %>%
    ggplot(aes(x = determinant_label, y = response_numeric)) +
    geom_boxplot(
      fill = box_fill,
      color = box_color,
      outlier.color = outlier_color,
      outlier.alpha = 0.4
    ) +
    coord_flip() +
    labs(
      title = paste("Determinant boxplot -", prof),
      subtitle = make_subtitle_self(profile_data_box),
      x = "Determinant",
      y = "Value"
    ) +
    theme_self +
    theme(
      axis.text.x = element_text(size = plot_axis_text_size),
      axis.text.y = element_text(size = plot_axis_text_size)
    )
  
  print(plot_profile_box)
  
  save_plot(
    plot_profile_box,
    paste0(filename_clean, "_determinants_boxplot"),
    width = 12,
    height = 10
  )
  
  profile_boxplot_list[[prof]] <- plot_profile_box
}


# 17. MEAN DETERMINANT PROFILES BY SELF-CLASSIFICATION
profiles <- summary_self_determinants_plot %>%
  pull(self_profile) %>%
  as.character() %>%
  unique()

profiles <- profile_order[profile_order %in% profiles]

profile_plots <- list()

for (prof in profiles) {
  
  profile_data <- summary_self_determinants_plot %>%
    filter(as.character(self_profile) == prof) %>%
    arrange(desc(mean))
  
  filename_clean <- clean_filename(prof)
  
  write_csv(
    profile_data,
    out_file(csv_dir, paste0(filename_clean, "_determinant_profile.csv"))
  )
  
  plot_profile <- profile_data %>%
    mutate(
      determinant_label = reorder(determinant_label, mean)
    ) %>%
    ggplot(aes(x = determinant_label, y = mean)) +
    geom_col(fill = bar_fill, color = bar_color) +
    coord_flip() +
    labs(
      title = paste("Mean determinant profile -", prof),
      subtitle = paste0(
        "n valid observations = ", sum(profile_data$n),
        "; n determinants = ", nrow(profile_data)
      ),
      x = "Determinant",
      y = "Mean"
    ) +
    theme_self +
    theme(
      axis.text.x = element_text(size = plot_axis_text_size),
      axis.text.y = element_text(size = plot_axis_text_size)
    )
  
  print(plot_profile)
  
  save_plot(
    plot_profile,
    paste0(filename_clean, "_determinant_profile"),
    width = 12,
    height = 10
  )
  
  profile_plots[[prof]] <- plot_profile
}


# 18. MULTIPAGE PDF WITH MEAN PROFILES

pdf(
  file = out_file(pdf_dir, "all_self_profiles_determinants.pdf"),
  width = 12,
  height = 10
)

for (p in profile_plots) {
  print(p)
}

dev.off()


# 19. MULTIPAGE PDF WITH PROFILE BOXPLOTS

pdf(
  file = out_file(pdf_dir, "all_self_profiles_boxplots.pdf"),
  width = 12,
  height = 10
)

for (p in profile_boxplot_list) {
  print(p)
}

dev.off()


# 20. 2x2 COMPARISON PLOTS

if (length(profile_plots) > 0) {
  comparison_profiles <- marrangeGrob(
    grobs = profile_plots,
    nrow = 2,
    ncol = 2,
    top = textGrob(
      "Mean determinant profiles",
      gp = gpar(fontsize = plot_title_size, fontface = "bold")
    )
  )
  
  ggsave(
    filename = out_file(plots_dir, "all_self_profiles_comparison.pdf"),
    plot = comparison_profiles,
    width = 18,
    height = 13
  )
}

if (length(profile_boxplot_list) > 0) {
  comparison_profile_boxplots <- marrangeGrob(
    grobs = profile_boxplot_list,
    nrow = 2,
    ncol = 2,
    top = textGrob(
      "Determinant boxplots by self-classified profile",
      gp = gpar(fontsize = plot_title_size, fontface = "bold")
    )
  )
  
  ggsave(
    filename = out_file(plots_dir, "all_self_profiles_boxplots_comparison.pdf"),
    plot = comparison_profile_boxplots,
    width = 18,
    height = 13
  )
}


# 21. FINAL PDF WITH ALL SCRIPT 09 FIGURES

pdf(
  file = out_file(pdf_dir, "self_classification_determinants_ALL.pdf"),
  width = 16,
  height = 10
)

print(plot_self_counts)
print(plot_heatmap_self)
print(plot_heatmap_self_ranked)
print(plot_boxplots_self_all)
print(plot_key_determinants)

for (p in profile_plots) {
  print(p)
}

for (p in profile_boxplot_list) {
  print(p)
}

dev.off()

cat("Self-classification analysis generated in:", plots_dir, "\n")
cat("Complete CSV files saved including Missing, Unclassified and Other.\n")
cat("Plots generated excluding Missing, Unclassified and Other.\n")

cat("\nMain English output files:\n")
cat("- en_determinants_by_self_classification_long.csv\n")
cat("- en_self_classification_counts.csv\n")
cat("- en_summary_self_profile_determinants.csv\n")
cat("- en_self_classification_counts.png\n")
cat("- en_heatmap_self_classification_determinants.pdf\n")
cat("- en_heatmap_self_classification_determinants_ranked.pdf\n")
cat("- en_boxplots_all_determinants_by_self_classification.pdf\n")
cat("- en_boxplots_key_determinants_by_self_classification.pdf\n")
cat("- en_all_self_profiles_determinants.pdf\n")
cat("- en_all_self_profiles_boxplots.pdf\n")
cat("- en_self_classification_determinants_ALL.pdf\n")