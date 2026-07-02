# SCRIPT 05 - EXPERIENCE AND TTM STAGES - EN

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tibble)

# ==============================================================================
# PATHS
# ==============================================================================

base_input_dir <- "initial_descriptive_analysis/output/clean_datasets"
base_output_dir <- "initial_descriptive_analysis/output/experience"

csv_dir <- file.path(base_output_dir, "csv")
plots_dir <- file.path(base_output_dir, "plots")
pdf_dir <- file.path(base_output_dir, "pdf")
logs_dir <- file.path(base_output_dir, "logs")
plots_technology_dir <- file.path(plots_dir, "by_technology")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_technology_dir, recursive = TRUE, showWarnings = FALSE)

# Prefix for all English output files
output_prefix <- "en_"

out_file <- function(directory, filename) {
  file.path(directory, paste0(output_prefix, filename))
}

# ==============================================================================
# LOAD DATA
# ==============================================================================

df <- read_csv(
  file.path(base_input_dir, "df_clean_general.csv"),
  show_col_types = FALSE
)

cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n")

# ==============================================================================
# INITIAL COLUMN INSPECTION
# ==============================================================================

cols_df <- tibble(
  index = seq_along(names(df)),
  column_name = names(df)
)

write_csv(
  cols_df,
  out_file(csv_dir, "column_names_df_clean_general.csv")
)

print(cols_df, n = Inf)

# ==============================================================================
# DEFINE RELEVANT COLUMNS
# ==============================================================================

df <- df %>%
  mutate(
    participant_id = coalesce(
      as.character(join_key),
      as.character(prolific_id),
      as.character(identification_code),
      as.character(row_number())
    )
  )

experience_tech_cols <- names(df)[
  str_detect(
    names(df),
    "^for_each_of_the_following_technologies_decisions_or_behaviours_.*_final$"
  )
]

renovation_age_cols <- names(df)[
  str_detect(
    names(df),
    "^please_indicate_when_the_technology_decision_was_implemented_or_contracted_.*_final$"
  )
]

year_of_birth_col <- "year_of_birth"
gender_col <- "what_is_your_gender_final"
country_col <- "in_which_country_do_you_currently_live_final"
size_city_col <- "what_is_the_approximate_population_size_of_the_city_where_you_live_final"
climate_zone_col <- "in_which_climate_zone_do_you_live_please_select_the_option_that_corresponds_to_your_location_if_you_re_not_sure_which_climate_zone_you_live_in_please_refer_to_the_map_below_or_select_the_option_that_best_matches_your_region_final"
employment_col <- "what_is_your_current_employment_status_please_indicate_your_main_contractual_status_if_you_are_temporarily_on_leave_such_as_sick_leave_parental_leave_or_a_temporary_reduction_in_working_hours_please_report_your_usual_employment_status_final"
education_level_col <- "what_is_the_highest_level_of_education_that_you_have_completed_if_you_are_currently_studying_and_have_not_yet_completed_a_level_please_select_the_last_level_you_have_finished_final"
work_home_col <- "do_you_currently_work_or_study_from_home_final"
type_house_col <- "what_type_of_household_do_you_live_in_please_select_the_option_that_best_describes_your_household_final"
tenure_col <- "what_is_the_current_tenure_status_of_your_home_final"
political_col <- "on_a_scale_from_0_to_100_where_0_means_most_left_and_100_means_most_right_where_would_you_place_yourself_politically_final"

cat("Experience technologies:", length(experience_tech_cols), "\n")
cat("Renovation age columns:", length(renovation_age_cols), "\n")

# ==============================================================================
# CHECK ORIGINAL EXPERIENCE OPTIONS
# ==============================================================================

experience_options_raw <- df %>%
  select(all_of(experience_tech_cols)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "technology_col",
    values_to = "experience_raw"
  ) %>%
  mutate(
    experience_raw = str_squish(as.character(experience_raw)),
    experience_raw = na_if(experience_raw, "")
  ) %>%
  filter(!is.na(experience_raw)) %>%
  count(experience_raw, sort = TRUE)

write_csv(
  experience_options_raw,
  out_file(csv_dir, "experience_options_raw.csv")
)

print(experience_options_raw, n = Inf)

# ==============================================================================
# RECODE EXPERIENCE OPTIONS
# ==============================================================================

experience_levels <- c(
  "Already present when I moved in",
  "I have installed / implemented / am currently doing this myself",
  "I have already actively looked for information about it",
  "I am unaware of this technology, decision, or behaviour",
  "I am aware of this and could consider it in the future",
  "I would never apply / use it"
)

recode_experience <- function(x) {
  x <- str_squish(as.character(x))
  
  case_when(
    x == "Already present when I moved in" ~
      "Already present when I moved in",
    
    x == "I have installed / implemented / am currently doing this myself" |
      x == "I installed / implemented this myself" |
      x == "Technologies implemented by me" ~
      "I have installed / implemented / am currently doing this myself",
    
    x == "I have already actively looked for information about it" ~
      "I have already actively looked for information about it",
    
    x == "I am unaware of this technology, decision, or behaviour" |
      x == "I don't know this technology" |
      x == "Technologies you are not aware" ~
      "I am unaware of this technology, decision, or behaviour",
    
    x == "I am aware of this and could consider it in the future" |
      x == "Technologies you are interested on" ~
      "I am aware of this and could consider it in the future",
    
    x == "I would never apply / use it" |
      x == "Technologies you will never do" ~
      "I would never apply / use it",
    
    TRUE ~ NA_character_
  )
}

# ==============================================================================
# CHECK CLEAN EXPERIENCE OPTIONS
# ==============================================================================

experience_options_clean <- df %>%
  select(all_of(experience_tech_cols)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "technology_col",
    values_to = "experience_raw"
  ) %>%
  mutate(
    experience_raw = str_squish(as.character(experience_raw)),
    experience_raw = na_if(experience_raw, ""),
    experience_clean = recode_experience(experience_raw)
  ) %>%
  filter(!is.na(experience_clean)) %>%
  count(experience_clean, sort = TRUE)

write_csv(
  experience_options_clean,
  out_file(csv_dir, "experience_options_clean.csv")
)

print(experience_options_clean, n = Inf)

# ==============================================================================
# CHECK UNCLASSIFIED EXPERIENCE RESPONSES
# ==============================================================================

experience_unclassified <- df %>%
  select(all_of(experience_tech_cols)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "technology_col",
    values_to = "experience_raw"
  ) %>%
  mutate(
    experience_raw = str_squish(as.character(experience_raw)),
    experience_raw = na_if(experience_raw, ""),
    experience_clean = recode_experience(experience_raw)
  ) %>%
  filter(
    !is.na(experience_raw),
    is.na(experience_clean)
  ) %>%
  count(experience_raw, sort = TRUE)

write_csv(
  experience_unclassified,
  out_file(csv_dir, "experience_unclassified.csv")
)

print(experience_unclassified, n = Inf)

# ==============================================================================
# CLEAN YEAR OF BIRTH
# ==============================================================================

project_year <- 2026

df <- df %>%
  mutate(
    year_of_birth_raw = suppressWarnings(as.numeric(.data[[year_of_birth_col]])),
    
    year_of_birth_clean = case_when(
      year_of_birth_raw >= 1900 & year_of_birth_raw <= 2007 ~ year_of_birth_raw,
      year_of_birth_raw >= 18 & year_of_birth_raw <= 100 ~ project_year - year_of_birth_raw,
      TRUE ~ NA_real_
    )
  )

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

clean_text_basic <- function(x) {
  x <- str_squish(as.character(x))
  x <- na_if(x, "")
  x <- na_if(x, "NA")
  x <- na_if(x, "NaN")
  x
}

get_optional_col <- function(data, col_name) {
  if (col_name %in% names(data)) {
    as.character(data[[col_name]])
  } else {
    rep(NA_character_, nrow(data))
  }
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
# SOCIOECONOMIC CLEANING FUNCTIONS
# ==============================================================================

clean_gender <- function(x) {
  x <- str_squish(as.character(x))
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    str_detect(x, regex("^male", ignore_case = TRUE)) ~ "Male",
    str_detect(x, regex("^female", ignore_case = TRUE)) ~ "Female",
    TRUE ~ "Other / Prefer not to say"
  )
}

clean_country <- function(x) {
  x <- str_squish(as.character(x))
  x <- str_remove(x, "\\s*\\(ID[0-9]+\\)$")
  x <- str_remove(x, "^[A-Z]{2}\\s*[–-]\\s*")
  
  x <- case_when(
    is.na(x) | x == "" ~ NA_character_,
    x %in% c("España") ~ "Spain",
    x %in% c("Alemania") ~ "Germany",
    x %in% c("Francia") ~ "France",
    x %in% c("Italia") ~ "Italy",
    x %in% c("Portugal") ~ "Portugal",
    x %in% c("Reino Unido") ~ "United Kingdom",
    x %in% c("Países Bajos") ~ "Netherlands",
    x %in% c("Bélgica") ~ "Belgium",
    x %in% c("Suiza") ~ "Switzerland",
    x %in% c("Suecia") ~ "Sweden",
    x %in% c("Noruega") ~ "Norway",
    x %in% c("Dinamarca") ~ "Denmark",
    x %in% c("Finlandia") ~ "Finland",
    x %in% c("Polonia") ~ "Poland",
    x %in% c("República Checa") ~ "Czech Republic",
    x %in% c("Czechia") ~ "Czech Republic",
    x %in% c("Moldova (Republic of Moldova)") ~ "Moldova",
    x %in% c("Russia", "Russian Federation") ~ "Russia",
    x %in% c("United Kingdom *", "UK", "Great Britain") ~ "United Kingdom",
    TRUE ~ x
  )
  
  x
}

clean_residence_region <- function(country) {
  country <- clean_country(country)
  
  case_when(
    country %in% c(
      "Denmark", "Estonia", "Finland", "Ireland", "Iceland",
      "Latvia", "Lithuania", "Norway", "United Kingdom", "Sweden"
    ) ~ "Northern Europe",
    
    country %in% c(
      "Germany", "Austria", "Belgium", "France", "Liechtenstein",
      "Luxembourg", "Monaco", "Netherlands", "Switzerland"
    ) ~ "Western Europe",
    
    country %in% c(
      "Albania", "Andorra", "Bosnia and Herzegovina", "Croatia",
      "Slovenia", "Spain", "Greece", "Italy", "Malta",
      "Montenegro", "Portugal", "North Macedonia", "San Marino",
      "Serbia", "Cyprus"
    ) ~ "Southern Europe",
    
    country %in% c(
      "Belarus", "Bulgaria", "Slovakia", "Hungary",
      "Moldova", "Poland", "Czech Republic", "Romania",
      "Russia", "Ukraine"
    ) ~ "Eastern Europe",
    
    is.na(country) ~ NA_character_,
    TRUE ~ "Other region"
  )
}

clean_city_size <- function(x) {
  x <- str_squish(as.character(x))
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    str_detect(x, regex("^Village", ignore_case = TRUE)) ~ "Village or rural area",
    str_detect(x, regex("^Small town", ignore_case = TRUE)) ~ "Small town",
    str_detect(x, regex("^Town", ignore_case = TRUE)) ~ "Town",
    str_detect(x, regex("^Small city", ignore_case = TRUE)) ~ "Small city",
    str_detect(x, regex("^Medium city", ignore_case = TRUE)) ~ "Medium-sized city",
    str_detect(x, regex("^Large city", ignore_case = TRUE)) ~ "Large city",
    str_detect(x, regex("^Global city", ignore_case = TRUE)) ~ "Metropolis",
    TRUE ~ NA_character_
  )
}

clean_climate_zone <- function(x) {
  x <- str_squish(as.character(x))
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    str_detect(x, regex("Subtropical", ignore_case = TRUE)) ~ "Warm/Mediterranean climate",
    str_detect(x, regex("Temperate", ignore_case = TRUE)) ~ "Temperate climate",
    str_detect(x, regex("Cold|Circumpolar", ignore_case = TRUE)) ~ "Cold/Polar climate",
    TRUE ~ NA_character_
  )
}

clean_employment <- function(x) {
  x <- str_squish(as.character(x))
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    str_detect(x, regex("Full time employed|Part-time employed|Self-employed", ignore_case = TRUE)) ~ "Employed",
    str_detect(x, regex("Student", ignore_case = TRUE)) ~ "Student",
    str_detect(x, regex("Unemployed", ignore_case = TRUE)) ~ "Unemployed",
    str_detect(x, regex("Stay-at-home|Retired", ignore_case = TRUE)) ~ "Other inactive status",
    TRUE ~ "Other status"
  )
}

clean_education <- function(x) {
  x <- str_squish(as.character(x))
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    str_detect(x, regex("University", ignore_case = TRUE)) ~ "University education",
    str_detect(x, regex("Primary|Secondary|Vocational", ignore_case = TRUE)) ~ "Non-university education",
    TRUE ~ NA_character_
  )
}

clean_work_home <- function(x) {
  x <- str_squish(as.character(x))
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    str_detect(x, regex("^Yes", ignore_case = TRUE)) ~ "Yes",
    str_detect(x, regex("^No", ignore_case = TRUE)) ~ "No",
    TRUE ~ NA_character_
  )
}

clean_type_house <- function(x) {
  x <- str_squish(as.character(x))
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    str_detect(x, regex("Uni-personal", ignore_case = TRUE)) ~ "Living alone",
    str_detect(x, regex("Dual Income No Kids|Ageing family", ignore_case = TRUE)) ~ "Couple without children",
    str_detect(x, regex("Traditional family|Large family|Single parenthood", ignore_case = TRUE)) ~ "Household with children",
    str_detect(x, regex("Poly-nuclear", ignore_case = TRUE)) ~ "Shared household",
    TRUE ~ "Other household type"
  )
}

clean_tenure <- function(x) {
  x <- str_squish(as.character(x))
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    str_detect(x, regex("own the home outright|fully paid-off", ignore_case = TRUE)) ~ "Homeowner without mortgage",
    str_detect(x, regex("mortgage|outstanding payments", ignore_case = TRUE)) ~ "Homeowner with mortgage",
    str_detect(x, regex("rent|rental", ignore_case = TRUE)) ~ "Rented home",
    TRUE ~ "Other tenure status"
  )
}

clean_political_orientation <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  
  case_when(
    is.na(x) ~ NA_character_,
    x < 20 ~ "Far left",
    x < 40 ~ "Left",
    x >= 40 & x <= 60 ~ "Centre",
    x > 60 & x <= 80 ~ "Right",
    x > 80 ~ "Far right",
    TRUE ~ NA_character_
  )
}

# ==============================================================================
# CLEAN TECHNOLOGY LABELS
# ==============================================================================

clean_technology <- function(x) {
  case_when(
    str_detect(x, "balcony_solar_kits") ~ "Balcony solar kits",
    str_detect(x, "change_of_electricity_tariff") ~ "Electricity tariff change",
    str_detect(x, "cooling_system") ~ "Cooling system",
    str_detect(x, "domestic_hot_water_system") ~ "Domestic hot water system",
    str_detect(x, "electric_vehicle") ~ "Electric vehicle",
    str_detect(x, "elevator") ~ "Elevator",
    str_detect(x, "energy_efficient_appliances") ~ "Energy-efficient appliances",
    str_detect(x, "energy_storage_systems") ~ "Energy storage systems",
    str_detect(x, "envelope_renovation") ~ "Envelope renovation",
    str_detect(x, "fosil_fuel_or_biomass_based_heating_system") ~ "Fossil fuel or biomass heating",
    str_detect(x, "heat_pump_based_heating_system") ~ "Heat pump",
    str_detect(x, "heat_recovery_mechanical_ventilation") ~ "Heat recovery ventilation",
    str_detect(x, "join_an_energy_community") ~ "Energy community",
    str_detect(x, "micro_efficiency_measures") ~ "Micro-efficiency measures",
    str_detect(x, "rooftop_photovoltaic_system") ~ "Rooftop photovoltaic system",
    str_detect(x, "smart_home_systems") ~ "Smart home systems",
    TRUE ~ x
  )
}

# ==============================================================================
# CREATE LONG EXPERIENCE TABLE
# ==============================================================================

experience_long <- df %>%
  mutate(
    country_clean_final = coalesce(
      clean_country(get_optional_col(., "country_clean")),
      clean_country(get_optional_col(., country_col))
    ),
    residence_region_final = coalesce(
      clean_residence_region(country_clean_final),
      clean_residence_region(get_optional_col(., "residence_region"))
    )
  ) %>%
  select(
    participant_id,
    year_of_birth = year_of_birth_clean,
    gender = all_of(gender_col),
    country = country_clean_final,
    residence_region = residence_region_final,
    city_size = all_of(size_city_col),
    climate_zone = all_of(climate_zone_col),
    employment = all_of(employment_col),
    education_level = all_of(education_level_col),
    work_home = all_of(work_home_col),
    type_house = all_of(type_house_col),
    tenure = all_of(tenure_col),
    political_orientation_raw = all_of(political_col),
    all_of(experience_tech_cols)
  ) %>%
  pivot_longer(
    cols = all_of(experience_tech_cols),
    names_to = "technology_col",
    values_to = "experience_raw"
  ) %>%
  mutate(
    technology = clean_technology(technology_col),
    
    experience_raw = str_squish(as.character(experience_raw)),
    experience_raw = na_if(experience_raw, ""),
    experience_clean = recode_experience(experience_raw),
    
    gender = clean_gender(gender),
    city_size = clean_city_size(city_size),
    climate_zone = clean_climate_zone(climate_zone),
    employment = clean_employment(employment),
    education_group = clean_education(education_level),
    work_home = clean_work_home(work_home),
    type_house = clean_type_house(type_house),
    tenure = clean_tenure(tenure),
    political_orientation = clean_political_orientation(political_orientation_raw),
    
    age_group = case_when(
      is.na(year_of_birth) ~ NA_character_,
      year_of_birth >= 2001 & year_of_birth <= 2007 ~ "Generation Z",
      year_of_birth >= 1986 & year_of_birth <= 2000 ~ "Millennials",
      year_of_birth >= 1971 & year_of_birth <= 1985 ~ "Generation X",
      year_of_birth >= 1932 & year_of_birth <= 1970 ~ "Boomers + Silent Generation",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(experience_clean))

write_csv(
  experience_long,
  out_file(csv_dir, "experience_long.csv")
)

glimpse(experience_long)

# ==============================================================================
# PREPARE SHORT EXPERIENCE LABELS
# ==============================================================================

experience_short_levels <- c(
  "Already present",
  "Implemented",
  "Searched for information",
  "Aware / would consider",
  "Unaware",
  "Would never use it"
)

awareness_order_levels <- c(
  "Unaware",
  "Aware / would consider"
)

experience_long <- experience_long %>%
  mutate(
    experience_clean = factor(experience_clean, levels = experience_levels),
    
    experience_short = case_when(
      experience_clean == "Already present when I moved in" ~ "Already present",
      experience_clean == "I have installed / implemented / am currently doing this myself" ~ "Implemented",
      experience_clean == "I have already actively looked for information about it" ~ "Searched for information",
      experience_clean == "I am unaware of this technology, decision, or behaviour" ~ "Unaware",
      experience_clean == "I am aware of this and could consider it in the future" ~ "Aware / would consider",
      experience_clean == "I would never apply / use it" ~ "Would never use it",
      TRUE ~ NA_character_
    ),
    
    experience_short = factor(experience_short, levels = experience_short_levels)
  )

write_csv(
  experience_long,
  out_file(csv_dir, "experience_long.csv")
)

# ==============================================================================
# GENERAL EXPERIENCE DISTRIBUTION
# ==============================================================================

experience_distribution <- experience_long %>%
  count(experience_short, sort = FALSE) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  arrange(desc(n)) %>%
  mutate(
    experience_short = factor(
      experience_short,
      levels = rev(as.character(experience_short))
    )
  )

write_csv(
  experience_distribution,
  out_file(csv_dir, "experience_distribution.csv")
)

print(experience_distribution, n = Inf)

# ==============================================================================
# EXPERIENCE DISTRIBUTION BY TECHNOLOGY
# ==============================================================================

experience_distribution_by_technology <- experience_long %>%
  count(technology, experience_short, sort = FALSE) %>%
  group_by(technology) %>%
  complete(
    experience_short = factor(experience_short_levels, levels = experience_short_levels),
    fill = list(n = 0)
  ) %>%
  mutate(
    total_technology = sum(n),
    percentage = if_else(total_technology > 0, n / total_technology * 100, 0)
  ) %>%
  ungroup()

technology_order <- experience_distribution_by_technology %>%
  filter(experience_short %in% awareness_order_levels) %>%
  group_by(technology) %>%
  summarise(
    score_awareness = sum(n),
    .groups = "drop"
  ) %>%
  arrange(score_awareness) %>%
  pull(technology)

experience_distribution_by_technology <- experience_distribution_by_technology %>%
  mutate(
    technology = factor(technology, levels = technology_order)
  )

write_csv(
  experience_distribution_by_technology,
  out_file(csv_dir, "experience_distribution_by_technology.csv")
)

print(experience_distribution_by_technology, n = Inf)

# ==============================================================================
# CHECKS
# ==============================================================================

n_participants_total <- n_distinct(df$participant_id)
n_participants_valid <- n_distinct(experience_long$participant_id)
n_technologies <- length(experience_tech_cols)
n_possible <- n_participants_total * n_technologies
n_valid <- nrow(experience_long)

subtitle_sample <- paste0(
  "n survey = ", n_participants_total,
  "; n with valid Experience = ", n_participants_valid,
  "; valid person-technology observations = ", n_valid
)

cat("Total participants:", n_participants_total, "\n")
cat("Participants with valid Experience:", n_participants_valid, "\n")
cat("Technologies:", n_technologies, "\n")
cat("Possible person-technology observations:", n_possible, "\n")
cat("Valid observations:", n_valid, "\n")
cat("Empty/NA observations:", n_possible - n_valid, "\n")

responses_per_participant <- experience_long %>%
  count(participant_id, name = "n_technologies_answered") %>%
  arrange(n_technologies_answered)

write_csv(
  responses_per_participant,
  out_file(csv_dir, "responses_per_participant_experience.csv")
)

print(responses_per_participant, n = Inf)

# ==============================================================================
# CHECK CLEAN VARIABLES
# ==============================================================================

variables_check <- experience_long %>%
  distinct(
    participant_id,
    year_of_birth,
    age_group,
    gender,
    country,
    residence_region,
    city_size,
    climate_zone,
    employment,
    education_group,
    work_home,
    type_house,
    tenure,
    political_orientation
  )

write_csv(
  variables_check,
  out_file(csv_dir, "socioeconomic_variables_check.csv")
)

# ==============================================================================
# SAVE PLOT FUNCTIONS
# ==============================================================================

save_plot_png <- function(plot, filename, width = 9, height = 5) {
  ggsave(
    filename = out_file(plots_dir, paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}

save_plot_png_technology <- function(plot, filename, width = 9, height = 5) {
  ggsave(
    filename = out_file(plots_technology_dir, paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}

# ==============================================================================
# PLOT STYLE
# ==============================================================================

main_palette <- c(
  "#0072B2", "#56B4E9", "#009E73", "#E69F00",
  "#D55E00", "#CC79A7", "#F0E442", "#999999",
  "#332288", "#88CCEE", "#44AA99", "#DDCC77"
)

make_named_palette <- function(levels_vec) {
  levels_vec <- as.character(levels_vec)
  colors <- rep(main_palette, length.out = length(levels_vec))
  names(colors) <- levels_vec
  colors
}

experience_colors <- make_named_palette(experience_short_levels)

plot_base_size <- 15
plot_title_size <- 18
plot_subtitle_size <- 13
plot_axis_title_size <- 15
plot_axis_text_size <- 15
plot_legend_text_size <- 13
plot_legend_title_size <- 14
plot_label_size <- 4.2
plot_stack_label_size <- 3.1
plot_heatmap_label_size <- 3.5

label_color_experience <- function(x) {
  case_when(
    as.character(x) %in% c("Already present", "Searched for information", "Unaware") ~ "white",
    TRUE ~ "black"
  )
}

label_color_by_position <- function(x, levels_vec) {
  idx <- match(as.character(x), as.character(levels_vec))
  
  if_else(
    idx %in% c(1, 3, 5, 6, 9, 13, 16, 18, 20, 21),
    "white",
    "black"
  )
}

theme_experience <- theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(face = "bold", size = plot_title_size),
    plot.subtitle = element_text(size = plot_subtitle_size),
    axis.title.x = element_text(size = plot_axis_title_size, margin = margin(t = 8)),
    axis.title.y = element_text(size = plot_axis_title_size, margin = margin(r = 8)),
    axis.text.x = element_text(size = plot_axis_text_size),
    axis.text.y = element_text(size = plot_axis_text_size),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = plot_legend_title_size),
    legend.text = element_text(size = plot_legend_text_size),
    panel.grid.minor = element_blank(),
    plot.margin = margin(12, 45, 12, 12)
  )

theme_experience_heatmap <- theme_minimal(base_size = plot_base_size) +
  theme(
    plot.title = element_text(face = "bold", size = plot_title_size),
    plot.subtitle = element_text(size = plot_subtitle_size),
    axis.title.x = element_text(size = plot_axis_title_size, margin = margin(t = 8)),
    axis.title.y = element_text(size = plot_axis_title_size, margin = margin(r = 8)),
    axis.text.x = element_text(size = plot_axis_text_size, angle = 35, hjust = 1),
    axis.text.y = element_text(size = plot_axis_text_size),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = plot_legend_title_size),
    legend.text = element_text(size = plot_legend_text_size),
    panel.grid.minor = element_blank(),
    plot.margin = margin(12, 12, 12, 12)
  )

# ==============================================================================
# TOTAL EXPERIENCE PLOT
# ==============================================================================

experience_distribution_plot <- experience_distribution %>%
  mutate(
    label_npct = paste0(n, " (", round(percentage, 1), "%)")
  )

plot_experience_total <- ggplot(
  experience_distribution_plot,
  aes(
    x = experience_short,
    y = percentage,
    fill = experience_short
  )
) +
  geom_col(
    color = "black",
    linewidth = 0.25
  ) +
  geom_text(
    aes(label = label_npct),
    hjust = -0.10,
    size = plot_label_size,
    lineheight = 0.9
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = experience_colors, drop = FALSE) +
  scale_y_continuous(
    limits = c(0, max(experience_distribution_plot$percentage) + 12),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Overall distribution of experience responses",
    subtitle = subtitle_sample,
    x = NULL,
    y = "Percentage",
    fill = "Experience category"
  ) +
  theme_experience

print(plot_experience_total)

save_plot_png(
  plot = plot_experience_total,
  filename = "experience_distribution_total",
  width = 11,
  height = 6
)

# ==============================================================================
# STACKED BARS BY TECHNOLOGY - ABSOLUTE VALUES
# ==============================================================================

experience_distribution_by_technology_plot <- experience_distribution_by_technology %>%
  mutate(
    technology_label = str_wrap(as.character(technology), width = 32),
    technology_label = factor(
      technology_label,
      levels = str_wrap(as.character(technology_order), width = 32)
    ),
    label_npct = if_else(
      n > 0 & percentage >= 1,
      paste0(n, " (", round(percentage, 0), "%)"),
      ""
    ),
    label_color = label_color_experience(experience_short)
  )

plot_experience_by_technology <- ggplot(
  experience_distribution_by_technology_plot,
  aes(
    x = technology_label,
    y = n,
    fill = experience_short
  )
) +
  geom_col(
    color = "black",
    linewidth = 0.15
  ) +
  geom_text(
    aes(
      label = label_npct,
      color = label_color
    ),
    position = position_stack(vjust = 0.5),
    size = plot_stack_label_size,
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_fill_manual(values = experience_colors, drop = FALSE) +
  scale_color_identity() +
  labs(
    title = "Experience responses by technology",
    subtitle = paste0(
      subtitle_sample,
      ". Technologies ordered by the sum of 'Unaware' and 'Aware / would consider'"
    ),
    x = NULL,
    y = "Number of person-technology responses",
    fill = "Experience category"
  ) +
  theme_experience

print(plot_experience_by_technology)

save_plot_png(
  plot = plot_experience_by_technology,
  filename = "experience_distribution_by_technology_absolute",
  width = 14,
  height = 9
)

# ==============================================================================
# STACKED BARS BY TECHNOLOGY - PERCENTAGE
# ==============================================================================

plot_experience_by_technology_percentage <- ggplot(
  experience_distribution_by_technology_plot,
  aes(
    x = technology_label,
    y = percentage,
    fill = experience_short
  )
) +
  geom_col(
    color = "black",
    linewidth = 0.15
  ) +
  geom_text(
    aes(
      label = label_npct,
      color = label_color
    ),
    position = position_stack(vjust = 0.5),
    size = plot_stack_label_size,
    lineheight = 0.9,
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_fill_manual(values = experience_colors, drop = FALSE) +
  scale_color_identity() +
  scale_y_continuous(
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Percentage distribution of experience by technology",
    subtitle = paste0(
      subtitle_sample,
      ". Percentages calculated within each technology"
    ),
    x = NULL,
    y = "Percentage",
    fill = "Experience category"
  ) +
  theme_experience

print(plot_experience_by_technology_percentage)

save_plot_png(
  plot = plot_experience_by_technology_percentage,
  filename = "experience_distribution_by_technology_percentage",
  width = 14,
  height = 9
)

# ==============================================================================
# HEATMAP BY TECHNOLOGY
# ==============================================================================

experience_distribution_by_technology_heatmap <- experience_distribution_by_technology %>%
  mutate(
    technology_label = str_wrap(as.character(technology), width = 32),
    technology_label = factor(
      technology_label,
      levels = str_wrap(as.character(technology_order), width = 32)
    )
  )

plot_experience_heatmap <- ggplot(
  experience_distribution_by_technology_heatmap,
  aes(
    x = experience_short,
    y = technology_label,
    fill = percentage
  )
) +
  geom_tile(color = "black", linewidth = 0.25) +
  scale_fill_gradientn(
    colours = c("white", main_palette[1], main_palette[2], main_palette[3], main_palette[5]),
    limits = c(0, 100)
  ) +
  labs(
    title = "Experience distribution by technology",
    subtitle = paste0(
      subtitle_sample,
      ". Percentages calculated within each technology"
    ),
    x = "Experience category",
    y = NULL,
    fill = "Percentage"
  ) +
  theme_experience_heatmap

print(plot_experience_heatmap)

save_plot_png(
  plot = plot_experience_heatmap,
  filename = "experience_distribution_heatmap_by_technology",
  width = 14,
  height = 9
)

# ==============================================================================
# INDIVIDUAL TECHNOLOGY PLOTS
# ==============================================================================

plot_single_technology_absolute <- function(technology_name) {
  
  plot_data <- experience_distribution_by_technology %>%
    filter(technology == technology_name) %>%
    mutate(
      experience_short = factor(
        experience_short,
        levels = rev(experience_short_levels)
      ),
      label_npct = if_else(
        n > 0,
        paste0(n, " (", round(percentage, 0), "%)"),
        ""
      ),
      label_color = label_color_experience(experience_short),
      label_y = case_when(
        n == 0 ~ 0,
        n < 2 ~ n + 0.25,
        TRUE ~ n / 2
      ),
      label_hjust = case_when(
        n == 0 ~ 0,
        n < 2 ~ 0,
        TRUE ~ 0.5
      )
    )
  
  total_technology <- unique(plot_data$total_technology)
  
  p <- ggplot(
    plot_data,
    aes(
      x = experience_short,
      y = n,
      fill = experience_short
    )
  ) +
    geom_col(
      color = "black",
      linewidth = 0.25
    ) +
    geom_text(
      aes(
        y = label_y,
        label = label_npct,
        color = label_color,
        hjust = label_hjust
      ),
      size = plot_label_size,
      show.legend = FALSE
    ) +
    coord_flip(clip = "off") +
    scale_fill_manual(values = experience_colors, drop = FALSE) +
    scale_color_identity() +
    scale_y_continuous(
      limits = c(0, max(plot_data$n, na.rm = TRUE) + 8),
      breaks = scales::pretty_breaks()
    ) +
    labs(
      title = paste0("Declared experience: ", technology_name),
      subtitle = paste0(
        "n survey = ", n_participants_total,
        "; person-technology responses for this intervention = ", total_technology
      ),
      x = NULL,
      y = "Number of responses",
      fill = "Experience category"
    ) +
    theme_experience
  
  print(p)
  
  save_plot_png_technology(
    plot = p,
    filename = paste0("experience_absolute_", clean_filename(technology_name)),
    width = 10,
    height = 6
  )
  
  return(p)
}

plot_single_technology_percentage <- function(technology_name) {
  
  plot_data <- experience_distribution_by_technology %>%
    filter(technology == technology_name) %>%
    mutate(
      experience_short = factor(
        experience_short,
        levels = rev(experience_short_levels)
      ),
      label_npct = if_else(
        n > 0 & percentage >= 1,
        paste0(n, " (", round(percentage, 0), "%)"),
        ""
      ),
      label_color = label_color_experience(experience_short),
      label_y = case_when(
        percentage == 0 ~ 0,
        percentage < 5 ~ percentage + 2,
        TRUE ~ percentage / 2
      ),
      label_hjust = case_when(
        percentage == 0 ~ 0,
        percentage < 5 ~ 0,
        TRUE ~ 0.5
      )
    )
  
  total_technology <- unique(plot_data$total_technology)
  
  p <- ggplot(
    plot_data,
    aes(
      x = experience_short,
      y = percentage,
      fill = experience_short
    )
  ) +
    geom_col(
      color = "black",
      linewidth = 0.25
    ) +
    geom_text(
      aes(
        y = label_y,
        label = label_npct,
        color = label_color,
        hjust = label_hjust
      ),
      size = plot_label_size,
      show.legend = FALSE
    ) +
    coord_flip(clip = "off") +
    scale_fill_manual(values = experience_colors, drop = FALSE) +
    scale_color_identity() +
    scale_y_continuous(
      limits = c(0, 108),
      breaks = seq(0, 100, 10),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = paste0("Percentage distribution: ", technology_name),
      subtitle = paste0(
        "n survey = ", n_participants_total,
        "; person-technology responses for this intervention = ", total_technology
      ),
      x = NULL,
      y = "Percentage",
      fill = "Experience category"
    ) +
    theme_experience
  
  print(p)
  
  save_plot_png_technology(
    plot = p,
    filename = paste0("experience_percentage_", clean_filename(technology_name)),
    width = 10,
    height = 6
  )
  
  return(p)
}

technology_names <- as.character(technology_order)

plot_experience_individual_absolute_by_technology <- lapply(
  technology_names,
  plot_single_technology_absolute
)

names(plot_experience_individual_absolute_by_technology) <- technology_names

plot_experience_individual_percentage_by_technology <- lapply(
  technology_names,
  plot_single_technology_percentage
)

names(plot_experience_individual_percentage_by_technology) <- technology_names

# ==============================================================================
# SOCIODEMOGRAPHIC GROUP FUNCTIONS
# ==============================================================================

calculate_experience_by_group <- function(data, group_col) {
  data %>%
    filter(
      !is.na(.data[[group_col]]),
      !is.na(experience_short)
    ) %>%
    count(
      group = .data[[group_col]],
      experience_short,
      sort = FALSE
    ) %>%
    group_by(group) %>%
    complete(
      experience_short = factor(experience_short_levels, levels = experience_short_levels),
      fill = list(n = 0)
    ) %>%
    mutate(
      total_group = sum(n),
      percentage = if_else(total_group > 0, n / total_group * 100, 0)
    ) %>%
    ungroup()
}

order_groups_by_awareness <- function(distribution_data) {
  distribution_data %>%
    filter(experience_short %in% awareness_order_levels) %>%
    group_by(group) %>%
    summarise(
      score_awareness = sum(n),
      .groups = "drop"
    ) %>%
    arrange(score_awareness) %>%
    pull(group)
}

get_natural_group_order <- function(distribution_data, variable_name = NULL) {
  
  natural_orders <- list(
    age_group = c(
      "Generation Z",
      "Millennials",
      "Generation X",
      "Boomers + Silent Generation"
    ),
    gender = c(
      "Female",
      "Male",
      "Other / Prefer not to say"
    ),
    residence_region = c(
      "Northern Europe",
      "Western Europe",
      "Southern Europe",
      "Eastern Europe",
      "Other region"
    ),
    city_size = c(
      "Village or rural area",
      "Small town",
      "Town",
      "Small city",
      "Medium-sized city",
      "Large city",
      "Metropolis"
    ),
    climate_zone = c(
      "Warm/Mediterranean climate",
      "Temperate climate",
      "Cold/Polar climate"
    ),
    employment = c(
      "Employed",
      "Student",
      "Unemployed",
      "Other inactive status",
      "Other status"
    ),
    education_group = c(
      "Non-university education",
      "University education"
    ),
    work_home = c(
      "Yes",
      "No"
    ),
    type_house = c(
      "Living alone",
      "Couple without children",
      "Household with children",
      "Shared household",
      "Other household type"
    ),
    tenure = c(
      "Homeowner without mortgage",
      "Homeowner with mortgage",
      "Rented home",
      "Other tenure status"
    ),
    political_orientation = c(
      "Far left",
      "Left",
      "Centre",
      "Right",
      "Far right"
    )
  )
  
  if (!is.null(variable_name) && variable_name %in% names(natural_orders)) {
    return(natural_orders[[variable_name]])
  }
  
  distribution_data %>%
    distinct(group) %>%
    pull(group) %>%
    as.character()
}

# ==============================================================================
# GROUP PLOT FUNCTIONS
# ==============================================================================

plot_experience_by_group <- function(distribution_data, title, subtitle, filename,
                                     width = 11, height = 6) {
  
  group_order <- order_groups_by_awareness(distribution_data)
  
  plot_data <- distribution_data %>%
    mutate(
      group_label = str_wrap(as.character(group), width = 28),
      group_label = factor(
        group_label,
        levels = str_wrap(as.character(group_order), width = 28)
      ),
      label_npct = if_else(
        n > 0 & percentage >= 1,
        paste0(n, " (", round(percentage, 0), "%)"),
        ""
      ),
      label_color = label_color_experience(experience_short)
    )
  
  p <- ggplot(
    plot_data,
    aes(
      x = group_label,
      y = n,
      fill = experience_short
    )
  ) +
    geom_col(
      color = "black",
      linewidth = 0.15
    ) +
    geom_text(
      aes(
        label = label_npct,
        color = label_color
      ),
      position = position_stack(vjust = 0.5),
      size = plot_stack_label_size,
      show.legend = FALSE
    ) +
    coord_flip() +
    scale_fill_manual(values = experience_colors, drop = FALSE) +
    scale_color_identity() +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Number of person-technology responses",
      fill = "Experience category"
    ) +
    theme_experience
  
  print(p)
  
  save_plot_png(
    plot = p,
    filename = filename,
    width = width,
    height = height
  )
  
  return(p)
}

plot_experience_percentage_by_group <- function(distribution_data, title, subtitle, filename,
                                                width = 11, height = 6) {
  
  group_order <- order_groups_by_awareness(distribution_data)
  
  plot_data <- distribution_data %>%
    mutate(
      group_label = str_wrap(as.character(group), width = 28),
      group_label = factor(
        group_label,
        levels = str_wrap(as.character(group_order), width = 28)
      ),
      label_npct = if_else(
        n > 0 & percentage >= 1,
        paste0(n, " (", round(percentage, 0), "%)"),
        ""
      ),
      label_color = label_color_experience(experience_short)
    )
  
  p <- ggplot(
    plot_data,
    aes(
      x = group_label,
      y = percentage,
      fill = experience_short
    )
  ) +
    geom_col(
      color = "black",
      linewidth = 0.15
    ) +
    geom_text(
      aes(
        label = label_npct,
        color = label_color
      ),
      position = position_stack(vjust = 0.5),
      size = plot_stack_label_size,
      show.legend = FALSE
    ) +
    coord_flip(clip = "off") +
    scale_fill_manual(values = experience_colors, drop = FALSE) +
    scale_color_identity() +
    scale_y_continuous(
      limits = c(0, 102),
      breaks = seq(0, 100, 25),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Percentage",
      fill = "Experience category"
    ) +
    theme_experience
  
  print(p)
  
  save_plot_png(
    plot = p,
    filename = filename,
    width = width,
    height = height
  )
  
  return(p)
}

plot_experience_heatmap_by_group <- function(distribution_data, title, subtitle, filename,
                                             width = 11, height = 6) {
  
  group_order <- order_groups_by_awareness(distribution_data)
  
  plot_data <- distribution_data %>%
    mutate(
      group_label = str_wrap(as.character(group), width = 28),
      group_label = factor(
        group_label,
        levels = str_wrap(as.character(group_order), width = 28)
      )
    )
  
  p <- ggplot(
    plot_data,
    aes(
      x = experience_short,
      y = group_label,
      fill = percentage
    )
  ) +
    geom_tile(color = "black", linewidth = 0.25) +
    scale_fill_gradientn(
      colours = c("white", main_palette[1], main_palette[2], main_palette[3], main_palette[5]),
      limits = c(0, 100)
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Experience category",
      y = NULL,
      fill = "Percentage"
    ) +
    theme_experience_heatmap
  
  print(p)
  
  save_plot_png(
    plot = p,
    filename = filename,
    width = width,
    height = height
  )
  
  return(p)
}

# ==============================================================================
# GROUP PLOT FUNCTIONS WITHOUT AWARENESS ORDER
# ==============================================================================

plot_experience_by_group_no_order <- function(distribution_data, title, subtitle, filename,
                                              variable_name = NULL,
                                              width = 11, height = 6) {
  
  group_order <- get_natural_group_order(distribution_data, variable_name)
  
  plot_data <- distribution_data %>%
    mutate(
      group_label = str_wrap(as.character(group), width = 28),
      group_label = factor(
        group_label,
        levels = str_wrap(group_order, width = 28)
      ),
      label_npct = if_else(
        n > 0 & percentage >= 1,
        paste0(n, " (", round(percentage, 0), "%)"),
        ""
      ),
      label_color = label_color_experience(experience_short)
    )
  
  p <- ggplot(
    plot_data,
    aes(
      x = group_label,
      y = n,
      fill = experience_short
    )
  ) +
    geom_col(
      color = "black",
      linewidth = 0.15
    ) +
    geom_text(
      aes(
        label = label_npct,
        color = label_color
      ),
      position = position_stack(vjust = 0.5),
      size = plot_stack_label_size,
      show.legend = FALSE
    ) +
    coord_flip() +
    scale_fill_manual(values = experience_colors, drop = FALSE) +
    scale_color_identity() +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Number of person-technology responses",
      fill = "Experience category"
    ) +
    theme_experience
  
  print(p)
  save_plot_png(p, filename, width, height)
  return(p)
}

plot_experience_percentage_by_group_no_order <- function(distribution_data, title, subtitle, filename,
                                                         variable_name = NULL,
                                                         width = 11, height = 6) {
  
  group_order <- get_natural_group_order(distribution_data, variable_name)
  
  plot_data <- distribution_data %>%
    mutate(
      group_label = str_wrap(as.character(group), width = 28),
      group_label = factor(
        group_label,
        levels = str_wrap(group_order, width = 28)
      ),
      label_npct = if_else(
        n > 0 & percentage >= 1,
        paste0(n, " (", round(percentage, 0), "%)"),
        ""
      ),
      label_color = label_color_experience(experience_short)
    )
  
  p <- ggplot(
    plot_data,
    aes(
      x = group_label,
      y = percentage,
      fill = experience_short
    )
  ) +
    geom_col(
      color = "black",
      linewidth = 0.15
    ) +
    geom_text(
      aes(
        label = label_npct,
        color = label_color
      ),
      position = position_stack(vjust = 0.5),
      size = plot_stack_label_size,
      show.legend = FALSE
    ) +
    coord_flip(clip = "off") +
    scale_fill_manual(values = experience_colors, drop = FALSE) +
    scale_color_identity() +
    scale_y_continuous(
      limits = c(0, 102),
      breaks = seq(0, 100, 25),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Percentage",
      fill = "Experience category"
    ) +
    theme_experience
  
  print(p)
  save_plot_png(p, filename, width, height)
  return(p)
}

plot_experience_heatmap_by_group_no_order <- function(distribution_data, title, subtitle, filename,
                                                      variable_name = NULL,
                                                      width = 11, height = 6) {
  
  group_order <- get_natural_group_order(distribution_data, variable_name)
  
  plot_data <- distribution_data %>%
    mutate(
      group_label = str_wrap(as.character(group), width = 28),
      group_label = factor(
        group_label,
        levels = str_wrap(group_order, width = 28)
      )
    )
  
  p <- ggplot(
    plot_data,
    aes(
      x = experience_short,
      y = group_label,
      fill = percentage
    )
  ) +
    geom_tile(color = "black", linewidth = 0.25) +
    scale_fill_gradientn(
      colours = c("white", main_palette[1], main_palette[2], main_palette[3], main_palette[5]),
      limits = c(0, 100)
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Experience category",
      y = NULL,
      fill = "Percentage"
    ) +
    theme_experience_heatmap
  
  print(p)
  save_plot_png(p, filename, width, height)
  return(p)
}

# ==============================================================================
# EXPERIENCE BY SOCIODEMOGRAPHIC VARIABLES
# ==============================================================================

variable_specs <- list(
  age_group = list(
    group_col = "age_group",
    label = "generation",
    table_label = "Generation",
    csv_key = "age_group",
    ordered_key = "age_group",
    no_order_key = "age_group",
    width = 12,
    height = 7
  ),
  gender = list(
    group_col = "gender",
    label = "gender",
    table_label = "Gender",
    csv_key = "gender",
    ordered_key = "gender",
    no_order_key = "gender",
    width = 11,
    height = 6
  ),
  country = list(
    group_col = "country",
    label = "country",
    table_label = "Country",
    csv_key = "country",
    ordered_key = "country",
    no_order_key = "country",
    width = 14,
    height = 9
  ),
  residence_region = list(
    group_col = "residence_region",
    label = "European region",
    table_label = "European region",
    csv_key = "residence_region",
    ordered_key = "residence_region",
    no_order_key = "residence_region",
    width = 11,
    height = 6
  ),
  city_size = list(
    group_col = "city_size",
    label = "city size",
    table_label = "City size",
    csv_key = "city_size",
    ordered_key = "city_size",
    no_order_key = "city_size",
    width = 12,
    height = 7
  ),
  climate_zone = list(
    group_col = "climate_zone",
    label = "climate zone",
    table_label = "Climate zone",
    csv_key = "climate_zone",
    ordered_key = "climate_zone",
    no_order_key = "climate_zone",
    width = 12,
    height = 7
  ),
  employment = list(
    group_col = "employment",
    label = "employment status",
    table_label = "Employment status",
    csv_key = "employment",
    ordered_key = "employment",
    no_order_key = "employment",
    width = 12,
    height = 7
  ),
  education_group = list(
    group_col = "education_group",
    label = "education level",
    table_label = "Education level",
    csv_key = "education",
    ordered_key = "education",
    no_order_key = "education_group",
    width = 11,
    height = 6
  ),
  work_home = list(
    group_col = "work_home",
    label = "work/study from home",
    table_label = "Work/study from home",
    csv_key = "work_home",
    ordered_key = "work_home",
    no_order_key = "work_home",
    width = 11,
    height = 6
  ),
  type_house = list(
    group_col = "type_house",
    label = "household type",
    table_label = "Household type",
    csv_key = "type_house",
    ordered_key = "type_house",
    no_order_key = "type_house",
    width = 12,
    height = 7
  ),
  tenure = list(
    group_col = "tenure",
    label = "tenure status",
    table_label = "Tenure status",
    csv_key = "tenure",
    ordered_key = "tenure",
    no_order_key = "tenure",
    width = 12,
    height = 7
  ),
  political_orientation = list(
    group_col = "political_orientation",
    label = "political orientation",
    table_label = "Political orientation",
    csv_key = "political_orientation",
    ordered_key = "political_orientation",
    no_order_key = "political_orientation",
    width = 12,
    height = 7
  )
)

experience_by_variables <- list()
plots_ordered <- list()
plots_no_order <- list()

for (var_name in names(variable_specs)) {
  
  spec <- variable_specs[[var_name]]
  
  distribution_data <- calculate_experience_by_group(
    experience_long,
    spec$group_col
  )
  
  experience_by_variables[[var_name]] <- distribution_data
  
  write_csv(
    distribution_data,
    out_file(csv_dir, paste0("experience_distribution_by_", spec$csv_key, ".csv"))
  )
  
  plots_ordered[[paste0(var_name, "_absolute")]] <- plot_experience_by_group(
    distribution_data = distribution_data,
    title = paste("Experience responses by", spec$label),
    subtitle = paste0(
      subtitle_sample,
      ". Groups ordered by the sum of 'Unaware' and 'Aware / would consider'"
    ),
    filename = paste0("experience_distribution_by_", spec$ordered_key, "_absolute"),
    width = spec$width,
    height = spec$height
  )
  
  plots_ordered[[paste0(var_name, "_percentage")]] <- plot_experience_percentage_by_group(
    distribution_data = distribution_data,
    title = paste("Percentage distribution of experience by", spec$label),
    subtitle = paste0(
      subtitle_sample,
      ". Percentages calculated within each ", spec$label
    ),
    filename = paste0("experience_distribution_by_", spec$ordered_key, "_percentage"),
    width = spec$width,
    height = spec$height
  )
  
  plots_ordered[[paste0(var_name, "_heatmap")]] <- plot_experience_heatmap_by_group(
    distribution_data = distribution_data,
    title = paste("Experience responses by", spec$label),
    subtitle = paste0(
      subtitle_sample,
      ". Percentages calculated within each ", spec$label
    ),
    filename = paste0("heatmap_experience_by_", spec$ordered_key),
    width = spec$width,
    height = spec$height
  )
  
  plots_no_order[[paste0(var_name, "_absolute_no_order")]] <- plot_experience_by_group_no_order(
    distribution_data = distribution_data,
    title = paste("Experience responses by", spec$label),
    subtitle = paste0(subtitle_sample, ". Groups not ordered by awareness"),
    filename = paste0("experience_distribution_by_", spec$no_order_key, "_absolute_no_order"),
    variable_name = spec$group_col,
    width = spec$width,
    height = spec$height
  )
  
  plots_no_order[[paste0(var_name, "_percentage_no_order")]] <- plot_experience_percentage_by_group_no_order(
    distribution_data = distribution_data,
    title = paste("Percentage distribution of experience by", spec$label),
    subtitle = paste0(subtitle_sample, ". Groups not ordered by awareness"),
    filename = paste0("experience_distribution_by_", spec$no_order_key, "_percentage_no_order"),
    variable_name = spec$group_col,
    width = spec$width,
    height = spec$height
  )
  
  plots_no_order[[paste0(var_name, "_heatmap_no_order")]] <- plot_experience_heatmap_by_group_no_order(
    distribution_data = distribution_data,
    title = paste("Experience responses by", spec$label),
    subtitle = paste0(subtitle_sample, ". Groups not ordered by awareness"),
    filename = paste0("heatmap_experience_by_", spec$no_order_key, "_no_order"),
    variable_name = spec$group_col,
    width = spec$width,
    height = spec$height
  )
}

# ==============================================================================
# SUMMARY TABLE OF PERCENTAGES BY VARIABLE
# ==============================================================================

create_summary_table_by_group <- function(data, group_col, variable_label) {
  
  distribution_data <- calculate_experience_by_group(data, group_col)
  
  participants_by_group <- data %>%
    filter(!is.na(.data[[group_col]])) %>%
    distinct(participant_id, group = .data[[group_col]]) %>%
    count(group, name = "n_participants")
  
  distribution_data %>%
    left_join(participants_by_group, by = "group") %>%
    mutate(
      variable = variable_label
    ) %>%
    select(
      variable,
      group,
      n_participants,
      experience_short,
      n_pairs = n,
      total_pairs = total_group,
      percentage
    )
}

summary_experience_by_variables <- bind_rows(
  lapply(
    variable_specs,
    function(spec) {
      create_summary_table_by_group(
        experience_long,
        spec$group_col,
        spec$table_label
      )
    }
  )
)

write_csv(
  summary_experience_by_variables,
  out_file(csv_dir, "summary_experience_by_variables.csv")
)

print(summary_experience_by_variables, n = Inf)

# ==============================================================================
# RENOVATION AGE FOR ALREADY PRESENT OR IMPLEMENTED TECHNOLOGIES
# ==============================================================================

implemented_experience_levels <- c(
  "Already present when I moved in",
  "I have installed / implemented / am currently doing this myself"
)

renovation_age_options_raw <- df %>%
  select(all_of(renovation_age_cols)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "renovation_age_col",
    values_to = "renovation_age_raw"
  ) %>%
  mutate(
    renovation_age_raw = str_squish(as.character(renovation_age_raw)),
    renovation_age_raw = na_if(renovation_age_raw, "")
  ) %>%
  filter(!is.na(renovation_age_raw)) %>%
  count(renovation_age_raw, sort = TRUE)

write_csv(
  renovation_age_options_raw,
  out_file(csv_dir, "renovation_age_options_raw.csv")
)

print(renovation_age_options_raw, n = Inf)

renovation_age_levels <- c(
  "Less than 5 years ago",
  "5-15 years ago",
  "15-25 years ago",
  "More than 25 years ago / never updated",
  "I do not know"
)

renovation_age_colors <- make_named_palette(renovation_age_levels)

clean_renovation_age <- function(x) {
  x <- str_squish(as.character(x))
  x <- str_remove(x, "\\s*\\(ID[0-9]+\\)$")
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    x == "Yes, less than 5 years ago" ~ "Less than 5 years ago",
    x == "Yes, between 15 and 5 years ago" ~ "5-15 years ago",
    x == "Yes, between 25 and 15 years ago" ~ "15-25 years ago",
    x == "Never or more than 25 years ago" ~ "More than 25 years ago / never updated",
    x == "I do not know" ~ "I do not know",
    TRUE ~ NA_character_
  )
}

experience_stage_for_age <- df %>%
  select(
    participant_id,
    all_of(experience_tech_cols)
  ) %>%
  pivot_longer(
    cols = all_of(experience_tech_cols),
    names_to = "technology_col",
    values_to = "experience_raw"
  ) %>%
  mutate(
    technology_index = match(technology_col, experience_tech_cols),
    technology = clean_technology(technology_col),
    
    experience_raw = str_squish(as.character(experience_raw)),
    experience_raw = na_if(experience_raw, ""),
    experience_clean = recode_experience(experience_raw)
  )

renovation_age_long <- df %>%
  select(
    participant_id,
    all_of(renovation_age_cols)
  ) %>%
  pivot_longer(
    cols = all_of(renovation_age_cols),
    names_to = "renovation_age_col",
    values_to = "renovation_age_raw"
  ) %>%
  mutate(
    technology_index = match(renovation_age_col, renovation_age_cols),
    
    renovation_age_raw = str_squish(as.character(renovation_age_raw)),
    renovation_age_raw = na_if(renovation_age_raw, ""),
    renovation_age_clean = clean_renovation_age(renovation_age_raw)
  ) %>%
  left_join(
    experience_stage_for_age %>%
      select(
        participant_id,
        technology_index,
        technology,
        experience_clean
      ),
    by = c("participant_id", "technology_index")
  ) %>%
  filter(
    experience_clean %in% implemented_experience_levels,
    !is.na(renovation_age_clean)
  ) %>%
  mutate(
    renovation_age_clean = factor(
      renovation_age_clean,
      levels = renovation_age_levels
    )
  )

write_csv(
  renovation_age_long,
  out_file(csv_dir, "renovation_age_long.csv")
)

glimpse(renovation_age_long)

n_renovation_participants <- n_distinct(renovation_age_long$participant_id)
n_renovation_pairs <- nrow(renovation_age_long)

subtitle_renovation_age <- paste0(
  "Only already present or implemented technologies. n participants = ",
  n_renovation_participants,
  "; person-technology observations = ",
  n_renovation_pairs
)

renovation_age_distribution_by_technology <- renovation_age_long %>%
  count(technology, renovation_age_clean, sort = FALSE) %>%
  group_by(technology) %>%
  complete(
    renovation_age_clean = factor(renovation_age_levels, levels = renovation_age_levels),
    fill = list(n = 0)
  ) %>%
  mutate(
    total_technology = sum(n),
    percentage = if_else(total_technology > 0, n / total_technology * 100, 0)
  ) %>%
  ungroup()

write_csv(
  renovation_age_distribution_by_technology,
  out_file(csv_dir, "renovation_age_distribution_by_technology.csv")
)

print(renovation_age_distribution_by_technology, n = Inf)

renovation_age_distribution_by_technology_plot <- renovation_age_distribution_by_technology %>%
  mutate(
    technology = factor(technology, levels = technology_order),
    technology_label = str_wrap(as.character(technology), width = 32),
    technology_label = factor(
      technology_label,
      levels = str_wrap(as.character(technology_order), width = 32)
    ),
    label_npct = if_else(
      n > 0 & percentage >= 1,
      paste0(n, " (", round(percentage, 0), "%)"),
      ""
    ),
    label_color = label_color_by_position(renovation_age_clean, renovation_age_levels)
  )

plot_renovation_age_by_technology <- ggplot(
  renovation_age_distribution_by_technology_plot,
  aes(
    x = technology_label,
    y = n,
    fill = renovation_age_clean
  )
) +
  geom_col(color = "black", linewidth = 0.15) +
  geom_text(
    aes(
      label = label_npct,
      color = label_color
    ),
    position = position_stack(vjust = 0.5),
    size = plot_stack_label_size,
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_fill_manual(values = renovation_age_colors, drop = FALSE) +
  scale_color_identity() +
  labs(
    title = "Age of existing or implemented renovations by technology",
    subtitle = subtitle_renovation_age,
    x = NULL,
    y = "Number of person-technology responses",
    fill = "Renovation age"
  ) +
  theme_experience

print(plot_renovation_age_by_technology)

save_plot_png(
  plot = plot_renovation_age_by_technology,
  filename = "renovation_age_distribution_by_technology_absolute",
  width = 14,
  height = 9
)

plot_renovation_age_by_technology_percentage <- ggplot(
  renovation_age_distribution_by_technology_plot,
  aes(
    x = technology_label,
    y = percentage,
    fill = renovation_age_clean
  )
) +
  geom_col(color = "black", linewidth = 0.15) +
  geom_text(
    aes(
      label = label_npct,
      color = label_color
    ),
    position = position_stack(vjust = 0.5),
    size = plot_stack_label_size,
    show.legend = FALSE
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = renovation_age_colors, drop = FALSE) +
  scale_color_identity() +
  scale_y_continuous(
    limits = c(0, 102),
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Percentage distribution of renovation age by technology",
    subtitle = paste0(
      subtitle_renovation_age,
      ". Percentages calculated within each technology"
    ),
    x = NULL,
    y = "Percentage",
    fill = "Renovation age"
  ) +
  theme_experience

print(plot_renovation_age_by_technology_percentage)

save_plot_png(
  plot = plot_renovation_age_by_technology_percentage,
  filename = "renovation_age_distribution_by_technology_percentage",
  width = 14,
  height = 9
)

plot_renovation_age_heatmap <- ggplot(
  renovation_age_distribution_by_technology_plot,
  aes(
    x = renovation_age_clean,
    y = technology_label,
    fill = percentage
  )
) +
  geom_tile(color = "black", linewidth = 0.25) +
  scale_fill_gradientn(
    colours = c("white", main_palette[1], main_palette[2], main_palette[3], main_palette[5]),
    limits = c(0, 100)
  ) +
  labs(
    title = "Age of existing or implemented renovations by technology",
    subtitle = paste0(
      subtitle_renovation_age,
      ". Percentages calculated within each technology"
    ),
    x = "Renovation age",
    y = NULL,
    fill = "Percentage"
  ) +
  theme_experience_heatmap

print(plot_renovation_age_heatmap)

save_plot_png(
  plot = plot_renovation_age_heatmap,
  filename = "renovation_age_heatmap_by_technology",
  width = 14,
  height = 9
)

# ==============================================================================
# SAVE ALL PLOTS IN A SINGLE PDF
# ==============================================================================

save_plots_pdf <- function(plot_list, filename, width = 14, height = 9) {
  plot_list <- plot_list[!sapply(plot_list, is.null)]
  
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

all_plots <- c(
  list(
    plot_experience_total,
    plot_experience_by_technology,
    plot_experience_by_technology_percentage,
    plot_experience_heatmap
  ),
  plot_experience_individual_absolute_by_technology,
  plot_experience_individual_percentage_by_technology,
  plots_ordered,
  plots_no_order,
  list(
    plot_renovation_age_by_technology,
    plot_renovation_age_by_technology_percentage,
    plot_renovation_age_heatmap
  )
)

all_plots <- all_plots[!sapply(all_plots, is.null)]

save_plots_pdf(
  plot_list = all_plots,
  filename = "experience_all_plots.pdf",
  width = 14,
  height = 9
)

cat("Plots saved in:\n")
cat(plots_dir, "\n")
cat("Combined PDF saved in:\n")
cat(out_file(pdf_dir, "experience_all_plots.pdf"), "\n")

cat("\nMain English output files:\n")
cat("- en_experience_long.csv\n")
cat("- en_experience_distribution.csv\n")
cat("- en_experience_distribution_by_technology.csv\n")
cat("- en_summary_experience_by_variables.csv\n")
cat("- en_renovation_age_long.csv\n")
cat("- en_renovation_age_distribution_by_technology.csv\n")
cat("- en_experience_all_plots.pdf\n")