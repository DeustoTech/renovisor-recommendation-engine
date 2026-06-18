
# SCRIPT 03.1 - SAMPLE DESCRIPTION - EN


library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(tibble)

# PATHS

base_input_dir <- "initial_descriptive_analysis/output/clean_datasets"
base_output_dir <- "initial_descriptive_analysis/output/sample_description"

csv_dir <- file.path(base_output_dir, "csv")
plots_dir <- file.path(base_output_dir, "plots")
pdf_dir <- file.path(base_output_dir, "pdf")
logs_dir <- file.path(base_output_dir, "logs")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

# Prefix for all English output files
output_prefix <- "en_"

out_file <- function(directory, filename) {
  file.path(directory, paste0(output_prefix, filename))
}

# LOAD DATA

df <- read_csv(
  file.path(base_input_dir, "df_clean_sociodemographic.csv"),
  show_col_types = FALSE
)

cat("Dataset loaded: df_clean_sociodemographic.csv\n")
cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n")

# GENERAL HELPER FUNCTIONS


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

coalesce_optional_cols <- function(data, candidates) {
  existing <- candidates[candidates %in% names(data)]
  
  if (length(existing) == 0) {
    return(rep(NA_character_, nrow(data)))
  }
  
  values <- lapply(existing, function(col) as.character(data[[col]]))
  do.call(coalesce, values)
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

# CREATE PARTICIPANT ID
df <- df %>%
  mutate(
    participant_id = coalesce(
      as.character(get_optional_col(., "participant_id")),
      as.character(get_optional_col(., "join_key")),
      as.character(get_optional_col(., "prolific_id")),
      as.character(get_optional_col(., "identification_code")),
      as.character(row_number())
    )
  )


# COLUMN DEFINITIONS

year_of_birth_candidates <- c(
  "year_of_birth_clean",
  "please_enter_your_year_of_birth_final",
  "year_of_birth"
)

age_candidates <- c(
  "age_clean",
  "age"
)

gender_col <- "what_is_your_gender_final"
country_col <- "in_which_country_do_you_currently_live_final"
country_clean_col <- "country_clean"
residence_region_col <- "residence_region"
size_city_col <- "what_is_the_approximate_population_size_of_the_city_where_you_live_final"
climate_zone_col <- "in_which_climate_zone_do_you_live_please_select_the_option_that_corresponds_to_your_location_if_you_re_not_sure_which_climate_zone_you_live_in_please_refer_to_the_map_below_or_select_the_option_that_best_matches_your_region_final"
employment_col <- "what_is_your_current_employment_status_please_indicate_your_main_contractual_status_if_you_are_temporarily_on_leave_such_as_sick_leave_parental_leave_or_a_temporary_reduction_in_working_hours_please_report_your_usual_employment_status_final"
education_level_col <- "what_is_the_highest_level_of_education_that_you_have_completed_if_you_are_currently_studying_and_have_not_yet_completed_a_level_please_select_the_last_level_you_have_finished_final"
work_home_col <- "do_you_currently_work_or_study_from_home_final"
type_house_col <- "what_type_of_household_do_you_live_in_please_select_the_option_that_best_describes_your_household_final"
tenure_col <- "what_is_the_current_tenure_status_of_your_home_final"
political_col <- "on_a_scale_from_0_to_100_where_0_means_most_left_and_100_means_most_right_where_would_you_place_yourself_politically_final"
vote_col <- "which_of_the_following_best_describes_your_general_approach_to_voting_in_elections_final"

# SOCIODEMOGRAPHIC CLEANING FUNCTIONS

clean_gender <- function(x) {
  x <- clean_text_basic(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("^male", ignore_case = TRUE)) ~ "Male",
    str_detect(x, regex("^female", ignore_case = TRUE)) ~ "Female",
    TRUE ~ "Other / Prefer not to say"
  )
}

clean_country <- function(x) {
  x <- clean_text_basic(x)
  x <- str_remove(x, "\\s*\\(ID[0-9]+\\)$")
  x <- str_remove(x, "^[A-Z]{2}\\s*[–-]\\s*")
  
  x <- case_when(
    is.na(x) | x == "" ~ NA_character_,
    x %in% c("United Kingdom *", "UK", "Great Britain", "United Kingdom") ~ "United Kingdom",
    x %in% c("Czechia", "Czech Republic") ~ "Czech Republic",
    x %in% c("Moldova (Republic of Moldova)", "Moldova") ~ "Moldova",
    x %in% c("Russia", "Russian Federation") ~ "Russia",
    TRUE ~ x
  )
  
  country_map <- c(
    "Albania" = "Albania",
    "Andorra" = "Andorra",
    "Austria" = "Austria",
    "Belarus" = "Belarus",
    "Bielorrusia" = "Belarus",
    "Belgium" = "Belgium",
    "Bélgica" = "Belgium",
    "Bosnia and Herzegovina" = "Bosnia and Herzegovina",
    "Bosnia y Herzegovina" = "Bosnia and Herzegovina",
    "Bulgaria" = "Bulgaria",
    "Croatia" = "Croatia",
    "Croacia" = "Croatia",
    "Cyprus" = "Cyprus",
    "Chipre" = "Cyprus",
    "Czech Republic" = "Czech Republic",
    "República Checa" = "Czech Republic",
    "Denmark" = "Denmark",
    "Dinamarca" = "Denmark",
    "Estonia" = "Estonia",
    "Finland" = "Finland",
    "Finlandia" = "Finland",
    "France" = "France",
    "Francia" = "France",
    "Germany" = "Germany",
    "Alemania" = "Germany",
    "Greece" = "Greece",
    "Grecia" = "Greece",
    "Hungary" = "Hungary",
    "Hungría" = "Hungary",
    "Iceland" = "Iceland",
    "Islandia" = "Iceland",
    "Ireland" = "Ireland",
    "Irlanda" = "Ireland",
    "Italy" = "Italy",
    "Italia" = "Italy",
    "Latvia" = "Latvia",
    "Letonia" = "Latvia",
    "Liechtenstein" = "Liechtenstein",
    "Lithuania" = "Lithuania",
    "Lituania" = "Lithuania",
    "Luxembourg" = "Luxembourg",
    "Luxemburgo" = "Luxembourg",
    "Malta" = "Malta",
    "Moldova" = "Moldova",
    "Moldavia" = "Moldova",
    "Monaco" = "Monaco",
    "Mónaco" = "Monaco",
    "Montenegro" = "Montenegro",
    "Netherlands" = "Netherlands",
    "Países Bajos" = "Netherlands",
    "North Macedonia" = "North Macedonia",
    "Macedonia del Norte" = "North Macedonia",
    "Norway" = "Norway",
    "Noruega" = "Norway",
    "Poland" = "Poland",
    "Polonia" = "Poland",
    "Portugal" = "Portugal",
    "Romania" = "Romania",
    "Rumanía" = "Romania",
    "Russia" = "Russia",
    "Rusia" = "Russia",
    "San Marino" = "San Marino",
    "Serbia" = "Serbia",
    "Slovakia" = "Slovakia",
    "Eslovaquia" = "Slovakia",
    "Slovenia" = "Slovenia",
    "Eslovenia" = "Slovenia",
    "Spain" = "Spain",
    "España" = "Spain",
    "Sweden" = "Sweden",
    "Suecia" = "Sweden",
    "Switzerland" = "Switzerland",
    "Suiza" = "Switzerland",
    "Ukraine" = "Ukraine",
    "Ucrania" = "Ukraine",
    "United Kingdom" = "United Kingdom",
    "Reino Unido" = "United Kingdom"
  )
  
  recode(x, !!!country_map, .default = x)
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
  x <- clean_text_basic(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("^Village", ignore_case = TRUE)) ~ "Village or rural area (<1,000 inhabitants)",
    str_detect(x, regex("^Small town", ignore_case = TRUE)) ~ "Small town (1,000-10,000 inhabitants)",
    str_detect(x, regex("^Town", ignore_case = TRUE)) ~ "Town (10,000-50,000 inhabitants)",
    str_detect(x, regex("^Small city", ignore_case = TRUE)) ~ "Small city (50,000-250,000 inhabitants)",
    str_detect(x, regex("^Medium city", ignore_case = TRUE)) ~ "Medium-sized city (250,000-500,000 inhabitants)",
    str_detect(x, regex("^Large city", ignore_case = TRUE)) ~ "Large city (500,000-1,000,000 inhabitants)",
    str_detect(x, regex("^Global city", ignore_case = TRUE)) ~ "Metropolis (>1,000,000 inhabitants)",
    TRUE ~ NA_character_
  )
}

clean_climate_zone <- function(x) {
  x <- clean_text_basic(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("Subtropical", ignore_case = TRUE)) ~ "Warm/Mediterranean climate",
    str_detect(x, regex("Temperate Climates - Maritime|Temperate Climates - Transitional|Temperate Climates - Intermediate|Temperate Climates - Continental", ignore_case = TRUE)) ~ "Temperate climate",
    str_detect(x, regex("Cold Climates|Circumpolar", ignore_case = TRUE)) ~ "Cold/Polar climate",
    TRUE ~ NA_character_
  )
}

clean_employment <- function(x) {
  x <- clean_text_basic(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("Full time employed|Part-time employed|Self-employed", ignore_case = TRUE)) ~ "Employed",
    str_detect(x, regex("Student", ignore_case = TRUE)) ~ "Student",
    str_detect(x, regex("Unemployed|Retired|Stay-at-home", ignore_case = TRUE)) ~ "Other employment status",
    TRUE ~ "Other employment status"
  )
}

clean_education <- function(x) {
  x <- clean_text_basic(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("University", ignore_case = TRUE)) ~ "University education",
    str_detect(x, regex("Primary|Secondary|Vocational", ignore_case = TRUE)) ~ "Non-university education",
    TRUE ~ NA_character_
  )
}

clean_work_home <- function(x) {
  x <- clean_text_basic(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("^Yes", ignore_case = TRUE)) ~ "Yes",
    str_detect(x, regex("^No", ignore_case = TRUE)) ~ "No",
    TRUE ~ NA_character_
  )
}

clean_type_house <- function(x) {
  x <- clean_text_basic(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("Dual Income No Kids|Ageing family", ignore_case = TRUE)) ~ "Couple without children",
    str_detect(x, regex("Traditional family|Large family|Single parenthood", ignore_case = TRUE)) ~ "Household with children",
    str_detect(x, regex("Uni-personal|Poly-nuclear", ignore_case = TRUE)) ~ "Other",
    TRUE ~ "Other"
  )
}

clean_tenure <- function(x) {
  x <- clean_text_basic(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("own the home outright|fully paid-off", ignore_case = TRUE)) ~ "Homeowner without mortgage",
    str_detect(x, regex("mortgage|outstanding payments", ignore_case = TRUE)) ~ "Homeowner with mortgage",
    str_detect(x, regex("rent|rental", ignore_case = TRUE)) ~ "Non-homeowner",
    TRUE ~ "Non-homeowner"
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

clean_vote <- function(x) {
  x <- clean_text_basic(x)
  x <- str_remove(x, "\\s*\\(ID[0-9]+\\)$")
  
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("pro-independence|regionalist", ignore_case = TRUE)) ~ "Regionalist/independence parties",
    str_detect(x, regex("candidate/program|type of election", ignore_case = TRUE)) ~ "Variable vote depending on candidate/programme",
    str_detect(x, regex("do not vote|blank|null|abstain", ignore_case = TRUE)) ~ "Does not vote / blank / null vote",
    str_detect(x, regex("national parties", ignore_case = TRUE)) ~ "National parties",
    str_detect(x, regex("other options", ignore_case = TRUE)) ~ "Other options",
    TRUE ~ "Other response"
  )
}

clean_source_survey <- function(x) {
  x <- clean_text_basic(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    x == "decision" ~ "Decision survey",
    x == "concerns1" ~ "Concerns survey",
    x == "rvs" ~ "RVS survey",
    TRUE ~ x
  )
}

clean_identifier_type <- function(x) {
  x <- clean_text_basic(x)
  
  case_when(
    is.na(x) ~ NA_character_,
    x == "prolific" ~ "Prolific ID",
    x == "codigo" ~ "Code",
    x == "prolific_y_codigo" ~ "Prolific ID and code",
    x == "sin_identificador" ~ "No identifier",
    TRUE ~ x
  )
}


# CLEAN AGE AND GENERATION


project_year <- 2026

year_of_birth_raw <- suppressWarnings(
  as.numeric(coalesce_optional_cols(df, year_of_birth_candidates))
)

age_raw <- suppressWarnings(
  as.numeric(coalesce_optional_cols(df, age_candidates))
)

df <- df %>%
  mutate(
    year_of_birth_raw = year_of_birth_raw,
    age_raw = age_raw,
    
    year_of_birth_clean = case_when(
      !is.na(year_of_birth_raw) & year_of_birth_raw >= 1900 & year_of_birth_raw <= 2007 ~ year_of_birth_raw,
      !is.na(year_of_birth_raw) & year_of_birth_raw >= 18 & year_of_birth_raw <= 100 ~ project_year - year_of_birth_raw,
      !is.na(age_raw) & age_raw >= 18 & age_raw <= 100 ~ project_year - age_raw,
      TRUE ~ NA_real_
    ),
    
    age_clean = case_when(
      !is.na(year_of_birth_clean) ~ project_year - year_of_birth_clean,
      !is.na(age_raw) & age_raw >= 18 & age_raw <= 100 ~ age_raw,
      TRUE ~ NA_real_
    ),
    
    age_group = case_when(
      year_of_birth_clean >= 2001 & year_of_birth_clean <= 2007 ~ "Generation Z",
      year_of_birth_clean >= 1986 & year_of_birth_clean <= 2000 ~ "Millennials",
      year_of_birth_clean >= 1971 & year_of_birth_clean <= 1985 ~ "Generation X",
      year_of_birth_clean >= 1932 & year_of_birth_clean <= 1970 ~ "Boomers +",
      TRUE ~ NA_character_
    )
  )

# CREATE SAMPLE DESCRIPTION TABLE

country_raw <- get_optional_col(df, country_col)
country_clean_from_dataset <- get_optional_col(df, country_clean_col)

sample_description <- df %>%
  transmute(
    participant_id,
    
    year_of_birth = year_of_birth_clean,
    age = age_clean,
    age_group,
    
    gender = clean_gender(get_optional_col(., gender_col)),
    
    country = coalesce(
      clean_country(country_clean_from_dataset),
      clean_country(country_raw)
    ),
    
    residence_region = clean_residence_region(country),
    
    city_size = clean_city_size(get_optional_col(., size_city_col)),
    climate_zone = clean_climate_zone(get_optional_col(., climate_zone_col)),
    employment = clean_employment(get_optional_col(., employment_col)),
    education_group = clean_education(get_optional_col(., education_level_col)),
    work_home = clean_work_home(get_optional_col(., work_home_col)),
    type_house = clean_type_house(get_optional_col(., type_house_col)),
    tenure = clean_tenure(get_optional_col(., tenure_col)),
    political_orientation = clean_political_orientation(get_optional_col(., political_col)),
    vote_type = clean_vote(get_optional_col(., vote_col)),
    
    source_survey = clean_source_survey(get_optional_col(., "source_survey")),
    identifier_type = clean_identifier_type(get_optional_col(., "identifier_type")),
    row_quality = get_optional_col(., "row_quality")
  ) %>%
  distinct(participant_id, .keep_all = TRUE)

write_csv(
  sample_description,
  out_file(csv_dir, "sample_description.csv")
)

cat("Unique participants in sample_description:", nrow(sample_description), "\n")

# NATURAL ORDERS AND COLOUR PALETTES

natural_orders <- list(
  age_group = c(
    "Generation Z",
    "Millennials",
    "Generation X",
    "Boomers +"
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
    "Village or rural area (<1,000 inhabitants)",
    "Small town (1,000-10,000 inhabitants)",
    "Town (10,000-50,000 inhabitants)",
    "Small city (50,000-250,000 inhabitants)",
    "Medium-sized city (250,000-500,000 inhabitants)",
    "Large city (500,000-1,000,000 inhabitants)",
    "Metropolis (>1,000,000 inhabitants)"
  ),
  climate_zone = c(
    "Warm/Mediterranean climate",
    "Temperate climate",
    "Cold/Polar climate"
  ),
  employment = c(
    "Employed",
    "Student",
    "Other employment status"
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
    "Couple without children",
    "Household with children",
    "Other"
  ),
  tenure = c(
    "Homeowner without mortgage",
    "Homeowner with mortgage",
    "Non-homeowner"
  ),
  political_orientation = c(
    "Far left",
    "Left",
    "Centre",
    "Right",
    "Far right"
  ),
  vote_type = c(
    "Regionalist/independence parties",
    "Variable vote depending on candidate/programme",
    "National parties",
    "Other options",
    "Does not vote / blank / null vote",
    "Other response"
  ),
  identifier_type = c(
    "Prolific ID",
    "Code",
    "Prolific ID and code",
    "No identifier"
  ),
  source_survey = c(
    "Decision survey",
    "Concerns survey",
    "RVS survey"
  )
)

sample_colors <- list(
  age_group = c(
    "Generation Z" = "#56B4E9",
    "Millennials" = "#009E73",
    "Generation X" = "#E69F00",
    "Boomers +" = "#CC79A7"
  ),
  gender = c(
    "Female" = "#009E73",
    "Male" = "#0072B2",
    "Other / Prefer not to say" = "#999999"
  ),
  residence_region = c(
    "Northern Europe" = "#56B4E9",
    "Western Europe" = "#009E73",
    "Southern Europe" = "#E69F00",
    "Eastern Europe" = "#D55E00",
    "Other region" = "#999999"
  )
)

default_sample_colors <- c(
  "#0072B2", "#56B4E9", "#009E73", "#E69F00",
  "#D55E00", "#CC79A7", "#F0E442", "#999999"
)

get_sample_colors <- function(variable_name, categories) {
  categories <- as.character(categories)
  
  if (variable_name %in% names(sample_colors)) {
    colors <- sample_colors[[variable_name]]
    colors <- colors[names(colors) %in% categories]
    
    missing_categories <- setdiff(categories, names(colors))
    
    if (length(missing_categories) > 0) {
      extra_colors <- rep(default_sample_colors, length.out = length(missing_categories))
      names(extra_colors) <- missing_categories
      colors <- c(colors, extra_colors)
    }
    
    return(colors)
  }
  
  colors <- rep(default_sample_colors, length.out = length(categories))
  names(colors) <- categories
  colors
}


# VISUAL CONFIGURATION

plot_base_size <- 16
plot_title_size <- 19
plot_subtitle_size <- 13
plot_axis_title_size <- 15
plot_axis_text_x_size <- 15
plot_axis_text_y_size <- 15
plot_label_size <- 4.5

theme_sample_tfm <- function() {
  theme_minimal(base_size = plot_base_size) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = plot_title_size
      ),
      plot.subtitle = element_text(
        size = plot_subtitle_size
      ),
      axis.title.x = element_text(
        size = plot_axis_title_size,
        margin = margin(t = 8)
      ),
      axis.title.y = element_text(
        size = plot_axis_title_size,
        margin = margin(r = 8)
      ),
      axis.text.x = element_text(
        size = plot_axis_text_x_size
      ),
      axis.text.y = element_text(
        size = plot_axis_text_y_size
      ),
      panel.grid.minor = element_blank(),
      plot.margin = margin(12, 80, 12, 12)
    )
}

# TABLE AND PLOT FUNCTIONS

get_variable_order <- function(variable_name, data_summary) {
  if (variable_name %in% names(natural_orders)) {
    return(natural_orders[[variable_name]])
  }
  
  data_summary %>%
    arrange(desc(n)) %>%
    pull(category) %>%
    as.character()
}

create_summary_table <- function(data, variable_name, variable_label) {
  data %>%
    filter(!is.na(.data[[variable_name]])) %>%
    count(category = .data[[variable_name]], name = "n") %>%
    mutate(
      variable = variable_label,
      percentage = round(n / sum(n) * 100, 1)
    ) %>%
    select(variable, category, n, percentage)
}

plot_sample_variable <- function(data, variable_name, variable_label, width = 9, height = 5) {
  summary_data <- create_summary_table(data, variable_name, variable_label)
  
  if (nrow(summary_data) == 0) {
    message("No data for variable: ", variable_name)
    return(NULL)
  }
  
  n_valid <- sum(summary_data$n)
  n_total <- nrow(data)
  n_missing <- n_total - n_valid
  
  variable_order <- get_variable_order(variable_name, summary_data)
  variable_order <- variable_order[variable_order %in% summary_data$category]
  
  colors <- get_sample_colors(variable_name, summary_data$category)
  
  plot_data <- summary_data %>%
    mutate(
      category = factor(category, levels = rev(variable_order)),
      label = paste0(n, " (", percentage, "%)")
    )
  
  p <- ggplot(plot_data, aes(x = category, y = n, fill = category)) +
    geom_col(color = "#2C3E50") +
    geom_text(
      aes(label = label),
      hjust = -0.1,
      size = plot_label_size
    ) +
    coord_flip(clip = "off") +
    scale_x_discrete(
      labels = function(x) str_wrap(x, width = 32)
    ) +
    scale_fill_manual(values = colors, drop = FALSE) +
    guides(fill = "none") +
    scale_y_continuous(
      limits = c(0, max(plot_data$n) * 1.30)
    ) +
    labs(
      title = paste("Sample distribution by", str_to_lower(variable_label)),
      subtitle = paste0(
        "valid n = ", n_valid,
        "; missing n = ", n_missing,
        "; total n = ", n_total
      ),
      x = NULL,
      y = "Number of participants"
    ) +
    theme_sample_tfm()
  
  print(p)
  
  filename <- paste0("sample_description_", clean_filename(variable_name))
  
  ggsave(
    out_file(plots_dir, paste0(filename, ".png")),
    p,
    width = width,
    height = height,
    dpi = 300
  )
  
  ggsave(
    out_file(pdf_dir, paste0(filename, ".pdf")),
    p,
    width = width,
    height = height
  )
  
  write_csv(
    summary_data,
    out_file(csv_dir, paste0(filename, ".csv"))
  )
  
  return(p)
}

plot_sample_variable_percentage <- function(data, variable_name, variable_label, width = 9, height = 5) {
  summary_data <- create_summary_table(data, variable_name, variable_label)
  
  if (nrow(summary_data) == 0) {
    message("No data for variable: ", variable_name)
    return(NULL)
  }
  
  n_valid <- sum(summary_data$n)
  n_total <- nrow(data)
  n_missing <- n_total - n_valid
  
  variable_order <- get_variable_order(variable_name, summary_data)
  variable_order <- variable_order[variable_order %in% summary_data$category]
  
  colors <- get_sample_colors(variable_name, summary_data$category)
  
  plot_data <- summary_data %>%
    mutate(
      category = factor(category, levels = rev(variable_order)),
      label = paste0(percentage, "% (n=", n, ")")
    )
  
  p <- ggplot(plot_data, aes(x = category, y = percentage, fill = category)) +
    geom_col(color = "#2C3E50") +
    geom_text(
      aes(label = label),
      hjust = -0.1,
      size = plot_label_size
    ) +
    coord_flip(clip = "off") +
    scale_x_discrete(
      labels = function(x) str_wrap(x, width = 32)
    ) +
    scale_fill_manual(values = colors, drop = FALSE) +
    guides(fill = "none") +
    scale_y_continuous(
      limits = c(0, max(plot_data$percentage) * 1.30),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = paste("Percentage distribution of the sample by", str_to_lower(variable_label)),
      subtitle = paste0(
        "valid n = ", n_valid,
        "; missing n = ", n_missing,
        "; total n = ", n_total
      ),
      x = NULL,
      y = "Percentage of participants"
    ) +
    theme_sample_tfm()
  
  print(p)
  
  filename <- paste0("sample_description_", clean_filename(variable_name), "_percentage")
  
  ggsave(
    out_file(plots_dir, paste0(filename, ".png")),
    p,
    width = width,
    height = height,
    dpi = 300
  )
  
  ggsave(
    out_file(pdf_dir, paste0(filename, ".pdf")),
    p,
    width = width,
    height = height
  )
  
  return(p)
}

plot_sample_variable_ordered <- function(data, variable_name, variable_label, width = 9, height = 5) {
  summary_data <- create_summary_table(data, variable_name, variable_label)
  
  if (nrow(summary_data) == 0) {
    message("No data for variable: ", variable_name)
    return(NULL)
  }
  
  n_valid <- sum(summary_data$n)
  n_total <- nrow(data)
  n_missing <- n_total - n_valid
  
  colors <- get_sample_colors(variable_name, summary_data$category)
  
  plot_data <- summary_data %>%
    arrange(n) %>%
    mutate(
      category = factor(category, levels = category),
      label = paste0(n, " (", percentage, "%)")
    )
  
  p <- ggplot(plot_data, aes(x = category, y = n, fill = category)) +
    geom_col(color = "#2C3E50") +
    geom_text(
      aes(label = label),
      hjust = -0.1,
      size = plot_label_size
    ) +
    coord_flip(clip = "off") +
    scale_x_discrete(
      labels = function(x) str_wrap(x, width = 32)
    ) +
    scale_fill_manual(values = colors, drop = FALSE) +
    guides(fill = "none") +
    scale_y_continuous(
      limits = c(0, max(plot_data$n) * 1.30)
    ) +
    labs(
      title = paste("Sample distribution by", str_to_lower(variable_label), "- ordered"),
      subtitle = paste0(
        "valid n = ", n_valid,
        "; missing n = ", n_missing,
        "; total n = ", n_total
      ),
      x = NULL,
      y = "Number of participants"
    ) +
    theme_sample_tfm()
  
  print(p)
  
  filename <- paste0("sample_description_", clean_filename(variable_name), "_ordered")
  
  ggsave(
    out_file(plots_dir, paste0(filename, ".png")),
    p,
    width = width,
    height = height,
    dpi = 300
  )
  
  ggsave(
    out_file(pdf_dir, paste0(filename, ".pdf")),
    p,
    width = width,
    height = height
  )
  
  return(p)
}

# VARIABLES TO DESCRIBE

sample_variables <- tibble(
  variable_name = c(
    "age_group",
    "gender",
    "country",
    "residence_region",
    "city_size",
    "climate_zone",
    "employment",
    "education_group",
    "work_home",
    "type_house",
    "tenure",
    "political_orientation",
    "vote_type",
    "source_survey",
    "identifier_type"
  ),
  variable_label = c(
    "generation",
    "gender",
    "country",
    "European region",
    "city size",
    "climate zone",
    "employment status",
    "education level",
    "work/study from home",
    "household type",
    "tenure status",
    "political orientation",
    "voting type",
    "source survey",
    "identifier type"
  ),
  width = c(
    9, 8, 11, 9, 9,
    9, 9, 8, 8, 9,
    9, 9, 11, 8, 8
  ),
  height = c(
    5.5, 5.5, 7.5, 5.5, 5.5,
    5.5, 5.5, 5.5, 5.5, 5.5,
    5.5, 5.5, 6, 5.5, 5.5
  )
)

write_csv(
  sample_variables,
  out_file(csv_dir, "sample_description_variables_used.csv")
)

# SUMMARY TABLES

summary_sample_all <- sample_variables %>%
  rowwise() %>%
  do(
    create_summary_table(
      data = sample_description,
      variable_name = .$variable_name,
      variable_label = .$variable_label
    )
  ) %>%
  ungroup()

write_csv(
  summary_sample_all,
  out_file(csv_dir, "summary_sample_description_all_variables.csv")
)

print(summary_sample_all, n = Inf)


# CATEGORICAL PLOTS


sample_plots <- list()

for (i in seq_len(nrow(sample_variables))) {
  
  variable_name_i <- sample_variables$variable_name[i]
  variable_label_i <- sample_variables$variable_label[i]
  width_i <- sample_variables$width[i]
  height_i <- sample_variables$height[i]
  
  sample_plots[[paste0(variable_name_i, "_n")]] <- plot_sample_variable(
    sample_description,
    variable_name_i,
    variable_label_i,
    width_i,
    height_i
  )
  
  sample_plots[[paste0(variable_name_i, "_percentage")]] <- plot_sample_variable_percentage(
    sample_description,
    variable_name_i,
    variable_label_i,
    width_i,
    height_i
  )
  
  sample_plots[[paste0(variable_name_i, "_ordered")]] <- plot_sample_variable_ordered(
    sample_description,
    variable_name_i,
    variable_label_i,
    width_i,
    height_i
  )
}

sample_plots <- sample_plots[!sapply(sample_plots, is.null)]

# NUMERICAL AGE SUMMARY AND HISTOGRAM


age_summary <- sample_description %>%
  summarise(
    n_valid = sum(!is.na(age)),
    mean_age = round(mean(age, na.rm = TRUE), 2),
    median_age = round(median(age, na.rm = TRUE), 2),
    sd_age = round(sd(age, na.rm = TRUE), 2),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE)
  )

write_csv(
  age_summary,
  out_file(csv_dir, "sample_age_numeric_summary.csv")
)

print(age_summary)

if (sum(!is.na(sample_description$age)) > 0) {
  
  plot_age_numeric <- sample_description %>%
    filter(!is.na(age)) %>%
    ggplot(aes(x = age)) +
    geom_histogram(
      bins = 15,
      fill = "#56B4E9",
      color = "#2C3E50"
    ) +
    labs(
      title = "Age distribution of the sample",
      subtitle = paste0(
        "valid n = ", sum(!is.na(sample_description$age)),
        "; total n = ", nrow(sample_description)
      ),
      x = "Age",
      y = "Number of participants"
    ) +
    theme_sample_tfm()
  
  print(plot_age_numeric)
  
  ggsave(
    out_file(plots_dir, "sample_description_age_numeric.png"),
    plot_age_numeric,
    width = 9,
    height = 5.5,
    dpi = 300
  )
  
  ggsave(
    out_file(pdf_dir, "sample_description_age_numeric.pdf"),
    plot_age_numeric,
    width = 9,
    height = 5.5
  )
  
  sample_plots[["age_numeric"]] <- plot_age_numeric
}


# MISSING VALUES BY VARIABLE

missing_sample_description <- sample_description %>%
  summarise(across(-participant_id, ~ sum(is.na(.x)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "n_missing"
  ) %>%
  mutate(
    n_total = nrow(sample_description),
    n_valid = n_total - n_missing,
    percentage_missing = round(n_missing / n_total * 100, 1)
  ) %>%
  arrange(desc(n_missing))

write_csv(
  missing_sample_description,
  out_file(csv_dir, "sample_description_missing_by_variable.csv")
)

print(missing_sample_description, n = Inf)


# SOCIODEMOGRAPHIC TABLE BY SELF-CLASSIFIED ARCHETYPE

df_general_self <- read_csv(
  file.path(base_input_dir, "df_clean_general.csv"),
  show_col_types = FALSE
)

add_participant_id_safe <- function(data) {
  data %>%
    mutate(
      participant_id = coalesce(
        if ("participant_id" %in% names(.)) as.character(.data[["participant_id"]]) else NA_character_,
        if ("join_key" %in% names(.)) as.character(.data[["join_key"]]) else NA_character_,
        if ("prolific_id" %in% names(.)) as.character(.data[["prolific_id"]]) else NA_character_,
        if ("identification_code" %in% names(.)) as.character(.data[["identification_code"]]) else NA_character_,
        as.character(row_number())
      )
    )
}

df_general_self <- add_participant_id_safe(df_general_self)

self_col_candidates <- c(
  "which_statement_best_describes_you_when_making_an_investment_decision_related_to_your_household_final",
  "which_statement_best_describes_you_when_making_an_investment_decision_related_to_your_household"
)

self_col <- intersect(self_col_candidates, names(df_general_self))[1]

if (is.na(self_col)) {
  stop("Self-classification column not found in df_clean_general.csv")
}

self_profiles <- df_general_self %>%
  transmute(
    participant_id = as.character(participant_id),
    self_response_raw = str_squish(as.character(.data[[self_col]])),
    self_response_raw = na_if(self_response_raw, ""),
    self_profile = case_when(
      is.na(self_response_raw) ~ "Missing",
      str_detect(self_response_raw, regex("environmental impact", ignore_case = TRUE)) ~ "Activist",
      str_detect(self_response_raw, regex("safety", ignore_case = TRUE)) ~ "Fearful",
      str_detect(self_response_raw, regex("social status", ignore_case = TRUE)) ~ "Influencer",
      str_detect(self_response_raw, regex("comfort", ignore_case = TRUE)) ~ "Careful",
      str_detect(self_response_raw, regex("not very interested", ignore_case = TRUE)) ~ "Uninterested",
      str_detect(self_response_raw, regex("early adopter", ignore_case = TRUE)) ~ "Early adopter",
      str_detect(self_response_raw, regex("ethical", ignore_case = TRUE)) ~ "Sentient",
      str_detect(self_response_raw, regex("cost-effective", ignore_case = TRUE)) ~ "Homo economicus",
      str_detect(self_response_raw, regex("[NΝ]one of the above", ignore_case = TRUE)) ~ "Unclassified",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!self_profile %in% c("Missing", "Unclassified", "Other")) %>%
  distinct(participant_id, .keep_all = TRUE)

archetype_order <- c(
  "Careful",
  "Activist",
  "Fearful",
  "Homo economicus",
  "Sentient",
  "Influencer",
  "Uninterested",
  "Early adopter"
)

sample_description_archetype <- sample_description %>%
  mutate(participant_id = as.character(participant_id)) %>%
  left_join(self_profiles, by = "participant_id") %>%
  mutate(
    self_profile = factor(self_profile, levels = archetype_order),
    
    country_group = case_when(
      is.na(country) ~ NA_character_,
      country == "Spain" ~ "Spain",
      TRUE ~ "Other European country"
    ),
    
    climate_zone_3 = case_when(
      is.na(climate_zone) ~ NA_character_,
      climate_zone %in% c(
        "Warm/Mediterranean climate",
        "Temperate climate",
        "Cold/Polar climate"
      ) ~ climate_zone,
      TRUE ~ "Other climate"
    ),
    
    employment_3 = case_when(
      is.na(employment) ~ NA_character_,
      employment == "Employed" ~ "Employed",
      employment == "Student" ~ "Student",
      TRUE ~ "Other employment status"
    )
  )

write_csv(
  sample_description_archetype,
  out_file(csv_dir, "sample_description_with_self_profile.csv")
)

cat("Base table with archetypes created.\n")
cat("Rows:", nrow(sample_description_archetype), "\n")
cat("With valid archetype:", sum(!is.na(sample_description_archetype$self_profile)), "\n")


# LONG TABLE WITH TOTAL + ARCHETYPES

profile_columns <- c("Total", archetype_order)

table_data_long <- bind_rows(
  sample_description_archetype %>%
    mutate(profile_group = "Total"),
  
  sample_description_archetype %>%
    filter(!is.na(self_profile)) %>%
    mutate(profile_group = as.character(self_profile))
) %>%
  mutate(
    profile_group = factor(profile_group, levels = profile_columns)
  )

format_n_pct <- function(n, denom) {
  n <- ifelse(is.na(n), 0, n)
  
  if (is.na(denom) || denom == 0) {
    return("")
  }
  
  paste0(n, " (", sprintf("%.1f", 100 * n / denom), "%)")
}

format_mean_sd <- function(mean_value, sd_value, n_valid) {
  if (is.na(n_valid) || n_valid == 0 || is.na(mean_value)) {
    return("")
  }
  
  if (is.na(sd_value)) {
    return(paste0(sprintf("%.1f", mean_value), " (NA)"))
  }
  
  paste0(sprintf("%.1f", mean_value), " (", sprintf("%.1f", sd_value), ")")
}

make_n_rows <- function(data) {
  data %>%
    mutate(profile_group = as.character(profile_group)) %>%
    distinct(profile_group, participant_id) %>%
    count(profile_group, name = "n") %>%
    complete(profile_group = profile_columns, fill = list(n = 0)) %>%
    mutate(
      section_order = 1,
      row_order = 1,
      row_type = "data",
      characteristic = "N participants",
      value = as.character(n)
    ) %>%
    select(section_order, row_order, row_type, characteristic, profile_group, value)
}

make_age_rows <- function(data) {
  data %>%
    mutate(profile_group = as.character(profile_group)) %>%
    group_by(profile_group) %>%
    summarise(
      n_valid = sum(!is.na(age)),
      mean_age = mean(age, na.rm = TRUE),
      sd_age = sd(age, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    complete(profile_group = profile_columns) %>%
    mutate(
      section_order = 2,
      row_order = 1,
      row_type = "data",
      characteristic = "Age, years (mean (SD))",
      value = mapply(format_mean_sd, mean_age, sd_age, n_valid)
    ) %>%
    select(section_order, row_order, row_type, characteristic, profile_group, value)
}

make_categorical_section <- function(data, var_name, label, levels_vec, section_order_value) {
  
  data <- data %>%
    mutate(profile_group = as.character(profile_group))
  
  header <- tibble(
    section_order = section_order_value,
    row_order = 0,
    row_type = "header",
    characteristic = label,
    profile_group = profile_columns,
    value = ""
  )
  
  denoms <- data %>%
    filter(!is.na(.data[[var_name]])) %>%
    count(profile_group, name = "denom")
  
  counts <- data %>%
    filter(!is.na(.data[[var_name]])) %>%
    mutate(category = as.character(.data[[var_name]])) %>%
    count(profile_group, category, name = "n")
  
  rows <- expand_grid(
    row_order = seq_along(levels_vec),
    category = levels_vec,
    profile_group = profile_columns
  ) %>%
    mutate(
      characteristic = paste0("  ", category)
    ) %>%
    left_join(counts, by = c("profile_group", "category")) %>%
    left_join(denoms, by = "profile_group") %>%
    mutate(
      section_order = section_order_value,
      row_type = "data",
      n = ifelse(is.na(n), 0, n),
      value = mapply(format_n_pct, n, denom)
    ) %>%
    select(section_order, row_order, row_type, characteristic, profile_group, value)
  
  bind_rows(header, rows)
}

table_blocks <- list(
  list(
    var = "age_group",
    label = "Generation",
    levels = c(
      "Generation Z",
      "Millennials",
      "Generation X",
      "Boomers +"
    )
  ),
  list(
    var = "gender",
    label = "Gender",
    levels = c("Female", "Male", "Other / Prefer not to say")
  ),
  list(
    var = "country_group",
    label = "Country of residence",
    levels = c("Spain", "Other European country")
  ),
  list(
    var = "residence_region",
    label = "European region",
    levels = c(
      "Northern Europe",
      "Western Europe",
      "Southern Europe",
      "Eastern Europe",
      "Other region"
    )
  ),
  list(
    var = "education_group",
    label = "Education level",
    levels = c("Non-university education", "University education")
  ),
  list(
    var = "employment_3",
    label = "Employment status",
    levels = c("Employed", "Student", "Other employment status")
  ),
  list(
    var = "work_home",
    label = "Work/study from home",
    levels = c("Yes", "No")
  ),
  list(
    var = "type_house",
    label = "Household type",
    levels = c(
      "Couple without children",
      "Household with children",
      "Other"
    )
  ),
  list(
    var = "tenure",
    label = "Tenure status",
    levels = c(
      "Homeowner without mortgage",
      "Homeowner with mortgage",
      "Non-homeowner"
    )
  ),
  list(
    var = "climate_zone_3",
    label = "Climate zone",
    levels = c(
      "Warm/Mediterranean climate",
      "Temperate climate",
      "Cold/Polar climate",
      "Other climate"
    )
  ),
  list(
    var = "political_orientation",
    label = "Political orientation",
    levels = c(
      "Far left",
      "Left",
      "Centre",
      "Right",
      "Far right"
    )
  )
)

table_long_parts <- list(
  make_n_rows(table_data_long),
  make_age_rows(table_data_long)
)

for (i in seq_along(table_blocks)) {
  block <- table_blocks[[i]]
  
  table_long_parts[[length(table_long_parts) + 1]] <- make_categorical_section(
    data = table_data_long,
    var_name = block$var,
    label = block$label,
    levels_vec = block$levels,
    section_order_value = i + 2
  )
}

sociodemographic_by_archetype_long <- bind_rows(table_long_parts) %>%
  mutate(
    profile_group = as.character(profile_group)
  ) %>%
  arrange(section_order, row_order, characteristic, profile_group)

sociodemographic_by_archetype_wide <- sociodemographic_by_archetype_long %>%
  select(section_order, row_order, row_type, characteristic, profile_group, value) %>%
  pivot_wider(
    names_from = profile_group,
    values_from = value,
    values_fill = ""
  ) %>%
  arrange(section_order, row_order) %>%
  select(section_order, row_order, row_type, characteristic, all_of(profile_columns))

sociodemographic_by_archetype_for_word <- sociodemographic_by_archetype_wide %>%
  select(-section_order, -row_order, -row_type)

write_csv(
  sociodemographic_by_archetype_long,
  out_file(csv_dir, "sociodemographic_by_archetype_long.csv")
)

write_csv(
  sociodemographic_by_archetype_wide,
  out_file(csv_dir, "sociodemographic_by_archetype_wide.csv")
)

write_csv(
  sociodemographic_by_archetype_for_word,
  out_file(csv_dir, "sociodemographic_by_archetype_for_word.csv")
)

cat("Archetype table correctly exported as CSV.\n")
cat(out_file(csv_dir, "sociodemographic_by_archetype_for_word.csv"), "\n")

# EXPORT TABLE TO XLSX

if (requireNamespace("openxlsx", quietly = TRUE)) {
  xlsx_path <- out_file(csv_dir, "sociodemographic_by_archetype_for_word.xlsx")
  
  openxlsx::write.xlsx(
    sociodemographic_by_archetype_for_word,
    file = xlsx_path,
    overwrite = TRUE
  )
  
  cat("Table exported as XLSX:\n")
  cat(xlsx_path, "\n")
} else {
  cat("Package openxlsx is not installed. XLSX export skipped.\n")
}

# EXPORT TABLE TO DOCX

if (
  requireNamespace("flextable", quietly = TRUE) &&
  requireNamespace("officer", quietly = TRUE)
) {
  
  table_for_docx <- sociodemographic_by_archetype_wide %>%
    select(-section_order, -row_order)
  
  header_rows <- which(table_for_docx$row_type == "header")
  
  table_for_docx_clean <- table_for_docx %>%
    select(-row_type)
  
  ft <- flextable::flextable(table_for_docx_clean)
  
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::fontsize(ft, size = 7, part = "all")
  ft <- flextable::fontsize(ft, size = 8, part = "header")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::bold(ft, i = header_rows, bold = TRUE, part = "body")
  ft <- flextable::align(ft, j = 2:ncol(table_for_docx_clean), align = "center", part = "all")
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  ft <- flextable::width(ft, j = 1, width = 2.7)
  ft <- flextable::width(ft, j = 2:ncol(table_for_docx_clean), width = 0.9)
  
  ft <- flextable::add_footer_lines(
    ft,
    values = "Note. Percentages are calculated within each column excluding missing values. Profiles with small sample sizes, especially Uninterested and Early adopter, should be interpreted with caution."
  )
  
  section_landscape <- officer::prop_section(
    page_size = officer::page_size(orient = "landscape"),
    page_margins = officer::page_mar(
      bottom = 0.5,
      top = 0.5,
      right = 0.5,
      left = 0.5
    )
  )
  
  docx_path <- out_file(csv_dir, "sociodemographic_by_archetype_for_word.docx")
  
  flextable::save_as_docx(
    "Table 13. Sociodemographic characteristics of the total sample and by self-classified archetype" = ft,
    path = docx_path,
    pr_section = section_landscape
  )
  
  cat("Table exported as DOCX:\n")
  cat(docx_path, "\n")
  
} else {
  cat("Packages flextable/officer are not installed. DOCX export skipped.\n")
}


# POPULATION PYRAMID BY AGE, GENDER AND GENERATION

age_levels_pyramid <- c(
  "19-25",
  "26-30",
  "31-35",
  "36-40",
  "41-45",
  "46-50",
  "51-55",
  "56-60",
  "61-65",
  "66-70",
  "71-75",
  "76-79",
  "80+"
)

generation_levels_pyramid <- c(
  "Generation Z",
  "Millennials",
  "Generation X",
  "Boomers +"
)

generation_colors_pyramid <- c(
  "Generation Z" = "#8EC1B8",
  "Millennials" = "#E8E6A0",
  "Generation X" = "#B4B0D0",
  "Boomers +" = "#E58373"
)

pyramid_counts <- sample_description %>%
  summarise(
    n_total = n(),
    n_valid = sum(!is.na(age) & gender %in% c("Male", "Female")),
    n_missing = n_total - n_valid
  )

n_total_pyramid <- pyramid_counts$n_total[1]
n_valid_pyramid <- pyramid_counts$n_valid[1]
n_missing_pyramid <- pyramid_counts$n_missing[1]

pyramid_data <- sample_description %>%
  filter(
    !is.na(age),
    !is.na(gender),
    gender %in% c("Male", "Female")
  ) %>%
  mutate(
    age_group_pyramid = case_when(
      age >= 19 & age <= 25 ~ "19-25",
      age >= 26 & age <= 30 ~ "26-30",
      age >= 31 & age <= 35 ~ "31-35",
      age >= 36 & age <= 40 ~ "36-40",
      age >= 41 & age <= 45 ~ "41-45",
      age >= 46 & age <= 50 ~ "46-50",
      age >= 51 & age <= 55 ~ "51-55",
      age >= 56 & age <= 60 ~ "56-60",
      age >= 61 & age <= 65 ~ "61-65",
      age >= 66 & age <= 70 ~ "66-70",
      age >= 71 & age <= 75 ~ "71-75",
      age >= 76 & age <= 79 ~ "76-79",
      age >= 80             ~ "80+",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(age_group_pyramid)) %>%
  count(age_group_pyramid, gender, name = "n") %>%
  complete(
    age_group_pyramid = age_levels_pyramid,
    gender = c("Male", "Female"),
    fill = list(n = 0)
  ) %>%
  mutate(
    generation_pyramid = case_when(
      age_group_pyramid == "19-25" ~ "Generation Z",
      age_group_pyramid %in% c("26-30", "31-35", "36-40") ~ "Millennials",
      age_group_pyramid %in% c("41-45", "46-50", "51-55") ~ "Generation X",
      age_group_pyramid %in% c(
        "56-60", "61-65", "66-70", "71-75", "76-79", "80+"
      ) ~ "Boomers +",
      TRUE ~ NA_character_
    ),
    
    age_group_pyramid = factor(
      age_group_pyramid,
      levels = age_levels_pyramid
    ),
    
    generation_pyramid = factor(
      generation_pyramid,
      levels = generation_levels_pyramid
    ),
    
    total_valid = n_valid_pyramid,
    percentage = round(100 * n / total_valid, 1),
    percentage_plot = if_else(gender == "Male", -percentage, percentage)
  )

write_csv(
  pyramid_data %>%
    select(age_group_pyramid, gender, generation_pyramid, n, percentage),
  out_file(csv_dir, "sample_description_population_pyramid_age_gender_generation.csv")
)

max_axis <- max(
  2,
  ceiling(max(abs(pyramid_data$percentage_plot), na.rm = TRUE) / 2) * 2
)

plot_population_pyramid <- ggplot(
  pyramid_data,
  aes(
    x = percentage_plot,
    y = age_group_pyramid,
    fill = generation_pyramid
  )
) +
  geom_col(
    width = 0.82,
    color = "white",
    linewidth = 0.25
  ) +
  geom_vline(
    xintercept = 0,
    color = "#333333",
    linewidth = 0.7
  ) +
  annotate(
    "text",
    x = -max_axis * 0.55,
    y = length(age_levels_pyramid) + 0.45,
    label = "Men",
    size = 5.2,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = max_axis * 0.55,
    y = length(age_levels_pyramid) + 0.45,
    label = "Women",
    size = 5.2,
    fontface = "bold"
  ) +
  scale_x_continuous(
    limits = c(-max_axis * 1.15, max_axis * 1.15),
    breaks = seq(-max_axis, max_axis, by = 2),
    labels = function(x) paste0(abs(x), "%")
  ) +
  scale_y_discrete(
    limits = age_levels_pyramid
  ) +
  scale_fill_manual(
    values = generation_colors_pyramid,
    name = "Generation",
    drop = FALSE
  ) +
  labs(
    title = "Age and gender pyramid of the sample",
    subtitle = paste0(
      "Percentage of participants with valid age and gender\n",
      "valid n = ", n_valid_pyramid,
      "; missing n = ", n_missing_pyramid,
      "; total n = ", n_total_pyramid
    ),
    x = "Percentage of the sample",
    y = "Age group"
  ) +
  theme_sample_tfm() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 14),
    plot.subtitle = element_text(size = 13, lineheight = 1.05),
    plot.margin = margin(12, 25, 12, 25)
  )

print(plot_population_pyramid)

ggsave(
  out_file(plots_dir, "sample_description_population_pyramid_age_gender_generation.png"),
  plot_population_pyramid,
  width = 11,
  height = 7,
  dpi = 300
)

ggsave(
  out_file(pdf_dir, "sample_description_population_pyramid_age_gender_generation.pdf"),
  plot_population_pyramid,
  width = 11,
  height = 7
)

ggsave(
  out_file(plots_dir, "sample_description_population_pyramid_generation.png"),
  plot_population_pyramid,
  width = 11,
  height = 7,
  dpi = 300
)

ggsave(
  out_file(pdf_dir, "sample_description_population_pyramid_generation.pdf"),
  plot_population_pyramid,
  width = 11,
  height = 7
)

ggsave(
  out_file(plots_dir, "population_pyramid_age_gender_generation.png"),
  plot_population_pyramid,
  width = 11,
  height = 7,
  dpi = 300
)

ggsave(
  out_file(pdf_dir, "population_pyramid_age_gender_generation.pdf"),
  plot_population_pyramid,
  width = 11,
  height = 7
)

sample_plots[["population_pyramid_age_gender_generation"]] <- plot_population_pyramid

# FINAL PDF WITH ALL PLOTS

pdf(
  file = out_file(pdf_dir, "sample_description_all_plots.pdf"),
  width = 12,
  height = 7,
  onefile = TRUE
)

for (p in sample_plots) {
  print(p)
}

dev.off()

# FINAL CHECKS

cat("\nSample description generated in:\n")
cat(base_output_dir, "\n")

cat("\nRows in sample_description:\n")
print(nrow(sample_description))

cat("\nMissing values by variable:\n")
print(missing_sample_description, n = Inf)

cat("\nMain files:\n")
cat("- en_sample_description.csv\n")
cat("- en_summary_sample_description_all_variables.csv\n")
cat("- en_sample_age_numeric_summary.csv\n")
cat("- en_sample_description_missing_by_variable.csv\n")
cat("- en_sample_description_all_plots.pdf\n")
cat("- en_sample_description_population_pyramid_age_gender_generation.pdf\n")
cat("- en_sociodemographic_by_archetype_for_word.csv\n")
cat("- en_sociodemographic_by_archetype_for_word.xlsx\n")
cat("- en_sociodemographic_by_archetype_for_word.docx\n")