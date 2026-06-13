

# SCRIPT 02 - ARMONIZACIÓN Y APPEND DE CONCERNS1 Y RVS

# Este script construye un dataset unificado a partir de:
# - El dataset base (Decision)
# - Dos encuestas adicionales (Concerns1 y RVS)
#
# Qué se hace exactamente:
#
# 1. Se cargan las encuestas originales (Excel) y se limpian los nombres de
#    variables para hacerlos consistentes (formato machine-readable).
#
# 2. Se construye un diccionario que permite rastrear cada variable a su
#    pregunta original (útil para debugging y trazabilidad).
#
# 3. Se seleccionan y renombran variables clave de cada encuesta usando
#    `transmute()`, creando variables con sufijo `_final`:
#       → Estas son las variables armonizadas (misma semántica entre encuestas)
#
# 4. IMPORTANTE:
#    - Las variables `_final` son la versión consolidada
#    - Las variables originales pueden diferir entre encuestas
#    - Por eso NO se combinan automáticamente, sino que se mapean manualmente
#
# 5. Se alinean las columnas de cada dataset con el dataset base:
#    - Si falta una variable → se crea como NA
#    - Se fuerza el mismo esquema de columnas en todos
#
# 6. Se hace un append vertical (`bind_rows`) de:
#       Decision + Concerns1 + RVS
#
# 7. Se añaden variables derivadas (edad a partir de año de nacimiento)
#
# 8. Se guarda el dataset final extendido:
#       df_decision_extended.csv
#
# Notas importantes:
# - Si una variable no aparece en el resultado final:
#     → probablemente el nombre no coincide exactamente
#     → o no está en df_base y se pierde en align_to_base
# - Las columnas como Prolific ID solo aparecerán si:
#     → existen en df_base O se añaden también allí


library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

# Carpetas de salida
base_output_dir <- "initial_descriptive_analysis/output/data_preparation"

csv_dir <- file.path(base_output_dir, "csv")
dict_dir <- file.path(base_output_dir, "dictionaries")
logs_dir <- file.path(base_output_dir, "logs")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dict_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

# Leer dataset base ya consolidado
df_base <- read_csv(
  file.path(csv_dir, "df_decision_consolidated_provisional.csv"),
  show_col_types = FALSE
)

# Funciones de lectura
clean_question_name <- function(x) {
  x %>%
    str_remove("\\s*\\(ID\\d+\\)\\s*$") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

read_survey <- function(path, survey_name) {
  raw <- read_excel(path, col_names = FALSE)
  original_names <- as.character(raw[4, ])
  df <- raw[-c(1:4), ]
  
  logical_names <- clean_question_name(original_names)
  logical_names <- make.unique(logical_names, sep = "_")
  names(df) <- logical_names
  
  dictionary <- data.frame(
    survey = survey_name,
    variable = logical_names,
    question = original_names,
    stringsAsFactors = FALSE
  )
  
  list(data = df, dictionary = dictionary)
}

# Leer nuevas encuestas
concerns1 <- read_survey(
  "initial_descriptive_analysis/data/Content_Export_RV-Concerns_Concerns1.xlsx",
  "concerns1"
)

rvs <- read_survey(
  "initial_descriptive_analysis/data/Content_Export_RVS_RVS.xlsx",
  "rvs"
)

df_concerns1 <- concerns1$data
df_rvs <- rvs$data

dictionary_extra <- bind_rows(
  concerns1$dictionary,
  rvs$dictionary
)

# 4. Helper para buscar variables
get_extra_q <- function(text) {
  dictionary_extra %>%
    filter(grepl(text, question, ignore.case = TRUE))
}

# Buscar nombres si tienes dudas
get_extra_q("year of birth")
get_extra_q("gender")
get_extra_q("country")
get_extra_q("education")


# DEFINIR VARIABLES COMUNES INICIALES
common_vars_final <- c(
  "please_enter_your_year_of_birth_final",
  "what_is_your_gender_final",
  "in_which_country_do_you_currently_live_final",
  "what_is_the_highest_level_of_education_that_you_have_completed_if_you_are_currently_studying_and_have_not_yet_completed_a_level_please_select_the_last_level_you_have_finished_final",
  "what_is_your_current_employment_status_please_indicate_your_main_contractual_status_if_you_are_temporarily_on_leave_such_as_sick_leave_parental_leave_or_a_temporary_reduction_in_working_hours_please_report_your_usual_employment_status_final"
)

df_base_common <- df_base %>%
  select(any_of(common_vars_final)) %>%
  mutate(source_survey = "decision")


# concerns1
df_concerns1_common <- df_concerns1 %>%
  transmute(
    # Perfil personal / socioeconómico
    please_enter_your_year_of_birth_final = please_enter_your_year_of_birth,
    what_is_your_gender_final = what_is_your_gender,
    what_type_of_household_do_you_live_in_please_select_the_option_that_best_describes_your_household_final =
      what_type_of_household_do_you_live_in_please_select_the_option_that_best_describes_your_household,
    what_is_the_highest_level_of_education_that_you_have_completed_if_you_are_currently_studying_and_have_not_yet_completed_a_level_please_select_the_last_level_you_have_finished_final =
      what_is_the_highest_level_of_education_that_you_have_completed_if_you_are_currently_studying_and_have_not_yet_completed_a_level_please_select_the_last_level_you_have_finished,
    what_is_your_current_employment_status_please_indicate_your_main_contractual_status_if_you_are_temporarily_on_leave_such_as_sick_leave_parental_leave_or_a_temporary_reduction_in_working_hours_please_report_your_usual_employment_status_final =
      what_is_your_current_employment_status_please_indicate_your_main_contractual_status_if_you_are_temporarily_on_leave_such_as_sick_leave_parental_leave_or_a_temporary_reduction_in_working_hours_please_report_your_usual_employment_status,
    do_you_have_any_health_condition_or_functional_limitation_that_requires_assistance_from_other_people_for_daily_activities_e_g_help_with_traveling_personal_care_or_household_tasks_final =
      do_you_have_any_health_condition_or_functional_limitation_that_requires_assistance_from_other_people_for_daily_activities_e_g_help_with_traveling_personal_care_or_household_tasks,
    
    # Movilidad
    in_a_typical_day_what_is_the_total_distance_you_travel_please_include_all_trips_and_all_modes_of_transport_car_bicycle_public_transport_walking_etc_final =
      in_a_typical_day_what_is_the_total_distance_you_travel_please_include_all_trips_and_all_modes_of_transport_car_bicycle_public_transport_walking_etc,
    in_a_typical_day_how_much_total_time_do_you_spend_travelling_please_include_all_trips_and_all_modes_of_transport_final =
      in_a_typical_day_how_much_total_time_do_you_spend_travelling_please_include_all_trips_and_all_modes_of_transport,
    do_you_currently_work_or_study_from_home_final = do_you_currently_work_or_study_from_home,
    which_of_the_following_best_describes_your_usual_travel_role_final =
      which_of_the_following_best_describes_your_usual_travel_role,
    
    # Política / decisión
    which_of_the_following_best_describes_your_general_approach_to_voting_in_elections_final =
      which_of_the_following_best_describes_your_general_approach_to_voting_in_elections,
    on_a_scale_from_0_to_100_where_0_means_most_left_and_100_means_most_right_where_would_you_place_yourself_politically_final =
      on_a_scale_from_0_to_100_where_0_means_most_left_and_100_means_most_right_where_would_you_place_yourself_politically,
    which_statement_best_describes_your_role_or_situation_regarding_household_renovation_decisions =
      which_statement_best_describes_your_role_or_situation_regarding_household_renovation_decisions,
    in_your_household_how_are_decisions_usually_made =
      in_a_renovation_process_how_are_decisions_usually_made_in_your_household,
    which_statement_best_describes_you_when_making_an_investment_decision_related_to_your_household_final =
      which_statement_best_describes_you_when_making_an_investment_decision_related_to_your_household,
    
    # Escalas
    on_a_scale_of_0_to_100_where_0_means_i_am_not_interested_in_energy_efficiency_at_all_and_100_means_i_am_interested_in_getting_the_highest_efficiency_in_my_home_even_if_the_actions_are_not_cost_effective_how_would_you_rate_your_energy_efficiency_goal_final =
      on_a_scale_of_0_to_100_where_0_means_i_am_not_interested_in_energy_efficiency_at_all_and_100_means_i_am_interested_in_getting_the_highest_efficiency_in_my_home_even_if_the_actions_are_not_cost_effective_how_would_you_rate_your_energy_efficiency_goal,
    on_a_scale_of_0_to_100_where_0_means_climate_change_does_not_exist_and_100_means_climate_change_is_real_and_caused_by_human_activities_how_would_you_rate_your_level_of_awareness_of_climate_change_final =
      on_a_scale_of_0_to_100_where_0_means_climate_change_does_not_exist_and_100_means_i_am_a_climate_change_expert_or_activist_how_would_you_rate_your_level_of_awareness_of_climate_change,
    on_a_scale_of_0_to_100_where_0_means_it_is_the_first_time_i_hear_about_it_and_100_means_i_am_an_expert_or_activist_how_would_you_rate_your_level_of_awareness_about_the_energy_transition_final =
      on_a_scale_of_0_to_100_where_0_means_it_is_the_first_time_i_hear_about_it_and_100_means_i_am_an_expert_or_activist_how_would_you_rate_your_level_of_awareness_about_the_energy_transition,
    
    # Vivienda
    in_which_country_do_you_currently_live_final = in_which_country_do_you_currently_live,
    what_is_the_approximate_population_size_of_the_city_where_you_live_final =
      what_is_the_approximate_population_size_of_the_city_where_you_live,
    in_which_climate_zone_do_you_live_please_select_the_option_that_corresponds_to_your_location_if_you_re_not_sure_which_climate_zone_you_live_in_please_refer_to_the_map_below_or_select_the_option_that_best_matches_your_region_final =
      in_which_climate_zone_do_you_live_please_select_the_option_that_corresponds_to_your_location_if_you_re_not_sure_which_climate_zone_you_live_in_please_refer_to_the_map_below_or_select_the_option_that_best_matches_your_region,
    what_is_the_current_tenure_status_of_your_home_final =
      what_is_the_current_tenure_status_of_your_home,
    
    #Experiencia con tecnologia
    # Experiencia con tecnologías
    for_each_of_the_following_technologies_decisions_or_behaviours_micro_efficiency_measures_e_g_avoiding_unnecessary_consumption_final =
      for_each_of_the_following_technologies_decisions_please_indicate_your_experience_micro_efficiency_measures_e_g_avoiding_unnecessary_consumption,
    for_each_of_the_following_technologies_decisions_or_behaviours_change_of_electricity_tariff_e_g_switching_to_a_time_of_use_tariff_final =
      for_each_of_the_following_technologies_decisions_please_indicate_your_experience_change_of_electricity_tariff_e_g_switching_to_a_time_of_use_tariff,
    for_each_of_the_following_technologies_decisions_or_behaviours_energy_efficient_appliances_final =
      for_each_of_the_following_technologies_decisions_please_indicate_your_experience_energy_efficient_appliances,
    for_each_of_the_following_technologies_decisions_or_behaviours_rooftop_photovoltaic_system_solar_pv_final =
      for_each_of_the_following_technologies_decisions_please_indicate_your_experience_photovoltaic_panels,
    for_each_of_the_following_technologies_decisions_or_behaviours_energy_storage_systems_final =
      for_each_of_the_following_technologies_decisions_please_indicate_your_experience_energy_storage_systems,
    for_each_of_the_following_technologies_decisions_or_behaviours_fosil_fuel_or_biomass_based_heating_system_final =
      for_each_of_the_following_technologies_decisions_please_indicate_your_experience_fosil_fuel_or_biomass_based_heating_system,
    for_each_of_the_following_technologies_decisions_or_behaviours_cooling_system_final =
      for_each_of_the_following_technologies_decisions_please_indicate_your_experience_cooling_system,
    for_each_of_the_following_technologies_decisions_or_behaviours_envelope_renovation_e_g_wall_roof_insulation_double_windows_final =
      for_each_of_the_following_technologies_decisions_please_indicate_your_experience_envelope_renovation_e_g_wall_roof_insulation_double_windows,
    for_each_of_the_following_technologies_decisions_or_behaviours_electric_vehicle_final =
      for_each_of_the_following_technologies_decisions_please_indicate_your_experience_electric_vehicle,
    for_each_of_the_following_technologies_decisions_or_behaviours_heat_recovery_mechanical_ventilation_final =
      for_each_of_the_following_technologies_decisions_please_indicate_your_experience_heat_recovery_mechanical_ventilation,
    for_each_of_the_following_technologies_decisions_or_behaviours_smart_home_systems_final =
      for_each_of_the_following_technologies_decisions_please_indicate_your_experience_smart_home_systems,
    for_each_of_the_following_technologies_decisions_or_behaviours_elevator_final =
      for_each_of_the_following_technologies_decisions_please_indicate_your_experience_elevator,
    for_each_of_the_following_technologies_decisions_or_behaviours_heat_pump_based_heating_system_final =
      for_each_of_the_following_technologies_decisions_please_indicate_your_experience_heat_pump_based_heating_system,
    for_each_of_the_following_technologies_decisions_or_behaviours_domestic_hot_water_system_e_g_gas_boiler_electric_water_heater_thermo_solar_thermal_system_final =
      for_each_of_the_following_technologies_decisions_please_indicate_your_experience_domestic_hot_water_system,
    
    # Fechas / antigüedad de tecnologías
    please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_micro_efficiency_measures_e_g_avoiding_unnecessary_consumption_final =
      please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_micro_efficiency_measures_e_g_avoiding_unnecessary_consumption,
    please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_change_of_electricity_tariff_e_g_switching_to_a_time_of_use_tariff_final =
      please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_change_of_electricity_tariff_e_g_switching_to_a_time_of_use_tariff,
    please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_energy_efficient_appliances_final =
      please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_energy_efficient_appliances,
    please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_rooftop_photovoltaic_system_solar_pv_final =
      please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_pv_panels,
    please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_energy_storage_systems_final =
      please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_energy_storage_systems,
    please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_fosil_fuel_or_biomass_based_heating_system_final =
      please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_fosil_fuel_or_biomass_based_heating_system,
    please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_cooling_system_final =
      please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_cooling_system,
    please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_envelope_renovation_e_g_wall_roof_insulation_double_windows_final =
      please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_envelope_renovation_e_g_wall_roof_insulation_double_windows,
    please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_electric_vehicle_final =
      please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_electric_vehicle,
    please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_heat_recovery_mechanical_ventilation_final =
      please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_heat_recovery_mechanical_ventilation,
    please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_smart_home_systems_final =
      please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_smart_home_systems,
    please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_elevator_final =
      please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_elevator,
    please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_heat_pump_based_heating_system_final =
      please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_heat_pump_based_heating_system,
    please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_domestic_hot_water_system_e_g_gas_boiler_electric_water_heater_thermo_solar_thermal_system_final =
      please_indicate_when_the_technology_decision_was_implemented_or_contracted_if_it_was_part_of_the_original_building_and_has_never_been_upgraded_please_indicate_the_age_of_the_building_domestic_hot_water_system,
    
    source_survey = "concerns1"
  )

# RVS
df_rvs_common <- df_rvs %>%
  transmute(
    do_you_have_a_prolific_id_or_an_identification_code_from_a_previous_survey =
      do_you_have_a_prolific_id_if_you_do_not_know_what_this_is_please_answer_no,
    
    please_provide_your_prolific_id=
      please_provide_your_prolific_id,
    # perfil respondent
    please_enter_your_year_of_birth_final =
      please_complete_one_row_for_each_member_of_your_household_starting_with_yourself_select_the_option_in_each_column_that_corresponds_to_each_household_member_respondent_year_of_birth,
    
    what_is_your_gender_final =
      please_complete_one_row_for_each_member_of_your_household_starting_with_yourself_select_the_option_in_each_column_that_corresponds_to_each_household_member_respondent_gender,
    
    what_is_the_highest_level_of_education_that_you_have_completed_if_you_are_currently_studying_and_have_not_yet_completed_a_level_please_select_the_last_level_you_have_finished_final =
      please_complete_one_row_for_each_member_of_your_household_starting_with_yourself_select_the_option_in_each_column_that_corresponds_to_each_household_member_respondent_educational_level,
    
    what_is_your_current_employment_status_please_indicate_your_main_contractual_status_if_you_are_temporarily_on_leave_such_as_sick_leave_parental_leave_or_a_temporary_reduction_in_working_hours_please_report_your_usual_employment_status_final =
      please_complete_one_row_for_each_member_of_your_household_starting_with_yourself_select_the_option_in_each_column_that_corresponds_to_each_household_member_respondent_employment_status,
    
    do_you_have_any_health_condition_or_functional_limitation_that_requires_assistance_from_other_people_for_daily_activities_e_g_help_with_traveling_personal_care_or_household_tasks_final =
      please_complete_one_row_for_each_member_of_your_household_starting_with_yourself_select_the_option_in_each_column_that_corresponds_to_each_household_member_respondent_disability_dependency_status,
    
    in_a_typical_day_what_is_the_total_distance_you_travel_please_include_all_trips_and_all_modes_of_transport_car_bicycle_public_transport_walking_etc_final =
      please_complete_one_row_for_each_member_of_your_household_starting_with_yourself_select_the_option_in_each_column_that_corresponds_to_each_household_member_respondent_daily_commuting_distance,
    
    in_a_typical_day_how_much_total_time_do_you_spend_travelling_please_include_all_trips_and_all_modes_of_transport_final =
      please_complete_one_row_for_each_member_of_your_household_starting_with_yourself_select_the_option_in_each_column_that_corresponds_to_each_household_member_respondent_daily_commuting_time,
    
    do_you_currently_work_or_study_from_home_final =
      please_complete_one_row_for_each_member_of_your_household_starting_with_yourself_select_the_option_in_each_column_that_corresponds_to_each_household_member_respondent_working_studying_at_home,
    
    which_of_the_following_best_describes_your_usual_travel_role_final =
      please_complete_one_row_for_each_member_of_your_household_starting_with_yourself_select_the_option_in_each_column_that_corresponds_to_each_household_member_respondent_travel_role,
    
    # vivienda/contexto
    in_which_country_do_you_currently_live_final =
      in_which_country_do_you_currently_live,
    
    what_is_the_approximate_population_size_of_the_city_where_you_live_final =
      what_is_the_approximate_population_size_of_the_city_where_you_live,
    
    in_which_climate_zone_do_you_live_please_select_the_option_that_corresponds_to_your_location_if_you_re_not_sure_which_climate_zone_you_live_in_please_refer_to_the_map_below_or_select_the_option_that_best_matches_your_region_final =
      in_which_climate_zone_do_you_live_please_select_the_option_that_corresponds_to_your_location_if_you_re_not_sure_which_climate_zone_you_live_in_please_refer_to_the_map_below_or_select_the_option_that_best_matches_your_region,
    
    # !!!! en Decision es tenure status, en RVS es own house
    what_is_the_current_tenure_status_of_your_home_final =
      do_you_own_your_house,
    
    # tecnologías experiencia
    for_each_of_the_following_technologies_decisions_or_behaviours_micro_efficiency_measures_e_g_avoiding_unnecessary_consumption_final =
      for_each_of_the_following_technologies_please_indicate_your_experience_micro_efficiency_measures_e_g_avoiding_unnecesary_loads,
    
    for_each_of_the_following_technologies_decisions_or_behaviours_change_of_electricity_tariff_e_g_switching_to_a_time_of_use_tariff_final =
      for_each_of_the_following_technologies_please_indicate_your_experience_change_of_electricity_tariff_e_g_switching_to_a_time_of_use_tariff,
    
    for_each_of_the_following_technologies_decisions_or_behaviours_energy_efficient_appliances_final =
      for_each_of_the_following_technologies_please_indicate_your_experience_energy_efficient_appliances,
    
    for_each_of_the_following_technologies_decisions_or_behaviours_rooftop_photovoltaic_system_solar_pv_final =
      for_each_of_the_following_technologies_please_indicate_your_experience_pv_panels,
    
    for_each_of_the_following_technologies_decisions_or_behaviours_energy_storage_systems_final =
      for_each_of_the_following_technologies_please_indicate_your_experience_energy_storage_systems,
    
    # RVS solo tiene heating_system general, no separa fossil/heat pump
    # Yo NO lo mapearía a fossil ni a heat pump.
    
    for_each_of_the_following_technologies_decisions_or_behaviours_cooling_system_final =
      for_each_of_the_following_technologies_please_indicate_your_experience_cooling_system,
    
    for_each_of_the_following_technologies_decisions_or_behaviours_envelope_renovation_e_g_wall_roof_insulation_double_windows_final =
      for_each_of_the_following_technologies_please_indicate_your_experience_envelope_renovation_e_g_wall_roof_insulation_double_windows,
    
    for_each_of_the_following_technologies_decisions_or_behaviours_electric_vehicle_final =
      for_each_of_the_following_technologies_please_indicate_your_experience_electric_vehicle,
    
    for_each_of_the_following_technologies_decisions_or_behaviours_heat_recovery_mechanical_ventilation_final =
      for_each_of_the_following_technologies_please_indicate_your_experience_heat_recovery_mechanical_ventilation,
    
    for_each_of_the_following_technologies_decisions_or_behaviours_smart_home_systems_final =
      for_each_of_the_following_technologies_please_indicate_your_experience_smart_home_systems,
    
    for_each_of_the_following_technologies_decisions_or_behaviours_elevator_final =
      for_each_of_the_following_technologies_please_indicate_your_experience_elevator,
    # 5.1 voting
    which_of_the_following_best_describes_your_general_approach_to_voting_in_elections_final =
      which_of_the_following_best_describes_your_usual_voting_behaviour,
    
    # 5.2 escala política
    on_a_scale_from_0_to_100_where_0_means_most_left_and_100_means_most_right_where_would_you_place_yourself_politically_final =
      on_a_scale_from_0_to_100_where_0_means_most_left_and_100_means_most_right_where_would_you_place_yourself_politically,
    
    which_statement_best_describes_your_role_or_situation_regarding_household_renovation_decisions =
      which_statement_best_describes_your_role_or_situation_regarding_household_renovation_decisions,
    
    in_your_household_how_are_decisions_usually_made =
      in_a_renovation_process_how_are_decisions_usually_made_in_your_hosehold,
    
    # 5.5 perfil decisión inversión
    which_statement_best_describes_you_when_making_an_investment_decision_related_to_your_household_final =
      which_statement_best_describes_you_when_making_an_investment_decision_related_to_your_household,
    
    # 5.6 interés eficiencia
    on_a_scale_of_0_to_100_where_0_means_i_am_not_interested_in_energy_efficiency_at_all_and_100_means_i_am_interested_in_getting_the_highest_efficiency_in_my_home_even_if_the_actions_are_not_cost_effective_how_would_you_rate_your_energy_efficiency_goal_final =
      on_a_scale_of_0_to_100_where_0_means_i_am_not_interested_in_energy_efficiency_at_all_and_100_means_i_am_interested_in_getting_the_highest_efficiency_in_my_home_even_if_the_actions_are_not_cost_effective_how_would_you_rate_your_energy_efficiency_goal,
    
    # 5.7 climate awareness
    on_a_scale_of_0_to_100_where_0_means_climate_change_does_not_exist_and_100_means_climate_change_is_real_and_caused_by_human_activities_how_would_you_rate_your_level_of_awareness_of_climate_change_final =
      on_a_scale_of_0_to_100_where_0_means_climate_change_does_not_exist_and_100_means_i_am_a_climate_change_expert_or_activist_how_would_you_rate_your_level_of_awareness_of_climate_change,
    
    # 5.8 energy transition awareness
    on_a_scale_of_0_to_100_where_0_means_it_is_the_first_time_i_hear_about_it_and_100_means_i_am_an_expert_or_activist_how_would_you_rate_your_level_of_awareness_about_the_energy_transition_final =
      on_a_scale_of_0_to_100_where_0_means_it_is_the_first_time_i_hear_about_it_and_100_means_i_am_an_expert_or_activist_how_would_you_rate_your_level_of_awareness_about_the_energy_transition,
    
    source_survey = "rvs"
  )

# FUNCIÓN PARA ALINEAR EXTRAS CON DF_BASE
align_to_base <- function(df_extra, base_cols) {
  
  df_extra <- df_extra %>%
    mutate(across(everything(), as.character))
  
  missing_cols <- setdiff(base_cols, names(df_extra))
  
  for (col in missing_cols) {
    df_extra[[col]] <- NA_character_
  }
  
  df_extra %>%
    select(all_of(base_cols))
}
base_cols <- union(names(df_base), "source_survey")
# CREAR DATASET EXTENDIDO
df_base_full <- df_base %>%
  mutate(source_survey = "decision") %>%
  mutate(across(everything(), as.character))

df_concerns1_aligned <- align_to_base(
  df_concerns1_common,
  names(df_base_full)
)

df_rvs_aligned <- align_to_base(
  df_rvs_common,
  names(df_base_full)
)

df_extended <- bind_rows(
  df_base_full,
  df_concerns1_aligned,
  df_rvs_aligned
)

df_extended <- df_extended %>%
  mutate(
    year_of_birth = as.numeric(please_enter_your_year_of_birth_final),
    age = 2026 - year_of_birth
  )

dim(df_base_full)
dim(df_extended)

table(df_extended$source_survey, useNA = "ifany")

View(df_extended)

# GUARDAR DATASET EXTENDIDO
write_csv(
  df_extended,
  file.path(csv_dir, "df_decision_extended.csv")
)

write_csv(
  dictionary_extra,
  file.path(dict_dir, "dictionary_extra_concerns1_rvs.csv")
)

## comprobar columnas join

# AUDITORÍA DE MAPEO ENTRE ENCUESTAS

# Variables finales que existen en el dataset base
base_cols <- names(df_base)

# Variables que se han mapeado manualmente desde Concerns1 y RVS
concerns1_mapped_cols <- names(df_concerns1_common)
rvs_mapped_cols <- names(df_rvs_common)

# Variables mapeadas que sí entrarán en el dataset final
mapped_coverage <- tibble(
  survey = c("concerns1", "rvs"),
  n_mapped_cols = c(
    length(concerns1_mapped_cols),
    length(rvs_mapped_cols)
  ),
  n_mapped_cols_in_base = c(
    sum(concerns1_mapped_cols %in% base_cols),
    sum(rvs_mapped_cols %in% base_cols)
  ),
  n_mapped_cols_not_in_base = c(
    sum(!concerns1_mapped_cols %in% base_cols),
    sum(!rvs_mapped_cols %in% base_cols)
  )
)

print(mapped_coverage)

# Variables mapeadas que NO existen en df_base
#    Estas se perderán al hacer align_to_base()
concerns1_mapped_not_in_base <- tibble(
  survey = "concerns1",
  variable = setdiff(concerns1_mapped_cols, base_cols)
)

rvs_mapped_not_in_base <- tibble(
  survey = "rvs",
  variable = setdiff(rvs_mapped_cols, base_cols)
)

mapped_not_in_base <- bind_rows(
  concerns1_mapped_not_in_base,
  rvs_mapped_not_in_base
)

print(mapped_not_in_base, n = Inf)

# Variables del dataset base que NO se han mapeado desde Concerns1/RVS
base_not_mapped_concerns1 <- tibble(
  survey = "concerns1",
  variable = setdiff(base_cols, concerns1_mapped_cols)
)

base_not_mapped_rvs <- tibble(
  survey = "rvs",
  variable = setdiff(base_cols, rvs_mapped_cols)
)

base_not_mapped <- bind_rows(
  base_not_mapped_concerns1,
  base_not_mapped_rvs
)

print(base_not_mapped, n = Inf)


#  Columnas originales de Concerns1/RVS que NO se han usado en el mapeo
concerns1_original_cols <- names(df_concerns1)
rvs_original_cols <- names(df_rvs)

# OJO: esto solo compara nombres originales con nombres armonizados.
# Sirve como primera revisión, pero no detecta equivalencias semánticas.
concerns1_original_not_used <- tibble(
  survey = "concerns1",
  original_variable = setdiff(concerns1_original_cols, concerns1_mapped_cols)
)

rvs_original_not_used <- tibble(
  survey = "rvs",
  original_variable = setdiff(rvs_original_cols, rvs_mapped_cols)
)

original_not_used <- bind_rows(
  concerns1_original_not_used,
  rvs_original_not_used
)

print(original_not_used, n = Inf)

# Guardar auditoría
write_csv(
  mapped_coverage,
  file.path(logs_dir, "mapping_coverage_summary.csv")
)

write_csv(
  mapped_not_in_base,
  file.path(logs_dir, "mapped_variables_not_in_base.csv")
)

write_csv(
  base_not_mapped,
  file.path(logs_dir, "base_variables_not_mapped_by_extra_surveys.csv")
)

write_csv(
  original_not_used,
  file.path(logs_dir, "original_variables_not_used_concerns1_rvs.csv")
)
