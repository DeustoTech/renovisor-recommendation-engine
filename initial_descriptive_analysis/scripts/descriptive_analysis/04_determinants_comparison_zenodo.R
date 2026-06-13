

# SCRIPT 04 - COMPARACIÓN DE DETERMINANTES: NUESTRA MUESTRA VS ZENODO
#
# Objetivo:
# Comparar las puntuaciones de los 32 determinantes de nuestra muestra con una
# muestra externa de referencia procedente de Zenodo.
#
# Qué se hace:
# 1. Se cargan ambos datasets:
#    - df_analysis_ready.csv: nuestra muestra
#    - Survey_September2023_cleaned.xlsx: muestra externa Zenodo
#
# 2. Se seleccionan las columnas correspondientes a los 32 determinantes en
#    ambos datasets.
#
# 3. Se renombran los determinantes con nombres comunes para poder compararlos.
#
# 4. Se normaliza la escala de Zenodo de 1-5 a 0-100:
#       1 = 0
#       2 = 25
#       3 = 50
#       4 = 75
#       5 = 100
#
# 5. Se transforma la información a formato largo para poder analizar y graficar.
#
# 6. Se calculan estadísticas descriptivas por determinante:
#    - n
#    - media
#    - mediana
#    - desviación típica
#    - diferencia de medias
#
# 7. Se genera un gráfico comparando las medias normalizadas de ambas muestras.
#
# 8. Se aplica un test de Wilcoxon por determinante para explorar diferencias
#    entre muestras. Estos resultados deben interpretarse con cautela porque
#    los tamaños muestrales son muy distintos.
#
# 9. Se calcula la correlación entre los perfiles medios de ambas muestras.
#    Esto permite saber si el patrón general de importancia de los determinantes
#    es parecido entre ambas muestras.
#
# 10. Se calcula el tamaño del efecto mediante Cohen's d para identificar en qué
#     determinantes la diferencia entre muestras es más grande.
#
# 11. Se compara el ranking de importancia de los determinantes en ambas muestras.
#     Esto permite ver qué determinantes ocupan posiciones similares y cuáles
#     cambian mucho entre nuestra muestra y Zenodo.
#
# Resultado esperado:
# Obtener evidencia descriptiva sobre si nuestra muestra piloto sigue una
# estructura de determinantes similar a la muestra externa, y detectar qué
# factores muestran mayores diferencias.


library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

#  Cargar datos
base_input_dir <- "initial_descriptive_analysis/output/clean_datasets"

df_ours <- read_csv(
  file.path(base_input_dir, "df_clean_general.csv")
)


# 1B. Carpetas de salida


base_output_dir <- "initial_descriptive_analysis/output/determinants_comparison"

csv_dir <- file.path(base_output_dir, "csv")
plots_dir <- file.path(base_output_dir, "plots")
pdf_dir <- file.path(base_output_dir, "pdf")
logs_dir <- file.path(base_output_dir, "logs")

dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

n_total <- nrow(df_ours)

df_ours_clean <- df_ours

cat("Total usado en comparación:", n_total, "\n")
cat("Attention check fallido Decision, pero conservado:", 
    sum(df_ours$failed_attention_check_decision, na.rm = TRUE), "\n")
cat("Attention check fallido any, pero conservado:", 
    sum(df_ours$failed_attention_check_any, na.rm = TRUE), "\n")

# n después
n_clean <- nrow(df_ours_clean)

cat("Total:", n_total, "\n")
cat("Tras filtros:", n_clean, "\n")
cat("Eliminados:", n_total - n_clean, "\n")

df_eliminadas <- df_ours %>%
  filter(
    !(is.na(.data[[att_42]]) | str_detect(.data[[att_42]], "^42\\.")) |
      !(is.na(.data[[att_4]]) | str_detect(.data[[att_4]], "^Option 4\\."))
  )

#View(df_eliminadas)

write_csv(df_eliminadas, file.path(logs_dir, "attention_check_removed_rows.csv"))

df_zenodo <- read_excel(
  "initial_descriptive_analysis/data/Survey_September2023_cleaned.xlsx"
)

# Nombres estándar de determinantes D1-D32
det_names <- c(
  "profits",
  "credit_score",
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
  "autarky",
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


# Columnas de nuestra encuesta, en el MISMO orden D1-D32
our_det_cols <- c(
  "profits_profits_are_what_guide_my_decision_making_i_always_prefer_to_earn_or_save_money_with_every_decision_i_take",
  "credit_score_access_to_funding_my_own_savings_deductions_exemptions_and_or_credits_is_the_main_factor_that_allows_hinders_me_to_make_an_investment_decision",
  "risk_profile_the_evaluation_of_the_risks_of_my_investment_s_is_what_will_always_guide_my_final_decision",
  "added_value_i_will_only_invest_if_my_actions_have_an_impact_beyond_the_monetary_gain_losses",
  "frugality_i_am_a_thrifty_person_so_i_only_invest_in_actions_that_allow_me_to_reduce_my_cost_impact_expenditures",
  "climate_protection_every_decision_i_take_serves_to_foster_the_planet_s_preservation_if_my_choice_might_harm_the_environment_i_will_always_avoid_taking_this_action",
  "legal_having_complete_certainty_that_my_actions_comply_with_the_legal_tax_and_administrative_regulations_are_what_guide_my_actions",
  "trust_i_only_make_decisions_if_i_trust_all_the_parties_involved_e_g_public_administration_neighbors_and_that_i_trust_the_technology_that_is_needed_to_accomplish_my_goal",
  "safety_i_only_make_decisions_if_the_outcome_of_them_ensures_or_improves_my_safety_or_the_ones_of_my_relatives",
  "cost_efficiency_i_always_review_and_assess_the_pros_and_cons_of_my_decisions_looking_for_the_most_cost_effective_option",
  "knowledge_i_do_not_make_a_decision_if_i_do_not_have_enough_knowledge_of_the_subject_matter",
  "own_competence_feeling_that_i_am_competent_to_make_an_investment_is_what_guides_my_decision_making",
  "technical_fit_i_carefully_check_that_the_technology_or_equipment_fits_my_lifestyle_or_the_technical_requirements_before_making_an_investment_decision",
  "environmental_concerns_i_always_review_and_assess_the_pros_and_cons_of_my_decisions_in_relation_to_the_environment_before_making_a_decision",
  "self_satisfaction_i_will_only_make_a_decision_if_i_feel_satisfied_with_the_action_and_the_expected_outcome",
  "commitment_i_only_make_a_decision_if_i_feel_personally_committed_to_the_action_and_the_expected_outcome",
  "adherence_i_will_only_make_a_decision_if_i_feel_that_i_can_sustain_it_throughout_time",
  "autonomy_self_sufficiency_and_individual_sovereignty_is_what_guide_my_decisions_i_will_only_make_a_decision_if_i_feel_that_the_investment_will_improve_my_control_of_all_circumstances_and_potential_outcomes",
  "wellbeing_i_will_only_make_a_decision_if_it_improves_my_well_being_or_the_well_being_of_my_relatives",
  "coziness_i_will_only_make_a_decision_if_it_improves_my_comfort_or_the_comfort_of_my_relatives",
  "rights_and_duties_i_firmly_believe_that_we_live_in_a_society_where_we_have_to_adhere_to_regulations_laws_and_community_agreements_by_all_means_so_my_investment_decision_has_to_agree_with_this_vision",
  "peer_pressure_my_investment_decisions_are_influenced_by_the_opinions_of_others_e_g_my_peers_relatives_or_family",
  "support_i_only_make_an_investment_decision_if_it_fulfils_a_social_need_or_improves_the_society_as_a_whole",
  "socialising_i_will_only_make_an_investment_decision_if_it_improves_my_possibilities_to_socialise_with_my_peers_and_relatives",
  "agreement_i_will_only_make_an_investment_decision_if_the_people_affected_by_it_e_g_my_relatives_peers_or_the_community_agree_with_the_decision_cohesively",
  "novelty_i_love_to_test_new_ideas_and_cutting_edge_technology_so_novelty_is_what_drives_my_investment_decisions",
  "fun_having_fun_is_important_to_me_therefore_i_will_only_make_a_decision_if_it_would_be_enjoyable_and_amusing_for_me",
  "recognition_i_make_investment_decisions_that_lead_to_my_increased_status_and_i_can_show_others_what_i_achieved",
  "trends_i_usually_follow_the_trends_when_making_a_decision_in_particular_i_usually_find_myself_sticking_to_the_ads_i_see_the_recommendations_of_people_i_admire_or_what_i_read_in_magazines_or_blogs_i_follow",
  "authority_i_only_make_a_decision_if_it_helps_me_improve_my_position_as_an_expert_on_the_subject_matter",
  "approval_i_only_make_a_decision_if_it_improves_my_peers_opinions_about_me_even_if_this_decision_is_not_always_what_i_would_do_only_for_myself",
  "own_significance_i_only_make_a_decision_if_the_action_has_a_personal_inner_meaning_for_me_beyond_any_economic_gain"
)


# Columnas Zenodo, asumidas en orden D1-D32
zenodo_det_cols <- c(
  paste0("Q21_", 1:8),
  paste0("Q22_", 1:8),
  paste0("Q23_", 1:8),
  paste0("Q24_", 1:8)
)

# Comprobar que existen
setdiff(zenodo_det_cols, names(df_zenodo))
setdiff(our_det_cols, names(df_ours_clean))


# Preparar nuestra muestra
ours_long <- df_ours_clean %>%
  select(all_of(our_det_cols)) %>%
  rename_with(~ det_names, all_of(our_det_cols)) %>%
  mutate(across(everything(), as.numeric)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "determinant",
    values_to = "score"
  ) %>%
  mutate(sample = "ours")


# Preparar Zenodo: 1-5 a 0-100
# Fórmula: 1 -> 0, 2 -> 25, 3 -> 50, 4 -> 75, 5 -> 100

zenodo_long <- df_zenodo %>%
  select(all_of(zenodo_det_cols)) %>%
  rename_with(~ det_names, all_of(zenodo_det_cols)) %>%
  mutate(across(everything(), ~ (as.numeric(.) - 1) / 4 * 100)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "determinant",
    values_to = "score"
  ) %>%
  mutate(sample = "zenodo")

# Unir ambas muestras
df_compare_long <- bind_rows(ours_long, zenodo_long)

# Comparación descriptiva por determinante
comparison_summary <- df_compare_long %>%
  group_by(sample, determinant) %>%
  summarise(
    n = sum(!is.na(score)),
    mean = mean(score, na.rm = TRUE),
    median = median(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = sample,
    values_from = c(n, mean, median, sd)
  ) %>%
  mutate(
    mean_diff_ours_minus_zenodo = mean_ours - mean_zenodo,
    abs_diff = abs(mean_diff_ours_minus_zenodo)
  ) %>%
  arrange(desc(abs_diff))

#View(comparison_summary)

write_csv(
  comparison_summary,
  file.path(csv_dir, "determinants_comparison_summary.csv")
)

####### GRÁFICOS
## medias comparadas
mean_plot_data <- df_compare_long %>%
  group_by(sample, determinant) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    .groups = "drop"
  )

plot_mean_comparison <- ggplot(mean_plot_data, aes(x = reorder(determinant, mean), y = mean, fill = sample)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Comparación de determinantes: nuestra muestra vs Zenodo",
    x = "Determinante",
    y = "Media normalizada 0-100",
    fill = "Muestra"
  ) +
  theme_minimal()
ggsave(
  file.path(plots_dir, "determinants_mean_comparison.png"),
  plot_mean_comparison,
  width = 12,
  height = 9,
  dpi = 300
)

ggsave(
  file.path(pdf_dir, "determinants_mean_comparison.pdf"),
  plot_mean_comparison,
  width = 12,
  height = 9
)

# 10. Test estadístico simple por determinante
# Ojo: con nuestra muestra pequeña, interpretar con cuidado
tests <- df_compare_long %>%
  group_by(determinant) %>%
  summarise(
    p_value = tryCatch(
      wilcox.test(score ~ sample)$p.value,
      error = function(e) NA_real_
    ),
    .groups = "drop"
  ) %>%
  left_join(comparison_summary, by = "determinant") %>%
  arrange(p_value)

#View(tests)

write_csv(
  tests,
  file.path(csv_dir, "determinants_comparison_tests.csv")
)


######## correlacion entre perfiles
profile_ours <- df_compare_long %>%
  filter(sample == "ours") %>%
  group_by(determinant) %>%
  summarise(mean = mean(score, na.rm = TRUE))

profile_zenodo <- df_compare_long %>%
  filter(sample == "zenodo") %>%
  group_by(determinant) %>%
  summarise(mean = mean(score, na.rm = TRUE))

correlation <- cor(profile_ours$mean, profile_zenodo$mean)

correlation
# parecido moderado-alto entre muestras


#####
effect_sizes <- df_compare_long %>%
  group_by(determinant) %>%
  summarise(
    mean_ours = mean(score[sample == "ours"], na.rm = TRUE),
    mean_zenodo = mean(score[sample == "zenodo"], na.rm = TRUE),
    sd_ours = sd(score[sample == "ours"], na.rm = TRUE),
    sd_zenodo = sd(score[sample == "zenodo"], na.rm = TRUE),
    n_ours = sum(sample == "ours"),
    n_zenodo = sum(sample == "zenodo"),
    
    pooled_sd = sqrt(((n_ours - 1)*sd_ours^2 + (n_zenodo - 1)*sd_zenodo^2) / 
                       (n_ours + n_zenodo - 2)),
    
    cohens_d = (mean_ours - mean_zenodo) / pooled_sd
  ) %>%
  arrange(desc(abs(cohens_d)))

#View(effect_sizes)
write_csv(effect_sizes, file.path(csv_dir, "determinants_effect_sizes.csv"))


##############
ranking <- profile_ours %>%
  rename(mean_ours = mean) %>%
  left_join(profile_zenodo %>% rename(mean_zenodo = mean), by = "determinant") %>%
  mutate(
    rank_ours = rank(-mean_ours),
    rank_zenodo = rank(-mean_zenodo),
    rank_diff = abs(rank_ours - rank_zenodo)
  ) %>%
  arrange(desc(rank_diff))

#View(ranking)  

write_csv(ranking, file.path(csv_dir, "determinants_ranking_comparison.csv"))


##### RESULTADOS

# Comparación de determinantes entre nuestra muestra y Zenodo

# El análisis comparativo muestra que, a pesar de la diferencia de tamaño muestral (n≈40 vs n=1899), 
# existe una consistencia general en la estructura de los determinantes, con una correlación moderada-alta
# entre perfiles (≈0.73). Esto indica que nuestra muestra reproduce razonablemente bien el patrón global
# observado en la muestra de referencia.
# 
# No obstante, se identifican diferencias relevantes tanto en intensidad como en priorización, que
# aportan información valiosa.
# 
# En primer lugar, los determinantes asociados a criterios instrumentales y normativos presentan
# una mayor relevancia en nuestra muestra. Destacan especialmente:
#   
# legal
# profits
# credit_score
# agreement
# 
# Estos factores no solo muestran diferencias de media elevadas (superiores a 20 puntos), sino
# también tamaños de efecto grandes (Cohen’s d ≈ 0.7–0.9), lo que indica diferencias sustanciales
# entre muestras. Además, el análisis de ranking refuerza esta idea: por ejemplo, legal pasa de una
# posición media-baja en Zenodo (rank 19) a una de las más altas en nuestra muestra (rank 3).
# 
# Por el contrario, la muestra de Zenodo presenta mayor peso en factores más vinculados a motivaciones
# intrínsecas o hedónicas, como:
#   
# fun
# novelty
# own_significance
# 
# Estos determinantes muestran diferencias negativas importantes (nuestra muestra puntúa más bajo) y
# también tamaños de efecto elevados en sentido inverso (d < -0.5). En términos de ranking, own_significance
# es especialmente destacable, pasando de una posición alta en Zenodo (rank 6) a una posición media-baja 
# en nuestra muestra (rank 23), lo que evidencia un cambio claro en el tipo de motivación predominante.
# 
# Además, algunos determinantes presentan una alta estabilidad entre ambas muestras, tanto en valores como
# en ranking:
#   
# approval
# own_competence
# peer_pressure
# trust
# 
# En estos casos, las diferencias de media son prácticamente nulas y los tamaños de efecto cercanos a cero,
# lo que sugiere que se trata de factores estructurales consistentes en el comportamiento.
# 
# Por último, el análisis de ranking permite identificar que las principales diferencias no solo son de magnitud,
# sino también de reordenación en la importancia relativa de los determinantes, lo que refuerza la idea de que
# ambas muestras comparten una base común, pero responden a perfiles o contextos distintos.
# 
# Conclusión
# 
# En conjunto, los resultados indican que nuestra muestra captura adecuadamente la estructura general de los
# determinantes, pero presenta un sesgo hacia factores más racionales, económicos y normativos, mientras que
# la muestra de Zenodo refleja un mayor peso de motivaciones intrínsecas y personales.
# 
# Esto sugiere que las diferencias observadas no son aleatorias, sino coherentes con posibles diferencias en 
# el perfil de los participantes o en el contexto de la encuesta.



# ==============================================================================


# columnas
col_impl <- "from_the_following_list_please_select_the_technology_or_energy_related_measure_you_have_implemented_at_home_that_you_consider_most_important_it_doesn_t_need_to_be_the_most_expensive_or_the_one_with_the_highest_energy_savings_simply_the_one_you_find_most_valuable_if_you_haven_t_installed_any_please_select_the_none_option"

col_interest <- "which_of_the_following_technologies_or_energy_related_measures_are_you_most_interested_in_implementing_in_your_home_please_choose_the_one_you_find_most_relevant_to_your_personal_needs_regardless_of_cost_or_potential_savings_select_none_option_if_you_are_not_interested_in_any"

col_curiosity <- "is_there_a_technology_or_energy_related_measures_you_don_t_know_much_about_but_that_sparks_your_curiosity_please_select_the_one_you_would_be_most_interested_in_learning_more_about_for_your_home_select_none_option_if_you_are_already_familiar_with_all_of_them"


df_ours_clean <- df_ours_clean %>%
  mutate(
    TTM_stage = case_when(
      
      # ya ha hecho algo
      !is.na(.data[[col_impl]]) &
        !str_detect(.data[[col_impl]], "None") ~ "already",
      
      # interesado activo
      !is.na(.data[[col_interest]]) &
        !str_detect(.data[[col_interest]], "None") ~ "consider",
      
      # curiosidad pero no intención clara
      !is.na(.data[[col_curiosity]]) &
        !str_detect(.data[[col_curiosity]], "None") ~ "aware",
      
      # resto
      TRUE ~ "never"
    )
  )


table(df_ours_clean$TTM_stage)