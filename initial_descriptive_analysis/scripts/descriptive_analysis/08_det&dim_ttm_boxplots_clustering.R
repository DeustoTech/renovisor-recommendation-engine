
# SCRIPT 08 - BOXPLOTS DETERMINANTES POR ETAPA TTM

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tibble)
library(purrr)
library(gridExtra)
library(grid)



# 1. CARGAR TABLA
df <- read_csv(
  file.path(
    "initial_descriptive_analysis/output/ttm_stage_analysis/csv",
    "ttm_stage_determinant_vector_wide.csv"
  ),
  show_col_types = FALSE
)

glimpse(df)



# 2. DEFINIR COLUMNAS CLAVE
id_col <- "participant_id"
stage_col <- "stage"
technology_col <- "technology"
dimension_col <- "dimension"



# 3. DICCIONARIOS EN CASTELLANO
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
    "Beneficio económico",
    "Acceso a financiación",
    "Perfil de riesgo",
    "Valor añadido",
    "Frugalidad",
    "Protección climática",
    "Cumplimiento legal",
    "Confianza",
    "Seguridad",
    "Eficiencia de costes",
    "Conocimiento",
    "Competencia propia",
    "Adecuación técnica",
    "Preocupación ambiental",
    "Satisfacción personal",
    "Compromiso",
    "Adherencia",
    "Autosuficiencia",
    "Bienestar",
    "Confort",
    "Derechos y deberes",
    "Presión social",
    "Apoyo social",
    "Socialización",
    "Acuerdo",
    "Novedad",
    "Diversión",
    "Reconocimiento",
    "Tendencias",
    "Autoridad",
    "Aprobación",
    "Significado personal"
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
    "Seguridad financiera",
    "Seguridad",
    "Competencia",
    "Autonomía",
    "Materialidad",
    "Vinculación",
    "Estímulo",
    "Popularidad",
    "Significado"
  )
)

determinant_label_levels <- determinant_dictionary$determinant_label
determinant_id_levels <- determinant_dictionary$determinant_id
dimension_label_levels <- dimension_dictionary$dimension_label


recode_dimension_label <- function(x) {
  x <- as.character(x)
  
  case_when(
    x %in% c("FINANCIAL", "Financial", "Financiera", "Financiero", "Dimensión financiera", "Seguridad financiera", "Seguridad Financiera") ~ "Seguridad financiera",
    x %in% c("SECURITY", "Security", "Seguridad") ~ "Seguridad",
    x %in% c("COMPETENCE", "Competence", "Competencia") ~ "Competencia",
    x %in% c("AUTONOMY", "Autonomy", "Autonomía") ~ "Autonomía",
    x %in% c("PHYSIOLOGICAL", "Physiological", "Fisiología", "Bienestar físico", "Materialidad") ~ "Materialidad",
    x %in% c("RELATEDNESS", "Relatedness", "Relación", "Relaciones sociales", "Vinculación", "Vinculación sociales") ~ "Vinculación",
    x %in% c("STIMULATION", "Stimulation", "Estimulación", "Estímulo", "Estimulo") ~ "Estímulo",
    x %in% c("POPULARITY", "Popularity", "Popularidad") ~ "Popularidad",
    x %in% c("MEANING", "Meaning", "Sentido", "Significado") ~ "Significado",
    TRUE ~ NA_character_
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

# 4. DEFINIR COLUMNAS DE DETERMINANTES
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

cat("Número de determinantes detectados:", length(determinant_cols), "\n")
print(determinant_cols)

# 5. CREAR CARPETAS DE SALIDA
base_output_dir <- "initial_descriptive_analysis/output/boxplots_ttm_determinants"

csv_dir <- file.path(base_output_dir, "csv")
plots_dir <- file.path(base_output_dir, "plots")
pdf_dir <- file.path(base_output_dir, "pdf")
profile_rank_dir <- file.path(plots_dir, "by_profile_stage_rank")

dir.create(csv_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(pdf_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(profile_rank_dir, showWarnings = FALSE, recursive = TRUE)


# 6. CONFIGURACIÓN VISUAL PARA GRÁFICOS DEL TFM
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

box_fill <- "#BDE3FF"
box_color <- "#2C3E50"
outlier_color <- "#4F4F4F"

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

# 7. FUNCIONES AUXILIARES
save_plot <- function(plot, filename, width = 12, height = 7) {
  ggsave(
    filename = file.path(plots_dir, paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}

make_subtitle <- function(data) {
  paste0(
    "n participantes = ", n_distinct(data$participant_id),
    "; observaciones participante-determinante válidas = ", nrow(data)
  )
}

# 8. PASAR A FORMATO LARGO Y TRADUCIR DETERMINANTES/DIMENSIONES
df_long <- df %>%
  pivot_longer(
    cols = all_of(determinant_cols),
    names_to = "determinant_id",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  mutate(
    value = as.numeric(value),
    etapa = .data[[stage_col]],
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
  file.path(csv_dir, "ttm_determinants_long_for_boxplots.csv")
)

glimpse(df_long)

# 9. CREAR DATASET GENERAL
df_general_long <- df_long %>%
  mutate(etapa = "General")

df_all_long <- bind_rows(
  df_general_long,
  df_long
) %>%
  mutate(
    etapa = factor(
      etapa,
      levels = c(
        "General",
        "Implementada",
        "La conoce / la consideraría",
        "No la conoce, pero le genera curiosidad"
      )
    ),
    determinant = factor(determinant, levels = determinant_label_levels),
    dimension = factor(dimension, levels = dimension_label_levels)
  )


# 10. BOXPLOTS DE 32 DETERMINANTES POR ETAPA
create_32det_boxplot <- function(data, etapa_filtrada, titulo) {
  
  data_filtrada <- data %>%
    filter(etapa == etapa_filtrada) %>%
    mutate(
      determinant = reorder(determinant, value, median)
    )
  
  ggplot(data_filtrada, aes(x = determinant, y = value)) +
    geom_boxplot(
      fill = box_fill,
      color = box_color,
      outlier.color = outlier_color,
      outlier.alpha = 0.45
    ) +
    labs(
      title = titulo,
      subtitle = make_subtitle(data_filtrada),
      x = "Determinante",
      y = "Valor"
    ) +
    theme_boxplot
}

#plot_32_general <- create_32det_boxplot(
#  df_all_long,
#  "General",
#  "Distribución de los 32 determinantes - General"
#)

plot_32_implementada <- create_32det_boxplot(
  df_all_long,
  "Implementada",
  "Distribución de los 32 determinantes - Implementada"
)

plot_32_consideraria <- create_32det_boxplot(
  df_all_long,
  "La conoce / la consideraría",
  "Distribución de los 32 determinantes - La conoce / la consideraría"
)

plot_32_curiosidad <- create_32det_boxplot(
  df_all_long,
  "No la conoce, pero le genera curiosidad",
  "Distribución de los 32 determinantes - No la conoce, pero le genera curiosidad"
)

plots_32 <- list(
  #general = plot_32_general,
  implementada = plot_32_implementada,
  consideraria = plot_32_consideraria,
  curiosidad = plot_32_curiosidad
)

walk2(
  plots_32,
  names(plots_32),
  ~ save_plot(.x, paste0("boxplot_32det_", .y), width = 16, height = 9)
)

pdf(
  file = file.path(pdf_dir, "boxplots_32det_individuales.pdf"),
  width = 16,
  height = 9
)

for (p in plots_32) {
  print(p)
}

dev.off()


# 11. COMPARACIÓN 2x2
plot_32_comparison_2x2 <- grid.arrange(
  plot_32_general,
  plot_32_implementada,
  plot_32_consideraria,
  plot_32_curiosidad,
  ncol = 2,
  nrow = 2,
  top = textGrob(
    "Comparación de los 32 determinantes por etapa",
    gp = gpar(fontsize = plot_title_size, fontface = "bold")
  )
)

ggsave(
  filename = file.path(plots_dir, "boxplots_32det_comparacion_2x2.png"),
  plot = plot_32_comparison_2x2,
  width = 24,
  height = 17,
  dpi = 300
)

ggsave(
  filename = file.path(plots_dir, "boxplots_32det_comparacion_2x2.pdf"),
  plot = plot_32_comparison_2x2,
  width = 24,
  height = 17
)



# 12. COMPARACIÓN POR DETERMINANTE
plot_determinants_by_stage <- df_all_long %>%
  ggplot(aes(x = etapa, y = value)) +
  geom_boxplot(
    fill = box_fill,
    color = box_color,
    outlier.color = outlier_color,
    outlier.alpha = 0.3
  ) +
  facet_wrap(~ determinant, scales = "free_y") +
  labs(
    title = "Comparación de determinantes por etapa",
    subtitle = make_subtitle(df_all_long),
    x = "Etapa",
    y = "Valor"
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
  filename = file.path(plots_dir, "boxplot_comparison_by_determinant.pdf"),
  plot = plot_determinants_by_stage,
  width = 20,
  height = 15
)

# 13. BOXPLOTS POR PERFIL: DETERMINANTES POR ETAPA CON COLOR DE RANKING
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
      is.na(self_response_raw) ~ "Faltante",
      str_detect(self_response_raw, regex("environmental impact", ignore_case = TRUE)) ~ "Activista",
      str_detect(self_response_raw, regex("safety", ignore_case = TRUE)) ~ "Temeroso/a",
      str_detect(self_response_raw, regex("social status", ignore_case = TRUE)) ~ "Influencer",
      str_detect(self_response_raw, regex("comfort", ignore_case = TRUE)) ~ "Cuidadoso/a",
      str_detect(self_response_raw, regex("not very interested", ignore_case = TRUE)) ~ "Desinteresado/a",
      str_detect(self_response_raw, regex("early adopter", ignore_case = TRUE)) ~ "Pionero",
      str_detect(self_response_raw, regex("ethical", ignore_case = TRUE)) ~ "Sensible",
      str_detect(self_response_raw, regex("cost-effective", ignore_case = TRUE)) ~ "Homo economicus",
      str_detect(self_response_raw, regex("[NΝ]one of the above", ignore_case = TRUE)) ~ "Sin clasificar",
      TRUE ~ "Otro"
    )
  ) %>%
  select(participant_id, self_profile)

stage_order_profile <- c(
  "No la conoce, pero le genera curiosidad",
  "La conoce / la consideraría",
  "Implementada"
)

rank_colors <- c(
  "1.º" = "#8B0000",
  "2.º" = "#D7191C",
  "3.º" = "#F46D43",
  "4.º" = "#FDAE61",
  "5.º" = "#FEE08B",
  "6.º-10.º" = "#D9EF8B",
  "11.º-15.º" = "#A6D96A",
  "Resto" = "#1A9641"
)

rank_levels <- c(
  "1.º",
  "2.º",
  "3.º",
  "4.º",
  "5.º",
  "6.º-10.º",
  "11.º-15.º",
  "Resto"
)

df_all_long_profile <- df_all_long %>%
  left_join(df_self_profile, by = "participant_id") %>%
  filter(
    etapa != "General",
    !is.na(self_profile),
    !self_profile %in% c("Faltante", "Otro", "Sin clasificar")
  ) %>%
  mutate(
    etapa = factor(etapa, levels = stage_order_profile),
    determinant = as.character(determinant)
  )

ranking_by_profile_stage <- df_all_long_profile %>%
  group_by(self_profile, etapa, determinant) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(self_profile, etapa) %>%
  arrange(desc(mean_value), .by_group = TRUE) %>%
  mutate(
    rank_stage = row_number(),
    rank_group = case_when(
      rank_stage == 1 ~ "1.º",
      rank_stage == 2 ~ "2.º",
      rank_stage == 3 ~ "3.º",
      rank_stage == 4 ~ "4.º",
      rank_stage == 5 ~ "5.º",
      rank_stage <= 10 ~ "6.º-10.º",
      rank_stage <= 15 ~ "11.º-15.º",
      TRUE ~ "Resto"
    )
  ) %>%
  ungroup()

df_all_long_profile_ranked <- df_all_long_profile %>%
  left_join(
    ranking_by_profile_stage %>%
      select(self_profile, etapa, determinant, mean_value, rank_stage, rank_group),
    by = c("self_profile", "etapa", "determinant")
  ) %>%
  mutate(
    rank_group = factor(rank_group, levels = rank_levels),
    determinant = factor(determinant, levels = determinant_label_levels)
  )

plots_profile_stage_rank <- list()

profiles_to_plot <- df_all_long_profile_ranked %>%
  pull(self_profile) %>%
  unique()

cat("Perfiles detectados para gráficos de ranking:\n")
print(profiles_to_plot)

cat("Número de filas en df_all_long_profile_ranked:", nrow(df_all_long_profile_ranked), "\n")

for (prof in profiles_to_plot) {
  
  data_prof <- df_all_long_profile_ranked %>%
    filter(self_profile == prof) %>%
    mutate(
      determinant = factor(
        determinant,
        levels = determinant_label_levels
      )
    )
  
  plot_prof <- data_prof %>%
    ggplot(aes(x = etapa, y = value, fill = rank_group)) +
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
      title = paste("Determinantes por etapa - Perfil:", prof),
      subtitle = paste0(
        "Color según ranking del determinante dentro de cada etapa y perfil. ",
        "n participantes = ", n_distinct(data_prof$participant_id),
        "; observaciones válidas = ", nrow(data_prof)
      ),
      x = "Etapa",
      y = "Valor",
      fill = "Ranking"
    ) +
    theme_boxplot_facets
  
  print(plot_prof)
  
  filename <- paste0(
    "boxplot_profile_stage_rank_",
    clean_filename(prof)
  )
  
  ggsave(
    file.path(profile_rank_dir, paste0(filename, ".png")),
    plot_prof,
    width = 22,
    height = 16,
    dpi = 300
  )
  
  ggsave(
    file.path(profile_rank_dir, paste0(filename, ".pdf")),
    plot_prof,
    width = 22,
    height = 16
  )
  
  plots_profile_stage_rank[[prof]] <- plot_prof
}

pdf(
  file = file.path(profile_rank_dir, "boxplots_by_profile_stage_rank_ALL.pdf"),
  width = 22,
  height = 16,
  onefile = TRUE
)

for (p in plots_profile_stage_rank) {
  print(p)
}

dev.off()


# 14. BOXPLOT AGREGADO POR DIMENSIÓN
plot_dimensions_by_stage <- df_all_long %>%
  ggplot(aes(x = etapa, y = value)) +
  geom_boxplot(
    fill = box_fill,
    color = box_color,
    outlier.color = outlier_color,
    outlier.alpha = 0.3
  ) +
  facet_wrap(~ dimension, scales = "free_y") +
  labs(
    title = "Distribución de valores por dimensión y etapa",
    subtitle = make_subtitle(df_all_long),
    x = "Etapa",
    y = "Valor"
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
  filename = file.path(plots_dir, "boxplot_dimensions_by_stage.pdf"),
  plot = plot_dimensions_by_stage,
  width = 16,
  height = 10
)

# 15. 32 DETERMINANTES RESALTANDO CADA DIMENSIÓN
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
        paste0("Pertenece a la dimensión: ", dim_i),
        "Otros determinantes"
      ),
      determinant = reorder(determinant, value, median)
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
    facet_wrap(~ etapa, ncol = 2) +
    scale_fill_manual(
      values = setNames(
        c("#0072B2", "#D9D9D9"),
        c(
          paste0("Pertenece a la dimensión: ", dim_i),
          "Otros determinantes"
        )
      )
    ) +
    labs(
      title = paste("32 determinantes resaltando dimensión:", dim_i),
      subtitle = paste0(
        make_subtitle(data_dim_highlight),
        "; determinantes resaltados = ", length(determinants_dim_i)
      ),
      x = "Determinante",
      y = "Valor",
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
    filename = file.path(plots_dir, paste0(filename, ".pdf")),
    plot = plot_dim_32det,
    width = 20,
    height = 12
  )
  
  plots_32det_by_dimension[[dim_i]] <- plot_dim_32det
}

pdf(
  file = file.path(pdf_dir, "boxplots_32det_resaltando_dimensiones.pdf"),
  width = 20,
  height = 12
)

for (p in plots_32det_by_dimension) {
  print(p)
}

dev.off()

# 16. PDF FINAL CON TODO
pdf(
  file = file.path(pdf_dir, "boxplots_ttm_determinants_TODO.pdf"),
  width = 16,
  height = 10
)

for (p in plots_32) {
  print(p)
}

grid.arrange(
  plot_32_general,
  plot_32_implementada,
  plot_32_consideraria,
  plot_32_curiosidad,
  ncol = 2,
  nrow = 2,
  top = textGrob(
    "Comparación de los 32 determinantes por etapa",
    gp = gpar(fontsize = plot_title_size, fontface = "bold")
  )
)

print(plot_determinants_by_stage)
print(plot_dimensions_by_stage)

for (p in plots_32det_by_dimension) {
  print(p)
}

dev.off()


# 17. RESÚMENES NUMÉRICOS
summary_by_stage_determinant <- df_all_long %>%
  group_by(etapa, determinant) %>%
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
  file.path(csv_dir, "summary_boxplots_by_stage_determinant.csv")
)

summary_by_stage_dimension <- df_all_long %>%
  group_by(etapa, dimension) %>%
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
  file.path(csv_dir, "summary_boxplots_by_stage_dimension.csv")
)

summary_by_stage_dimension_determinant <- df_all_long %>%
  group_by(etapa, dimension, determinant) %>%
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
  file.path(csv_dir, "summary_boxplots_by_stage_dimension_determinant.csv")
)

cat("Boxplots generados en:", plots_dir, "\n")