# SCRIPT 09 - AUTOCLASIFICACIÓN 4.3 Y PERFIL DE DETERMINANTES

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tibble)
library(purrr)
library(gridExtra)


# 1. Cargar datos
df <- read_csv(
  "initial_descriptive_analysis/output/df_analysis_ready.csv",
  show_col_types = FALSE
)

glimpse(df)


# 2. Crear participant_id
df <- df %>%
  mutate(
    participant_id = coalesce(
      as.character(join_key),
      as.character(prolific_id),
      as.character(identification_code),
      as.character(row_number())
    )
  )

# 2B. Filtrar attention checks --------------------------------------------

att_42_col <- "please_follow_the_instruction_below_when_selecting_your_answer_select_42"
att_4_col  <- "please_follow_the_instruction_below_when_selecting_your_answer_select_option_4"

n_before_attention <- nrow(df)

df <- df %>%
  filter(
    if (att_42_col %in% names(.)) {
      is.na(.data[[att_42_col]]) |
        str_detect(as.character(.data[[att_42_col]]), "^42")
    } else {
      TRUE
    },
    if (att_4_col %in% names(.)) {
      is.na(.data[[att_4_col]]) |
        str_detect(as.character(.data[[att_4_col]]), "^Option 4")
    } else {
      TRUE
    }
  )

cat("Filas antes de attention checks:", n_before_attention, "\n")
cat("Filas tras attention checks:", nrow(df), "\n")
cat("Filas eliminadas:", n_before_attention - nrow(df), "\n")

# ==============================================================================
# 3. Definir columnas de determinantes
# ==============================================================================

determinant_cols <- names(df)[8:39]

cat("Número de determinantes:", length(determinant_cols), "\n")
print(determinant_cols)

# ==============================================================================
# 4. Detectar columna de la pregunta 4.3
# ==============================================================================

self_col <- "which_statement_best_describes_you_when_making_an_investment_decision_related_to_your_household_final"

cat("Columna usada para autoclasificación:", self_col, "\n")

# ==============================================================================
# 5. Carpeta de salida
# ==============================================================================

plots_dir <- "initial_descriptive_analysis/output/self_classification_plots"

dir.create(
  plots_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

# Perfiles que se guardan en CSV pero NO salen en gráficos
excluded_plot_profiles <- c("Missing", "Unclassified", "Other")

make_subtitle_self <- function(data) {
  paste0(
    "n participantes = ", n_distinct(data$participant_id),
    "; observaciones participante-determinante válidas = ", nrow(data)
  )
}

# ==============================================================================
# 6. Estilo gráfico
# ==============================================================================

box_fill <- "#BDE3FF"
box_color <- "#2C3E50"
outlier_color <- "#4F4F4F"
bar_fill <- "#BDE3FF"
bar_color <- "#2C3E50"

theme_self <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

save_plot <- function(plot, filename, width = 12, height = 7) {
  ggsave(
    filename = file.path(plots_dir, paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}

# ==============================================================================
# 7. Diccionario de determinantes
# ==============================================================================

determinant_dictionary <- tibble(
  determinant_col = determinant_cols,
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
    "Persistencia",
    "Autosuficiencia",
    "Bienestar",
    "Confort",
    "Derechos y deberes",
    "Presión social",
    "Apoyo social",
    "Sociabilidad",
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

# ==============================================================================
# 8. Limpiar escala de determinantes
# ==============================================================================

clean_determinant_score <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  
  case_when(
    is.na(x) ~ NA_real_,
    x >= 0 & x <= 100 ~ x,
    TRUE ~ NA_real_
  )
}

# ==============================================================================
# 9. Recodificar autoclasificación 4.3
# ==============================================================================

df_self <- df %>%
  mutate(
    self_response_raw = as.character(.data[[self_col]]),
    self_response_raw = str_squish(str_trim(self_response_raw)),
    self_response_raw = na_if(self_response_raw, ""),
    
    self_profile = case_when(
      is.na(self_response_raw) ~ "Missing",
      
      str_detect(
        self_response_raw,
        regex("environmental impact", ignore_case = TRUE)
      ) ~ "Activist",
      
      str_detect(
        self_response_raw,
        regex("safety", ignore_case = TRUE)
      ) ~ "Fearful",
      
      str_detect(
        self_response_raw,
        regex("social status", ignore_case = TRUE)
      ) ~ "Influencer",
      
      str_detect(
        self_response_raw,
        regex("comfort", ignore_case = TRUE)
      ) ~ "Careful",
      
      str_detect(
        self_response_raw,
        regex("not very interested", ignore_case = TRUE)
      ) ~ "Uninterested",
      
      str_detect(
        self_response_raw,
        regex("early adopter", ignore_case = TRUE)
      ) ~ "Early Adopter",
      
      str_detect(
        self_response_raw,
        regex("ethical", ignore_case = TRUE)
      ) ~ "Sentient",
      
      str_detect(
        self_response_raw,
        regex("cost-effective", ignore_case = TRUE)
      ) ~ "Homo Economicus",
      
      str_detect(
        self_response_raw,
        regex("[NΝ]one of the above", ignore_case = TRUE)
      ) ~ "Unclassified",
      
      TRUE ~ "Other"
    )
  )

# ==============================================================================
# 10. Tabla larga de determinantes + autoclasificación
# ==============================================================================

determinants_self_long <- df_self %>%
  select(
    participant_id,
    self_response_raw,
    self_profile,
    all_of(determinant_cols)
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
    determinant_dictionary,
    by = "determinant_col"
  ) %>%
  filter(!is.na(response_numeric))

# CSV completo: aquí SÍ se guardan Missing, Unclassified y Other
write_csv(
  determinants_self_long,
  "initial_descriptive_analysis/output/determinants_by_self_classification_long.csv"
)

# Dataset solo para gráficos
determinants_self_long_plot <- determinants_self_long %>%
  filter(!self_profile %in% excluded_plot_profiles)

# ==============================================================================
# 11. Distribución de autoclasificación
# ==============================================================================

self_profile_counts <- df_self %>%
  count(self_profile, name = "n_participants") %>%
  arrange(desc(n_participants))

# CSV completo
write_csv(
  self_profile_counts,
  "initial_descriptive_analysis/output/self_classification_counts.csv"
)

# Tabla solo para gráfico
self_profile_counts_plot <- self_profile_counts %>%
  filter(!self_profile %in% excluded_plot_profiles)

plot_self_counts <- self_profile_counts_plot %>%
  mutate(
    self_profile = reorder(self_profile, n_participants)
  ) %>%
  ggplot(aes(x = self_profile, y = n_participants)) +
  geom_col(fill = bar_fill, color = bar_color) +
  geom_text(
    aes(label = n_participants),
    hjust = -0.2,
    size = 3.5
  ) +
  coord_flip() +
  labs(
    title = "Distribución de perfiles autoclasificados",
    subtitle = paste0(
      "n participantes clasificados = ",
      sum(self_profile_counts_plot$n_participants)
    ),
    x = "Perfil autoclasificado",
    y = "Número de participantes"
  ) +
  theme_self

print(plot_self_counts)

save_plot(
  plot_self_counts,
  "self_classification_counts",
  width = 9,
  height = 6
)

# ==============================================================================
# 12. Media de determinantes por perfil
# ==============================================================================

summary_self_determinants <- determinants_self_long %>%
  group_by(
    self_profile,
    determinant_id,
    determinant_label
  ) %>%
  summarise(
    n = n(),
    mean = mean(response_numeric, na.rm = TRUE),
    median = median(response_numeric, na.rm = TRUE),
    sd = sd(response_numeric, na.rm = TRUE),
    .groups = "drop"
  )

# CSV completo
write_csv(
  summary_self_determinants,
  "initial_descriptive_analysis/output/summary_self_profile_determinants.csv"
)

# Resumen solo para gráficos
summary_self_determinants_plot <- summary_self_determinants %>%
  filter(!self_profile %in% excluded_plot_profiles)

# ==============================================================================
# 13. Heatmap perfil x determinantes
# ==============================================================================

plot_heatmap_self <- summary_self_determinants_plot %>%
  mutate(
    self_profile = factor(
      self_profile,
      levels = self_profile_counts_plot$self_profile
    ),
    determinant_label = factor(
      determinant_label,
      levels = rev(determinant_dictionary$determinant_label)
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
    title = "Perfil medio de determinantes según autoclasificación",
    subtitle = make_subtitle_self(determinants_self_long_plot),
    x = "Perfil autoclasificado",
    y = "Determinante",
    fill = "Media"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )

print(plot_heatmap_self)

save_plot(
  plot_heatmap_self,
  "heatmap_self_classification_determinants",
  width = 12,
  height = 10
)

ggsave(
  filename = file.path(plots_dir, "heatmap_self_classification_determinants.pdf"),
  plot = plot_heatmap_self,
  width = 12,
  height = 10
)

# ==============================================================================
# 13B. Heatmap perfil x determinantes por ranking
# ==============================================================================
# ==============================================================================
# 13B. Heatmap perfil x determinantes por ranking DENTRO DE CADA PERFIL
# ==============================================================================

rank_colors <- c(
  "Top 1" = "#8B0000",
  "Top 2" = "#D7191C",
  "Top 3" = "#F46D43",
  "Top 4" = "#FDAE61",
  "Top 5" = "#FEE08B",
  "Top 6-10" = "#D9EF8B",
  "Top 11-15" = "#A6D96A",
  "Resto" = "#1A9641"
)

plot_heatmap_self_ranked <- summary_self_determinants_plot %>%
  group_by(self_profile) %>%
  arrange(desc(mean), desc(n), determinant_id, .by_group = TRUE) %>%
  mutate(
    rank_in_profile = row_number(),
    rank_group = case_when(
      rank_in_profile == 1 ~ "Top 1",
      rank_in_profile == 2 ~ "Top 2",
      rank_in_profile == 3 ~ "Top 3",
      rank_in_profile == 4 ~ "Top 4",
      rank_in_profile == 5 ~ "Top 5",
      rank_in_profile <= 10 ~ "Top 6-10",
      rank_in_profile <= 15 ~ "Top 11-15",
      TRUE ~ "Resto"
    )
  ) %>%
  ungroup() %>%
  mutate(
    self_profile = factor(
      self_profile,
      levels = self_profile_counts_plot$self_profile
    ),
    determinant_label = factor(
      determinant_label,
      levels = rev(determinant_dictionary$determinant_label)
    ),
    rank_group = factor(
      rank_group,
      levels = c(
        "Top 1",
        "Top 2",
        "Top 3",
        "Top 4",
        "Top 5",
        "Top 6-10",
        "Top 11-15",
        "Resto"
      )
    )
  ) %>%
  ggplot(
    aes(
      x = self_profile,
      y = determinant_label,
      fill = rank_group
    )
  ) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(
    aes(label = round(mean, 0)),
    size = 2.5
  ) +
  scale_fill_manual(
    values = rank_colors,
    drop = FALSE,
    name = "Ranking dentro del perfil"
  ) +
  labs(
    title = "Ranking de determinantes dentro de cada perfil",
    subtitle = paste0(
      "Top 1-5 con color individual; resto agrupado por ranking. ",
      make_subtitle_self(determinants_self_long_plot)
    ),
    x = "Perfil autoclasificado",
    y = "Determinante"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )

print(plot_heatmap_self_ranked)

save_plot(
  plot_heatmap_self_ranked,
  "heatmap_self_classification_determinants_ranked",
  width = 12,
  height = 10
)

ggsave(
  filename = file.path(
    plots_dir,
    "heatmap_self_classification_determinants_ranked.pdf"
  ),
  plot = plot_heatmap_self_ranked,
  width = 12,
  height = 10
)

# ==============================================================================
# 14. Boxplots de todos los determinantes por autoclasificación
# ==============================================================================

plot_boxplots_self_all <- determinants_self_long_plot %>%
  ggplot(aes(x = self_profile, y = response_numeric)) +
  geom_boxplot(
    fill = box_fill,
    color = box_color,
    outlier.color = outlier_color,
    outlier.alpha = 0.35
  ) +
  facet_wrap(~ determinant_label, scales = "free_y") +
  labs(
    title = "Distribución de determinantes según autoclasificación",
    subtitle = make_subtitle_self(determinants_self_long_plot),
    x = "Perfil autoclasificado",
    y = "Valor"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    strip.text = element_text(size = 7),
    panel.grid.minor = element_blank()
  )

print(plot_boxplots_self_all)

save_plot(
  plot_boxplots_self_all,
  "boxplots_all_determinants_by_self_classification",
  width = 18,
  height = 14
)

ggsave(
  filename = file.path(
    plots_dir,
    "boxplots_all_determinants_by_self_classification.pdf"
  ),
  plot = plot_boxplots_self_all,
  width = 18,
  height = 14
)

# ==============================================================================
# 15. Boxplots de determinantes clave
# ==============================================================================

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
  ggplot(aes(x = self_profile, y = response_numeric)) +
  geom_boxplot(
    fill = box_fill,
    color = box_color,
    outlier.color = outlier_color,
    outlier.alpha = 0.4
  ) +
  facet_wrap(~ determinant_label, scales = "free_y") +
  labs(
    title = "Determinantes clave según autoclasificación",
    subtitle = make_subtitle_self(
      determinants_self_long_plot %>%
        filter(determinant_id %in% key_determinants)
    ),
    x = "Perfil autoclasificado",
    y = "Valor"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    strip.text = element_text(face = "bold", size = 8),
    panel.grid.minor = element_blank()
  )

print(plot_key_determinants)

save_plot(
  plot_key_determinants,
  "boxplots_key_determinants_by_self_classification",
  width = 16,
  height = 10
)

ggsave(
  filename = file.path(
    plots_dir,
    "boxplots_key_determinants_by_self_classification.pdf"
  ),
  plot = plot_key_determinants,
  width = 16,
  height = 10
)

# ==============================================================================
# 15B. BOXPLOTS INDIVIDUALES POR PERFIL AUTOCATEGORIZADO
# ==============================================================================

profiles_boxplots <- determinants_self_long_plot %>%
  pull(self_profile) %>%
  unique()

profile_boxplot_list <- list()

for (prof in profiles_boxplots) {
  
  top_dets_prof <- summary_self_determinants_plot %>%
    filter(self_profile == prof) %>%
    slice_max(order_by = mean, n = 10, with_ties = FALSE) %>%
    pull(determinant_id)
  
  profile_data_box <- determinants_self_long_plot %>%
    filter(
      self_profile == prof,
      determinant_id %in% top_dets_prof
    )
  
  filename_clean <- str_replace_all(
    str_to_lower(prof),
    "[^a-z0-9]+",
    "_"
  )
  
  plot_profile_box <- profile_data_box %>%
    mutate(
      determinant_label = reorder(
        determinant_label,
        response_numeric,
        median,
        na.rm = TRUE
      )
    ) %>%
    ggplot(
      aes(
        x = determinant_label,
        y = response_numeric
      )
    ) +
    geom_boxplot(
      fill = box_fill,
      color = box_color,
      outlier.color = outlier_color,
      outlier.alpha = 0.4
    ) +
    coord_flip() +
    labs(
      title = paste("Boxplot de determinantes -", prof),
      subtitle = make_subtitle_self(profile_data_box),
      x = "Determinante",
      y = "Valor"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.y = element_text(size = 8),
      panel.grid.minor = element_blank()
    )
  
  print(plot_profile_box)
  
  save_plot(
    plot_profile_box,
    paste0(filename_clean, "_determinants_boxplot"),
    width = 10,
    height = 9
  )
  
  ggsave(
    filename = file.path(
      plots_dir,
      paste0(filename_clean, "_determinants_boxplot.pdf")
    ),
    plot = plot_profile_box,
    width = 10,
    height = 9
  )
  
  profile_boxplot_list[[prof]] <- plot_profile_box
}

# ==============================================================================
# 16. PERFILES MEDIOS DE DETERMINANTES POR ARQUETIPO
# ==============================================================================

profiles <- summary_self_determinants_plot %>%
  pull(self_profile) %>%
  unique()

profile_plots <- list()

for (prof in profiles) {
  
  profile_data <- summary_self_determinants_plot %>%
    filter(self_profile == prof) %>%
    arrange(desc(mean))
  
  filename_clean <- str_replace_all(
    str_to_lower(prof),
    "[^a-z0-9]+",
    "_"
  )
  
  write_csv(
    profile_data,
    file.path(
      "initial_descriptive_analysis/output",
      paste0(filename_clean, "_determinant_profile.csv")
    )
  )
  
  plot_profile <- profile_data %>%
    mutate(
      determinant_label = reorder(
        determinant_label,
        mean
      )
    ) %>%
    ggplot(
      aes(
        x = determinant_label,
        y = mean
      )
    ) +
    geom_col(
      fill = bar_fill,
      color = bar_color
    ) +
    coord_flip() +
    labs(
      title = paste("Perfil medio de determinantes -", prof),
      subtitle = paste0(
        "n observaciones válidas = ", sum(profile_data$n),
        "; n determinantes = ", nrow(profile_data)
      ),
      x = "Determinante",
      y = "Media"
    ) +
    theme_self
  
  print(plot_profile)
  
  save_plot(
    plot_profile,
    paste0(filename_clean, "_determinant_profile"),
    width = 9,
    height = 9
  )
  
  ggsave(
    filename = file.path(
      plots_dir,
      paste0(filename_clean, "_determinant_profile.pdf")
    ),
    plot = plot_profile,
    width = 9,
    height = 9
  )
  
  profile_plots[[prof]] <- plot_profile
}

# ==============================================================================
# PDF MULTIPÁGINA CON TODOS LOS PERFILES
# ==============================================================================

pdf(
  file = file.path(
    plots_dir,
    "all_self_profiles_determinants.pdf"
  ),
  width = 10,
  height = 9
)

for (p in profile_plots) {
  print(p)
}

for (p in profile_boxplot_list) {
  print(p)
}

dev.off()

# ==============================================================================
# COMPARACIÓN 2x2
# ==============================================================================

if (length(profile_plots) > 0) {
  
  comparison_profiles <- marrangeGrob(
    grobs = profile_plots,
    nrow = 2,
    ncol = 2,
    top = "Perfiles medios de determinantes"
  )
  
  ggsave(
    filename = file.path(
      plots_dir,
      "all_self_profiles_comparison.pdf"
    ),
    plot = comparison_profiles,
    width = 16,
    height = 12
  )
}

# ==============================================================================
# PDF multipágina con boxplots por perfil
# ==============================================================================

pdf(
  file = file.path(
    plots_dir,
    "all_self_profiles_boxplots.pdf"
  ),
  width = 10,
  height = 9
)

for (p in profile_boxplot_list) {
  print(p)
}

dev.off()

# ==============================================================================
# Comparación 2x2 de boxplots por perfil
# ==============================================================================

if (length(profile_boxplot_list) > 0) {
  
  comparison_profile_boxplots <- marrangeGrob(
    grobs = profile_boxplot_list,
    nrow = 2,
    ncol = 2,
    top = "Boxplots de determinantes por perfil autoclasificado"
  )
  
  ggsave(
    filename = file.path(
      plots_dir,
      "all_self_profiles_boxplots_comparison.pdf"
    ),
    plot = comparison_profile_boxplots,
    width = 16,
    height = 12
  )
}

# ==============================================================================
# PDF FINAL CON TODOS LOS GRÁFICOS DEL SCRIPT 09
# ==============================================================================

pdf(
  file = file.path(
    plots_dir,
    "self_classification_determinants_TODO.pdf"
  ),
  width = 14,
  height = 8
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

cat("Análisis de autoclasificación generado en:", plots_dir, "\n")
cat("CSV completos guardados incluyendo Missing, Unclassified y Other.\n")
cat("Gráficos generados excluyendo Missing, Unclassified y Other.\n")