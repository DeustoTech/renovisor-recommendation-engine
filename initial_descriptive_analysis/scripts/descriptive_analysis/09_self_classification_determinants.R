# ==============================================================================
# SCRIPT 09 - AUTOCLASIFICACIÓN 4.3 Y PERFIL DE DETERMINANTES
# ==============================================================================

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
# 1. CARGAR DATOS YA LIMPIOS
# ==============================================================================

df <- read_csv(
  "initial_descriptive_analysis/output/clean_datasets/df_clean_general.csv",
  show_col_types = FALSE
)

glimpse(df)


# ==============================================================================
# 2. ASEGURAR PARTICIPANT_ID
# ==============================================================================

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

cat("Filas usadas en Script 09:", nrow(df), "\n")


# ==============================================================================
# 3. DICCIONARIO ROBUSTO DE DETERMINANTES EN CASTELLANO
# ==============================================================================

determinant_dictionary <- tribble(
  ~determinant_id, ~determinant_label, ~determinant_prefix,
  "profits", "Beneficio económico", "profits",
  "credit_score_access_to_funding", "Acceso a financiación", "credit_score_access_to_funding",
  "risk_profile", "Perfil de riesgo", "risk_profile",
  "added_value", "Valor añadido", "added_value",
  "frugality", "Frugalidad", "frugality",
  "climate_protection", "Protección climática", "climate_protection",
  "legal", "Cumplimiento legal", "legal",
  "trust", "Confianza", "trust",
  "safety", "Seguridad", "safety",
  "cost_efficiency", "Eficiencia de costes", "cost_efficiency",
  "knowledge", "Conocimiento", "knowledge",
  "own_competence", "Competencia propia", "own_competence",
  "technical_fit", "Adecuación técnica", "technical_fit",
  "environmental_concerns", "Preocupación ambiental", "environmental_concerns",
  "self_satisfaction", "Satisfacción personal", "self_satisfaction",
  "commitment", "Compromiso", "commitment",
  "adherence", "Adherencia", "adherence",
  "autarky", "Autosuficiencia", "autonomy",
  "wellbeing", "Bienestar", "wellbeing",
  "coziness", "Confort", "coziness",
  "rights_and_duties", "Derechos y deberes", "rights_and_duties",
  "peer_pressure", "Presión social", "peer_pressure",
  "support", "Apoyo social", "support",
  "socialising", "Socialización", "socialising",
  "agreement", "Acuerdo", "agreement",
  "novelty", "Novedad", "novelty",
  "fun", "Diversión", "fun",
  "recognition", "Reconocimiento", "recognition",
  "trends", "Tendencias", "trends",
  "authority", "Autoridad", "authority",
  "approval", "Aprobación", "approval",
  "own_significance", "Significado personal", "own_significance"
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
      "Faltan columnas de determinantes en el dataset: ",
      paste(missing_determinants$determinant_id, collapse = ", ")
    )
  )
}

determinant_cols <- determinant_dictionary$determinant_col
determinant_label_levels <- determinant_dictionary$determinant_label

cat("Número de determinantes detectados:", length(determinant_cols), "\n")
print(determinant_dictionary %>% select(determinant_id, determinant_label, determinant_col))


# ==============================================================================
# 4. DETECTAR COLUMNA DE AUTOCLASIFICACIÓN 4.3
# ==============================================================================

self_col_candidates <- c(
  "which_statement_best_describes_you_when_making_an_investment_decision_related_to_your_household_final",
  "which_statement_best_describes_you_when_making_an_investment_decision_related_to_your_household"
)

self_col <- intersect(self_col_candidates, names(df))[1]

if (is.na(self_col)) {
  stop("No se encuentra la columna de autoclasificación 4.3 en el dataset.")
}

cat("Columna usada para autoclasificación:", self_col, "\n")


# ==============================================================================
# 5. CARPETAS DE SALIDA
# ==============================================================================

base_output_dir <- "initial_descriptive_analysis/output/self_classification"

csv_dir <- file.path(base_output_dir, "csv")
plots_dir <- file.path(base_output_dir, "plots")
pdf_dir <- file.path(base_output_dir, "pdf")

dir.create(csv_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(pdf_dir, showWarnings = FALSE, recursive = TRUE)

excluded_plot_profiles <- c("Faltante", "Sin clasificar", "Otro")


# ==============================================================================
# 6. CONFIGURACIÓN VISUAL PARA GRÁFICOS DEL TFM
# ==============================================================================

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


# ==============================================================================
# 7. FUNCIONES AUXILIARES
# ==============================================================================

make_subtitle_self <- function(data) {
  paste0(
    "n participantes = ", n_distinct(data$participant_id),
    "; observaciones participante-determinante válidas = ", nrow(data)
  )
}

save_plot <- function(plot, filename, width = 12, height = 7) {
  ggsave(
    filename = file.path(plots_dir, paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
  
  ggsave(
    filename = file.path(pdf_dir, paste0(filename, ".pdf")),
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


# ==============================================================================
# 8. RECODIFICAR AUTOCLASIFICACIÓN 4.3 EN CASTELLANO
# ==============================================================================

df_self <- df %>%
  mutate(
    self_response_raw = as.character(.data[[self_col]]),
    self_response_raw = str_squish(str_trim(self_response_raw)),
    self_response_raw = na_if(self_response_raw, ""),
    
    self_profile = case_when(
      is.na(self_response_raw) ~ "Faltante",
      str_detect(self_response_raw, regex("environmental impact", ignore_case = TRUE)) ~ "Activista",
      str_detect(self_response_raw, regex("safety", ignore_case = TRUE)) ~ "Temeroso/a",
      str_detect(self_response_raw, regex("social status", ignore_case = TRUE)) ~ "Influyente",
      str_detect(self_response_raw, regex("comfort", ignore_case = TRUE)) ~ "Cuidadoso/a",
      str_detect(self_response_raw, regex("not very interested", ignore_case = TRUE)) ~ "Desinteresado/a",
      str_detect(self_response_raw, regex("early adopter", ignore_case = TRUE)) ~ "Pionero",
      str_detect(self_response_raw, regex("ethical", ignore_case = TRUE)) ~ "Sensible",
      str_detect(self_response_raw, regex("cost-effective", ignore_case = TRUE)) ~ "Homo economicus",
      str_detect(self_response_raw, regex("[NΝ]one of the above", ignore_case = TRUE)) ~ "Sin clasificar",
      TRUE ~ "Otro"
    )
  )


# ==============================================================================
# 9. TABLA LARGA DE DETERMINANTES + AUTOCLASIFICACIÓN
# ==============================================================================

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
    )
  )

write_csv(
  determinants_self_long,
  file.path(csv_dir, "determinants_by_self_classification_long.csv")
)

determinants_self_long_plot <- determinants_self_long %>%
  filter(!self_profile %in% excluded_plot_profiles)


# ==============================================================================
# 10. DISTRIBUCIÓN DE AUTOCLASIFICACIÓN
# ==============================================================================

self_profile_counts <- df_self %>%
  count(self_profile, name = "n_participants") %>%
  arrange(desc(n_participants))

write_csv(
  self_profile_counts,
  file.path(csv_dir, "self_classification_counts.csv")
)

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
    size = plot_label_size
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
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
  width = 10,
  height = 7
)


# ==============================================================================
# 11. MEDIA DE DETERMINANTES POR PERFIL
# ==============================================================================

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
  file.path(csv_dir, "summary_self_profile_determinants.csv")
)

summary_self_determinants_plot <- summary_self_determinants %>%
  filter(!self_profile %in% excluded_plot_profiles)


# ==============================================================================
# 12. HEATMAP PERFIL X DETERMINANTES
# ==============================================================================

plot_heatmap_self <- summary_self_determinants_plot %>%
  mutate(
    self_profile = factor(
      self_profile,
      levels = self_profile_counts_plot$self_profile
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
    title = "Perfil medio de determinantes según autoclasificación",
    subtitle = make_subtitle_self(determinants_self_long_plot),
    x = "Perfil autoclasificado",
    y = "Determinante",
    fill = "Media"
  ) +
  theme_self_heatmap

print(plot_heatmap_self)

save_plot(
  plot_heatmap_self,
  "heatmap_self_classification_determinants",
  width = 14,
  height = 12
)


# ==============================================================================
# 13. HEATMAP DE RANKING DENTRO DE CADA PERFIL
# ==============================================================================

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

plot_heatmap_self_ranked <- summary_self_determinants_plot %>%
  group_by(self_profile) %>%
  arrange(desc(mean), desc(n), determinant_id, .by_group = TRUE) %>%
  mutate(
    rank_in_profile = row_number(),
    rank_group = case_when(
      rank_in_profile == 1 ~ "1.º",
      rank_in_profile == 2 ~ "2.º",
      rank_in_profile == 3 ~ "3.º",
      rank_in_profile == 4 ~ "4.º",
      rank_in_profile == 5 ~ "5.º",
      rank_in_profile <= 10 ~ "6.º-10.º",
      rank_in_profile <= 15 ~ "11.º-15.º",
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
    name = "Posición dentro del perfil"
  ) +
  labs(
    title = "Clasificación de determinantes dentro de cada perfil",
    subtitle = paste0(
      "Primeras posiciones con color individual; resto agrupado por posición. ",
      make_subtitle_self(determinants_self_long_plot)
    ),
    x = "Perfil autoclasificado",
    y = "Determinante"
  ) +
  theme_self_heatmap

print(plot_heatmap_self_ranked)

save_plot(
  plot_heatmap_self_ranked,
  "heatmap_self_classification_determinants_ranked",
  width = 14,
  height = 12
)


# ==============================================================================
# 14. BOXPLOTS DE TODOS LOS DETERMINANTES POR AUTOCLASIFICACIÓN
# ==============================================================================

plot_boxplots_self_all <- determinants_self_long_plot %>%
  mutate(
    determinant_label = factor(
      determinant_label,
      levels = determinant_label_levels
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
    title = "Distribución de determinantes según autoclasificación",
    subtitle = make_subtitle_self(determinants_self_long_plot),
    x = "Perfil autoclasificado",
    y = "Valor"
  ) +
  theme_self

print(plot_boxplots_self_all)

save_plot(
  plot_boxplots_self_all,
  "boxplots_all_determinants_by_self_classification",
  width = 22,
  height = 16
)


# ==============================================================================
# 15. BOXPLOTS DE DETERMINANTES CLAVE
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
  mutate(
    determinant_label = factor(
      determinant_label,
      levels = determinant_label_levels
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
    title = "Determinantes clave según autoclasificación",
    subtitle = make_subtitle_self(
      determinants_self_long_plot %>%
        filter(determinant_id %in% key_determinants)
    ),
    x = "Perfil autoclasificado",
    y = "Valor"
  ) +
  theme_self

print(plot_key_determinants)

save_plot(
  plot_key_determinants,
  "boxplots_key_determinants_by_self_classification",
  width = 18,
  height = 12
)


# ==============================================================================
# 16. BOXPLOTS INDIVIDUALES POR PERFIL AUTOCLASIFICADO
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
      title = paste("Boxplot de determinantes -", prof),
      subtitle = make_subtitle_self(profile_data_box),
      x = "Determinante",
      y = "Valor"
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


# ==============================================================================
# 17. PERFILES MEDIOS DE DETERMINANTES POR AUTOCLASIFICACIÓN
# ==============================================================================

profiles <- summary_self_determinants_plot %>%
  pull(self_profile) %>%
  unique()

profile_plots <- list()

for (prof in profiles) {
  
  profile_data <- summary_self_determinants_plot %>%
    filter(self_profile == prof) %>%
    arrange(desc(mean))
  
  filename_clean <- clean_filename(prof)
  
  write_csv(
    profile_data,
    file.path(csv_dir, paste0(filename_clean, "_determinant_profile.csv"))
  )
  
  plot_profile <- profile_data %>%
    mutate(
      determinant_label = reorder(determinant_label, mean)
    ) %>%
    ggplot(aes(x = determinant_label, y = mean)) +
    geom_col(fill = bar_fill, color = bar_color) +
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


# ==============================================================================
# 18. PDF MULTIPÁGINA CON PERFILES MEDIOS
# ==============================================================================

pdf(
  file = file.path(pdf_dir, "all_self_profiles_determinants.pdf"),
  width = 12,
  height = 10
)

for (p in profile_plots) {
  print(p)
}

dev.off()


# ==============================================================================
# 19. PDF MULTIPÁGINA CON BOXPLOTS POR PERFIL
# ==============================================================================

pdf(
  file = file.path(pdf_dir, "all_self_profiles_boxplots.pdf"),
  width = 12,
  height = 10
)

for (p in profile_boxplot_list) {
  print(p)
}

dev.off()


# ==============================================================================
# 20. COMPARACIONES 2x2
# ==============================================================================

if (length(profile_plots) > 0) {
  comparison_profiles <- marrangeGrob(
    grobs = profile_plots,
    nrow = 2,
    ncol = 2,
    top = textGrob(
      "Perfiles medios de determinantes",
      gp = gpar(fontsize = plot_title_size, fontface = "bold")
    )
  )
  
  ggsave(
    filename = file.path(plots_dir, "all_self_profiles_comparison.pdf"),
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
      "Boxplots de determinantes por perfil autoclasificado",
      gp = gpar(fontsize = plot_title_size, fontface = "bold")
    )
  )
  
  ggsave(
    filename = file.path(plots_dir, "all_self_profiles_boxplots_comparison.pdf"),
    plot = comparison_profile_boxplots,
    width = 18,
    height = 13
  )
}


# ==============================================================================
# 21. PDF FINAL CON TODOS LOS GRÁFICOS DEL SCRIPT 09
# ==============================================================================

pdf(
  file = file.path(pdf_dir, "self_classification_determinants_TODO.pdf"),
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


# ==============================================================================
# 22. MENSAJES FINALES
# ==============================================================================

cat("Análisis de autoclasificación generado en:", plots_dir, "\n")
cat("CSV completos guardados incluyendo Faltante, Sin clasificar y Otro.\n")
cat("Gráficos generados excluyendo Faltante, Sin clasificar y Otro.\n")
