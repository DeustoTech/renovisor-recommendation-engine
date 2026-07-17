
# Objetivo:
# Crear gráficos presentables del estado actual del dataset:
# - fuentes
# - calidad
# - determinantes
# - fases/dimensiones
# - propensity/bootstrap preliminar
#
suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(stringr)
  library(ggplot2)
})

processed_root <- "paper1_cluster/data/processed"

summary_dir <- file.path(
  processed_root,
  "03_3_summary_dataset_status"
)

propensity_dir <- file.path(
  processed_root,
  "04_propensity_score_and_bootstrap"
)

figures <- "paper1_cluster/figures/processed"

out_dir <- file.path(
  figures,
  "04_1_figures_current_dataset_status"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

save_plot <- function(p, filename, width = 10, height = 6) {
  ggsave(
    filename = file.path(out_dir, filename),
    plot = p,
    width = width,
    height = height,
    dpi = 300
  )
}

read_if_exists <- function(path) {
  if (!file.exists(path)) {
    warning("No existe: ", path)
    return(NULL)
  }
  
  read_csv(path, show_col_types = FALSE)
}

# ============================================================
# 1. Leer resúmenes
# ============================================================

summary_00 <- read_if_exists(file.path(summary_dir, "summary_00_files_dimensions.csv"))
summary_01 <- read_if_exists(file.path(summary_dir, "summary_01_sources.csv"))
summary_02 <- read_if_exists(file.path(summary_dir, "summary_02_quality_by_source.csv"))
summary_03 <- read_if_exists(file.path(summary_dir, "summary_03_quality_counts.csv"))
summary_05 <- read_if_exists(file.path(summary_dir, "summary_05_determinants_by_source.csv"))
summary_09 <- read_if_exists(file.path(summary_dir, "summary_09_propensity_variable_coverage.csv"))
summary_10 <- read_if_exists(file.path(summary_dir, "summary_10_propensity_readiness.csv"))
summary_13 <- read_if_exists(file.path(summary_dir, "summary_13_phase_dimension_frequencies.csv"))
summary_14 <- read_if_exists(file.path(summary_dir, "summary_14_phase_scores.csv"))
summary_15 <- read_if_exists(file.path(summary_dir, "summary_15_dimension_mapping_counts.csv"))

prop_pred <- read_if_exists(file.path(propensity_dir, "diagnostics_propensity_predictions_by_source.csv"))
boot_targets <- read_if_exists(file.path(propensity_dir, "bootstrap_targets_final.csv"))
voter_pool <- read_if_exists(file.path(propensity_dir, "diagnostics_voter_pool_by_group.csv"))
fallbacks <- read_if_exists(file.path(propensity_dir, "bootstrap_fallback_summary.csv"))

# ============================================================
# 2. Gráfico: evolución de dimensiones de archivos
# ============================================================

if (!is.null(summary_00)) {
  p <- summary_00 %>%
    mutate(step = factor(step, levels = step)) %>%
    ggplot(aes(x = step, y = n_cols)) +
    geom_col() +
    geom_text(aes(label = n_cols), vjust = -0.3, size = 3.5) +
    theme_minimal(base_size = 13) +
    labs(
      title = "Dataset construction: number of columns by processing step",
      x = "Processing step",
      y = "Number of columns"
    ) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))
  
  save_plot(p, "01_columns_by_processing_step.png")
}

# ============================================================
# 3. Gráfico: filas por fuente
# ============================================================

if (!is.null(summary_01)) {
  p <- summary_01 %>%
    filter(dataset_source != "TOTAL") %>%
    mutate(
      dataset_source = factor(dataset_source, levels = dataset_source[order(n_rows)])
    ) %>%
    ggplot(aes(x = dataset_source, y = n_rows)) +
    geom_col() +
    geom_text(aes(label = n_rows), hjust = -0.15, size = 4) +
    coord_flip() +
    theme_minimal(base_size = 13) +
    labs(
      title = "Rows by data source",
      x = "Data source",
      y = "Number of rows"
    )
  
  save_plot(p, "02_rows_by_source.png")
}

# ============================================================
# 4. Gráfico: calidad usable por fuente
# ============================================================

if (!is.null(summary_02)) {
  p <- summary_02 %>%
    select(
      dataset_source,
      n_rows,
      n_usable_for_main_analysis
    ) %>%
    pivot_longer(
      cols = c(n_rows, n_usable_for_main_analysis),
      names_to = "metric",
      values_to = "n"
    ) %>%
    mutate(
      metric = recode(
        metric,
        n_rows = "Total rows",
        n_usable_for_main_analysis = "Usable for main analysis"
      )
    ) %>%
    ggplot(aes(x = dataset_source, y = n, fill = metric)) +
    geom_col(position = "dodge") +
    geom_text(
      aes(label = n),
      position = position_dodge(width = 0.9),
      vjust = -0.3,
      size = 3.5
    ) +
    theme_minimal(base_size = 13) +
    labs(
      title = "Usable rows for main analysis by source",
      x = "Data source",
      y = "Number of rows",
      fill = NULL
    )
  
  save_plot(p, "03_usable_rows_by_source.png")
}

# ============================================================
# 5. Gráfico: categorías de calidad
# ============================================================

if (!is.null(summary_03)) {
  p <- summary_03 %>%
    mutate(
      row_quality_final = str_replace_all(row_quality_final, "_", " "),
      row_quality_final = factor(row_quality_final, levels = unique(row_quality_final))
    ) %>%
    ggplot(aes(x = row_quality_final, y = n, fill = dataset_source)) +
    geom_col(position = "dodge") +
    coord_flip() +
    theme_minimal(base_size = 12) +
    labs(
      title = "Final row quality categories",
      x = "Quality category",
      y = "Number of rows",
      fill = "Source"
    )
  
  save_plot(p, "04_quality_categories_by_source.png", width = 11, height = 7)
}

# ============================================================
# 6. Gráfico: determinantes por fuente
# ============================================================

if (!is.null(summary_05)) {
  p <- summary_05 %>%
    select(
      dataset_source,
      n_rows,
      n_with_any_det,
      n_complete_32det,
      n_usable_for_clustering
    ) %>%
    pivot_longer(
      cols = -dataset_source,
      names_to = "metric",
      values_to = "n"
    ) %>%
    mutate(
      metric = recode(
        metric,
        n_rows = "Total rows",
        n_with_any_det = "Any determinant",
        n_complete_32det = "Complete 32 determinants",
        n_usable_for_clustering = "Usable for clustering"
      )
    ) %>%
    ggplot(aes(x = dataset_source, y = n, fill = metric)) +
    geom_col(position = "dodge") +
    theme_minimal(base_size = 13) +
    labs(
      title = "Determinant coverage by source",
      x = "Data source",
      y = "Number of rows",
      fill = NULL
    ) +
    theme(axis.text.x = element_text(angle = 0))
  
  save_plot(p, "05_determinant_coverage_by_source.png", width = 12, height = 6)
}

# ============================================================
# 7. Gráfico: cobertura variables propensity
# ============================================================

if (!is.null(summary_09)) {
  p <- summary_09 %>%
    mutate(
      variable = str_replace_all(variable, "_", " "),
      prop_valid_pct = prop_valid * 100
    ) %>%
    ggplot(aes(x = variable, y = prop_valid_pct, fill = dataset_source)) +
    geom_col(position = "dodge") +
    coord_flip() +
    theme_minimal(base_size = 12) +
    labs(
      title = "Coverage of candidate variables for propensity score",
      x = "Variable",
      y = "% valid",
      fill = "Source"
    )
  
  save_plot(p, "06_propensity_variable_coverage.png", width = 11, height = 7)
}

# ============================================================
# 8. Gráfico: preparación propensity score
# ============================================================

if (!is.null(summary_10)) {
  p <- summary_10 %>%
    select(
      dataset_source,
      n_has_binary_vote_outcome,
      n_model_ready_minimal
    ) %>%
    pivot_longer(
      cols = -dataset_source,
      names_to = "metric",
      values_to = "n"
    ) %>%
    mutate(
      metric = recode(
        metric,
        n_has_binary_vote_outcome = "Binary vote outcome",
        n_model_ready_minimal = "Model-ready minimal"
      )
    ) %>%
    ggplot(aes(x = dataset_source, y = n, fill = metric)) +
    geom_col(position = "dodge") +
    geom_text(
      aes(label = n),
      position = position_dodge(width = 0.9),
      vjust = -0.3,
      size = 3.5
    ) +
    theme_minimal(base_size = 13) +
    labs(
      title = "Propensity score readiness by source",
      x = "Data source",
      y = "Number of rows",
      fill = NULL
    )
  
  save_plot(p, "07_propensity_readiness.png")
}

# ============================================================
# 9. Gráfico: dimensiones seleccionadas por fase
# ============================================================

if (!is.null(summary_13)) {
  p <- summary_13 %>%
    mutate(
      phase_prefix = recode(
        phase_prefix,
        phase_implemented_reasons = "Implemented reasons",
        phase_more_likely_1 = "More likely 1",
        phase_more_likely_2 = "More likely 2"
      ),
      dimension = factor(dimension, levels = rev(unique(dimension)))
    ) %>%
    ggplot(aes(x = dimension, y = n_selected, fill = phase_prefix)) +
    geom_col(position = "dodge") +
    coord_flip() +
    theme_minimal(base_size = 12) +
    labs(
      title = "Selected dimensions by decision phase",
      x = "Dimension",
      y = "Number of selections",
      fill = "Phase"
    )
  
  save_plot(p, "08_selected_dimensions_by_phase.png", width = 11, height = 7)
}

# ============================================================
# 10. Gráfico: scores de dimensiones/fase en RV Decision
# ============================================================

if (!is.null(summary_14)) {
  p <- summary_14 %>%
    filter(dataset_source == "rv", str_detect(source_survey, "decision")) %>%
    mutate(
      score_variable = recode(
        score_variable,
        phase_implemented_reasons_selected_dimension_score_mean = "Implemented reasons",
        phase_more_likely_1_selected_dimension_score_mean = "More likely 1",
        phase_more_likely_2_selected_dimension_score_mean = "More likely 2"
      )
    ) %>%
    ggplot(aes(x = score_variable, y = mean_score)) +
    geom_col() +
    geom_errorbar(
      aes(
        ymin = mean_score - sd_score,
        ymax = mean_score + sd_score
      ),
      width = 0.2
    ) +
    geom_text(aes(label = round(mean_score, 1)), vjust = -0.5, size = 4) +
    theme_minimal(base_size = 13) +
    labs(
      title = "Mean selected-dimension scores in RV Decision",
      x = "Phase",
      y = "Mean score, 0-100"
    )
  
  save_plot(p, "09_phase_dimension_scores_rv_decision.png")
}

# ============================================================
# 11. Gráfico: mapeo dimensiones → determinantes
# ============================================================

if (!is.null(summary_15)) {
  p <- summary_15 %>%
    mutate(dimension = factor(dimension, levels = dimension[order(n_determinants)])) %>%
    ggplot(aes(x = dimension, y = n_determinants)) +
    geom_col() +
    geom_text(aes(label = n_determinants), hjust = -0.2, size = 4) +
    coord_flip() +
    theme_minimal(base_size = 13) +
    labs(
      title = "Number of determinants mapped to each dimension",
      x = "Dimension",
      y = "Number of determinants"
    )
  
  save_plot(p, "10_dimension_determinant_mapping.png")
}

# ============================================================
# 12. Gráfico: predicciones propensity por fuente
# ============================================================

if (!is.null(prop_pred)) {
  p <- prop_pred %>%
    select(dataset_source, mean_p_vote, mean_p_abstention) %>%
    pivot_longer(
      cols = c(mean_p_vote, mean_p_abstention),
      names_to = "probability",
      values_to = "mean_value"
    ) %>%
    mutate(
      probability = recode(
        probability,
        mean_p_vote = "Predicted vote probability",
        mean_p_abstention = "Predicted abstention probability"
      )
    ) %>%
    ggplot(aes(x = dataset_source, y = mean_value, fill = probability)) +
    geom_col(position = "dodge") +
    geom_text(
      aes(label = round(mean_value, 2)),
      position = position_dodge(width = 0.9),
      vjust = -0.3,
      size = 3.5
    ) +
    theme_minimal(base_size = 13) +
    labs(
      title = "Mean predicted voting/abstention probability by source",
      x = "Data source",
      y = "Mean predicted probability",
      fill = NULL
    )
  
  save_plot(p, "11_propensity_predictions_by_source.png")
}

# ============================================================
# 13. Gráfico: targets bootstrap
# ============================================================

if (!is.null(boot_targets)) {
  p <- boot_targets %>%
    mutate(
      target_electoral_group = factor(
        target_electoral_group,
        levels = target_electoral_group[order(target_share_global)]
      ),
      target_share_pct = target_share_global * 100
    ) %>%
    ggplot(aes(x = target_electoral_group, y = target_share_pct)) +
    geom_col() +
    geom_text(aes(label = paste0(round(target_share_pct, 1), "%")), hjust = -0.2, size = 3.5) +
    coord_flip() +
    theme_minimal(base_size = 13) +
    labs(
      title = "Bootstrap target composition",
      x = "Target group",
      y = "% of each bootstrap sample"
    )
  
  save_plot(p, "12_bootstrap_targets.png")
}

# ============================================================
# 14. Gráfico: pool votante vs target
# ============================================================

if (!is.null(voter_pool)) {
  p <- voter_pool %>%
    mutate(
      electoral_group_model = factor(
        electoral_group_model,
        levels = electoral_group_model[order(n_pool)]
      )
    ) %>%
    ggplot(aes(x = electoral_group_model, y = n_pool, fill = pool_empty)) +
    geom_col() +
    geom_text(aes(label = n_pool), hjust = -0.2, size = 3.5) +
    coord_flip() +
    theme_minimal(base_size = 13) +
    labs(
      title = "Available voter pool by electoral group",
      subtitle = "Groups with zero or very small pools require methodological decision",
      x = "Electoral group",
      y = "Available usable rows",
      fill = "Pool empty"
    )
  
  save_plot(p, "13_voter_pool_by_group.png")
}

# ============================================================
# 15. Gráfico: fallbacks bootstrap
# ============================================================

if (!is.null(fallbacks)) {
  p <- fallbacks %>%
    mutate(
      target_electoral_group = factor(
        target_electoral_group,
        levels = unique(target_electoral_group)
      )
    ) %>%
    ggplot(aes(x = target_electoral_group, y = n, fill = fallback_used)) +
    geom_col() +
    coord_flip() +
    theme_minimal(base_size = 13) +
    labs(
      title = "Fallback usage in preliminary bootstrap",
      subtitle = "This should be zero or explicitly justified in the final method",
      x = "Target group",
      y = "Number of bootstrap selections",
      fill = "Fallback used"
    )
  
  save_plot(p, "14_bootstrap_fallbacks.png")
}

message("Listo. Gráficos guardados en: ", out_dir)