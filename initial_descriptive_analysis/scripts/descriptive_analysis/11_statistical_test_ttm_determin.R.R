#install.packages("PMCMRplus")
#install.packages("rstatix")

# ==============================================================================
# SCRIPT 09 - CONTRASTES ESTADÍSTICOS POR ETAPA TTM
# ==============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(rstatix)
library(PMCMRplus)

# 1. Cargar datos ---------------------------------------------------------

df_long <- read_csv(
  "initial_descriptive_analysis/output/ttm_determinants_long_for_boxplots.csv",
  show_col_types = FALSE
)

# 2. Preparar datos -------------------------------------------------------

stage_order <- c(
  "No la conoce, pero le genera curiosidad",
  "La conoce / la consideraría",
  "Implementada"
)

df_tests <- df_long %>%
  filter(
    etapa %in% stage_order,
    !is.na(value),
    value >= 0,
    value <= 100
  ) %>%
  mutate(
    etapa = factor(etapa, levels = stage_order),
    determinant = as.character(determinant),
    dimension = as.character(dimension)
  )

# 3. Carpeta de salida ----------------------------------------------------

output_dir <- "initial_descriptive_analysis/output/statistical_tests_ttm"

dir.create(
  output_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

# 4. Kruskal-Wallis por determinante -------------------------------------

kruskal_one_determinant <- function(data) {
  
  if (n_distinct(data$etapa) < 2) {
    return(tibble(statistic = NA_real_, p = NA_real_))
  }
  
  test <- tryCatch(
    kruskal.test(value ~ etapa, data = data),
    error = function(e) NULL
  )
  
  if (is.null(test)) {
    return(tibble(statistic = NA_real_, p = NA_real_))
  }
  
  tibble(
    statistic = as.numeric(test$statistic),
    p = test$p.value
  )
}

kruskal_by_determinant <- df_tests %>%
  group_by(determinant) %>%
  group_modify(~ kruskal_one_determinant(.x)) %>%
  ungroup() %>%
  filter(!is.na(p)) %>%
  mutate(
    p_adj = p.adjust(p, method = "BH"),
    significance = case_when(
      p_adj < 0.001 ~ "***",
      p_adj < 0.01 ~ "**",
      p_adj < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  arrange(p)

write_csv(
  kruskal_by_determinant,
  file.path(output_dir, "kruskal_by_determinant.csv")
)

print(kruskal_by_determinant, n = Inf)

# 4B. Test de tendencia Jonckheere-Terpstra -----------------------------

trend_by_determinant <- df_tests %>%
  group_by(determinant) %>%
  group_modify(~{
    
    test <- tryCatch(
      jonckheere.test(
        x = .x$value,
        g = as.numeric(.x$etapa),
        alternative = "increasing"
      ),
      error = function(e) NULL
    )
    
    if (is.null(test)) {
      return(tibble(
        statistic = NA_real_,
        p = NA_real_
      ))
    }
    
    tibble(
      statistic = as.numeric(test$statistic),
      p = test$p.value
    )
    
  }) %>%
  ungroup() %>%
  mutate(
    p_adj = p.adjust(p, method = "BH")
  ) %>%
  arrange(p)

write_csv(
  trend_by_determinant,
  file.path(output_dir, "jonckheere_trend_by_determinant.csv")
)

print(trend_by_determinant, n = Inf)


# 5. Posthoc Dunn por determinante ---------------------------------------

significant_determinants <- kruskal_by_determinant %>%
  filter(p < 0.05) %>%
  pull(determinant)

if (length(significant_determinants) > 0) {
  
  dunn_by_determinant <- df_tests %>%
    filter(determinant %in% significant_determinants) %>%
    group_by(determinant) %>%
    dunn_test(
      value ~ etapa,
      p.adjust.method = "BH"
    ) %>%
    ungroup() %>%
    arrange(determinant, p.adj)
  
} else {
  
  dunn_by_determinant <- tibble()
  message("No hay determinantes con p < 0.05 en Kruskal-Wallis; no se calcula posthoc.")
  }

write_csv(
  dunn_by_determinant,
  file.path(output_dir, "dunn_posthoc_by_determinant.csv")
)

print(dunn_by_determinant, n = Inf)


# 8. Resumen compacto -----------------------------------------------------

summary_significant_determinants <- kruskal_by_determinant %>%
  mutate(
    significant_raw = p < 0.05,
    significant_adjusted = p_adj < 0.05
  ) %>%
  summarise(
    n_determinants = n(),
    n_raw_significant = sum(significant_raw),
    n_adjusted_significant = sum(significant_adjusted)
  )

write_csv(
  summary_significant_determinants,
  file.path(output_dir, "summary_significant_kruskal_determinants.csv")
)

print(summary_significant_determinants)

cat("Contrastes estadísticos guardados en:", output_dir, "\n")