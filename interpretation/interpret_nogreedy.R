library(data.table)
library(dplyr)

mode <- "9_top3data"
# mode <- "32"
# mode <- "9"

if (mode == "32") {
  base_dir <- "results/det32/results_no_greedy_from_freq"
  scenarios <- c("100-32", "10-32")
  out_dir <- "results/det32/cumulative_interpretation_32"
}

if (mode == "9") {
  base_dir <- "results/det9/results_no_greedy_from_freq_9"
  scenarios <- c("100-9", "10-9")
  out_dir <- "results/det9/cumulative_interpretation_9"
}

if (mode == "9_top3data") {
  base_dir <- "results/det9_top3/results_no_greedy_from_freq_9_top3"
  scenarios <- c("100-9-top3", "10-9-top3")
  out_dir <- "results/det9_top3/cumulative_interpretation_9_top3"
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

types <- c("pos", "ext")
refs <- c("experts", "kmeans", "sins")
D_values <- seq(0, 10, by = 2)
D_order <- 4

safe_read <- function(path) {
  if (!file.exists(path)) return(NULL)
  fread(path)
}

all_results <- list()
all_kmeanings <- list()

for (sc in scenarios) {
  for (tp in types) {
    for (ref_name in refs) {
      
      ref_dir <- file.path(base_dir, sc, tp, ref_name)
      
      f_all <- file.path(base_dir, paste0("ALL_RESULTS_ORDER_D", D_order, "_THEN_CUMULATIVE.csv"))
      f_kmap <- file.path(ref_dir, paste0("K_meaning_order_D", D_order, ".csv"))
      f_order <- file.path(ref_dir, paste0("archetype_order_by_individual_coverage_D", D_order, ".csv"))
      
      all_df <- safe_read(f_all)
      kmap_df <- safe_read(f_kmap)
      order_df <- safe_read(f_order)
      
      if (!is.null(all_df)) {
        sub <- all_df %>%
          filter(scenario == sc, type == tp, reference == ref_name) %>%
          arrange(K, D)
        
        if (nrow(sub) > 0) {
          all_results[[length(all_results) + 1]] <- sub
        }
      }
      
      if (!is.null(kmap_df)) {
        kmap_df$mode <- mode
        kmap_df$scenario <- sc
        kmap_df$type <- tp
        kmap_df$reference <- ref_name
        
        if (!is.null(order_df) && "archetype" %in% names(order_df)) {
          kmap_df$archetype_order_full <- paste(order_df$archetype, collapse = " | ")
        } else {
          kmap_df$archetype_order_full <- NA_character_
        }
        
        all_kmeanings[[length(all_kmeanings) + 1]] <- kmap_df
      }
    }
  }
}

if (length(all_results) == 0) stop("No se encontraron resultados cumulative.")
if (length(all_kmeanings) == 0) stop("No se encontraron K_meaning.")

results_df <- bind_rows(all_results) %>%
  arrange(scenario, type, reference, K, D)

kmeanings_df <- bind_rows(all_kmeanings) %>%
  arrange(scenario, type, reference, K)

write.csv(
  results_df,
  file.path(out_dir, paste0("cumulative_results_", mode, ".csv")),
  row.names = FALSE
)

write.csv(
  kmeanings_df,
  file.path(out_dir, paste0("cumulative_kmeanings_", mode, ".csv")),
  row.names = FALSE
)

for (sc in unique(results_df$scenario)) {
  for (tp in unique(results_df$type)) {
    for (ref_name in unique(results_df$reference)) {
      
      sub <- results_df %>%
        filter(scenario == sc, type == tp, reference == ref_name) %>%
        arrange(K, D)
      
      if (nrow(sub) > 0) {
        write.csv(
          sub,
          file.path(out_dir, paste0("cumulative_", tp, "_", sc, "_", ref_name, ".csv")),
          row.names = FALSE
        )
      }
    }
  }
}

cat("OK cumulative. Archivos en:", out_dir, "\n")