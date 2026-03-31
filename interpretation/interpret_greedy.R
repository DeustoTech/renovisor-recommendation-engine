library(data.table)
library(dplyr)

# CONFIG

#mode <- "9_top3data"
mode <- "32"
# mode <- "9"

if (mode == "32") {
  base_dir <- "results/det32/results_greedy_from_freq"
  scenarios <- c("100-32", "10-32")
  out_dir <- "results/det32/greedy_interpretation_32"
}

if (mode == "9") {
  base_dir <- "results/det9/results_greedy_from_freq_9"
  scenarios <- c("100-9", "10-9")
  out_dir <- "results/det9/greedy_interpretation_9"
}

if (mode == "9_top3data") {
  base_dir <- "results/det9_top3/results_greedy_from_freq_9_top3"
  scenarios <- c("100-9-top3", "10-9-top3")
  out_dir <- "results/det9_top3/greedy_interpretation_9_top3"
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

types <- c("pos", "ext")
K_values <- 1:8
D_values <- seq(0, 10, by = 2)

get_active_dims <- function(df_row) {
  vals <- as.numeric(df_row)
  names(df_row)[which(vals > 0)]
}

safe_read <- function(path) {
  if (!file.exists(path)) return(NULL)
  fread(path)
}

all_clusters <- list()
all_summary <- list()

for (sc in scenarios) {
  for (tp in types) {
    
    for (K in K_values) {
      
      f_assign <- file.path(base_dir, sc, tp, paste0("K_", K), "assigned_vs_D.csv")
      assign_df <- safe_read(f_assign)
      
      for (D in D_values) {
        
        f_centers <- file.path(
          base_dir, sc, tp, paste0("K_", K), paste0("D_", D),
          "greedy_cluster_centers.csv"
        )
        
        f_summary <- file.path(
          base_dir, sc, tp, paste0("K_", K), paste0("D_", D),
          "greedy_cluster_summary_by_patterns.csv"
        )
        
        centers <- safe_read(f_centers)
        sum_patterns <- safe_read(f_summary)
        
        if (is.null(centers)) next
        
        assigned_pct <- NA_real_
        unassigned_pct <- NA_real_
        
        if (!is.null(assign_df)) {
          rowD <- assign_df[assign_df$D == D, ]
          if (nrow(rowD) > 0) {
            assigned_pct <- rowD$assigned_pct[1]
            unassigned_pct <- rowD$unassigned_pct[1]
          }
        }
        
        for (i in seq_len(nrow(centers))) {
          row_vals <- centers[i, -1, with = FALSE]
          dims <- get_active_dims(row_vals)
          
          n_patterns <- NA_real_
          pct_total_patterns <- NA_real_
          pct_rest_patterns <- NA_real_
          
          if (!is.null(sum_patterns)) {
            rr <- sum_patterns[sum_patterns$cluster == centers$cluster[i], ]
            if (nrow(rr) > 0) {
              n_patterns <- rr$n_patterns[1]
              pct_total_patterns <- rr$pct_total_patterns[1]
              pct_rest_patterns <- rr$pct_rest_patterns[1]
            }
          }
          
          all_clusters[[length(all_clusters) + 1]] <- data.frame(
            mode = mode,
            scenario = sc,
            type = tp,
            K = K,
            D = D,
            cluster = centers$cluster[i],
            n_active_dims = length(dims),
            active_dims = paste(dims, collapse = " | "),
            n_patterns = n_patterns,
            pct_total_patterns = pct_total_patterns,
            pct_rest_patterns = pct_rest_patterns,
            assigned_pct = assigned_pct,
            unassigned_pct = unassigned_pct,
            stringsAsFactors = FALSE
          )
        }
        
        all_summary[[length(all_summary) + 1]] <- data.frame(
          mode = mode,
          scenario = sc,
          type = tp,
          K = K,
          D = D,
          n_clusters_found = nrow(centers),
          assigned_pct = assigned_pct,
          unassigned_pct = unassigned_pct,
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

clusters_df <- bind_rows(all_clusters) %>%
  arrange(scenario, type, K, D, cluster)

summary_df <- bind_rows(all_summary) %>%
  distinct() %>%
  arrange(scenario, type, K, D)

write.csv(
  clusters_df,
  file.path(out_dir, paste0("greedy_clusters_contents_", mode, ".csv")),
  row.names = FALSE
)

write.csv(
  summary_df,
  file.path(out_dir, paste0("greedy_summary_", mode, ".csv")),
  row.names = FALSE
)

# CSV extra por escenario y tipo
for (sc in unique(clusters_df$scenario)) {
  for (tp in unique(clusters_df$type)) {
    
    sub_df <- clusters_df %>%
      filter(scenario == sc, type == tp) %>%
      arrange(K, D, cluster)
    
    if (nrow(sub_df) > 0) {
      write.csv(
        sub_df,
        file.path(out_dir, paste0("greedy_contents_", tp, "_", sc, ".csv")),
        row.names = FALSE
      )
    }
  }
}

cat("OK. Archivos guardados en:", out_dir, "\n")