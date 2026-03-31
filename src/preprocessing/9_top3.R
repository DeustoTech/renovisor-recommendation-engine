library(data.table)

base_dir <- "~/Desktop/master/PFM_extra/1000000"
out_base <- "data/9_top3"

if (!dir.exists(out_base)) dir.create(out_base, recursive = TRUE)

scenario_map <- c(
  "100-9" = "100-9-top3",
  "10-9"  = "10-9-top3"
)

MAX <- 50
Q3 <- 25
N_TOP <- 3

BINARIZAR_TOP_FAST <- function(M, n_top = 3) {
  M <- as.matrix(M)
  mode(M) <- "numeric"
  
  nr <- nrow(M)
  nc <- ncol(M)
  
  res <- matrix(0L, nrow = nr, ncol = nc)
  colnames(res) <- colnames(M)
  rownames(res) <- rownames(M)
  
  for (i in seq_len(nr)) {
    x <- M[i, ]
    ok <- !is.na(x)
    if (!any(ok)) next
    
    idx_ok <- which(ok)
    x_ok <- x[idx_ok]
    
    ord <- order(x_ok, decreasing = TRUE)
    keep <- idx_ok[ord[seq_len(min(n_top, length(idx_ok)))]]
    res[i, keep] <- 1L
  }
  
  res
}

process_cluster_centers_top3 <- function(input_dir, output_dir, max_cut = 50, q3_cut = 25, n_top = 3) {
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  in_file <- file.path(input_dir, "cluster_cen.csv")
  if (!file.exists(in_file)) {
    stop(paste("No existe:", in_file))
  }
  
  cat("Leyendo:", in_file, "\n")
  cl <- fread(in_file)
  
  cl_mat <- as.matrix(cl)
  mode(cl_mat) <- "numeric"
  
  cat("Procesando POS...\n")
  pos <- cl_mat
  pos[pos < max_cut] <- NA
  
  pos_bin <- BINARIZAR_TOP_FAST(pos, n_top = n_top)
  pos_dt <- as.data.table(pos_bin)
  setnames(pos_dt, colnames(cl))
  
  freq_pos <- pos_dt[, .N, by = names(pos_dt)]
  setorder(freq_pos, -N)
  
  fwrite(pos_dt, file.path(output_dir, "cluster_det_pos.csv"))
  fwrite(freq_pos, file.path(output_dir, "freq_cluster_det_pos.csv"))
  
  cat("Procesando EXT...\n")
  ext <- abs(cl_mat - 50)
  ext[ext < q3_cut] <- NA
  
  ext_bin <- BINARIZAR_TOP_FAST(ext, n_top = n_top)
  ext_dt <- as.data.table(ext_bin)
  setnames(ext_dt, colnames(cl))
  
  freq_ext <- ext_dt[, .N, by = names(ext_dt)]
  setorder(freq_ext, -N)
  
  fwrite(ext_dt, file.path(output_dir, "cluster_det_ext.csv"))
  fwrite(freq_ext, file.path(output_dir, "freq_cluster_det_ext.csv"))
  
  summary_df <- data.frame(
    file = c("cluster_det_pos.csv", "freq_cluster_det_pos.csv", "cluster_det_ext.csv", "freq_cluster_det_ext.csv"),
    n_rows = c(nrow(pos_dt), nrow(freq_pos), nrow(ext_dt), nrow(freq_ext)),
    stringsAsFactors = FALSE
  )
  write.csv(summary_df, file.path(output_dir, "summary_top3_files.csv"), row.names = FALSE)
  
  cat("OK:", input_dir, "->", output_dir, "\n")
  cat("   POS patrones únicos:", nrow(freq_pos), "\n")
  cat("   EXT patrones únicos:", nrow(freq_ext), "\n")
}

for (sc_in in names(scenario_map)) {
  sc_out <- scenario_map[[sc_in]]
  
  input_dir  <- file.path(base_dir, sc_in)
  output_dir <- file.path(out_base, sc_out)
  
  process_cluster_centers_top3(
    input_dir  = input_dir,
    output_dir = output_dir,
    max_cut    = MAX,
    q3_cut     = Q3,
    n_top      = N_TOP
  )
}

cat("fin\n")