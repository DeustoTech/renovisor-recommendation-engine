# fichero para juntar por cada escenario los acumulativos + greedy en un 2x2
library(magick)

combine_2x2_pdfs <- function(file_experts, file_kmeans, file_sins, file_greedy, output_file) {
  img1 <- image_read_pdf(file_experts, density = 150)
  img2 <- image_read_pdf(file_kmeans, density = 150)
  img3 <- image_read_pdf(file_sins, density = 150)
  img4 <- image_read_pdf(file_greedy, density = 150)
  
  row1 <- image_append(c(img1, img2))
  row2 <- image_append(c(img3, img4))
  final_img <- image_append(c(row1, row2), stack = TRUE)
  
  image_write(final_img, path = output_file, format = "pdf")
}

# 32
# base_cum <- "results/det32/results_no_greedy_from_freq/by_scenario"
# base_greedy <- "results/det32/results_greedy_from_freq/by_scenario"
# out_dir <- "results/det32/final_compare_pdfs_cumulative"

# 9
# base_cum <- "results/det9/results_no_greedy_from_freq_9/by_scenario"
# base_greedy <- "results/det9/results_greedy_from_freq_9/by_scenario"
# out_dir <- "results/det9/final_compare_pdfs_cumulative_9"

# 9 top3data
base_cum <- "results/det9_top3/results_no_greedy_from_freq_9_top3/by_scenario"
base_greedy <- "results/det9_top3/results_greedy_from_freq_9_top3/by_scenario"
out_dir <- "results/det9_top3/final_compare_pdfs_cumulative_9_top3"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

types <- c("pos", "ext")

# 32
# scenarios <- c("100-32", "10-32")

# 9
# scenarios <- c("100-9", "10-9")

# 9 top3data
scenarios <- c("100-9-top3", "10-9-top3")

for (tp in types) {
  for (sc in scenarios) {
    
    file_experts <- file.path(base_cum, paste0("cumulative_", tp, "_", sc, "_experts.pdf"))
    file_kmeans  <- file.path(base_cum, paste0("cumulative_", tp, "_", sc, "_kmeans.pdf"))
    file_sins    <- file.path(base_cum, paste0("cumulative_", tp, "_", sc, "_sins.pdf"))
    file_greedy  <- file.path(base_greedy, paste0("greedy_", tp, "_", sc, ".pdf"))
    
    if (all(file.exists(c(file_experts, file_kmeans, file_sins, file_greedy)))) {
      output_file <- file.path(out_dir, paste0("compare_cumulative_", tp, "_", sc, ".pdf"))
      
      combine_2x2_pdfs(
        file_experts,
        file_kmeans,
        file_sins,
        file_greedy,
        output_file
      )
      
      cat("OK:", output_file, "\n")
    } else {
      cat("Faltan archivos para:", tp, sc, "\n")
      print(c(file_experts, file_kmeans, file_sins, file_greedy))
    }
  }
}