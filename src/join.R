### fichero para juntar individual y el cumulative de cada csv (experts, kmeans, sins)

library(magick)
library(pdftools)

# 32
# base1 <- "results/det32/results_no_greedy_from_freq"
# base2 <- "results/det32/results_each_archetype_separate"
# out_dir <- "results/det32/combined_side_by_side"

# 9
# base1 <- "results/det9/results_no_greedy_from_freq_9"
# base2 <- "results/det9/results_each_archetype_separate_9"
# out_dir <- "results/det9/combined_side_by_side_9"

# 9 top3data
base1 <- "results/det9_top3/results_no_greedy_from_freq_9_top3"
base2 <- "results/det9_top3/results_each_archetype_separate_9_top3"
out_dir <- "results/det9_top3/combined_side_by_side_9_top3"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

types <- c("pos", "ext")
refs <- c("experts", "kmeans", "sins")

combine_side_by_side <- function(file1, file2, output) {
  img1 <- image_read_pdf(file1, density = 150)
  img2 <- image_read_pdf(file2, density = 150)
  
  combined <- image_append(c(img1, img2))
  
  image_write(combined, path = output, format = "pdf")
}

for (tp in types) {
  for (ref in refs) {
    
    file_cumulative <- file.path(base1, paste0("order_D4_then_cumulative_", tp, "_", ref, ".pdf"))
    file_individual <- file.path(base2, paste0("each_archetype_separate_", tp, "_", ref, ".pdf"))
    
    if (file.exists(file_cumulative) && file.exists(file_individual)) {
      
      output_file <- file.path(out_dir, paste0("compare_side_", tp, "_", ref, ".pdf"))
      
      combine_side_by_side(file_individual, file_cumulative, output_file)
      
      cat("OK:", output_file, "\n")
    } else {
      cat("Falta alguno:", tp, ref, "\n")
      print(c(file_cumulative, file_individual))
    }
  }
}