

### fichero para juntar individual y el cumulativ de cada csv(exp, kmeans, sins)

library(magick)
library(pdftools)

base1 <- "results/results_no_greedy_from_freq"
base2 <- "results/results_each_archetype_separate"
out_dir <- "results/combined_side_by_side"

dir.create(out_dir, showWarnings = FALSE)

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
    }
  }
}