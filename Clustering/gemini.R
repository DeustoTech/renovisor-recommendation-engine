library(data.table)
library(foreach)
library(doFuture)
library(iterators)
library(future.apply)

doFuture::registerDoFuture()
plan(list(
  tweak(multicore, workers = 4), 
  tweak(multicore, workers = 4)
))

BINARIZAR <- function(M, n_top) {    
  ranks <- apply(M, 1, function(x) rank(-x, ties.method = "first"))
  ranks <- t(ranks) 
  
  res <- matrix(0, nrow = nrow(M), ncol = ncol(M))
  res[ranks <= n_top & !is.na(M)] <- 1
  return(res)
}

GET_FREQ <- function(z, DIR){
  if (!dir.exists(DIR)) dir.create(DIR, recursive = TRUE)
  
  cl <-  foreach (b = 1:BOOT, .options.future = list(seed = TRUE)) %dofuture% {
    idx  <- sample.int(nrow(z), replace = TRUE)
    km_b <- kmeans(z[idx,], centers = K, iter.max = 20)
    return(km_b$centers)
  }
  cl <- as.matrix(data.table::rbindlist(cl))
  fwrite(cl, file = paste0(DIR, "/cluster_cen.csv"))
 
  pos <- ext <- cl
  ext <- abs(ext-50)
  pos[pos < MAX] <- NA
  ext[ext < Q3]  <- NA
  
  pdf(file=paste0(DIR,"/","boxplots.pdf"))
    on.exit(if (!is.null(dev.list())) dev.off(), add = TRUE)
    boxplot(z,  main="RAW answers")
    boxplot(cl, main="Centers")
    boxplot(pos,main="Centers upper half truncated")
    boxplot(ext,main="Centers extreme truncated")

  pos <- as.data.table(BINARIZAR(pos, NF))
  setnames(pos, colnames(z))
  freq_pos <- pos[, .N, by = names(pos)]
  
  fwrite(pos,      file = paste0(DIR, "/cluster_det_pos.csv"))
  fwrite(freq_pos, file = paste0(DIR, "/freq_cluster_det_pos.csv"))
  
  ext <- as.data.table(BINARIZAR(ext, NF))
  setnames(ext, colnames(z))
  freq_ext <- ext[, .N, by = names(ext)]
  
  fwrite(ext,      file = paste0(DIR, "/cluster_det_ext.csv"))
  fwrite(freq_ext, file = paste0(DIR, "/freq_cluster_det_ext.csv"))
}

K    <- 8     # Number of clusters
MAX  <- 50    # punto de corte para considerar el determinante solo positivo
Q3   <- 25    # punto de corte para considerar el determinante extremos alto
NF   <- 15    # si no esta entre los 15 valores mas altos, se pone 0
BOOT <- 1e6   # numero de repeticiones de bootstraping

d  <- read.csv("data/Content_Export_Investment_Arquetypes_2022_full-latin-final.csv", skip = 2)
dm <- read.csv2("data/determinants.csv",row.names = 1)

z <- as.matrix(d[,50:81])
z[is.na(z)] <- 50  # Rellenar NA con valor neutro

det_names <- c("PROFITS", "CREDIT SCORE", "RISK PROFILE", "ADDED VALUE",
               "FRUGALITY", "CLIMATE PROTECTION", "LEGAL", "TRUST", "SAFETY",
               "COST-EFFICIENCY", "KNOWLEDGE", "OWN COMPETENCE", "TECHNICAL FIT",
               "ENVIRONMENTAL CONCERNS", "SELF-SATISFACTION", "COMMITMENT",
               "ADHERENCE", "AUTARKY", "WELLBEING", "COZINESS", "RIGHTS AND DUTIES",
               "PEER-PRESSURE", "SUPPORT", "SOCIALISING", "AGREEMENT", "NOVELTY",
               "FUN", "RECOGNITION", "TRENDS", "AUTHORITY", "OWN SIGNIFICANCE", "APPROVAL")
colnames(z) <- det_names
rm(d)

resultados <-  future.apply::future_mapply(
  FUN = GET_FREQ, 
  z = list(z, round(z,-1), z  %*% as.matrix(dm),round(z,-1) %*% as.matrix(dm)), 
  DIR = list("100-32","10-32","100-9","10-9"),
  future.seed = TRUE
)
