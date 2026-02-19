library(r2r)
library(foreach)
library(doRNG)

doFuture::registerDoFuture()
future::plan("multicore")

d <- read.csv("data/Content_Export_Investment_Arquetypes_2022_full-latin-final.csv", skip = 2)
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

# comprobar esto!
zz <- round(z,-1)/10

z9  <- z  %*% as.matrix(dm)
zz9 <- zz %*% as.matrix(dm)

K    <- 8
NN   <- 1
MAX  <- 50
NF   <- 15 # si no esta entre los 15 valores mas altos, se pone 0
BOOT <- 100000

m <- hashmap(default = 0, on_missing_key = "default")

# bootstrap y kmeans sobre la matriz binaria
par_out <- foreach (b = 1:BOOT, .options.future = list(seed = TRUE)) %dorng% {
  idx  <- sample(1:nrow(zz), replace = TRUE)
  km_b <- kmeans(zz[idx,], centers = K, nstart = NN)
  
  aux <- km_b$centers
  for (j in 1:K) {
    aux[j, km_b$centers[j,] <= MAX] <- NA
    aux[j, order(aux[j,], decreasing = T, na.last = NA)[1:NF]] <- 1
    aux[j, aux[j,] > 1] <- NA
    aux[is.na(aux)] <- 0
  }
  
  for (i in 1:K) {
    if (!has_key(m, aux[i,])) insert(m, aux[i,], 1)
    else                     insert(m, aux[i,], query(m, aux[i,]) + 1)
  }
  
  return(list(km_b$centers, aux))
}

saveRDS(m, file = "cluster_hash_ranges.rds.xz", compress = 'xz')
saveRDS(unlist(par_out, recursive = F)[c(TRUE,FALSE)], file = "cluster_cen_ranges.rds.xz", compress = 'xz')
saveRDS(unlist(par_out, recursive = F)[c(FALSE,TRUE)], file = "cluster_det_ranges.rds.xz", compress = 'xz')
rm(par_out)
