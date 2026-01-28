library(r2r)
library(dplyr)

d <- read.csv("data/Content_Export_Investment_Arquetypes_2022_full-latin-final.csv", skip = 2)

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

K   <- 8
NN  <- 1
MAX <- 50
NFACTORS <- 15

km_det <- kmeans(z, centers = K, nstart = NN)
write.csv(km_det$cluster, "results/clusters_determinants.csv", row.names = FALSE)

BOOT <- 100 000
boot_cen <- list()
boot_det <- list()
m        <- hashmap(default = 0,on_missing_key = "default")

for (b in 1:BOOT) {
  idx  <- sample(1:nrow(z), replace = TRUE)
  km_b <- kmeans(z[idx,], centers = K, nstart = NN)
  boot_cen[[b]] <- km_b$centers

  aux <- km_b$centers
  for (j in 1:K){
    aux[j,km_b$centers[j,] <= MAX] <- NA
    aux[j,order(aux[j,],decreasing=T,na.last=NA)[1:NFACTORS]] <- 1
    aux[j,aux[j,] > 1] <- NA
    aux[is.na(aux)] <- 0
  }
  boot_det[[b]] <- aux

  for (i in 1:K){
    if (!has_key(m,aux[i,])) insert(m,aux[i,],1)
    else                     insert(m,aux[i,],query(m,aux[i,])+1)
  }
}