library(r2r)
library(dplyr)


#### DATA LOADING. DET SELECTION AND PREPROCESS
d <- read.csv("data/Content_Export_Investment_Arquetypes_2022_full-latin-final.csv", skip = 2)

z <- as.matrix(d[,50:81]) # 32 col of det
z[is.na(z)] <- 50  # replace missing values with 50 

det_names <- c("PROFITS", "CREDIT SCORE", "RISK PROFILE", "ADDED VALUE",
               "FRUGALITY", "CLIMATE PROTECTION", "LEGAL", "TRUST", "SAFETY",
               "COST-EFFICIENCY", "KNOWLEDGE", "OWN COMPETENCE", "TECHNICAL FIT",
               "ENVIRONMENTAL CONCERNS", "SELF-SATISFACTION", "COMMITMENT",
               "ADHERENCE", "AUTARKY", "WELLBEING", "COZINESS", "RIGHTS AND DUTIES",
               "PEER-PRESSURE", "SUPPORT", "SOCIALISING", "AGREEMENT", "NOVELTY",
               "FUN", "RECOGNITION", "TRENDS", "AUTHORITY", "OWN SIGNIFICANCE", "APPROVAL")
colnames(z) <- det_names

#### MODEL PARAMETERS
K   <- 8 # num of clusters
NN  <- 1 # num of random initializations in k-means
MAX <- 50 # threshold: values below MAX are considered non-relevant
NFACTORS <- 15 # num of det defining each archetype

km_det <- kmeans(z, centers = K, nstart = NN) # k-means on the full dataset
write.csv(km_det$cluster, "results/clusters_determinants.csv", row.names = FALSE)


#### BOOTSTRAP PROCEDURE
BOOT <- 100 000 # num of replications
boot_cen <- list()
boot_det <- list()
m        <- hashmap(default = 0, on_missing_key = "default") # hashmap to count how often each determinant combination appears

# Main bootstrap loop
for (b in 1:BOOT) {
  idx  <- sample(1:nrow(z), replace = TRUE)
  km_b <- kmeans(z[idx,], centers = K, nstart = NN)
  boot_cen[[b]] <- km_b$centers # store cluster centroids

  aux <- km_b$centers

  # for each cluster / archetype
  for (j in 1:K){
    aux[j,km_b$centers[j,] <= MAX] <- NA
    # select the NFACTORS most relevant determinants
    aux[j,order(aux[j,],decreasing=T,na.last=NA)[1:NFACTORS]] <- 1
    aux[j,aux[j,] > 1] <- NA
    aux[is.na(aux)] <- 0
  }
  boot_det[[b]] <- aux

 # count how often each determinant combination appears
  for (i in 1:K) {
  for (i in 1:K){
    if (!has_key(m,aux[i,])) insert(m,aux[i,],1)
    else                     insert(m,aux[i,],query(m,aux[i,])+1)
  }
}