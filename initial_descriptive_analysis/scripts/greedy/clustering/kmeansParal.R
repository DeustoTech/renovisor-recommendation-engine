<<<<<<< HEAD
library(data.table)
=======
<<<<<<< HEAD
library(r2r)
>>>>>>> b26e2e4 (sdfgdfg)
library(foreach)
library(doFuture)
library(iterators)

doFuture::registerDoFuture()
plan(list(
  tweak(multicore, workers = 4), 
  tweak(multicore, workers = 4)
))

GET_FREQ <- function(z,DIR){
  if (!file.exists(DIR)) dir.create(DIR, recursive = TRUE)
  
  C <- foreach (b = 1:BOOT,.options.future = list(seed = TRUE)) %dofuture% {
    idx  <- sample(1:nrow(z), replace = TRUE)
    km_b <- kmeans(z[idx,], centers = K)
    return(km_b$centers)
  }
  C <- data.table::rbindlist(C)
  fwrite(C,file=paste0(DIR,"/","cluster_cen.csv"))

  pos <- ext <- C
  ext <- abs(ext-50)
  pos[pos < MAX] <- NA
  ext[ext < Q3]  <- NA
  
   pdf(file=paste0(DIR,"/","boxplots.pdf"))
    boxplot(z,main="RAW answers")
    boxplot(C,main="Centers")
    boxplot(pos,main="Centers upper half truncated")
    boxplot(ext,main="Centers extreme truncated")
  dev.off()

  pos <- foreach(fila=iter(pos,by="row"),.combine=rbind) %dofuture% { ## binarizo solo positivio
    fila[order(fila,decreasing=T,na.last=NA)[1:NF]] <- 1
    fila[fila>1]      <- NA
    fila[is.na(fila)] <- 0
    return(fila)
  }
  pos <- as.data.table(pos)
  freq_pos <- pos[, .N, by = names(pos)]
  fwrite(pos,     file=paste0(DIR,"/","cluster_det_pos.csv"))
  fwrite(freq_pos,file=paste0(DIR,"/","freq_cluster_det_pos.csv"))

  ext <- foreach(fila=iter(ext,by="row"),.combine=rbind) %dofuture% { ## binarizo extremos
    fila[order(fila,decreasing=T,na.last=NA)[1:NF]] <- 1
    fila[fila>1]      <- NA
    fila[is.na(fila)] <- 0
    return(fila)
  }
  ext <- as.data.table(ext)
  freq_ext <- ext[, .N, by = names(ext)]
  fwrite(pos,     file=paste0(DIR,"/","cluster_det_ext.csv"))
  fwrite(freq_pos,file=paste0(DIR,"/","freq_cluster_det_ext.csv"))
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

resultados <- future_mapply(
  FUN = GET_FREQ, 
  datos = list(z, round(z,-1), z  %*% as.matrix(dm),round(z,-1) %*% as.matrix(dm)), 
  names = list("100-32","10-32","100-9","10-9")
)

<<<<<<< HEAD
# GET_FREQ(z,  "100-32")
# GET_FREQ(round(z,-1), "10-32")
# GET_FREQ(z  %*% as.matrix(dm), "100-9")
# GET_FREQ(round(z,-1) %*% as.matrix(dm),"10-9")
=======
m <- hashmap(default = 0, on_missing_key = "default")
par_out <- foreach (b = 1:BOOT, .options.future = list(seed = TRUE)) %dorng% {
  idx  <- sample(1:nrow(z), replace = TRUE)
  km_b <- kmeans(z[idx,], centers = K, nstart = NN)

  aux <- km_b$centers
  for (j in 1:K) {
    aux[j,km_b$centers[j,] <= MAX] <- NA
    aux[j,order(aux[j,],decreasing=T,na.last=NA)[1:NF]] <- 1
    aux[j,aux[j,] > 1] <- NA
    aux[is.na(aux)] <- 0
  }

  for (i in 1:K){
    if (!has_key(m,aux[i,])) insert(m,aux[i,],1)
    else                     insert(m,aux[i,],query(m,aux[i,])+1)
  }
  return(list(km_b$centers,aux))
}

saveRDS(m,       file="cluster_hash.rds.xz",compress = 'xz')
saveRDS(unlist(par_out,recursive=F)[c(TRUE,FALSE)],
                 file="cluster_cen.rds.xz", compress = 'xz')
saveRDS(unlist(par_out,recursive=F)[c(FALSE,TRUE)],
                 file="cluster_det.rds.xz", compress = 'xz')
rm(par_out)
=======
library(r2r)
library(foreach)
library(doFuture)

#doFuture::registerDoFuture()
#future::plan("multisession")
future::plan("multicore")

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

K    <- 8
NN   <- 1
MAX  <- 50
NF   <- 15
BOOT <- 100000

m <- hashmap(default = 0, on_missing_key = "default")
par_out <- foreach(b = 1:BOOT, .options.future = list(
                    globals = structure(TRUE, add = "m"),
                    seed = TRUE)) %do% {
  idx  <- sample(1:nrow(z), replace = TRUE)
  km_b <- kmeans(z[idx,], centers = K, nstart = NN)

  aux <- km_b$centers
  for (j in 1:K) {
    aux[j,km_b$centers[j,] <= MAX] <- NA
    aux[j,order(aux[j,],decreasing=T,na.last=NA)[1:NF]] <- 1
    aux[j,aux[j,] > 1] <- NA
    aux[is.na(aux)] <- 0
  }

  for (i in 1:K){
    if (!has_key(m,aux[i,])) insert(m,aux[i,],1)
    else                     insert(m,aux[i,],query(m,aux[i,])+1)
  }
  return(list(km_b$centers,aux))
}

saveRDS(m,       file="cluster_hash.rds.xz",compress = 'xz')
saveRDS(unlist(par_out,recursive=F)[c(TRUE,FALSE)],
                 file="cluster_cen.rds.xz", compress = 'xz')
saveRDS(unlist(par_out,recursive=F)[c(FALSE,TRUE)],
                 file="cluster_det.rds.xz", compress = 'xz')
#rm(par_out)
>>>>>>> a1e21bb (sdfgdfg)
>>>>>>> b26e2e4 (sdfgdfg)
