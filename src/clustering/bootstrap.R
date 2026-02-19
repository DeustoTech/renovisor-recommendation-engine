library("r2r")
library("effectsize")
library("skimr")
library("PMCMRplus")

options(digits=2)
options(es.use_symbols = TRUE)

COMPARE2 <- function(a,b)
{
  aux <- numeric(length(b))
  rel <- colSums(!is.na(b))

  for (j in 1:length(b))
    aux[j] <- sum(a %in% b[,j])

  names(aux) <- names(b)
  return(100*t(aux)/rel)
}

bootstrapping <- function(z,BOOT,BS,MIN)
{
  m <- hashmap(default = 0)
  for(j in 1:BOOT)
  {
    S <- sample(length(z[,1]),BS)
    k <- kmeans( z[S,], NCLUSTER)
    a <- apply(k$centers,1,function(x) { AUX <- abs(x-50)/50; names(head(AUX[order(-AUX)],NFACTORS))})

    for (i in 1:NCLUSTER)
      m[[a[,i]]] <- m[[a[,i]]] + 1
  }

  BEST <- character(length(m))
  l    <- 1
  for (k in keys(m))
  {
    aux    <- COMPARE2(k,KM)
    aux[aux <= MIN] <- NA
    if (all(is.na(aux))) {BEST[l] <- NA}
    else                 {BEST[l] <- colnames(aux)[which.max(aux)]}
    l <- l+1
  }

  ZZ <- unique(BEST)
  r  <- numeric(length(ZZ))
  names(r) <- ZZ

  r[1] <- sum(unlist(values(m)[is.na(BEST)]))
  x <- 2
  for (c in unique(na.omit(ZZ)))
  {
    r[x] <- sum(unlist(values(m)[BEST==c]))
    x <- x+1
  }
  return(100*r[order(names(r))]/length(m))
}

d  <- read.csv("data/Content_Export_Investment_Arquetypes_2022_full-latin-final.csv",skip=2)
KM <- read.csv("data/archetypeKmeans.csv")[-1]

SAMPLE  <- length(d[[1]])
FACTORS <- c("Profits", "Credit Score", "Risk Profile", "Added Value",
             "Frugality", "Legal", "Trust", "Safety", "Climate Protection",
             "Cost-Efficiency", "Knowledge", "Own Competence",
             "Technical Fit", "Environmental Concerns", "Self-Satisfaction",
             "Commitment", "Adherence", "Autarky", "Wellbeing", "Cozyness",
             "Rights and Duties", "Peer-Pressure", "Support", "Socialising",
             "Agreement", "Novelty", "Fun", "Brag", "Trends", "Authority",
             "Own Significance", "Poseur")
NFACTORS <- 15
NCLUSTER <- 8

z <- as.matrix(d[,50:81])
z[is.na(z)] <- 50
z <- z[complete.cases(z),]
dimnames(z) <- list(1:SAMPLE, FACTORS)

EU <- z[1:1000,]
LA <- z[1001:SAMPLE,]

REP <- 10000
dEU <- bootstrapping(EU,REP,length(EU[,1])/2,100*8/NFACTORS)
dLA <- bootstrapping(LA,REP,length(LA[,1])/2,100*8/NFACTORS)
for (n in 9:12)
{
  dEU <- rbind(dEU,bootstrapping(EU,REP,floor(length(EU[,1])/2),100*n/NFACTORS))
  dLA <- rbind(dLA,bootstrapping(LA,REP,floor(length(LA[,1])/2),100*n/NFACTORS))
}
rownames(dEU) <- rownames(dLA) <- 8:12
write.csv(dEU,file="dEU.csv")
write.csv(dLA,file="dLA.csv")

vEU <- bootstrapping(EU,REP,length(EU[,1])/2,12/NFACTORS)
vLA <- bootstrapping(LA,REP,length(LA[,1])/2,12/NFACTORS)
for (v in 1:(REP/100))
{
  vEU <- rbind(vEU,bootstrapping(EU,REP,floor(length(EU[,1])/2),100*12/NFACTORS))
  vLA <- rbind(vLA,bootstrapping(LA,REP,floor(length(LA[,1])/2),100*12/NFACTORS))
  gc()
}
rownames(vEU) <- rownames(vLA) <- 1:(1+(REP/100))
write.csv(vEU,file="vEU.csv")
write.csv(vLA,file="vLA.csv")

pdf("comparison.pdf",width=12)
 plot(frdAllPairsSiegelTest(as.matrix(vEU,rownames.force=T)),las=2)
 plot(frdAllPairsSiegelTest(as.matrix(vLA,rownames.force=T)),las=2)
dev.off()

write.csv(skim(vEU),file="vEU.csv")
write.csv(skim(vLA),file="vLA.csv")

### From https://en.wikipedia.org/wiki/68%E2%80%9395%E2%80%9399.7_rule
# A weaker three-sigma rule can be derived from Chebyshev's inequality,
# stating that even for non-normally distributed variables, at least 88.8%
# of cases should fall within properly calculated three-sigma intervals.
###

ALPHA <- 1-0.888
ALPHA <- 1-0.26

cat(c("NA",names(vEU)),"\n",file="confidence.csv")
cat(apply(vEU,2,quantile,ALPHA/2),"\n",apply(vEU,2,quantile,1-ALPHA/2),"\n",file="confidence.csv",append=T)
cat(apply(vLA,2,quantile,ALPHA/2),"\n",apply(vLA,2,quantile,1-ALPHA/2),"\n",file="confidence.csv",append=T)




# SORT <- order(unlist(values(m)),decreasing=T)
# BEST <- BEST[SORT]
#
# x  <- 8
# while (length(unique(na.omit(head(BEST,n=x)))) < NCLUSTER)
#  x <- x + 1

# sum(100*unlist(values(m)[head(SORT,n=x)])/length(m))

# OK   <- data.frame(keys(m)[head(SORT,n=NCLUSTER)])
# OV   <- 100*unlist(values(m)[head(SORT,n=NCLUSTER)])/length(m)
# names(OK) <- 1:NCLUSTER
# write.csv(OK,file="bootstraped_clusters.csv")
#
# oKM <- COMPARE(OK,KM)
# rownames(oKM)[apply(oKM,2,which.max)]

# COMPARE <- function(a,b)
# {
#   aux <- matrix(nrow=length(a),ncol=length(b))
#   rel <- colSums(!is.na(b))
#
#   for (i in 1:length(a))
#     for (j in 1:length(b))
#       aux[i,j] <- sum(a[,i] %in% b[,j])
#
#   colnames(aux) <- names(b)
#   rownames(aux) <- names(a)
#
#   return(100*t(aux)/rel)
# }
