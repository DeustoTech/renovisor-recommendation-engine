# library("stringi")
# library("PMCMRplus")
# library("effectsize")

library("clValid")
library("factoextra")
library("kohonen")
library("caret")
library("foreach")
library("doParallel")

COMPARE <- function(M,BOOT,NC,NAME)
{
  bootfit <- 0
  df_best <- data.frame()
  for (b in 1:BOOT)
  {
      MCLUST <- M
      df <- data.frame()
      for (ii in 1:NC)
      {
          max_m <- max(MCLUST)
          idx   <- which(MCLUST == max_m, arr.ind=T)
          iaux  <- sample(nrow(idx),1)
          df    <- rbind(df, list(row=idx[iaux,1], col=idx[iaux,2], dist=max_m))
          MCLUST[idx[iaux,1],] <- -Inf
          MCLUST[,idx[iaux,2]] <- -Inf
      }

      if (bootfit < sum(df$dist))
      {
          bootfit <- sum(df$dist)
          df_best <- df
      }
  }

  names(df_best) <- c(NAME,"sins","dist")
  return(df_best[order(df_best$sins),])
}

d <- read.csv("data/Content_Export_Investment_Arquetypes_2022_full-latin-final.csv",skip=2)

SAMPLE  <- length(d[[1]])
FACTORS <- c("Profits", "Credit Score", "Risk Profile", "Added Value",
             "Frugality", "Legal", "Trust", "Safety", "Climate Protection",
             "Cost-Efficiency", "Knowledge", "Own Competence",
             "Technical Fit", "Environmental Concerns", "Self-Satisfaction",
             "Commitment", "Adherence", "Autarky", "Wellbeing", "Cozyness",
             "Rights and Duties", "Peer-Pressure", "Support", "Socialising",
             "Agreement", "Novelty", "Fun", "Brag", "Trends", "Authority",
             "Own Significance", "Poseur")

z <- as.matrix(d[,50:81])
z[is.na(z)] <- 50
dimnames(z) <- list(1:SAMPLE, FACTORS)
x <- scale(z)
pdf("results/cluster.pdf")
  par(mar = c(12, 3, 1, 1))
    boxplot(z,outline=F,names=FACTORS,las=2)
    boxplot(x,outline=F,names=FACTORS,las=2)
  par(mar = c(1,1,1,1))
dev.off()

PST   <- as.factor(d[,47])
NUDGE <- as.factor(d[,48])
SINS  <- as.factor(d[,49])

zz <- z/100

pstm   <- glm(PST~zz-1,  family=binomial(link='logit'))
nudgem <- glm(NUDGE~zz-1,family=binomial(link='logit'))
sinsm  <- glm(SINS~zz-1, family=binomial(link='logit'))

summary(pstm)
summary(nudgem)
summary(sinsm)


## Hacer un millón de cluaterizaciones kmeas y random
## Calcular la distancia a cada una de las clusterizaciones hechas por los expertos (incluida la nuestra) -> agreement
## Calcular la "distancia" a nuestros centros de cluster -> euclídea
## Calcular la distancia (en términos de bien clasificados) usando pst, nudge y sins -> kappa


# cluster <- clValid(z[complete.cases(z),],5:20,
#                    clMethods=c("kmeans","som","pam","diana"),
#                    validation=c("internal","stability"),maxitems=2000)
#
# cluster <- clValid(z[complete.cases(z),],5:15,
#                    clMethods=c("kmeans","som","pam","diana"),
#                    validation=c("internal"),maxitems=2000)
# plot(cluster)
#
# fviz_eig(pca)
# fviz_pca_ind(pca,
#             col.ind = "cos2", # Color by the quality of representation
#             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#             repel = TRUE     # Avoid text overlapping)
# fviz_pca_var(pca,haotmail
#           col.var = "contrib", # Color by contributions to the PC
#           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#           repel = TRUE     # Avoid text overlapping)


# archetypesKq <- apply(zzz$centers,1,function(x) { AUX <- abs(x-50)/50; names(AUX[which(AUX >= quantile(AUX,QUANTILE))])})

TARGET   <- read.csv("data/archetypeKmeans.csv")[,-1]
NFACTORS <- 15
NCLUSTER <- 8
SAVE     <- 0
C        <- 0
o        <- list()
of       <- data.frame()

while (SAVE < (NCLUSTER*NFACTORS))
{

    k_model     <- kmeans( z[complete.cases(z),], NCLUSTER)
    archetypesK <- apply(k_model$centers,1,function(x) { AUX <- abs(x-50)/50; names(head(AUX[order(-AUX)],NFACTORS))})

    MP <- matrix(nrow=NCLUSTER,ncol=NCLUSTER)
    for (i in 1:NCLUSTER)
        for (j in 1:NCLUSTER)
            MP[i,j] <- length(intersect(archetypesK[,i],TARGET[,j]))

    df  <- COMPARE(MP,1,NCLUSTER,"k-means")
    aux <- sum(df$dist)

    if (aux > SAVE)
    {
        SAVE <- aux
        o    <- k_model
        of   <- df
    }

    C <- C + 1
    cat(C," ",SAVE," ",sum(df$dist)," ",sum(apply(MP,2,max))," ",NCLUSTER*NFACTORS,"\n")
}
centersKmeans <- t(round(k_model$centers-50,2))

colnames(archetypesK)[df[[1]]] <- names(archetypes7)[df$sins]
colnames(centersKmeans)        <- colnames(archetypesK)

write.csv(of,file="translation.csv")
saveRDS(o,file="k_means_model-8-15")
write.csv(centersKmeans,file="weight.csv")
write.csv(o$cluster,file="clusters.csv")

pdf(file="boxplot-per-claster.pdf")
    for (i in 1:NCLUSTER)
        boxplot(z[which(o$cluster == i),],outline=F,names=FACTORS,las=2,main=names(archetypes7)[df$sins][i])
dev.off()

MED <- matrix(nrow=length(FACTORS),ncol=NCLUSTER)
for (i in 1:length(FACTORS))
    for (j in 1:NCLUSTER)
        MED[i,j] <- ecdf(z[,i])(o$centers[j])

archetypes7 <- read.csv("data/archetypeSINS.csv")
MSIM        <- colSums(archetypes7!= "")
BOOT        <- 200

cl <- parallel::makeCluster(parallel::detectCores() - 1)
doParallel::registerDoParallel(cl)
#     for (NCLUSTER in 4:9)
#         for (NFACTORS in 6:16) {
RESULT <- foreach(NCLUSTER=4:9,.combine='rbind') %:%
            foreach(NFACTORS=6:16,.combine='rbind',.packages=c("factoextra","kohonen","caret")) %dopar% {

  print(c(NCLUSTER,NFACTORS))

  RDIR <- paste("results/arquetypos",NCLUSTER,NFACTORS,sep="-")
  dir.create(RDIR, showWarnings = FALSE)

  # inicializo variables
  archetypesR <- matrix(nrow=NFACTORS,ncol=NCLUSTER)
  es <- ek <- er <- numeric(BOOT)
  bk <- bs <- bh <- bp <- br <- -10                   ## Kappa puede ser negativo
  ak <- as <- ah <- ap <- ar <- data.frame()
  dk <- ds <- dh <- dp <- dr <- data.frame()

  ces <- cek <- cer <- numeric(BOOT)
  cbk <- cbs <- cbh <- cbp <- cbr <- -10              ## Kappa puede ser negativo
  cak <- cas <- cah <- cap <- car <- data.frame()
  cdk <- cds <- cdh <- cdp <- cdr <- data.frame()

  # PCA y hierarchical son entrenamientos deterministas
  pca          <- prcomp(z[complete.cases(z),],scale=T)
  summary(pca)

  archetypesP  <- apply(pca$rotation,2,function(x) {names(head(x[order(-x)],NFACTORS))})[,1:NCLUSTER]

  hie_model    <- cutree(tree=hclust(dist(z[complete.cases(z),])),k=NCLUSTER)
  hie_centroid <- matrix(nrow=NCLUSTER,ncol=length(colnames(z)))
  colnames(hie_centroid) <- FACTORS
  for (i in 1:NCLUSTER)
      hie_centroid[i,] <- colMeans(z[hie_model == i,])
  archetypesH <- apply(hie_centroid,1,function(x) { AUX <- abs(x-50)/50; names(head(AUX[order(-AUX)],NFACTORS))})

  MP <- MH <- matrix(nrow=NCLUSTER,ncol=7)
  for (i in 1:NCLUSTER)
    for (j in 1:7)
    {
      MP[i,j] <- length(intersect(archetypesP[,i],archetypes7[,j]))
      MH[i,j] <- length(intersect(archetypesH[,i],archetypes7[,j]))
    }

  dfMP <- COMPARE(MP,BOOT/2,min(7,NCLUSTER),"PCA")
  dfMH <- COMPARE(MH,BOOT/2,min(7,NCLUSTER),"hierarchical")

  bp <- sum(dfMP$dist/MSIM[dfMP$sins])/min(7,NCLUSTER)*100
  ap <- archetypesP
  dp <- dfMP

  bh <- sum(dfMH$dist/MSIM[dfMH$sins])/min(7,NCLUSTER)*100
  ah <- archetypesH
  dh <- dfMH

  for (b in 1:BOOT)
  {
    print(c(NCLUSTER,NFACTORS,b))

    k_model <- kmeans( z[complete.cases(z),], NCLUSTER)

    som_model <- som( z[complete.cases(z),],grid = somgrid(NCLUSTER,1, "hexagonal"))
    som_centroid  <- matrix(nrow=NCLUSTER,ncol=length(colnames(z)))
    colnames(som_centroid) <- FACTORS
    for (i in 1:NCLUSTER)
      som_centroid[i,] <- colMeans(z[som_model$unit.classif == i,])

    archetypesK <- apply(k_model$centers,1,function(x) { AUX <- abs(x-50)/50; names(head(AUX[order(-AUX)],NFACTORS))})
    archetypesS <- apply(som_centroid,   1,function(x) { AUX <- abs(x-50)/50; names(head(AUX[order(-AUX)],NFACTORS))})

    for (i in 1:NCLUSTER)
      archetypesR[,i] <- sample(FACTORS,NFACTORS)

    MS <- MK <- MR <- matrix(nrow=NCLUSTER,ncol=7)
    for (i in 1:NCLUSTER)
      for (j in 1:7)
      {
        MS[i,j] <- length(intersect(archetypesS[,i],archetypes7[,j]))
        MK[i,j] <- length(intersect(archetypesK[,i],archetypes7[,j]))
        MR[i,j] <- length(intersect(archetypesR[,i],archetypes7[,j]))
      }

    dfMS <- COMPARE(MS,BOOT/2,min(7,NCLUSTER),"SOM")
    dfMK <- COMPARE(MK,BOOT/2,min(7,NCLUSTER),"k-means")
    dfMR <- COMPARE(MR,BOOT/2,min(7,NCLUSTER),"random")

    es[b] <- sum(dfMS$dist/MSIM[dfMS$sins])/min(7,NCLUSTER)*100
    ek[b] <- sum(dfMK$dist/MSIM[dfMK$sins])/min(7,NCLUSTER)*100
    er[b] <- sum(dfMR$dist/MSIM[dfMR$sins])/min(7,NCLUSTER)*100

    if (es[b] > bs)
    {
      bs <- es[b]
      as <- archetypesS
      ds <- dfMS
    }

    if (ek[b] > bk)
    {
      bk <- ek[b]
      ak <- archetypesK
      dk <- dfMK
    }

    if (er[b] > br)
    {
      br <- er[b]
      ar <- archetypesR
      dr <- dfMR
    }

    # confusion Matrix
    CMS <- CMK <- CMR <- matrix(nrow=NCLUSTER,ncol=7)

    CMS <- table(som_model$unit.classif,SINS)
    CMK <- table(k_model$cluster,SINS)

    cdfMS <- COMPARE(CMS,BOOT/2,min(7,NCLUSTER),"SOM")
    cdfMK <- COMPARE(CMK,BOOT/2,min(7,NCLUSTER),"k-means")

    ps <- factor(som_model$unit.classif,levels=c((1:9)[cdfMS[[1]]], setdiff(1:9,cdfMS[[1]])), labels=SINSN)
    pk <- factor(k_model$cluster,       levels=c((1:9)[cdfMK[[1]]], setdiff(1:9,cdfMK[[1]])), labels=SINSN)

    cons <- confusionMatrix(data= ps, reference = SINS)
    conk <- confusionMatrix(data= pk, reference = SINS)

    ces[b] <- cons$overall[2]
    cek[b] <- conk$overall[2]

    if (ces[b] > cbs)
    {
      cbs <- ces[b]
      cas <- archetypesS
      cds <- dfMS
    }

    if (cek[b] > cbk)
    {
      cbk <- cek[b]
      cak <- archetypesK
      cdk <- dfMK
    }
  }

  pdf(paste(RDIR,"success.pdf",sep="/"))
    boxplot(es,ek,er,outline=F,notch=T,ylab="%",names=c("SOM","K-means","Random"),main="Success distribution (%)")
  dev.off()

  pdf(paste(RDIR,"kappa.pdf",sep="/"))
    boxplot(ces,cek, outline=F,notch=T,ylab="%",names=c("SOM","K-means"),main="Kappa distribution")
  dev.off()

  colnames(ah)  <- 1:NCLUSTER
  colnames(ap)  <- 1:NCLUSTER
  colnames(ak)  <- 1:NCLUSTER
  colnames(as)  <- 1:NCLUSTER
  colnames(ah)  <- 1:NCLUSTER
  colnames(ar)  <- 1:NCLUSTER
  colnames(cak) <- 1:NCLUSTER
  colnames(cas) <- 1:NCLUSTER

  colnames(ah)[dh[[1]]] <-  names(archetypes7)[dh$sins]
  colnames(ap)[dp[[1]]] <-  names(archetypes7)[dp$sins]
  colnames(ak)[dk[[1]]] <-  names(archetypes7)[dk$sins]
  colnames(as)[ds[[1]]] <-  names(archetypes7)[ds$sins]
  colnames(ah)[dh[[1]]] <-  names(archetypes7)[dh$sins]
  colnames(ar)[dr[[1]]] <-  names(archetypes7)[dr$sins]

  write.csv(ap, file=paste(RDIR,"archetypePCA.csv"   ,sep="/"))
  write.csv(ak, file=paste(RDIR,"archetypeKmeans.csv",sep="/"))
  write.csv(as, file=paste(RDIR,"archetypeSOM.csv"   ,sep="/"))
  write.csv(ah, file=paste(RDIR,"archetypeHie.csv"   ,sep="/"))
  write.csv(ar, file=paste(RDIR,"archetypeRandom.csv",sep="/"))

  write.csv(dp, file=paste(RDIR,"traductionPCA.csv"   ,sep="/"))
  write.csv(dk, file=paste(RDIR,"traductionKmeans.csv",sep="/"))
  write.csv(ds, file=paste(RDIR,"traductionSOM.csv"   ,sep="/"))
  write.csv(dh, file=paste(RDIR,"traductionHie.csv"   ,sep="/"))
  write.csv(dr, file=paste(RDIR,"traductionRandom.csv",sep="/"))

  write.csv(k_model$cluster       , file=paste(RDIR,"asignacionRAW-Kmeans.csv",sep="/"))
  write.csv(som_model$unit.classif, file=paste(RDIR,"asignacionRAW-SOM.csv"   ,sep="/"))

  colnames(cak)[cdk[[1]]] <- names(archetypes7)[cdk$sins]
  colnames(cas)[cds[[1]]] <- names(archetypes7)[cds$sins]

  write.csv(cak, file=paste(RDIR,"archetypeKmeansConfusion.csv",sep="/"))
  write.csv(cas, file=paste(RDIR,"archetypeSOMConfusion.csv"   ,sep="/"))

  write.csv(cdk, file=paste(RDIR,"traductionKmeansConfusion.csv",sep="/"))
  write.csv(cds, file=paste(RDIR,"traductionSOMConfusion.csv"   ,sep="/"))

  cat(c(bp,bs,bh,bk,br),file=paste(RDIR,"success.txt"          ,sep="/"))
  cat(c(cbs,cbk),       file=paste(RDIR,"success_confusion.txt",sep="/"))

  c(paste(NCLUSTER,NFACTORS,sep="-"),NCLUSTER,NFACTORS,bp,bs,bh,bk,br,cbs,cbk)
}
parallel::stopCluster(cl)

RESULT            <- as.data.frame(unlist(RESULT))
row.names(RESULT) <- RESULT[,1]
RESULT            <- RESULT[,-1]
colnames(RESULT)  <- c("NCLUSTER","NFACTORS","success.PCA","success.SOM", "success.HIE", "success.KMEAN","success.RANDOM","kappa.SOM","kappa.KMEAN")

write.csv(RESULT,file="results/summaryCluster.csv")


pdf("results/personal-character.pdf")
    PSTN   <- c("Pinball","Shortcut","Thoughtful","Other","NA")
    NUDGEN <- c("Well-informed","Comfort","Awareness","Materialist","Peers Pressure","Indifferent","Other","NA")
    SINSN  <- c("Early adopter", "Uninterested", "Cost-effective", "Safety", "Environmental", "Authority", "Comfort", "Other", "NA")

    PST    <- unlist(stri_extract_all_regex(d[[9]],pattern="ID[0-9]"))
    NUDGE  <- unlist(stri_extract_all_regex(d[[10]],pattern="ID[0-9]+"))
    SINS   <- unlist(stri_extract_all_regex(d[[11]],pattern="ID[0-9]+"))

    PST    <- factor(PST,  levels=c("ID6","ID7","ID8","ID9","ID10"), labels=PSTN)
    NUDGE  <- factor(NUDGE, levels=c("ID21","ID22","ID126","ID128","ID129","ID130","ID15","ID16"), labels=NUDGEN)
    SINS   <- factor(SINS, levels=c("ID148","ID149","ID150","ID151","ID152","ID153","ID154","ID17","ID23"), labels=SINSN)

    PST[  is.na(PST)]   <- "NA"
    NUDGE[is.na(NUDGE)] <- "NA"
    SINS[ is.na(SINS)]  <- "NA"

#     barplot(table(PST))
#     barplot(table(NUDGE))
#     barplot(table(SINS))

    par(mfrow=c(1,3))
        barplot(100*prop.table(table(PST)),names.arg=PSTN, ylab="%")
        barplot(100*prop.table(table(NUDGE)),names.arg=NUDGEN, ylab="%")
        barplot(100*prop.table(table(SINS)),names.arg=SINSN, ylab="%")
    par(mfrow=c(1,1))
dev.off()


#     fviz_cluster(zzz, data = z[complete.cases(z),],
#                 palette = rainbow(NCLUSTER),
#                 geom = "point",
#                 ellipse.type = "convex",
#                 ggtheme = theme_bw()
#                 )
#     apply(abs(zzz$centers-50)/50,1,max) <- DEBUG

# table(factor((unlist(as.vector(a))),levels=FACTORS))
# b <- data.frame()
# for (i in names(a)) b <- rbind(b,(table(factor(a[,i],levels=FACTORS,labels=FACTORS))))
# names(a)    <- FACTORS
# rownames(b) <- names(a)

# CODO  <- numeric(length(2:32)-1)
# FINAL <- matrix(nrow=(length(7:10)),ncol=(length(2:32)))
# for (NCLUSTER in 7:10)
# {
#     for (NFACTORS in 2:32)
#     {
#         som_model <- som( z[complete.cases(z),],grid = somgrid(NCLUSTER,1, "hexagonal"))
#         som_centroid  <- matrix(nrow=NCLUSTER,ncol=length(colnames(z)))
#         colnames(som_centroid) <- FACTORS
#         for (i in 1:NCLUSTER)
#             som_centroid[i,] <- colMeans(z[som_model$unit.classif == i,])
#
#         archetypesS <- apply(som_centroid,1,function(x) { AUX <- abs(x-50)/50; names(head(AUX[order(-AUX)],NFACTORS))})
#
#         MS <- matrix(nrow=NCLUSTER,ncol=7)
#         for (i in 1:NCLUSTER)
#             for (j in 1:7)
#                 MS[i,j] <- length(intersect(archetypesS[,i],archetypes7[,j]))
#
#         dfMS <- COMPARE(MS,1000,7,"SOM")
#         CODO[NFACTORS] <- sum((dfMS[order(dfMS$col),]/MSIM)$dist)
#     }
#     FINAL[NCLUSTER-6,] <- (CODO/7*100)[2:32]
# }
#
# plot(2:32,FINAL[1,],type="l",col=rainbow(4)[1],xlab="Number of Determinants",ylab="%")
# lines(2:32,FINAL[2,],col=rainbow(4)[2])
# lines(2:32,FINAL[3,],col=rainbow(4)[3])
# lines(2:32,FINAL[4,],col=rainbow(4)[4])
# legend(x="topleft",legend=7:10,col=rainbow(4),fill=rainbow(4))
#
# dendo <- heatmap(cor(z[complete.cases(z),]),symm=T, keep.dendro=T)
# sort(cutree(tree=as.hclust(dendo$Rowv),k=NCLUSTER))
# plot(as.hclust(dendo$Rowv))
