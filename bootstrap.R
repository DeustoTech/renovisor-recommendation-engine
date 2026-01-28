
# Este código evalúa la robustez y estabilidad de una tipología de clusters mediante bootstrapping,
# comparando arquetipos obtenidos por k-means con arquetipos expertos, y cuantificando
# la coincidencia estadística por región
#################################################################################################################################

################################################################## librerias ###############################################################

library("r2r") # se usa para estructuras tipo hashmap, que permiten contar frecuencias de forma eficiente.
library("effectsize") # cálculo y visualización de tamaños de efecto (no se usa directamente aquí, pero afecta a outputs).
library("skimr") # genera resúmenes estadísticos rápidos de data frames.
library("PMCMRplus") # tests no paramétricos post-hoc (aquí se usa frdAllPairsSiegelTest).

# options() para configurar ajustes globales del entorno de R
options(digits=2) # Muestra números con 2 decimales
options(es.use_symbols = TRUE) # Usa símbolos para tamaños de efecto

################################################################### funciones auxiliares ##############################################################
# Calcula el porcentaje de coincidencias de un vector 'a' con cada columna de 'b'(matriz de clusters de referencia).
# Retorna un vector con los porcentajes de match, tomando en cuenta los NA.
COMPARE2 <- function(a,b)
{
  aux <- numeric(length(b)) # Crea un vector numérico vacío     // Longitud = número de columnas de b
  rel <- colSums(!is.na(b)) # Cuenta cuántos valores no NA tiene cada columna de b

  for (j in 1:length(b)) # Para cada columna j de b:
    aux[j] <- sum(a %in% b[,j]) # Comprueba cuántos elementos de a aparecen en esa columna // Guarda el número de coincidencias en aux[j]

  names(aux) <- names(b) # Asigna a aux los nombres de las columnas de b
  return(100*t(aux)/rel) # Convierte el conteo en porcentaje
}

# # bootstrapping realiza múltiples clusterizaciones sobre submuestras de 'z'.
# Para cada cluster, identifica los factores más importantes y compara con clusters de referencia (KM).
# Retorna un vector con los porcentajes de coincidencia por cluster.
bootstrapping <- function(z,BOOT,BS,MIN)
  # z  matriz de datos
  # BOOT num de repeticiones bootstrap
  # BS tamaño de la submuestras
  # MIN umbral mínimo de coincidencia(%%)
{
  m <- hashmap(default = 0) # Crea un diccionario (hashmap)
  for(j in 1:BOOT) # bucle principal de bootstrap, repite el proceso boot veces
  {
    S <- sample(length(z[,1]),BS) # Selecciona aleatoriamente BS observaciones --> Sin reemplazo!!! --> S = Simula una submuestra bootstrap
    k <- kmeans( z[S,], NCLUSTER) # Aplica k-means sobre la submuestra --> Resultado --> Obtiene NCLUSTER clusters.
    # clave:
      # para cada centro de cluster:
        # calcula cuánto se aleja cada factor del valor neutro (50)
        # Ordena de mayor a menor importancia
        # Selecciona los NFACTORS más relevantes
        # Resultado: arquetipo del cluster
    a <- apply(k$centers,1, function(x) {
      AUX <- abs(x-50)/50;
      names(head(AUX[order(-AUX)],NFACTORS))
    })

# Cuenta cuántas veces aparece esa combinación de factores
# Cada cluster contribuye a la frecuencia total
    for (i in 1:NCLUSTER)
      m[[a[,i]]] <- m[[a[,i]]] + 1
  }

# BEST almacenará a qué cluster experto se parece más cada combinación
  BEST <- character(length(m))
  l    <- 1
  for (k in keys(m)) # Recorre todas las combinaciones(de determinantes que definen el arquetipo del cluster) observadas en el bootstrap
  {
    aux    <- COMPARE2(k,KM) # Compara la combinación k con los clusters expertos (KM)
    aux[aux <= MIN] <- NA # Descarta coincidencias débiles // Solo se consideran matches suficientemente fuertes
    
    if (all(is.na(aux))) {BEST[l] <- NA} # Si no hay match → NA // Si hay → se asigna el cluster experto con mayor coincidencia
    else                 {BEST[l] <- colnames(aux)[which.max(aux)]}
    l <- l+1
  }

  ZZ <- unique(BEST) # Identifica clusters únicos asignados
  r  <- numeric(length(ZZ)) # los cuenta
  names(r) <- ZZ

  r[1] <- sum(unlist(values(m)[is.na(BEST)])) # Cuenta casos sin correspondencia clara
  x <- 2

  for (c in unique(na.omit(ZZ))) # Suma frecuencias por cluster experto
  {
    r[x] <- sum(unlist(values(m)[BEST==c]))
    x <- x+1
  }
  return(100*r[order(names(r))]/length(m)) # Devuelve distribución final
}

############################################################## lectura de datos ###################################################
 
d  <- read.csv("data/Content_Export_Investment_Arquetypes_2022_full-latin-final.csv",skip=2)
KM <- read.csv("data/archetypeKmeans.csv")[-1] # elimina primera col

SAMPLE  <- length(d[[1]]) # numero total de observaciones

FACTORS <- c("Profits", "Credit Score", "Risk Profile", "Added Value",
             "Frugality", "Legal", "Trust", "Safety", "Climate Protection",
             "Cost-Efficiency", "Knowledge", "Own Competence",
             "Technical Fit", "Environmental Concerns", "Self-Satisfaction",
             "Commitment", "Adherence", "Autarky", "Wellbeing", "Cozyness",
             "Rights and Duties", "Peer-Pressure", "Support", "Socialising",
             "Agreement", "Novelty", "Fun", "Brag", "Trends", "Authority",
             "Own Significance", "Poseur")

NFACTORS <- 15 # cada arquet det
NCLUSTER <- 8 # 8 arquet

z <- as.matrix(d[,50:81]) # las columnas q se cogen
z[is.na(z)] <- 50 # rellena valores faltantes con valor neutro
z <- z[complete.cases(z),] # elimina filas con NA restantes
# z matriz. FILAS <- sample, COL <- num factores
dimnames(z) <- list(1:SAMPLE, FACTORS) # pone nombres a las dimensiones de z. --> pone las filas como "1", "2", "3","4"... y las col -> con los nombres de 'FACTORS' 

# separa la muestra en Europa (EU) y Latinoamérica (LA)
EU <- z[1:1000,]
LA <- z[1001:SAMPLE,]

#################################################################################### se ejecuta bootstrapping #############################################################################
# Se hacen 10,000 repeticiones de bootstrap con submuestras del 50% de cada región.
# Se calculan coincidencias respecto a los clusters de referencia.

# REP <- 10000
REP <- 100
# bootstrap para cada region
dEU <- bootstrapping(EU,REP,length(EU[,1])/2,100*8/NFACTORS)
dLA <- bootstrapping(LA,REP,length(LA[,1])/2,100*8/NFACTORS)

for (n in 9:12)
{
  # Repite análisis variando el umbral mínimo -- analiza sensibilidad del resutlado
  dEU <- rbind(dEU,bootstrapping(EU,REP,floor(length(EU[,1])/2),100*n/NFACTORS))
  dLA <- rbind(dLA,bootstrapping(LA,REP,floor(length(LA[,1])/2),100*n/NFACTORS))
}
rownames(dEU) <- rownames(dLA) <- 8:12

# guardado y visualización
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

# Evalúa si hay diferencias significativas
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
#################################################################################### intervalo de confianza ####################################################################################

# calculos de intervalos de confianza para los resultados de bootstrapping
# Regla de Chebysev
ALPHA <- 1-0.888
ALPHA <- 1-0.26

cat(c("NA",names(vEU)),"\n",file="confidence.csv")
# Límite inferior y superior del intervalo de confianza --> qauntile()
cat(apply(vEU,2,quantile,ALPHA/2),"\n",apply(vEU,2,quantile,1-ALPHA/2),"\n",file="confidence.csv",append=T)
cat(apply(vLA,2,quantile,ALPHA/2),"\n",apply(vLA,2,quantile,1-ALPHA/2),"\n",file="confidence.csv",append=T)

########################################################################################################################################################################

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
