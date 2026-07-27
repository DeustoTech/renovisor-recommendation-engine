library(data.table)

## fichero para pasar de 32 determinantes a 9 dimensiones

norm_txt <- function(x) {
  x <- toupper(as.character(x))
  x <- trimws(x)
  x <- gsub("[[:punct:]]", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  x
}

recode_dets <- function(x) {
  x <- norm_txt(x)
  x[x == "BRAG"]     <- "RECOGNITION"
  x[x == "POSEUR"]   <- "APPROVAL"
  x[x == "COZYNESS"] <- "COZINESS"
  x
}

# leer binarizados 32 ya preparados
bin_experts_32 <- fread("data/dataPreproc/archetypeExperts_bin_32.csv")
bin_kmeans_32  <- fread("data/dataPreproc/archetypeKmeans_bin_32.csv")
bin_sins_32    <- fread("data/dataPreproc/archetypeSINS_bin_32.csv")

# pasar nombres de fila
bin_experts_32 <- as.data.frame(bin_experts_32, check.names = FALSE)
bin_kmeans_32  <- as.data.frame(bin_kmeans_32, check.names = FALSE)
bin_sins_32    <- as.data.frame(bin_sins_32, check.names = FALSE)

rownames(bin_experts_32) <- bin_experts_32$Archetype
rownames(bin_kmeans_32)  <- bin_kmeans_32$Archetype
rownames(bin_sins_32)    <- bin_sins_32$Archetype

bin_experts_32$Archetype <- NULL
bin_kmeans_32$Archetype  <- NULL
bin_sins_32$Archetype    <- NULL

# normalizar nombres de columnas
colnames(bin_experts_32) <- recode_dets(colnames(bin_experts_32))
colnames(bin_kmeans_32)  <- recode_dets(colnames(bin_kmeans_32))
colnames(bin_sins_32)    <- recode_dets(colnames(bin_sins_32))

# a matriz numérica
bin_experts_32 <- as.matrix(bin_experts_32)
bin_kmeans_32  <- as.matrix(bin_kmeans_32)
bin_sins_32    <- as.matrix(bin_sins_32)

mode(bin_experts_32) <- "numeric"
mode(bin_kmeans_32)  <- "numeric"
mode(bin_sins_32)    <- "numeric"

# leer matriz 32 -> 9
dm <- read.csv2("data/determinants.csv", row.names = 1, check.names = FALSE)
dm <- as.matrix(dm)
mode(dm) <- "numeric"

# normalizar nombres de dm
rownames(dm) <- recode_dets(rownames(dm))
colnames(dm) <- norm_txt(colnames(dm))

# comprobar si coincide todo
setdiff(colnames(bin_experts_32), rownames(dm))

# asegurar mismo orden de determinantes
dm_32_9 <- dm[colnames(bin_experts_32), , drop = FALSE]

# proyección a 9
experts_9 <- bin_experts_32 %*% dm_32_9
kmeans_9  <- bin_kmeans_32  %*% dm_32_9
sins_9    <- bin_sins_32    %*% dm_32_9

# pasar a data.frame
experts_9 <- as.data.frame(experts_9)
kmeans_9  <- as.data.frame(kmeans_9)
sins_9    <- as.data.frame(sins_9)

rownames(experts_9) <- rownames(bin_experts_32)
rownames(kmeans_9)  <- rownames(bin_kmeans_32)
rownames(sins_9)    <- rownames(bin_sins_32)

# guardar
fwrite(as.data.table(experts_9, keep.rownames = "Archetype"),
       "data/archetypeExperts_9.csv")

fwrite(as.data.table(kmeans_9, keep.rownames = "Archetype"),
       "data/archetypeKmeans_9.csv")

fwrite(as.data.table(sins_9, keep.rownames = "Archetype"),
       "data/archetypeSINS_9.csv")