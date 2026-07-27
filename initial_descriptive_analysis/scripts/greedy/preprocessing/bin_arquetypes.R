library(data.table)

## fichero para tansformar los csv de expertos a binario!! 
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

add_missing_cols <- function(df, target_cols) {
  missing <- setdiff(target_cols, colnames(df))
  if (length(missing) > 0) {
    for (m in missing) df[[m]] <- 0
  }
  df[, target_cols, drop = FALSE]
}


binarizar_arquetipos <- function(file_path) {
  df <- fread(file_path, fill = TRUE)
  
  # quitar columnas basura tipo Unnamed
  df <- df[, !grepl("^Unnamed", names(df)), with = FALSE]
  
  # limpiar contenido
  df <- as.data.frame(lapply(df, function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x[x == ""] <- NA
    x[x == "NA"] <- NA
    x
  }))
  
  # determinantes únicos
  dets <- sort(unique(na.omit(unlist(df))))
  
  # matriz binaria
  bin <- matrix(0, nrow = ncol(df), ncol = length(dets))
  colnames(bin) <- dets
  rownames(bin) <- colnames(df)
  
  for (i in seq_along(df)) {
    vals <- unique(na.omit(df[[i]]))
    idx <- match(vals, colnames(bin))
    idx <- idx[!is.na(idx)]
    if (length(idx) > 0) bin[i, idx] <- 1
  }
  
  as.data.frame(bin, check.names = FALSE)
}

binarizar_kmeans <- function(file_path) {
  df <- fread(file_path, fill = TRUE, header = FALSE)
  
  # quitar primera columna índice
  df <- df[, -1, with = FALSE]
  
  # primera fila = nombres reales de arquetipos
  nuevos_nombres <- as.character(unlist(df[1, ]))
  setnames(df, nuevos_nombres)
  
  # quitar esa primera fila
  df <- df[-1, ]
  
  # limpiar contenido
  df <- as.data.frame(lapply(df, function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x[x == ""] <- NA
    x[x == "NA"] <- NA
    x
  }))
  
  # determinantes únicos
  dets <- sort(unique(na.omit(unlist(df))))
  
  # matriz binaria
  bin <- matrix(0, nrow = ncol(df), ncol = length(dets))
  colnames(bin) <- dets
  rownames(bin) <- colnames(df)
  
  for (i in seq_along(df)) {
    vals <- unique(na.omit(df[[i]]))
    idx <- match(vals, colnames(bin))
    idx <- idx[!is.na(idx)]
    if (length(idx) > 0) bin[i, idx] <- 1
  }
  
  as.data.frame(bin, check.names = FALSE)
}

bin_experts <- binarizar_arquetipos("data/archetypeExperts.csv")
bin_kmeans  <- binarizar_kmeans("data/archetypeKmeans.csv")
bin_sins    <- binarizar_arquetipos("data/archetypeSINS.csv")

dm <- read.csv2("data/determinants.csv", row.names = 1, check.names = FALSE)
all_dets <- recode_dets(rownames(dm))


colnames(bin_experts) <- recode_dets(colnames(bin_experts))
colnames(bin_kmeans)  <- recode_dets(colnames(bin_kmeans))
colnames(bin_sins)    <- recode_dets(colnames(bin_sins))

# opcional: arreglar nombre raro de kmeans
rownames(bin_kmeans)[rownames(bin_kmeans) == "8"] <- "Cluster8"


bin_experts_32 <- add_missing_cols(bin_experts, all_dets)
bin_kmeans_32  <- add_missing_cols(bin_kmeans,  all_dets)
bin_sins_32    <- add_missing_cols(bin_sins,    all_dets)

# asegurar orden y tipo
bin_experts_32 <- as.data.frame(bin_experts_32, check.names = FALSE)
bin_kmeans_32  <- as.data.frame(bin_kmeans_32,  check.names = FALSE)
bin_sins_32    <- as.data.frame(bin_sins_32,    check.names = FALSE)


fwrite(
  as.data.table(bin_experts_32, keep.rownames = "Archetype"),
  "data/dataPreproc/archetypeExperts_bin_32.csv"
)

fwrite(
  as.data.table(bin_kmeans_32, keep.rownames = "Archetype"),
  "data/dataPreproc/archetypeKmeans_bin_32.csv"
)

fwrite(
  as.data.table(bin_sins_32, keep.rownames = "Archetype"),
  "data/dataPreproc/archetypeSINS_bin_32.csv"
)

# comproaciones
cat("Experts columnas:", ncol(bin_experts_32), "\n")
cat("Kmeans  columnas:", ncol(bin_kmeans_32), "\n")
cat("SINS    columnas:", ncol(bin_sins_32), "\n")

cat("Columns iguales Experts/Kmeans:", identical(colnames(bin_experts_32), colnames(bin_kmeans_32)), "\n")
cat("Columns iguales Experts/SINS:", identical(colnames(bin_experts_32), colnames(bin_sins_32)), "\n")
cat("Columns iguales con determinants:", identical(colnames(bin_experts_32), all_dets), "\n")