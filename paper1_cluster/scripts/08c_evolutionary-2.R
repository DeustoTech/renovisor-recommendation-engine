source("00_common.R")

library("future.apply")
library("data.table")
library("bigstatsr")
library("R.utils")
library("stringr")
library("progressr")

# Configurar R para usar todos los núcleos disponibles (excepto 1 para no congelar el PC)
plan(multisession, workers = availableCores() - 2)
options(future.globals.maxSize = 10 * 1024^3)

handlers(global = TRUE)
handlers("cli")

# 1. Función para calcular la matriz de Similitud Coseno
calc_cosine_sim <- function(A, B) {
  # Normas de cada fila
  norm_A <- sqrt(rowSums(A^2))
  norm_B <- sqrt(rowSums(B^2))
  
  # Evitar división por cero
  norm_A[norm_A == 0] <- 1e-10
  norm_B[norm_B == 0] <- 1e-10
  
  # Producto escalar normalizado
  sim <- (A %*% t(B)) / (norm_A %*% t(norm_B))
  return(sim)
}

# 1. Función para evaluar la cobertura AL VUELO (Ultra ligera en memoria)
evaluate_coverage_chunk <- function(centroid_indices, H, X, freqs) {
  # Extraer solo las k filas de los centroides elegidos: dimensión (k x 32)
  centroids <- X[centroid_indices, , drop = FALSE]

  # Cálculo algebraico de distancia Hamming entre todos los N patrones y los k centroides:
  # Para datos 0/1: Hamming(a,b) = sum(a != b) = a*(1-b) + (1-a)*b
  # Resultado: matriz de dimensión (N x k)
  X_mat <- X[]
  dists <- X_mat %*% (1 - t(centroids)) + (1 - X_mat) %*% t(centroids)

  # Distancia mínima de cada patrón a cualquiera de los k centroides
  if (length(centroid_indices) > 1) {
    min_dists <- apply(dists, 1, min)
  } else {
    min_dists <- dists[, 1]
  }

  # Suma de frecuencias de los patrones a distancia <= H
  return(sum(freqs[min_dists <= H]))
}

in_file <- file.path(
  processed_root,
  "08b_greedy_kmeans_efa_Dpooled",
  "02_pattern_frequency_Dpooled.csv.gz"
)

in_pat <- file.path(
  data_root,
  "archetypes",
  "archetypeExperts_bin_32.csv"
)

out_dir <- file.path(
  processed_root,
  "08c_evolutionary"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

df      <- fread(in_file)
experts <- read.csv(in_pat)[,2:33]

freqs      <- df$n_occurrences
total      <- sum(freqs)
matriz_str <- str_split_fixed(df$pattern_key, pattern = "", n = 32)
matriz_num <- matrix(as.integer(matriz_str), ncol = 32)
X          <- as.data.table(matriz_num)
colnames(X)[1:32] <- paste0("v", 1:32)
X          <- as_FBM(X)
rm(df)

# 1. Crear una rejilla (grid) con todas las combinaciones de k y H
grid <- expand.grid(k = 2:8, H = 0:10)
cat(sprintf("Ejecutando %d experimentos en paralelo...\n", nrow(grid)))

# 2. Paralelizar la iteración sobre la rejilla
# future_lapply se encarga de repartir los experimentos entre los núcleos

# 3. Run future_lapply with a progress bar
with_progress({
  # Initialize a progress handler matching the number of iterations
  p <- progressor(steps = 10)
  results_list <- future_lapply(1:nrow(grid), function(idx) {

    k <- grid$k[idx]
    H <- grid$H[idx]

    # Ejecutar Algoritmo Genético para este par (k, H)
    pop_size      <- 50
    generations   <- 50
    mutation_rate <- 0.1
    n_patterns    <- nrow(X)

    # Inicializar población (cada individuo es una combinación de k índices)
    population <- replicate(pop_size, sample(1:n_patterns, k), simplify = FALSE)

    best_solution <- NULL
    best_fitness <- -1

    for (gen in 1:generations) {
      # Evaluar aptitud (fitness) de cada individuo
      fitness <- sapply(population, function(ind) {
        evaluate_coverage_chunk(ind, H, X, freqs)
      })

      # Guardar la mejor solución de la generación
      max_idx <- which.max(fitness)
      if (fitness[max_idx] > best_fitness) {
        best_fitness <- fitness[max_idx]
        best_solution <- population[[max_idx]]
      }

      # Selección por Torneo
      selected <- list()
      for (i in 1:pop_size) {
        tour_indices <- sample(1:pop_size, 2)
        if (fitness[tour_indices[1]] >= fitness[tour_indices[2]]) {
          selected[[i]] <- population[[tour_indices[1]]]
        } else {
          selected[[i]] <- population[[tour_indices[2]]]
        }
      }

      # Cruce (Crossover) y Mutación
      next_generation <- list()
      for (i in seq(1, pop_size, by = 2)) {
        parent1 <- selected[[i]]
        parent2 <- selected[[(i %% pop_size) + 1]]

        # Mezclar índices únicos de ambos padres
        pool <- unique(c(parent1, parent2))

        if (length(pool) >= k) {
          child1 <- sample(pool, k)
          child2 <- sample(pool, k)
        } else {
          child1 <- parent1
          child2 <- parent2
        }

        # Mutación
        children <- list(child1, child2)
        for (c_idx in 1:2) {
          if (runif(1) < mutation_rate) {
            replace_pos <- sample(1:k, 1)
            new_centroid <- sample(1:n_patterns, 1)
            if (!(new_centroid %in% children[[c_idx]])) {
              children[[c_idx]][replace_pos] <- new_centroid
            }
          }
        }

        next_generation[[length(next_generation) + 1]] <- children[[1]]
        next_generation[[length(next_generation) + 1]] <- children[[2]]
      }

      population <- next_generation
    }

    ga_res <- list(indices = best_solution, max_freq = best_fitness)

    centroid_indices <- ga_res$indices
    centroids <- X[centroid_indices, , drop = FALSE]

    # Métrica 1: Coseno Interno
    if (k > 1) {
      cos_int <- calc_cosine_sim(centroids, centroids)
      diag(cos_int) <- 0
      internal_metric <- sum(abs(cos_int)) / 2
    } else {
      internal_metric <- 0.0
    }

    # Métrica 2: Coseno Externo
    external_metric <- 0.0
    if (!is.null(experts)) {
      cos_ext <- calc_cosine_sim(centroids, experts)
      external_metric <- sum(abs(cos_ext))
    }

    return(data.frame(
      k = k,
      H = H,
      Frecuencia_Aglutinada  = ga_res$max_freq/total,
      Coseno_Interno_abs_sum = internal_metric,
      Coseno_Externo_abs_sum = external_metric,
      Patrones_Seleccionados = paste(apply(centroids, 1, paste, collapse = ""),collapse=";"),
      stringsAsFactors = FALSE
    ))
  }, future.seed = TRUE) # future.seed = TRUE asegura aleatoriedad segura entre hilos
})

# Unir todos los resultados
resultados <- do.call(rbind, results_list)

write.csv(resultados, file=paste0(out_dir,"results.csv"))
# Mostrar resultados
print(resultados[, 1:5])
