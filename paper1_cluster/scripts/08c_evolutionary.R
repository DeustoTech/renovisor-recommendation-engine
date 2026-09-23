source("00_common.R")

library("future.apply")
library("data.table")
library("bigstatsr")
library("R.utils")
library("stringr")

# Configurar R para usar todos los núcleos disponibles (excepto 1 para no congelar el PC)
plan(multisession, workers = availableCores() - 2)
options(future.globals.maxSize = 10 * 1024^3)

# ==============================================================================
# OPTIMIZADOR DE PATRONES DE COMPORTAMIENTO EN R
# ==============================================================================

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

# 2. Función para evaluar la cobertura (Frecuencia Aglutinada)
evaluate_coverage <- function(centroid_indices, H, hamming_matrix, freqs) {
  if (length(centroid_indices) == 1) {
    min_dists <- hamming_matrix[, centroid_indices]
  } else {
    # Mínima distancia a cualquiera de los centroides elegidos
    min_dists <- apply(hamming_matrix[, centroid_indices], 1, min)
  }
  
  # Suma de frecuencias para los patrones a distancia <= H
  covered_mask <- min_dists <= H
  return(sum(freqs[covered_mask]))
}

# 1. Función para evaluar la cobertura AL VUELO (Ultra ligera en memoria)
evaluate_coverage_chunk <- function(centroid_indices, H, X, freqs) {
  # Extraer solo las k filas de los centroides elegidos: dimensión (k x 32)
  centroids <- X[centroid_indices, , drop = FALSE]

  # Cálculo algebraico de distancia Hamming entre todos los N patrones y los k centroides:
  # Para datos 0/1: Hamming(a,b) = sum(a != b) = a*(1-b) + (1-a)*b
  # Resultado: matriz de dimensión (N x k)
  dists <- X %*% (1 - t(centroids)) + (1 - X) %*% t(centroids)

  # Distancia mínima de cada patrón a cualquiera de los k centroides
  if (length(centroid_indices) > 1) {
    min_dists <- apply(dists, 1, min)
  } else {
    min_dists <- dists[, 1]
  }

  # Suma de frecuencias de los patrones a distancia <= H
  return(sum(freqs[min_dists <= H]))
}

# 3. Algoritmo Genético para un k y H fijos
run_genetic_algorithm <- function(k, H, X, freqs,
                                  pop_size = 50, generations = 50, mutation_rate = 0.1) {
  n_patterns <- nrow(X)
  
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
  
  return(list(indices = best_solution, max_freq = best_fitness))
}

# 4. Función Principal de Exploración (Grid Search)
run_pattern_optimizer <- function(df, k_values, H_values, 
                                 predefined_patterns = NULL, 
                                 pop_size = 50, generations = 50) {
  
  # Extraer datos: columnas 1 a 32 son patrones, columna 33 es frecuencia
  X <- as_FBM(as.matrix(df[, 1:32])) ### <- hago la variable global para que la compartan entre hilos
  freqs <- as.numeric(df[, 33])
  
  # Precalculo: En datos binarios, la distancia Manhattan es idéntica a Hamming
#   cat("Precalculando matriz de distancias Hamming en R...\n")
#   hamming_matrix <- as.matrix(dist(X, method = "manhattan"))
  
  results_list <- list()
  
  for (k in k_values) {
    for (H in H_values) {
      cat(sprintf("Optimizando para k = %d, H = %d...\n", k, H))
      
      # Ejecutar algoritmo genético
      ga_res <- run_genetic_algorithm(k, H, X, freqs, pop_size, generations)
      centroid_indices <- ga_res$indices
      centroids <- X[centroid_indices, , drop = FALSE]
      
      # Métrica 1: Coseno Interno (Diversidad)
      if (k > 1) {
        cos_int <- calc_cosine_sim(centroids, centroids)
        diag(cos_int) <- 0 # Ignorar diagonal
        internal_metric <- sum(abs(cos_int)) / 2
      } else {
        internal_metric <- 0.0
      }
      
      # Métrica 2: Coseno Externo (Alineación con prefijados)
      external_metric <- 0.0
      if (!is.null(predefined_patterns)) {
        P <- as.matrix(predefined_patterns)
        cos_ext <- calc_cosine_sim(centroids, P)
        external_metric <- sum(abs(cos_ext))
      }
      
      # Guardar resultados
      results_list[[length(results_list) + 1]] <- data.frame(
        k = k,
        H = H,
        Frecuencia_Aglutinada = ga_res$max_freq,
        Coseno_Interno_abs_sum = internal_metric,
        Coseno_Externo_abs_sum = external_metric,
        Patrones_Seleccionados = paste(centroid_indices, collapse = ","),
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Devolver DataFrame unificado
  return(do.call(rbind, results_list))
}

### df[,1:32] <- determinantes
### df[,33]   <- frequencia
run_pattern_optimizer_parallel <- function(df, k_values, H_values,
                                           predefined_patterns = NULL,
                                           pop_size = 50, generations = 50) {

  X <- as.matrix(df[, 1:32])
  freqs <- as.numeric(df[, 33])

#   cat("Precalculando matriz de distancias Hamming...\n")
#   hamming_matrix <- as.matrix(dist(X, method = "manhattan"))

  # 1. Crear una rejilla (grid) con todas las combinaciones de k y H
  grid <- expand.grid(k = k_values, H = H_values)
  cat(sprintf("Ejecutando %d experimentos en paralelo...\n", nrow(grid)))

  # 2. Paralelizar la iteración sobre la rejilla
  # future_lapply se encarga de repartir los experimentos entre los núcleos
  results_list <- future_lapply(1:nrow(grid), function(idx) {

    k_curr <- grid$k[idx]
    H_curr <- grid$H[idx]

    # Ejecutar Algoritmo Genético para este par (k, H)
    ga_res <- run_genetic_algorithm(k_curr, H_curr, X, freqs, pop_size, generations)
    centroid_indices <- ga_res$indices
    centroids <- X[centroid_indices, , drop = FALSE]

    # Métrica 1: Coseno Interno
    if (k_curr > 1) {
      cos_int <- calc_cosine_sim(centroids, centroids)
      diag(cos_int) <- 0
      internal_metric <- sum(abs(cos_int)) / 2
    } else {
      internal_metric <- 0.0
    }

    # Métrica 2: Coseno Externo
    external_metric <- 0.0
    if (!is.null(predefined_patterns)) {
      P <- as.matrix(predefined_patterns)
      cos_ext <- calc_cosine_sim(centroids, P)
      external_metric <- sum(abs(cos_ext))
    }

    return(data.frame(
      k = k_curr,
      H = H_curr,
      Frecuencia_Aglutinada = ga_res$max_freq,
      Coseno_Interno_abs_sum = internal_metric,
      Coseno_Externo_abs_sum = external_metric,
      Patrones_Seleccionados = paste(centroid_indices, collapse = ","),
      stringsAsFactors = FALSE
    ))
  }, future.seed = TRUE) # future.seed = TRUE asegura aleatoriedad segura entre hilos

  # Unir todos los resultados
  return(do.call(rbind, results_list))
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

df                  <- fread(in_file)
patrones_prefijados <- fread(in_pat)

matriz_str <- str_split_fixed(df$pattern_key, pattern = "", n = 32)
matriz_num <- matrix(as.integer(matriz_str), ncol = 32)
df_33_cols <- as.data.frame(cbind(matriz_num, frecuencia = df$n_occurrences))
colnames(df_33_cols)[1:32] <- paste0("v", 1:32)

rm(df)

# Ejecutar optimización
resultados <- run_pattern_optimizer_parallel(
  df = df_33_cols,
  k_values = 2:8,
  H_values = 0:10,
  predefined_patterns = patrones_prefijados,
  pop_size = 40,
  generations = 50
)

write.csv(file=paste0(out_dir,"results.csv"))
# Mostrar resultados
print(resultados[, 1:5])
