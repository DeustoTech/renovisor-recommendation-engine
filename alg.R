# La idea es hacer una especie de kmeans para buscar los arquetipos centroides que maximizan
# la frecuencia de los arquetipos que están dentro de cada cluster (y por lo tanto minimizan la 
# frecuencia de los que se quedan fuera). 

# Vamos a intentar una versión Greedy a ver que tal sale 
# y luego si eso intentamos una estrategia evolutiva. El algoritmo iría tal que así:

    # i = 1
    # Recorrer m (el hashmap) para localizar el arquetipo X (clave de m) más frecuente (valor de m más alto) que no haya sido incluído en un cluster.
    # Localizar todos los arquetipos (claves de m) que distan como mucho D determinantes de X y que no haya sido
    # incluído en un cluster. Esto es, todas las claves Y tales que sum(Y!=X) <= D (por lo que hay que volver a recorrer las claves de m). 
    # Marcar todas estas claves como pertenecientes al cluster i y asociarles de métrica de calificación la suma de las frecuencias de dichos
    #  arquetipos (suma de los valores de m de dichas claves).
    # i++
    # Repetir mientras i <= K o queden claves sin incluir. 
    # Luego tendremos que analizar los clusters que quedan y cuánto se parecen a los que salieron de los expertos, los que salían de la otra 
    # clusterización o los que hicimos a mano.

# Transformar esto en un evolutivo es fácil pues la población son simplemente K claves y la función de calificación es la suma de los valores
#  de las claves D similares (calculada en orden). Buscamos las K claves que maximizan la función de calificación. 

# Por supuesto, tenemos unos cuantos grados de libertad / hiperparámetros para tocar para ver si conseguimos que los resultados se parezcan algo 
# a lo que hicimos con los expertos: el número de clusters inicial K, el número de determinantes NM, el punto donde cortamos MAX y el
#  parámetros de similitud D. Ojo que no deberíamos de caer en la tentación de hacer cherry picking y minimizar la distancia con lo que dijeron
# los expertos, aunque podría ser "metodológicamente" aceptable. 

# Finalmente, podríamos usar kmodes [1] o un kmean con distancia de Hamming [2] a ver qué sale pero esta estrategia no tendría en cuenta la frecuencia con la que aparecen los arquetipos. 

# [1] https://search.r-project.org/CRAN/refmans/klaR/html/kmodes.html
# [2] https://cran.r-project.org/web/packages/Kmedians/index.html


# de los expertos --> archetypesExperts.csv
#e <- read.csv("data/archetypesExperts.csv", skip = 2)

###########################################################################################################################################################################

library(r2r)

m <- readRDS("cluster_hash.rds.xz")

K <- 8      # número de clusters
D <- 3      # distancia máxima de Hamming (hiperparámetro) --> CAMBIAR MUY RESTRICTIVA

# claves del hashmap -> arquetipos
keys_list <- keys(m)

# frecuencia asociada a cada arquetipo
freqs <- sapply(keys_list, function(k) query(m, k))

# lista de arquetipos a matriz
# filas = arquetipos, columnas = determinantes
keys_mat <- do.call(rbind, keys_list)

n_arch <- nrow(keys_mat)  # número total de arquetipos únicos

# distancia de Hamming --> cuántos determinantes son distintos entre dos arquetipos
hamming_dist <- function(x, y) {
  sum(x != y)
}

# dado un centro:
#   - busca todos los arquetipos  no usados
#   - cuya distancia de Hamming <= D
#   - devuelve índices y frecuencia total cubierta
find_similar <- function(center, keys_mat, freqs, used, D) {

  # indices de arquetipos todavía no asignados
  free_idx <- which(!used)

  # distancia de Hamming al centro
  dists <- apply(
    keys_mat[free_idx, ],
    1,
    function(y) hamming_dist(center, y)
  )

  # seleccionamos los suficientemente similares -> CAMBUO
  sel <- free_idx[dists <= D]

  list(
    indices = sel,
    score   = sum(freqs[sel])
  )
}

# algoritmo greedy:
#   - elige el arquetipo más frecuente no usado
#   - agrupa todos los similares (<= D)
#   - los marca como usados
greedy_clusters <- function(keys_mat, freqs, K, D) {

  n <- nrow(keys_mat)

  used <- rep(FALSE, n)      #  arquetipos ya están asignados
  clusters <- vector("list", K)
  scores   <- numeric(K)

  for (i in 1:K) {

    # stop si ya no quedan arquetipos libres
    if (all(used)) break

    # centro = arquetipo más frecuente no usado
    candidates <- which(!used)
    center_idx <- candidates[which.max(freqs[candidates])]
    center     <- keys_mat[center_idx, ]

    # buscar arquetipos similares
    sim <- find_similar(center, keys_mat, freqs, used, D)

    # marcarlos como usados
    used[sim$indices] <- TRUE

    clusters[[i]] <- list(
      center_index = center_idx,
      center       = center,
      members      = sim$indices
    )

    scores[i] <- sim$score
  }

  list(
    clusters = clusters,
    scores   = scores,
    used     = used
  )
}


res <- greedy_clusters(keys_mat, freqs, K, D)

# frecuencia total cubierta por cada cluster
res$scores
# número de arquetipos en cada cluster
sapply(res$clusters, function(cl) length(cl$members))
# arquetipos no asignados
sum(!res$used)


###################################################################################################################################################

dir.create("results", showWarnings = FALSE)

centers_df <- do.call(
  rbind,
  lapply(seq_along(res$clusters), function(i) {
    data.frame(
      cluster = i,
      t(res$clusters[[i]]$center)
    )
  })
)

write.csv(
  centers_df,
  "results/greedy_cluster_centers.csv",
  row.names = FALSE
)

assignments_df <- do.call(
  rbind,
  lapply(seq_along(res$clusters), function(i) {
    data.frame(
      archetype_id = res$clusters[[i]]$members,
      cluster      = i,
      frequency    = freqs[res$clusters[[i]]$members]
    )
  })
)

write.csv(
  assignments_df,
  "results/greedy_cluster_assignments.csv",
  row.names = FALSE
)

summary_df <- data.frame(
  cluster         = seq_len(K),
  n_archetypes    = sapply(res$clusters, function(cl) length(cl$members)),
  total_frequency = res$scores
)

write.csv(
  summary_df,
  "results/greedy_cluster_summary.csv",
  row.names = FALSE
)

unassigned_df <- data.frame(
  archetype_id = which(!res$used),
  frequency    = freqs[!res$used]
)

write.csv(
  unassigned_df,
  "results/greedy_unassigned_archetypes.csv",
  row.names = FALSE
)


###################################################################################################################################################


# ver arquetipos de cluster 1
cluster1_indices <- res$clusters[[1]]$members
cluster1_archetypes <- keys_mat[cluster1_indices, ]
cluster1_freqs <- freqs[cluster1_indices]



############################
# arquetipos sin clutser erstantes???
# indices de arquetipos no asignados
unassigned_idx <- which(!res$used)

# número de arquetipos sin cluster
length(unassigned_idx)

# frecuencias de esos arquetipos en tu hashmap m
freqs[unassigned_idx]

# si quieres ver los arquetipos en sí
keys_mat[unassigned_idx, ]