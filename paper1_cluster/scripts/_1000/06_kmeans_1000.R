# 06_kmeans_1000.R
#
# OBJETIVO
# Ejecutar K-means sobre los 1000 bootstraps definitivos utilizando
# las matrices de 32 determinantes generadas en 05.
#
# QUÉ HACE
# - Analiza 7 muestras, 4 matrices y K = 2:8.
# - Mantiene NSTART = 25 e ITER_MAX = 100.
# - Mantiene las semillas deterministas de cada ejecución.
# - Ejecuta de forma secuencial para limitar el uso de RAM.
# - Procesa los bootstraps en bloques y escribe resultados en disco.
# - Permite continuar desde las muestras ya completadas.
# - Guarda los resultados en 06_kmeans_bootstrap/_1000.


suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(stringr)
  library(ggplot2)
})


set.seed(123)


# Configuración

project_root <- path.expand(
  "~/Desktop/MASTER/recommendation-engine/TFM"
)

processed_root <- file.path(
  project_root,
  "paper1_cluster/data/processed"
)

bootstrap_file <- file.path(
  processed_root,
  "04_2f_combine_eu_latam_bootstrap",
  "bootstrap_samples_index.csv.gz"
)

matrix_dir <- file.path(
  processed_root,
  "05_clustering_matrices"
)

out_dir <- file.path(
  processed_root,
  "06_kmeans_bootstrap",
  "_1000"
)

fig_dir <- file.path(
  out_dir,
  "figures"
)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  fig_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


ANALYSIS_SAMPLES <- c(
  "COMPLETE",
  "EUROPE",
  "LATAM",
  "DIEGO",
  "RENOVISOR",
  "WHY_EUROPE",
  "WHY_LATAM"
)


MATRICES_TO_RUN <- c(
  "matrix_32_raw_0_1",
  "matrix_32_pos_0_1",
  "matrix_32_ext_0_1",
  "matrix_32_z_abs"
)


K_GRID <- 2:8

NSTART <- 25
ITER_MAX <- 100

EXPECTED_N_BOOT <- 1000L
MAX_BOOTSTRAPS <- 1000L

CHUNK_SIZE <- 50L

RESUME_COMPLETED_SAMPLES <- TRUE

DONE_FILE_NAME <- "_DONE_1000_MEMORY_SAFE.txt"


SAVE_ASSIGNMENTS <- FALSE
SAVE_COMPOSITION <- FALSE
SAVE_HEATMAPS <- TRUE


SILHOUETTE_MODE <- "sampled"
SILHOUETTE_MAX_ROWS <- 500L


if (
  !SILHOUETTE_MODE %in%
  c(
    "full",
    "sampled",
    "off"
  )
) {
  stop(
    "SILHOUETTE_MODE debe ser 'full', 'sampled' u 'off'."
  )
}


COMPOSITION_VARS <- c(
  "comparison_region",
  "subsample",
  "dataset_source",
  "source_survey",
  "bootstrap_source",
  "target_sample_type",
  "target_electoral_group",
  "country_code",
  "country",
  "income_band"
)


# Funciones auxiliares

clean_category <- function(x) {
  
  x <- str_squish(
    as.character(x)
  )
  
  x[
    is.na(x) |
      x == ""
  ] <- "Missing"
  
  x
}


get_bootstrap_sample <- function(
    bootstrap_index,
    analysis_sample
) {
  
  if (
    analysis_sample == "COMPLETE"
  ) {
    
    out <- bootstrap_index
    
  } else if (
    analysis_sample == "EUROPE"
  ) {
    
    out <- bootstrap_index %>%
      filter(
        comparison_region == "EUROPE"
      )
    
  } else if (
    analysis_sample == "LATAM"
  ) {
    
    out <- bootstrap_index %>%
      filter(
        comparison_region == "LATAM"
      )
    
  } else {
    
    out <- bootstrap_index %>%
      filter(
        subsample == analysis_sample
      )
  }
  
  
  if (
    nrow(out) == 0
  ) {
    stop(
      "La muestra ",
      analysis_sample,
      " tiene 0 draws en el bootstrap."
    )
  }
  
  
  out
}


read_matrix_file <- function(
    analysis_sample,
    matrix_name
) {
  
  file <- file.path(
    matrix_dir,
    analysis_sample,
    paste0(
      matrix_name,
      ".csv"
    )
  )
  
  
  if (
    !file.exists(file)
  ) {
    stop(
      "No encuentro la matriz:\n",
      file
    )
  }
  
  
  mat_df <- read_csv(
    file,
    show_col_types = FALSE
  ) %>%
    mutate(
      integrated_row_id =
        as.character(
          integrated_row_id
        )
    )
  
  
  if (
    anyDuplicated(
      mat_df$integrated_row_id
    )
  ) {
    stop(
      "La matriz ",
      analysis_sample,
      " / ",
      matrix_name,
      " contiene integrated_row_id duplicados."
    )
  }
  
  
  feature_cols <- names(
    mat_df
  )[
    str_detect(
      names(mat_df),
      "^det_\\d{2}_"
    )
  ]
  
  
  if (
    length(feature_cols) != 32
  ) {
    stop(
      "La matriz ",
      analysis_sample,
      " / ",
      matrix_name,
      " debería tener 32 determinantes y tiene ",
      length(feature_cols),
      "."
    )
  }
  
  
  mat_df <- mat_df %>%
    select(
      integrated_row_id,
      all_of(feature_cols)
    ) %>%
    mutate(
      across(
        all_of(feature_cols),
        as.numeric
      )
    )
  
  
  matrix_values <- mat_df %>%
    select(
      all_of(feature_cols)
    ) %>%
    as.matrix()
  
  
  storage.mode(
    matrix_values
  ) <- "double"
  
  
  row_lookup <- seq_len(
    nrow(mat_df)
  )
  
  names(
    row_lookup
  ) <- mat_df$integrated_row_id
  
  
  list(
    data = mat_df,
    matrix = matrix_values,
    row_lookup = row_lookup,
    feature_cols = feature_cols
  )
}


safe_kmeans <- function(
    x,
    k
) {
  
  tryCatch(
    kmeans(
      x = x,
      centers = k,
      nstart = NSTART,
      iter.max = ITER_MAX
    ),
    error = function(e) e
  )
}


calculate_silhouette <- function(
    x,
    clusters,
    seed
) {
  
  if (
    SILHOUETTE_MODE == "off"
  ) {
    return(
      list(
        value = NA_real_,
        n_used = 0L,
        subsampled = FALSE
      )
    )
  }
  
  
  n <- nrow(x)
  
  
  if (
    n < 2 ||
    n_distinct(clusters) < 2
  ) {
    return(
      list(
        value = NA_real_,
        n_used = n,
        subsampled = FALSE
      )
    )
  }
  
  
  idx <- seq_len(n)
  
  subsampled <- FALSE
  
  
  if (
    SILHOUETTE_MODE == "sampled" &&
    is.finite(SILHOUETTE_MAX_ROWS) &&
    n > SILHOUETTE_MAX_ROWS
  ) {
    
    set.seed(seed)
    
    
    by_cluster <- split(
      seq_len(n),
      clusters
    )
    
    
    mandatory_idx <- unlist(
      map(
        by_cluster,
        ~ sample(
          .x,
          1
        )
      ),
      use.names = FALSE
    )
    
    
    remaining <- setdiff(
      seq_len(n),
      mandatory_idx
    )
    
    
    n_extra <- min(
      length(remaining),
      SILHOUETTE_MAX_ROWS -
        length(mandatory_idx)
    )
    
    
    extra_idx <- if (
      n_extra > 0
    ) {
      
      sample(
        remaining,
        n_extra
      )
      
    } else {
      
      integer()
    }
    
    
    idx <- sort(
      c(
        mandatory_idx,
        extra_idx
      )
    )
    
    
    subsampled <- TRUE
  }
  
  
  silhouette_value <- tryCatch(
    {
      
      sil <- cluster::silhouette(
        clusters[idx],
        dist(
          x[
            idx,
            ,
            drop = FALSE
          ],
          method = "euclidean"
        )
      )
      
      
      mean(
        sil[
          ,
          "sil_width"
        ],
        na.rm = TRUE
      )
      
    },
    error = function(e) NA_real_
  )
  
  
  list(
    value = silhouette_value,
    n_used = length(idx),
    subsampled = subsampled
  )
}


make_distance_long <- function(
    centers_ordered,
    feature_cols,
    analysis_sample,
    matrix_name,
    bootstrap_id,
    k
) {
  
  center_matrix <- centers_ordered %>%
    arrange(
      cluster_rank
    ) %>%
    select(
      all_of(feature_cols)
    ) %>%
    as.matrix()
  
  
  dmat <- as.matrix(
    dist(
      center_matrix,
      method = "euclidean"
    )
  )
  
  
  idx <- which(
    upper.tri(dmat),
    arr.ind = TRUE
  )
  
  
  tibble(
    analysis_sample = analysis_sample,
    matrix_name = matrix_name,
    bootstrap_id = bootstrap_id,
    k = k,
    
    cluster_a =
      as.integer(
        idx[
          ,
          "row"
        ]
      ),
    
    cluster_b =
      as.integer(
        idx[
          ,
          "col"
        ]
      ),
    
    euclidean_distance =
      as.numeric(
        dmat[idx]
      )
  )
}


make_composition_long <- function(
    assignments,
    composition_levels,
    k_current
) {
  
  if (
    length(composition_levels) == 0
  ) {
    return(
      tibble()
    )
  }
  
  
  map_dfr(
    names(composition_levels),
    
    function(var) {
      
      categories <- composition_levels[[var]]
      
      
      current <- assignments %>%
        transmute(
          cluster_rank,
          
          composition_category =
            clean_category(
              .data[[var]]
            )
        ) %>%
        count(
          cluster_rank,
          composition_category,
          name = "n"
        )
      
      
      crossing(
        cluster_rank =
          seq_len(k_current),
        
        composition_category =
          categories
      ) %>%
        left_join(
          current,
          
          by = c(
            "cluster_rank",
            "composition_category"
          )
        ) %>%
        mutate(
          n =
            replace_na(
              n,
              0L
            )
        ) %>%
        group_by(
          cluster_rank
        ) %>%
        mutate(
          prop_within_cluster =
            if (
              sum(n) > 0
            ) {
              
              n / sum(n)
              
            } else {
              
              rep(
                NA_real_,
                n()
              )
            }
        ) %>%
        ungroup() %>%
        mutate(
          analysis_sample =
            unique(
              assignments$analysis_sample
            ),
          
          matrix_name =
            unique(
              assignments$matrix_name
            ),
          
          bootstrap_id =
            unique(
              assignments$bootstrap_id
            ),
          
          k =
            unique(
              assignments$k
            ),
          
          composition_variable =
            var
        ) %>%
        select(
          analysis_sample,
          matrix_name,
          bootstrap_id,
          k,
          cluster_rank,
          composition_variable,
          composition_category,
          n,
          prop_within_cluster
        )
    }
  )
}


failed_result <- function(
    analysis_sample,
    matrix_name,
    bootstrap_id,
    k,
    status,
    n_bootstrap_draws,
    n_rows_used,
    error_message
) {
  
  n_rows_dropped <-
    n_bootstrap_draws -
    n_rows_used
  
  
  prop_rows_used <- if (
    n_bootstrap_draws > 0
  ) {
    
    n_rows_used /
      n_bootstrap_draws
    
  } else {
    
    NA_real_
  }
  
  
  list(
    
    metrics =
      tibble(
        analysis_sample = analysis_sample,
        matrix_name = matrix_name,
        bootstrap_id = bootstrap_id,
        k = k,
        status = status,
        n_bootstrap_draws = n_bootstrap_draws,
        n_rows_used = n_rows_used,
        n_rows_dropped = n_rows_dropped,
        prop_rows_used = prop_rows_used,
        totss = NA_real_,
        tot_withinss = NA_real_,
        betweenss = NA_real_,
        between_over_total = NA_real_,
        calinski_harabasz = NA_real_,
        mean_silhouette = NA_real_,
        silhouette_n_used = NA_integer_,
        silhouette_subsampled = NA,
        mean_cluster_distance = NA_real_,
        min_cluster_distance = NA_real_,
        max_cluster_distance = NA_real_,
        iter = NA_integer_,
        ifault = NA_integer_,
        error_message = error_message
      ),
    
    centers_long =
      tibble(),
    
    cluster_sizes =
      tibble(),
    
    assignments =
      tibble(),
    
    distances =
      tibble(),
    
    composition =
      tibble()
  )
}


run_one_kmeans <- function(
    analysis_sample_current,
    analysis_sample_index,
    bootstrap_id_current,
    boot_b,
    matrix_name_current,
    matrix_index_current,
    matrix_obj,
    k_current,
    composition_levels
) {
  
  row_idx <- unname(
    matrix_obj$row_lookup[
      boot_b$integrated_row_id
    ]
  )
  
  
  if (
    any(
      is.na(row_idx)
    )
  ) {
    stop(
      "Hay IDs del bootstrap ",
      analysis_sample_current,
      " que no existen en la matriz ",
      matrix_name_current,
      "."
    )
  }
  
  
  x <- matrix_obj$matrix[
    row_idx,
    ,
    drop = FALSE
  ]
  
  
  complete_rows <- complete.cases(
    x
  )
  
  
  sample_use <- boot_b[
    complete_rows,
    ,
    drop = FALSE
  ]
  
  
  x <- x[
    complete_rows,
    ,
    drop = FALSE
  ]
  
  
  n_bootstrap_draws <- nrow(
    boot_b
  )
  
  n_rows_used <- nrow(
    x
  )
  
  
  if (
    n_rows_used < k_current
  ) {
    
    return(
      failed_result(
        analysis_sample =
          analysis_sample_current,
        
        matrix_name =
          matrix_name_current,
        
        bootstrap_id =
          bootstrap_id_current,
        
        k =
          k_current,
        
        status =
          "skipped_too_few_rows",
        
        n_bootstrap_draws =
          n_bootstrap_draws,
        
        n_rows_used =
          n_rows_used,
        
        error_message =
          "Too few rows for K"
      )
    )
  }
  
  
  run_seed <-
    analysis_sample_index *
    10000000 +
    matrix_index_current *
    1000000 +
    k_current *
    10000 +
    bootstrap_id_current
  
  
  set.seed(
    run_seed
  )
  
  
  km <- safe_kmeans(
    x = x,
    k = k_current
  )
  
  
  if (
    inherits(
      km,
      "error"
    )
  ) {
    
    return(
      failed_result(
        analysis_sample =
          analysis_sample_current,
        
        matrix_name =
          matrix_name_current,
        
        bootstrap_id =
          bootstrap_id_current,
        
        k =
          k_current,
        
        status =
          "error",
        
        n_bootstrap_draws =
          n_bootstrap_draws,
        
        n_rows_used =
          n_rows_used,
        
        error_message =
          km$message
      )
    )
  }
  
  
  centers_df <- as_tibble(
    km$centers
  ) %>%
    mutate(
      cluster_original =
        row_number(),
      
      .before = 1
    )
  
  
  center_order <- centers_df %>%
    mutate(
      center_mean =
        rowMeans(
          as.matrix(
            select(
              .,
              all_of(
                matrix_obj$feature_cols
              )
            )
          ),
          na.rm = TRUE
        )
    ) %>%
    arrange(
      desc(center_mean),
      cluster_original
    ) %>%
    mutate(
      cluster_rank =
        row_number()
    ) %>%
    select(
      cluster_original,
      cluster_rank,
      center_mean
    )
  
  
  rank_lookup <-
    center_order$cluster_rank
  
  
  names(
    rank_lookup
  ) <- as.character(
    center_order$cluster_original
  )
  
  
  assignments <- sample_use %>%
    mutate(
      analysis_sample =
        analysis_sample_current,
      
      matrix_name =
        matrix_name_current,
      
      bootstrap_id =
        bootstrap_id_current,
      
      k =
        k_current,
      
      cluster_original =
        as.integer(
          km$cluster
        ),
      
      cluster_rank =
        as.integer(
          rank_lookup[
            as.character(
              cluster_original
            )
          ]
        )
    )
  
  
  cluster_sizes <- assignments %>%
    count(
      cluster_rank,
      name = "n_cluster"
    ) %>%
    complete(
      cluster_rank =
        seq_len(k_current),
      
      fill =
        list(
          n_cluster = 0L
        )
    ) %>%
    mutate(
      analysis_sample =
        analysis_sample_current,
      
      matrix_name =
        matrix_name_current,
      
      bootstrap_id =
        bootstrap_id_current,
      
      k =
        k_current,
      
      prop_cluster =
        n_cluster /
        sum(n_cluster)
    ) %>%
    select(
      analysis_sample,
      matrix_name,
      bootstrap_id,
      k,
      cluster_rank,
      n_cluster,
      prop_cluster
    )
  
  
  centers_ordered <- centers_df %>%
    left_join(
      center_order,
      by = "cluster_original"
    ) %>%
    mutate(
      analysis_sample =
        analysis_sample_current,
      
      matrix_name =
        matrix_name_current,
      
      bootstrap_id =
        bootstrap_id_current,
      
      k =
        k_current
    ) %>%
    select(
      analysis_sample,
      matrix_name,
      bootstrap_id,
      k,
      cluster_rank,
      cluster_original,
      center_mean,
      all_of(
        matrix_obj$feature_cols
      )
    )
  
  
  centers_long <- centers_ordered %>%
    pivot_longer(
      cols =
        all_of(
          matrix_obj$feature_cols
        ),
      
      names_to =
        "determinant",
      
      values_to =
        "center_value"
    )
  
  
  distances <- make_distance_long(
    centers_ordered =
      centers_ordered,
    
    feature_cols =
      matrix_obj$feature_cols,
    
    analysis_sample =
      analysis_sample_current,
    
    matrix_name =
      matrix_name_current,
    
    bootstrap_id =
      bootstrap_id_current,
    
    k =
      k_current
  )
  
  
  silhouette_result <- calculate_silhouette(
    x = x,
    
    clusters =
      km$cluster,
    
    seed =
      run_seed +
      500000000
  )
  
  
  mean_cluster_distance <- if (
    nrow(distances) > 0
  ) {
    
    mean(
      distances$euclidean_distance,
      na.rm = TRUE
    )
    
  } else {
    
    NA_real_
  }
  
  
  min_cluster_distance <- if (
    nrow(distances) > 0
  ) {
    
    min(
      distances$euclidean_distance,
      na.rm = TRUE
    )
    
  } else {
    
    NA_real_
  }
  
  
  max_cluster_distance <- if (
    nrow(distances) > 0
  ) {
    
    max(
      distances$euclidean_distance,
      na.rm = TRUE
    )
    
  } else {
    
    NA_real_
  }
  
  
  calinski_harabasz <- if (
    n_rows_used > k_current &&
    is.finite(
      km$tot.withinss
    ) &&
    km$tot.withinss > 0
  ) {
    
    (
      km$betweenss /
        (
          k_current -
            1
        )
    ) /
      (
        km$tot.withinss /
          (
            n_rows_used -
              k_current
          )
      )
    
  } else {
    
    NA_real_
  }
  
  
  metrics <- tibble(
    analysis_sample =
      analysis_sample_current,
    
    matrix_name =
      matrix_name_current,
    
    bootstrap_id =
      bootstrap_id_current,
    
    k =
      k_current,
    
    status =
      "ok",
    
    n_bootstrap_draws =
      n_bootstrap_draws,
    
    n_rows_used =
      n_rows_used,
    
    n_rows_dropped =
      n_bootstrap_draws -
      n_rows_used,
    
    prop_rows_used =
      n_rows_used /
      n_bootstrap_draws,
    
    totss =
      km$totss,
    
    tot_withinss =
      km$tot.withinss,
    
    betweenss =
      km$betweenss,
    
    between_over_total =
      if_else(
        km$totss > 0,
        km$betweenss /
          km$totss,
        NA_real_
      ),
    
    calinski_harabasz =
      calinski_harabasz,
    
    mean_silhouette =
      silhouette_result$value,
    
    silhouette_n_used =
      as.integer(
        silhouette_result$n_used
      ),
    
    silhouette_subsampled =
      silhouette_result$subsampled,
    
    mean_cluster_distance =
      mean_cluster_distance,
    
    min_cluster_distance =
      min_cluster_distance,
    
    max_cluster_distance =
      max_cluster_distance,
    
    iter =
      km$iter,
    
    ifault =
      if (
        is.null(
          km$ifault
        )
      ) {
        NA_integer_
      } else {
        as.integer(
          km$ifault
        )
      },
    
    error_message =
      NA_character_
  )
  
  
  composition <- if (
    SAVE_COMPOSITION
  ) {
    
    make_composition_long(
      assignments =
        assignments,
      
      composition_levels =
        composition_levels,
      
      k_current =
        k_current
    )
    
  } else {
    
    tibble()
  }
  
  
  assignments_output <- if (
    SAVE_ASSIGNMENTS
  ) {
    
    assignments %>%
      select(
        analysis_sample,
        matrix_name,
        bootstrap_id,
        k,
        draw_id,
        integrated_row_id,
        any_of(
          COMPOSITION_VARS
        ),
        cluster_rank,
        cluster_original
      )
    
  } else {
    
    tibble()
  }
  
  
  list(
    metrics =
      metrics,
    
    centers_long =
      centers_long,
    
    cluster_sizes =
      cluster_sizes,
    
    assignments =
      assignments_output,
    
    distances =
      distances,
    
    composition =
      composition
  )
}


append_csv_safe <- function(
    x,
    path
) {
  
  if (
    nrow(x) == 0
  ) {
    return(
      invisible(NULL)
    )
  }
  
  
  write_csv(
    x,
    path,
    append =
      file.exists(path)
  )
  
  
  invisible(NULL)
}


summarise_center_chunk <- function(
    centers_chunk
) {
  
  if (
    nrow(centers_chunk) == 0
  ) {
    return(
      tibble()
    )
  }
  
  
  centers_chunk %>%
    group_by(
      analysis_sample,
      matrix_name,
      k,
      cluster_rank,
      determinant
    ) %>%
    summarise(
      n_center =
        sum(
          !is.na(
            center_value
          )
        ),
      
      sum_center =
        sum(
          center_value,
          na.rm = TRUE
        ),
      
      sumsq_center =
        sum(
          center_value^2,
          na.rm = TRUE
        ),
      
      .groups =
        "drop"
    )
}


combine_center_stats <- function(
    center_stats_parts
) {
  
  center_stats <- bind_rows(
    center_stats_parts
  )
  
  
  if (
    nrow(center_stats) == 0
  ) {
    return(
      tibble()
    )
  }
  
  
  center_stats %>%
    group_by(
      analysis_sample,
      matrix_name,
      k,
      cluster_rank,
      determinant
    ) %>%
    summarise(
      n_center =
        sum(
          n_center
        ),
      
      sum_center =
        sum(
          sum_center
        ),
      
      sumsq_center =
        sum(
          sumsq_center
        ),
      
      .groups =
        "drop"
    ) %>%
    mutate(
      mean_center_value =
        if_else(
          n_center > 0,
          sum_center /
            n_center,
          NA_real_
        ),
      
      center_variance =
        if_else(
          n_center > 1,
          
          (
            sumsq_center -
              (
                sum_center^2 /
                  n_center
              )
          ) /
            (
              n_center -
                1
            ),
          
          NA_real_
        ),
      
      center_variance =
        if_else(
          !is.na(center_variance) &
            center_variance < 0 &
            abs(center_variance) < 1e-12,
          0,
          center_variance
        ),
      
      sd_center_value =
        if_else(
          !is.na(center_variance) &
            center_variance >= 0,
          sqrt(center_variance),
          NA_real_
        )
    ) %>%
    select(
      analysis_sample,
      matrix_name,
      k,
      cluster_rank,
      determinant,
      mean_center_value,
      sd_center_value
    ) %>%
    arrange(
      matrix_name,
      k,
      cluster_rank,
      determinant
    )
}


load_completed_sample <- function(
    sample_out_dir
) {
  
  composition_summary_file <- file.path(
    sample_out_dir,
    "kmeans_cluster_composition_summary.csv"
  )
  
  
  list(
    metrics_summary =
      read_csv(
        file.path(
          sample_out_dir,
          "kmeans_metrics_summary.csv"
        ),
        show_col_types = FALSE
      ),
    
    cluster_size_summary =
      read_csv(
        file.path(
          sample_out_dir,
          "kmeans_cluster_size_summary.csv"
        ),
        show_col_types = FALSE
      ),
    
    centers_mean =
      read_csv(
        file.path(
          sample_out_dir,
          "kmeans_centers_mean_across_bootstraps.csv"
        ),
        show_col_types = FALSE
      ),
    
    distance_summary =
      read_csv(
        file.path(
          sample_out_dir,
          "kmeans_cluster_distance_summary.csv"
        ),
        show_col_types = FALSE
      ),
    
    composition_summary =
      if (
        file.exists(
          composition_summary_file
        )
      ) {
        read_csv(
          composition_summary_file,
          show_col_types = FALSE
        )
      } else {
        tibble()
      },
    
    run_status_summary =
      read_csv(
        file.path(
          sample_out_dir,
          "kmeans_run_status_summary.csv"
        ),
        show_col_types = FALSE
      )
  )
}


# Leer bootstrap final

if (
  !file.exists(
    bootstrap_file
  )
) {
  stop(
    "No encuentro el bootstrap final:\n",
    bootstrap_file
  )
}


bootstrap_colnames <- names(
  read_csv(
    bootstrap_file,
    n_max = 0,
    show_col_types = FALSE
  )
)


required_boot_cols <- c(
  "bootstrap_id",
  "draw_id",
  "integrated_row_id",
  "comparison_region",
  "subsample",
  "dataset_source"
)


missing_boot_cols <- setdiff(
  required_boot_cols,
  bootstrap_colnames
)


if (
  length(
    missing_boot_cols
  ) > 0
) {
  stop(
    "Faltan columnas necesarias en 04_2f: ",
    paste(
      missing_boot_cols,
      collapse = ", "
    )
  )
}


bootstrap_cols_to_read <- unique(
  c(
    required_boot_cols,
    "region_draw_id",
    COMPOSITION_VARS
  )
)


bootstrap_cols_to_read <-
  bootstrap_cols_to_read[
    bootstrap_cols_to_read %in%
      bootstrap_colnames
  ]


bootstrap_index <- read_csv(
  bootstrap_file,
  
  col_select =
    all_of(
      bootstrap_cols_to_read
    ),
  
  show_col_types = FALSE
) %>%
  mutate(
    bootstrap_id =
      as.integer(
        bootstrap_id
      ),
    
    draw_id =
      as.integer(
        draw_id
      ),
    
    integrated_row_id =
      as.character(
        integrated_row_id
      ),
    
    comparison_region =
      as.character(
        comparison_region
      ),
    
    subsample =
      as.character(
        subsample
      ),
    
    dataset_source =
      as.character(
        dataset_source
      )
  )


all_boot_ids <- sort(
  unique(
    bootstrap_index$bootstrap_id
  )
)


if (
  length(
    all_boot_ids
  ) !=
  EXPECTED_N_BOOT
) {
  stop(
    "Se esperaban ",
    EXPECTED_N_BOOT,
    " bootstraps y se han encontrado ",
    length(
      all_boot_ids
    ),
    "."
  )
}


if (
  !identical(
    all_boot_ids,
    seq_len(
      EXPECTED_N_BOOT
    )
  )
) {
  stop(
    "Los bootstrap_id no son exactamente 1:",
    EXPECTED_N_BOOT,
    "."
  )
}


boot_ids <- all_boot_ids


if (
  is.finite(
    MAX_BOOTSTRAPS
  )
) {
  
  boot_ids <- head(
    boot_ids,
    as.integer(
      MAX_BOOTSTRAPS
    )
  )
  
  
  bootstrap_index <- bootstrap_index %>%
    filter(
      bootstrap_id %in%
        boot_ids
    )
}


# Comprobar estructura del bootstrap

bootstrap_sizes <- map_dfr(
  ANALYSIS_SAMPLES,
  
  function(sample_name) {
    
    get_bootstrap_sample(
      bootstrap_index =
        bootstrap_index,
      
      analysis_sample =
        sample_name
    ) %>%
      count(
        bootstrap_id,
        name = "n_draws"
      ) %>%
      mutate(
        analysis_sample =
          sample_name,
        
        .before = 1
      )
  }
)


missing_bootstrap_samples <- crossing(
  analysis_sample =
    ANALYSIS_SAMPLES,
  
  bootstrap_id =
    boot_ids
) %>%
  anti_join(
    bootstrap_sizes %>%
      select(
        analysis_sample,
        bootstrap_id
      ),
    
    by = c(
      "analysis_sample",
      "bootstrap_id"
    )
  )


if (
  nrow(
    missing_bootstrap_samples
  ) > 0
) {
  stop(
    "Alguna muestra de análisis no aparece en todos los bootstraps."
  )
}


fixed_expected_sizes <- c(
  COMPLETE = 1608L,
  EUROPE = 1000L,
  LATAM = 608L,
  WHY_LATAM = 608L
)


for (
  sample_name in
  names(
    fixed_expected_sizes
  )
) {
  
  current_sizes <- bootstrap_sizes %>%
    filter(
      analysis_sample ==
        sample_name
    ) %>%
    pull(
      n_draws
    )
  
  
  if (
    any(
      current_sizes !=
      fixed_expected_sizes[[sample_name]]
    )
  ) {
    stop(
      "Tamaño inesperado en ",
      sample_name,
      ". Esperaba ",
      fixed_expected_sizes[[sample_name]],
      " draws por bootstrap."
    )
  }
}


bootstrap_size_summary <- bootstrap_sizes %>%
  group_by(
    analysis_sample
  ) %>%
  summarise(
    mean_n =
      mean(
        n_draws
      ),
    
    sd_n =
      sd(
        n_draws
      ),
    
    min_n =
      min(
        n_draws
      ),
    
    max_n =
      max(
        n_draws
      ),
    
    n_bootstraps =
      n_distinct(
        bootstrap_id
      ),
    
    .groups =
      "drop"
  )


write_csv(
  bootstrap_sizes,
  
  file.path(
    out_dir,
    "bootstrap_sample_sizes_by_run.csv.gz"
  )
)


write_csv(
  bootstrap_size_summary,
  
  file.path(
    out_dir,
    "bootstrap_sample_sizes_summary.csv"
  )
)


# Ejecutar una muestra

run_analysis_sample <- function(
    analysis_sample_current,
    analysis_sample_index
) {
  
  sample_out_dir <- file.path(
    out_dir,
    analysis_sample_current
  )
  
  
  sample_fig_dir <- file.path(
    fig_dir,
    analysis_sample_current
  )
  
  
  done_file <- file.path(
    sample_out_dir,
    DONE_FILE_NAME
  )
  
  
  if (
    RESUME_COMPLETED_SAMPLES &&
    file.exists(
      done_file
    )
  ) {
    
    cat(
      "\nMUESTRA YA COMPLETADA: ",
      analysis_sample_current,
      "\nSe reutilizan los resultados existentes.\n",
      sep = ""
    )
    
    
    return(
      load_completed_sample(
        sample_out_dir
      )
    )
  }
  
  
  if (
    dir.exists(
      sample_out_dir
    )
  ) {
    unlink(
      sample_out_dir,
      recursive = TRUE,
      force = TRUE
    )
  }
  
  
  dir.create(
    sample_out_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  dir.create(
    sample_fig_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  cat(
    "\n============================================================\n",
    "MUESTRA: ",
    analysis_sample_current,
    "\n============================================================\n",
    sep = ""
  )
  
  
  bootstrap_sample <- get_bootstrap_sample(
    bootstrap_index =
      bootstrap_index,
    
    analysis_sample =
      analysis_sample_current
  )
  
  
  # Guardamos solo los índices de fila de cada bootstrap.
  # Esto evita duplicar todo el data frame 1000 veces en memoria.
  
  boot_row_lookup <- split(
    seq_len(
      nrow(
        bootstrap_sample
      )
    ),
    bootstrap_sample$bootstrap_id
  )
  
  
  composition_vars_available <- intersect(
    COMPOSITION_VARS,
    names(
      bootstrap_sample
    )
  )
  
  
  composition_levels <- setNames(
    map(
      composition_vars_available,
      
      ~ sort(
        unique(
          clean_category(
            bootstrap_sample[[.x]]
          )
        )
      )
    ),
    
    composition_vars_available
  )
  
  
  metrics_file <- file.path(
    sample_out_dir,
    "kmeans_metrics_by_run.csv"
  )
  
  sizes_file <- file.path(
    sample_out_dir,
    "kmeans_cluster_sizes_by_run.csv.gz"
  )
  
  centers_file <- file.path(
    sample_out_dir,
    "kmeans_centers_long.csv.gz"
  )
  
  distances_file <- file.path(
    sample_out_dir,
    "kmeans_cluster_distances_by_run.csv.gz"
  )
  
  composition_file <- file.path(
    sample_out_dir,
    "kmeans_cluster_composition_by_run.csv.gz"
  )
  
  assignments_file <- file.path(
    sample_out_dir,
    "kmeans_assignments_index.csv.gz"
  )
  
  
  files_to_reset <- c(
    metrics_file,
    sizes_file,
    centers_file,
    distances_file,
    composition_file,
    assignments_file
  )
  
  
  existing_files <- files_to_reset[
    file.exists(
      files_to_reset
    )
  ]
  
  
  if (
    length(
      existing_files
    ) > 0
  ) {
    file.remove(
      existing_files
    )
  }
  
  
  metrics_summary_parts <- list()
  size_summary_parts <- list()
  centers_mean_parts <- list()
  distance_summary_parts <- list()
  composition_summary_parts <- list()
  run_status_parts <- list()
  
  summary_counter <- 0L
  
  
  bootstrap_chunks <- split(
    boot_ids,
    ceiling(
      seq_along(
        boot_ids
      ) /
        CHUNK_SIZE
    )
  )
  
  
  for (
    matrix_index in
    seq_along(
      MATRICES_TO_RUN
    )
  ) {
    
    matrix_name <- MATRICES_TO_RUN[
      matrix_index
    ]
    
    
    cat(
      "\nMatriz: ",
      matrix_name,
      "\n",
      sep = ""
    )
    
    
    matrix_obj <- read_matrix_file(
      analysis_sample =
        analysis_sample_current,
      
      matrix_name =
        matrix_name
    )
    
    
    missing_matrix_ids <- bootstrap_sample %>%
      distinct(
        integrated_row_id
      ) %>%
      filter(
        !integrated_row_id %in%
          names(
            matrix_obj$row_lookup
          )
      )
    
    
    if (
      nrow(
        missing_matrix_ids
      ) > 0
    ) {
      stop(
        "Faltan ",
        nrow(
          missing_matrix_ids
        ),
        " IDs de ",
        analysis_sample_current,
        " en ",
        matrix_name,
        "."
      )
    }
    
    
    for (
      k_current in
      K_GRID
    ) {
      
      cat(
        "\n  K = ",
        k_current,
        " | ",
        length(boot_ids),
        " bootstraps | secuencial | bloques de ",
        CHUNK_SIZE,
        "\n",
        sep = ""
      )
      
      
      metrics_k_parts <- list()
      sizes_k_parts <- list()
      distances_k_parts <- list()
      center_stats_parts <- list()
      
      composition_k_parts <- if (
        SAVE_COMPOSITION
      ) {
        list()
      } else {
        NULL
      }
      
      
      for (
        chunk_index in
        seq_along(
          bootstrap_chunks
        )
      ) {
        
        chunk_ids <- bootstrap_chunks[[chunk_index]]
        
        
        results_chunk <- vector(
          "list",
          length(
            chunk_ids
          )
        )
        
        
        for (
          i in
          seq_along(
            chunk_ids
          )
        ) {
          
          bootstrap_id_current <- chunk_ids[i]
          
          
          boot_rows <- boot_row_lookup[[as.character(bootstrap_id_current)]]
          
          
          if (
            is.null(
              boot_rows
            )
          ) {
            stop(
              "No encuentro bootstrap_id ",
              bootstrap_id_current,
              " en ",
              analysis_sample_current,
              "."
            )
          }
          
          
          boot_b <- bootstrap_sample[
            boot_rows,
            ,
            drop = FALSE
          ]
          
          
          results_chunk[[i]] <- run_one_kmeans(
            analysis_sample_current =
              analysis_sample_current,
            
            analysis_sample_index =
              analysis_sample_index,
            
            bootstrap_id_current =
              bootstrap_id_current,
            
            boot_b =
              boot_b,
            
            matrix_name_current =
              matrix_name,
            
            matrix_index_current =
              matrix_index,
            
            matrix_obj =
              matrix_obj,
            
            k_current =
              k_current,
            
            composition_levels =
              composition_levels
          )
        }
        
        
        metrics_chunk <- map_dfr(
          results_chunk,
          "metrics"
        )
        
        centers_chunk <- map_dfr(
          results_chunk,
          "centers_long"
        )
        
        sizes_chunk <- map_dfr(
          results_chunk,
          "cluster_sizes"
        )
        
        distances_chunk <- map_dfr(
          results_chunk,
          "distances"
        )
        
        
        composition_chunk <- if (
          SAVE_COMPOSITION
        ) {
          map_dfr(
            results_chunk,
            "composition"
          )
        } else {
          tibble()
        }
        
        
        assignments_chunk <- if (
          SAVE_ASSIGNMENTS
        ) {
          map_dfr(
            results_chunk,
            "assignments"
          )
        } else {
          tibble()
        }
        
        
        append_csv_safe(
          metrics_chunk,
          metrics_file
        )
        
        append_csv_safe(
          sizes_chunk,
          sizes_file
        )
        
        append_csv_safe(
          centers_chunk,
          centers_file
        )
        
        append_csv_safe(
          distances_chunk,
          distances_file
        )
        
        
        if (
          SAVE_COMPOSITION
        ) {
          append_csv_safe(
            composition_chunk,
            composition_file
          )
        }
        
        
        if (
          SAVE_ASSIGNMENTS
        ) {
          append_csv_safe(
            assignments_chunk,
            assignments_file
          )
        }
        
        
        metrics_k_parts[[chunk_index]] <- metrics_chunk
        
        
        sizes_k_parts[[chunk_index]] <- sizes_chunk
        
        
        distances_k_parts[[chunk_index]] <- distances_chunk
        
        
        center_stats_parts[[chunk_index]] <- summarise_center_chunk(
          centers_chunk
        )
        
        
        if (
          SAVE_COMPOSITION
        ) {
          composition_k_parts[[chunk_index]] <- composition_chunk
        }
        
        
        cat(
          "    bloque ",
          chunk_index,
          "/",
          length(
            bootstrap_chunks
          ),
          " | bootstrap ",
          min(
            chunk_ids
          ),
          "-",
          max(
            chunk_ids
          ),
          "\n",
          sep = ""
        )
        
        
        rm(
          results_chunk,
          metrics_chunk,
          centers_chunk,
          sizes_chunk,
          distances_chunk,
          composition_chunk,
          assignments_chunk
        )
        
        
        invisible(
          gc()
        )
      }
      
      
      metrics_k <- bind_rows(
        metrics_k_parts
      )
      
      
      sizes_k <- bind_rows(
        sizes_k_parts
      )
      
      
      distances_k <- bind_rows(
        distances_k_parts
      )
      
      
      centers_mean_k <- combine_center_stats(
        center_stats_parts
      )
      
      
      summary_counter <-
        summary_counter +
        1L
      
      
      metrics_summary_parts[[summary_counter]] <- metrics_k %>%
        group_by(
          analysis_sample,
          matrix_name,
          k
        ) %>%
        summarise(
          n_runs =
            n(),
          
          n_ok =
            sum(
              status == "ok"
            ),
          
          n_error =
            sum(
              status != "ok"
            ),
          
          mean_n_rows_used =
            mean(
              n_rows_used,
              na.rm = TRUE
            ),
          
          mean_n_rows_dropped =
            mean(
              n_rows_dropped,
              na.rm = TRUE
            ),
          
          mean_prop_rows_used =
            mean(
              prop_rows_used,
              na.rm = TRUE
            ),
          
          mean_totss =
            mean(
              totss,
              na.rm = TRUE
            ),
          
          mean_tot_withinss =
            mean(
              tot_withinss,
              na.rm = TRUE
            ),
          
          sd_tot_withinss =
            sd(
              tot_withinss,
              na.rm = TRUE
            ),
          
          mean_betweenss =
            mean(
              betweenss,
              na.rm = TRUE
            ),
          
          mean_between_over_total =
            mean(
              between_over_total,
              na.rm = TRUE
            ),
          
          sd_between_over_total =
            sd(
              between_over_total,
              na.rm = TRUE
            ),
          
          mean_calinski_harabasz =
            mean(
              calinski_harabasz,
              na.rm = TRUE
            ),
          
          sd_calinski_harabasz =
            sd(
              calinski_harabasz,
              na.rm = TRUE
            ),
          
          mean_silhouette =
            mean(
              mean_silhouette,
              na.rm = TRUE
            ),
          
          sd_silhouette =
            sd(
              mean_silhouette,
              na.rm = TRUE
            ),
          
          mean_cluster_distance =
            mean(
              mean_cluster_distance,
              na.rm = TRUE
            ),
          
          sd_cluster_distance =
            sd(
              mean_cluster_distance,
              na.rm = TRUE
            ),
          
          mean_min_cluster_distance =
            mean(
              min_cluster_distance,
              na.rm = TRUE
            ),
          
          mean_max_cluster_distance =
            mean(
              max_cluster_distance,
              na.rm = TRUE
            ),
          
          .groups =
            "drop"
        )
      
      
      size_summary_parts[[summary_counter]] <- sizes_k %>%
        group_by(
          analysis_sample,
          matrix_name,
          k,
          cluster_rank
        ) %>%
        summarise(
          mean_n_cluster =
            mean(
              n_cluster,
              na.rm = TRUE
            ),
          
          sd_n_cluster =
            sd(
              n_cluster,
              na.rm = TRUE
            ),
          
          min_n_cluster =
            min(
              n_cluster,
              na.rm = TRUE
            ),
          
          max_n_cluster =
            max(
              n_cluster,
              na.rm = TRUE
            ),
          
          mean_prop_cluster =
            mean(
              prop_cluster,
              na.rm = TRUE
            ),
          
          sd_prop_cluster =
            sd(
              prop_cluster,
              na.rm = TRUE
            ),
          
          .groups =
            "drop"
        )
      
      
      centers_mean_parts[[summary_counter]] <- centers_mean_k
      
      
      distance_summary_parts[[summary_counter]] <- distances_k %>%
        group_by(
          analysis_sample,
          matrix_name,
          k
        ) %>%
        summarise(
          n_distances =
            n(),
          
          mean_distance =
            mean(
              euclidean_distance,
              na.rm = TRUE
            ),
          
          sd_distance =
            sd(
              euclidean_distance,
              na.rm = TRUE
            ),
          
          min_distance =
            min(
              euclidean_distance,
              na.rm = TRUE
            ),
          
          q25_distance =
            as.numeric(
              quantile(
                euclidean_distance,
                0.25,
                na.rm = TRUE
              )
            ),
          
          median_distance =
            median(
              euclidean_distance,
              na.rm = TRUE
            ),
          
          q75_distance =
            as.numeric(
              quantile(
                euclidean_distance,
                0.75,
                na.rm = TRUE
              )
            ),
          
          max_distance =
            max(
              euclidean_distance,
              na.rm = TRUE
            ),
          
          .groups =
            "drop"
        )
      
      
      run_status_parts[[summary_counter]] <- metrics_k %>%
        count(
          analysis_sample,
          matrix_name,
          k,
          status,
          name = "n_runs"
        )
      
      
      if (
        SAVE_COMPOSITION
      ) {
        
        composition_k <- bind_rows(
          composition_k_parts
        )
        
        
        composition_summary_parts[[summary_counter]] <- composition_k %>%
          group_by(
            analysis_sample,
            matrix_name,
            k,
            cluster_rank,
            composition_variable,
            composition_category
          ) %>%
          summarise(
            mean_n =
              mean(
                n,
                na.rm = TRUE
              ),
            
            sd_n =
              sd(
                n,
                na.rm = TRUE
              ),
            
            mean_prop_within_cluster =
              mean(
                prop_within_cluster,
                na.rm = TRUE
              ),
            
            sd_prop_within_cluster =
              sd(
                prop_within_cluster,
                na.rm = TRUE
              ),
            
            .groups =
              "drop"
          )
        
        
        rm(
          composition_k
        )
      }
      
      
      rm(
        metrics_k_parts,
        sizes_k_parts,
        distances_k_parts,
        center_stats_parts,
        composition_k_parts,
        metrics_k,
        sizes_k,
        distances_k,
        centers_mean_k
      )
      
      
      invisible(
        gc()
      )
    }
    
    
    rm(
      matrix_obj
    )
    
    
    invisible(
      gc()
    )
  }
  
  
  # Consolidar únicamente los resúmenes
  
  kmeans_metrics_summary <- bind_rows(
    metrics_summary_parts
  ) %>%
    arrange(
      matrix_name,
      k
    )
  
  
  cluster_size_summary <- bind_rows(
    size_summary_parts
  ) %>%
    arrange(
      matrix_name,
      k,
      cluster_rank
    )
  
  
  kmeans_centers_mean <- bind_rows(
    centers_mean_parts
  ) %>%
    arrange(
      matrix_name,
      k,
      cluster_rank,
      determinant
    )
  
  
  cluster_distance_summary <- bind_rows(
    distance_summary_parts
  ) %>%
    arrange(
      matrix_name,
      k
    )
  
  
  run_status_summary <- bind_rows(
    run_status_parts
  ) %>%
    arrange(
      matrix_name,
      k,
      status
    )
  
  
  cluster_composition_summary <- if (
    SAVE_COMPOSITION
  ) {
    
    bind_rows(
      composition_summary_parts
    ) %>%
      arrange(
        matrix_name,
        k,
        cluster_rank,
        composition_variable,
        desc(
          mean_prop_within_cluster
        )
      )
    
  } else {
    
    tibble()
  }
  
  
  # Guardar resúmenes
  
  write_csv(
    kmeans_metrics_summary,
    
    file.path(
      sample_out_dir,
      "kmeans_metrics_summary.csv"
    )
  )
  
  
  write_csv(
    cluster_size_summary,
    
    file.path(
      sample_out_dir,
      "kmeans_cluster_size_summary.csv"
    )
  )
  
  
  write_csv(
    kmeans_centers_mean,
    
    file.path(
      sample_out_dir,
      "kmeans_centers_mean_across_bootstraps.csv"
    )
  )
  
  
  write_csv(
    cluster_distance_summary,
    
    file.path(
      sample_out_dir,
      "kmeans_cluster_distance_summary.csv"
    )
  )
  
  
  write_csv(
    run_status_summary,
    
    file.path(
      sample_out_dir,
      "kmeans_run_status_summary.csv"
    )
  )
  
  
  if (
    SAVE_COMPOSITION
  ) {
    
    write_csv(
      cluster_composition_summary,
      
      file.path(
        sample_out_dir,
        "kmeans_cluster_composition_summary.csv"
      )
    )
  }
  
  
  # Heatmaps medios
  
  if (
    SAVE_HEATMAPS &&
    nrow(
      kmeans_centers_mean
    ) > 0
  ) {
    
    for (
      matrix_name_current in
      MATRICES_TO_RUN
    ) {
      
      for (
        k_current in
        K_GRID
      ) {
        
        plot_data <- kmeans_centers_mean %>%
          filter(
            matrix_name ==
              matrix_name_current,
            
            k ==
              k_current
          )
        
        
        if (
          nrow(
            plot_data
          ) == 0
        ) {
          next
        }
        
        
        plot_data <- plot_data %>%
          mutate(
            cluster_rank =
              factor(
                cluster_rank
              ),
            
            determinant =
              factor(
                determinant,
                
                levels =
                  rev(
                    sort(
                      unique(
                        determinant
                      )
                    )
                  )
              )
          )
        
        
        p <- ggplot(
          plot_data,
          
          aes(
            x =
              cluster_rank,
            
            y =
              determinant,
            
            fill =
              mean_center_value
          )
        ) +
          geom_tile() +
          theme_minimal(
            base_size = 10
          ) +
          labs(
            title =
              paste0(
                analysis_sample_current,
                " | ",
                matrix_name_current,
                " | K = ",
                k_current
              ),
            
            subtitle =
              paste0(
                "Mean centroids across ",
                length(
                  boot_ids
                ),
                " bootstrap replicates"
              ),
            
            x =
              "Cluster rank",
            
            y =
              "Determinant",
            
            fill =
              "Mean center"
          )
        
        
        ggsave(
          filename =
            file.path(
              sample_fig_dir,
              
              paste0(
                "heatmap_",
                matrix_name_current,
                "_K",
                k_current,
                ".png"
              )
            ),
          
          plot =
            p,
          
          width =
            9,
          
          height =
            8,
          
          dpi =
            300
        )
      }
    }
  }
  
  
  # Marcar la muestra como terminada
  
  writeLines(
    c(
      paste0(
        "analysis_sample=",
        analysis_sample_current
      ),
      
      paste0(
        "n_bootstraps=",
        length(
          boot_ids
        )
      ),
      
      paste0(
        "completed_at=",
        Sys.time()
      )
    ),
    
    done_file
  )
  
  
  cat(
    "\nCompletada muestra: ",
    analysis_sample_current,
    "\n",
    sep = ""
  )
  
  
  print(
    run_status_summary,
    n = Inf,
    width = Inf
  )
  
  
  rm(
    bootstrap_sample,
    boot_row_lookup,
    metrics_summary_parts,
    size_summary_parts,
    centers_mean_parts,
    distance_summary_parts,
    composition_summary_parts,
    run_status_parts
  )
  
  
  invisible(
    gc()
  )
  
  
  list(
    metrics_summary =
      kmeans_metrics_summary,
    
    cluster_size_summary =
      cluster_size_summary,
    
    centers_mean =
      kmeans_centers_mean,
    
    distance_summary =
      cluster_distance_summary,
    
    composition_summary =
      cluster_composition_summary,
    
    run_status_summary =
      run_status_summary
  )
}


# Ejecutar las siete muestras

result_summaries <- vector(
  "list",
  length(
    ANALYSIS_SAMPLES
  )
)


names(
  result_summaries
) <- ANALYSIS_SAMPLES


for (
  sample_index in
  seq_along(
    ANALYSIS_SAMPLES
  )
) {
  
  sample_name <- ANALYSIS_SAMPLES[
    sample_index
  ]
  
  
  result_summaries[[sample_name]] <- run_analysis_sample(
    analysis_sample_current =
      sample_name,
    
    analysis_sample_index =
      sample_index
  )
  
  
  invisible(
    gc()
  )
}


# Resúmenes globales

kmeans_metrics_summary_all <- bind_rows(
  map(
    result_summaries,
    "metrics_summary"
  )
)


cluster_size_summary_all <- bind_rows(
  map(
    result_summaries,
    "cluster_size_summary"
  )
)


kmeans_centers_mean_all <- bind_rows(
  map(
    result_summaries,
    "centers_mean"
  )
)


cluster_distance_summary_all <- bind_rows(
  map(
    result_summaries,
    "distance_summary"
  )
)


cluster_composition_summary_all <- bind_rows(
  map(
    result_summaries,
    "composition_summary"
  )
)


run_status_summary_all <- bind_rows(
  map(
    result_summaries,
    "run_status_summary"
  )
)


write_csv(
  kmeans_metrics_summary_all,
  
  file.path(
    out_dir,
    "kmeans_metrics_summary_all_samples.csv"
  )
)


write_csv(
  cluster_size_summary_all,
  
  file.path(
    out_dir,
    "kmeans_cluster_size_summary_all_samples.csv"
  )
)


write_csv(
  kmeans_centers_mean_all,
  
  file.path(
    out_dir,
    "kmeans_centers_mean_all_samples.csv"
  )
)


write_csv(
  cluster_distance_summary_all,
  
  file.path(
    out_dir,
    "kmeans_cluster_distance_summary_all_samples.csv"
  )
)


if (
  SAVE_COMPOSITION
) {
  
  write_csv(
    cluster_composition_summary_all,
    
    file.path(
      out_dir,
      "kmeans_cluster_composition_summary_all_samples.csv"
    )
  )
}


write_csv(
  run_status_summary_all,
  
  file.path(
    out_dir,
    "kmeans_run_status_summary_all_samples.csv"
  )
)


# Parámetros

parameters <- tibble(
  parameter = c(
    "bootstrap_file",
    "matrix_dir",
    "analysis_samples",
    "matrices_to_run",
    "k_grid",
    "expected_n_bootstraps",
    "max_bootstraps",
    "nstart",
    "iter_max",
    "silhouette_mode",
    "silhouette_max_rows",
    "save_assignments",
    "save_composition",
    "save_heatmaps",
    "execution_mode",
    "chunk_size",
    "resume_completed_samples",
    "n_cores",
    "output_dir"
  ),
  
  value = c(
    bootstrap_file,
    
    matrix_dir,
    
    paste(
      ANALYSIS_SAMPLES,
      collapse = ", "
    ),
    
    paste(
      MATRICES_TO_RUN,
      collapse = ", "
    ),
    
    paste(
      K_GRID,
      collapse = ", "
    ),
    
    as.character(
      EXPECTED_N_BOOT
    ),
    
    as.character(
      MAX_BOOTSTRAPS
    ),
    
    as.character(
      NSTART
    ),
    
    as.character(
      ITER_MAX
    ),
    
    SILHOUETTE_MODE,
    
    as.character(
      SILHOUETTE_MAX_ROWS
    ),
    
    as.character(
      SAVE_ASSIGNMENTS
    ),
    
    as.character(
      SAVE_COMPOSITION
    ),
    
    as.character(
      SAVE_HEATMAPS
    ),
    
    "sequential_memory_safe",
    
    as.character(
      CHUNK_SIZE
    ),
    
    as.character(
      RESUME_COMPLETED_SAMPLES
    ),
    
    "1",
    
    out_dir
  )
)


write_csv(
  parameters,
  
  file.path(
    out_dir,
    "kmeans_bootstrap_parameters.csv"
  )
)


# Resumen en consola

cat(
  "\n============================================================\n",
  "06. K-MEANS 1000 BOOTSTRAPS COMPLETADO\n",
  "============================================================\n"
)


cat(
  "\nBootstrap utilizado:\n",
  bootstrap_file,
  "\n",
  sep = ""
)


cat(
  "\nNúmero de bootstraps utilizados: ",
  length(
    boot_ids
  ),
  "\n",
  sep = ""
)


cat(
  "\nModo de ejecución: secuencial\n"
)


cat(
  "Núcleos utilizados: 1\n"
)


cat(
  "Tamaño de bloque: ",
  CHUNK_SIZE,
  "\n",
  sep = ""
)


cat(
  "\nMuestras analizadas:\n"
)


print(
  ANALYSIS_SAMPLES
)


cat(
  "\nMatrices analizadas:\n"
)


print(
  MATRICES_TO_RUN
)


cat(
  "\nK explorados:\n"
)


print(
  K_GRID
)


cat(
  "\nTamaños de las muestras bootstrap:\n\n"
)


print(
  bootstrap_size_summary,
  n = Inf,
  width = Inf
)


cat(
  "\nEstado de las ejecuciones:\n\n"
)


print(
  run_status_summary_all,
  n = Inf,
  width = Inf
)


cat(
  "\nResultados comparativos principales:\n",
  out_dir,
  "\n",
  sep = ""
)


cat(
  "\nResultados detallados por muestra:\n"
)


for (
  sample_name in
  ANALYSIS_SAMPLES
) {
  
  cat(
    "  - ",
    file.path(
      out_dir,
      sample_name
    ),
    "\n",
    sep = ""
  )
}


message(
  "\nListo. K-means 1000 ejecutado de forma secuencial y con control de memoria."
)