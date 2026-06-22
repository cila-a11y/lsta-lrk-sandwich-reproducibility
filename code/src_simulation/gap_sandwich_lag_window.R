run_mc_lagwindow_sandwich_jtsa <- function(
    output_dir = "results/mc_simulation_sandwich",
    paper_mode = TRUE,
    seed = 20260323,
    n_replications = NULL,
    sample_sizes = NULL,
    probability_grid = seq(0.05, 0.95, by = 0.05),
    max_bandwidth_cap = 160,
    negative_eigen_tolerance = -1e-10,
    near_singular_tolerance = 1e-4,
    polynomial_beta = 2.2,
    polynomial_filter_length = 600,
    save_replication_level_metrics = TRUE,
    return_helpers = FALSE
) {
  
  required_pkgs <- c(
    "ggplot2", "dplyr", "tidyr", "readr", "tibble",
    "pbivnorm", "knitr", "here"
  )
  missing_pkgs <- required_pkgs[
    !vapply(
      required_pkgs,
      requireNamespace,
      logical(1L),
      quietly = TRUE
    )
  ]
  if (length(missing_pkgs) > 0L) {
    stop(
      "Missing required R packages: ",
      paste(missing_pkgs, collapse = ", "),
      ". Restore the frozen project environment before execution.",
      call. = FALSE
    )
  }

  library(here)

  suppressPackageStartupMessages({
    library(ggplot2)
    library(dplyr)
    library(tidyr)
    library(readr)
    library(tibble)
    library(knitr)
  })
  
  if (is.null(n_replications)) {
    n_replications <- if (paper_mode) 500 else 100
  }
  
  if (is.null(sample_sizes)) {
    sample_sizes <- if (paper_mode) c(300L, 600L, 1200L) else c(250L, 500L)
  }
  
  set.seed(seed)
  
  figures_dir <- file.path(output_dir, "figures")
  tables_dir  <- file.path(output_dir, "tables")
  objects_dir <- file.path(output_dir, "objects")
  logs_dir    <- file.path(output_dir, "logs")
  
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(objects_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(logs_dir, showWarnings = FALSE, recursive = TRUE)
  
  write_csv_safely <- function(x, path) {
    readr::write_csv(x, path)
    invisible(path)
  }
  
  write_tex_safely <- function(x, path, digits = 3) {
    tex <- knitr::kable(x, format = "latex", booktabs = TRUE, digits = digits)
    writeLines(tex, con = path)
    invisible(path)
  }
  
  write_table_pair <- function(x, stem, digits = 3) {
    write_csv_safely(x, file.path(tables_dir, paste0(stem, ".csv")))
    write_tex_safely(x, file.path(tables_dir, paste0(stem, ".tex")), digits = digits)
  }
  
  save_plot_dual <- function(plot_object, file_stub, width = 7.5, height = 5.5, dpi = 400) {
    png_path <- file.path(figures_dir, paste0(file_stub, ".png"))
    pdf_path <- file.path(figures_dir, paste0(file_stub, ".pdf"))
    
    ggplot2::ggsave(filename = png_path, plot = plot_object, width = width, height = height, dpi = dpi)
    
    if (capabilities("cairo")) {
      ggplot2::ggsave(
        filename = pdf_path,
        plot = plot_object,
        width = width,
        height = height,
        device = grDevices::cairo_pdf
      )
    } else {
      ggplot2::ggsave(filename = pdf_path, plot = plot_object, width = width, height = height)
    }
    
    invisible(c(png_path, pdf_path))
  }
  
  make_bandwidth_grid <- function(n, cap = max_bandwidth_cap) {
    max_bw <- min(floor(n / 4), cap)
    grid <- unique(round(exp(seq(log(4), log(max_bw), length.out = 10))))
    grid <- sort(unique(as.integer(grid)))
    grid[grid >= 4L & grid < n]
  }
  
  nearest_geq <- function(x, grid) {
    candidates <- grid[grid >= x]
    if (length(candidates) == 0) max(grid) else min(candidates)
  }
  
  normalize_weights <- function(w) {
    w / sqrt(sum(w^2))
  }
  
  make_polynomial_weights <- function(beta, L) {
    normalize_weights((seq_len(L + 1))^(-beta))
  }
  
  simulate_ar1_gaussian <- function(n, rho, burn_in = 1000L) {
    total_n <- n + burn_in
    x <- numeric(total_n)
    innov_sd <- sqrt(1 - rho^2)
    x[1] <- stats::rnorm(1)
    if (total_n >= 2) {
      for (t in 2:total_n) {
        x[t] <- rho * x[t - 1] + innov_sd * stats::rnorm(1)
      }
    }
    x[(burn_in + 1):total_n]
  }
  
  simulate_positive_linear_gaussian <- function(n, weights, burn_in = 1000L) {
    L <- length(weights) - 1L
    total_n <- n + burn_in + L
    e <- stats::rnorm(total_n)
    x_full <- stats::filter(e, filter = weights, method = "convolution", sides = 1)
    x_full <- as.numeric(x_full)
    x_full <- x_full[(L + 1):length(x_full)]
    x_full[(burn_in + 1):(burn_in + n)]
  }
  
  theoretical_acf_from_weights <- function(weights) {
    denom <- sum(weights^2)
    L <- length(weights) - 1L
    rho <- numeric(L)
    if (L >= 1) {
      for (k in seq_len(L)) {
        rho[k] <- sum(weights[1:(L + 1 - k)] * weights[(1 + k):(L + 1)]) / denom
      }
    }
    rho
  }
  
  bvn_cdf <- function(z1, z2, rho) {
    if (abs(rho) < 1e-14) {
      return(stats::pnorm(z1) * stats::pnorm(z2))
    }
    pbivnorm::pbivnorm(z1, z2, rho)
  }
  
  compute_true_gamma <- function(prob_grid, rho_sequence, scenario_label) {
    z <- stats::qnorm(prob_grid)
    base_gamma <- outer(prob_grid, prob_grid, pmin) - tcrossprod(prob_grid)
    gamma_true <- base_gamma
    
    if (length(rho_sequence) > 0) {
      for (rho in rho_sequence) {
        cdf_mat <- outer(
          z, z,
          Vectorize(function(a, b) bvn_cdf(a, b, rho))
        )
        cov_mat <- cdf_mat - tcrossprod(prob_grid)
        gamma_true <- gamma_true + 2 * cov_mat
      }
    }
    
    diag_truth <- tibble(
      probability = prob_grid,
      true_diagonal = diag(gamma_true),
      iid_diagonal = prob_grid * (1 - prob_grid),
      inflation_over_iid = diag(gamma_true) / (prob_grid * (1 - prob_grid)),
      scenario = scenario_label
    )
    
    list(matrix = gamma_true, diagonal = diag_truth)
  }
  
  make_indicator_matrix <- function(x, thresholds) {
    outer(x, thresholds, FUN = "<=") * 1
  }
  
  make_centered_indicator_proxy <- function(indicator_matrix) {
    centered <- sweep(indicator_matrix, 2, colMeans(indicator_matrix), "-")
    as.numeric(centered %*% rep(1 / sqrt(ncol(centered)), ncol(centered)))
  }
  
  select_bandwidth_screen <- function(proxy_series, bandwidth_grid, consecutive = 5L) {
    n <- length(proxy_series)
    max_lag <- max(bandwidth_grid)
    acf_vals <- stats::acf(proxy_series, lag.max = max_lag, plot = FALSE, demean = TRUE)$acf[-1]
    threshold <- 2 / sqrt(n)
    
    pilot <- max_lag
    if (length(acf_vals) >= consecutive) {
      found <- FALSE
      for (k in seq_len(length(acf_vals) - consecutive + 1L)) {
        if (all(abs(acf_vals[k:(k + consecutive - 1L)]) < threshold)) {
          pilot <- max(4L, k)
          found <- TRUE
          break
        }
      }
      if (!found) pilot <- max_lag
    }
    
    screen_bw <- nearest_geq(pilot, bandwidth_grid)
    list(
      pilot_bandwidth = as.integer(pilot),
      selected_bandwidth = as.integer(screen_bw),
      threshold = threshold
    )
  }
  
  build_moments <- function(indicator_matrix, max_lag) {
    n <- nrow(indicator_matrix)
    Fn <- colMeans(indicator_matrix)
    Y  <- sweep(indicator_matrix, 2, Fn, "-")
    
    S0_raw <- crossprod(indicator_matrix) / n - tcrossprod(Fn)
    S0_centered <- crossprod(Y) / n
    
    lag_phi_raw <- vector("list", max_lag)
    lag_phi_centered <- vector("list", max_lag)
    
    if (max_lag >= 1) {
      for (k in seq_len(max_lag)) {
        joint_hat <- crossprod(
          indicator_matrix[1:(n - k), , drop = FALSE],
          indicator_matrix[(k + 1):n, , drop = FALSE]
        ) / (n - k)
        
        lag_phi_raw[[k]] <- joint_hat - tcrossprod(Fn)
        
        lag_phi_centered[[k]] <- crossprod(
          Y[1:(n - k), , drop = FALSE],
          Y[(k + 1):n, , drop = FALSE]
        ) / n
      }
    }
    
    list(
      Fn = Fn,
      Y = Y,
      S0_raw = S0_raw,
      S0_centered = S0_centered,
      lag_phi_raw = lag_phi_raw,
      lag_phi_centered = lag_phi_centered
    )
  }
  
  lag_weights <- function(bandwidth, max_lag, method) {
    if (max_lag <= 0) return(numeric(0))
    k <- seq_len(max_lag)
    
    if (method == "bartlett") {
      pmax(1 - k / bandwidth, 0)
    } else if (method == "hard") {
      as.numeric(k <= bandwidth)
    } else {
      stop("unknown method")
    }
  }
  
  estimate_lagwindow <- function(moments, bandwidth, method) {
    max_available_lag <- length(moments$lag_phi_raw)
    max_lag <- min(max_available_lag, bandwidth)
    
    gamma_hat <- moments$S0_raw
    
    if (max_lag >= 1) {
      w <- lag_weights(bandwidth = bandwidth, max_lag = max_lag, method = method)
      for (k in seq_len(max_lag)) {
        if (w[k] != 0) {
          gamma_hat <- gamma_hat + w[k] * (moments$lag_phi_raw[[k]] + t(moments$lag_phi_raw[[k]]))
        }
      }
    }
    
    (gamma_hat + t(gamma_hat)) / 2
  }
  
  estimate_sandwich_bartlett <- function(moments, bandwidth) {
    max_available_lag <- length(moments$lag_phi_centered)
    max_lag <- min(max_available_lag, bandwidth)
    
    gamma_hat <- moments$S0_centered
    
    if (max_lag >= 1) {
      w <- lag_weights(bandwidth = bandwidth, max_lag = max_lag, method = "bartlett")
      for (k in seq_len(max_lag)) {
        if (w[k] != 0) {
          gamma_hat <- gamma_hat + w[k] * (moments$lag_phi_centered[[k]] + t(moments$lag_phi_centered[[k]]))
        }
      }
    }
    
    (gamma_hat + t(gamma_hat)) / 2
  }
  
  matrix_to_long <- function(mat, prob_grid, value_name = "value") {
    df <- as.data.frame(mat)
    colnames(df) <- paste0("p", sprintf("%02d", round(100 * prob_grid)))
    df$probability_p <- prob_grid
    
    df |>
      tidyr::pivot_longer(
        cols = -probability_p,
        names_to = "probability_q_label",
        values_to = value_name
      ) |>
      dplyr::mutate(probability_q = as.numeric(sub("p", "", probability_q_label)) / 100) |>
      dplyr::select(probability_p, probability_q, dplyr::all_of(value_name))
  }
  
  matrix_metrics <- function(gamma_hat, gamma_true, prob_grid, negative_tol, near_singular_tol) {
    diff_mat <- gamma_hat - gamma_true
    eig_hat <- eigen(gamma_hat, symmetric = TRUE, only.values = TRUE)$values
    eig_diff <- eigen(diff_mat, symmetric = TRUE, only.values = TRUE)$values
    
    fro_error <- sqrt(sum(diff_mat^2))
    true_fro <- sqrt(sum(gamma_true^2))
    
    idx50 <- which.min(abs(prob_grid - 0.50))
    idx90 <- which.min(abs(prob_grid - 0.90))
    idx95 <- which.min(abs(prob_grid - 0.95))
    idx10 <- which.min(abs(prob_grid - 0.10))
    
    tibble(
      frobenius_error = fro_error,
      relative_frobenius_error = fro_error / true_fro,
      spectral_error = max(abs(eig_diff)),
      trace = sum(diag(gamma_hat)),
      min_eigenvalue = min(eig_hat),
      max_eigenvalue = max(eig_hat),
      negative_min_eigen = min(eig_hat) < negative_tol,
      near_singular = min(eig_hat) < near_singular_tol,
      mean_diagonal = mean(diag(gamma_hat)),
      max_diagonal = max(diag(gamma_hat)),
      diag_50 = gamma_hat[idx50, idx50],
      diag_90 = gamma_hat[idx90, idx90],
      diag_95 = gamma_hat[idx95, idx95],
      cross_10_90 = gamma_hat[idx10, idx90],
      diagonal_inflation_90 = gamma_hat[idx90, idx90] / (0.90 * 0.10)
    )
  }
  
  gap_metrics <- function(gamma_sandwich, gamma_lagwindow, gamma_true, prob_grid) {
    gap_mat <- gamma_sandwich - gamma_lagwindow
    lw_error <- gamma_lagwindow - gamma_true
    
    tibble(
      gap_frobenius = sqrt(sum(gap_mat^2)),
      gap_supnorm = max(abs(gap_mat)),
      gap_trace = sum(diag(gamma_sandwich)) - sum(diag(gamma_lagwindow)),
      gap_min_eigen = min(eigen(gamma_sandwich, symmetric = TRUE, only.values = TRUE)$values) -
        min(eigen(gamma_lagwindow, symmetric = TRUE, only.values = TRUE)$values),
      gap_over_lw_error = sqrt(sum(gap_mat^2)) / sqrt(sum(lw_error^2))
    )
  }
  
  summarize_bandwidth_risk <- function(df) {
    df |>
      group_by(scenario, scenario_family, sample_size, method, bandwidth) |>
      summarise(
        mean_frobenius_error = mean(frobenius_error),
        mean_relative_frobenius_error = mean(relative_frobenius_error),
        mean_spectral_error = mean(spectral_error),
        mean_trace = mean(trace),
        mean_min_eigenvalue = mean(min_eigenvalue),
        negative_eigen_frequency = mean(negative_min_eigen),
        near_singular_frequency = mean(near_singular),
        .groups = "drop"
      )
  }
  
  build_oracle_bandwidth_table <- function(bandwidth_summary) {
    bandwidth_summary |>
      group_by(scenario, scenario_family, sample_size, method) |>
      slice_min(order_by = mean_relative_frobenius_error, n = 1, with_ties = FALSE) |>
      ungroup() |>
      rename(
        oracle_bandwidth = bandwidth,
        oracle_mean_frobenius_error = mean_frobenius_error,
        oracle_mean_relative_frobenius_error = mean_relative_frobenius_error,
        oracle_mean_spectral_error = mean_spectral_error,
        oracle_negative_eigen_frequency = negative_eigen_frequency,
        oracle_near_singular_frequency = near_singular_frequency
      )
  }
  
  if (isTRUE(return_helpers)) {
    return(list(
      make_bandwidth_grid = make_bandwidth_grid,
      nearest_geq = nearest_geq,
      normalize_weights = normalize_weights,
      make_polynomial_weights = make_polynomial_weights,
      simulate_ar1_gaussian = simulate_ar1_gaussian,
      simulate_positive_linear_gaussian =
        simulate_positive_linear_gaussian,
      theoretical_acf_from_weights =
        theoretical_acf_from_weights,
      bvn_cdf = bvn_cdf,
      compute_true_gamma = compute_true_gamma,
      make_indicator_matrix = make_indicator_matrix,
      make_centered_indicator_proxy =
        make_centered_indicator_proxy,
      select_bandwidth_screen = select_bandwidth_screen,
      build_moments = build_moments,
      lag_weights = lag_weights,
      estimate_lagwindow = estimate_lagwindow,
      estimate_sandwich_bartlett =
        estimate_sandwich_bartlett,
      matrix_metrics = matrix_metrics,
      gap_metrics = gap_metrics
    ))
  }

  polynomial_weights <- make_polynomial_weights(polynomial_beta, polynomial_filter_length)
  
  scenario_table <- tibble(
    scenario_id = c("geom_04", "geom_08", "tpl_22"),
    scenario = c(
      "AR(1), rho = 0.4",
      "AR(1), rho = 0.8",
      paste0("truncated positive linear, beta = ", polynomial_beta)
    ),
    scenario_family = c("geometric", "geometric", "truncated_linear_proxy"),
    generator = c("ar1", "ar1", "positive_linear"),
    parameter_1 = c(0.4, 0.8, polynomial_beta),
    parameter_2 = c(NA_real_, NA_real_, polynomial_filter_length)
  )
  
  write_table_pair(scenario_table, "table_00_simulation_design", digits = 3)
  
  truth_list <- vector("list", nrow(scenario_table))
  names(truth_list) <- scenario_table$scenario_id
  truth_summary_rows <- list()
  
  for (i in seq_len(nrow(scenario_table))) {
    scen <- scenario_table[i, ]
    
    if (scen$generator == "ar1") {
      rho <- scen$parameter_1
      k_max <- ceiling(log(1e-10) / log(rho))
      rho_sequence <- rho^(seq_len(k_max))
    } else {
      rho_sequence <- theoretical_acf_from_weights(polynomial_weights)
      rho_sequence <- rho_sequence[abs(rho_sequence) > 1e-12]
    }
    
    truth_object <- compute_true_gamma(
      prob_grid = probability_grid,
      rho_sequence = rho_sequence,
      scenario_label = scen$scenario
    )
    
    truth_list[[scen$scenario_id]] <- list(
      scenario_info = scen,
      rho_sequence = rho_sequence,
      gamma_true = truth_object$matrix,
      diagonal_truth = truth_object$diagonal
    )
    
    truth_summary_rows[[length(truth_summary_rows) + 1L]] <- tibble(
      scenario_id = scen$scenario_id,
      scenario = scen$scenario,
      scenario_family = scen$scenario_family,
      n_lags_in_truth_sum = length(rho_sequence),
      trace_true = sum(diag(truth_object$matrix)),
      min_eigenvalue_true = min(eigen(truth_object$matrix, symmetric = TRUE, only.values = TRUE)$values),
      max_diagonal_true = max(diag(truth_object$matrix)),
      true_diag_90 = truth_object$matrix[which.min(abs(probability_grid - 0.90)), which.min(abs(probability_grid - 0.90))]
    )
  }
  
  truth_summary <- bind_rows(truth_summary_rows)
  write_table_pair(truth_summary, "table_01_truth_summary", digits = 5)
  saveRDS(truth_list, file.path(objects_dir, "truth_objects.rds"))
  
  all_replication_metrics <- list()
  selected_replication_metrics <- list()
  selected_gap_metrics <- list()
  all_gap_metrics <- list()
  selected_bandwidth_rows <- list()
  selected_matrix_store <- list()
  
  total_jobs <- nrow(scenario_table) * length(sample_sizes) * n_replications
  progress_counter <- 0L
  pb <- txtProgressBar(min = 0, max = total_jobs, style = 3)
  
  for (i in seq_len(nrow(scenario_table))) {
    scen <- scenario_table[i, ]
    gamma_true <- truth_list[[scen$scenario_id]]$gamma_true
    thresholds <- stats::qnorm(probability_grid)
    
    for (n in sample_sizes) {
      bandwidth_grid <- make_bandwidth_grid(n)
      max_bandwidth <- max(bandwidth_grid)
      scenario_key <- paste0(scen$scenario_id, "__n", n)
      selected_matrix_store[[scenario_key]] <- list(
        bartlett_lag = list(),
        bartlett_sandwich = list(),
        hard = list()
      )
      
      for (rep in seq_len(n_replications)) {
        if (scen$generator == "ar1") {
          x <- simulate_ar1_gaussian(n = n, rho = scen$parameter_1)
        } else {
          x <- simulate_positive_linear_gaussian(n = n, weights = polynomial_weights)
        }
        
        indicator_matrix <- make_indicator_matrix(x, thresholds)
        proxy_series <- make_centered_indicator_proxy(indicator_matrix)
        bw_screen <- select_bandwidth_screen(proxy_series, bandwidth_grid)
        selected_bw <- bw_screen$selected_bandwidth
        
        moments <- build_moments(indicator_matrix = indicator_matrix, max_lag = max_bandwidth)
        
        selected_bandwidth_rows[[length(selected_bandwidth_rows) + 1L]] <- tibble(
          scenario_id = scen$scenario_id,
          scenario = scen$scenario,
          scenario_family = scen$scenario_family,
          sample_size = n,
          replication = rep,
          pilot_bandwidth = bw_screen$pilot_bandwidth,
          selected_bandwidth = selected_bw,
          acf_screen_threshold = bw_screen$threshold
        )
        
        for (bw in bandwidth_grid) {
          gamma_bartlett_lag <- estimate_lagwindow(moments, bw, "bartlett")
          gamma_bartlett_sandwich <- estimate_sandwich_bartlett(moments, bw)
          gamma_hard <- estimate_lagwindow(moments, bw, "hard")
          
          estimator_list <- list(
            bartlett_lag = gamma_bartlett_lag,
            bartlett_sandwich = gamma_bartlett_sandwich,
            hard = gamma_hard
          )
          
          for (method_name in names(estimator_list)) {
            metrics_row <- matrix_metrics(
              gamma_hat = estimator_list[[method_name]],
              gamma_true = gamma_true,
              prob_grid = probability_grid,
              negative_tol = negative_eigen_tolerance,
              near_singular_tol = near_singular_tolerance
            ) |>
              mutate(
                scenario_id = scen$scenario_id,
                scenario = scen$scenario,
                scenario_family = scen$scenario_family,
                sample_size = n,
                replication = rep,
                method = method_name,
                bandwidth = bw
              )
            
            all_replication_metrics[[length(all_replication_metrics) + 1L]] <- metrics_row
            
            if (bw == selected_bw) {
              selected_replication_metrics[[length(selected_replication_metrics) + 1L]] <-
                metrics_row |>
                mutate(
                  selected_bandwidth = selected_bw,
                  pilot_bandwidth = bw_screen$pilot_bandwidth
                )
            }
          }
          
          gap_row <- gap_metrics(
            gamma_sandwich = gamma_bartlett_sandwich,
            gamma_lagwindow = gamma_bartlett_lag,
            gamma_true = gamma_true,
            prob_grid = probability_grid
          ) |>
            mutate(
              scenario_id = scen$scenario_id,
              scenario = scen$scenario,
              scenario_family = scen$scenario_family,
              sample_size = n,
              replication = rep,
              bandwidth = bw
            )
          
          all_gap_metrics[[length(all_gap_metrics) + 1L]] <- gap_row
          
          if (bw == selected_bw) {
            selected_gap_metrics[[length(selected_gap_metrics) + 1L]] <-
              gap_row |>
              mutate(
                selected_bandwidth = selected_bw,
                pilot_bandwidth = bw_screen$pilot_bandwidth
              )
            
            selected_matrix_store[[scenario_key]]$bartlett_lag[[rep]] <- gamma_bartlett_lag
            selected_matrix_store[[scenario_key]]$bartlett_sandwich[[rep]] <- gamma_bartlett_sandwich
            selected_matrix_store[[scenario_key]]$hard[[rep]] <- gamma_hard
          }
        }
        
        progress_counter <- progress_counter + 1L
        setTxtProgressBar(pb, progress_counter)
      }
    }
  }
  close(pb)
  
  all_replication_metrics_df <- bind_rows(all_replication_metrics)
  selected_replication_metrics_df <- bind_rows(selected_replication_metrics)
  selected_gap_metrics_df <- bind_rows(selected_gap_metrics)
  all_gap_metrics_df <- bind_rows(all_gap_metrics)
  selected_bandwidth_df <- bind_rows(selected_bandwidth_rows)
  
  if (save_replication_level_metrics) {
    write_csv_safely(all_replication_metrics_df, file.path(tables_dir, "mc_replication_metrics_all_bandwidths.csv"))
    write_csv_safely(selected_replication_metrics_df, file.path(tables_dir, "mc_replication_metrics_selected_bandwidth.csv"))
    write_csv_safely(all_gap_metrics_df, file.path(tables_dir, "mc_gap_metrics_all_bandwidths.csv"))
    write_csv_safely(selected_gap_metrics_df, file.path(tables_dir, "mc_gap_metrics_selected_bandwidth.csv"))
    write_csv_safely(selected_bandwidth_df, file.path(tables_dir, "mc_selected_bandwidths.csv"))
  }
  
  saveRDS(selected_matrix_store, file.path(objects_dir, "selected_gamma_matrices.rds"))
  
  bandwidth_risk_summary <- summarize_bandwidth_risk(all_replication_metrics_df)
  oracle_bandwidth_table <- build_oracle_bandwidth_table(bandwidth_risk_summary)
  
  selected_performance_main <- selected_replication_metrics_df |>
    group_by(scenario, scenario_family, sample_size, method) |>
    summarise(
      mean_selected_bandwidth = mean(selected_bandwidth),
      mean_relative_frobenius_error = mean(relative_frobenius_error),
      mean_spectral_error = mean(spectral_error),
      mean_min_eigenvalue = mean(min_eigenvalue),
      negative_eigen_frequency = mean(negative_min_eigen),
      near_singular_frequency = mean(near_singular),
      mean_trace = mean(trace),
      .groups = "drop"
    )
  
  sandwich_gap_main <- selected_gap_metrics_df |>
    group_by(scenario, scenario_family, sample_size) |>
    summarise(
      mean_gap_frobenius = mean(gap_frobenius),
      mean_gap_supnorm = mean(gap_supnorm),
      mean_gap_min_eigen = mean(gap_min_eigen),
      mean_gap_over_lw_error = mean(gap_over_lw_error),
      .groups = "drop"
    )
  
  gap_over_bandwidth_summary <- all_gap_metrics_df |>
    group_by(scenario, scenario_family, sample_size, bandwidth) |>
    summarise(
      mean_gap_frobenius = mean(gap_frobenius),
      mean_gap_supnorm = mean(gap_supnorm),
      mean_gap_over_lw_error = mean(gap_over_lw_error),
      .groups = "drop"
    )
  
  write_table_pair(selected_performance_main, "table_03_selected_performance_main", digits = 4)
  write_table_pair(sandwich_gap_main, "table_04_sandwich_gap_main", digits = 4)
  write_table_pair(oracle_bandwidth_table, "table_05_oracle_bandwidths", digits = 4)
  write_table_pair(bandwidth_risk_summary, "table_06_bandwidth_risk_summary", digits = 4)
  
  base_theme <- theme_minimal(base_size = 12) +
    theme(
      plot.title = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      strip.text = element_text(size = 10),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10)
    )
  
  p_risk <- ggplot(
    bandwidth_risk_summary,
    aes(x = bandwidth, y = mean_relative_frobenius_error, color = method)
  ) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.2) +
    facet_grid(scenario ~ sample_size, scales = "free_y") +
    labs(x = "bandwidth", y = "mean relative Frobenius error") +
    base_theme +
    theme(legend.position = "bottom")
  save_plot_dual(p_risk, "figure_02_risk_curves_relative_frobenius", width = 11, height = 8)
  
  p_neg <- ggplot(
    bandwidth_risk_summary,
    aes(x = bandwidth, y = negative_eigen_frequency, color = method)
  ) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.2) +
    facet_grid(scenario ~ sample_size) +
    labs(x = "bandwidth", y = "frequency of negative minimum eigenvalue") +
    base_theme +
    theme(legend.position = "bottom")
  save_plot_dual(p_neg, "figure_03_negative_eigen_frequency", width = 11, height = 8)
  
  p_gap <- ggplot(
    gap_over_bandwidth_summary,
    aes(x = bandwidth, y = mean_gap_over_lw_error)
  ) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.2) +
    facet_grid(scenario ~ sample_size, scales = "free_y") +
    labs(x = "bandwidth", y = "mean sandwich-gap / lag-window error") +
    base_theme
  save_plot_dual(p_gap, "figure_04_bartlett_gap_over_bandwidth", width = 11, height = 8)
  
  summary_note <- c(
    "Monte Carlo outputs for the JTSA version of the lag-window / sandwich paper",
    paste0("seed: ", seed),
    paste0("replications per scenario and sample size: ", n_replications),
    paste0("sample sizes: ", paste(sample_sizes, collapse = ", ")),
    paste0("probability grid: ", paste(probability_grid, collapse = ", ")),
    "methods compared: bartlett lag-window, bartlett sandwich, hard truncation",
    "main Monte Carlo outputs written in both CSV and LaTeX tabular format."
  )
  writeLines(summary_note, con = file.path(output_dir, "mc_simulation_summary.txt"))
  
  invisible(list(
    truth_summary = truth_summary,
    selected_performance_main = selected_performance_main,
    sandwich_gap_main = sandwich_gap_main,
    oracle_bandwidth_table = oracle_bandwidth_table
  ))
}

if (
  sys.nframe() == 0L &&
  "--run" %in% commandArgs(trailingOnly = TRUE)
) {
  run_mc_lagwindow_sandwich_jtsa()
}
