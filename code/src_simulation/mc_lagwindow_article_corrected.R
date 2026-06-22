# Monte Carlo simulation script for the article
# lag-window estimation of the long-run covariance kernel
# of the empirical process under association
#
# This version is aligned with the article-level lag-window definitions:
#   1) Bartlett lag-window estimator
#   2) classical hard-truncation estimator
#
# Important implementation note:
#   - the estimators below are the classical lag-window forms based on
#     \hat{F}_{k,n}(s,t) - F_n(s) F_n(t), with denominator (n-k)
#     in the lag-k empirical joint distribution;
#   - this is not the sample-centered sandwich implementation.
#
# Simulation design:
#   - associated Gaussian AR(1) with moderate dependence (geometric covariance decay)
#   - associated Gaussian AR(1) with strong dependence (geometric covariance decay)
#   - associated truncated positive linear Gaussian process with polynomially decaying
#     coefficients (finite-memory proxy design)
#
# The target object is the probability-indexed long-run covariance kernel
# evaluated on p in {0.05, 0.10, ..., 0.95}.
#
# The script computes:
#   - exact / numerically exact truth on the probability grid
#   - Monte Carlo risk curves over bandwidths
#   - practical bandwidth selection performance
#   - oracle bandwidths by scenario, method, and sample size
#   - entrywise bias / rmse for key entries
#   - spectral stability diagnostics
#   - article-ready plots without titles

run_mc_lagwindow_article <- function(
  output_dir = "results/mc_simulation_lagwindow",
  paper_mode = TRUE,
  seed = 20260319,
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

  # --------------------------------------------------------------------------------------------
  # 0) package management
  # --------------------------------------------------------------------------------------------
  
  required_pkgs <- c(
    "ggplot2", "dplyr", "tidyr", "readr",
    "tibble", "pbivnorm", "here"
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

  suppressPackageStartupMessages({
    library(ggplot2)
    library(dplyr)
    library(tidyr)
    library(readr)
    library(tibble)
  })

  # --------------------------------------------------------------------------------------------
  # 1) defaults and folders
  # --------------------------------------------------------------------------------------------
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

  # --------------------------------------------------------------------------------------------
  # 2) helper functions
  # --------------------------------------------------------------------------------------------
  cat_line <- function(char = "=", n = 100) cat(paste(rep(char, n), collapse = ""), "\n", sep = "")

  print_section <- function(text) {
    cat("\n")
    cat_line("=")
    cat(text, "\n")
    cat_line("=")
  }

  print_subsection <- function(text) {
    cat("\n")
    cat_line("-")
    cat(text, "\n")
    cat_line("-")
  }

  write_csv_safely <- function(x, path) {
    readr::write_csv(x, path)
    invisible(path)
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
    if (max_bw < 6) {
      return(unique(pmax(4L, seq.int(4L, max(4L, n - 2L), by = 1L))))
    }
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
    m <- length(prob_grid)
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

    dimnames(gamma_true) <- list(
      paste0("p", sprintf("%02d", round(100 * prob_grid))),
      paste0("p", sprintf("%02d", round(100 * prob_grid)))
    )

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

  # Classical lag-window building blocks:
  #   S0      = F_n(s \wedge t) - F_n(s) F_n(t)
  #   lag_phi = \hat{F}_{k,n}(s,t) - F_n(s) F_n(t), with denominator (n-k)
  build_lagwindow_moments <- function(indicator_matrix, max_lag) {
    n <- nrow(indicator_matrix)
    Fn <- colMeans(indicator_matrix)

    S0 <- crossprod(indicator_matrix) / n - tcrossprod(Fn)
    lag_phi <- vector("list", max_lag)

    if (max_lag >= 1) {
      for (k in seq_len(max_lag)) {
        joint_hat <- crossprod(
          indicator_matrix[1:(n - k), , drop = FALSE],
          indicator_matrix[(k + 1):n, , drop = FALSE]
        ) / (n - k)

        lag_phi[[k]] <- joint_hat - tcrossprod(Fn)
      }
    }

    list(Fn = Fn, S0 = S0, lag_phi = lag_phi)
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

  estimate_gamma_from_lagwindow_moments <- function(moments, bandwidth, method) {
    max_available_lag <- length(moments$lag_phi)
    max_lag <- min(max_available_lag, bandwidth)

    gamma_hat <- moments$S0

    if (max_lag >= 1) {
      w <- lag_weights(bandwidth = bandwidth, max_lag = max_lag, method = method)
      for (k in seq_len(max_lag)) {
        if (w[k] != 0) {
          gamma_hat <- gamma_hat + w[k] * (moments$lag_phi[[k]] + t(moments$lag_phi[[k]]))
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
      dplyr::mutate(
        probability_q = as.numeric(sub("p", "", probability_q_label)) / 100
      ) |>
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

  summarize_selected_performance <- function(df) {
    df |>
      group_by(scenario, scenario_family, sample_size, method) |>
      summarise(
        n_replications = n(),
        mean_selected_bandwidth = mean(selected_bandwidth),
        sd_selected_bandwidth = stats::sd(selected_bandwidth),
        mean_frobenius_error = mean(frobenius_error),
        sd_frobenius_error = stats::sd(frobenius_error),
        mean_relative_frobenius_error = mean(relative_frobenius_error),
        sd_relative_frobenius_error = stats::sd(relative_frobenius_error),
        mean_spectral_error = mean(spectral_error),
        mean_trace = mean(trace),
        mean_min_eigenvalue = mean(min_eigenvalue),
        negative_eigen_frequency = mean(negative_min_eigen),
        near_singular_frequency = mean(near_singular),
        mean_diag_90 = mean(diag_90),
        bias_diag_90 = mean(diag_90 - true_diag_90),
        rmse_diag_90 = sqrt(mean((diag_90 - true_diag_90)^2)),
        bias_cross_10_90 = mean(cross_10_90 - true_cross_10_90),
        rmse_cross_10_90 = sqrt(mean((cross_10_90 - true_cross_10_90)^2)),
        .groups = "drop"
      )
  }

  summarize_bandwidth_risk <- function(df) {
    df |>
      group_by(scenario, scenario_family, sample_size, method, bandwidth) |>
      summarise(
        mean_frobenius_error = mean(frobenius_error),
        median_frobenius_error = stats::median(frobenius_error),
        mean_relative_frobenius_error = mean(relative_frobenius_error),
        mean_spectral_error = mean(spectral_error),
        mean_min_eigenvalue = mean(min_eigenvalue),
        negative_eigen_frequency = mean(negative_min_eigen),
        near_singular_frequency = mean(near_singular),
        mean_trace = mean(trace),
        mean_diag_90 = mean(diag_90),
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

  # --------------------------------------------------------------------------------------------
  # 3) simulation design
  # --------------------------------------------------------------------------------------------
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
      build_lagwindow_moments = build_lagwindow_moments,
      lag_weights = lag_weights,
      estimate_gamma_from_lagwindow_moments =
        estimate_gamma_from_lagwindow_moments,
      matrix_metrics = matrix_metrics
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

  print_section("1) Monte Carlo design")
  cat("seed:", seed, "\n")
  cat("replications per scenario and sample size:", n_replications, "\n")
  cat("sample sizes:", paste(sample_sizes, collapse = ", "), "\n")
  cat("probability grid:", paste(probability_grid, collapse = ", "), "\n")
  cat("output folder:", normalizePath(output_dir, winslash = "/", mustWork = FALSE), "\n")
  cat("note: the third design is a truncated positive linear finite-memory proxy.\n")
  print(scenario_table)

  write_csv_safely(scenario_table, file.path(tables_dir, "table_01_simulation_design.csv"))

  # --------------------------------------------------------------------------------------------
  # 4) exact / numerically exact truth by scenario
  # --------------------------------------------------------------------------------------------
  print_section("2) Computing the truth on the probability grid")

  truth_list <- vector("list", nrow(scenario_table))
  names(truth_list) <- scenario_table$scenario_id
  truth_summary_rows <- list()

  for (i in seq_len(nrow(scenario_table))) {
    scen <- scenario_table[i, ]
    cat("processing truth for", scen$scenario, "...\n")

    if (scen$generator == "ar1") {
      rho <- scen$parameter_1
      k_max <- ceiling(log(1e-10) / log(rho))
      rho_sequence <- rho^(seq_len(k_max))
    } else if (scen$generator == "positive_linear") {
      rho_sequence <- theoretical_acf_from_weights(polynomial_weights)
      rho_sequence <- rho_sequence[abs(rho_sequence) > 1e-12]
    } else {
      stop("unknown generator")
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

    diag_90 <- truth_object$matrix[
      which.min(abs(probability_grid - 0.90)),
      which.min(abs(probability_grid - 0.90))
    ]
    cross_10_90 <- truth_object$matrix[
      which.min(abs(probability_grid - 0.10)),
      which.min(abs(probability_grid - 0.90))
    ]

    truth_summary_rows[[length(truth_summary_rows) + 1L]] <- tibble(
      scenario_id = scen$scenario_id,
      scenario = scen$scenario,
      scenario_family = scen$scenario_family,
      n_lags_in_truth_sum = length(rho_sequence),
      trace_true = sum(diag(truth_object$matrix)),
      min_eigenvalue_true = min(eigen(truth_object$matrix, symmetric = TRUE, only.values = TRUE)$values),
      max_eigenvalue_true = max(eigen(truth_object$matrix, symmetric = TRUE, only.values = TRUE)$values),
      mean_diagonal_true = mean(diag(truth_object$matrix)),
      max_diagonal_true = max(diag(truth_object$matrix)),
      true_diag_90 = diag_90,
      true_cross_10_90 = cross_10_90
    )

    truth_long <- matrix_to_long(truth_object$matrix, probability_grid, value_name = "gamma_true") |>
      mutate(scenario = scen$scenario, scenario_id = scen$scenario_id)
    write_csv_safely(truth_long, file.path(tables_dir, paste0("truth_gamma_", scen$scenario_id, ".csv")))

    truth_diag_path <- file.path(tables_dir, paste0("truth_diagonal_", scen$scenario_id, ".csv"))
    write_csv_safely(truth_object$diagonal, truth_diag_path)
  }

  truth_summary <- bind_rows(truth_summary_rows)
  write_csv_safely(truth_summary, file.path(tables_dir, "table_02_truth_summary.csv"))
  saveRDS(truth_list, file.path(objects_dir, "truth_objects.rds"))

  print(truth_summary)

  # --------------------------------------------------------------------------------------------
  # 5) Monte Carlo loop
  # --------------------------------------------------------------------------------------------
  print_section("3) Running the Monte Carlo loop")

  all_replication_metrics <- list()
  selected_replication_metrics <- list()
  selected_matrix_store <- list()
  selected_bandwidth_rows <- list()

  total_jobs <- nrow(scenario_table) * length(sample_sizes) * n_replications
  progress_counter <- 0L
  pb <- txtProgressBar(min = 0, max = total_jobs, style = 3)

  for (i in seq_len(nrow(scenario_table))) {
    scen <- scenario_table[i, ]
    truth_object <- truth_list[[scen$scenario_id]]
    gamma_true <- truth_object$gamma_true

    idx90 <- which.min(abs(probability_grid - 0.90))
    idx10 <- which.min(abs(probability_grid - 0.10))
    true_diag_90 <- gamma_true[idx90, idx90]
    true_cross_10_90 <- gamma_true[idx10, idx90]
    thresholds <- stats::qnorm(probability_grid)

    for (n in sample_sizes) {
      bandwidth_grid <- make_bandwidth_grid(n)
      max_bandwidth <- max(bandwidth_grid)
      scenario_key <- paste0(scen$scenario_id, "__n", n)
      selected_matrix_store[[scenario_key]] <- list(bartlett = list(), hard = list())

      cat("\nscenario:", scen$scenario, "| n =", n,
          "| bandwidth grid:", paste(bandwidth_grid, collapse = ", "), "\n")

      for (rep in seq_len(n_replications)) {
        if (scen$generator == "ar1") {
          x <- simulate_ar1_gaussian(n = n, rho = scen$parameter_1)
        } else if (scen$generator == "positive_linear") {
          x <- simulate_positive_linear_gaussian(n = n, weights = polynomial_weights)
        } else {
          stop("unknown generator")
        }

        indicator_matrix <- make_indicator_matrix(x, thresholds)
        proxy_series <- make_centered_indicator_proxy(indicator_matrix)
        bw_screen <- select_bandwidth_screen(proxy_series = proxy_series, bandwidth_grid = bandwidth_grid)
        selected_bw <- bw_screen$selected_bandwidth

        lagwindow_moments <- build_lagwindow_moments(
          indicator_matrix = indicator_matrix,
          max_lag = max_bandwidth
        )

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

        for (method in c("bartlett", "hard")) {
          gamma_selected <- NULL

          for (bw in bandwidth_grid) {
            gamma_hat <- estimate_gamma_from_lagwindow_moments(
              moments = lagwindow_moments,
              bandwidth = bw,
              method = method
            )

            metrics_row <- matrix_metrics(
              gamma_hat = gamma_hat,
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
                method = method,
                bandwidth = bw,
                true_diag_90 = true_diag_90,
                true_cross_10_90 = true_cross_10_90
              ) |>
              select(
                scenario_id, scenario, scenario_family, sample_size, replication,
                method, bandwidth, everything()
              )

            all_replication_metrics[[length(all_replication_metrics) + 1L]] <- metrics_row

            if (bw == selected_bw) {
              gamma_selected <- gamma_hat
              selected_replication_metrics[[length(selected_replication_metrics) + 1L]] <-
                metrics_row |>
                mutate(
                  selected_bandwidth = selected_bw,
                  pilot_bandwidth = bw_screen$pilot_bandwidth
                )
            }
          }

          if (!is.null(gamma_selected)) {
            selected_matrix_store[[scenario_key]][[method]][[rep]] <- gamma_selected
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
  selected_bandwidth_df <- bind_rows(selected_bandwidth_rows)

  if (save_replication_level_metrics) {
    write_csv_safely(all_replication_metrics_df, file.path(tables_dir, "mc_replication_metrics_all_bandwidths.csv"))
    write_csv_safely(selected_replication_metrics_df, file.path(tables_dir, "mc_replication_metrics_selected_bandwidth.csv"))
    write_csv_safely(selected_bandwidth_df, file.path(tables_dir, "mc_selected_bandwidths.csv"))
  }

  saveRDS(selected_matrix_store, file.path(objects_dir, "selected_gamma_matrices.rds"))

  # --------------------------------------------------------------------------------------------
  # 6) summaries and article tables
  # --------------------------------------------------------------------------------------------
  print_section("4) Building summary tables")

  bandwidth_risk_summary <- summarize_bandwidth_risk(all_replication_metrics_df)
  oracle_bandwidth_table <- build_oracle_bandwidth_table(bandwidth_risk_summary)
  selected_performance_summary <- summarize_selected_performance(selected_replication_metrics_df)

  truth_diag_lookup <- lapply(truth_list, function(obj) {
    gamma_true <- obj$gamma_true
    tibble(
      scenario_id = obj$scenario_info$scenario_id,
      true_diag_50 = gamma_true[which.min(abs(probability_grid - 0.50)), which.min(abs(probability_grid - 0.50))],
      true_diag_90 = gamma_true[which.min(abs(probability_grid - 0.90)), which.min(abs(probability_grid - 0.90))],
      true_diag_95 = gamma_true[which.min(abs(probability_grid - 0.95)), which.min(abs(probability_grid - 0.95))],
      true_cross_10_90 = gamma_true[which.min(abs(probability_grid - 0.10)), which.min(abs(probability_grid - 0.90))]
    )
  }) |>
    bind_rows()

  selected_replication_metrics_aug <- selected_replication_metrics_df |>
    select(-true_diag_90, -true_cross_10_90) |>
    left_join(truth_diag_lookup, by = "scenario_id")

  entrywise_summary <- selected_replication_metrics_aug |>
    group_by(scenario, scenario_family, sample_size, method) |>
    summarise(
      bias_diag_50 = mean(diag_50 - true_diag_50),
      rmse_diag_50 = sqrt(mean((diag_50 - true_diag_50)^2)),
      bias_diag_90 = mean(diag_90 - true_diag_90),
      rmse_diag_90 = sqrt(mean((diag_90 - true_diag_90)^2)),
      bias_diag_95 = mean(diag_95 - true_diag_95),
      rmse_diag_95 = sqrt(mean((diag_95 - true_diag_95)^2)),
      bias_cross_10_90 = mean(cross_10_90 - true_cross_10_90),
      rmse_cross_10_90 = sqrt(mean((cross_10_90 - true_cross_10_90)^2)),
      .groups = "drop"
    )

  bandwidth_selection_summary <- selected_bandwidth_df |>
    group_by(scenario, scenario_family, sample_size) |>
    summarise(
      mean_pilot_bandwidth = mean(pilot_bandwidth),
      sd_pilot_bandwidth = stats::sd(pilot_bandwidth),
      mean_selected_bandwidth = mean(selected_bandwidth),
      sd_selected_bandwidth = stats::sd(selected_bandwidth),
      min_selected_bandwidth = min(selected_bandwidth),
      median_selected_bandwidth = stats::median(selected_bandwidth),
      max_selected_bandwidth = max(selected_bandwidth),
      .groups = "drop"
    )

  psd_summary <- selected_replication_metrics_df |>
    group_by(scenario, scenario_family, sample_size, method) |>
    summarise(
      negative_eigen_frequency = mean(negative_min_eigen),
      near_singular_frequency = mean(near_singular),
      mean_min_eigenvalue = mean(min_eigenvalue),
      median_min_eigenvalue = stats::median(min_eigenvalue),
      min_min_eigenvalue = min(min_eigenvalue),
      .groups = "drop"
    )

  write_csv_safely(bandwidth_risk_summary, file.path(tables_dir, "table_03_bandwidth_risk_summary.csv"))
  write_csv_safely(oracle_bandwidth_table, file.path(tables_dir, "table_04_oracle_bandwidths.csv"))
  write_csv_safely(selected_performance_summary, file.path(tables_dir, "table_05_selected_bandwidth_performance.csv"))
  write_csv_safely(entrywise_summary, file.path(tables_dir, "table_06_entrywise_bias_rmse.csv"))
  write_csv_safely(bandwidth_selection_summary, file.path(tables_dir, "table_07_bandwidth_selection_summary.csv"))
  write_csv_safely(psd_summary, file.path(tables_dir, "table_08_psd_stability_summary.csv"))

  print_subsection("4.1 oracle bandwidth summary")
  print(oracle_bandwidth_table)

  print_subsection("4.2 selected-bandwidth performance summary")
  print(selected_performance_summary)

  print_subsection("4.3 spectral stability summary")
  print(psd_summary)

  # --------------------------------------------------------------------------------------------
  # 7) mean selected matrices and heatmap data
  # --------------------------------------------------------------------------------------------
  print_section("5) Building mean estimated covariance kernels")

  mean_selected_matrices_long <- list()
  mean_selected_diagonals <- list()

  for (scenario_key in names(selected_matrix_store)) {
    split_key <- strsplit(scenario_key, "__n", fixed = TRUE)[[1]]
    scenario_id <- split_key[1]
    sample_size <- as.integer(split_key[2])
    scenario_label <- scenario_table$scenario[match(scenario_id, scenario_table$scenario_id)]
    scenario_family <- scenario_table$scenario_family[match(scenario_id, scenario_table$scenario_id)]

    truth_matrix <- truth_list[[scenario_id]]$gamma_true

    for (method in c("bartlett", "hard")) {
      mats <- selected_matrix_store[[scenario_key]][[method]]
      if (length(mats) == 0) next

      mean_mat <- Reduce("+", mats) / length(mats)
      bias_mat <- mean_mat - truth_matrix

      mean_long <- matrix_to_long(mean_mat, probability_grid, value_name = "mean_gamma_hat") |>
        mutate(
          scenario_id = scenario_id,
          scenario = scenario_label,
          scenario_family = scenario_family,
          sample_size = sample_size,
          method = method,
          matrix_type = "mean_estimate"
        )

      bias_long <- matrix_to_long(bias_mat, probability_grid, value_name = "bias_value") |>
        mutate(
          scenario_id = scenario_id,
          scenario = scenario_label,
          scenario_family = scenario_family,
          sample_size = sample_size,
          method = method,
          matrix_type = "bias"
        )

      mean_selected_matrices_long[[length(mean_selected_matrices_long) + 1L]] <- mean_long
      mean_selected_matrices_long[[length(mean_selected_matrices_long) + 1L]] <- bias_long

      mean_selected_diagonals[[length(mean_selected_diagonals) + 1L]] <- tibble(
        scenario_id = scenario_id,
        scenario = scenario_label,
        scenario_family = scenario_family,
        sample_size = sample_size,
        method = method,
        probability = probability_grid,
        mean_diagonal = diag(mean_mat),
        true_diagonal = diag(truth_matrix),
        diagonal_inflation_estimate = diag(mean_mat) / (probability_grid * (1 - probability_grid)),
        diagonal_inflation_true = diag(truth_matrix) / (probability_grid * (1 - probability_grid))
      )
    }
  }

  mean_selected_matrices_long_df <- bind_rows(mean_selected_matrices_long)
  mean_selected_diagonals_df <- bind_rows(mean_selected_diagonals)

  write_csv_safely(mean_selected_matrices_long_df, file.path(tables_dir, "table_09_mean_selected_matrices_long.csv"))
  write_csv_safely(mean_selected_diagonals_df, file.path(tables_dir, "table_10_mean_selected_diagonals.csv"))

  # --------------------------------------------------------------------------------------------
  # 8) figures
  # --------------------------------------------------------------------------------------------
  print_section("6) Producing article-ready figures")

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

  truth_diagonal_plot_data <- bind_rows(lapply(truth_list, function(obj) obj$diagonal))

  p1 <- ggplot(truth_diagonal_plot_data, aes(x = probability, y = inflation_over_iid, color = scenario)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.4) +
    labs(x = "probability", y = "inflation over i.i.d.") +
    base_theme +
    theme(legend.position = "bottom")
  save_plot_dual(p1, "figure_01_true_diagonal_inflation_by_scenario", width = 7.5, height = 5.2)

  p2 <- ggplot(bandwidth_risk_summary, aes(x = bandwidth, y = mean_frobenius_error, color = method)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.3) +
    facet_grid(scenario ~ sample_size, scales = "free_y") +
    labs(x = "bandwidth", y = "mean frobenius error") +
    base_theme +
    theme(legend.position = "bottom")
  save_plot_dual(p2, "figure_02_risk_curves_frobenius", width = 11, height = 8)

  p3 <- ggplot(selected_replication_metrics_df, aes(x = method, y = relative_frobenius_error)) +
    geom_boxplot(outlier.shape = NA, width = 0.65) +
    geom_jitter(width = 0.12, alpha = 0.12, size = 0.6) +
    facet_grid(scenario ~ sample_size, scales = "free_y") +
    labs(x = "method", y = "relative frobenius error") +
    base_theme +
    theme(legend.position = "none")
  save_plot_dual(p3, "figure_03_selected_relative_frobenius_boxplot", width = 11, height = 8)

  p4 <- ggplot(bandwidth_risk_summary, aes(x = bandwidth, y = negative_eigen_frequency, color = method)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.3) +
    facet_grid(scenario ~ sample_size) +
    labs(x = "bandwidth", y = "frequency of negative minimum eigenvalue") +
    base_theme +
    theme(legend.position = "bottom")
  save_plot_dual(p4, "figure_04_negative_eigen_frequency", width = 11, height = 8)

  p5 <- ggplot(mean_selected_diagonals_df, aes(x = probability, y = diagonal_inflation_estimate, color = method)) +
    geom_line(linewidth = 0.8) +
    facet_grid(scenario ~ sample_size, scales = "free_y") +
    labs(x = "probability", y = "estimated diagonal inflation") +
    base_theme +
    theme(legend.position = "bottom")
  save_plot_dual(p5, "figure_05_estimated_diagonal_inflation_selected", width = 11, height = 8)

  p6_truth_df <- mean_selected_diagonals_df |>
    distinct(scenario, sample_size, probability, true_diagonal)

  p6 <- ggplot(mean_selected_diagonals_df, aes(x = probability, y = mean_diagonal, color = method)) +
    geom_line(linewidth = 0.8) +
    geom_line(
      data = p6_truth_df,
      aes(x = probability, y = true_diagonal),
      linewidth = 0.7,
      linetype = "dashed",
      inherit.aes = FALSE
    ) +
    facet_grid(scenario ~ sample_size, scales = "free_y") +
    labs(x = "probability", y = "diagonal of estimated kernel") +
    base_theme +
    theme(legend.position = "bottom")
  save_plot_dual(p6, "figure_06_diagonal_vs_truth_selected", width = 11, height = 8)

  truth_heatmap_data <- bind_rows(lapply(names(truth_list), function(id) {
    matrix_to_long(truth_list[[id]]$gamma_true, probability_grid, value_name = "gamma_true") |>
      mutate(
        scenario_id = id,
        scenario = truth_list[[id]]$scenario_info$scenario,
        scenario_family = truth_list[[id]]$scenario_info$scenario_family,
        sample_size = factor("truth")
      )
  }))

  p7 <- ggplot(truth_heatmap_data, aes(x = probability_q, y = probability_p, fill = gamma_true)) +
    geom_tile() +
    facet_wrap(~ scenario, ncol = 3) +
    labs(x = "probability q", y = "probability p", fill = "value") +
    base_theme +
    theme(legend.position = "right")
  save_plot_dual(p7, "figure_07_truth_heatmaps", width = 12, height = 4.5)

  largest_n <- max(sample_sizes)
  heatmap_selected_mean <- mean_selected_matrices_long_df |>
    filter(matrix_type == "mean_estimate", sample_size == largest_n)

  p8 <- ggplot(heatmap_selected_mean, aes(x = probability_q, y = probability_p, fill = mean_gamma_hat)) +
    geom_tile() +
    facet_grid(scenario ~ method) +
    labs(x = "probability q", y = "probability p", fill = "value") +
    base_theme +
    theme(legend.position = "right")
  save_plot_dual(p8, paste0("figure_08_mean_selected_heatmaps_n", largest_n), width = 9.5, height = 10)

  heatmap_selected_bias <- mean_selected_matrices_long_df |>
    filter(matrix_type == "bias", sample_size == largest_n)

  p9 <- ggplot(heatmap_selected_bias, aes(x = probability_q, y = probability_p, fill = bias_value)) +
    geom_tile() +
    facet_grid(scenario ~ method) +
    labs(x = "probability q", y = "probability p", fill = "bias") +
    base_theme +
    theme(legend.position = "right")
  save_plot_dual(p9, paste0("figure_09_bias_heatmaps_n", largest_n), width = 9.5, height = 10)

  p10 <- ggplot(oracle_bandwidth_table, aes(x = sample_size, y = oracle_bandwidth, color = method, group = method)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.8) +
    facet_wrap(~ scenario, ncol = 3, scales = "free_y") +
    labs(x = "sample size", y = "oracle bandwidth") +
    base_theme +
    theme(legend.position = "bottom")
  save_plot_dual(p10, "figure_10_oracle_bandwidths", width = 10.5, height = 4.8)

  p11 <- ggplot(bandwidth_selection_summary, aes(x = sample_size, y = mean_selected_bandwidth, group = scenario, color = scenario)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.8) +
    labs(x = "sample size", y = "mean selected bandwidth") +
    base_theme +
    theme(legend.position = "bottom")
  save_plot_dual(p11, "figure_11_practical_bandwidth_selection", width = 7.5, height = 5.2)

  p12 <- ggplot(entrywise_summary, aes(x = method, y = rmse_diag_90, fill = method)) +
    geom_col(width = 0.65, show.legend = FALSE) +
    facet_grid(scenario ~ sample_size, scales = "free_y") +
    labs(x = "method", y = "rmse of diagonal entry at p = 0.90") +
    base_theme
  save_plot_dual(p12, "figure_12_rmse_diag90", width = 11, height = 8)

  # --------------------------------------------------------------------------------------------
  # 9) manifest and summary note
  # --------------------------------------------------------------------------------------------
  print_section("7) Writing manifest and summary note")

  summary_note <- c(
    "Monte Carlo simulation outputs for the lag-window covariance-kernel paper",
    paste0("seed: ", seed),
    paste0("replications per scenario and sample size: ", n_replications),
    paste0("sample sizes: ", paste(sample_sizes, collapse = ", ")),
    paste0("probability grid: ", paste(probability_grid, collapse = ", ")),
    "main methods compared: bartlett lag-window, hard truncation",
    "the third design is a truncated positive linear finite-memory proxy with polynomially decaying coefficients.",
    paste0("largest sample size used for the heatmaps of the mean selected estimators: ", largest_n),
    "all tables are in the tables folder; all article-ready figures are in the figures folder."
  )
  writeLines(summary_note, con = file.path(output_dir, "mc_simulation_summary.txt"))

  all_files <- list.files(output_dir, recursive = TRUE, full.names = TRUE)
  output_dir_norm <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  files_norm <- normalizePath(all_files, winslash = "/", mustWork = FALSE)

  manifest <- tibble(
    relative_path = substring(files_norm, nchar(output_dir_norm) + 2L),
    bytes = file.info(all_files)$size
  ) |>
    arrange(relative_path)

  write_csv_safely(manifest, file.path(output_dir, "file_manifest.csv"))

  print(manifest)

  invisible(list(
    scenario_table = scenario_table,
    truth_summary = truth_summary,
    oracle_bandwidth_table = oracle_bandwidth_table,
    selected_performance_summary = selected_performance_summary,
    bandwidth_selection_summary = bandwidth_selection_summary,
    psd_summary = psd_summary,
    manifest = manifest
  ))
}

if (
  sys.nframe() == 0L &&
  "--run" %in% commandArgs(trailingOnly = TRUE)
) {
  run_mc_lagwindow_article()
}
