args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 4L) {
  stop(
    paste(
      "Usage: run_unified_task.R",
      "<work_repo> <design_dir> <output_root> <task_id>"
    ),
    call. = FALSE
  )
}

work_repo <- normalizePath(args[[1L]], mustWork = TRUE)
design_dir <- normalizePath(args[[2L]], mustWork = TRUE)
output_root <- normalizePath(args[[3L]], mustWork = FALSE)
task_id <- as.integer(args[[4L]])

if (
  length(task_id) != 1L ||
  is.na(task_id) ||
  task_id < 1L
) {
  stop("`task_id` must be one positive integer.", call. = FALSE)
}

source(
  file.path(work_repo, "hpc", "unified", "study_spec.R"),
  local = .GlobalEnv
)

required_packages <- c(
  "digest",
  "ggplot2",
  "dplyr",
  "tidyr",
  "readr",
  "tibble",
  "pbivnorm",
  "knitr",
  "here"
)

available <- vapply(
  required_packages,
  requireNamespace,
  logical(1L),
  quietly = TRUE
)

if (!all(available)) {
  stop(
    "Missing packages: ",
    paste(required_packages[!available], collapse = ", "),
    call. = FALSE
  )
}

write_csv <- function(x, path) {
  utils::write.table(
    x,
    file = path,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = TRUE,
    qmethod = "double",
    na = "NA",
    fileEncoding = "UTF-8"
  )

  invisible(path)
}

object_sha256 <- function(object) {
  digest::digest(
    serialize(object, NULL, version = 3),
    algo = "sha256",
    serialize = FALSE
  )
}

file_sha256 <- function(path) {
  digest::digest(
    path,
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
}

estimate_exact_flat_top <- function(moments, bandwidth) {
  max_available_lag <- length(moments$lag_phi_raw)
  max_lag <- min(max_available_lag, as.integer(bandwidth))

  gamma_hat <- moments$S0_raw

  if (max_lag >= 1L) {
    weights <- unified_exact_flat_top_weights(
      bandwidth = bandwidth,
      max_lag = max_lag
    )

    for (k in seq_len(max_lag)) {
      if (weights[[k]] != 0) {
        gamma_hat <- gamma_hat +
          weights[[k]] * (
            moments$lag_phi_raw[[k]] +
              t(moments$lag_phi_raw[[k]])
          )
      }
    }
  }

  (gamma_hat + t(gamma_hat)) / 2
}

load_legacy_helpers <- function(work_repo, spec) {
  source_environment <- new.env(parent = globalenv())

  sys.source(
    file.path(
      work_repo,
      "src",
      "simulation",
      "gap_sandwich_lag_window.R"
    ),
    envir = source_environment
  )

  if (!is.function(source_environment$run_mc_lagwindow_sandwich_jtsa)) {
    stop("Patched simulation entry point unavailable.", call. = FALSE)
  }

  helper_output <- tempfile("unified_worker_helper_")

  on.exit(
    unlink(helper_output, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  helpers <- source_environment$run_mc_lagwindow_sandwich_jtsa(
    output_dir = helper_output,
    paper_mode = FALSE,
    seed = spec$base_seed,
    n_replications = 1L,
    sample_sizes = 80L,
    probability_grid = c(0.25, 0.50, 0.75),
    max_bandwidth_cap = 12L,
    negative_eigen_tolerance = spec$negative_eigen_tolerance,
    near_singular_tolerance = spec$near_singular_tolerance,
    polynomial_beta = spec$polynomial_beta,
    polynomial_filter_length = spec$polynomial_filter_length,
    save_replication_level_metrics = FALSE,
    return_helpers = TRUE
  )

  needed <- c(
    "make_bandwidth_grid",
    "make_polynomial_weights",
    "simulate_ar1_gaussian",
    "simulate_positive_linear_gaussian",
    "theoretical_acf_from_weights",
    "compute_true_gamma",
    "make_indicator_matrix",
    "make_centered_indicator_proxy",
    "select_bandwidth_screen",
    "build_moments",
    "estimate_lagwindow",
    "estimate_sandwich_bartlett",
    "matrix_metrics",
    "gap_metrics"
  )

  ok <- vapply(helpers[needed], is.function, logical(1L))

  if (!all(ok)) {
    stop(
      "Missing legacy helper(s): ",
      paste(needed[!ok], collapse = ", "),
      call. = FALSE
    )
  }

  helpers
}

compute_truth_for_task <- function(task, spec, helpers) {
  if (task$generator == "ar1") {
    rho <- task$parameter_1

    k_max <- ceiling(
      log(spec$truth_rho_tolerance) / log(rho)
    )

    rho_sequence <- rho^seq_len(k_max)
    truth_type <- "Gaussian AR(1)"
  } else if (task$generator == "positive_linear") {
    weights <- helpers$make_polynomial_weights(
      beta = spec$polynomial_beta,
      L = spec$polynomial_filter_length
    )

    rho_sequence <- helpers$theoretical_acf_from_weights(weights)
    truth_type <- "finite-memory positive linear Gaussian"
  } else {
    stop(
      "Unknown generator: ",
      task$generator,
      call. = FALSE
    )
  }

  truth <- helpers$compute_true_gamma(
    prob_grid = spec$probability_grid,
    rho_sequence = rho_sequence,
    scenario_label = task$scenario_label
  )$matrix

  truth <- (truth + t(truth)) / 2

  values <- eigen(
    truth,
    symmetric = TRUE,
    only.values = TRUE
  )$values

  if (min(values) < -1e-8) {
    stop(
      "Truth matrix is not numerically PSD.",
      call. = FALSE
    )
  }

  list(
    gamma_true = truth,
    truth_type = truth_type,
    n_lags_truth = length(rho_sequence),
    truth_sha256 = object_sha256(truth),
    min_eigenvalue_truth = min(values),
    max_eigenvalue_truth = max(values),
    trace_truth = sum(diag(truth))
  )
}

simulate_one_sample <- function(task, spec, helpers) {
  if (task$generator == "ar1") {
    x <- helpers$simulate_ar1_gaussian(
      n = task$sample_size,
      rho = task$parameter_1,
      burn_in = 1000L
    )
  } else if (task$generator == "positive_linear") {
    weights <- helpers$make_polynomial_weights(
      beta = spec$polynomial_beta,
      L = spec$polynomial_filter_length
    )

    x <- helpers$simulate_positive_linear_gaussian(
      n = task$sample_size,
      weights = weights,
      burn_in = 1000L
    )
  } else {
    stop(
      "Unknown generator: ",
      task$generator,
      call. = FALSE
    )
  }

  if (
    length(x) != task$sample_size ||
    any(!is.finite(x))
  ) {
    stop(
      "Generated sample has invalid length or non-finite values.",
      call. = FALSE
    )
  }

  as.numeric(x)
}

run_task <- function() {
  started <- Sys.time()

  spec <- readRDS(
    file.path(design_dir, "study_spec.rds")
  )

  manifest_file <- file.path(design_dir, "task_manifest.rds")

  if (!file.exists(manifest_file)) {
    manifest_file <- file.path(design_dir, "pilot_manifest.rds")
  }

  if (!file.exists(manifest_file)) {
    stop(
      "No task manifest was found in the design directory.",
      call. = FALSE
    )
  }

  manifest <- readRDS(manifest_file)

  streams <- readRDS(
    file.path(design_dir, "rng_streams.rds")
  )

  if (task_id > nrow(manifest)) {
    stop(
      "task_id exceeds manifest size.",
      call. = FALSE
    )
  }

  task <- manifest[task_id, , drop = FALSE]

  if (nrow(task) != 1L) {
    stop("Task selection failed.", call. = FALSE)
  }

  if (!"rng_probe_sha256" %in% names(task)) {
    task$rng_probe_sha256 <- NA_character_
  }

  if (length(streams) < task$rng_stream_index) {
    stop("RNG stream index out of range.", call. = FALSE)
  }

  final_dir <- file.path(
    output_root,
    task$output_relative_path
  )

  temporary_dir <- paste0(
    final_dir,
    ".tmp.",
    Sys.getpid()
  )

  if (file.exists(final_dir)) {
    stop(
      "Final task directory already exists: ",
      final_dir,
      call. = FALSE
    )
  }

  if (file.exists(temporary_dir)) {
    unlink(
      temporary_dir,
      recursive = TRUE,
      force = TRUE
    )
  }

  dir.create(
    temporary_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  completed <- FALSE

  on.exit({
    if (!completed && file.exists(temporary_dir)) {
      unlink(temporary_dir, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  helpers <- load_legacy_helpers(
    work_repo = work_repo,
    spec = spec
  )

  stream <- streams[[task$rng_stream_index]]
  stream_sha256 <- object_sha256(stream)

  if (!identical(stream_sha256, task$rng_stream_sha256)) {
    stop("RNG stream hash mismatch.", call. = FALSE)
  }

  unified_set_rng_stream(stream)

  sample <- simulate_one_sample(
    task = task,
    spec = spec,
    helpers = helpers
  )

  sample_sha256 <- object_sha256(sample)

  truth <- compute_truth_for_task(
    task = task,
    spec = spec,
    helpers = helpers
  )

  thresholds <- stats::qnorm(
    spec$probability_grid
  )

  indicator_matrix <- helpers$make_indicator_matrix(
    sample,
    thresholds
  )

  bandwidth_grid <- helpers$make_bandwidth_grid(
    n = task$sample_size,
    cap = spec$max_bandwidth_cap
  )

  max_bandwidth <- max(bandwidth_grid)

  proxy <- helpers$make_centered_indicator_proxy(
    indicator_matrix
  )

  selected <- helpers$select_bandwidth_screen(
    proxy,
    bandwidth_grid,
    consecutive = 5L
  )

  selected_bandwidth <- selected$selected_bandwidth

  moments <- helpers$build_moments(
    indicator_matrix,
    max_lag = max_bandwidth
  )

  gamma_true <- truth$gamma_true

  metrics_rows <- list()
  gap_rows <- list()

  row_counter <- 0L
  gap_counter <- 0L

  selected_matrices <- list()

  for (bandwidth in bandwidth_grid) {
    estimates <- list(
      BL = helpers$estimate_lagwindow(
        moments,
        bandwidth = bandwidth,
        method = "bartlett"
      ),
      BS = helpers$estimate_sandwich_bartlett(
        moments,
        bandwidth = bandwidth
      ),
      HT = helpers$estimate_lagwindow(
        moments,
        bandwidth = bandwidth,
        method = "hard"
      ),
      FT = estimate_exact_flat_top(
        moments,
        bandwidth = bandwidth
      )
    )

    if (!identical(
      names(estimates),
      c("BL", "BS", "HT", "FT")
    )) {
      stop("Unexpected method order.", call. = FALSE)
    }

    for (method in names(estimates)) {
      gamma_hat <- (estimates[[method]] + t(estimates[[method]])) / 2

      metric <- as.data.frame(
        helpers$matrix_metrics(
          gamma_hat = gamma_hat,
          gamma_true = gamma_true,
          prob_grid = spec$probability_grid,
          negative_tol = spec$negative_eigen_tolerance,
          near_singular_tol = spec$near_singular_tolerance
        ),
        stringsAsFactors = FALSE
      )

      if (nrow(metric) != 1L) {
        stop("matrix_metrics did not return one row.", call. = FALSE)
      }

      row_counter <- row_counter + 1L

      metric <- cbind(
        task[
          ,
          c(
            "task_id",
            "task_key",
            "scenario_index",
            "scenario_id",
            "scenario_label",
            "scenario_family",
            "generator",
            "parameter_1",
            "parameter_2",
            "sample_size_index",
            "sample_size",
            "replication",
            "rng_stream_index",
            "rng_kind",
            "rng_stream_sha256",
            "rng_probe_sha256",
            "design_version"
          ),
          drop = FALSE
        ],
        data.frame(
          method_code = method,
          bandwidth = bandwidth,
          selected_bandwidth = selected_bandwidth,
          is_selected_bandwidth = bandwidth == selected_bandwidth,
          pilot_bandwidth = selected$pilot_bandwidth,
          sample_sha256 = sample_sha256,
          truth_sha256 = truth$truth_sha256,
          truth_type = truth$truth_type,
          n_lags_truth = truth$n_lags_truth,
          stringsAsFactors = FALSE
        ),
        metric
      )

      metrics_rows[[row_counter]] <- metric

      if (bandwidth == selected_bandwidth) {
        selected_matrices[[method]] <- gamma_hat
      }
    }

    gap <- as.data.frame(
      helpers$gap_metrics(
        gamma_sandwich = estimates$BS,
        gamma_lagwindow = estimates$BL,
        gamma_true = gamma_true,
        prob_grid = spec$probability_grid
      ),
      stringsAsFactors = FALSE
    )

    if (nrow(gap) != 1L) {
      stop("gap_metrics did not return one row.", call. = FALSE)
    }

    gap_counter <- gap_counter + 1L

    gap_rows[[gap_counter]] <- cbind(
      task[
        ,
        c(
          "task_id",
          "task_key",
          "scenario_id",
          "sample_size",
          "replication",
          "design_version"
        ),
        drop = FALSE
      ],
      data.frame(
        bandwidth = bandwidth,
        selected_bandwidth = selected_bandwidth,
        is_selected_bandwidth = bandwidth == selected_bandwidth,
        sample_sha256 = sample_sha256,
        truth_sha256 = truth$truth_sha256,
        stringsAsFactors = FALSE
      ),
      gap
    )
  }

  metrics <- do.call(rbind, metrics_rows)
  gap_metrics <- do.call(rbind, gap_rows)

  if (
    nrow(metrics) !=
      length(bandwidth_grid) * 4L
  ) {
    stop(
      "Unexpected number of metric rows.",
      call. = FALSE
    )
  }

  if (
    nrow(gap_metrics) !=
      length(bandwidth_grid)
  ) {
    stop(
      "Unexpected number of gap rows.",
      call. = FALSE
    )
  }

  if (!all(c("BL", "BS", "HT", "FT") %in% names(selected_matrices))) {
    stop(
      "Selected-bandwidth matrices were not retained for all methods.",
      call. = FALSE
    )
  }

  selected_matrix_hashes <- data.frame(
    task_id = task$task_id,
    method_code = names(selected_matrices),
    selected_bandwidth = selected_bandwidth,
    matrix_sha256 = vapply(
      selected_matrices,
      object_sha256,
      character(1L)
    ),
    stringsAsFactors = FALSE
  )

  timing <- data.frame(
    task_id = task$task_id,
    task_key = task$task_key,
    scenario_id = task$scenario_id,
    sample_size = task$sample_size,
    replication = task$replication,
    bandwidth_grid_size = length(bandwidth_grid),
    selected_bandwidth = selected_bandwidth,
    started_utc = format(
      started,
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    finished_utc = format(
      Sys.time(),
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    elapsed_seconds = as.numeric(
      difftime(Sys.time(), started, units = "secs")
    ),
    stringsAsFactors = FALSE
  )

  write_csv(
    metrics,
    file.path(temporary_dir, "metrics_all_bandwidths.csv")
  )

  write_csv(
    metrics[metrics$is_selected_bandwidth, , drop = FALSE],
    file.path(temporary_dir, "metrics_selected_bandwidth.csv")
  )

  write_csv(
    gap_metrics,
    file.path(temporary_dir, "gap_all_bandwidths.csv")
  )

  write_csv(
    gap_metrics[gap_metrics$is_selected_bandwidth, , drop = FALSE],
    file.path(temporary_dir, "gap_selected_bandwidth.csv")
  )

  write_csv(
    selected_matrix_hashes,
    file.path(temporary_dir, "selected_matrix_hashes.csv")
  )

  write_csv(
    timing,
    file.path(temporary_dir, "timing.csv")
  )

  saveRDS(
    list(
      task = task,
      bandwidth_grid = bandwidth_grid,
      selected_bandwidth = selected_bandwidth,
      selected_matrices = selected_matrices,
      gamma_true = gamma_true,
      sample_sha256 = sample_sha256,
      truth_sha256 = truth$truth_sha256
    ),
    file.path(temporary_dir, "task_object.rds"),
    version = 3,
    compress = "xz"
  )

  artifact_files <- c(
    "metrics_all_bandwidths.csv",
    "metrics_selected_bandwidth.csv",
    "gap_all_bandwidths.csv",
    "gap_selected_bandwidth.csv",
    "selected_matrix_hashes.csv",
    "timing.csv",
    "task_object.rds"
  )

  artifact_hashes <- data.frame(
    file = artifact_files,
    bytes = file.info(
      file.path(temporary_dir, artifact_files)
    )$size,
    sha256 = vapply(
      file.path(temporary_dir, artifact_files),
      file_sha256,
      character(1L)
    ),
    stringsAsFactors = FALSE
  )

  write_csv(
    artifact_hashes,
    file.path(temporary_dir, "artifact_sha256.csv")
  )

  writeLines(
    c(
      "TASK_STATUS=PASS",
      paste0("TASK_ID=", task$task_id),
      paste0("TASK_KEY=", task$task_key),
      paste0("SCENARIO_ID=", task$scenario_id),
      paste0("SAMPLE_SIZE=", task$sample_size),
      paste0("REPLICATION=", task$replication),
      paste0("METHODS=BL,BS,HT,FT"),
      paste0("BANDWIDTH_GRID_SIZE=", length(bandwidth_grid)),
      paste0("SELECTED_BANDWIDTH=", selected_bandwidth),
      paste0("SAMPLE_SHA256=", sample_sha256),
      paste0("TRUTH_SHA256=", truth$truth_sha256),
      paste0("ELAPSED_SECONDS=", timing$elapsed_seconds)
    ),
    con = file.path(temporary_dir, "task_status.txt")
  )

  if (!file.rename(
    from = temporary_dir,
    to = final_dir
  )) {
    stop(
      "Atomic rename of the task directory failed.",
      call. = FALSE
    )
  }

  completed <- TRUE

  cat(
    "TASK_STATUS=PASS\n",
    "TASK_ID=", task$task_id, "\n",
    "TASK_KEY=", task$task_key, "\n",
    "ELAPSED_SECONDS=", timing$elapsed_seconds, "\n",
    sep = ""
  )
}

tryCatch(
  run_task(),
  error = function(error) {
    message(
      "TASK_STATUS=FAIL"
    )
    message(
      conditionMessage(error)
    )
    quit(
      save = "no",
      status = 1L
    )
  }
)
