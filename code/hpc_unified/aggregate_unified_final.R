args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 4L) {
  stop(
    "Usage: aggregate_unified_final.R <design_dir> <task_root> <agg_dir> <worker_log_root>",
    call. = FALSE
  )
}

design_dir <- normalizePath(args[[1L]], mustWork = TRUE)
task_root <- normalizePath(args[[2L]], mustWork = TRUE)
agg_dir <- normalizePath(args[[3L]], mustWork = FALSE)
worker_log_root <- normalizePath(args[[4L]], mustWork = TRUE)

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package `digest` is required.", call. = FALSE)
}

if (file.exists(agg_dir)) {
  stop("Aggregation directory already exists: ", agg_dir, call. = FALSE)
}

tmp_dir <- paste0(agg_dir, ".tmp.", Sys.getpid())
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

completed <- FALSE
on.exit({
  if (!completed && file.exists(tmp_dir)) {
    unlink(tmp_dir, recursive = TRUE, force = TRUE)
  }
}, add = TRUE)

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

read_csv <- function(path) {
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
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

read_status_file <- function(path) {
  lines <- readLines(path, warn = FALSE)
  split <- strsplit(lines, "=", fixed = TRUE)
  keys <- vapply(split, `[`, character(1L), 1L)
  values <- vapply(
    split,
    function(x) paste(x[-1L], collapse = "="),
    character(1L)
  )
  out <- as.list(values)
  names(out) <- keys
  out
}

manifest <- readRDS(file.path(design_dir, "task_manifest.rds"))

if (nrow(manifest) != 4500L) {
  stop("Final manifest must contain 4500 tasks.", call. = FALSE)
}

required_files <- c(
  "task_status.txt",
  "metrics_all_bandwidths.csv",
  "metrics_selected_bandwidth.csv",
  "gap_all_bandwidths.csv",
  "gap_selected_bandwidth.csv",
  "selected_matrix_hashes.csv",
  "timing.csv",
  "artifact_sha256.csv",
  "task_object.rds"
)

task_status_rows <- list()
timing_rows <- list()
metrics_all_rows <- list()
metrics_selected_rows <- list()
gap_all_rows <- list()
gap_selected_rows <- list()
matrix_hash_rows <- list()

missing <- character()
failed <- character()

for (i in seq_len(nrow(manifest))) {
  task <- manifest[i, ]
  task_dir <- file.path(task_root, task$output_relative_path)

  if (!dir.exists(task_dir)) {
    missing <- c(missing, task$task_key)
    next
  }

  absent <- required_files[
    !file.exists(file.path(task_dir, required_files))
  ]

  if (length(absent) > 0L) {
    missing <- c(missing, paste0(task$task_key, ":", paste(absent, collapse = "|")))
    next
  }

  status <- read_status_file(file.path(task_dir, "task_status.txt"))

  if (!identical(status$TASK_STATUS, "PASS")) {
    failed <- c(failed, task$task_key)
  }

  task_status_rows[[length(task_status_rows) + 1L]] <- data.frame(
    task_id = as.integer(status$TASK_ID),
    task_key = status$TASK_KEY,
    scenario_id = status$SCENARIO_ID,
    sample_size = as.integer(status$SAMPLE_SIZE),
    replication = as.integer(status$REPLICATION),
    selected_bandwidth = as.integer(status$SELECTED_BANDWIDTH),
    sample_sha256 = status$SAMPLE_SHA256,
    truth_sha256 = status$TRUTH_SHA256,
    elapsed_seconds_status = as.numeric(status$ELAPSED_SECONDS),
    stringsAsFactors = FALSE
  )

  timing_rows[[length(timing_rows) + 1L]] <-
    read_csv(file.path(task_dir, "timing.csv"))

  metrics_all_rows[[length(metrics_all_rows) + 1L]] <-
    read_csv(file.path(task_dir, "metrics_all_bandwidths.csv"))

  metrics_selected_rows[[length(metrics_selected_rows) + 1L]] <-
    read_csv(file.path(task_dir, "metrics_selected_bandwidth.csv"))

  gap_all_rows[[length(gap_all_rows) + 1L]] <-
    read_csv(file.path(task_dir, "gap_all_bandwidths.csv"))

  gap_selected_rows[[length(gap_selected_rows) + 1L]] <-
    read_csv(file.path(task_dir, "gap_selected_bandwidth.csv"))

  matrix_hash_rows[[length(matrix_hash_rows) + 1L]] <-
    read_csv(file.path(task_dir, "selected_matrix_hashes.csv"))
}

if (length(missing) > 0L) {
  writeLines(missing, file.path(tmp_dir, "missing_or_incomplete_tasks.txt"))
  stop("Missing or incomplete final tasks.", call. = FALSE)
}

if (length(failed) > 0L) {
  writeLines(failed, file.path(tmp_dir, "failed_tasks.txt"))
  stop("Failed final tasks.", call. = FALSE)
}

task_status <- do.call(rbind, task_status_rows)
timing <- do.call(rbind, timing_rows)
metrics_all <- do.call(rbind, metrics_all_rows)
metrics_selected <- do.call(rbind, metrics_selected_rows)
gap_all <- do.call(rbind, gap_all_rows)
gap_selected <- do.call(rbind, gap_selected_rows)
matrix_hashes <- do.call(rbind, matrix_hash_rows)

expected_methods <- c("BL", "BS", "HT", "FT")
expected_method_set <- paste(sort(expected_methods), collapse = ",")

if (!identical(sort(task_status$task_id), seq_len(4500L))) {
  stop("Final task identifiers are incomplete.", call. = FALSE)
}

if (nrow(timing) != 4500L) {
  stop("Timing table must have 4500 rows.", call. = FALSE)
}

if (nrow(metrics_selected) != 4500L * 4L) {
  stop("Selected metrics must have 18000 rows.", call. = FALSE)
}

if (nrow(gap_selected) != 4500L) {
  stop("Selected gap table must have 4500 rows.", call. = FALSE)
}

method_sets <- stats::aggregate(
  method_code ~ task_id,
  data = metrics_selected,
  FUN = function(x) paste(sort(unique(x)), collapse = ",")
)

method_counts <- stats::aggregate(
  method_code ~ task_id,
  data = metrics_selected,
  FUN = length
)

if (
  !all(method_sets$method_code == expected_method_set) ||
  !all(method_counts$method_code == 4L)
) {
  stop("Not every final task has exactly {BL, BS, HT, FT}.", call. = FALSE)
}

sample_truth_counts <- stats::aggregate(
  cbind(sample_sha256, truth_sha256) ~ task_id,
  data = metrics_selected,
  FUN = function(x) length(unique(x))
)

if (
  any(sample_truth_counts$sample_sha256 != 1L) ||
  any(sample_truth_counts$truth_sha256 != 1L)
) {
  stop("Sample/truth hashes are not constant within tasks.", call. = FALSE)
}

bs_selected <- metrics_selected[metrics_selected$method_code == "BS", ]

if (any(bs_selected$negative_min_eigen)) {
  stop("At least one BS matrix is indefinite.", call. = FALSE)
}

cell_counts <- stats::aggregate(
  replication ~ scenario_id + sample_size,
  data = task_status,
  FUN = length
)

names(cell_counts)[names(cell_counts) == "replication"] <- "n_tasks"

if (nrow(cell_counts) != 9L || any(cell_counts$n_tasks != 500L)) {
  stop("Each final cell must contain exactly 500 tasks.", call. = FALSE)
}

negative_summary <- stats::aggregate(
  negative_min_eigen ~ scenario_id + scenario_label + sample_size + method_code,
  data = metrics_selected,
  FUN = mean
)

names(negative_summary)[names(negative_summary) == "negative_min_eigen"] <-
  "negative_eigen_frequency"

selected_summary <- stats::aggregate(
  cbind(
    relative_frobenius_error,
    frobenius_error,
    spectral_error,
    trace,
    min_eigenvalue,
    diagonal_inflation_90
  ) ~ scenario_id + scenario_label + scenario_family + sample_size + method_code,
  data = metrics_selected,
  FUN = function(x) c(
    mean = mean(x),
    sd = stats::sd(x),
    min = min(x),
    median = stats::median(x),
    q90 = as.numeric(stats::quantile(x, 0.90, names = FALSE)),
    q95 = as.numeric(stats::quantile(x, 0.95, names = FALSE)),
    max = max(x)
  )
)

selected_summary_flat <- do.call(data.frame, selected_summary)
names(selected_summary_flat) <- make.names(names(selected_summary_flat), unique = TRUE)

timing_summary <- stats::aggregate(
  elapsed_seconds ~ scenario_id + sample_size,
  data = timing,
  FUN = function(x) c(
    n_tasks = length(x),
    mean = mean(x),
    sd = stats::sd(x),
    min = min(x),
    median = stats::median(x),
    p90 = as.numeric(stats::quantile(x, 0.90, names = FALSE)),
    p95 = as.numeric(stats::quantile(x, 0.95, names = FALSE)),
    max = max(x),
    total_serial = sum(x)
  )
)

timing_summary_flat <- do.call(data.frame, timing_summary)
names(timing_summary_flat) <- make.names(names(timing_summary_flat), unique = TRUE)

worker_status_files <- list.files(
  worker_log_root,
  pattern = "^worker_[0-9]{3}\\.status$",
  full.names = TRUE
)

worker_status <- do.call(
  rbind,
  lapply(worker_status_files, function(path) {
    x <- read_status_file(path)
    data.frame(
      worker_id = as.integer(x$WORKER_ID),
      worker_status = x$WORKER_STATUS,
      tasks_done = as.integer(x$TASKS_DONE),
      tasks_skipped = as.integer(x$TASKS_SKIPPED),
      stringsAsFactors = FALSE
    )
  })
)

if (nrow(worker_status) != 128L || any(worker_status$worker_status != "PASS")) {
  stop("Not all final workers passed.", call. = FALSE)
}

worker_stderr_files <- list.files(
  worker_log_root,
  pattern = "^task_[0-9]{4}\\.err$",
  full.names = TRUE
)

worker_log_summary <- data.frame(
  worker_status_files = length(worker_status_files),
  task_stdout_files = length(list.files(worker_log_root, pattern = "^task_[0-9]{4}\\.out$")),
  task_stderr_files = length(worker_stderr_files),
  nonempty_task_stderr_files = sum(file.info(worker_stderr_files)$size > 0),
  stringsAsFactors = FALSE
)

write_csv(task_status, file.path(tmp_dir, "final_task_status.csv"))
write_csv(timing, file.path(tmp_dir, "final_timing.csv"))
write_csv(metrics_all, file.path(tmp_dir, "final_metrics_all_bandwidths.csv"))
write_csv(metrics_selected, file.path(tmp_dir, "final_metrics_selected_bandwidth.csv"))
write_csv(gap_all, file.path(tmp_dir, "final_gap_all_bandwidths.csv"))
write_csv(gap_selected, file.path(tmp_dir, "final_gap_selected_bandwidth.csv"))
write_csv(matrix_hashes, file.path(tmp_dir, "final_selected_matrix_hashes.csv"))
write_csv(cell_counts, file.path(tmp_dir, "final_cell_counts_observed.csv"))
write_csv(negative_summary, file.path(tmp_dir, "final_negative_eigen_summary.csv"))
write_csv(selected_summary_flat, file.path(tmp_dir, "final_selected_summary.csv"))
write_csv(timing_summary_flat, file.path(tmp_dir, "final_timing_by_cell.csv"))
write_csv(worker_status, file.path(tmp_dir, "final_worker_status.csv"))
write_csv(worker_log_summary, file.path(tmp_dir, "final_worker_log_summary.csv"))

utils::capture.output(
  sessionInfo(),
  file = file.path(tmp_dir, "sessionInfo.txt")
)

files <- list.files(tmp_dir, full.names = TRUE)

hashes <- data.frame(
  file = basename(files),
  bytes = file.info(files)$size,
  sha256 = vapply(files, file_sha256, character(1L)),
  stringsAsFactors = FALSE
)

write_csv(hashes, file.path(tmp_dir, "final_aggregate_sha256.csv"))

writeLines(
  c(
    "FINAL_AGGREGATION_STATUS=PASS",
    paste0("TASKS_OBSERVED=", nrow(task_status)),
    "TASKS_EXPECTED=4500",
    paste0("METRICS_SELECTED_ROWS=", nrow(metrics_selected)),
    "METRICS_SELECTED_ROWS_EXPECTED=18000",
    paste0("GAP_SELECTED_ROWS=", nrow(gap_selected)),
    "GAP_SELECTED_ROWS_EXPECTED=4500",
    paste0("BS_NEGATIVE_EIGEN_COUNT=", sum(bs_selected$negative_min_eigen)),
    paste0("WORKERS_OBSERVED=", nrow(worker_status)),
    "WORKERS_EXPECTED=128",
    paste0("MEAN_TASK_SECONDS=", format(mean(timing$elapsed_seconds), digits = 8)),
    paste0("MAX_TASK_SECONDS=", format(max(timing$elapsed_seconds), digits = 8)),
    "METHODS=BL,BS,HT,FT"
  ),
  con = file.path(tmp_dir, "final_aggregation_status.txt")
)

if (!file.rename(tmp_dir, agg_dir)) {
  stop("Atomic rename of final aggregation directory failed.", call. = FALSE)
}

completed <- TRUE

cat("FINAL_AGGREGATION_STATUS=PASS\n")
cat("TASKS_OBSERVED=", nrow(task_status), "\n", sep = "")
cat("METRICS_SELECTED_ROWS=", nrow(metrics_selected), "\n", sep = "")
cat("BS_NEGATIVE_EIGEN_COUNT=", sum(bs_selected$negative_min_eigen), "\n", sep = "")
