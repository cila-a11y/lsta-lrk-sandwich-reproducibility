args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 4L) {
  stop(
    paste(
      "Usage: aggregate_unified_pilot.R",
      "<design_dir> <task_root> <agg_dir> <worker_log_root>"
    ),
    call. = FALSE
  )
}

design_dir <- normalizePath(args[[1L]], mustWork = TRUE)
task_root <- normalizePath(args[[2L]], mustWork = TRUE)
agg_dir <- normalizePath(args[[3L]], mustWork = FALSE)
worker_log_root <- normalizePath(args[[4L]], mustWork = FALSE)

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package `digest` is required.", call. = FALSE)
}

if (file.exists(agg_dir)) {
  stop(
    "Aggregation directory already exists: ",
    agg_dir,
    call. = FALSE
  )
}

temporary_dir <- paste0(
  agg_dir,
  ".tmp.",
  Sys.getpid()
)

if (file.exists(temporary_dir)) {
  unlink(temporary_dir, recursive = TRUE, force = TRUE)
}

dir.create(temporary_dir, recursive = TRUE, showWarnings = FALSE)

completed <- FALSE

on.exit({
  if (!completed && file.exists(temporary_dir)) {
    unlink(temporary_dir, recursive = TRUE, force = TRUE)
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

file_sha256 <- function(path) {
  digest::digest(
    path,
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
}

safe_read_csv <- function(path) {
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
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

manifest <- readRDS(
  file.path(design_dir, "pilot_manifest.rds")
)

if (nrow(manifest) != 90L) {
  stop("Pilot manifest must contain 90 tasks.", call. = FALSE)
}

expected_methods <- c("BL", "BS", "HT", "FT")

task_status_rows <- list()
timing_rows <- list()
metrics_all_rows <- list()
metrics_selected_rows <- list()
gap_all_rows <- list()
gap_selected_rows <- list()
matrix_hash_rows <- list()
artifact_hash_rows <- list()

missing_dirs <- character()
missing_files <- list()
failed_tasks <- character()

required_task_files <- c(
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

for (index in seq_len(nrow(manifest))) {
  task <- manifest[index, ]
  task_dir <- file.path(task_root, task$output_relative_path)

  if (!dir.exists(task_dir)) {
    missing_dirs <- c(missing_dirs, task$task_key)
    next
  }

  absent <- required_task_files[
    !file.exists(file.path(task_dir, required_task_files))
  ]

  if (length(absent) > 0L) {
    missing_files[[task$task_key]] <- absent
    next
  }

  status <- read_status_file(file.path(task_dir, "task_status.txt"))

  if (!identical(status$TASK_STATUS, "PASS")) {
    failed_tasks <- c(failed_tasks, task$task_key)
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
    safe_read_csv(file.path(task_dir, "timing.csv"))

  metrics_all_rows[[length(metrics_all_rows) + 1L]] <-
    safe_read_csv(file.path(task_dir, "metrics_all_bandwidths.csv"))

  metrics_selected_rows[[length(metrics_selected_rows) + 1L]] <-
    safe_read_csv(file.path(task_dir, "metrics_selected_bandwidth.csv"))

  gap_all_rows[[length(gap_all_rows) + 1L]] <-
    safe_read_csv(file.path(task_dir, "gap_all_bandwidths.csv"))

  gap_selected_rows[[length(gap_selected_rows) + 1L]] <-
    safe_read_csv(file.path(task_dir, "gap_selected_bandwidth.csv"))

  matrix_hash_rows[[length(matrix_hash_rows) + 1L]] <-
    safe_read_csv(file.path(task_dir, "selected_matrix_hashes.csv"))

  artifact_table <- safe_read_csv(file.path(task_dir, "artifact_sha256.csv"))
  artifact_table$task_id <- task$task_id
  artifact_table$task_key <- task$task_key
  artifact_hash_rows[[length(artifact_hash_rows) + 1L]] <- artifact_table
}

if (length(missing_dirs) > 0L) {
  stop(
    "Missing task directories: ",
    paste(missing_dirs, collapse = ", "),
    call. = FALSE
  )
}

if (length(missing_files) > 0L) {
  stop(
    "Some task directories are incomplete.",
    call. = FALSE
  )
}

if (length(failed_tasks) > 0L) {
  stop(
    "Failed tasks: ",
    paste(failed_tasks, collapse = ", "),
    call. = FALSE
  )
}

task_status <- do.call(rbind, task_status_rows)
timing <- do.call(rbind, timing_rows)
metrics_all <- do.call(rbind, metrics_all_rows)
metrics_selected <- do.call(rbind, metrics_selected_rows)
gap_all <- do.call(rbind, gap_all_rows)
gap_selected <- do.call(rbind, gap_selected_rows)
matrix_hashes <- do.call(rbind, matrix_hash_rows)
artifact_hashes <- do.call(rbind, artifact_hash_rows)

# ------------------------------------------------------------------
# Structural validation
# ------------------------------------------------------------------

if (!identical(
  sort(task_status$task_id),
  seq_len(90L)
)) {
  stop("Task-status identifiers are incomplete.", call. = FALSE)
}

if (anyDuplicated(task_status$task_id)) {
  stop("Duplicate task identifiers in task status.", call. = FALSE)
}

if (anyDuplicated(task_status$task_key)) {
  stop("Duplicate task keys in task status.", call. = FALSE)
}

if (nrow(timing) != 90L) {
  stop("Timing table must have 90 rows.", call. = FALSE)
}

if (anyDuplicated(timing$task_id)) {
  stop("Duplicate task identifiers in timing table.", call. = FALSE)
}

if (nrow(metrics_selected) != 90L * 4L) {
  stop("Selected metrics must have 360 rows.", call. = FALSE)
}

if (nrow(gap_selected) != 90L) {
  stop("Selected gap table must have 90 rows.", call. = FALSE)
}

if (nrow(matrix_hashes) != 90L * 4L) {
  stop("Selected matrix-hash table must have 360 rows.", call. = FALSE)
}

task_method_sets <- stats::aggregate(
  method_code ~ task_id,
  data = metrics_selected,
  FUN = function(x) paste(sort(unique(x)), collapse = ",")
)

task_method_row_counts <- stats::aggregate(
  method_code ~ task_id,
  data = metrics_selected,
  FUN = length
)

expected_method_set <- paste(sort(expected_methods), collapse = ",")

if (
  !all(task_method_sets$method_code == expected_method_set) ||
  !all(task_method_row_counts$method_code == length(expected_methods))
) {
  stop(
    "Not every task has exactly the method set {BL, BS, HT, FT}.",
    call. = FALSE
  )
}

# Same sample/truth across all methods within each task.
sample_truth_counts <- stats::aggregate(
  cbind(sample_sha256, truth_sha256) ~ task_id,
  data = metrics_selected,
  FUN = function(x) length(unique(x))
)

if (any(sample_truth_counts$sample_sha256 != 1L) ||
    any(sample_truth_counts$truth_sha256 != 1L)) {
  stop("At least one task does not share sample/truth across methods.", call. = FALSE)
}

# BS must not have a negative minimum eigenvalue.
bs_selected <- metrics_selected[metrics_selected$method_code == "BS", ]

if (any(bs_selected$negative_min_eigen)) {
  stop("At least one Bartlett sandwich estimate is indefinite.", call. = FALSE)
}

if (any(bs_selected$min_eigenvalue < -1e-10)) {
  stop("At least one Bartlett sandwich minimum eigenvalue is below tolerance.", call. = FALSE)
}

# There should be exactly 10 replications per cell.
cell_counts <- stats::aggregate(
  replication ~ scenario_id + sample_size,
  data = task_status,
  FUN = length
)

names(cell_counts)[names(cell_counts) == "replication"] <- "n_tasks"

if (nrow(cell_counts) != 9L || any(cell_counts$n_tasks != 10L)) {
  stop("Pilot cell counts are not 10 in every cell.", call. = FALSE)
}

# ------------------------------------------------------------------
# Summaries
# ------------------------------------------------------------------

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
    max = max(x)
  )
)

selected_summary_flat <- do.call(
  data.frame,
  selected_summary
)

names(selected_summary_flat) <- make.names(names(selected_summary_flat), unique = TRUE)

negative_summary <- stats::aggregate(
  negative_min_eigen ~ scenario_id + scenario_label + sample_size + method_code,
  data = metrics_selected,
  FUN = mean
)

names(negative_summary)[names(negative_summary) == "negative_min_eigen"] <-
  "negative_eigen_frequency"

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

timing_summary_flat <- do.call(
  data.frame,
  timing_summary
)

names(timing_summary_flat) <- make.names(names(timing_summary_flat), unique = TRUE)

overall_mean_task_time <- mean(timing$elapsed_seconds)
overall_total_serial_for_4500 <- overall_mean_task_time * 4500

worker_counts <- c(90L, 120L, 128L)

sizing <- data.frame(
  worker_count = worker_counts,
  pilot_mean_task_seconds = overall_mean_task_time,
  final_tasks = 4500L,
  estimated_serial_seconds_4500 = overall_total_serial_for_4500,
  estimated_wall_seconds_no_overhead =
    overall_total_serial_for_4500 / worker_counts,
  estimated_wall_seconds_25pct_overhead =
    1.25 * overall_total_serial_for_4500 / worker_counts,
  estimated_wall_minutes_25pct_overhead =
    1.25 * overall_total_serial_for_4500 / worker_counts / 60,
  stringsAsFactors = FALSE
)

# Conservative cell-based sizing.
cell_mean <- stats::aggregate(
  elapsed_seconds ~ scenario_id + sample_size,
  data = timing,
  FUN = mean
)

cell_total_serial_final <- sum(cell_mean$elapsed_seconds * 500)

sizing$cell_based_serial_seconds_4500 <- cell_total_serial_final
sizing$cell_based_wall_minutes_25pct_overhead <-
  1.25 * cell_total_serial_final / sizing$worker_count / 60

# ------------------------------------------------------------------
# Write aggregated outputs
# ------------------------------------------------------------------

write_csv(task_status, file.path(temporary_dir, "pilot_task_status.csv"))
write_csv(timing, file.path(temporary_dir, "pilot_timing.csv"))
write_csv(metrics_all, file.path(temporary_dir, "pilot_metrics_all_bandwidths.csv"))
write_csv(metrics_selected, file.path(temporary_dir, "pilot_metrics_selected_bandwidth.csv"))
write_csv(gap_all, file.path(temporary_dir, "pilot_gap_all_bandwidths.csv"))
write_csv(gap_selected, file.path(temporary_dir, "pilot_gap_selected_bandwidth.csv"))
write_csv(matrix_hashes, file.path(temporary_dir, "pilot_selected_matrix_hashes.csv"))
write_csv(artifact_hashes, file.path(temporary_dir, "pilot_task_artifact_hashes.csv"))
write_csv(cell_counts, file.path(temporary_dir, "pilot_cell_counts_observed.csv"))
write_csv(selected_summary_flat, file.path(temporary_dir, "pilot_selected_summary.csv"))
write_csv(negative_summary, file.path(temporary_dir, "pilot_negative_eigen_summary.csv"))
write_csv(timing_summary_flat, file.path(temporary_dir, "pilot_timing_by_cell.csv"))
write_csv(sizing, file.path(temporary_dir, "pilot_final_job_sizing.csv"))

# Count worker logs.
worker_stdout <- list.files(worker_log_root, pattern = "\\.out$", full.names = TRUE)
worker_stderr <- list.files(worker_log_root, pattern = "\\.err$", full.names = TRUE)

worker_log_summary <- data.frame(
  stdout_files = length(worker_stdout),
  stderr_files = length(worker_stderr),
  nonempty_stderr_files = sum(file.info(worker_stderr)$size > 0),
  stringsAsFactors = FALSE
)

write_csv(worker_log_summary, file.path(temporary_dir, "pilot_worker_log_summary.csv"))

artifact_files <- list.files(temporary_dir, full.names = TRUE)
artifact_hash <- data.frame(
  file = basename(artifact_files),
  bytes = file.info(artifact_files)$size,
  sha256 = vapply(artifact_files, file_sha256, character(1L)),
  stringsAsFactors = FALSE
)

write_csv(artifact_hash, file.path(temporary_dir, "pilot_aggregate_sha256.csv"))

writeLines(
  c(
    "PILOT_AGGREGATION_STATUS=PASS",
    paste0("TASKS_OBSERVED=", nrow(task_status)),
    "TASKS_EXPECTED=90",
    paste0("METRICS_SELECTED_ROWS=", nrow(metrics_selected)),
    "METRICS_SELECTED_ROWS_EXPECTED=360",
    paste0("GAP_SELECTED_ROWS=", nrow(gap_selected)),
    "GAP_SELECTED_ROWS_EXPECTED=90",
    paste0("BS_NEGATIVE_EIGEN_COUNT=", sum(bs_selected$negative_min_eigen)),
    paste0("MEAN_TASK_SECONDS=", format(overall_mean_task_time, digits = 8)),
    paste0("MAX_TASK_SECONDS=", format(max(timing$elapsed_seconds), digits = 8)),
    "METHODS=BL,BS,HT,FT"
  ),
  con = file.path(temporary_dir, "pilot_aggregation_status.txt")
)

if (!file.rename(temporary_dir, agg_dir)) {
  stop("Atomic rename of aggregation directory failed.", call. = FALSE)
}

completed <- TRUE

cat("PILOT_AGGREGATION_STATUS=PASS\n")
cat("TASKS_OBSERVED=", nrow(task_status), "\n", sep = "")
cat("METRICS_SELECTED_ROWS=", nrow(metrics_selected), "\n", sep = "")
cat("MEAN_TASK_SECONDS=", overall_mean_task_time, "\n", sep = "")
