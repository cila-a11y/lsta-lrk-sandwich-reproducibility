args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2L) {
  stop(
    paste(
      "Usage: materialize_pilot_design.R",
      "<work_repo> <design_dir>"
    ),
    call. = FALSE
  )
}

work_repo <- normalizePath(
  args[[1L]],
  mustWork = TRUE
)

design_dir <- normalizePath(
  args[[2L]],
  mustWork = FALSE
)

spec_file <- file.path(
  work_repo,
  "hpc",
  "unified",
  "study_spec.R"
)

if (!file.exists(spec_file)) {
  stop(
    "Study specification not found: ",
    spec_file,
    call. = FALSE
  )
}

source(
  spec_file,
  local = .GlobalEnv
)

required_functions <- c(
  "unified_study_spec",
  "unified_scenario_table",
  "unified_exact_flat_top_weights",
  "unified_make_rng_streams",
  "unified_set_rng_stream",
  "unified_build_task_table",
  "unified_expected_pilot_tasks",
  "unified_expected_final_tasks"
)

available_functions <- vapply(
  required_functions,
  function(name) {
    exists(
      name,
      mode = "function",
      inherits = TRUE
    )
  },
  logical(1L)
)

if (!all(available_functions)) {
  stop(
    "Missing specification functions: ",
    paste(
      required_functions[!available_functions],
      collapse = ", "
    ),
    call. = FALSE
  )
}

if (!requireNamespace("digest", quietly = TRUE)) {
  stop(
    "Package `digest` is required.",
    call. = FALSE
  )
}

write_csv <- function(object, path) {
  utils::write.table(
    object,
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
  serialized <- serialize(
    object,
    connection = NULL,
    version = 3
  )

  digest::digest(
    serialized,
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

rng_probe <- function(stream) {
  unified_set_rng_stream(stream)

  probe <- list(
    normals = stats::rnorm(8L),
    uniforms = stats::runif(8L),
    integers = sample.int(
      n = 1000000L,
      size = 8L,
      replace = FALSE
    )
  )

  list(
    values = probe,
    sha256 = object_sha256(probe)
  )
}

if (file.exists(design_dir)) {
  stop(
    "Design directory already exists: ",
    design_dir,
    call. = FALSE
  )
}

parent_dir <- dirname(design_dir)

dir.create(
  parent_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

temporary_dir <- paste0(
  design_dir,
  ".tmp.",
  Sys.getpid()
)

if (file.exists(temporary_dir)) {
  stop(
    "Temporary design directory already exists: ",
    temporary_dir,
    call. = FALSE
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
    unlink(
      temporary_dir,
      recursive = TRUE,
      force = TRUE
    )
  }
}, add = TRUE)

# ============================================================
# 1. Scientific specification
# ============================================================

spec <- unified_study_spec()
scenarios <- unified_scenario_table(spec)
methods <- spec$methods

expected_methods <- c(
  "BL",
  "BS",
  "HT",
  "FT"
)

if (!identical(
  methods$method_code,
  expected_methods
)) {
  stop(
    "Unexpected method order.",
    call. = FALSE
  )
}

if (
  sum(methods$psd_guaranteed) != 1L ||
  methods$method_code[methods$psd_guaranteed] != "BS"
) {
  stop(
    "The PSD-guarantee specification is invalid.",
    call. = FALSE
  )
}

if (!identical(
  as.integer(spec$sample_sizes),
  c(300L, 600L, 1200L)
)) {
  stop(
    "Unexpected sample sizes.",
    call. = FALSE
  )
}

if (spec$pilot_replications != 10L) {
  stop(
    "The pilot must contain 10 replications per cell.",
    call. = FALSE
  )
}

if (spec$final_replications != 500L) {
  stop(
    "The final design must contain 500 replications per cell.",
    call. = FALSE
  )
}

# Validate the exact flat-top definition.
flat_top_test <- unified_exact_flat_top_weights(
  bandwidth = 10L,
  max_lag = 14L
)

if (
  !all(flat_top_test[1:5] == 1) ||
  abs(flat_top_test[6] - 0.8) > 1e-15 ||
  abs(flat_top_test[9] - 0.2) > 1e-15 ||
  any(flat_top_test[10:14] != 0)
) {
  stop(
    "Exact flat-top weight validation failed.",
    call. = FALSE
  )
}

# ============================================================
# 2. Pilot manifest
# ============================================================

manifest <- unified_build_task_table(
  replications = spec$pilot_replications,
  spec = spec
)

expected_pilot_tasks <-
  unified_expected_pilot_tasks(spec)

expected_final_tasks <-
  unified_expected_final_tasks(spec)

if (expected_pilot_tasks != 90L) {
  stop(
    "Expected pilot-task count is not 90.",
    call. = FALSE
  )
}

if (expected_final_tasks != 4500L) {
  stop(
    "Expected final-task count is not 4500.",
    call. = FALSE
  )
}

if (nrow(manifest) != 90L) {
  stop(
    "Pilot manifest must contain exactly 90 rows.",
    call. = FALSE
  )
}

if (!identical(
  manifest$task_id,
  seq_len(90L)
)) {
  stop(
    "Task identifiers are not exactly 1,...,90.",
    call. = FALSE
  )
}

unique_checks <- c(
  task_id =
    anyDuplicated(manifest$task_id),
  task_key =
    anyDuplicated(manifest$task_key),
  rng_stream_index =
    anyDuplicated(manifest$rng_stream_index),
  output_relative_path =
    anyDuplicated(manifest$output_relative_path)
)

if (any(unique_checks != 0L)) {
  stop(
    "Manifest uniqueness failure: ",
    paste(
      names(unique_checks)[unique_checks != 0L],
      collapse = ", "
    ),
    call. = FALSE
  )
}

if (!identical(
  manifest$rng_stream_index,
  manifest$task_id
)) {
  stop(
    "RNG stream indices must equal task identifiers.",
    call. = FALSE
  )
}

cell_counts <- stats::aggregate(
  replication ~
    scenario_index +
    scenario_id +
    sample_size_index +
    sample_size,
  data = manifest,
  FUN = length
)

names(cell_counts)[
  names(cell_counts) == "replication"
] <- "n_replications"

if (
  nrow(cell_counts) != 9L ||
  any(cell_counts$n_replications != 10L)
) {
  stop(
    "Each of the nine cells must contain 10 replications.",
    call. = FALSE
  )
}

for (scenario_index in seq_len(3L)) {
  for (sample_size_index in seq_len(3L)) {
    subset_rows <- manifest[
      manifest$scenario_index == scenario_index &
        manifest$sample_size_index == sample_size_index,
      ,
      drop = FALSE
    ]

    if (!identical(
      subset_rows$replication,
      seq_len(10L)
    )) {
      stop(
        "Replication indices are incomplete in cell ",
        scenario_index,
        "/",
        sample_size_index,
        ".",
        call. = FALSE
      )
    }
  }
}

# ============================================================
# 3. Independent L'Ecuyer-CMRG streams
# ============================================================

rng_streams <- unified_make_rng_streams(
  n_streams = nrow(manifest),
  base_seed = spec$base_seed
)

if (length(rng_streams) != 90L) {
  stop(
    "Exactly 90 RNG streams are required.",
    call. = FALSE
  )
}

valid_streams <- vapply(
  rng_streams,
  function(stream) {
    is.integer(stream) &&
      length(stream) == 7L &&
      all(is.finite(stream))
  },
  logical(1L)
)

if (!all(valid_streams)) {
  stop(
    "One or more RNG streams are invalid.",
    call. = FALSE
  )
}

stream_hashes <- vapply(
  rng_streams,
  object_sha256,
  character(1L)
)

if (anyDuplicated(stream_hashes)) {
  stop(
    "Duplicate RNG streams were generated.",
    call. = FALSE
  )
}

rng_probes <- lapply(
  rng_streams,
  rng_probe
)

probe_hashes <- vapply(
  rng_probes,
  function(probe) probe$sha256,
  character(1L)
)

if (anyDuplicated(probe_hashes)) {
  stop(
    "Duplicate RNG probe sequences were generated.",
    call. = FALSE
  )
}

# Reproducibility within a stream.
probe_first_a <- rng_probe(rng_streams[[1L]])
probe_first_b <- rng_probe(rng_streams[[1L]])

if (!identical(
  probe_first_a$values,
  probe_first_b$values
)) {
  stop(
    "Repeated use of the first RNG stream is not reproducible.",
    call. = FALSE
  )
}

# Order independence: the task-specific result must not depend on
# the order in which streams are evaluated.
order_test_ids <- c(
  1L,
  10L,
  37L,
  64L,
  90L
)

forward_hashes <- setNames(
  vapply(
    order_test_ids,
    function(task_id) {
      rng_probe(rng_streams[[task_id]])$sha256
    },
    character(1L)
  ),
  as.character(order_test_ids)
)

reverse_ids <- rev(order_test_ids)

reverse_hashes <- setNames(
  vapply(
    reverse_ids,
    function(task_id) {
      rng_probe(rng_streams[[task_id]])$sha256
    },
    character(1L)
  ),
  as.character(reverse_ids)
)

if (!identical(
  forward_hashes,
  reverse_hashes[names(forward_hashes)]
)) {
  stop(
    "RNG order-independence validation failed.",
    call. = FALSE
  )
}

manifest$rng_kind <- "L'Ecuyer-CMRG"
manifest$rng_normal_kind <- "Inversion"
manifest$rng_sample_kind <- "Rejection"
manifest$rng_stream_sha256 <- stream_hashes
manifest$rng_probe_sha256 <- probe_hashes

rng_stream_manifest <- data.frame(
  task_id = manifest$task_id,
  task_key = manifest$task_key,
  rng_stream_index = manifest$rng_stream_index,
  rng_stream_sha256 = stream_hashes,
  rng_probe_sha256 = probe_hashes,
  stringsAsFactors = FALSE
)

# ============================================================
# 4. Metadata
# ============================================================

git_commit <- system2(
  command = "git",
  args = c(
    "-C",
    work_repo,
    "rev-parse",
    "HEAD"
  ),
  stdout = TRUE,
  stderr = TRUE
)

if (
  length(git_commit) != 1L ||
  !grepl("^[0-9a-f]{40}$", git_commit)
) {
  stop(
    "Unable to determine the current Git commit.",
    call. = FALSE
  )
}

git_status <- system2(
  command = "git",
  args = c(
    "-C",
    work_repo,
    "status",
    "--porcelain=v1"
  ),
  stdout = TRUE,
  stderr = TRUE
)

spec_sha256 <- file_sha256(spec_file)

design_metadata <- data.frame(
  design_version = spec$design_version,
  git_commit = git_commit,
  git_worktree_clean = length(git_status) == 0L,
  study_spec_sha256 = spec_sha256,
  base_seed = spec$base_seed,
  rng_kind = "L'Ecuyer-CMRG",
  rng_normal_kind = "Inversion",
  rng_sample_kind = "Rejection",
  n_scenarios = nrow(scenarios),
  n_sample_sizes = length(spec$sample_sizes),
  pilot_replications_per_cell =
    spec$pilot_replications,
  final_replications_per_cell =
    spec$final_replications,
  n_pilot_tasks = nrow(manifest),
  n_final_tasks = expected_final_tasks,
  n_rng_streams = length(rng_streams),
  n_methods = nrow(methods),
  probability_grid_size =
    length(spec$probability_grid),
  max_bandwidth_cap =
    spec$max_bandwidth_cap,
  polynomial_beta =
    spec$polynomial_beta,
  polynomial_filter_length =
    spec$polynomial_filter_length,
  stringsAsFactors = FALSE
)

# ============================================================
# 5. Write requested artifacts
# ============================================================

pilot_manifest_csv <- file.path(
  temporary_dir,
  "pilot_manifest.csv"
)

pilot_manifest_rds <- file.path(
  temporary_dir,
  "pilot_manifest.rds"
)

rng_streams_rds <- file.path(
  temporary_dir,
  "rng_streams.rds"
)

method_table_csv <- file.path(
  temporary_dir,
  "method_table.csv"
)

scenario_table_csv <- file.path(
  temporary_dir,
  "scenario_table.csv"
)

study_spec_rds <- file.path(
  temporary_dir,
  "study_spec.rds"
)

write_csv(
  manifest,
  pilot_manifest_csv
)

saveRDS(
  manifest,
  pilot_manifest_rds,
  version = 3,
  compress = "xz"
)

saveRDS(
  rng_streams,
  rng_streams_rds,
  version = 3,
  compress = "xz"
)

write_csv(
  methods,
  method_table_csv
)

write_csv(
  scenarios,
  scenario_table_csv
)

saveRDS(
  spec,
  study_spec_rds,
  version = 3,
  compress = "xz"
)

# Additional validation artifacts.
write_csv(
  rng_stream_manifest,
  file.path(
    temporary_dir,
    "rng_stream_manifest.csv"
  )
)

write_csv(
  cell_counts,
  file.path(
    temporary_dir,
    "pilot_cell_counts.csv"
  )
)

write_csv(
  design_metadata,
  file.path(
    temporary_dir,
    "design_metadata.csv"
  )
)

write_csv(
  data.frame(
    task_id = as.integer(names(forward_hashes)),
    forward_probe_sha256 =
      unname(forward_hashes),
    reverse_probe_sha256 =
      unname(
        reverse_hashes[
          names(forward_hashes)
        ]
      ),
    identical =
      unname(forward_hashes) ==
      unname(
        reverse_hashes[
          names(forward_hashes)
        ]
      ),
    stringsAsFactors = FALSE
  ),
  file.path(
    temporary_dir,
    "rng_order_independence_check.csv"
  )
)

writeLines(
  git_status,
  con = file.path(
    temporary_dir,
    "git_status_at_materialization.txt"
  )
)

utils::capture.output(
  sessionInfo(),
  file = file.path(
    temporary_dir,
    "sessionInfo.txt"
  )
)

# ============================================================
# 6. Read-back validation
# ============================================================

manifest_rds_check <- readRDS(
  pilot_manifest_rds
)

streams_rds_check <- readRDS(
  rng_streams_rds
)

spec_rds_check <- readRDS(
  study_spec_rds
)

manifest_csv_check <- utils::read.csv(
  pilot_manifest_csv,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

if (!identical(
  manifest_rds_check,
  manifest
)) {
  stop(
    "pilot_manifest.rds failed its round-trip check.",
    call. = FALSE
  )
}

if (!identical(
  streams_rds_check,
  rng_streams
)) {
  stop(
    "rng_streams.rds failed its round-trip check.",
    call. = FALSE
  )
}

if (!identical(
  spec_rds_check,
  spec
)) {
  stop(
    "study_spec.rds failed its round-trip check.",
    call. = FALSE
  )
}

if (
  nrow(manifest_csv_check) != 90L ||
  !identical(
    manifest_csv_check$task_key,
    manifest$task_key
  ) ||
  !identical(
    manifest_csv_check$rng_stream_sha256,
    manifest$rng_stream_sha256
  )
) {
  stop(
    "pilot_manifest.csv failed its round-trip check.",
    call. = FALSE
  )
}

# ============================================================
# 7. File hashes and final status
# ============================================================

requested_files <- c(
  "pilot_manifest.csv",
  "pilot_manifest.rds",
  "rng_streams.rds",
  "method_table.csv",
  "scenario_table.csv",
  "study_spec.rds"
)

additional_files <- c(
  "rng_stream_manifest.csv",
  "pilot_cell_counts.csv",
  "design_metadata.csv",
  "rng_order_independence_check.csv",
  "git_status_at_materialization.txt",
  "sessionInfo.txt"
)

all_artifacts <- c(
  requested_files,
  additional_files
)

missing_artifacts <- all_artifacts[
  !file.exists(
    file.path(
      temporary_dir,
      all_artifacts
    )
  )
]

if (length(missing_artifacts) > 0L) {
  stop(
    "Missing design artifacts: ",
    paste(
      missing_artifacts,
      collapse = ", "
    ),
    call. = FALSE
  )
}

artifact_hashes <- data.frame(
  file = all_artifacts,
  bytes = file.info(
    file.path(
      temporary_dir,
      all_artifacts
    )
  )$size,
  sha256 = vapply(
    file.path(
      temporary_dir,
      all_artifacts
    ),
    file_sha256,
    character(1L)
  ),
  stringsAsFactors = FALSE
)

if (any(
  !is.finite(artifact_hashes$bytes) |
    artifact_hashes$bytes <= 0
)) {
  stop(
    "One or more design artifacts are empty.",
    call. = FALSE
  )
}

write_csv(
  artifact_hashes,
  file.path(
    temporary_dir,
    "design_artifact_sha256.csv"
  )
)

writeLines(
  c(
    "PILOT_DESIGN_STATUS=PASS",
    paste0(
      "DESIGN_VERSION=",
      spec$design_version
    ),
    paste0(
      "PILOT_TASKS=",
      nrow(manifest)
    ),
    paste0(
      "EXPECTED_PILOT_TASKS=",
      expected_pilot_tasks
    ),
    paste0(
      "FINAL_TASKS=",
      expected_final_tasks
    ),
    paste0(
      "RNG_STREAMS=",
      length(rng_streams)
    ),
    paste0(
      "UNIQUE_RNG_STREAMS=",
      length(unique(stream_hashes))
    ),
    paste0(
      "METHODS=",
      paste(
        methods$method_code,
        collapse = ","
      )
    ),
    "RNG_REPEATED_STREAM_CHECK=PASS",
    "RNG_ORDER_INDEPENDENCE_CHECK=PASS",
    "RDS_ROUNDTRIP_CHECK=PASS",
    "CSV_ROUNDTRIP_CHECK=PASS",
    paste0(
      "GIT_COMMIT=",
      git_commit
    ),
    paste0(
      "STUDY_SPEC_SHA256=",
      spec_sha256
    )
  ),
  con = file.path(
    temporary_dir,
    "design_status.txt"
  )
)

# Rename only after all validations pass.
if (!file.rename(
  from = temporary_dir,
  to = design_dir
)) {
  stop(
    "Atomic rename to the final design directory failed.",
    call. = FALSE
  )
}

completed <- TRUE

cat(
  "============================================================\n",
  "PILOT_DESIGN_STATUS=PASS\n",
  "PILOT_TASKS=", nrow(manifest), "\n",
  "RNG_STREAMS=", length(rng_streams), "\n",
  "METHODS=",
  paste(methods$method_code, collapse = ","),
  "\n",
  "DESIGN_DIR=", design_dir, "\n",
  "============================================================\n",
  sep = ""
)
