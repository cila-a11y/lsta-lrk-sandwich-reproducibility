args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2L) {
  stop(
    paste(
      "Usage: materialize_final_design.R",
      "<work_repo> <final_design_dir>"
    ),
    call. = FALSE
  )
}

work_repo <- normalizePath(args[[1L]], mustWork = TRUE)
design_dir <- normalizePath(args[[2L]], mustWork = FALSE)

source(
  file.path(work_repo, "hpc", "unified", "study_spec.R"),
  local = .GlobalEnv
)

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package `digest` is required.", call. = FALSE)
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

file_sha256 <- function(path) {
  digest::digest(
    path,
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
}

object_sha256 <- function(object) {
  digest::digest(
    serialize(object, NULL, version = 3),
    algo = "sha256",
    serialize = FALSE
  )
}

rng_probe <- function(stream) {
  unified_set_rng_stream(stream)

  probe <- list(
    normals = stats::rnorm(4L),
    uniforms = stats::runif(4L),
    integers = sample.int(1000000L, 4L)
  )

  object_sha256(probe)
}

if (file.exists(design_dir)) {
  stop(
    "Final design directory already exists: ",
    design_dir,
    call. = FALSE
  )
}

tmp_dir <- paste0(design_dir, ".tmp.", Sys.getpid())
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

completed <- FALSE
on.exit({
  if (!completed && file.exists(tmp_dir)) {
    unlink(tmp_dir, recursive = TRUE, force = TRUE)
  }
}, add = TRUE)

spec <- unified_study_spec()
scenarios <- unified_scenario_table(spec)
methods <- spec$methods

if (!identical(methods$method_code, c("BL", "BS", "HT", "FT"))) {
  stop("Unexpected method order.", call. = FALSE)
}

if (spec$final_replications != 500L) {
  stop("Final design must use 500 replications per cell.", call. = FALSE)
}

manifest <- unified_build_task_table(
  replications = spec$final_replications,
  spec = spec
)

expected_final_tasks <- unified_expected_final_tasks(spec)

if (expected_final_tasks != 4500L) {
  stop("Expected final task count is not 4500.", call. = FALSE)
}

if (nrow(manifest) != 4500L) {
  stop("Final manifest must contain exactly 4500 rows.", call. = FALSE)
}

if (!identical(manifest$task_id, seq_len(4500L))) {
  stop("Task IDs must be exactly 1,...,4500.", call. = FALSE)
}

if (anyDuplicated(manifest$task_key) ||
    anyDuplicated(manifest$output_relative_path) ||
    anyDuplicated(manifest$rng_stream_index)) {
  stop("Duplicate keys/paths/streams in final manifest.", call. = FALSE)
}

cell_counts <- stats::aggregate(
  replication ~ scenario_id + sample_size,
  data = manifest,
  FUN = length
)

names(cell_counts)[names(cell_counts) == "replication"] <- "n_replications"

if (nrow(cell_counts) != 9L || any(cell_counts$n_replications != 500L)) {
  stop("Each final cell must contain exactly 500 replications.", call. = FALSE)
}

rng_streams <- unified_make_rng_streams(
  n_streams = nrow(manifest),
  base_seed = spec$base_seed
)

if (length(rng_streams) != 4500L) {
  stop("Exactly 4500 RNG streams are required.", call. = FALSE)
}

stream_hashes <- vapply(
  rng_streams,
  object_sha256,
  character(1L)
)

if (anyDuplicated(stream_hashes)) {
  stop("Duplicate RNG streams were generated.", call. = FALSE)
}

# Probe a subset, not all 4500, to keep materialization light but meaningful.
probe_ids <- c(1L, 2L, 90L, 91L, 500L, 501L, 900L, 901L, 2500L, 4500L)
probe_hashes <- vapply(
  probe_ids,
  function(i) rng_probe(rng_streams[[i]]),
  character(1L)
)

if (anyDuplicated(probe_hashes)) {
  stop("Duplicate RNG probes in selected final streams.", call. = FALSE)
}

# Order-independence check on selected streams.
forward <- vapply(
  probe_ids,
  function(i) rng_probe(rng_streams[[i]]),
  character(1L)
)

reverse <- vapply(
  rev(probe_ids),
  function(i) rng_probe(rng_streams[[i]]),
  character(1L)
)

names(forward) <- as.character(probe_ids)
names(reverse) <- as.character(rev(probe_ids))

if (!identical(forward, reverse[names(forward)])) {
  stop("Final RNG order-independence check failed.", call. = FALSE)
}

manifest$rng_kind <- "L'Ecuyer-CMRG"
manifest$rng_normal_kind <- "Inversion"
manifest$rng_sample_kind <- "Rejection"
manifest$rng_stream_sha256 <- stream_hashes

rng_stream_manifest <- data.frame(
  task_id = manifest$task_id,
  task_key = manifest$task_key,
  rng_stream_index = manifest$rng_stream_index,
  rng_stream_sha256 = stream_hashes,
  stringsAsFactors = FALSE
)

git_commit <- system2(
  "git",
  args = c("-C", work_repo, "rev-parse", "HEAD"),
  stdout = TRUE
)

git_status <- system2(
  "git",
  args = c("-C", work_repo, "status", "--porcelain=v1"),
  stdout = TRUE
)

metadata <- data.frame(
  design_type = "final",
  design_version = spec$design_version,
  git_commit = git_commit,
  git_worktree_clean = length(git_status) == 0L,
  base_seed = spec$base_seed,
  final_replications_per_cell = spec$final_replications,
  n_final_tasks = nrow(manifest),
  n_rng_streams = length(rng_streams),
  n_methods = nrow(methods),
  methods = paste(methods$method_code, collapse = ","),
  probability_grid_size = length(spec$probability_grid),
  max_bandwidth_cap = spec$max_bandwidth_cap,
  polynomial_beta = spec$polynomial_beta,
  polynomial_filter_length = spec$polynomial_filter_length,
  stringsAsFactors = FALSE
)

write_csv(manifest, file.path(tmp_dir, "task_manifest.csv"))
saveRDS(manifest, file.path(tmp_dir, "task_manifest.rds"), version = 3, compress = "xz")
saveRDS(rng_streams, file.path(tmp_dir, "rng_streams.rds"), version = 3, compress = "xz")
write_csv(methods, file.path(tmp_dir, "method_table.csv"))
write_csv(scenarios, file.path(tmp_dir, "scenario_table.csv"))
saveRDS(spec, file.path(tmp_dir, "study_spec.rds"), version = 3, compress = "xz")
write_csv(cell_counts, file.path(tmp_dir, "final_cell_counts.csv"))
write_csv(rng_stream_manifest, file.path(tmp_dir, "rng_stream_manifest.csv"))
write_csv(metadata, file.path(tmp_dir, "final_design_metadata.csv"))

write_csv(
  data.frame(
    probe_task_id = probe_ids,
    probe_sha256 = unname(forward),
    reverse_probe_sha256 = unname(reverse[names(forward)]),
    identical = unname(forward) == unname(reverse[names(forward)]),
    stringsAsFactors = FALSE
  ),
  file.path(tmp_dir, "rng_order_independence_check.csv")
)

utils::capture.output(
  sessionInfo(),
  file = file.path(tmp_dir, "sessionInfo.txt")
)

# Round-trip checks.
manifest_check <- readRDS(file.path(tmp_dir, "task_manifest.rds"))
streams_check <- readRDS(file.path(tmp_dir, "rng_streams.rds"))
spec_check <- readRDS(file.path(tmp_dir, "study_spec.rds"))

stopifnot(
  identical(manifest_check, manifest),
  identical(streams_check, rng_streams),
  identical(spec_check, spec)
)

files <- c(
  "task_manifest.csv",
  "task_manifest.rds",
  "rng_streams.rds",
  "method_table.csv",
  "scenario_table.csv",
  "study_spec.rds",
  "final_cell_counts.csv",
  "rng_stream_manifest.csv",
  "final_design_metadata.csv",
  "rng_order_independence_check.csv",
  "sessionInfo.txt"
)

hashes <- data.frame(
  file = files,
  bytes = file.info(file.path(tmp_dir, files))$size,
  sha256 = vapply(file.path(tmp_dir, files), file_sha256, character(1L)),
  stringsAsFactors = FALSE
)

write_csv(hashes, file.path(tmp_dir, "final_design_sha256.csv"))

writeLines(
  c(
    "FINAL_DESIGN_STATUS=PASS",
    paste0("FINAL_TASKS=", nrow(manifest)),
    "EXPECTED_FINAL_TASKS=4500",
    paste0("RNG_STREAMS=", length(rng_streams)),
    paste0("UNIQUE_RNG_STREAMS=", length(unique(stream_hashes))),
    "METHODS=BL,BS,HT,FT",
    "RNG_ORDER_INDEPENDENCE_CHECK=PASS",
    "RDS_ROUNDTRIP_CHECK=PASS"
  ),
  con = file.path(tmp_dir, "final_design_status.txt")
)

if (!file.rename(tmp_dir, design_dir)) {
  stop("Atomic rename to final design directory failed.", call. = FALSE)
}

completed <- TRUE

cat("FINAL_DESIGN_STATUS=PASS\n")
cat("FINAL_TASKS=", nrow(manifest), "\n", sep = "")
cat("RNG_STREAMS=", length(rng_streams), "\n", sep = "")
cat("METHODS=BL,BS,HT,FT\n")
