args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2L) {
  stop(
    "Usage: make_final_article_tables.R <final_agg_dir> <table_dir>",
    call. = FALSE
  )
}

final_agg_dir <- normalizePath(args[[1L]], mustWork = TRUE)
table_dir <- normalizePath(args[[2L]], mustWork = FALSE)

dir.create(
  table_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

read_csv <- function(file) {
  utils::read.csv(
    file.path(final_agg_dir, file),
    stringsAsFactors = FALSE,
    check.names = FALSE
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
    na = "",
    fileEncoding = "UTF-8"
  )
  invisible(path)
}

latex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("&", "\\\\&", x)
  x <- gsub("%", "\\\\%", x)
  x <- gsub("\\$", "\\\\$", x)
  x <- gsub("#", "\\\\#", x)
  x <- gsub("_", "\\\\_", x)
  x <- gsub("\\{", "\\\\{", x)
  x <- gsub("\\}", "\\\\}", x)
  x <- gsub("~", "\\\\textasciitilde{}", x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x
}

format_num <- function(x, digits = 3L) {
  ifelse(
    is.na(x),
    "",
    formatC(
      x,
      format = "f",
      digits = digits
    )
  )
}

format_pct <- function(x, digits = 1L) {
  ifelse(
    is.na(x),
    "",
    paste0(
      formatC(
        100 * x,
        format = "f",
        digits = digits
      ),
      "\\%"
    )
  )
}

mean_sd <- function(x, digits = 3L) {
  paste0(
    format_num(mean(x), digits),
    " (",
    format_num(stats::sd(x), digits),
    ")"
  )
}

write_latex_table <- function(
  x,
  path,
  caption,
  label,
  align = NULL,
  small = TRUE
) {
  if (is.null(align)) {
    align <- paste0(
      "l",
      paste(rep("c", ncol(x) - 1L), collapse = "")
    )
  }

  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)

  writeLines("\\begin{table}[!htbp]", con)

  if (small) {
    writeLines("\\small", con)
  }

  writeLines("\\centering", con)
  writeLines(paste0("\\caption{", caption, "}"), con)
  writeLines(paste0("\\label{", label, "}"), con)
  writeLines(paste0("\\begin{tabular}{", align, "}"), con)
  writeLines("\\toprule", con)

  header <- paste(
    latex_escape(names(x)),
    collapse = " & "
  )

  writeLines(paste0(header, " \\\\"), con)
  writeLines("\\midrule", con)

  for (i in seq_len(nrow(x))) {
    row <- paste(
      latex_escape(unlist(x[i, ], use.names = FALSE)),
      collapse = " & "
    )
    writeLines(paste0(row, " \\\\"), con)
  }

  writeLines("\\bottomrule", con)
  writeLines("\\end{tabular}", con)
  writeLines("\\end{table}", con)

  invisible(path)
}

scenario_display <- function(id) {
  out <- id
  out[id == "geom_04"] <- "AR(1), rho = 0.4"
  out[id == "geom_08"] <- "AR(1), rho = 0.8"
  out[id == "tpl_22"] <- "Truncated linear, beta = 2.2"
  out
}

method_order <- c("BL", "BS", "FT", "HT")

method_display <- function(code) {
  out <- code
  out[code == "BL"] <- "Bartlett LW"
  out[code == "BS"] <- "Bartlett sandwich"
  out[code == "FT"] <- "Exact flat-top"
  out[code == "HT"] <- "Hard truncation"
  out
}

wide_method_table <- function(
  data,
  value_col,
  value_formatter,
  by_cols = c("scenario_id", "sample_size")
) {
  rows <- unique(data[by_cols])
  rows <- rows[order(rows$scenario_id, rows$sample_size), , drop = FALSE]

  out <- data.frame(
    Scenario = scenario_display(rows$scenario_id),
    n = rows$sample_size,
    stringsAsFactors = FALSE
  )

  for (method in method_order) {
    values <- character(nrow(rows))

    for (i in seq_len(nrow(rows))) {
      hit <- data[
        data$scenario_id == rows$scenario_id[i] &
          data$sample_size == rows$sample_size[i] &
          data$method_code == method,
        ,
        drop = FALSE
      ]

      if (nrow(hit) == 0L) {
        values[i] <- ""
      } else {
        values[i] <- value_formatter(hit[[value_col]])
      }
    }

    out[[method]] <- values
  }

  names(out)[names(out) %in% method_order] <-
    method_display(method_order)

  out
}

metrics <- read_csv("final_metrics_selected_bandwidth.csv")
gap <- read_csv("final_gap_selected_bandwidth.csv")
timing <- read_csv("final_timing.csv")
negative <- read_csv("final_negative_eigen_summary.csv")

required_metric_cols <- c(
  "task_id",
  "scenario_id",
  "sample_size",
  "method_code",
  "relative_frobenius_error",
  "frobenius_error",
  "spectral_error",
  "min_eigenvalue",
  "negative_min_eigen",
  "selected_bandwidth"
)

missing_metric_cols <- setdiff(
  required_metric_cols,
  names(metrics)
)

if (length(missing_metric_cols) > 0L) {
  stop(
    "Missing metric columns: ",
    paste(missing_metric_cols, collapse = ", "),
    call. = FALSE
  )
}

stopifnot(
  nrow(metrics) == 18000L,
  nrow(gap) == 4500L,
  nrow(timing) == 4500L
)

if (sum(metrics$method_code == "BS" & metrics$negative_min_eigen) != 0L) {
  stop(
    "BS has at least one negative eigenvalue in the selected metrics.",
    call. = FALSE
  )
}

# ------------------------------------------------------------
# Table 1: design
# ------------------------------------------------------------

design_cells <- unique(
  metrics[
    ,
    c("scenario_id", "scenario_label", "sample_size")
  ]
)

design_cells <- design_cells[
  order(design_cells$scenario_id, design_cells$sample_size),
  ,
  drop = FALSE
]

design_table <- data.frame(
  Scenario = scenario_display(design_cells$scenario_id),
  n = design_cells$sample_size,
  Replications = 500L,
  Methods = "BL, BS, FT, HT",
  stringsAsFactors = FALSE
)

write_csv(
  design_table,
  file.path(table_dir, "table_01_simulation_design.csv")
)

write_latex_table(
  design_table,
  file.path(table_dir, "table_01_simulation_design.tex"),
  caption = paste(
    "Final Monte Carlo design.",
    "Each cell contains 500 independent replications.",
    "The four estimators are computed on the same generated sample within each replication."
  ),
  label = "tab:simulation-design",
  align = "lccc"
)

# ------------------------------------------------------------
# Table 2: negative eigenvalue frequency
# ------------------------------------------------------------

negative_table <- wide_method_table(
  data = negative,
  value_col = "negative_eigen_frequency",
  value_formatter = function(x) format_pct(x, digits = 1L)
)

write_csv(
  negative_table,
  file.path(table_dir, "table_02_negative_eigen_frequency.csv")
)

write_latex_table(
  negative_table,
  file.path(table_dir, "table_02_negative_eigen_frequency.tex"),
  caption = paste(
    "Frequency of indefinite selected covariance estimates.",
    "Entries are percentages of replications with a negative minimum eigenvalue."
  ),
  label = "tab:negative-eigen-frequency",
  align = "lccccc"
)

# ------------------------------------------------------------
# Table 3: relative Frobenius error
# ------------------------------------------------------------

rel_frob_summary <- stats::aggregate(
  relative_frobenius_error ~ scenario_id + sample_size + method_code,
  data = metrics,
  FUN = function(x) mean_sd(x, digits = 3L)
)

rel_frob_table <- wide_method_table(
  data = rel_frob_summary,
  value_col = "relative_frobenius_error",
  value_formatter = function(x) x
)

write_csv(
  rel_frob_table,
  file.path(table_dir, "table_03_relative_frobenius_error.csv")
)

write_latex_table(
  rel_frob_table,
  file.path(table_dir, "table_03_relative_frobenius_error.tex"),
  caption = paste(
    "Relative Frobenius error at the selected bandwidth.",
    "Entries report mean and standard deviation over 500 replications."
  ),
  label = "tab:relative-frobenius-error",
  align = "lccccc"
)

# ------------------------------------------------------------
# Table 4: spectral error
# ------------------------------------------------------------

spectral_summary <- stats::aggregate(
  spectral_error ~ scenario_id + sample_size + method_code,
  data = metrics,
  FUN = function(x) mean_sd(x, digits = 3L)
)

spectral_table <- wide_method_table(
  data = spectral_summary,
  value_col = "spectral_error",
  value_formatter = function(x) x
)

write_csv(
  spectral_table,
  file.path(table_dir, "table_04_spectral_error.csv")
)

write_latex_table(
  spectral_table,
  file.path(table_dir, "table_04_spectral_error.tex"),
  caption = paste(
    "Spectral norm error at the selected bandwidth.",
    "Entries report mean and standard deviation over 500 replications."
  ),
  label = "tab:spectral-error",
  align = "lccccc"
)

# ------------------------------------------------------------
# Table 5: selected bandwidth
# ------------------------------------------------------------

bandwidth_summary <- stats::aggregate(
  selected_bandwidth ~ scenario_id + sample_size,
  data = unique(metrics[, c("task_id", "scenario_id", "sample_size", "selected_bandwidth")]),
  FUN = function(x) {
    paste0(
      format_num(mean(x), 2L),
      " [",
      format_num(stats::median(x), 0L),
      "; ",
      format_num(as.numeric(stats::quantile(x, 0.90, names = FALSE)), 0L),
      "]"
    )
  }
)

bandwidth_table <- data.frame(
  Scenario = scenario_display(bandwidth_summary$scenario_id),
  n = bandwidth_summary$sample_size,
  "Selected bandwidth: mean [median; q90]" =
    bandwidth_summary$selected_bandwidth,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

write_csv(
  bandwidth_table,
  file.path(table_dir, "table_05_selected_bandwidth.csv")
)

write_latex_table(
  bandwidth_table,
  file.path(table_dir, "table_05_selected_bandwidth.tex"),
  caption = paste(
    "Selected bandwidth by simulation cell.",
    "Entries report mean, median and empirical 90th percentile over 500 replications."
  ),
  label = "tab:selected-bandwidth",
  align = "lcc"
)

# ------------------------------------------------------------
# Table 6: BS versus BL gap
# ------------------------------------------------------------

gap_summary <- stats::aggregate(
  cbind(
    gap_frobenius,
    gap_supnorm,
    gap_over_lw_error
  ) ~ scenario_id + sample_size,
  data = gap,
  FUN = function(x) c(
    mean = mean(x),
    sd = stats::sd(x),
    median = stats::median(x),
    q90 = as.numeric(stats::quantile(x, 0.90, names = FALSE))
  )
)

gap_flat <- do.call(data.frame, gap_summary)
names(gap_flat) <- make.names(names(gap_flat), unique = TRUE)

gap_table <- data.frame(
  Scenario = scenario_display(gap_flat$scenario_id),
  n = gap_flat$sample_size,
  "Frobenius gap mean (sd)" = paste0(
    format_num(gap_flat$gap_frobenius.mean, 3L),
    " (",
    format_num(gap_flat$gap_frobenius.sd, 3L),
    ")"
  ),
  "Supremum gap mean (sd)" = paste0(
    format_num(gap_flat$gap_supnorm.mean, 3L),
    " (",
    format_num(gap_flat$gap_supnorm.sd, 3L),
    ")"
  ),
  "Gap / BL error mean" =
    format_num(gap_flat$gap_over_lw_error.mean, 3L),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

write_csv(
  gap_table,
  file.path(table_dir, "table_06_bs_bl_gap.csv")
)

write_latex_table(
  gap_table,
  file.path(table_dir, "table_06_bs_bl_gap.tex"),
  caption = paste(
    "Finite-sample gap between Bartlett sandwich and Bartlett lag-window estimates.",
    "The comparison is computed at the selected bandwidth on the same replication."
  ),
  label = "tab:bs-bl-gap",
  align = "lcccc"
)

# ------------------------------------------------------------
# Table 7: computing time
# ------------------------------------------------------------

timing_summary <- stats::aggregate(
  elapsed_seconds ~ scenario_id + sample_size,
  data = timing,
  FUN = function(x) c(
    mean = mean(x),
    median = stats::median(x),
    q95 = as.numeric(stats::quantile(x, 0.95, names = FALSE)),
    max = max(x)
  )
)

timing_flat <- do.call(data.frame, timing_summary)
names(timing_flat) <- make.names(names(timing_flat), unique = TRUE)

timing_table <- data.frame(
  Scenario = scenario_display(timing_flat$scenario_id),
  n = timing_flat$sample_size,
  "Mean seconds" = format_num(timing_flat$elapsed_seconds.mean, 2L),
  "Median seconds" = format_num(timing_flat$elapsed_seconds.median, 2L),
  "95th percentile" = format_num(timing_flat$elapsed_seconds.q95, 2L),
  "Maximum seconds" = format_num(timing_flat$elapsed_seconds.max, 2L),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

write_csv(
  timing_table,
  file.path(table_dir, "table_07_computing_time.csv")
)

write_latex_table(
  timing_table,
  file.path(table_dir, "table_07_computing_time.tex"),
  caption = paste(
    "Computing time per atomic replication.",
    "Times are measured in seconds and include all four estimators."
  ),
  label = "tab:computing-time",
  align = "lccccc"
)

# ------------------------------------------------------------
# Main textual summary for manuscript notes
# ------------------------------------------------------------

bs_negative_count <- sum(
  metrics$method_code == "BS" &
    metrics$negative_min_eigen
)

negative_by_method <- stats::aggregate(
  negative_min_eigen ~ method_code,
  data = metrics,
  FUN = mean
)

negative_by_method <- negative_by_method[
  match(method_order, negative_by_method$method_code),
  ,
  drop = FALSE
]

summary_lines <- c(
  "FINAL_ARTICLE_TABLES_STATUS=PASS",
  paste0("N_REPLICATIONS_TOTAL=", length(unique(metrics$task_id))),
  paste0("N_SELECTED_METRIC_ROWS=", nrow(metrics)),
  paste0("N_GAP_ROWS=", nrow(gap)),
  paste0("BS_NEGATIVE_EIGEN_COUNT=", bs_negative_count),
  "",
  "Overall negative-eigenvalue frequencies by method:",
  paste0(
    negative_by_method$method_code,
    ": ",
    formatC(
      100 * negative_by_method$negative_min_eigen,
      format = "f",
      digits = 2
    ),
    "%"
  )
)

writeLines(
  summary_lines,
  con = file.path(table_dir, "final_article_tables_status.txt")
)

# ------------------------------------------------------------
# Hashes
# ------------------------------------------------------------

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package digest is required for hashes.", call. = FALSE)
}

table_files <- list.files(
  table_dir,
  full.names = TRUE
)

hashes <- data.frame(
  file = basename(table_files),
  bytes = file.info(table_files)$size,
  sha256 = vapply(
    table_files,
    digest::digest,
    character(1L),
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  ),
  stringsAsFactors = FALSE
)

write_csv(
  hashes,
  file.path(table_dir, "final_article_tables_sha256.csv")
)

cat("FINAL_ARTICLE_TABLES_STATUS=PASS\n")
cat("TABLE_DIR=", table_dir, "\n", sep = "")
cat("BS_NEGATIVE_EIGEN_COUNT=", bs_negative_count, "\n", sep = "")
