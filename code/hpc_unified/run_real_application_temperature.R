#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(digest)
})

# ============================================================
# Paths
# ============================================================

project_dir <- Sys.getenv("PROJECT_DIR")
results_dir <- Sys.getenv("RESULTS_DIR")

if (project_dir == "" || results_dir == "") {
  stop("PROJECT_DIR and RESULTS_DIR must be defined.", call. = FALSE)
}

work_repo <- file.path(project_dir, "repository_work")
data_file <- file.path(
  work_repo,
  "data",
  "dados_publicos_ipma_para_R.xlsx"
)

if (!file.exists(data_file)) {
  stop("Data file not found: ", data_file, call. = FALSE)
}

out_dir <- file.path(results_dir, "real_application", "final_temperature")
tab_dir <- file.path(out_dir, "tables")
fig_dir <- file.path(out_dir, "figures")
tex_dir <- file.path(out_dir, "latex")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tex_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Helper functions
# ============================================================

parse_date_safe <- function(x) {
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }

  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    return(as.Date(x))
  }

  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }

  x_chr <- as.character(x)

  out <- suppressWarnings(as.Date(x_chr))

  if (all(is.na(out))) {
    out <- suppressWarnings(as.Date(x_chr, format = "%d/%m/%Y"))
  }

  if (all(is.na(out))) {
    out <- suppressWarnings(as.Date(x_chr, format = "%Y-%m-%d"))
  }

  out
}

to_numeric_safe <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  x_chr <- as.character(x)
  x_chr <- gsub(",", ".", x_chr)
  suppressWarnings(as.numeric(x_chr))
}

save_pdf <- function(plot, name, width = 7.2, height = 5.0) {
  file <- file.path(fig_dir, paste0(name, ".pdf"))

  grDevices::pdf(
    file = file,
    width = width,
    height = height,
    onefile = TRUE,
    useDingbats = FALSE
  )

  print(plot)
  grDevices::dev.off()

  invisible(file)
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
  x
}

write_latex_table <- function(x, file, caption, label, align = NULL) {
  if (is.null(align)) {
    align <- paste(rep("l", ncol(x)), collapse = "")
  }

  con <- file(file, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)

  writeLines("\\begin{table}[!htbp]", con)
  writeLines("\\centering", con)
  writeLines("\\small", con)
  writeLines(paste0("\\caption{", caption, "}"), con)
  writeLines(paste0("\\label{", label, "}"), con)
  writeLines(paste0("\\begin{tabular}{", align, "}"), con)
  writeLines("\\toprule", con)
  writeLines(paste(latex_escape(names(x)), collapse = " & "), con)
  writeLines(" \\\\", con)
  writeLines("\\midrule", con)

  for (i in seq_len(nrow(x))) {
    writeLines(
      paste(latex_escape(unlist(x[i, ], use.names = FALSE)), collapse = " & "),
      con
    )
    writeLines(" \\\\", con)
  }

  writeLines("\\bottomrule", con)
  writeLines("\\end{tabular}", con)
  writeLines("\\end{table}", con)

  invisible(file)
}

base_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# ============================================================
# Load temperature data
# ============================================================

temp_raw <- readxl::read_excel(
  data_file,
  sheet = "long_daily_lisbon_temp",
  col_types = "text"
)

required_columns <- c(
  "source_url",
  "station_code",
  "station_name",
  "date",
  "tmin",
  "tmax"
)

missing_columns <- setdiff(required_columns, names(temp_raw))

if (length(missing_columns) > 0L) {
  stop(
    "Missing columns in long_daily_lisbon_temp: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

temp <- temp_raw %>%
  transmute(
    source_url = as.character(source_url),
    station_code = as.character(station_code),
    station_name = as.character(station_name),
    date = as.Date(as.numeric(date), origin = "1899-12-30"),
    tmin = to_numeric_safe(tmin),
    tmax = to_numeric_safe(tmax)
  ) %>%
  filter(!is.na(date)) %>%
  arrange(date)

temp <- temp %>%
  mutate(
    tmean = (tmin + tmax) / 2
  )

if (
  nrow(temp) != 59444L ||
  min(temp$date, na.rm = TRUE) != as.Date("1855-12-01") ||
  max(temp$date, na.rm = TRUE) != as.Date("2018-08-31")
) {
  stop(
    paste(
      "Unexpected Lisbon temperature range:",
      "nrow=", nrow(temp),
      "min=", min(temp$date, na.rm = TRUE),
      "max=", max(temp$date, na.rm = TRUE)
    ),
    call. = FALSE
  )
}

# Collapse duplicates, if any.
temp_daily <- temp %>%
  group_by(date) %>%
  summarise(
    source_url = first(na.omit(source_url)),
    station_code = first(na.omit(station_code)),
    station_name = first(na.omit(station_name)),
    tmin = mean(tmin, na.rm = TRUE),
    tmax = mean(tmax, na.rm = TRUE),
    tmean = mean(tmean, na.rm = TRUE),
    n_records = n(),
    .groups = "drop"
  )

temp_daily$tmin[!is.finite(temp_daily$tmin)] <- NA_real_
temp_daily$tmax[!is.finite(temp_daily$tmax)] <- NA_real_
temp_daily$tmean[!is.finite(temp_daily$tmean)] <- NA_real_

date_grid <- data.frame(
  date = seq(min(temp_daily$date), max(temp_daily$date), by = "day")
)

temp_complete <- date_grid %>%
  left_join(temp_daily, by = "date") %>%
  arrange(date)

analysis_data <- temp_complete %>%
  filter(!is.na(tmean)) %>%
  arrange(date)

if (nrow(analysis_data) < 365L) {
  stop("Too few non-missing daily temperature values.", call. = FALSE)
}

x <- analysis_data$tmean
dates <- analysis_data$date
n <- length(x)

# ============================================================
# Load existing estimator helpers
# ============================================================

helper_env <- new.env(parent = globalenv())

sys.source(
  file.path(work_repo, "src", "simulation", "gap_sandwich_lag_window.R"),
  envir = helper_env
)

if (!is.function(helper_env$run_mc_lagwindow_sandwich_jtsa)) {
  stop("Cannot access patched helper-returning simulation function.", call. = FALSE)
}

helper_output <- tempfile("real_app_helper_")

helpers <- helper_env$run_mc_lagwindow_sandwich_jtsa(
  output_dir = helper_output,
  paper_mode = FALSE,
  seed = 20260622L,
  n_replications = 1L,
  sample_sizes = 80L,
  probability_grid = c(0.25, 0.50, 0.75),
  max_bandwidth_cap = 12L,
  negative_eigen_tolerance = -1e-10,
  near_singular_tolerance = 1e-4,
  polynomial_beta = 2.2,
  polynomial_filter_length = 600L,
  save_replication_level_metrics = FALSE,
  return_helpers = TRUE
)

unlink(helper_output, recursive = TRUE, force = TRUE)

needed <- c(
  "make_bandwidth_grid",
  "make_indicator_matrix",
  "make_centered_indicator_proxy",
  "select_bandwidth_screen",
  "build_moments",
  "estimate_lagwindow",
  "estimate_sandwich_bartlett"
)

ok <- vapply(helpers[needed], is.function, logical(1L))

if (!all(ok)) {
  stop(
    "Missing helper functions: ",
    paste(needed[!ok], collapse = ", "),
    call. = FALSE
  )
}

# ============================================================
# Threshold empirical-process covariance estimation
# ============================================================

prob_grid <- seq(0.05, 0.95, by = 0.05)
thresholds <- as.numeric(
  stats::quantile(
    x,
    probs = prob_grid,
    type = 8,
    na.rm = TRUE
  )
)

indicator_matrix <- helpers$make_indicator_matrix(
  x,
  thresholds
)

bandwidth_grid <- helpers$make_bandwidth_grid(
  n = n,
  cap = min(160L, floor(n / 4))
)

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
  max_lag = max(bandwidth_grid)
)

exact_flat_top <- function(moments, bandwidth) {
  max_available_lag <- length(moments$lag_phi_raw)
  max_lag <- min(max_available_lag, as.integer(bandwidth))

  gamma_hat <- moments$S0_raw

  if (max_lag >= 1L) {
    for (k in seq_len(max_lag)) {
      u <- k / bandwidth

      weight <- if (u <= 0.5) {
        1
      } else if (u <= 1) {
        2 * (1 - u)
      } else {
        0
      }

      if (weight != 0) {
        gamma_hat <- gamma_hat +
          weight * (
            moments$lag_phi_raw[[k]] +
              t(moments$lag_phi_raw[[k]])
          )
      }
    }
  }

  (gamma_hat + t(gamma_hat)) / 2
}

estimates <- list(
  BL = helpers$estimate_lagwindow(
    moments,
    bandwidth = selected_bandwidth,
    method = "bartlett"
  ),
  BS = helpers$estimate_sandwich_bartlett(
    moments,
    bandwidth = selected_bandwidth
  ),
  FT = exact_flat_top(
    moments,
    bandwidth = selected_bandwidth
  ),
  HT = helpers$estimate_lagwindow(
    moments,
    bandwidth = selected_bandwidth,
    method = "hard"
  )
)

method_labels <- c(
  BL = "Bartlett lag-window",
  BS = "Bartlett sandwich",
  FT = "Exact flat-top",
  HT = "Hard truncation"
)

diagnostics <- lapply(names(estimates), function(code) {
  mat <- (estimates[[code]] + t(estimates[[code]])) / 2
  eig <- eigen(mat, symmetric = TRUE, only.values = TRUE)$values

  positive_eig <- eig[eig > 1e-10]

  data.frame(
    method_code = code,
    method = method_labels[[code]],
    selected_bandwidth = selected_bandwidth,
    trace = sum(diag(mat)),
    min_eigenvalue = min(eig),
    max_eigenvalue = max(eig),
    negative_min_eigen = min(eig) < -1e-10,
    near_singular = min(abs(eig)) < 1e-4,
    condition_proxy = ifelse(
      length(positive_eig) == 0L,
      NA_real_,
      max(eig) / min(positive_eig)
    ),
    stringsAsFactors = FALSE
  )
}) %>%
  bind_rows()

# Gap between BS and BL.
bs_bl_gap <- data.frame(
  selected_bandwidth = selected_bandwidth,
  frobenius_gap = sqrt(sum((estimates$BS - estimates$BL)^2)),
  supnorm_gap = max(abs(estimates$BS - estimates$BL)),
  trace_gap = sum(diag(estimates$BS)) - sum(diag(estimates$BL)),
  stringsAsFactors = FALSE
)

# ============================================================
# Figures
# ============================================================

series_plot <- ggplot(
  analysis_data,
  aes(x = date, y = tmean)
) +
  geom_line(linewidth = 0.25, colour = "#2C3E50") +
  labs(
    x = "Date",
    y = "Daily mean temperature"
  ) +
  base_theme +
  theme(legend.position = "none")

save_pdf(
  series_plot,
  "fig_real_lisbon_temperature_series",
  width = 7.2,
  height = 3.4
)

bs_mat <- estimates$BS
row_lab <- paste0("p", sprintf("%02d", round(100 * prob_grid)))
col_lab <- row_lab

rownames(bs_mat) <- row_lab
colnames(bs_mat) <- col_lab

bs_long <- as.data.frame(as.table(bs_mat))
names(bs_long) <- c("row", "column", "value")
bs_long$row <- factor(bs_long$row, levels = rev(row_lab))
bs_long$column <- factor(bs_long$column, levels = col_lab)

heatmap_plot <- ggplot(
  bs_long,
  aes(x = column, y = row, fill = value)
) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0
  ) +
  labs(
    x = "Probability threshold",
    y = "Probability threshold",
    fill = "Estimate"
  ) +
  base_theme +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

save_pdf(
  heatmap_plot,
  "fig_real_bartlett_sandwich_heatmap",
  width = 6.8,
  height = 5.6
)

eig_plot <- diagnostics %>%
  mutate(
    method = factor(
      method,
      levels = method_labels[c("BL", "BS", "FT", "HT")]
    )
  ) %>%
  ggplot(
    aes(x = method, y = min_eigenvalue)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.45) +
  geom_col(fill = "#377EB8", width = 0.65) +
  coord_flip() +
  labs(
    x = "Estimator",
    y = "Minimum eigenvalue"
  ) +
  base_theme +
  theme(legend.position = "none")

save_pdf(
  eig_plot,
  "fig_real_minimum_eigenvalue",
  width = 6.8,
  height = 3.6
)

# ============================================================
# Tables
# ============================================================

source_url <- analysis_data$source_url[which(!is.na(analysis_data$source_url))[1]]
station_name <- analysis_data$station_name[which(!is.na(analysis_data$station_name))[1]]
station_code <- analysis_data$station_code[which(!is.na(analysis_data$station_code))[1]]

data_summary <- data.frame(
  Item = c(
    "Data file",
    "Worksheet",
    "Station",
    "Station code",
    "First date",
    "Last date",
    "Daily grid length",
    "Observed temperature values",
    "Missing values on daily grid",
    "Temperature definition",
    "Probability grid",
    "Selected bandwidth",
    "Source URL"
  ),
  Value = c(
    basename(data_file),
    "long_daily_lisbon_temp",
    station_name,
    station_code,
    as.character(min(temp_complete$date)),
    as.character(max(temp_complete$date)),
    as.character(nrow(temp_complete)),
    as.character(nrow(analysis_data)),
    as.character(sum(is.na(temp_complete$tmean))),
    "(tmin + tmax) / 2",
    "0.05, 0.10, ..., 0.95",
    as.character(selected_bandwidth),
    source_url
  ),
  stringsAsFactors = FALSE
)

diagnostics_table <- diagnostics %>%
  transmute(
    Method = method,
    `Selected bandwidth` = selected_bandwidth,
    Trace = sprintf("%.4f", trace),
    `Minimum eigenvalue` = sprintf("%.6f", min_eigenvalue),
    `Maximum eigenvalue` = sprintf("%.4f", max_eigenvalue),
    `Negative minimum eigenvalue` = ifelse(
      negative_min_eigen,
      "yes",
      "no"
    ),
    `Near singular` = ifelse(
      near_singular,
      "yes",
      "no"
    )
  )

readr::write_csv(
  temp_complete,
  file.path(tab_dir, "real_lisbon_temperature_clean_daily.csv")
)

readr::write_csv(
  data_summary,
  file.path(tab_dir, "real_application_data_summary.csv")
)

readr::write_csv(
  diagnostics,
  file.path(tab_dir, "real_application_estimator_diagnostics.csv")
)

readr::write_csv(
  diagnostics_table,
  file.path(tab_dir, "real_application_estimator_diagnostics_for_latex.csv")
)

readr::write_csv(
  bs_bl_gap,
  file.path(tab_dir, "real_application_bs_bl_gap.csv")
)

write_latex_table(
  data_summary,
  file.path(tex_dir, "table_real_application_data_summary.tex"),
  caption = "Summary of the real-data illustration.",
  label = "tab:real-data-summary",
  align = "ll"
)

write_latex_table(
  diagnostics_table,
  file.path(tex_dir, "table_real_application_estimator_diagnostics.tex"),
  caption = "Estimator diagnostics for the real-data illustration.",
  label = "tab:real-estimator-diagnostics",
  align = "lcccccc"
)

# ============================================================
# BibTeX suggestion
# ============================================================

bibtex <- c(
  "@misc{IPMALisbonTemperatureData,",
  "  author       = {{Instituto Portugu\\^es do Mar e da Atmosfera}},",
  "  title        = {Daily meteorological observations for Lisbon},",
  "  year         = {2026},",
  paste0("  howpublished = {\\url{", source_url, "}},"),
  "  note         = {Data workbook used in the real-data illustration. Accessed on 22 June 2026},",
  "}"
)

writeLines(
  bibtex,
  file.path(tex_dir, "real_application_bibtex_entry.bib")
)

# ============================================================
# Main text block
# ============================================================

text_block <- c(
  "\\section{Real-data illustration}",
  "\\label{sec:real-application}",
  "",
  "We conclude with a short real-data illustration based on daily meteorological observations from Lisbon. The purpose of this example is not to provide a climatological analysis, but to show how the proposed estimator behaves when applied to a genuinely dependent time series. We use daily mean temperature, defined as the average of the recorded daily minimum and maximum temperatures, and consider the centred threshold indicators associated with the probability grid $0.05,0.10,\\ldots,0.95$.",
  "",
  paste0(
    "The retained series contains $n=",
    n,
    "$ observed daily values, spanning the period from ",
    min(dates),
    " to ",
    max(dates),
    ". The selected bandwidth is $b=",
    selected_bandwidth,
    "$. All four estimators are computed on the same threshold grid and using the same data."
  ),
  "",
  "Table~\\ref{tab:real-estimator-diagnostics} reports basic matrix diagnostics. The relevant diagnostic is the minimum eigenvalue, since the target object is a covariance kernel. In applications involving simulation, regularisation, inversion or quadratic-form inference, an indefinite covariance estimate may require an additional repair step. The Bartlett sandwich estimator avoids this issue by construction and yields a positive semidefinite estimate in this empirical illustration.",
  "",
  "\\input{table_real_application_estimator_diagnostics}",
  "",
  "\\begin{figure}[!htbp]",
  "\\centering",
  "\\includegraphics[width=0.80\\textwidth]{fig_real_bartlett_sandwich_heatmap.pdf}",
  "\\caption{Bartlett sandwich estimate of the long-run covariance matrix of centred threshold indicators in the Lisbon daily-temperature illustration.}",
  "\\label{fig:real-bs-heatmap}",
  "\\end{figure}",
  "",
  "\\begin{figure}[!htbp]",
  "\\centering",
  "\\includegraphics[width=0.80\\textwidth]{fig_real_minimum_eigenvalue.pdf}",
  "\\caption{Minimum eigenvalue of the real-data covariance estimates across the four estimators.}",
  "\\label{fig:real-min-eigen}",
  "\\end{figure}"
)

writeLines(
  text_block,
  file.path(tex_dir, "real_application_main_text_block.tex")
)

# ============================================================
# Hashes and status
# ============================================================

outputs <- list.files(
  out_dir,
  recursive = TRUE,
  full.names = TRUE
)

hashes <- data.frame(
  relative_path = sub(paste0("^", out_dir, "/?"), "", outputs),
  bytes = file.info(outputs)$size,
  sha256 = vapply(
    outputs,
    digest::digest,
    character(1L),
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  ),
  stringsAsFactors = FALSE
)

readr::write_csv(
  hashes,
  file.path(out_dir, "real_application_sha256.csv")
)

writeLines(
  c(
    "REAL_APPLICATION_TEMPERATURE_STATUS=PASS",
    paste0("DATA_FILE=", data_file),
    "WORKSHEET=long_daily_lisbon_temp",
    paste0("N_TEMPERATURE=", n),
    paste0("DATE_MIN=", min(dates)),
    paste0("DATE_MAX=", max(dates)),
    paste0("SELECTED_BANDWIDTH=", selected_bandwidth),
    paste0("OUTPUT_DIR=", out_dir)
  ),
  file.path(out_dir, "real_application_temperature_status.txt")
)

cat("REAL_APPLICATION_TEMPERATURE_STATUS=PASS\n")
cat("N_TEMPERATURE=", n, "\n", sep = "")
cat("SELECTED_BANDWIDTH=", selected_bandwidth, "\n", sep = "")
cat("OUTPUT_DIR=", out_dir, "\n", sep = "")
