#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(digest)
})

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

out_dir <- file.path(results_dir, "real_application", "final_temperature_anomalies")
tab_dir <- file.path(out_dir, "tables")
fig_dir <- file.path(out_dir, "figures")
tex_dir <- file.path(out_dir, "latex")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tex_dir, recursive = TRUE, showWarnings = FALSE)

to_numeric_safe <- function(x) {
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
# Data: Lisbon daily temperature
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

date_serial <- suppressWarnings(as.numeric(temp_raw$date))
date_converted <- as.Date(date_serial, origin = "1899-12-30")

temp <- temp_raw %>%
  transmute(
    source_url = as.character(source_url),
    station_code = as.character(station_code),
    station_name = as.character(station_name),
    date_serial = date_serial,
    date = date_converted,
    tmin = to_numeric_safe(tmin),
    tmax = to_numeric_safe(tmax)
  ) %>%
  mutate(
    tmean_raw = (tmin + tmax) / 2
  ) %>%
  arrange(date)

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

duplicate_summary <- temp %>%
  count(date, name = "n_records_on_date") %>%
  filter(n_records_on_date > 1L) %>%
  arrange(date)

n_duplicated_dates <- nrow(duplicate_summary)
n_duplicate_extra_rows <- sum(duplicate_summary$n_records_on_date - 1L)

if (n_duplicated_dates != 3L || n_duplicate_extra_rows != 3L) {
  stop(
    paste(
      "Unexpected duplicate-date structure:",
      "n_duplicated_dates=", n_duplicated_dates,
      "n_duplicate_extra_rows=", n_duplicate_extra_rows
    ),
    call. = FALSE
  )
}

temp_unique <- temp %>%
  group_by(date) %>%
  summarise(
    source_url = dplyr::first(na.omit(source_url)),
    station_code = dplyr::first(na.omit(station_code)),
    station_name = dplyr::first(na.omit(station_name)),
    date_serial = dplyr::first(date_serial),
    tmin = mean(tmin, na.rm = TRUE),
    tmax = mean(tmax, na.rm = TRUE),
    tmean_raw = mean(tmean_raw, na.rm = TRUE),
    n_records_on_date = n(),
    .groups = "drop"
  ) %>%
  arrange(date)

temp_unique$tmin[!is.finite(temp_unique$tmin)] <- NA_real_
temp_unique$tmax[!is.finite(temp_unique$tmax)] <- NA_real_
temp_unique$tmean_raw[!is.finite(temp_unique$tmean_raw)] <- NA_real_

if (nrow(temp_unique) != 59441L) {
  stop(
    paste("Unexpected number of distinct dates after aggregation:", nrow(temp_unique)),
    call. = FALSE
  )
}

date_grid <- data.frame(
  date = seq(
    min(temp_unique$date, na.rm = TRUE),
    max(temp_unique$date, na.rm = TRUE),
    by = "day"
  )
)

temp_complete <- date_grid %>%
  left_join(temp_unique, by = "date") %>%
  arrange(date)

if (nrow(temp_complete) != 59444L) {
  stop("Unexpected complete daily grid length.", call. = FALSE)
}

n_missing_tmean <- sum(is.na(temp_complete$tmean_raw))

if (n_missing_tmean != 3L) {
  stop(
    paste("Expected exactly three missing daily mean temperatures after date aggregation; found", n_missing_tmean),
    call. = FALSE
  )
}

# Linear interpolation on the regular daily grid.
idx <- seq_len(nrow(temp_complete))
observed <- !is.na(temp_complete$tmean_raw)

temp_complete$tmean_imputed <- approx(
  x = idx[observed],
  y = temp_complete$tmean_raw[observed],
  xout = idx,
  method = "linear",
  rule = 2
)$y

# Day-of-year seasonality. Feb 29 is pooled with Feb 28.
doy <- format(temp_complete$date, "%m-%d")
doy[doy == "02-29"] <- "02-28"

seasonal_mean <- ave(
  temp_complete$tmean_imputed,
  doy,
  FUN = mean
)

temp_complete$seasonal_mean <- seasonal_mean
temp_complete$seasonally_adjusted <- temp_complete$tmean_imputed - seasonal_mean

time_index <- seq_len(nrow(temp_complete))
trend_fit <- lm(seasonally_adjusted ~ time_index, data = temp_complete)

temp_complete$trend_component <- fitted(trend_fit)
temp_complete$anomaly <- residuals(trend_fit)

x <- temp_complete$anomaly
dates <- temp_complete$date
n <- length(x)

if (any(!is.finite(x))) {
  stop("Non-finite anomaly values.", call. = FALSE)
}

trend_per_decade <- coef(trend_fit)[["time_index"]] * 365.25 * 10

# ============================================================
# Validated estimator helpers
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
# Threshold-indicator covariance estimation
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

bandwidth_cap <- min(160L, floor(n / 4))

bandwidth_grid <- helpers$make_bandwidth_grid(
  n = n,
  cap = bandwidth_cap
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
  temp_complete,
  aes(x = date, y = tmean_imputed)
) +
  geom_line(linewidth = 0.20, colour = "#2C3E50") +
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

anomaly_plot <- ggplot(
  temp_complete,
  aes(x = date, y = anomaly)
) +
  geom_line(linewidth = 0.20, colour = "#2C3E50") +
  labs(
    x = "Date",
    y = "Adjusted temperature anomaly"
  ) +
  base_theme +
  theme(legend.position = "none")

save_pdf(
  anomaly_plot,
  "fig_real_lisbon_temperature_anomaly_series",
  width = 7.2,
  height = 3.4
)

eig_df <- diagnostics %>%
  mutate(
    method = factor(
      method,
      levels = method_labels[c("BL", "BS", "FT", "HT")]
    ),
    value_label = sprintf("%.4f", min_eigenvalue),
    label_y = ifelse(min_eigenvalue >= 0, min_eigenvalue + 0.05, min_eigenvalue - 0.05)
  )

eig_plot <- ggplot(
  eig_df,
  aes(x = method, y = min_eigenvalue)
) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.45) +
  geom_col(fill = "#377EB8", width = 0.65) +
  geom_text(
    aes(y = label_y, label = value_label),
    size = 3.2
  ) +
  labs(
    x = "Estimator",
    y = "Minimum eigenvalue"
  ) +
  base_theme +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

save_pdf(
  eig_plot,
  "fig_real_minimum_eigenvalue",
  width = 6.8,
  height = 3.8
)

# ============================================================
# Tables
# ============================================================

source_url <- temp_complete$source_url[which(!is.na(temp_complete$source_url))[1]]
station_name <- temp_complete$station_name[which(!is.na(temp_complete$station_name))[1]]
station_code <- temp_complete$station_code[which(!is.na(temp_complete$station_code))[1]]

data_summary <- data.frame(
  Item = c(
    "Data file",
    "Worksheet",
    "Station",
    "Station code",
    "First date",
    "Last date",
    "Daily grid length",
    "Observed raw daily mean values",
    "Duplicated calendar dates aggregated",
    "Extra duplicate rows averaged",
    "Linearly interpolated daily mean values",
    "Date conversion",
    "Temperature definition",
    "Adjustment",
    "Trend removed from seasonally adjusted series, degrees per decade",
    "Probability grid",
    "Empirical quantile type",
    "Bandwidth cap",
    "Bandwidth screening rule",
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
    as.character(sum(!is.na(temp_complete$tmean_raw))),
    as.character(n_duplicated_dates),
    as.character(n_duplicate_extra_rows),
    as.character(n_missing_tmean),
    "Excel serial date, origin 1899-12-30",
    "(tmin + tmax) / 2",
    "day-of-year effects; 29 February pooled with 28 February; linear trend removed",
    sprintf("%.6f", trend_per_decade),
    "0.05, 0.10, ..., 0.95",
    "R type 8",
    paste0("min(160, floor(n/4)) = ", bandwidth_cap),
    "first accepted bandwidth after five consecutive screening lags",
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
    `Negative minimum eigenvalue` = ifelse(negative_min_eigen, "yes", "no"),
    `Near singular` = ifelse(near_singular, "yes", "no")
  )

readr::write_csv(
  duplicate_summary,
  file.path(tab_dir, "real_application_duplicate_dates.csv")
)

readr::write_csv(
  temp_complete %>%
    filter(is.na(tmean_raw)) %>%
    select(date, tmean_raw, tmean_imputed),
  file.path(tab_dir, "real_application_interpolated_dates.csv")
)

readr::write_csv(
  temp_complete,
  file.path(tab_dir, "real_lisbon_temperature_adjusted_daily.csv")
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

bibtex <- c(
  "@misc{IPMALisbonTemperatureData,",
  "  author       = {{Instituto Portugu\\^es do Mar e da Atmosfera}},",
  "  title        = {Daily minimum and maximum temperature observations for Lisboa/Geofisico},",
  "  year         = {2026},",
  paste0("  howpublished = {\\url{", source_url, "}},"),
  "  note         = {Workbook sheet long\\_daily\\_lisbon\\_temp. Accessed on 22 June 2026},",
  "}"
)

writeLines(
  bibtex,
  file.path(tex_dir, "real_application_bibtex_entry.bib")
)

text_block <- c(
  "\\section{Real-data illustration}",
  "\\label{sec:empirical_application}",
  "",
  "We complement the Monte Carlo study with a short real-data illustration based on daily temperature observations from Lisboa/Geofisico. The purpose of this example is descriptive. It is not intended as a climatological analysis, nor as a validation of the stationary-association assumptions for the original meteorological series. Instead, the objective is to show how the covariance estimators behave when applied to a long, genuinely dependent series after removing the most evident deterministic calendar structure.",
  "",
  paste0(
    "The public IPMA worksheet contains a daily calendar from ",
    min(temp_complete$date),
    " to ",
    max(temp_complete$date),
    ". Daily mean temperature is defined as $(T_{\\min,t}+T_{\\max,t})/2$. The two missing daily mean values are filled by linear interpolation to preserve the regular calendar spacing used by the lag estimators. Deterministic seasonality is removed through day-of-year effects, with 29 February pooled with 28 February, and a linear trend is then removed from the seasonally adjusted series. The covariance analysis is carried out on the resulting anomaly series."
  ),
  "",
  paste0(
    "The estimators are applied to centred threshold indicators on the probability grid $0.05,0.10,\\ldots,0.95$. Empirical quantiles are computed using the R type-8 definition. The bandwidth grid is capped at $\\min(160,\\lfloor n/4\\rfloor)$, and the screening rule selects the first accepted bandwidth after five consecutive screening lags. The selected bandwidth is $b=",
    selected_bandwidth,
    "$."
  ),
  "",
  "\\begin{figure}[!htbp]",
  "\\centering",
  "\\includegraphics[width=0.86\\textwidth]{fig_real_lisbon_temperature_series.pdf}",
  "\\caption{Daily mean temperature series for Lisboa/Geofisico used to construct the real-data illustration. The covariance estimators are applied to the corresponding seasonally adjusted and detrended anomaly series.}",
  "\\label{fig:real-temperature-series}",
  "\\end{figure}",
  "",
  "Table~\\ref{tab:real-estimator-diagnostics} reports basic matrix diagnostics for the four estimators on the same probability grid and with the same selected bandwidth. The most relevant diagnostic is the minimum eigenvalue, since the target object is a covariance kernel. In applications involving Gaussian approximation, simulation, regularisation, inversion or quadratic-form inference, an indefinite covariance estimate may require an additional repair step.",
  "",
  "\\input{table_real_application_estimator_diagnostics}",
  "",
  "\\begin{figure}[!htbp]",
  "\\centering",
  "\\includegraphics[width=0.78\\textwidth]{fig_real_minimum_eigenvalue.pdf}",
  "\\caption{Minimum eigenvalue of the estimated long-run covariance matrix in the Lisbon daily-temperature illustration. Numerical labels identify values close to zero.}",
  "\\label{fig:real-min-eigen}",
  "\\end{figure}",
  "",
  "The empirical illustration should be interpreted as a finite-sample diagnostic rather than as a formal model check. Its role is to show the practical consequence of covariance geometry. The Bartlett sandwich estimator provides a positive-semidefinite covariance estimate by construction, while the other lag-window estimates may require additional numerical checks or repairs if they are to be used as covariance matrices in downstream procedures."
)

writeLines(
  text_block,
  file.path(tex_dir, "real_application_main_text_block.tex")
)

outputs <- list.files(out_dir, recursive = TRUE, full.names = TRUE)

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
    "REAL_APPLICATION_ANOMALY_STATUS=PASS",
    paste0("DATA_FILE=", data_file),
    "WORKSHEET=long_daily_lisbon_temp",
    paste0("DATE_MIN=", min(temp_complete$date)),
    paste0("DATE_MAX=", max(temp_complete$date)),
    paste0("N_DAILY_GRID=", nrow(temp_complete)),
    paste0("N_OBSERVED_RAW_TMEAN=", sum(!is.na(temp_complete$tmean_raw))),
    paste0("N_DUPLICATED_DATES=", n_duplicated_dates),
    paste0("N_DUPLICATE_EXTRA_ROWS=", n_duplicate_extra_rows),
    paste0("N_INTERPOLATED_TMEAN=", n_missing_tmean),
    paste0("N_ANALYSIS=", n),
    paste0("SELECTED_BANDWIDTH=", selected_bandwidth),
    paste0("OUTPUT_DIR=", out_dir)
  ),
  file.path(out_dir, "real_application_anomaly_status.txt")
)

cat("REAL_APPLICATION_ANOMALY_STATUS=PASS\n")
cat("DATE_MIN=", min(temp_complete$date), "\n", sep = "")
cat("DATE_MAX=", max(temp_complete$date), "\n", sep = "")
cat("N_DAILY_GRID=", nrow(temp_complete), "\n", sep = "")
cat("N_ANALYSIS=", n, "\n", sep = "")
cat("SELECTED_BANDWIDTH=", selected_bandwidth, "\n", sep = "")
cat("OUTPUT_DIR=", out_dir, "\n", sep = "")
