#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(digest)
})

# ============================================================
# Paths
# ============================================================

results_dir <- Sys.getenv("RESULTS_DIR", unset = "")

if (results_dir == "") {
  stop(
    "RESULTS_DIR is not defined. Run `source project.env` before this script.",
    call. = FALSE
  )
}

table_dir <- file.path(
  results_dir,
  "manuscript_tables",
  "final4500"
)

agg_dir <- file.path(
  results_dir,
  "unified_final",
  "aggregated"
)

validation_dir <- file.path(
  results_dir,
  "validation",
  "unified_final_4500"
)

fig_dir <- file.path(
  results_dir,
  "manuscript_figures",
  "final4500"
)

dir.create(
  fig_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# Validation
# ============================================================

status_file <- file.path(
  validation_dir,
  "final_4500_validation_status.txt"
)

if (!file.exists(status_file)) {
  stop(
    "Final validation status file not found: ",
    status_file,
    call. = FALSE
  )
}

status_lines <- readLines(status_file, warn = FALSE)

required_status <- c(
  "FINAL_4500_VALIDATION_STATUS=PASS",
  "TASKS_OBSERVED=4500",
  "METRICS_SELECTED_ROWS=18000",
  "GAP_SELECTED_ROWS=4500",
  "BS_NEGATIVE_EIGEN_COUNT=0",
  "FINAL_TASK_STDERR_CHECK=PASS"
)

missing_status <- setdiff(
  required_status,
  status_lines
)

if (length(missing_status) > 0L) {
  stop(
    "Final validation is incomplete. Missing: ",
    paste(missing_status, collapse = "; "),
    call. = FALSE
  )
}

# ============================================================
# Helpers
# ============================================================

parse_percent <- function(x) {
  as.numeric(
    gsub("\\\\%|%", "", x)
  ) / 100
}

extract_mean <- function(x) {
  as.numeric(
    sub("^\\s*([0-9.]+)\\s*\\(.*$", "\\1", x)
  )
}

extract_sd <- function(x) {
  as.numeric(
    sub("^.*\\(([0-9.]+)\\)\\s*$", "\\1", x)
  )
}

percent_lab <- function(x) {
  paste0(
    formatC(
      100 * x,
      format = "f",
      digits = 0
    ),
    "%"
  )
}

save_figure <- function(plot, name, width, height) {
  pdf_file <- file.path(fig_dir, paste0(name, ".pdf"))
  png_file <- file.path(fig_dir, paste0(name, ".png"))

  ggsave(
    filename = pdf_file,
    plot = plot,
    width = width,
    height = height,
    device = cairo_pdf
  )

  ggsave(
    filename = png_file,
    plot = plot,
    width = width,
    height = height,
    dpi = 360,
    bg = "white"
  )

  invisible(
    c(pdf = pdf_file, png = png_file)
  )
}

method_levels <- c(
  "Bartlett LW",
  "Bartlett sandwich",
  "Exact flat-top",
  "Hard truncation"
)

method_palette <- c(
  "Bartlett LW" = "#1B9E77",
  "Bartlett sandwich" = "#377EB8",
  "Exact flat-top" = "#D95F02",
  "Hard truncation" = "#7570B3"
)

method_shapes <- c(
  "Bartlett LW" = 16,
  "Bartlett sandwich" = 17,
  "Exact flat-top" = 15,
  "Hard truncation" = 18
)

base_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.box = "horizontal"
  )

# ============================================================
# Input files
# ============================================================

f_negative <- file.path(
  table_dir,
  "table_02_negative_eigen_frequency.csv"
)

f_frobenius <- file.path(
  table_dir,
  "table_03_relative_frobenius_error.csv"
)

f_gap <- file.path(
  table_dir,
  "table_06_bs_bl_gap.csv"
)

f_time <- file.path(
  agg_dir,
  "final_timing_by_cell.csv"
)

required_files <- c(
  f_negative,
  f_frobenius,
  f_gap
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0L) {
  stop(
    "Missing required input files: ",
    paste(missing_files, collapse = "; "),
    call. = FALSE
  )
}

# ============================================================
# Figure 1: negative eigenvalue frequency
# ============================================================

negative <- read_csv(
  f_negative,
  show_col_types = FALSE
)

negative_long <- negative %>%
  pivot_longer(
    cols = all_of(method_levels),
    names_to = "Method",
    values_to = "frequency_text"
  ) %>%
  mutate(
    n = as.integer(n),
    Method = factor(Method, levels = method_levels),
    frequency = parse_percent(frequency_text)
  )

fig1 <- ggplot(
  negative_long,
  aes(
    x = n,
    y = frequency,
    colour = Method,
    shape = Method,
    group = Method
  )
) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 2.3) +
  facet_wrap(
    ~ Scenario,
    ncol = 1
  ) +
  scale_x_continuous(
    breaks = c(300, 600, 1200)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = percent_lab,
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  scale_colour_manual(values = method_palette) +
  scale_shape_manual(values = method_shapes) +
  labs(
    x = "Sample size",
    y = "Frequency of negative minimum eigenvalue"
  ) +
  base_theme

save_figure(
  fig1,
  "fig_mc_negative_eigen_frequency",
  width = 7.2,
  height = 7.6
)

# ============================================================
# Figure 2: relative Frobenius error
# ============================================================

frobenius <- read_csv(
  f_frobenius,
  show_col_types = FALSE
)

frobenius_long <- frobenius %>%
  pivot_longer(
    cols = all_of(method_levels),
    names_to = "Method",
    values_to = "value_text"
  ) %>%
  mutate(
    n = as.integer(n),
    Method = factor(Method, levels = method_levels),
    mean_error = extract_mean(value_text),
    sd_error = extract_sd(value_text)
  )

pd <- position_dodge(width = 70)

fig2 <- ggplot(
  frobenius_long,
  aes(
    x = n,
    y = mean_error,
    colour = Method,
    shape = Method,
    group = Method
  )
) +
  geom_line(
    linewidth = 0.85,
    position = pd
  ) +
  geom_point(
    size = 2.3,
    position = pd
  ) +
  geom_errorbar(
    aes(
      ymin = pmax(mean_error - sd_error, 0),
      ymax = mean_error + sd_error
    ),
    width = 35,
    linewidth = 0.4,
    alpha = 0.75,
    position = pd
  ) +
  facet_wrap(
    ~ Scenario,
    ncol = 1
  ) +
  scale_x_continuous(
    breaks = c(300, 600, 1200)
  ) +
  scale_colour_manual(values = method_palette) +
  scale_shape_manual(values = method_shapes) +
  labs(
    x = "Sample size",
    y = "Relative Frobenius error"
  ) +
  base_theme

save_figure(
  fig2,
  "fig_mc_relative_frobenius_error",
  width = 7.2,
  height = 7.6
)

# ============================================================
# Figure 3: BS-BL gap ratio
# ============================================================

gap <- read_csv(
  f_gap,
  show_col_types = FALSE
)

gap_df <- gap %>%
  mutate(
    n = as.integer(n),
    gap_ratio = as.numeric(`Gap / BL error mean`)
  )

fig3 <- ggplot(
  gap_df,
  aes(
    x = n,
    y = gap_ratio,
    group = 1
  )
) +
  geom_line(
    linewidth = 0.85,
    colour = "#2C3E50"
  ) +
  geom_point(
    size = 2.5,
    colour = "#2C3E50"
  ) +
  facet_wrap(
    ~ Scenario,
    ncol = 1
  ) +
  scale_x_continuous(
    breaks = c(300, 600, 1200)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    x = "Sample size",
    y = "Mean BS--BL Frobenius gap divided by BL error"
  ) +
  base_theme +
  theme(
    legend.position = "none"
  )

save_figure(
  fig3,
  "fig_mc_bs_bl_gap_ratio",
  width = 7.2,
  height = 7.6
)

# ============================================================
# Optional Figure 4: computing time
# ============================================================

if (file.exists(f_time)) {
  timing <- read_csv(
    f_time,
    show_col_types = FALSE
  ) %>%
    mutate(
      Scenario = case_when(
        scenario_id == "geom_04" ~ "AR(1), rho = 0.4",
        scenario_id == "geom_08" ~ "AR(1), rho = 0.8",
        scenario_id == "tpl_22" ~ "Truncated linear, beta = 2.2",
        TRUE ~ scenario_id
      ),
      n = as.integer(sample_size),
      mean_seconds = .data[["elapsed_seconds.mean"]],
      p95_seconds = .data[["elapsed_seconds.p95"]]
    )

  fig4 <- ggplot(
    timing,
    aes(
      x = n,
      y = mean_seconds,
      group = 1
    )
  ) +
    geom_line(
      linewidth = 0.85,
      colour = "#7F0000"
    ) +
    geom_point(
      size = 2.5,
      colour = "#7F0000"
    ) +
    geom_errorbar(
      aes(
        ymin = mean_seconds,
        ymax = p95_seconds
      ),
      width = 35,
      linewidth = 0.45,
      alpha = 0.75,
      colour = "#7F0000"
    ) +
    facet_wrap(
      ~ Scenario,
      ncol = 1,
      scales = "free_y"
    ) +
    scale_x_continuous(
      breaks = c(300, 600, 1200)
    ) +
    labs(
      x = "Sample size",
      y = "Elapsed time per replication, seconds"
    ) +
    base_theme +
    theme(
      legend.position = "none"
    )

  save_figure(
    fig4,
    "fig_mc_computing_time",
    width = 7.2,
    height = 7.6
  )
}

# ============================================================
# Manifest and hashes
# ============================================================

generated <- list.files(
  fig_dir,
  pattern = "^fig_mc_.*\\.(pdf|png)$",
  full.names = TRUE
)

manifest <- data.frame(
  file = basename(generated),
  bytes = file.info(generated)$size,
  sha256 = vapply(
    generated,
    digest::digest,
    character(1L),
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  ),
  stringsAsFactors = FALSE
)

manifest <- manifest[
  order(manifest$file),
  ,
  drop = FALSE
]

write_csv(
  manifest,
  file.path(
    fig_dir,
    "final_mc_figures_manifest.csv"
  )
)

status <- c(
  "FINAL_MC_FIGURES_STATUS=PASS",
  paste0("FIGURE_DIR=", fig_dir),
  paste0("FILES_GENERATED=", nrow(manifest)),
  "MAIN_TEXT_FIGURES=fig_mc_negative_eigen_frequency,fig_mc_relative_frobenius_error,fig_mc_bs_bl_gap_ratio",
  "OPTIONAL_FIGURES=fig_mc_computing_time"
)

writeLines(
  status,
  file.path(
    fig_dir,
    "final_mc_figures_status.txt"
  )
)

cat(
  paste0(status, collapse = "\n"),
  "\n"
)
