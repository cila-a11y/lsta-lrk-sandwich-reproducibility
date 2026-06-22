#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readxl)
  library(readr)
  library(dplyr)
})

project_dir <- Sys.getenv("PROJECT_DIR")
results_dir <- Sys.getenv("RESULTS_DIR")

data_file <- file.path(
  project_dir,
  "repository_work",
  "data",
  "dados_publicos_ipma_para_R.xlsx"
)

out_dir <- file.path(
  results_dir,
  "real_application",
  "date_audit"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

raw_default <- readxl::read_excel(
  data_file,
  sheet = "long_daily_lisbon_temp"
)

raw_text <- readxl::read_excel(
  data_file,
  sheet = "long_daily_lisbon_temp",
  col_types = "text"
)

date_default <- raw_default$date
date_text <- raw_text$date

audit <- data.frame(
  item = c(
    "n_rows",
    "default_date_class",
    "text_date_class",
    "first_default_dates",
    "first_text_dates",
    "last_default_dates",
    "last_text_dates",
    "source_url_first",
    "source_url_last"
  ),
  value = c(
    as.character(nrow(raw_default)),
    paste(class(date_default), collapse = ", "),
    paste(class(date_text), collapse = ", "),
    paste(head(as.character(date_default), 10), collapse = " | "),
    paste(head(as.character(date_text), 10), collapse = " | "),
    paste(tail(as.character(date_default), 10), collapse = " | "),
    paste(tail(as.character(date_text), 10), collapse = " | "),
    as.character(raw_default$source_url[1]),
    as.character(raw_default$source_url[nrow(raw_default)])
  )
)

readr::write_csv(
  audit,
  file.path(out_dir, "lisbon_temperature_date_audit.csv")
)

capture.output(
  print(audit, row.names = FALSE),
  file = file.path(out_dir, "lisbon_temperature_date_audit.txt")
)

# Check whether a daily reconstruction from 1855-01-01 to 2018-08-31
# matches the number of rows in the worksheet.
date_reconstructed <- seq(
  as.Date("1855-01-01"),
  as.Date("2018-08-31"),
  by = "day"
)

reconstruction <- data.frame(
  item = c(
    "reconstructed_start",
    "reconstructed_end",
    "reconstructed_length",
    "worksheet_rows",
    "length_matches"
  ),
  value = c(
    as.character(min(date_reconstructed)),
    as.character(max(date_reconstructed)),
    as.character(length(date_reconstructed)),
    as.character(nrow(raw_default)),
    as.character(length(date_reconstructed) == nrow(raw_default))
  )
)

readr::write_csv(
  reconstruction,
  file.path(out_dir, "lisbon_temperature_reconstruction_check.csv")
)

writeLines(
  c(
    "LISBON_TEMPERATURE_DATE_AUDIT_STATUS=PASS",
    paste0("N_ROWS=", nrow(raw_default)),
    paste0("RECONSTRUCTED_LENGTH=", length(date_reconstructed)),
    paste0("LENGTH_MATCHES=", length(date_reconstructed) == nrow(raw_default))
  ),
  file.path(out_dir, "lisbon_temperature_date_audit_status.txt")
)

cat("LISBON_TEMPERATURE_DATE_AUDIT_STATUS=PASS\n")
cat("N_ROWS=", nrow(raw_default), "\n", sep = "")
cat("RECONSTRUCTED_LENGTH=", length(date_reconstructed), "\n", sep = "")
cat("LENGTH_MATCHES=", length(date_reconstructed) == nrow(raw_default), "\n", sep = "")
