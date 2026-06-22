#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(readr)
  library(digest)
})

project_dir <- Sys.getenv("PROJECT_DIR")
results_dir <- Sys.getenv("RESULTS_DIR")

if (project_dir == "" || results_dir == "") {
  stop("PROJECT_DIR and RESULTS_DIR must be defined.", call. = FALSE)
}

repo_dir <- file.path(project_dir, "repository_work")
data_dir <- file.path(repo_dir, "data")
out_dir <- file.path(results_dir, "real_application", "audit")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

data_files <- list.files(
  data_dir,
  pattern = "\\.(xlsx|csv|rds|RDS)$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(data_files) == 0L) {
  stop("No data files found in repository_work/data.", call. = FALSE)
}

file_inventory <- data.frame(
  file = basename(data_files),
  path = data_files,
  extension = tools::file_ext(data_files),
  bytes = file.info(data_files)$size,
  sha256 = vapply(
    data_files,
    digest::digest,
    character(1L),
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  ),
  stringsAsFactors = FALSE
)

readr::write_csv(
  file_inventory,
  file.path(out_dir, "real_data_file_inventory.csv")
)

xlsx_files <- data_files[
  grepl("\\.xlsx$", data_files, ignore.case = TRUE)
]

sheet_rows <- list()
preview_rows <- list()

for (file in xlsx_files) {
  sheets <- readxl::excel_sheets(file)

  for (sheet in sheets) {
    dat <- readxl::read_excel(file, sheet = sheet, n_max = 30)

    sheet_rows[[length(sheet_rows) + 1L]] <- data.frame(
      file = basename(file),
      sheet = sheet,
      n_preview_rows = nrow(dat),
      n_columns = ncol(dat),
      column_names = paste(names(dat), collapse = " | "),
      stringsAsFactors = FALSE
    )

    if (nrow(dat) > 0L) {
      preview <- as.data.frame(dat)
      preview$.file <- basename(file)
      preview$.sheet <- sheet
      preview_rows[[length(preview_rows) + 1L]] <- preview
    }
  }
}

if (length(sheet_rows) > 0L) {
  sheet_audit <- dplyr::bind_rows(sheet_rows)

  readr::write_csv(
    sheet_audit,
    file.path(out_dir, "real_data_sheet_audit.csv")
  )

  capture.output(
    print(sheet_audit),
    file = file.path(out_dir, "real_data_sheet_audit.txt")
  )
}

if (length(preview_rows) > 0L) {
  preview <- dplyr::bind_rows(preview_rows)

  readr::write_csv(
    preview,
    file.path(out_dir, "real_data_preview.csv")
  )
}

writeLines(
  c(
    "REAL_DATA_AUDIT_STATUS=PASS",
    paste0("N_DATA_FILES=", length(data_files)),
    paste0("N_XLSX_FILES=", length(xlsx_files)),
    paste0("AUDIT_DIR=", out_dir)
  ),
  con = file.path(out_dir, "real_data_audit_status.txt")
)

cat("REAL_DATA_AUDIT_STATUS=PASS\n")
cat("AUDIT_DIR=", out_dir, "\n", sep = "")
