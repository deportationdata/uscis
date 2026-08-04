# Read benefits applications form data and stage under data/staging/<form_id>/ so
# arrow can later treat each form's parts as a single dataset.
#
# Any .xlsb sources must be converted to .xlsx first by running
# `Rscript code/convert_xlsb.R`. This script reads only .xlsx.

library(dplyr)
library(purrr)
library(tidyr)
library(readxl)
library(furrr)
library(stringr)

source("code/functions/check_dttm_and_convert_to_date.R")

release_dir <- "inputs/mukherjee-v-uscis"
out_dir <- "data/staging"

date_cols <- c(
  "status_date",
  "natz_test_date",
  "intv_date",
  "natz_date",
  "lpr_date",
  "rec_date",
  "decision_date",
  "latest_notice_date",
  "latest_rfe_date",
  "latest_noid_date"
)

year_cols <- c(
  "ben_year_of_birth",
  "latest_trvl_depart_yr",
  "latest_trvl_return_yr"
)

# "null" is USCIS's NA marker; the rest are FOIA-exemption redaction codes.
na_strings <- c("null", "(b)(6)", "(b)(3) (b)(6) (b)(7)(c)")

# form_id -> filename prefix and human-readable label
benefit_forms <- tribble(
  ~form_id , ~pattern             , ~label                     ,
  "i821"   , "PAER0021096_i821_"  , "I-821 TPS applications"   ,
  "i821d"  , "PAER0021096_i821d_" , "I-821D DACA applications"
)

benefits_per_file <-
  benefit_forms |>
  mutate(
    file_original = map(pattern, \(prefix) {
      # Restrict to .xlsx so .xlsb (converted by code/convert_xlsb.R)
      # aren't picked up twice.
      list.files(
        release_dir,
        pattern = paste0(prefix, ".*\\.xlsx$"),
        full.names = TRUE,
        recursive = TRUE
      )
    })
  ) |>
  select(form_id, label, file_original) |>
  unnest(file_original)

walk(
  file.path(out_dir, benefit_forms$form_id),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)

# Each worker reads a source file and writes its own parquet into
# data/staging/<form_id>/, so the parts can be read back as one dataset.
plan(multisession)

future_pwalk(benefits_per_file, \(form_id, label, file_original) {
  # Type every column explicitly so the per-file parquets share a schema —
  # otherwise readxl's "guess" turns an all-NA column into logical in one
  # file and character in another, and arrow::open_dataset can't union them.
  cols <- names(read_excel(file_original, n_max = 0))
  col_types <- case_when(
    cols %in% date_cols ~ "date",
    cols %in% year_cols ~ "numeric",
    TRUE ~ "text"
  )

  out_file <- file.path(
    out_dir,
    form_id,
    file_original |>
      tools::file_path_sans_ext() |>
      basename() |>
      str_replace(" ", "_") |>
      str_c(".parquet")
  )

  read_excel(file_original, col_types = col_types, na = na_strings) |>
    mutate(across(any_of(year_cols), as.integer)) |>
    mutate(across(
      where(\(x) inherits(x, "POSIXt")),
      check_dttm_and_convert_to_date
    )) |>
    # NB: these are from the pwalk arguments, not the source file.
    mutate(
      form_id = form_id,
      label = label,
      file_original = file_original,
      row_original = row_number()
    ) |>
    arrow::write_parquet(
      out_file,
      compression = "zstd"
    )
})

plan(sequential)
gc()
