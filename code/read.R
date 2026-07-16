library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(readxl)
library(furrr)

# Any .xlsb sources in release_dir must be converted to .xlsx first by
# running `Rscript code/convert_xlsb.R`. This script reads only .xlsx.
source("code/functions/check_dttm_and_convert_to_date.R")
source("code/column_types.R")

plan(multisession)

release_dir <- "inputs/mukherjee-v-uscis"

# form_id -> filename prefix and human-readable label
benefit_forms <- tribble(
  ~form_id , ~pattern             , ~label                     ,
  "i821"   , "PAER0021096_i821_"  , "I-821 TPS applications"   ,
  "i821d"  , "PAER0021096_i821d_" , "I-821D DACA applications"
)

benefits_per_file <-
  benefit_forms |>
  mutate(
    file_original = map(pattern, \(pat) {
      # Restrict to .xlsx so .xlsb siblings (converted by code/convert_xlsb.R)
      # aren't picked up twice.
      list.files(
        release_dir,
        full.names = TRUE,
        recursive = TRUE,
        pattern = paste0(pat, ".*\\.xlsx$")
      )
    })
  ) |>
  select(form_id, label, file_original) |>
  unnest(file_original)

# Each worker reads ONE excel file, cleans it, and writes its own parquet
# to a per-form_id staging subdir. The parts for each form are stitched
# into a single per-form parquet below.
out_dir <- tempfile("benefit_applications_")
for (fid in unique(benefits_per_file$form_id)) {
  dir.create(file.path(out_dir, fid), recursive = TRUE)
}

future_pwalk(benefits_per_file, \(form_id, label, file_original) {
  cols <- names(read_excel(file_original, n_max = 0))
  # Type every column explicitly so the per-file parquets share a schema —
  # otherwise readxl's "guess" turns an all-NA column into logical in one
  # file and character in another, and arrow::open_dataset can't union them.
  types <- case_when(
    cols %in% date_cols ~ "date",
    cols %in% year_cols ~ "numeric",
    TRUE ~ "text"
  )
  read_excel(file_original, col_types = types, na = na_strings) |>
    mutate(
      form_id = form_id,
      label = label,
      file_original = file_original,
      row_original = row_number()
    ) |>
    mutate(across(any_of(year_cols), as.integer)) |>
    mutate(across(
      where(\(x) inherits(x, "POSIXt")),
      check_dttm_and_convert_to_date
    )) |>
    relocate(form_id, label) |>
    relocate(file_original, row_original, .after = last_col()) |>
    arrow::write_parquet(
      file.path(
        out_dir,
        form_id,
        paste0(
          gsub(" ", "_", basename(tools::file_path_sans_ext(file_original))),
          ".parquet"
        )
      ),
      compression = "zstd",
      compression_level = 13
    )
})

plan(sequential)
gc()

for (fid in unique(benefits_per_file$form_id)) {
  arrow::open_dataset(file.path(out_dir, fid)) |>
    dplyr::collect() |>
    arrow::write_parquet(
      file.path("data", paste0(fid, ".parquet")),
      compression = "zstd"
    )
}

# delete all the interim files
unlink(out_dir, recursive = TRUE)
