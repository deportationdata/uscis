library(tidyverse, writexl)

source("code/country_codes.R")
source("code/functions/repair_double_encoded_utf8.R")
source("code/functions/write_xlsx_by_fy.R")
source("code/functions/is_not_blank_or_redacted.R")

daca_df <-
  arrow::open_dataset("data/staging/i821d") |>
  collect() |>
  select(-form_id, -label) |>
  # remove columns that are fully blank (all NA) or fully redacted
  select(where(is_not_blank_or_redacted)) |>
  # add indicator for fully-redacted rows
  mutate(row_redacted = if_all(!c(file_original, row_original), is.na)) |>
  # C3 records store country of birth as a 5-letter code, ELIS records store
  # the full name, so map the codes and keep the names as they are.
  left_join(uscis_country_codes, by = join_by(ben_country_of_birth == code)) |>
  mutate(
    # a C3 value that the code table cannot resolve is not a country code so blank
    ben_country_of_birth = case_when(
      data_source == "ELIS" ~ repair_double_encoded_utf8(ben_country_of_birth),
      !is.na(country) ~ country,
      .default = "Unlisted Entry (Any foreign country not listed)"
    ),
    # recode gender and form type given differences across C3 and ELIS
    ben_gender = replace_values(
      ben_gender,
      "F" ~ "Female",
      "M" ~ "Male",
      "U" ~ "Unknown"
    )
  ) |>
  select(-country) |>
  mutate(
    unique_alien_id_nona = coalesce(
      unique_alien_id,
      paste0("noid_", row_number())
    )
  ) |>
  arrange(unique_alien_id_nona, rec_date, file_original, row_original) |>
  mutate(
    duplicate_identifier = cumsum(coalesce(rec_date != lag(rec_date), TRUE)),
    .by = unique_alien_id_nona
  ) |>
  mutate(
    duplicate_last = row_number() == n(),
    duplicate_likely = if_else(
      is.na(unique_alien_id) | is.na(rec_date),
      NA,
      n() > 1L
    ),
    .by = c(unique_alien_id_nona, duplicate_identifier)
  ) |>
  select(-unique_alien_id_nona) |>
  mutate(
    duplicate_drop_row = coalesce(duplicate_likely, FALSE) &
      !duplicate_last
  ) |>
  relocate(row_redacted, file_original, row_original, .after = last_col())

arrow::write_parquet(daca_df, "data/daca-latest.parquet", compression = "zstd")
write_xlsx_by_fy(daca_df, "data/daca-latest.xlsx", label = "DACA")
