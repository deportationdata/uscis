library(tidyverse)

source("code/country_codes.R")
source("code/functions/repair_double_encoded_utf8.R")
source("code/functions/write_xlsx_by_fy.R")

daca_df <-
  arrow::open_dataset("data/staging/i821d") |>
  collect() |>
  # C3 records store country of birth as a 5-letter code, ELIS records store
  # the full name, so map the codes and keep the names as they are.
  left_join(uscis_country_codes, by = join_by(ben_country_of_birth == code)) |>
  mutate(
    # a C3 value that the code table cannot resolve is not a country code so blank
    ben_country_of_birth = case_when(
      data_source == "ELIS" ~ repair_double_encoded_utf8(ben_country_of_birth),
      !is.na(country) ~ country,
      .default = NA_character_
    ),
    # recode gender and form type given differences across C3 and ELIS
    ben_gender = replace_values(
      ben_gender,
      "F" ~ "Female",
      "M" ~ "Male",
      "U" ~ "Unknown"
    ),
    form_type = recode_values(
      form_type,
      "This is my annual registration/re-registration application" ~ "TPS - Re-registration",
      "This is my first application" ~ "TPS - Initial"
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
    duplicate_first = row_number() == 1L,
    duplicate_likely = if_else(
      is.na(unique_alien_id) | is.na(rec_date),
      NA,
      n() > 1L
    ),
    .by = c(unique_alien_id_nona, duplicate_identifier)
  ) |>
  mutate(
    duplicate_drop_row = coalesce(duplicate_likely, FALSE) &
      !duplicate_first
  ) |>
  relocate(form_id, label, file_original, row_original, .after = last_col())

arrow::write_parquet(daca_df, "data/daca.parquet", compression = "zstd")
write_xlsx_by_fy(daca_df, "data/daca.xlsx", label = "DACA")
