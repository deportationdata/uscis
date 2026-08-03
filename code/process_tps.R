library(tidyverse)

source("code/country_codes.R")

tps_df <-
  arrow::open_dataset("data/staging/i821") |>
  collect() |>
  # C3 records store country of birth as a 5-letter code, ELIS records store
  # the full name, so map the codes and keep the names as they are.
  left_join(uscis_country_codes, by = join_by(ben_country_of_birth == code)) |>
  mutate(
    ben_country_of_birth_original = ben_country_of_birth,
    ben_country_of_birth = coalesce(country, ben_country_of_birth_original),
    # recode gender and form type given differences across C3 and ELIS
    ben_gender = recode_values(
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
  select(-unique_alien_id_nona) |>
  mutate(
    duplicate_drop_row = coalesce(duplicate_likely, FALSE) &
      !duplicate_first
  )

arrow::write_parquet(tps_df, "data/tps.parquet", compression = "zstd")
