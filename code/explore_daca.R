library(pacman)

p_load(tidyverse)

daca <- arrow::read_parquet("data/i821d.parquet")

file_order <- c(
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821d_1 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821d_2 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821d_3 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821d_4 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821d_5 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821d_6 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821d_7 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821d_8 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821d_9 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821d_10 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821d_11 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821d_12 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821d_13 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821d_14 redacted.xlsx"
)

daca <- daca |> 
  mutate(fully_redacted = if_all(c(unique_alien_id, form_number, form_type), is.na),
      file_original = factor(file_original, file_order),
      rec_year = year(rec_date),
      rec_month = zoo::as.yearmon(rec_date),
      dec_year = year(decision_date),
      dec_month = zoo::as.yearmon(decision_date))

# For checking for overlap of IDs between datasets. A small number show up in both.

daca_ids <- unique(daca$unique_alien_id)

save(daca_ids, file = "data/daca_ids.RData")

# Count fuly redacted rows per original file

fully_redacted_tbl <- daca |> 
  count(file_original, fully_redacted) |> 
  pivot_wider(names_from = "fully_redacted", values_from = "n")

# Fully redacted rows are not distributed evenly across original files

p1 <- daca |> 
  count(file_original, fully_redacted) |> 
  ggplot(aes(x = file_original, y = n, fill = fully_redacted)) +
  geom_col()

p1

# We can't know for sure because `data_source` is among redacted fields,
# but note fully-redacted records are only present in original files which include
# records drawn from ELIS system

p2 <- daca |> 
  count(file_original, data_source) |> 
  ggplot(aes(x = file_original, y = n, fill = data_source)) +
  geom_col()

p2

# Count missing unique IDs per original file after dropping fully-redacted rows
# Most records missing unique ID are in file 5; > CY 2021

missing_ID_tbl <- daca |> 
  filter(fully_redacted == FALSE) |>
  mutate(missing_ID = is.na(unique_alien_id)) |> 
  count(file_original, missing_ID) |> 
  pivot_wider(names_from = "missing_ID", values_from = "n")

p3 <- daca |> 
  filter(fully_redacted == FALSE) |>
  mutate(missing_ID = is.na(unique_alien_id)) |> 
  count(file_original, missing_ID) |> 
  ggplot(aes(x = file_original, y = n, fill = missing_ID)) +
  geom_col()

p3

p3.1 <- daca |> 
  filter(fully_redacted == FALSE) |>
  mutate(missing_ID = is.na(unique_alien_id)) |> 
  count(rec_year, missing_ID) |> 
  ggplot(aes(x = rec_year, y = n, fill = missing_ID)) +
  geom_col()

p3.1

# Original files are structured approximately sequentially, with exception of
# older records drawn from C3 system in first and latter files

p4 <- daca |> 
  count(file_original, rec_year) |> 
  ggplot(aes(x = file_original, y = n, fill = rec_year)) +
  geom_col()

p4

p4.1 <- daca |> 
  count(file_original, dec_year) |> 
  ggplot(aes(x = file_original, y = n, fill = dec_year)) +
  geom_col()

p4.1

# # Do files contain any hidden structure based on row position? This is a bit heavy for processing.
# # Mostly we just see that files are limited by date range but with some exceptions.

# p5 <- daca |> 
#   ggplot(aes(x = rec_date, y = row_original, color = file_original)) +
#   geom_point() +
#   facet_wrap(~file_original)

# p5

### Substantive analysis

apps_daily <- daca |>
  filter(!is.na(rec_date)) |> 
  count(rec_date) |> 
  ggplot(aes(x = rec_date, y = n)) +
  geom_line() +
  ylim(0, NA) +
  geom_vline(
    xintercept = as.Date("2017-09-05"),
    lty = "dashed",
    col = "red"
  ) +
    geom_vline(
    xintercept = as.Date("2018-01-09"),
    lty = "dashed",
    col = "blue"
  ) +
    annotate(
    "text",
    x = as.Date("2017-09-05")-30,
    y = 10000,
    label = "Trump admin.\nannounces termination\nof DACA",
    hjust = 1,
    size = 4,
    col = "red"
  ) +
  annotate(
    "text",
    x = as.Date("2018-01-09")+30,
    y = 10000,
    label = "USCIS required to\nresume accepting\ncertain DACA applications",
    hjust = 0,
    size = 4,
    col = "blue"
  ) +
  labs(title = "DACA applications over time")

apps_daily

# DACA applications have an approximate two year cycle based on re-application timeline

apps_yearly <- daca |>
  filter(!is.na(rec_date)) |> 
  count(rec_year) |> 
  ggplot(aes(x = rec_year, y = n)) +
  geom_line() +
  ylim(0, NA)

apps_yearly

# Top country of birth of applicants
# Split by data source for now until values standardized

top_elis_country <- daca |> 
  filter(data_source == "ELIS") |> 
  count(ben_country_of_birth) |> 
  arrange(desc(n))

dat <- daca |> 
  filter(data_source == "ELIS")

elis_country_of_birth <- dat |>
  mutate(ben_country_of_birth = case_when(
ben_country_of_birth %in% head(top_elis_country$ben_country_of_birth, 10) ~ ben_country_of_birth,
TRUE ~ "All others"
  )) |> 
  count(rec_year, ben_country_of_birth) |> 
  ggplot(aes(x = rec_year, y = n, fill = ben_country_of_birth)) +
  geom_col()

elis_country_of_birth

top_c3_country <- daca |> 
  filter(data_source == "C3") |> 
  count(ben_country_of_birth) |> 
  arrange(desc(n))

dat <- daca |> 
  filter(data_source == "C3")

c3_country_of_birth <- dat |>
  mutate(ben_country_of_birth = case_when(
ben_country_of_birth %in% head(top_c3_country$ben_country_of_birth, 10) ~ ben_country_of_birth,
TRUE ~ "All others"
  )) |> 
  count(rec_year, ben_country_of_birth) |> 
  ggplot(aes(x = rec_year, y = n, fill = ben_country_of_birth)) +
  geom_col()

c3_country_of_birth

daca_decisions_by_dec_year <- daca |> 
  count(dec_year, decision) |> 
  ggplot(aes(x = dec_year, y = n, fill = decision)) +
  geom_col()

daca_decisions_by_dec_year

daca_decisions_by_dec_year_month <- daca |> 
  count(dec_month, decision) |> 
  ggplot(aes(x = dec_month, y = n, fill = decision)) +
  geom_col()

daca_decisions_by_dec_year_month

daca_decisions_by_rec_year <- daca |> 
  count(rec_year, form_type ) |> 
    ggplot(aes(x = rec_year, y = n, fill = form_type)) +
  geom_col()

daca_decisions_by_rec_year

daca_decisions_by_rec_year_month <- daca |> 
  count(rec_month, form_type ) |> 
    ggplot(aes(x = rec_month, y = n, fill = form_type)) +
  geom_col()

daca_decisions_by_rec_year_month