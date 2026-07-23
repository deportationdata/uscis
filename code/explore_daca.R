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
  mutate(fully_redacted = is.na(unique_alien_id),
      file_original = factor(file_original, file_order),
      rec_year = year(rec_date),
      rec_month = zoo::as.yearmon(rec_date),
      dec_year = year(decision_date),
      dec_month = zoo::as.yearmon(decision_date))

# For checking for overlap of IDs between datasets. A small number show up in both.

daca_ids <- unique(daca$unique_alien_id)

save(daca_ids, file = "data/daca_ids.RData")

fully_redacted_tbl <- daca |> 
  count(file_original, fully_redacted) |> 
  pivot_wider(names_from = "fully_redacted", values_from = "n")

p1 <- daca |> 
  count(file_original, fully_redacted) |> 
  ggplot(aes(x = file_original, y = n, fill = fully_redacted)) +
  geom_col()

p1

p2 <- daca |> 
  count(file_original, rec_year) |> 
  ggplot(aes(x = file_original, y = n, fill = rec_year)) +
  geom_col()

p2

p2.1 <- daca |> 
  count(file_original, dec_year) |> 
  ggplot(aes(x = file_original, y = n, fill = dec_year)) +
  geom_col()

p2.1

p3 <- daca |> 
  count(file_original, data_source) |> 
  ggplot(aes(x = file_original, y = n, fill = data_source)) +
  geom_col()

p3

# # Do files contain any hidden structure based on row position? This is a bit heavy for processing.

# p4 <- daca |> 
#   ggplot(aes(x = rec_date, y = row_original, color = file_original)) +
#   geom_point() +
#   facet_wrap(~file_original)

# p4

top_elis_country <- daca |> 
  filter(data_source == "ELIS") |> 
  count(ben_country_of_birth) |> 
  arrange(desc(n))

dat <- daca |> 
  filter(data_source == "ELIS")

p5 <- dat |>
  mutate(ben_country_of_birth = case_when(
ben_country_of_birth %in% head(top_elis_country$ben_country_of_birth, 10) ~ ben_country_of_birth,
TRUE ~ "All others"
  )) |> 
  count(rec_year, ben_country_of_birth) |> 
  ggplot(aes(x = rec_year, y = n, fill = ben_country_of_birth)) +
  geom_col()

p5

top_c3_country <- daca |> 
  filter(data_source == "C3") |> 
  count(ben_country_of_birth) |> 
  arrange(desc(n))

dat <- daca |> 
  filter(data_source == "C3")

p6 <- dat |>
  mutate(ben_country_of_birth = case_when(
ben_country_of_birth %in% head(top_c3_country$ben_country_of_birth, 10) ~ ben_country_of_birth,
TRUE ~ "All others"
  )) |> 
  count(rec_year, ben_country_of_birth) |> 
  ggplot(aes(x = rec_year, y = n, fill = ben_country_of_birth)) +
  geom_col()

p6

p7 <- daca |> 
  count(dec_year, decision) |> 
  ggplot(aes(x = dec_year, y = n, fill = decision)) +
  geom_col()

p7

p7.1 <- daca |> 
  count(dec_month, decision) |> 
  ggplot(aes(x = dec_month, y = n, fill = decision)) +
  geom_col()

p7.1

p8 <- daca |> 
  count(rec_year, form_type ) |> 
    ggplot(aes(x = rec_year, y = n, fill = form_type)) +
  geom_col()

p8

p8.1 <- daca |> 
  count(rec_month, form_type ) |> 
    ggplot(aes(x = rec_month, y = n, fill = form_type)) +
  geom_col()

p8.1