library(pacman)

p_load(tidyverse)

tps <- arrow::read_parquet("data/i821.parquet")

file_order <- c(
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821_1 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821_2 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821_3 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821_4 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821_5 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821_6 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821_7 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821_8 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821_9 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821_10 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821_11 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821_12 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821_13 redacted.xlsx",
"inputs/mukherjee-v-uscis/production1/PAER0021096_i821_14 redacted.xlsx"
)

tps <- tps |> 
  mutate(fully_redacted = is.na(unique_alien_id),
      file_original = factor(file_original, file_order),
      rec_year = year(rec_date),
      rec_month = zoo::as.yearmon(rec_date),
      dec_year = year(decision_date),
      dec_month = zoo::as.yearmon(decision_date))

# For checking for overlap of IDs between datasets. A small number show up in both.

tps_ids <- unique(tps$unique_alien_id)

save(tps_ids, file = "data/tps_ids.RData")

fully_redacted_tbl <- tps |> 
  count(file_original, fully_redacted) |> 
  pivot_wider(names_from = "fully_redacted", values_from = "n")

p1 <- tps |> 
  count(file_original, fully_redacted) |> 
  ggplot(aes(x = file_original, y = n, fill = fully_redacted)) +
  geom_col()

p1

p2 <- tps |> 
  count(file_original, rec_year) |> 
  ggplot(aes(x = file_original, y = n, fill = rec_year)) +
  geom_col()

p2

p2.1 <- tps |> 
  count(file_original, dec_year) |> 
  ggplot(aes(x = file_original, y = n, fill = dec_year)) +
  geom_col()

p2.1

p3 <- tps |> 
  count(file_original, data_source) |> 
  ggplot(aes(x = file_original, y = n, fill = data_source)) +
  geom_col()

p3

# # Do files contain any hidden structure based on row position? This is a bit heavy for processing.

# p4 <- tps |> 
#   ggplot(aes(x = rec_date, y = row_original, color = file_original)) +
#   geom_point() +
#   facet_wrap(~file_original)

# p4

###

tps_ven <- tps |> 
  filter(ben_country_of_birth %in% c("Venezuela", "VENEZ"))

# All VEN TPS applications processed in ELIS

p4 <- tps_ven |> 
  filter(rec_date >= "2021-01-01") |> 
  count(rec_month, data_source) |> 
  ggplot(aes(x = rec_month, y = n, fill = data_source)) +
  geom_col()

p4

p5 <- tps_ven |> 
  filter(rec_date >= "2021-01-01") |> 
  count(rec_month, form_type) |> 
  ggplot(aes(x = rec_month, y = n, fill = form_type)) +
  geom_col(position="stack")

p5

p6 <- tps_ven |> 
  filter(rec_date >= "2021-01-01") |> 
  count(rec_month, decision) |> 
  ggplot(aes(x = rec_month, y = n, fill = decision)) +
  geom_col(position="stack")

p6

p6.1 <- tps_ven |> 
  filter(rec_date >= "2021-01-01") |> 
  count(dec_month, decision) |> 
  ggplot(aes(x = dec_month, y = n, fill = decision)) +
  geom_col(position="stack")

p6.1

p7 <- tps_ven |> 
  filter(rec_date >= "2021-01-01") |> 
  count(rec_month, uscis_service_center) |> 
  ggplot(aes(x = rec_month, y = n, fill = uscis_service_center)) +
  geom_col(position="stack")

p7