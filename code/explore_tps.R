library(pacman)

p_load(tidyverse, digest)

options(scipen = 100)

vdigest <- Vectorize(digest)

tps <- arrow::read_parquet("data/i821_dedupe.parquet")

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

ben_country_of_birth <- read_delim("data/ben_country_of_birth.csv", delim=',')

ben_country_of_birth_lookup <- set_names(as.list(ben_country_of_birth$ben_country_of_birth_ELIS), ben_country_of_birth$ben_country_of_birth_C3)

tps <- tps |> 
  mutate(
      file_original = factor(file_original, file_order),
      ben_country_of_birth_original = ben_country_of_birth,
      ben_country_of_birth = recode(ben_country_of_birth, !!!ben_country_of_birth_lookup),
      ben_gender_original = ben_gender,
      ben_gender = case_when(ben_gender == "M" ~ "Male",
                             ben_gender == "F" ~ "Female",
                             ben_gender == "U" ~ "Unknown",
                            TRUE ~ ben_gender),
      rec_year = year(rec_date),
      rec_month = zoo::as.yearmon(rec_date),
      dec_year = year(decision_date),
      dec_month = zoo::as.yearmon(decision_date))

# Count missing unique IDs per original file (dupes and fully-redacted rows have been dropped)

missing_ID_tbl <- tps |> 
  mutate(missing_ID = is.na(unique_alien_id)) |> 
  count(file_original, missing_ID) |> 
  pivot_wider(names_from = "missing_ID", values_from = "n")

p3 <- tps |> 
  mutate(missing_ID = is.na(unique_alien_id)) |>
  filter(missing_ID == TRUE) |> 
  count(file_original, missing_ID) |> 
  ggplot(aes(x = file_original, y = n, fill = missing_ID)) +
  geom_col()

p3

p3.1 <- tps |> 
  mutate(missing_ID = is.na(unique_alien_id)) |> 
  filter(missing_ID == TRUE) |> 
  count(rec_year, missing_ID) |> 
  ggplot(aes(x = rec_year, y = n, fill = missing_ID)) +
  geom_col()

p3.1

# Original files are structured approximately sequentially, with exception of
# older records drawn from C3 system latter files

p4 <- tps |> 
  count(file_original, rec_year) |> 
  ggplot(aes(x = file_original, y = n, fill = rec_year)) +
  geom_col()

p4

p4.1 <- tps |> 
  count(file_original, dec_year) |> 
  ggplot(aes(x = file_original, y = n, fill = dec_year)) +
  geom_col()

p4.1

# # Do files contain any hidden structure based on row position? This is a bit heavy for processing.
# # Mostly we just see that files are limited by date range but with some exceptions.

# p5 <- tps |> 
#   ggplot(aes(x = rec_date, y = row_original, color = file_original)) +
#   geom_point() +
#   facet_wrap(~file_original)

# p5

### Substantive analysis

tps_ven <- tps |> 
  filter(!is.na(unique_alien_id),
    ben_country_of_birth %in% c("Venezuela", "VENEZ"))

# Almost all VEN TPS applications processed in ELIS

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
  geom_line()

p5

p6 <- tps_ven |> 
  filter(rec_date >= "2021-01-01") |> 
  count(rec_month, decision) |> 
  ggplot(aes(x = rec_month, y = n, fill = decision)) +
  geom_line()

p6

p6.1 <- tps_ven |> 
  filter(rec_date >= "2021-01-01") |> 
  count(dec_month, decision) |> 
  ggplot(aes(x = dec_month, y = n, fill = decision)) +
  geom_line()

p6.1

p7 <- tps_ven |> 
  filter(rec_date >= "2021-01-01") |> 
  count(rec_month, uscis_service_center) |> 
  ggplot(aes(x = rec_month, y = n, fill = uscis_service_center)) +
  geom_line()

p7

# MPI: "As of January 2025, roughly 607,000 Venezuelans were covered by Temporary Protected Status (TPS)"
# https://www.migrationpolicy.org/journal/spotlight/venezuelan-immigrants-united-states

dat <- tps_ven |> 
  filter(form_type %in% c("TPS - Initial", "This is my first application")) |> 
  count(dec_month, decision) |> 
  mutate(total_tps = cumsum(n), .by=decision)

p8 <- dat |> 
  filter(dec_month > "2020-12-01") |> 
  ggplot(aes(x = dec_month, y = total_tps, color = decision, group=decision)) +
  geom_line()

p8

rm(tps_ven)

# Haiti

# Over 330,000 Haitians with TPS per AIC: https://www.americanimmigrationcouncil.org/fact-sheet/temporary-protected-status-tps-overview/

tps_hai <- tps |> 
  filter(!is.na(unique_alien_id),
    ben_country_of_birth %in% c("Haiti", "HAITI"))

p5 <- tps_hai |> 
  # filter(rec_date >= "2021-01-01") |> 
  count(rec_month, form_type) |> 
  ggplot(aes(x = rec_month, y = n, color = form_type)) +
  geom_line()

p5

p6 <- tps_hai |> 
  # filter(rec_date >= "2021-01-01") |> 
  count(rec_month, decision) |> 
  ggplot(aes(x = rec_month, y = n, color = decision)) +
  geom_line()

p6

dat <- tps_hai |> 
  filter(form_type %in% c("TPS - Initial", "This is my first application")) |> 
  count(dec_month, decision) |> 
  mutate(total_tps = cumsum(n), .by=decision)

p9 <- dat |> 
  ggplot(aes(x = dec_month, y = total_tps, color = decision, group=decision)) +
  geom_line()

p9

rm(tps_hai)

# Honduras

# Over 50,000 Hondurans with TPS per AIC: https://www.americanimmigrationcouncil.org/fact-sheet/temporary-protected-status-tps-overview/
# Numbers below don't line up with this when filtering for initial applications because first TPS designation was in 1999

honduras_vals <- c("Honduras", "2HOND", "H0NDU", "HNDU", "HODNU", "HODU", "HOND", "HONDI", "HONDU", "HONOD", "HONU", "HONUD", "HOUDU", "HOUND", "UHOND")

tps_hon <- tps |> 
  filter(!is.na(unique_alien_id),
    ben_country_of_birth %in% honduras_vals)

p5 <- tps_hon |> 
  # filter(rec_date >= "2021-01-01") |> 
  count(rec_month, form_type) |> 
  ggplot(aes(x = rec_month, y = n, color = form_type)) +
  geom_line()

p5

p6 <- tps_hon |> 
  # filter(rec_date >= "2021-01-01") |> 
  count(rec_month, decision) |> 
  ggplot(aes(x = rec_month, y = n, color = decision)) +
  geom_line()

p6

dat <- tps_hon |> 
  # filter(form_type %in% c("TPS - Initial", "This is my first application")) |> 
  count(dec_month, decision) |> 
  mutate(total_tps = cumsum(n), .by=decision)

p9 <- dat |> 
  ggplot(aes(x = dec_month, y = total_tps, color = decision, group=decision)) +
  geom_line()

p9

rm(tps_hon)

# Ukraine

# 

tps_ukr <- tps |> 
  filter(!is.na(unique_alien_id),
    ben_country_of_birth %in% c("Ukraine", "UKRAI"))

p5 <- tps_ukr |> 
  # filter(rec_date >= "2021-01-01") |> 
  count(rec_month, form_type) |> 
  ggplot(aes(x = rec_month, y = n, color = form_type)) +
  geom_line()

p5

p6 <- tps_ukr |> 
  # filter(rec_date >= "2021-01-01") |> 
  count(rec_month, decision) |> 
  ggplot(aes(x = rec_month, y = n, color = decision)) +
  geom_line()

p6

dat <- tps_ukr |> 
  filter(form_type %in% c("TPS - Initial", "This is my first application")) |> 
  count(dec_month, decision) |> 
  mutate(total_tps = cumsum(n), .by=decision)

p9 <- dat |> 
  ggplot(aes(x = dec_month, y = total_tps, color = decision, group=decision)) +
  geom_line()

p9

rm(tps_ukr)

###

# TPS totals per CRS: https://www.congress.gov/crs-product/RS20844
# Could add relevant dates here as well

tps_crs <- tribble(
  ~country, ~total_tps_crs,
  "Afghanistan", 8105,
  "Burma", 3670,
  "Cameroon", 4920,
  "El Salvador", 170125,
  "Ethiopia", 4540,
  "Haiti", 330735,
  "Honduras", 51225,
  "Lebanon", 140,
  "Nepal", 7160,
  "Nicaragua", 2910,
  "Somalia", 705,
  "South Sudan", 210,
  "Sudan", 1790,
  "Syria", 3860,
  "Ukraine", 101150,
  "Venezuela", 605015,
  "Yemen", 1380
) |> 
  mutate(total_tps_crs = as.integer(total_tps_crs))

# Check TPS total approved applicants per country
# Ideally separately for each data source and both, compare across
# What makes up "All other countries"?

dat1 <- tps |> 
  filter(decision == "Approved",
         data_source == "ELIS"
        ) |> 
  distinct(unique_alien_id, .keep_all = TRUE) |> 
  mutate(ben_country_of_birth =
  case_when(ben_country_of_birth %in% c("Afghanistan", "AFGHA") ~ "Afghanistan",
            ben_country_of_birth %in% c("Burma", "BURMA") ~ "Burma",
            ben_country_of_birth %in% c("Cameroon", "CAMER") ~ "Cameroon",
            ben_country_of_birth %in% c("El Salvador", "ELSAL") ~ "El Salvador",
            ben_country_of_birth %in% c("Ethiopia", "ETHIO") ~ "Ethiopia",
            ben_country_of_birth %in% c("Haiti", "HAITI") ~ "Haiti",
            ben_country_of_birth %in% c("Honduras", "HONDU") ~ "Honduras",
            ben_country_of_birth %in% c("Lebanon", "LEBAN") ~ "Lebanon",
            ben_country_of_birth %in% c("Nepal", "NEPAL") ~ "Nepal",
            ben_country_of_birth %in% c("Nicaragua", "NICAR") ~ "Nicaragua",
            ben_country_of_birth %in% c("Somalia", "SOMAL") ~ "Somalia",
            ben_country_of_birth %in% c("Sudan", "SUDAN") ~ "Sudan",
            ben_country_of_birth %in% c("South Sudan", "SSUDA") ~ "South Sudan",
            ben_country_of_birth %in% c("Syria", "SYRIA") ~ "Syria",
            ben_country_of_birth %in% c("Ukraine", "UKRAI") ~ "Ukraine",
            ben_country_of_birth %in% c("Venezuela", "VENEZ") ~ "Venezuela",
            ben_country_of_birth %in% c("Yemen", "YEMEN") ~ "Yemen",
            TRUE ~ "All other countries")) |> 
  count(ben_country_of_birth) |> 
  mutate(data_source = "ELIS") |> 
  left_join(tps_crs, by=c("ben_country_of_birth" = "country"))

dat1$pct_diff <- ((dat1$n - dat1$total_tps_crs)/dat1$total_tps_crs) * 100

dat2 <- tps |> 
  filter(decision == "Approved",
         data_source == "C3"
        ) |> 
  distinct(unique_alien_id, .keep_all = TRUE) |> 
  mutate(ben_country_of_birth =
  case_when(ben_country_of_birth %in% c("Afghanistan", "AFGHA") ~ "Afghanistan",
            ben_country_of_birth %in% c("Burma", "BURMA") ~ "Burma",
            ben_country_of_birth %in% c("Cameroon", "CAMER") ~ "Cameroon",
            ben_country_of_birth %in% c("El Salvador", "ELSAL") ~ "El Salvador",
            ben_country_of_birth %in% c("Ethiopia", "ETHIO") ~ "Ethiopia",
            ben_country_of_birth %in% c("Haiti", "HAITI") ~ "Haiti",
            ben_country_of_birth %in% c("Honduras", "HONDU") ~ "Honduras",
            ben_country_of_birth %in% c("Lebanon", "LEBAN") ~ "Lebanon",
            ben_country_of_birth %in% c("Nepal", "NEPAL") ~ "Nepal",
            ben_country_of_birth %in% c("Nicaragua", "NICAR") ~ "Nicaragua",
            ben_country_of_birth %in% c("Somalia", "SOMAL") ~ "Somalia",
            ben_country_of_birth %in% c("Sudan", "SUDAN") ~ "Sudan",
            ben_country_of_birth %in% c("South Sudan", "SSUDA") ~ "South Sudan",
            ben_country_of_birth %in% c("Syria", "SYRIA") ~ "Syria",
            ben_country_of_birth %in% c("Ukraine", "UKRAI") ~ "Ukraine",
            ben_country_of_birth %in% c("Venezuela", "VENEZ") ~ "Venezuela",
            ben_country_of_birth %in% c("Yemen", "YEMEN") ~ "Yemen",
            TRUE ~ "All other countries")) |> 
  count(ben_country_of_birth) |> 
  mutate(data_source = "C3") |> 
  left_join(tps_crs, by=c("ben_country_of_birth" = "country"))

dat2$pct_diff <- ((dat2$n - dat2$total_tps_crs)/dat2$total_tps_crs) * 100

dat3 <- tps |> 
  filter(decision == "Approved"
        ) |> 
  distinct(unique_alien_id, .keep_all = TRUE) |> 
  mutate(ben_country_of_birth =
  case_when(ben_country_of_birth %in% c("Afghanistan", "AFGHA") ~ "Afghanistan",
            ben_country_of_birth %in% c("Burma", "BURMA") ~ "Burma",
            ben_country_of_birth %in% c("Cameroon", "CAMER") ~ "Cameroon",
            ben_country_of_birth %in% c("El Salvador", "ELSAL") ~ "El Salvador",
            ben_country_of_birth %in% c("Ethiopia", "ETHIO") ~ "Ethiopia",
            ben_country_of_birth %in% c("Haiti", "HAITI") ~ "Haiti",
            ben_country_of_birth %in% c("Honduras", "HONDU") ~ "Honduras",
            ben_country_of_birth %in% c("Lebanon", "LEBAN") ~ "Lebanon",
            ben_country_of_birth %in% c("Nepal", "NEPAL") ~ "Nepal",
            ben_country_of_birth %in% c("Nicaragua", "NICAR") ~ "Nicaragua",
            ben_country_of_birth %in% c("Somalia", "SOMAL") ~ "Somalia",
            ben_country_of_birth %in% c("Sudan", "SUDAN") ~ "Sudan",
            ben_country_of_birth %in% c("South Sudan", "SSUDA") ~ "South Sudan",
            ben_country_of_birth %in% c("Syria", "SYRIA") ~ "Syria",
            ben_country_of_birth %in% c("Ukraine", "UKRAI") ~ "Ukraine",
            ben_country_of_birth %in% c("Venezuela", "VENEZ") ~ "Venezuela",
            ben_country_of_birth %in% c("Yemen", "YEMEN") ~ "Yemen",
            TRUE ~ "All other countries")) |> 
  count(ben_country_of_birth) |> 
  mutate(data_source = "ELIS+C3") |> 
  left_join(tps_crs, by=c("ben_country_of_birth" = "country"))

dat3$pct_diff <- ((dat3$n - dat3$total_tps_crs)/dat3$total_tps_crs) * 100

tbl <- rbind(dat1, dat2, dat3)

clipr::write_clip(tbl)

# Count number of unique people who ever had an approved application or re-registration

dat <- tps |> 
  filter(decision == "Approved",
         data_source == "C3") |> 
  distinct(unique_alien_id, .keep_all = TRUE) |> 
  mutate(ben_country_of_birth =
  case_when(ben_country_of_birth %in% c("Venezuela", "VENEZ") ~ "Venezuela",
            ben_country_of_birth %in% c("El Salvador", "ELSAL") ~ "El Salvador",
            ben_country_of_birth %in% c("Honduras", "HONDU") ~ "Honduras",
            ben_country_of_birth %in% c("Haiti", "HAITI") ~ "Haiti",
            ben_country_of_birth %in% c("Ukraine", "UKRAI") ~ "Ukraine",
            ben_country_of_birth %in% c("Cameroon", "CAMER") ~ "Cameroon",
            ben_country_of_birth %in% c("Nepal", "NEPAL") ~ "Nepal",
            TRUE ~ "All other countries")) |> 
  count(dec_month, ben_country_of_birth) |> 
  mutate(total_tps = cumsum(n), .by=c(ben_country_of_birth))

p1.1 <- dat |> 
  ggplot(aes(x = dec_month, y = total_tps, color = ben_country_of_birth, group= ben_country_of_birth)) +
  geom_line() +
  ggrepel::geom_text_repel(
  data = dat %>% group_by(ben_country_of_birth) %>% filter(dec_month == max(dec_month)),
  aes(label = total_tps),
  nudge_x = 0.5,
  direction = "y"
) + labs(
  title = "Total unique approved TPS applicants by country",
  subtitle = "C3 system only"
)

p1.1

dat <- tps |> 
  filter(decision == "Approved",
         data_source == "ELIS") |> 
  distinct(unique_alien_id, .keep_all = TRUE) |> 
  mutate(ben_country_of_birth =
  case_when(ben_country_of_birth %in% c("Venezuela", "VENEZ") ~ "Venezuela",
            ben_country_of_birth %in% c("El Salvador", "ELSAL") ~ "El Salvador",
            ben_country_of_birth %in% c("Honduras", "HONDU") ~ "Honduras",
            ben_country_of_birth %in% c("Haiti", "HAITI") ~ "Haiti",
            ben_country_of_birth %in% c("Ukraine", "UKRAI") ~ "Ukraine",
            ben_country_of_birth %in% c("Cameroon", "CAMER") ~ "Cameroon",
            ben_country_of_birth %in% c("Nepal", "NEPAL") ~ "Nepal",
            TRUE ~ "All other countries")) |> 
  count(dec_month, ben_country_of_birth) |> 
  mutate(total_tps = cumsum(n), .by=c(ben_country_of_birth))

p1.2 <- dat |> 
  ggplot(aes(x = dec_month, y = total_tps, color = ben_country_of_birth, group= ben_country_of_birth)) +
  geom_line() +
  ggrepel::geom_text_repel(
  data = dat %>% group_by(ben_country_of_birth) %>% filter(dec_month == max(dec_month)),
  aes(label = total_tps),
  nudge_x = 0.5,
  direction = "y"
) + labs(
  title = "Total unique approved TPS applicants by country",
  subtitle = "ELIS system only"
)

p1.2

rm(dat)

### Person-level dataset

system.time({
app_id_cols <- c("unique_alien_id", "rec_date", "form_type", "decision_date", "decision", "latest_notice_type", "latest_notice_date")

tps <- tps |> 
  rowwise() |> 
  unite(united_hash_cols, all_of(app_id_cols), sep = "", remove = FALSE) %>% 
  mutate(application_id = vdigest(united_hash_cols, algo = "md5")) %>%
  select(-united_hash_cols)
      })

system.time({

tps <- tps |> 
  arrange(unique_alien_id, rec_date) |> 
  filter(!is.na(unique_alien_id),
        ) |>
  group_by(unique_alien_id) |> 
  mutate(n = n(),
         n_applications = n_distinct(application_id),
         diff_n_n_applications = n - n_applications,
         application_count = data.table::rleid(application_id),
         latest_app = max(rec_date, na.rm=TRUE),
         n_data_source = n_distinct(data_source),
         n_ben_country_of_birth = n_distinct(ben_country_of_birth),
         first_app_type = form_type[[1]],
         first_app_decision = decision[[1]],
         first_app_rec_date = rec_date[[1]],
         first_app_decision_date = decision_date[[1]], # Will this be correct if we're sorting by `rec_date` above? Could get max date instead.
         latest_app_type = form_type[[length(form_type)]],
         latest_app_decision = decision[[length(decision)]], # Will this be correct if we're sorting by `rec_date` above? Could get max date instead, or use more precise indexing.
         latest_app_rec_date = rec_date[[length(rec_date)]],
         latest_app_decision_date = decision_date[[length(decision_date)]],
         latest_classification = classification[[length(classification)]],
         first_imm_status = immigration_status[[1]],
         latest_imm_status = immigration_status[[length(immigration_status)]],
         first_status = status[[1]],
         latest_status = status[[length(status)]],
         ever_approved = "Approved" %in% decision,
        #  most_recent_approved_dec_date = max(decision_date[decision == "Approved"], na.rm = TRUE) # Very slow?
        ) |>
  ungroup()

      })

tps_unique <- tps |> 
    distinct(unique_alien_id, .keep_all=TRUE)

hist(tps_unique$n)

hist(tps_unique$n_applications)

hist(tps_unique$diff_n_n_applications)

# # Trying to count unique applications--seems like there might be a small number of repeat applications with slight differences: corrections?

temp <- tps_unique[tps_unique$diff_n_n_applications > 0,]

temp2 <- tps[tps$unique_alien_id %in% temp$unique_alien_id,]

# temp <- tps_unique[tps_unique$n_ben_country_of_birth > 1,]

# temp2 <- tps[tps$unique_alien_id %in% temp$unique_alien_id,]

# temp <- tps_unique[tps_unique$ben_country_of_birth == "Chile",]

# temp2 <- tps[tps$unique_alien_id %in% temp$unique_alien_id,]

# These numbers are similar but not exact same as above

tps_unique |>
  filter(decision == "Approved") |> 
  mutate(ben_country_of_birth =
  case_when(ben_country_of_birth %in% c("Venezuela", "VENEZ") ~ "Venezuela",
            ben_country_of_birth %in% c("El Salvador", "ELSAL") ~ "El Salvador",
            ben_country_of_birth %in% c("Honduras", "HONDU") ~ "Honduras",
            ben_country_of_birth %in% c("Haiti", "HAITI") ~ "Haiti",
            ben_country_of_birth %in% c("Ukraine", "UKRAI") ~ "Ukraine",
            ben_country_of_birth %in% c("Cameroon", "CAMER") ~ "Cameroon",
            ben_country_of_birth %in% c("Nepal", "NEPAL") ~ "Nepal",
            TRUE ~ "All other countries")) |> 
  count(ben_country_of_birth) |> 
  arrange(desc(n))



hist(tps_unique$n_data_source)

hist(tps_unique$n_ben_country_of_birth)

tps_unique |> 
  count(first_imm_status) |>
  arrange(desc(n))

tps_unique |> 
  count(latest_imm_status) |>
  arrange(desc(n))

tps_unique |> 
  count(first_status) |>
  arrange(desc(n))

tps_unique |> 
  count(latest_status) |>
  arrange(desc(n))

# How to calculate point in time active TPS? Can that be done?