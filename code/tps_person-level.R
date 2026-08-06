library(pacman)

p_load(tidyverse, digest, lubridate)

options(scipen = 100)

vdigest <- Vectorize(digest)

tps <- arrow::read_parquet("data/tps-latest.parquet")

### Draft person-level dataset

system.time({

tps <- tps |> 
  filter(!is.na(unique_alien_id),
        ) |>
  arrange(unique_alien_id, rec_date, decision_date) |> 
  group_by(unique_alien_id) |> 
  mutate(
        #  first_app_type = first(form_type),
        #  first_app_decision = first(decision),
        #  first_app_rec_date = first(rec_date),
        #  first_app_decision_date = first(decision_date),
        #  latest_app_type = last(form_type),
        #  latest_app_decision = last(decision),
        #  latest_app_rec_date = last(rec_date),
        #  latest_app_decision_date = last(decision_date),
        #  first_classification = first(classification),
        #  latest_classification = last(classification),
        #  first_imm_status = first(immigration_status),
        #  latest_imm_status = last(immigration_status),
        #  first_status = first(status),
        #  latest_status = last(status),
        #  ever_approved = any("Approved" %in% decision),
         first_approved_decision = first(decision_date["Approved" %in% decision]),
         latest_approved_decision = last(decision_date["Approved" %in% decision])
        ) |>
  ungroup()

      })

tps_unique <- tps |>
    distinct(unique_alien_id, .keep_all=TRUE)

### Check if this can get close to published TPS approval statistics
# Each TPS designation has different expiration timeline ranging from 6-18 months, here we test each of these thresholds

point_in_time <- ymd("2025-03-31")

point_in_time_minus_6 <- point_in_time - days(182)
point_in_time_minus_12 <- point_in_time - days(364)
point_in_time_minus_18 <- point_in_time - days(546)

# For most recent time period, we just check if an individual has had approved TPS within 6-12-18 months of date

dat <- tps |>
  distinct(unique_alien_id, .keep_all = TRUE) |> 
  mutate(
  latest_approved_within_6_mos = point_in_time_minus_6 <= latest_approved_decision,
  latest_approved_within_12_mos = point_in_time_minus_12 <= latest_approved_decision,
  latest_approved_within_18_mos = point_in_time_minus_18 <= latest_approved_decision,
) |> 
  group_by(ben_country_of_birth) |> 
  summarise(approved_within_6 = sum(latest_approved_within_6_mos, na.rm=TRUE),
            approved_within_12 = sum(latest_approved_within_12_mos, na.rm=TRUE),
            approved_within_18 = sum(latest_approved_within_18_mos, na.rm=TRUE)
)

dat_long <- dat |> 
  pivot_longer(-ben_country_of_birth, names_to = "source") |> 
  rename(country = ben_country_of_birth)


# TPS approval statistics via CRS, following Figure 1 here:
# https://www.migrationpolicy.org/journal/policy-beat/attacked-de-facto-amnesty-us-temporary-protected-status-abruptly-eroded

crs <- read_delim("inputs/uscis-public/crs_tps_approvals_2015-2025.csv", delim=",") |> 
  janitor::clean_names()

crs_long <- crs |> 
  pivot_longer(!country, names_to = "report_date", names_prefix = "x"
               ) |> 
  mutate(report_date = as.Date(str_replace_all(report_date, "_", "\\-"))) |> 
  mutate(source = "CRS")

crs_long$value[is.na(crs_long$value)] <- 0


dat2 <- crs_long |> 
  filter(report_date == point_in_time,
         value > 0) |> 
  dplyr::select(-report_date)

dat3 <- dat_long |> 
  filter(country %in% dat2$country)

dat_compare <- rbind(dat2, dat3)

# In general, we expect estimates to be close to but under total TPS approvals reported by CRS

p1 <- dat_compare |> 
  ggplot(aes(y = country, x = value, color = source)) +
  geom_point(size = 4) +
  # scale_x_log10() +
  labs(title = "Approximate active TPS approvals by country",
       subtitle = point_in_time)

p1


#####

# For each CRS report date, get most recent approved decision for each individual
# For each row, is approval date within 6-12-18 months of report date

report_dates <- unique(crs_long$report_date)

results <- map_df(report_dates, function(v) {
  
  point_in_time_minus_6 <- v - days(182)
  point_in_time_minus_12 <- v - days(364)
  point_in_time_minus_18 <- v - days(546)

  tps |> 
    filter(decision_date <= v) |> 
    group_by(unique_alien_id) |> 
    mutate(latest_approved_decision = last(decision_date["Approved" %in% decision])) |>
    ungroup() |>
    distinct(unique_alien_id, .keep_all = TRUE) |> 
    mutate(latest_approved_within_6_mos = point_in_time_minus_6 <= latest_approved_decision,
           latest_approved_within_12_mos = point_in_time_minus_12 <= latest_approved_decision,
           latest_approved_within_18_mos = point_in_time_minus_18 <= latest_approved_decision) |> 
    group_by(ben_country_of_birth) |> 
    summarise(approved_within_6 = sum(latest_approved_within_6_mos, na.rm=TRUE),
              approved_within_12 = sum(latest_approved_within_12_mos, na.rm=TRUE),
              approved_within_18 = sum(latest_approved_within_18_mos, na.rm=TRUE)) |> 
  mutate(report_date = v)

})

# Compare with CRS stats

dat <- results |> 
  mutate(country = case_when(ben_country_of_birth %in% unique(crs_long$country) ~ ben_country_of_birth,
                            TRUE ~ "All other countries")) |> 
  group_by(country, report_date) |> 
  summarize(across(where(is.integer), \(x) sum(x, na.rm=TRUE))) |> 
  pivot_longer(-c(country, report_date), names_to="source", values_to = "value") |> 
  rbind(crs_long)

# Eyeballing

p1 <- dat |> 
  ggplot(aes(x = report_date, y = value, color = source, group=source)) +
  geom_line() +
  facet_wrap(~country, scales="free_y") +
  labs(title = "Estimated individuals with approved TPS over time vs. CRS")

p1

p1.1 <- dat |> 
  filter(country %in% c("Haiti", "Venezuela", "El Salvador", "Ukraine")) |> 
  ggplot(aes(x = report_date, y = value, color = source, group=source)) +
  geom_line() +
  facet_wrap(~country, scales="free_y") +
  labs(title = "Estimated individuals with approved TPS over time vs. CRS",
subtitle = "Selected countries")

p1.1



p2 <- tps |> 
  filter(ben_country_of_birth == "El Salvador",
        #  decision == "Approved"
        ) |> 
  count(decision_date, decision) |> 
  ggplot(aes(x = decision_date, y = n, color=decision)) +
  geom_line() +
  labs(title = "All TPS decisions",
subtitle = "El Salvador")

p2

p2.1 <- tps |> 
  filter(ben_country_of_birth == "Venezuela",
        #  decision == "Approved"
        ) |> 
  count(decision_date, decision) |> 
  ggplot(aes(x = decision_date, y = n, color=decision)) +
  geom_line()

p2.1

p2.2 <- tps |> 
  filter(ben_country_of_birth == "Haiti",
        #  decision == "Approved"
        ) |> 
  count(decision_date, decision) |> 
  ggplot(aes(x = decision_date, y = n, color=decision)) +
  geom_line()

p2.2

# END.
