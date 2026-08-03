library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readxl)
library(arrow)

# A value cell is a count, a suppression code, or a dash for zero.
val <- "[0-9][0-9,]*|[DH-]"

cols <- c(
  "country",
  paste(
    rep(c("Q1", "Q2", "Q3", "Q4", "FY"), each = 4),
    c("received", "approved", "denied", "pending"),
    sep = "."
  )
)

# FY22, FY23 — pdftotext -layout keeps the columns aligned, so a data row is
# just a country label followed by exactly 20 value tokens.
from_pdf <-
  list.files("inputs/uscis-public", "\\.pdf$", full.names = TRUE) |>
  map(\(path) {
    txt <- str_squish(system2(
      "pdftotext",
      c("-layout", shQuote(path), "-"),
      stdout = TRUE
    ))
    m <- str_match(
      txt,
      sprintf("^(.*?[^\\s0-9])\\s+((?:%s)(?: (?:%s)){19})$", val, val)
    )
    m <- m[!is.na(m[, 1]), ]
    out <- cbind(m[, 2], str_split_fixed(m[, 3], " ", 20))
    colnames(out) <- cols
    as_tibble(out) |> mutate(source_file = basename(path))
  }) |>
  list_rbind()

# FY24, FY25 — data rows are the ones whose first value cell is a count or a
# code, which drops the title, header and footnote rows in one go.
from_excel <- list.files(
  "inputs/uscis-public",
  "i821.*\\.xlsx$",
  full.names = TRUE
) |>
  map(\(path) {
    x <- suppressMessages(read_excel(
      path,
      col_names = FALSE,
      col_types = "text"
    ))
    out <- x[str_detect(replace_na(x[[2]], ""), sprintf("^(%s)$", val)), ]
    colnames(out) <- cols
    out |> mutate(source_file = basename(path))
  }) |>
  list_rbind()

tps <-
  bind_rows(from_pdf, from_excel) |>
  pivot_longer(
    -c(country, source_file),
    names_to = c("period", "metric"),
    names_sep = "\\."
  ) |>
  mutate(
    country = str_squish(country),
    fiscal_year = as.integer(str_extract(source_file, "(?i)(?<=fy)\\d+")), # 22 or 2022
    fiscal_year = fiscal_year + 2000L * (fiscal_year < 100),
    quarter = as.integer(str_extract(period, "\\d")), # NA on the FY total rows
    code = if_else(value %in% c("D", "H"), value, NA_character_),
    value = suppressWarnings(as.numeric(str_remove_all(
      str_replace(value, "^-$", "0"),
      ","
    )))
  ) |>
  select(
    fiscal_year,
    quarter,
    period,
    country,
    metric,
    value,
    code,
    source_file
  ) |>
  arrange(fiscal_year, quarter, country, metric)

write_parquet(tps, "data/i821_radp_public.parquet")
