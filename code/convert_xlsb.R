# Convert every .xlsb in the FOIA release dir to a sibling .xlsx so the
# main pipeline (code/read.R) can ingest them with readxl like every other
# file. Skips an .xlsx already newer than its source.

library(readxlsb)
library(writexl)

source("code/column_types.R")

release_dir <- "inputs/mukherjee-v-uscis"

xlsb_files <- list.files(
  release_dir,
  pattern = "\\.xlsb$",
  full.names = TRUE,
  recursive = TRUE
)

for (src in xlsb_files) {
  dst <- sub("\\.xlsb$", ".xlsx", src)
  if (file.exists(dst) && file.mtime(dst) >= file.mtime(src)) {
    message("skip (up to date): ", basename(dst))
    next
  }

  message("converting: ", basename(src))
  df <- read_xlsb(src, sheet = 1, na = na_strings)

  # readxlsb returns date cells as Date when the cell format is tagged, but
  # as numeric Excel serials when it isn't (e.g. rec_date in this file).
  # Normalize to Date so writexl emits real Excel dates that readxl reads
  # back as POSIXct.
  for (nm in intersect(date_cols, names(df))) {
    x <- df[[nm]]
    if (is.numeric(x)) {
      df[[nm]] <- as.Date(x, origin = "1899-12-30")
    } else if (is.character(x)) {
      df[[nm]] <- as.Date(x)
    }
  }
  for (nm in intersect(year_cols, names(df))) {
    df[[nm]] <- as.integer(df[[nm]])
  }

  write_xlsx(df, dst)
}
