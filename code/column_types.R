# Column-type metadata shared by code/read.R and code/convert_xlsb.R.

# Columns that should be parsed as Date.
date_cols <- c(
  "status_date",
  "natz_test_date",
  "intv_date",
  "natz_date",
  "lpr_date",
  "rec_date",
  "decision_date",
  "latest_notice_date",
  "latest_rfe_date",
  "latest_noid_date"
)

# Columns that hold a 4-digit year. Coerced to integer after the read —
# readxl has no "integer" col_type.
year_cols <- c(
  "ben_year_of_birth",
  "latest_trvl_depart_yr",
  "latest_trvl_return_yr"
)

# Missing-values in the source files: "null" is USCIS's NA marker,
# and the FOIA exemption codes are redacted cells. Treating them as NA at
# read time keeps them from polluting readxl's type guessing.
na_strings <- c("null", "(b)(6)", "(b)(3) (b)(6) (b)(7)(c)")
