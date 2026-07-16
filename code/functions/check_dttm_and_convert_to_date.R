# Converts a POSIXt column to Date if every non-NA value has a zero time
# component (so date-only fields don't sit in storage as datetimes).
# trunc(x, "days") is tz-aware and faster than three separate lubridate calls.
check_dttm_and_convert_to_date <- function(x) {
  if (!inherits(x, "POSIXt")) {
    stop("Input must be a POSIXt object (datetime).")
  }
  if (all(trunc(x, "days") == x, na.rm = TRUE)) {
    as.Date(x)
  } else {
    x
  }
}
