# Returns FALSE for a column that is entirely NA, empty strings, or FOIA
# redaction codes (e.g. "(b)(6)", "b(7)c"). Used with select(where(...)) to
# drop columns that carry no information.
is_not_blank_or_redacted <- function(x) {
  if (is.character(x)) {
    vals <- str_squish(x)
    redact_pattern <- regex(
      "\\(b\\)|\\(B\\)|b\\([0-9]\\)|B\\([0-9]\\)",
      ignore_case = TRUE
    )
    redacted <- str_detect(vals, redact_pattern)
    redacted[is.na(redacted)] <- FALSE
    !all(is.na(x) | vals == "" | vals == "NA" | redacted)
  } else {
    !all(is.na(x))
  }
}
