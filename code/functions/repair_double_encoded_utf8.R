# fix double encoded special characters, e.g. Côte d'Ivoire, by converting to latin1 and back to UTF-8
repair_double_encoded_utf8 <- function(x) {
  repaired <- iconv(x, from = "UTF-8", to = "latin1")
  Encoding(repaired) <- "UTF-8"
  if_else(is.na(repaired) | !validUTF8(repaired), x, repaired)
}
