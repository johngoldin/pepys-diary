

#str_extract("lots of money x50,000", "[lL](\\d+|\\d+,\\d\\d\\d)")
# test:  fix_pounds("lots of money L50,000 comes here")
# text  fix_pounds("all, money L50,000 =40,000l  L5,123  l500,000 L45 L9 L34,00 el.")

fix_pounds <- function(text, pattern = "([lL]\\d+|[lL]\\d+,\\d\\d\\d)|\\d+[lL]|\\d+,\\d\\d\\d[lL]") {
  while (!is.na(fnd <- str_extract(text, pattern))) {
  #print(fnd)
    if (str_sub(fnd, 1, 1) %in% c("L", "l")) {
      with_fix <- str_replace(fnd, "[lL]", "£")
      text <- str_replace(text, fnd, with_fix)
    } else if (str_sub(fnd, str_length(fnd)) %in% c("L", "l")) {
      with_fix <- str_c("£", str_sub(fnd, 1, str_length(fnd) -1))
    }
    text <- str_replace(text, fnd, with_fix)
  }
  text
}