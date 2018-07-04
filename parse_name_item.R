
# Take an entry from the name list on Pepys web site and
# split it into a three-list item: last name, first name, stuff in parentheses

#test:   item <- parse_name_item("last, first")
#test:   parse_name_item("last, first (more stuff)")
#test:   parse_name_item("last (more stuff)")
#test:   parse_name_item("last, Sir first (more stuff)")
#test:   parse_name_item("last, Sir first ")

parse_name_item <- function(x) {
  t <- c("Gen.", "Lieut.", "Col.", "Sir", "Capt.", "Mrs", "Dr", "Lady", "Maj.", 
         "Ald.", "Ms", "Mr", "Cardinal", "Lt-Adm.")
  title <- ""
  search_comma <- unlist(str_split(x, "\\,"))
  #browser()
  if (length(search_comma) == 1) {
    first_name <- ""
    search_paren <- unlist(str_split(x, "\\("))
    if (length(search_paren) == 1) {
      last_name <- str_trim(search_paren)
      other_name <- ""
    } else {
      last_name <- str_trim(search_paren[1])
      other_name <- str_trim(search_paren[2])
    }
  } else {
    last_name <- str_trim(search_comma[1])
    search_paren <- unlist(str_split(search_comma[2], "\\("))
    if (length(search_paren) == 1) {
      first_name <- str_trim(search_paren)
      other_name <- ""
    } else {
      first_name <- str_trim(search_paren[1])
      other_name <- str_trim(search_paren[2])
    }
    if (first_name %in% t) {
      title <- first_name
      first_name <- ""
    }
    else if (any(str_detect(first_name, t))) {
      spaced <- unlist(str_split(first_name, " "))
      if (length(spaced > 1)) {
        title <- spaced[1]
        first_name <- spaced[2]
      }
    }
  }
  other_name <- str_replace(other_name, "\\)", "")
  return(list(last_name = last_name, first_name = first_name, title = title, other_name = other_name, people = x))
}