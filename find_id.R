find_from_phrase <- function(phrase, sex, pn = pepys_names) {
  found <- pn %>% filter(str_detect(other_name, phrase), gender == sex)
  if (phrase == "") return("")
  if (dim(found)[1] == 1) {
    return(found$id[1])
  }
  else if (dim(found)[1] == 0) {
    return("")
  }
  else {
    return(str_c(found$id[1], "+"))
  }
}
# test:  find_from_phrase("Duke of York", pepys_names)
# > find_from_phrase("Duke of York", pepys_names)
# [1] "S2417"

#' Lookup up a name in the pepys_names table.
#'
#' @param word1 first word of n-gram
#' @param word2 second word of n-gram
#' @param word3 third word of n-gram (or NULL)
#' @param word4 fourth word of n-gram (or NULL)
#' @param cap1 is 1st word capitalized?
#' @param cap2 is 2nd word capitalized?
#' @param cap3 is 3rd word capitalized?
#' @param cap4 is 4th word capitalized?
#' @param non_names list of words that cannot be part of a name
#' @param non_titles list of words that are not titles
#' @param non_proper list of words that are not a proper name but might be part of a title
#' @param pn 
find_id <- function(word1, word2 = NULL, word3 = NULL, word4 = NULL, 
                    cap1, cap2, cap3, cap4, the_name = "",
                    non_names1 = non_names, non_titles1 = non_titles, non_proper1 = non_proper,
                    pn = pepys_names) {
  result1 <- NULL
  if (!is.null(word4)) {  # four-way
    # find things like "the King of England", "the Duke of York"
    x_of_something <- df %>%
      filter(word1 %in% c("my", "the"), 
             cap2,
             word3 %in% c("of", "de"),
             cap4,
             !str_detect(word4, "['/']"), 
             !(word2 %in% non_titles1 ),
             !(word2 %in% c("Archbishoprick", "States", "Treasury", "Government", "Commission", "North"))) 
    result1 <- map2_chr(x_of_something$search_term, find_from_phrase)
  }
  return(result1)
}

# my_something$result1 <- map_chr( xx$search_term, find_from_phrase)
# my_something$result2 <- map_chr( xx$search_term, find_from_phrase)

