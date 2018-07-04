

spelling_fixes <- c("Duchesse" = "Duchess", "Yorke" = "York", "Lenox" = "Lennox", "Portugall" = "Portugal", 
                    "Bristoll" = "Bristol", "Warwicke" = "Warwick",
                    "Suffolke" = "Suffolk", "[Tt]est" = "passed")


#' do global change to fix spelling
#'
#' @param text_vector text to search for needed fixes
#' @param fixes A named vector of spelling fixes
#'
#' @return
#' @export
#'
#' @examples
#' spelling_fixes <- c("Duchesse" = "Duchess", "Yorke" = "York", "Lenox" = "Lennox", "Portugall" = "Portugal", 
#' "Bristoll" = "Bristol", "Warwicke" = "Warwick","[Tt]est" = "passed")
#' fix_spelling(c("The Duchesse of Oxford", "the Duke of Yorke")
fix_spelling <- function(text_vector, fixes = spelling_fixes) {
  text_vector %>% str_replace_all(fixes)
}

#' Change first letter of text so it is a regex search string that will match either upper or
#' lower of first character (in case phrase is at the start of a sentence.)
#'
#' @param text_vector text to be changed
#'
#' @return regex version of string
#'
#' @examples
#' first_letter_uplow("the Duke of York")
first_letter_uplow <- function(text_vector) {
  str_sub(text_vector, 1, 1) <- str_c("[", str_to_upper(str_sub(text_vector, 1, 1)),
                                      str_to_lower(str_sub(text_vector, 1, 1)), "]") 
  return(text_vector)
}


                            