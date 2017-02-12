
# tdf <- filter(trump_tweets_df[1:10, ], map_lgl(trump_tweets_df$text[1:10], detect_exclamation))
detect_exclamation <- function(atext, words = exclamations) {
  for (i in seq_along(words)) {
    if (str_detect(atext, 
                   regex(paste0("\\.[A-Za-z0-9 \"\'\\?]*\\..*", words[i], ".$"), ignore.case = TRUE))) return(TRUE)
  }
  return(FALSE)
}

# stuff <- map_lgl(trump_tweets_df$text[1:10], detect_exclamation)

