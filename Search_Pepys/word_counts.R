


word_count <- function(df, search_for, ignore_case = TRUE, whole_word = FALSE) {
  good_search <- try(str_detect("xxx", search_for), silent = TRUE)
  if (good_search != FALSE) {
    print(str_c("Bad search expression: ", search_for, " [in word_in_context()]"))
    return(NA)
  }
  found <- as.numeric(str_detect(df$entry_text, regex(search_for, ignore_case = ignore_case)))
  if (sum(found, na.rm = TRUE) == 0) print(str_c(search_for, " not found."))
  return(found)
}  

entry_counts <- select(entry_df, date, year, month, week)
histogram_word <- function(df, wrd = "betime") {
  df$words <- word_count(entry_df, wrd)
  p <- ggplot(df, aes(x = date)) + geom_histogram(aes(weight = words), binwidth = 60*60*24*30) +
    ggtitle(str_c("Frequency of Entries Containing: ", wrd)) + ylab("# of Entries/Month") + xlab(NULL)
  print(p)
}

# histogram_word(entry_counts, wrd = "Brampton")
# histogram_word(entry_counts, wrd = "so to bed")
# histogram_word(entry_counts, wrd = "my wife")
# histogram_word(entry_counts, wrd = "the king")
# histogram_word(entry_counts, wrd = "duke of york")
# histogram_word(entry_counts, wrd = "deptford")
# histogram_word(entry_counts, wrd = "by water")
# histogram_word(entry_counts, wrd = "(\\dL|L\\d)")
# entry_counts$betime <- word_count(entry_df, "betime")
# entry_counts$so_to_bed <- word_count(entry_df, "so to bed")
# entry_counts$the_king <- word_count(entry_df, "the king")
# entry_counts$duke_of_york <- word_count(entry_df, "duke of york")
# entry_counts$by_water <- word_count(entry_df, "by water")
# entry_counts$money_amount <- word_count(entry_df, "(\\dL|L\\d)")
