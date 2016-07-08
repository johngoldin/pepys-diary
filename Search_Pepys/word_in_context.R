

# test:   cat(word_in_entry(entry_df$entry_text[10], "oVertaking"))
# test:   cat(word_in_entry(entry_df$entry_text[entry_df$date == ymd("1660-10-30")], "Penn"))
# test:   cat(word_in_entry(entry_df$entry_text[10], "to"))
# test:   cat(word_in_entry(entry_df$entry_text[10], "\\bto\\b", whole_word = TRUE, max_returns = 3, preface = as.character(entry_df$date[10])))

word_in_entry <- function(text, search_for, nchars = 25, ignore_case = TRUE, 
                            max_returns = 5, preface = "") {
  # text is an entry of text.
  # str_split returns a list of lists. That's because str_split(string, pattern) is vectorized over
  # both string and pattern. The first list is for the first pattern.
  # normally expect preface to be something like as.character(entry_df$date[1])
  hit <- str_locate_all(text, regex(search_for, ignore_case = ignore_case))[[1]]
  if (dim(hit)[1] == 0) return(str_c(preface, ": ", search_for, " not found in entry."))
  if (dim(hit)[1] > max_returns) hit <- hit[1:max_returns, ]
  for_return <- ""
  for (i in 1:dim(hit)[1]) {
    if (i > 1) for_return <- paste(for_return, "\n", sep = "")
    the_start <- hit[i, 1] - nchars
    start_ellipsis <- "..."
    
    if (the_start <= 1) {
      start_ellipsis <- ""
      the_start <- 1
    }
    start_words <- str_sub(text, the_start)
    next_word <- str_locate(start_words, " ")
    if (!is.na(next_word[1])) if ((the_start + next_word[1]) < hit[i, 1]) the_start <- the_start + next_word[1]
    
    finish_ellipsis <- ""
    finish <- hit[i, 2] + nchars
    if (finish >= str_length(text)) {
      finish <- str_length(text)
    } else {
      finish_words <- str_sub(text, finish)
      next_word <- str_locate(finish_words, "[ ]")
      if (!is.na(next_word[1])) finish <- finish + next_word[1] - 2
      if (finish >= str_length(text)) finish <- str_length(text) 
      else finish_ellipsis <- "..."
    }
    
    for_return <- str_c(for_return, str_c(preface, ": "), start_ellipsis, 
                        str_sub(text, the_start, finish), 
                        finish_ellipsis)
  }
  for_return
}

# test:   word_in_context(entry_df, "viol")
# test:   word_in_context(entry_df, "\\b(violin|viallin|viollin)\\b", max_entries = 100)
# test:   word_in_context(entry_df, "(Captain Holland|Captain Philip Holland)")
# test:   word_in_context(entry_df, "so to bed")
# test:   word_in_context(entry_df, "Duke of York", max_entries = 40)
# test:   word_in_context(entry_df, "(\\dL|L\\d)")
word_in_context <- function(df, search_for, ignore_case = TRUE, whole_word = FALSE,
                            max_entries = 20, do_print = TRUE) {
  good_search <- try(str_detect("xxx", search_for), silent = TRUE)
  if (good_search != FALSE) return(str_c("Bad search expression: ", search_for, " [in word_in_context()]"))
  entries <- which(str_detect(df$entry_text, regex(search_for, ignore_case = ignore_case)))
  if (length(entries) == 0) return(str_c(search_for, " not found."))
  loop_limit <- length(entries)
  if (loop_limit > max_entries) loop_limit <- max_entries
  for_return <- array(data = rep("", loop_limit))
  for (i in 1:loop_limit) {
    for_return[i] <- word_in_entry(df$entry_text[entries[i]], search_for, preface = as.character(df$date[entries[i]]),
                  ignore_case = TRUE)
  }
  if (!do_print) return(for_return)
  cat(str_c(for_return, "\n"))
}