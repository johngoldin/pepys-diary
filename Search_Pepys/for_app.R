# functions used by the app

#   xx <- entries_with_word(entry_df, "nuclear war")
entries_with_word <- function(df, search_for, ignore_case = TRUE, whole_word = FALSE) {
    good_search <- try(str_detect("xxx", search_for), silent = TRUE)
  if (good_search != FALSE) {
    print(str_c("Bad search expression: ", search_for, " [in word_in_context()]"))
    #browser()
    return(NA)
  }
  found <- str_detect(df$entry_text, regex(search_for, ignore_case = ignore_case))
  return(found)
}  

# test:   word_in_entry(entry_df$date[10], entry_df$entry_text[10], "oVertaking", entry_df$date_url[10])$text
# test:   word_in_entry(entry_df$date[entry_df$date == ymd("1660-10-30")],entry_df$entry_text[entry_df$date == ymd("1660-10-30")], "Penn")
# test:   word_in_entry(entry_df$date[10], entry_df$entry_text[10], "and")
# test:   word_in_entry(entry_df$date[10], entry_df$entry_text[10], "nuclear bombs")

word_in_entry <- function(the_date, text, search_for, the_date_url, nchars = 45, ignore_case = TRUE, 
                          max_returns = 10000) {
  # text is an entry of text.
  # str_split returns a list of lists. That's because str_split(string, pattern) is vectorized over
  # both string and pattern. The first list is for the first pattern.
  hit <- str_locate_all(text, regex(search_for, ignore_case = ignore_case))[[1]]
  if (dim(hit)[1] == 0) return(data_frame(date_url = c(""), text = c("No matches found.")))
  if (dim(hit)[1] > max_returns) hit <- hit[1:max_returns, ]
  for_return <- vector("list", dim(hit)[1])
  for (i in seq_along(hit[ , 1])) {
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
    the_find <- str_extract(str_sub(text, the_start, finish), regex(search_for, ignore_case = ignore_case))
    the_line <- str_replace_all(str_sub(text, the_start, finish), regex(search_for, ignore_case = ignore_case), str_c("<em>", the_find, "</em>"))
    for_return[[i]] <- str_c(start_ellipsis, 
                        the_line, 
                        finish_ellipsis)
  }
  data_frame(date_url = the_date_url, text = unlist(for_return))
}

#  xx <- word_in_context(entry_df, "Greenwich")
#   word_in_context(entry_df, "nuclear war")

word_in_context <- function(df, search_for, time_out = 2, start_time = NULL,
                            max_entries = 20000, ...) {
  # entries_with_word <- function(df, search_for, ignore_case = TRUE, whole_word = FALSE) {
  #   good_search <- try(str_detect("xxx", search_for), silent = TRUE)
  if (is.null(start_time)) start_time <- now()
  cat(file = stderr(), "Searching for...", search_for)
  found <- entries_with_word(df, search_for, ...)
  if (is.null(found)) return(data_frame(date_url = c(""), text = c(str_c(search_for, " is not a valid search expression."))))
  entries <- which(found)
  if (length(entries) == 0) {
    cat(file = stderr(), sprintf(" %5.1fsecs. not found.\n", difftime(now(), start_time, units = "secs")))
    return(data_frame(date = "", text = str_c('"', search_for,  '" not found.')))
  }
  loop_limit <- length(entries)
  if (loop_limit > max_entries) loop_limit <- max_entries
  for_return <- vector("list", loop_limit)
  for (i in seq_along(for_return)) {
    if (difftime(now(), start_time, units = "secs") > time_out) {
      cat(file = stderr(), sprintf(" %5.1fsecs. Timed out.\n", difftime(now(), start_time, units = "secs")))
      for_return[[length(for_return)]] <- data_frame(date_url = c(""), text = c("Stopped by time limit."))
      return(bind_rows(for_return))
    }
    for_return[[i]] <- word_in_entry(df$date[entries[i]], df$entry_text[entries[i]], search_for,
                                     df$date_url[entries[i]], ignore_case = TRUE)
  }
  cat(file = stderr(), sprintf(" %5.1fsecs.\n", difftime(now(), start_time, units = "secs")))
  return(bind_rows(for_return))
}

#test:    form_url_from_date(entry_df$date[2])
form_url_from_date <- function(d) {
  if (!is.POSIXct(d)) return("")
  # need the overall as.character because a() returns name, children, and attributes
  as.character(a(as.character(d), target = "_blank", href = str_c("http://www.pepysdiary.com/diary/", sprintf("%04d/%02d/%02d", year(d), month(d), day(d)))))
}

 
#test: word_histogram(entry_df, search_for = "Brampton")
word_histogram <- function(df, search_for, ignore_case = TRUE, whole_word = FALSE) {
  good_search <- try(str_detect("xxx", search_for), silent = TRUE)
  if (good_search != FALSE) {
    print(str_c("Bad search expression: ", search_for, " [in word_in_context()]"))
    return(NULL)
  }
  found <- as.numeric(str_detect(df$entry_text, regex(search_for, ignore_case = ignore_case)))
  if (sum(found, na.rm = TRUE) == 0) return(NULL)
  
  
  p <- ggplot(filter(df, found == 1), aes(x = date)) + 
    geom_histogram(binwidth = 60*60*24*30) +
    scale_x_datetime(date_breaks = "1 year",
                     date_minor_breaks = "3 months", 
                     date_labels = "%Y",
                     limits = c(min(df$date, na.rm = TRUE), max(df$date, na.rm = TRUE))) +
    ggtitle(str_c("Frequency of Entries Containing: ", search_for)) + ylab("# of Entries/Month") + xlab(NULL)
  p
}


