
library(compiler)
library(httr)
library(rvest)
library(jsonlite)

# See https://www.pepysdiary.com/api/docs/ for a description of Pepys API

#' retrieve_entry_dates
#' Use the Pepys API to retrieve information about entries. By default it will
#' retrieve one row for each entry in the diary.
#'
#' @param next_url URL to get the next batch (page) of entry information
#' @param max_page_count Maximum number of batches (pages) to retrieve
#'
#' @return A dataframe with one row per entry including the date and the API URL for that entry
#' @export
#'
#' @examples xx <- retrieve_entry_dates(max_page_count = 13)
retrieve_entry_dates <- function(next_url = "https://www.pepysdiary.com/api/v1/entries?page=1", max_page_count = 10000) {
  accum_df <- NULL
  page_count <- 0
  entry_count <- 0
  has_data <- 0
  # Ideally, one would set list_size so large that it will never be an issue.
  # But if it is not enough, accumulate results in accum_df
  list_size <- 20000
  save_results <- 0
  accum <- vector("list", list_size)
  repeat {
    page_count <- page_count + 1
    save_results <- save_results + 1
    if (page_count > max_page_count) {
      warning("Maximum page count reached.")
      break
    }
    # print(next_url)
    if (is.null(next_url)) break
    if (next_url == "") break
    response <- GET(next_url)
    if (response$status_code != 200) {
      print(str_c("Status code is ", response$status_code))
      break
    }
    # print("just after break")
    the_result <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
    next_url <- the_result$`nextPageURL`
    total_entries <- the_result$totalResults
    total_pages <- the_result$totalPages
    if (save_results > list_size) {
      print(str_c("Saving results. ", save_results, " ", list_size))
      accum_df <- bind_rows(accum_df, accum)
      accum <- vector("list", list_size)
      save_results <- 1
    }
    accum[[save_results]] <- the_result$results
    entry_count <- entry_count + dim(the_result$results)[1]
    has_data <- 1
  }
  if (has_data) {
    print(str_c("Entry_count = ", entry_count, " page_count = ", page_count - 1, 
                " total_entries = ", total_entries, "total_pages = ", total_pages))
    return(bind_rows(accum_df, accum))
  }
  return(NULL)
}
retrieve_entry_dates <- cmpfun(retrieve_entry_dates)

#' pepys_entry_page_df
#' Return df rather than vector of ids
#' @param d Date of entry yyyy-mm-dd
#'
#' @return data frame with date and encyclopedia ID of each topic in the entry
#' @export
#'
#' @examples
#' test:   xx <- pepys_entry_page_df(ymd("1666-09-02"))
pepys_entry_page_df <- function(d) {
  ids <- fromJSON(str_c("https://www.pepysdiary.com/api/v1/entries/", sprintf("%04d-%02d-%02d", year(d), month(d), day(d))))$entry_html %>% 
    read_html() %>%
    html_nodes("a") %>%   # see vignette("selectorgadget")
    html_attrs() %>% 
    keep(function(x) str_detect(x, "www.pepysdiary.com/encyclopedia")) %>%  # verify link is to encyclopedia entry
    str_extract("\\d+")    # extract id from entry
  return(data_frame(date = d, id = ids))
}

test <- function(d) {
  ids <- fromJSON(str_c("https://www.pepysdiary.com/api/v1/entries/", sprintf("%04d-%02d-%02d", year(d), month(d), day(d))))$entry_html 
  ids <- ids %>%  read_html() 
  ids <- ids %>% html_nodes("a")    # see vignette("selectorgadget")
    ids <- ids %>% html_attrs() 
    if(length(ids) < 1) print("html attrs")
    ids <- ids %>% keep(function(x) length(x) == 1)
    ids <- ids %>% keep(function(x) str_detect(x, "www.pepysdiary.com/encyclopedia"))   # verify link is to encyclopedia entry
    if(length(ids) < 1) print("str_detect")
    ids <- ids %>% str_extract("\\d+")    # extract id from entry
    if(length(ids) < 1) print("d+")
    return(data_frame(date = d, id = ids))
}
#' For a particular entry, get all of the topics (encyclopedia id's) in that entry
#'
#' @param d date of entry (in Date format)
#' @param url URL for the API for an entry
#'
#' @return data frame of id's and titles
#' @export
#'
#' @examples
#' test: xx <- pepys_entry_topics(d = "1666-09-02")
pepys_entry_topics <- function(d, url = NULL) {
  if (is.null(url)) {
    url <- str_c("https://www.pepysdiary.com/api/v1/entries/", sprintf("%04d-%02d-%02d", year(d), month(d), day(d)))
  }
  response <- GET(url)
  if (response$status_code != 200) {
    error(str_c("Status code is ", response$status_code, " url is ", url))
  }
  the_result <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  if (is.null(the_result$topics)) return(NULL)
  ids <- the_result$topics %>%
    keep(function(x) str_detect(x, "www.pepysdiary.com/api/v1/topics")) %>%  # verify link is to encyclopedia entry
    str_extract("(?<=v1/topics/)\\d+")    # extract id from entry
  return(data_frame(date = d, id = ids))
}

# test: xx <- tidy_pepys_entry_topics(entry_dates[100:110, ])
tidy_pepys_entry_topics <- function(entry_dates) {
    map2_dfr(entry_dates$date, entry_dates$api_url, pepys_entry_topics)
}

#test: xx <- retrieve_topics(max_page = 3)
retrieve_topics <- function(next_url = "https://www.pepysdiary.com/api/v1/topics?page=1", max_page_count = 10000) {
  accum_df <- NULL
  page_count <- 0
  topic_count <- 0
  has_data <- 0
  # Ideally, one would set list_size so large that it will never be an issue.
  # But if it is not enough, accumulate results in accum_df
  list_size <- 20000
  save_results <- 0
  accum <- vector("list", list_size)
  repeat {
    page_count <- page_count + 1
    save_results <- save_results + 1
    if (page_count > max_page_count) {
      warning("Maximum page count reached.")
      break
    }
    # print(next_url)
    if (is.null(next_url)) break
    if (next_url == "") break
    response <- GET(next_url)
    if (response$status_code != 200) {
      print(str_c("Status code is ", response$status_code))
      break
    }
    # print("just after break")
    the_result <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
    next_url <- the_result$`next`
    if (save_results > list_size) {
      print(str_c("Saving results. ", save_results, " ", list_size))
      accum_df <- bind_rows(accum_df, accum)
      accum <- vector("list", list_size)
      save_results <- 1
    }
    accum[[save_results]] <- the_result$results
    topic_count <- topic_count + dim(the_result$results)[1]
    has_data <- 1
  }
  if (has_data) {
    print(str_c("Topic count = ", topic_count, " page_count = ", page_count - 1))
    return(bind_rows(accum_df, accum))
  }
  return(NULL)
}
retrieve_topics <- cmpfun(retrieve_topics)

#' Using an enclopedia id, get the title of the topic
#'
#' @param id encyclopedia id
#'
#' @return title of the encyclopedia entry for that id
#' @export
#'
#' @examples
#' topic_detail_from_id(796)
topic_detail_from_id <- function(id) {
  response <- GET(str_c("https://www.pepysdiary.com/api/v1/topics/", id))
  if (response$status_code != 200) {
    print(str_c("Status code is ", response$status_code))
    return(NULL)
  }
  # print("just after break")
  the_result <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  return(the_result$title)
  # There are lots of other things besides title that could be returned:
  # "title": "Fenchurch Street",
  # "order_title": "Fenchurch Street",
  # "wheatley_html": "",
  # "tooltip_text": "A large street running from Aldgate to Gracechurch Street and Lombard Street.",
  # "wikipedia_url": "https://en.wikipedia.org/wiki/Fenchurch_Street",
  # "thumbnail_url": null,
  # "annotation_count": 3,
  # "last_annotation_time": "2013-06-19T15:00:26.464647Z",
  # "is_person": false,
  # "is_place": true,
  # "latitude": 51.51185,
  # "longitude": -0.081099,
  # "zoom": 15,
  # "shape": "51.511944,-0.085127;51.511876,-0.084971;51.511814,-0.084736;51.511747,-0.084366;51.51172,-0.08425;51.511663,-0.083922;51.511626,-0.083552;51.511578,-0.083132;51.511575,-0.082678;51.511593,-0.082298;51.511625,-0.082109;51.511639,-0.081946;51.511701,-0.081622;51.511727,-0.081547;51.511977,-0.080659;51.512185,-0.079935;51.512305,-0.07956;51.512455,-0.079224;51.512685,-0.078695;51.512801,-0.078459;51.512923,-0.078217;51.513044,-0.077973;51.513201,-0.07769;51.513296,-0.077548",
  # "categories": [
  #   "https://www.pepysdiary.com/api/v1/categories/streets"
  #   ],
  
}