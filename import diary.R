# I'm using this info from tidytext as my model: 
# https://github.com/juliasilge/tidytext#tidytext-text-mining-using-dplyr-ggplot2-and-other-tidy-tools

library(readr)
library(lubridate)
library(plyr)
library(dplyr)
library(stringi)
library(stringr)
library(tidytext)
#library(tokenizers)
library(devtools)

# install_github("juliasilge/tidytext")

# pepys_text <- read_lines("http://www.gutenberg.org/cache/epub/3331/pg3331.txt", skip = 913, n_max = 1000)

# this is the URL for Guttenberg version that is the same as pepys.diary.com:
# http://www.gutenberg.org/cache/epub/4200/pg4200.txt
# human readable html version: http://www.gutenberg.org/files/4200/4200-h/4200-h.htm

# this is previous edition
#pepys_text <- read_lines("http://www.gutenberg.org/cache/epub/3331/pg3331.txt", skip = 913, n_max = 1000)
pepys_text <- read_lines("http://www.gutenberg.org/cache/epub/4200/pg4200.txt", skip = 1696) #, n_max = 30000)

# find lines beginning with 5 spaces (may be commentary rather than Pepys text)
#save_pepys_text <- pepys_text
pepys_text <- pepys_text[which(!str_detect(pepys_text, "\\A     "))]

# lines containing year
# per stringr docs, used http://www.regular-expressions.info/anchors.html#multi to figure out expression
# \A anchors to start of line, + is 1 or more, need to escape period because it matches anything
# Find beginning of an entry (but misses the first entry of the month such as January 1st)
#pepys_text[which(str_detect(pepys_text, regex("\\A[0-9]+(th|nd|rd|st)\\. ", ignore_case = TRUE)))]

# repair typo in dates
error1 <- which(str_detect(pepys_text, "4th. At the office all the morning. At noon I went by appointment"))
pepys_text[error1] <- str_replace(pepys_text[error1], "4th", "14th")

error2 <- which(str_detect(pepys_text, "\\A10th. Sir W. Pen and I did a little business "))
pepys_text[error2] <- str_replace(pepys_text[error2], "10th", "20th")

error3 <- which(str_detect(pepys_text, "\\A21 st. Up, and to my office, where"))
pepys_text[error3] <- str_replace(pepys_text[error3], "21 st.", "21st.")

# find the heading for the month that contains the year
year_heading_index <- which(str_detect(pepys_text, regex("\\A(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?|Sept\\.)\\b[\\s]+\\d\\d\\d\\d", ignore_case = TRUE)))
# now find the month heading information
months <- ldply(year_heading_index, function(x){
  mnths <- c(JANUARY = 1, FEBRUARY = 2, MARCH = 3, APRIL = 4, MAY = 5, JUNE = 6, JULY = 7, AUGUST = 8, 
             SEPTEMBER = 9, OCTOBER = 10, NOVEMBER = 11, DECEMBER = 12)
  month <- mnths[str_extract(pepys_text[x], "[\\w]+")]
  return(data_frame(index = x, text = pepys_text[x], month = month, year = as.numeric(str_extract(pepys_text[x], "\\d\\d\\d\\d")) + str_detect(pepys_text[x], "\\d\\d\\d\\d-\\d\\d")))
})
# look for some odd entry beginnings
#odd_index <- which(str_detect(pepys_text, regex("\\A[0-9]+(d\\.|d )", ignore_case = TRUE)))

# Find beginning of the first entry of the month
#pepys_text[which(str_detect(pepys_text, regex("\\A(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)\\b[\\s\\.]+[0-9]+(th|nd|rd|st)", ignore_case = TRUE)))]
entry_index <- c(which(str_detect(pepys_text, regex("\\A[0-9]+(th|nd|rd|st)", ignore_case = TRUE))),
                 which(str_detect(pepys_text, regex("\\A[0-9]+(d\\.|d )", ignore_case = TRUE))), # starts 2d. or 22d 
                 which(str_detect(pepys_text, regex("\\A(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Sept|Oct(ober)?|Nov(ember)?|Dec(ember)?)\\b[\\s\\.]+[0-9]+(th|nd|rd|st)", ignore_case = TRUE))))
entry_index <- entry_index[order(entry_index)]

# Find beginning of a month or the first entry of the month (use to find end of an entry)
#pepys_text[which(str_detect(pepys_text, regex("\\A(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)\\b", ignore_case = TRUE)))]
next_entry_index <- which(str_detect(pepys_text, regex("\\A(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)\\b", ignore_case = TRUE)))
next_entry_index <- unique(c(next_entry_index, entry_index))
next_entry_index <- next_entry_index[order(next_entry_index)]
entry_last_index <- next_entry_index - 1

entry_df <- ldply(entry_index, function(x) {
  day <- as.numeric(str_extract(pepys_text[x],"[\\d]+"))
  month <- months$month[months$index == max(months$index[x > months$index])] 
  year <- months$year[months$index == max(months$index[x > months$index])] 
  entry_date <- ymd(sprintf("%d-%d-%d", year, month, day))
  week <- week(entry_date)
  last_index <- min(entry_last_index[entry_last_index > x], na.rm = TRUE)
  if (is.infinite(last_index)) last_index <- length(pepys_text)
  
  #entry_text <- str_c(pepys_text[x:last_index], collapse = " ")
  # remove everything between square brackets:
  entry_text <- str_replace_all(str_c(pepys_text[x:last_index], collapse = " "), "\\[.*?]","")
  #tricky entry: xx <- entry_df$entry_text[entry_df$date == ymd("1663-05-15")]
  
  char_stats <- stri_stats_latex(entry_text)
   word_count <- char_stats[4]
   characters <- char_stats[1]
  #print(sprintf("%d day=%d %d %d", x, day, length(last_index), length(entry_text)))
  data_frame(entry_index = x, entry_last_index = last_index, entry_text = entry_text,
             word_count = word_count, characters = characters,
             heading_text = pepys_text[x], 
             day = day, month = month, year = year, week = week, date = entry_date)
}, .progress = "text")
# regular expression for text between brackets. See http://stackoverflow.com/questions/2403122/regular-expression-to-extract-text-between-square-brackets
# You can use the following regex globally:
#   \[(.*?)\]
# 
# Explanation:
#   \[ : [ is a meta char and needs to be escaped if you want to match it literally.
#   (.*?) : match everything in a non-greedy way and capture it.
#   \] : ] is a meta char and needs to be escaped if you want to match it literally.

# explore problem entry
# str_extract_all(xx, "\\[(.*?)\\]") 
# str_replace_all(xx, "\\[.*?]","")
# check this entry:  entry_df$entry_text[entry_df$date == ymd("1663-05-15")]

all_dates <- data_frame(date = seq(min(entry_df$date), max(entry_df$date), by = "days"))
entry_df <- full_join(entry_df, all_dates)
missing <- filter(entry_df, is.na(day))
date_to_fix <- ymd("1661-11-26")
entry_df$entry_text[entry_df$date == date_to_fix] <- "Not well in the morning and lay long in bed. At last rise and at noon with my wife to my Uncle Wights, where we met Mr. Cole, Mr. Rawlinson, Norbury and his wife and her daughter, and other friends to the Chine of beef that I sent them the other day, and eat and were merry. By and by I am called to the office, whither I went and there we sat late; and after the office done, Sir Wms both and I and Captain Cock and Mr. Bence (who being drunk, showed himself by his talk a bold foole, and so we were fain to put him off and get him away) we sat till 9 a-clock by ourselfs in the office, talking and drinking three or four bottles of wine. And so home and to bed. My wife and her mayde Dorothé falling out, I was troubled at it."
char_stats <- stri_stats_latex(entry_df$entry_text[entry_df$date == date_to_fix])
entry_df$word_count[entry_df$date == date_to_fix] <- char_stats[4]
entry_df$characters[entry_df$date == date_to_fix] <- char_stats[1]
entry_df$day[entry_df$date == date_to_fix] <- day(date_to_fix)
entry_df$week[entry_df$date == date_to_fix] <- week(date_to_fix)

date_to_fix <- ymd("1662-03-25")
entry_df$entry_text[entry_df$date == date_to_fix] <- "Lady Day. All the morning at the office. Dined with my wife at home. Then to the office, where (while Sir Wms both did examine the Victuallers account) I sat in my closet drawing letters and other businesses — being much troubled for want of an order of the Councells lately sent us, about making of boates for some ships now going to Jamaica. At last, late at night, I had a Copy sent me of it by Sir G. Lane from the Council Chamber. With my mind well at ease, home and to supper and bed."
char_stats <- stri_stats_latex(entry_df$entry_text[entry_df$date == date_to_fix])
entry_df$word_count[entry_df$date == date_to_fix] <- char_stats[4]
entry_df$characters[entry_df$date == date_to_fix] <- char_stats[1]
entry_df$day[entry_df$date == date_to_fix] <- day(date_to_fix)
entry_df$week[entry_df$date == date_to_fix] <- week(date_to_fix)

date_to_fix <- ymd("1662-05-13")
entry_df$entry_text[entry_df$date == date_to_fix] <- "At the office all morning. Dined at home alone, my wife being sick of her Mois in bed. Then to walk to Pauls churchyard, and there evened all reckonings to this day. So back to the office and so home. And Will Joyce came with a friend, a Cosen of his, to see me and I made them drink a bottle of wine; and so to sing and read and to bed."
char_stats <- stri_stats_latex(entry_df$entry_text[entry_df$date == date_to_fix])
entry_df$word_count[entry_df$date == date_to_fix] <- char_stats[4]
entry_df$characters[entry_df$date == date_to_fix] <- char_stats[1]
entry_df$day[entry_df$date == date_to_fix] <- day(date_to_fix)
entry_df$week[entry_df$date == date_to_fix] <- week(date_to_fix)

date_to_fix <- ymd("1662-08-16")
entry_df$entry_text[entry_df$date == date_to_fix] <- "Up by 4 a-clock. And up looking over my work, what they did yesterday; and am pretty well pleased, but I find it will be long before they have done, though the house is cover-d and I free from the weather. We met and sat all the morning, and at noon was sent for by my Uncle Wight to Mr. Rawlinson’s, and there we had a pigg, and Dr Fairebrother came to me to see me and dined with us. And after dinner he went away, and I by my uncles desire stayed; and there he begun to discourse about our difference with Mr. Young about Flaggs, pleading for him, which he did desire might be made up; but I told him how things was, and so he was satisfied and said no more. So home and above with my workmen, who I find busy and my work going on pretty well. And so to my office till night; and so to eat a bit and so to bed."
char_stats <- stri_stats_latex(entry_df$entry_text[entry_df$date == date_to_fix])
entry_df$word_count[entry_df$date == date_to_fix] <- char_stats[4]
entry_df$characters[entry_df$date == date_to_fix] <- char_stats[1]
entry_df$day[entry_df$date == date_to_fix] <- day(date_to_fix)
entry_df$week[entry_df$date == date_to_fix] <- week(date_to_fix)

date_to_fix <- ymd("1662-11-19")
entry_df$entry_text[entry_df$date == date_to_fix] <- "At home all the morning, putting some of my goods in order in my house; and after dinner, the like in the afternoon. And in the evening to my office, and there till 11 a-clock at night upon my Lord Treasurer’s letter again, and so home to bed."
char_stats <- stri_stats_latex(entry_df$entry_text[entry_df$date == date_to_fix])
entry_df$word_count[entry_df$date == date_to_fix] <- char_stats[4]
entry_df$characters[entry_df$date == date_to_fix] <- char_stats[1]
entry_df$day[entry_df$date == date_to_fix] <- day(date_to_fix)
entry_df$week[entry_df$date == date_to_fix] <- week(date_to_fix)

date_to_fix <- ymd("1662-11-26")
entry_df$entry_text[entry_df$date == date_to_fix] <- "Not well in the morning and lay long in bed. At last rise and at noon with my wife to my Uncle Wights, where we met Mr. Cole, Mr. Rawlinson, Norbury and his wife and her daughter, and other friends to the Chine of beef that I sent them the other day, and eat and were merry. By and by I am called to the office, whither I went and there we sat late; and after the office done, Sir Wms both and I and Captain Cock and Mr. Bence (who being drunk, showed himself by his talk a bold foole, and so we were fain to put him off and get him away) we sat till 9 a-clock by ourselfs in the office, talking and drinking three or four bottles of wine. And so home and to bed. My wife and her mayde Dorothé falling out, I was troubled at it."
char_stats <- stri_stats_latex(entry_df$entry_text[entry_df$date == date_to_fix])
entry_df$word_count[entry_df$date == date_to_fix] <- char_stats[4]
entry_df$characters[entry_df$date == date_to_fix] <- char_stats[1]
entry_df$day[entry_df$date == date_to_fix] <- day(date_to_fix)
entry_df$week[entry_df$date == date_to_fix] <- week(date_to_fix)

# two entries mixed up
date_to_fix <- ymd("1664-09-04")
entry_df$entry_text[entry_df$date == date_to_fix] <- "Lords day. Lay long in bed; then up and took physique, Mr Hollyard[‘s]. But it being cold weather and myself negligent of myself, I fear I took cold and stopped the working of it. But I feel myself pretty well. All the morning looking over my old wardrobe and laying by things for my brother John and my father, by which I shall leave myself very bare in clothes, but yet as much as I need and the rest would but spoil in the keeping. Dined, my wife and I, very well. All the afternoon my wife and I above, and then the boy and I to singing of psalms, and then came in Mr. Hill and he sung with us a while; and he being gone, the boy and I again to the singing of Mr. Porter’s mottets, and it is a great joy to me that I am come to this condition to maintain a person in the house able to give me such pleasure as this boy doth by his thorough understand of music, as he sing[s] any thing at first sight. Mr. Hill came to tell me that he had got a gentlewoman for my wife, one Mrs. Ferrabosco, that sings most admirably. I seemed glad of it; but I hear she is too gallant for me, and I am not sorry that I miss her. Thence to the office, setting some papers right, and so home to supper and to bed, after prayers."
char_stats <- stri_stats_latex(entry_df$entry_text[entry_df$date == date_to_fix])
entry_df$word_count[entry_df$date == date_to_fix] <- char_stats[4]
entry_df$characters[entry_df$date == date_to_fix] <- char_stats[1]
entry_df$day[entry_df$date == date_to_fix] <- day(date_to_fix)
entry_df$week[entry_df$date == date_to_fix] <- week(date_to_fix)

date_to_fix <- ymd("1664-09-03")
entry_df$entry_text[entry_df$date == date_to_fix] <- paste("I have had a bad night’s rest to-night, not sleeping well, as my wife observed, and once or twice she did wake me, and", 
                                                           "I thought myself to be mightily bit with fleas, and in the morning she chid her mayds for not looking the fleas a-days.",
                                                           "But, when I rose, I found that it is only the change of the weather from hot to cold, which, as I was two winters ago,", 
                                                           "do stop my pores, and so my blood tingles and itches all day all over my body, and so continued to-day all the day long",
                                                           "just as I was then, and if it continues to be so cold I fear I must come to the same pass, but sweating cured me then,",
                                                           "and I hope, and am told, will this also. At the office sat all the morning, dined at home, and after dinner to White Hall,", 
                                                           "to the Fishing Committee, but not above four of us met, which could do nothing, and a sad thing it is to see so great a", 
                                                           "work so ill followed, for at this pace it can come to nothing but disgrace to us all. Broke up and did nothing.",
                                                           "So I walked to Westminster, and there at my barber’s had good luck to find Jane alone; and there I talked with her",
                                                           "and got the poor wretch to promise to meet me in the abbey on tomorrow come sennit, telling me that her maister and",
                                                           "mistress have a mind to get her a husband, and so will not let her go abroad without them — but only in sermon time",
                                                           "a-Sundays she doth go out. I would I could get a good husband for her, for she is one I always thought a good-natured",
                                                           "as well as a well-looked girl. Thence home, doing some errands by the way; and so to my office, whither Mr. Holliard came",
                                                           "to me to discourse about the privileges of the Surgeon’s hall as to our signing of bills, wherein I did give him a little, and but a little, satisfaction; for we won’t lose our power of recommending them once approved of by the hall. He gone, I late to send by the post &c; and so to supper and to bed — my itching and tickling continuing still, the weather continuing cold. And Mr. Holliard tells me that sweating will cure me at any time.")
char_stats <- stri_stats_latex(entry_df$entry_text[entry_df$date == date_to_fix])
entry_df$word_count[entry_df$date == date_to_fix] <- char_stats[4]
entry_df$characters[entry_df$date == date_to_fix] <- char_stats[1]
entry_df$day[entry_df$date == date_to_fix] <- day(date_to_fix)
entry_df$week[entry_df$date == date_to_fix] <- week(date_to_fix)

date_to_fix <- ymd("1664-12-06")
entry_df$entry_text[entry_df$date == date_to_fix] <- "Up, and in Sir W. Batten’s coach to White Hall, but the Duke being gone forth, I to Westminster Hall, and there spent much time till towards noon to and fro with people. So by and by Mrs. Lane comes and plucks me by the cloak to speak to me, and I was fain to go to her shop, and pretending to buy some bands made her go home, and by and by followed her, and there did what I would with her, and so after many discourses and her intreating me to do something for her husband, which I promised to do, and buying a little band of her, which I intend to keep to, I took leave, there coming a couple of footboys to her with a coach to fetch her abroad I know not to whom. She is great with child, and she says I must be godfather, but I do not intend it. Thence by coach to the Old Exchange, and there hear that the Dutch are fitting their ships out again, which puts us to new discourse, and to alter our thoughts of the Dutch, as to their want of courage or force. Thence by appointment to the White Horse Taverne in Lumbard Streete, and there dined with my Lord Rutherford, Povy, Mr. Gauden, Creed, and others, and very merry, and after dinner among other things Povy and I withdrew, and I plainly told him that I was concerned in profit, but very justly, in this business of the Bill that I have been these two or three days about, and he consents to it, and it shall be paid. He tells me how he believes, and in part knows, Creed to be worth 10,000l.; nay, that now and then he [Povy] hath three or 4,000l. in his hands, for which he gives the interest that the King gives, which is ten per cent., and that Creed do come and demand it every three months the interest to be paid him, which Povy looks upon as a cunning and mean tricke of him; but for all that, he will do and is very rich. Thence to the office, where we sat and where Mr. Coventry came the first time after his return from sea, which I was glad of. So after office to my office, and then home to supper, and to my office again, and then late home to bed."
char_stats <- stri_stats_latex(entry_df$entry_text[entry_df$date == date_to_fix])
entry_df$word_count[entry_df$date == date_to_fix] <- char_stats[4]
entry_df$characters[entry_df$date == date_to_fix] <- char_stats[1]
entry_df$day[entry_df$date == date_to_fix] <- day(date_to_fix)
entry_df$week[entry_df$date == date_to_fix] <- week(date_to_fix)

missing <- filter(entry_df, is.na(day))
#    missing$date

#str_detect("Sept. 1st. Up pretty betimes, and aft", regex("\\A(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Sept|Oct(ober)?|Nov(ember)?|Dec(ember)?)\\b[\\s\\.]+[0-9]+(th|nd|rd|st)", ignore_case = TRUE))

# change cutoffs for year and month
entry_df$month <- cut(as.Date(entry_df$date), breaks = "month")
entry_df$year_scale <- cut(as.Date(entry_df$date), breaks = "year")
entry_df$week_scale <- cut(as.Date(entry_df$date), breaks = "week")

entry_df$date_url <- map_chr(entry_df$date, form_url_from_date)

# load("Search_Pepys/entry_df.RData")

# change money amounts to use £
source("fix_pounds.R")
entry_df$entry_text <- map(entry_df$entry_text, fix_pounds)
#yy <- str_detect(xx, "L\\d")
#     yy <- str_detect(xx, "£")


save(entry_df, file = "Search_Pepys/entry_df.RData")


