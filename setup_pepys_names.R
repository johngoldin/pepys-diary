
source("parse_name_item.R")
source("names_from_web_site.R")

#   load("~/Dropbox/Programming/R_Stuff/pepys-diary/pepys_names_from_website.RData")

split1 <- map(pepys_names$people, parse_name_item)

pepys_names$last_name <- unlist(map(split1, pluck, 1))
pepys_names$first_name <- unlist(map(split1, pluck, 2))
pepys_names$title <- unlist(map(split1, pluck, 3))
pepys_names$other_name <- unlist(map(split1, pluck, 4))
pepys_names$initial <- str_sub(pepys_names$first_name, 1, 1)
pepys_names$title2 <- str_replace(pepys_names$title, "\\.", "")

# Create a unique version of pepys_names

pepys_names <- pepys_names %>% 
  ungroup() %>% mutate(id = str_c(str_to_upper(str_sub(last_name, 1, 1)), dense_rank(people))) %>%
  group_by(last_name, first_name, title) %>%
  add_tally() %>% 
  mutate(last_first_title_use = ifelse(is.na(lag(n)), "TRUE", "FALSE"),
         last_first_title_dup = ifelse(n > 1, "dup", "")) %>% select(-n) %>%
  group_by(last_name, initial, title) %>%
  add_tally() %>% 
  mutate(last_initial_title_use = ifelse(is.na(lag(n)), "TRUE", "FALSE"),
         last_initial_title_dup = ifelse(n > 1, "dup", "")) %>% select(-n) %>%
  group_by(last_name, first_name) %>%
  add_tally() %>% 
  mutate(last_first_use = ifelse(is.na(lag(n)), "TRUE", "FALSE"),
         last_first_dup = ifelse(n > 1, "dup", "")) %>% select(-n) %>%
  group_by(last_name, initial) %>%
  add_tally() %>% 
  mutate(last_initial_use = ifelse(is.na(lag(n)), "TRUE", "FALSE"),
         last_initial_dup = ifelse(n > 1, "dup", "")) %>% select(-n) %>%
  group_by(last_name, title) %>%
  add_tally() %>% 
  mutate(last_title_use = ifelse(is.na(lag(n)), "TRUE", "FALSE"),
         last_title_dup = ifelse(n > 1, "dup", "")) %>% select(-n) 



# save(pepys_names, file = "pepys_names_from_website.RData")

# Here are some odd names: Sir without space
# > which(str_detect(title, "^Sir[A-Z]"))
# [1]  604  626  799 1002 1015
# > pepys_names$names[604]
# [1] "Cooper, SirAnthony Ashley (Baron Ashley, Chancellor of the Exchequer)"
# > pepys_names$names[626]
# [1] "Cotton, SirRobert Bruce"
# > pepys_names$names[799]
# [1] "Dyck, SirAnthony Van"
# > pepys_names$names[1002]
# [1] "Godfrey, SirEdmund Berry"
# > pepys_names$names[1015]
# [1] "Gomme, SirBernard de"


