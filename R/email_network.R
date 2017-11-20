library(dplyr)
library(tidytext)
#

# employee_list <- read.csv('Employee_info.csv')

nrc_neg <- get_sentiments('nrc') %>%
  filter(sentiment == 'negative')

# emails <- read.csv('email_info.csv')
emails$content <- as.character(emails$content)

#Splits emails by word
neg_emails <- emails %>%
  select(X, user, date, content) %>%
  unnest_tokens(word, content)

#Sums neg words by user
neg_users <- neg_emails %>%
  inner_join(nrc_neg) %>%
  count(word, index = user, sort = TRUE) %>%
  group_by(index) %>%
  summarise(count_neg = sum(n)) %>%
  data.frame() %>% rename(user = index)

# #Sums neg words by ind email
# neg_indemails <- neg_emails %>%
#   inner_join(nrc_neg) %>%
#   count(word, index = X, sort = TRUE) %>%
#   group_by(index) %>%
#   summarise(count = sum(n)) %>% data.frame()

total_emails <- emails %>%
  group_by(user) %>%
  summarise(total_emails = n()) %>% data.frame() %>%
  merge(neg_users, all = TRUE) %>%
  mutate(count_neg = ifelse(is.na(count_neg), 0, count_neg))

unmatched <- emails %>%
  mutate(domain = sapply(strsplit(as.character(emails$from), "@"), `[`,2)) %>%
  filter(domain == 'dtaa.com') %>%
  group_by(user) %>%
  summarise(unmatched = length(unique(from))) %>%
  mutate(unmatched = ifelse(unmatched > 1, 1,0)) %>%
  data.frame()

name_list <- data.frame(word = unique(sapply(strsplit(as.character(employee_list$employee_name)," "), `[`,1)))
name_list[,1] <- sapply(as.character(name_list[,1]), tolower)

gossip <- neg_emails %>%
  inner_join(name_list) %>%
  count(word, index = user, sort = TRUE) %>%
  group_by(index) %>%
  summarise(names_used = sum(n)) %>%
  rename(user = index) %>%
  data.frame()

spam <- emails %>%
  mutate(n_words = unlist(lapply(lapply(str_match_all(emails$content, "\\S+"),unique),length))) %>%
  filter(n_words < 10) %>%
  group_by(user) %>%
  summarise(spam = ifelse(n() > 1,1,0)) %>%
  data.frame()

output <- merge(total_emails, unmatched, all = T)
output$unmatched <- ifelse(is.na(output$unmatched), 0, output$unmatched)
output <- merge(output, gossip, all = T)
output$names_used <- ifelse(is.na(output$names_used), 0, output$names_used)
output <- merge(output, spam, all = T)
output$spam <- ifelse(is.na(output$spam), 0, output$spam)

email_distribution <- output
