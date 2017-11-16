library(dplyr)
library(tidytext)

employee_list <- read.csv('Employee_info.csv')

nrc_neg <- get_sentiments('nrc') %>%
  filter(sentiment == 'negative')

emails <- read.csv('email_info.csv')
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
  summarise(count = sum(n), emails = n()) %>% data.frame()

#Sums neg words by ind email
neg_indemails <- neg_emails %>%
  inner_join(nrc_neg) %>%
  count(word, index = X, sort = TRUE) %>%
  group_by(index) %>%
  summarise(count = sum(n)) %>% data.frame()

