library(dplyr)
library(tidytext)

employee_list <- read.csv('Employee_info.csv')

nrc_neg <- get_sentiments('nrc') %>%
  filter(sentiment == 'negative')

emails <- read.csv('email_info.csv', nrows = 10000)
emails$content <- as.character(emails$content)
neg_emails <- emails %>%
  select(user, date, content) %>%
  unnest_tokens(word, content) %>%
  inner_join(nrc_neg) %>%
  count(word, index = user, sort = TRUE)

neg_users <- neg_emails %>%
  group_by(index) %>%
  summarise(count = sum(n)) %>% data.frame()

