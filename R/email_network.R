library(dplyr)
library(tidytext)

psycho <- read.csv("psychometric_info.csv")
employee_list <-  unique(psycho$user)

nrc_neg <- get_sentiments('nrc') %>%
  filter(sentiment == 'negative')

emails <- read.csv('email_info.csv', nrows = 1000)
emails$content <- as.character(emails$content)
neg_emails <- emails %>%
  select(user, content) %>%
  unnest_tokens(word, content) %>%
  inner_join(nrc_neg) %>%
  count(word, index = user, sort = TRUE)
