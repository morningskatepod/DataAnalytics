library(dplyr)
library(stringr)
# emails <- read.csv('data/email_info.csv')
emails$content <- as.character(emails$content)
emails$day <- format(as.POSIXct(strptime(emails$date,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%Y-%m-%d")
emails$spam <- ifelse(unlist(lapply(lapply(str_match_all(emails$content, "\\S+"),unique),length))<20,1,0)
days <- unique(emails$day)

emails <- subset(emails, emails$spam == 1)

newusers <- c()
daycounts <- matrix(ncol = 2, nrow = 0)
colnames <- c('date','count')

for(d in days) {
  users <- emails %>%
    filter(day == d) %>%
    summarise(u = unique(user)) %>% data.frame()
  daycounts <- rbind(daycounts, c(date = d,count = sum(users[,1] %in% newusers)))
  newusers <- unique(c(newusers, as.character(users[,1])))
  }
