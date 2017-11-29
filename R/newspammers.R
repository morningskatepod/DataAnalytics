library(dplyr)
# emails <- read.csv('data/email_info.csv')
emails$content <- as.character(emails$content)
emails$day <- format(as.POSIXct(strptime(emails$date,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%Y-%m-%d")

days <- unique(emails$day)

newusers <- c()
daycounts <- matrix(ncol = 2, nrow = 0)
colnames <- c('date','count')

for(d in days) {
  users <- emails %>%
    filter(day == d) %>%
    summarise(u = unique(user)) %>% data.frame()
  daycounts <- rbind(daycounts, c(date = d,count = sum(users[,1] %in% newusers)))
  newusers <- c(newusers, as.character(users[,1]))
  }
