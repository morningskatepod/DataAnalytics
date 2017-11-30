library(dplyr)

#determine time between visiting url and next url
webtimes <- http %>%
  mutate(time = format(as.POSIXct(strptime(date,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%H:%M:%S")) %>%
  mutate(day = format(as.POSIXct(strptime(date,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%Y-%m-%d")) %>%
  select(user, url, time, date, day) %>%
  group_by(user) %>%
  arrange(date) %>%
  mutate(time_between = as.numeric(((as.POSIXct(lead(time, 1), format = '%H:%M:%S')) -
  (as.POSIXct(time, format = '%H:%M:%S')))/60, units = 'secs')) %>%
  ungroup() %>%
  filter(time_between > 0) %>%
  data.frame()

# filter out the urls on facebook
facebook <- webtimes %>%
  mutate(fb = sum(str_detect(url, 'facebook'))) %>%
  filter(fb == 1, time_between < 60) %>%
  group_by_(.dots = c("day","user")) %>%
  summarise(total = sum(time_between)) %>% ungroup() %>%
  group_by(user) %>%
  summarise(median = median(total), avg = mean(total)) %>% data.frame()
