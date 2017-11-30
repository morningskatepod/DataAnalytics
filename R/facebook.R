library(dplyr)

webtimes <- http %>%
  mutate(time = format(as.POSIXct(strptime(date,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%H:%M:%S"))
  select(user, url, time, date) %>%
  group_by(user) %>%
  arrange(date) %>%
  mutate(time_between = (time - lead(time, 1))/60) %>%
  ungroup() %>%
  filter(time_between > 0) %>%
  data.frame()
