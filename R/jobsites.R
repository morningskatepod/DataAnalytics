library(dplyr)
library(stringr)

'''
searches urls for the job search sites
returns the total sites visited over the time period

'''

job_list <- c('monster', 'indeed','job','careerbuilder', 'ziprecruiter')

jobuse <- function(url) {
  for(job in job_list) {
    if(str_detect(url, job) == TRUE) {return(TRUE);break}
  }
return(FALSE)
}

http$visit <- unlist(lapply(http$url, FUN = jobuse))

# filter out the urls on facebook
total_visits <- http %>%
  filter(visit == TRUE) %>%
  mutate(day = format(as.POSIXct(strptime(date,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%Y-%m-%d")) %>%
  group_by_(.dots = c("day","user")) %>%
  summarise(total = ifelse(sum(visit)>0, 1,0)) %>% ungroup() %>%
  group_by(user) %>%
  summarise(days_onsite = sum(total)) %>% data.frame()
