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
  group_by(user) %>%
  summarise(sites_visited = sum(visit)) %>% data.frame()
