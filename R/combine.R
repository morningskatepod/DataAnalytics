library(tidyverse)

# current employee from most recent employee data set
current <- read_csv("~/data_analytics/DataSets1_9182017/Employees_info/2011-May.csv")
current <- current %>%
  mutate(user = stringr::str_c(stringr::str_sub(Domain, 1, 4), user_id, sep = "/"))

# all employee data sites combined into single dataframe
emp <- list.files(path = "~/data_analytics/DataSets1_9182017/Employees_info/",
                  pattern = "*.csv", full.names = TRUE)
emp <- do.call(rbind, lapply(emp, read_csv)) %>% 
  mutate(user = stringr::str_c(stringr::str_sub(Domain, 1, 4), user_id, sep = "/"))
emp$Role <- stringr::str_replace_all(emp$Role, " ", "")