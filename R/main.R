# main
library(readr)
library(stringr)
rm(list = ls())
options(stringsAsFactors = FALSE)

libs <- c("readr", "stringr", "lubridate", "hms", "caret", "glmnet")
lapply(libs, library, character.only = TRUE)

# tenure_distribution <- read_csv("../data/tenure_distribution.csv")
source("DataAnalytics/R/tenure_dist.R")
fired <- tenure_distribution %>%
  filter(attrition == 1) %>%
  arrange(end_date)

all_data <- read_csv("data/email_info.csv")
all_data$day <- as.Date(format(as.POSIXct(strptime(all_data$date,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%Y-%m-%d"))

start_date <- as.Date(min(unique(fired$start_date)))
end_dates <- unique(fired$end_date)
# test
# end_dates <- end_dates[1:3]

for(end_date in end_dates){

  end_date <- as.Date(end_date)

  # create directory
  dir.create(paste0("output/", end_date))

  # filter dates
  emails <- all_data %>%
    filter(between(day, start_date, end_date))

  # source files
  source('DataAnalytics/R/email_network.R')


  # write outputs
  write_csv(email_distribution,
            path = paste0("output/", end_date, '/', end_date,
                          "_email_distribution.csv"))

  # garbage collection
  rm(email_distribution, raw,
     logfit, trc)

}
