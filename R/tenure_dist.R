# loop through employee data sets and find employee start and end dates
library(dplyr)
library(lubridate)
library(readr)
library(hms)
employee_list <- read.csv('data/Employee_info.csv')
big_data <- read_csv("data/logon_info.csv")
unique_users <- unique(big_data$user)
big_data$day <- format(as.POSIXct(strptime(big_data$date,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%Y-%m-%d")
# usr <- sample(unique_users, 1)
#
# usr_dat <- big_data %>%
#   filter(user == usr) %>%
#   arrange(date)
# tenure <- head(usr_dat$day, 1) %--% tail(usr_dat$day, 1)
# tenure <- as.duration(tenure)
# tenure <- as.numeric(tenure, "days")

for(usr in unique_users){
  usr_dat <- big_data %>%
    filter(user == usr) %>%
    arrange(day)
  tenure <- head(usr_dat$day, 1) %--% tail(usr_dat$day, 1)
  tenure <- as.duration(tenure)
  tenure <- as.numeric(tenure, "days")

  row <- data.frame(user = usr,
                    # attrition = unique(usr_dat$attrition),
                    # role = unique(usr_dat$role),
                    tenure_days = tenure,
                    start_date = head(usr_dat$day, 1),
                    end_date = tail(usr_dat$day, 1))

  if(!exists("tenure_distribution")){
    tenure_distribution <- row
  } else{
    tenure_distribution <- rbind(tenure_distribution, row)
  }
}

tenure_distribution <- merge(tenure_distribution, employee_list[,c("user_id","role","attrition")], by.x = 'user',by.y = 'user_id')

# write_csv(tenure_distribution, "tenure_distribution.csv")
