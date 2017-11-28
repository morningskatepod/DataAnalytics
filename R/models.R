# fit some models to get some results
library(dplyr)
library(glmnet)
library(caret)
library(e1071)

# rm(list = ls())


# email distribution model -------------------------------------------------
source("DataAnalytics/R/tenure_dist.R")
fired <- tenure_distribution %>%
  filter(attrition == 1) %>%
  arrange(end_date)

employee_info <- read_csv('data/Employee_info.csv')

end_dates <- unique(fired$end_date)
end_dates <- subset(end_dates, as.Date(end_dates) < as.Date('2010-09-22'))
results <- rep(0, length(end_dates))
for(end_date in end_dates) {

email_distribution <- read_csv(paste0("output/",end_date,"/",end_date,
                                      "_email_distribution.csv"))
email_distribution <- merge(email_distribution, employee_info[,c('user_id', "attrition")],
      by.x = 'user', by.y = 'user_id')

raw <- email_distribution[, -1]
raw <- scale(raw[,-6])
raw <- cbind(raw, attrition = email_distribution[,'attrition'])

trc <- trainControl(method = "cv", number = 10)
logfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "glm",
                family = binomial())
paste0("Overall error rate: ", 1 - logfit$results[, 2])

}
