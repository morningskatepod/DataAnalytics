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
resultsrf <- rep(0, length(end_dates))
resultscart <- rep(0, length(end_dates))
resultsknn <- rep(0, length(end_dates))
vars <- matrix(ncol = 4)

for(end_date in end_dates) {

email_distribution <- read_csv(paste0("output/",end_date,"/",end_date,
                                      "_email_distribution.csv"))
email_distribution <- email_distribution %>%
  mutate(neg_rate = count_neg/total_emails) %>%
  mutate(names_rate = names_used/total_emails) %>%
  select(-unmatched)

email_distribution <- merge(email_distribution, employee_info[,c('user_id', "attrition")],
      by.x = 'user', by.y = 'user_id')

end_d <- end_date
unfired <- tenure_distribution %>%
  filter(as.Date(end_date) > as.Date(end_d)) %>%
  select(user)
email_distribution <- email_distribution[email_distribution$user %in% unfired[,1],]

raw <- email_distribution[, -1]
raw <- scale(raw[,-c(4,7)])
raw <- cbind(raw, spam = email_distribution$spam)
raw <- cbind(raw, attrition = email_distribution$attrition)
raw <- raw[,-c(2,3,6)]

trc <- trainControl(method = "cv", number = 10)
logfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "glm",
                family = binomial())
rffit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "rf",
                family = binomial())
cartfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "rpart",
                cp = 0.005)
knnfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "knn"
                )

paste0("Overall error rate: ", 1 - logfit$results[, 2])
results[match(end_date, end_dates)] <- 1 - logfit$results[, 2]
resultsrf[match(end_date, end_dates)] <- tail(rffit$finalModel[4]$err.rate[,1],1)
resultscart[match(end_date, end_dates)] <- 1 - cartfit$results[3, 2]
resultsknn[match(end_date, end_dates)] <- 1 - knnfit$results[2, 2]
vars <- rbind(vars, logfit$finalModel[1]$coefficients)
}
output <- data.frame(lasso = results, RF = resultsrf, CART = resultscart, KNN = resultsknn)
