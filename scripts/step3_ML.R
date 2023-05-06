# Final Project

# Load package
library(dplyr)
library(lubridate)
library(ROCR)
# library(ranger)
# library(MASS)

### set the seed to 1234
set.seed(1234)

## Load cleaned data
load("./data/cleaned_data.Rdata")

# mutate data, get hour, month
complaint_data <- complaint_data %>%
  mutate(hour = hour(occurrence_start_time),
         month = month(occurrence_start_time)) %>%
  dplyr::select(-occurrence_start_time, -occurrence_finish_time, 
         -finish_time, -report_police_date, -pd_desc, -offense_desc, -premises)

# For multi-classification, build three dataset
felony_data <- complaint_data %>%
  mutate(judge_felony = ifelse(offense_level=="felony", 1, 0)) %>%
  select(-offense_level)

misdemeanor_data <- complaint_data %>%
  mutate(judge_misdemeanor = ifelse(offense_level=="misdemeanor", 1, 0)) %>%
  select(-offense_level)

violation_data <- complaint_data %>%
  mutate(judge_violation = ifelse(offense_level=="violation", 1, 0)) %>%
  select(-offense_level)

# split train-test with years
data_train_felony <- felony_data %>%
  filter(!occurrence_year == 2021, !occurrence_year == 2020)
data_test_felony <- felony_data %>%
  filter(occurrence_year == 2021 | occurrence_year == 2020)

data_train_misdemeanor <- misdemeanor_data %>%
  filter(!occurrence_year == 2021, !occurrence_year == 2020)
data_test_misdemeanor <- misdemeanor_data %>%
  filter(occurrence_year == 2021 | occurrence_year == 2020)

data_train_violation <- violation_data %>%
  filter(!occurrence_year == 2021, !occurrence_year == 2020)
data_test_violation <- violation_data %>%
  filter(occurrence_year == 2021 | occurrence_year == 2020)





#### Logistic Regression for felony
# Perform logistic regression
model_felony <- glm(judge_felony ~ ., 
                    data = data_train_felony, 
                    family="binomial")

#AUC
#make prediction and calculate auc
prediction <- predict(model_felony, data_test_felony,type = 'response')
prediction <- ifelse(prediction >= 0.3, 1, 0)
test.pred <- prediction(prediction, data_test_felony$judge_felony)
test.perf <- performance(test.pred, "auc")
cat('the auc score is ', test.perf@y.values[[1]], "\n")

#metrics
# Compute confusion matrix
confusion_matrix <- table(prediction, data_test_felony$judge_felony)

# Compute true positives, false positives, true negatives, and false negatives
tp <- confusion_matrix[2, 2]
fp <- confusion_matrix[2, 1]
tn <- confusion_matrix[1, 1]
fn <- confusion_matrix[1, 2]

# Compute accuracy
accuracy <- (tp + tn) / (tp + fp + tn + fn)
# Compute precision
precision <- tp / (tp + fp)
# Compute recall
recall <- tp / (tp + fn)
# Compute F1 score
f1_score <- 2 * precision * recall / (precision + recall)

# Print results
cat(sprintf("F1 score: %0.2f\n", f1_score))
cat(sprintf("Recall: %0.2f\n", recall))
cat(sprintf("Precision: %0.2f\n", precision))
cat(sprintf("Accuracy: %0.2f\n", accuracy))





#### Logistic Regression for misdemeanor
# Perform logistic regression
model_misdemeanor <- glm(judge_misdemeanor ~ ., 
                    data = data_train_misdemeanor, 
                    family="binomial")

#AUC
#make prediction and calculate auc
prediction <- predict(model_misdemeanor, 
                      data_test_misdemeanor,
                      type = 'response')
prediction <- ifelse(prediction >= 0.5, 1, 0)
test.pred <- prediction(prediction, data_test_misdemeanor$judge_misdemeanor)
test.perf <- performance(test.pred, "auc")
cat('the auc score is ', test.perf@y.values[[1]], "\n") 

#metrics
# Compute confusion matrix
confusion_matrix <- table(prediction, data_test_misdemeanor$judge_misdemeanor)

# Compute true positives, false positives, true negatives, and false negatives
tp <- confusion_matrix[2, 2]
fp <- confusion_matrix[2, 1]
tn <- confusion_matrix[1, 1]
fn <- confusion_matrix[1, 2]

# Compute accuracy
accuracy <- (tp + tn) / (tp + fp + tn + fn)
# Compute precision
precision <- tp / (tp + fp)
# Compute recall
recall <- tp / (tp + fn)
# Compute F1 score
f1_score <- 2 * precision * recall / (precision + recall)

# Print results
cat(sprintf("F1 score: %0.2f\n", f1_score))
cat(sprintf("Recall: %0.2f\n", recall))
cat(sprintf("Precision: %0.2f\n", precision))
cat(sprintf("Accuracy: %0.2f\n", accuracy))





#### Logistic Regression for violation
# Perform logistic regression
model_violation <- glm(judge_violation ~ ., 
                       data = data_train_violation, 
                       family="binomial")

#AUC
#make prediction and calculate auc
prediction <- predict(model_violation, 
                      data_test_violation, 
                      type = 'response')
prediction <- ifelse(prediction >= 0.4, 1, 0)
test.pred <- prediction(prediction, data_test_violation$judge_violation)
test.perf <- performance(test.pred, "auc")
cat('the auc score is ', test.perf@y.values[[1]], "\n") 

#metrics
# Compute confusion matrix
confusion_matrix <- table(prediction, data_test_violation$judge_violation)

# Compute true positives, false positives, true negatives, and false negatives
tp <- confusion_matrix[2, 2]
fp <- confusion_matrix[2, 1]
tn <- confusion_matrix[1, 1]
fn <- confusion_matrix[1, 2]

# Compute accuracy
accuracy <- (tp + tn) / (tp + fp + tn + fn)
# Compute precision
precision <- tp / (tp + fp)
# Compute recall
recall <- tp / (tp + fn)
# Compute F1 score
f1_score <- 2 * precision * recall / (precision + recall)

# Print results
cat(sprintf("F1 score: %0.2f\n", f1_score))
cat(sprintf("Recall: %0.2f\n", recall))
cat(sprintf("Precision: %0.2f\n", precision))
cat(sprintf("Accuracy: %0.2f\n", accuracy))




# ##Random Forest
# #2.
# # fit a random forest model 
# model2<- ranger(offense_level ~ ., data = data_train, probability = TRUE, 
#                 respect.unordered.factors = TRUE, num.trees = 1000)
# 
# prediction.2 <- predict(model2, data_test, type='response')$predictions[,2]
# test.pred_test.2 <- prediction(prediction.2 , data_test$offense_level)
# test.perf_test.2 <- performance(data_test.pred_test.2, "auc")
# cat('the auc score is ', test.perf_test.2@y.values[[1]], "\n")
# 
# #metrics
# # Compute confusion matrix
# confusion_matrix <- table(prediction.2, data_test$offense_level)
# 
# # Compute true positives, false positives, true negatives, and false negatives
# tp <- confusion_matrix[2, 2]
# fp <- confusion_matrix[1, 2]
# tn <- confusion_matrix[1, 1]
# fn <- confusion_matrix[2, 1]
# 
# # Compute accuracy
# accuracy <- (tp + tn) / (tp + fp + tn + fn)
# 
# # Compute precision
# precision <- tp / (tp + fp)
# 
# # Compute recall
# recall <- tp / (tp + fn)
# 
# # Compute F1 score
# f1_score <- 2 * precision * recall / (precision + recall)
# 
# # Print results
# cat(sprintf("F1 score: %0.2f\n", f1_score))
# cat(sprintf("Recall: %0.2f\n", recall))
# cat(sprintf("Precision: %0.2f\n", precision))
# cat(sprintf("Accuracy: %0.2f\n", accuracy))
# 
# 
# # make precision-at-k% plot
# data_test$predicted_test <- prediction
# log.plot <- data_test[order(data_test$predicted_test, decreasing = T), ]
# log.plot <- log.plot[-c(1:100), ]
# data_test$predicted_test_2 <- prediction.2
# ran.plot <- data_test[order(data_test$predicted_test_2, decreasing = T), ]
# ran.plot <- ran.plot[-c(1:100), ]
# log.plot$rank <- 1:nrow(log.plot)
# ran.plot$rank <-  1:nrow(ran.plot)
# log.plot$precision <-  cumsum(log.plot$outcome==1)/log.plot$rank
# ran.plot$precision <-  cumsum(ran.plot$outcome==1)/ran.plot$rank
# 
# png(file="figures/performance_plot.png")
# plot(log.plot$rank, log.plot$precision, xlab="The Number of Restaurants", 
#      ylab="Precision", type="l", col="red")
# lines(ran.plot$rank, ran.plot$precision, type="l", col="green")
# title("Precision-at-k Curves")
# legend(12000, 1, c("Random Forest", "logitstic regression"), lwd=c(3,2), 
#        col=c("green","red"), y.intersp=1)
# dev.off()
# 
# plot(log.plot$rank, log.plot$precision, xlab="The Number of Restaurants", 
#      ylab="Precision", type="l", col="red")
# lines(ran.plot$rank, ran.plot$precision, type="l", col="green")
# title("Precision-at-k Curves")
# legend(12000, 1, c("Random Forest", "logitstic regression"), lwd=c(3,2), 
#        col=c("green","red"), y.intersp=1)
# 
# 
# 
# 
