# Final Project

# Load package
library(dplyr)
library(lubridate)
library(ggplot2)

### set the seed to 1234
set.seed(1234)

## Load cleaned data
load("./data/cleaned_data.Rdata")

# mutate data, get hour, month
complaint_data <- complaint_data %>%
  mutate(hour = hour(occurrence_start_time),
         month = month(occurrence_start_time)) %>%
  select(-occurrence_start_time, -occurrence_finish_time, 
         -finish_time, -report_police_date, -pd_desc, -offense_desc)

# shuffle data and split train-test with 0.8
n <- nrow(complaint_data) # 6165765
train_indices <- sample(seq_len(n), size = floor(0.1 * n), replace = FALSE)
data_train <- complaint_data[train_indices, ]
data_test <- complaint_data[-train_indices, ]

# Perform logistic regression with stepwise selection
model <- glm(offense_level ~ ., data = data_train, family="binomial")
model <- stepAIC(model, direction = "both")


#AUC
#make prediction and calculate auc
prediction <- predict(model,data_test,type = 'response')
test.pred <- prediction(prediction, data_test$offense_level)
test.perf <- performance(test.pred, "auc")
cat('the auc score is ', test.perf@y.values[[1]], "\n")

#metrics
# Compute confusion matrix
confusion_matrix <- table(prediction, data_test$offense_level)

# Compute true positives, false positives, true negatives, and false negatives
tp <- confusion_matrix[2, 2]
fp <- confusion_matrix[1, 2]
tn <- confusion_matrix[1, 1]
fn <- confusion_matrix[2, 1]

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

##Random Forest
#2.
# fit a random forest model 
model2<- ranger(offense_level ~ ., data = data_train, probability = TRUE, respect.unordered.factors = TRUE, num.trees = 1000)

prediction.2 <- predict(model2, data_test, type='response')$predictions[,2]
test.pred_test.2 <- prediction(prediction.2 , data_test$offense_level)
test.perf_test.2 <- performance(data_test.pred_test.2, "auc")
cat('the auc score is ', test.perf_test.2@y.values[[1]], "\n")

#metrics
# Compute confusion matrix
confusion_matrix <- table(prediction.2, data_test$offense_level)

# Compute true positives, false positives, true negatives, and false negatives
tp <- confusion_matrix[2, 2]
fp <- confusion_matrix[1, 2]
tn <- confusion_matrix[1, 1]
fn <- confusion_matrix[2, 1]

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


# make precision-at-k% plot
data_test$predicted_test <- prediction
log.plot <- data_test[order(data_test$predicted_test, decreasing = T), ]
log.plot <- log.plot[-c(1:100), ]
data_test$predicted_test_2 <- prediction.2
ran.plot <- data_test[order(data_test$predicted_test_2, decreasing = T), ]
ran.plot <- ran.plot[-c(1:100), ]
log.plot$rank <- 1:nrow(log.plot)
ran.plot$rank <-  1:nrow(ran.plot)
log.plot$precision <-  cumsum(log.plot$outcome==1)/log.plot$rank
ran.plot$precision <-  cumsum(ran.plot$outcome==1)/ran.plot$rank

png(file="figures/performance_plot.png")
plot(log.plot$rank, log.plot$precision, xlab="The Number of Restaurants", ylab="Precision", type="l", col="red")
lines(ran.plot$rank, ran.plot$precision, type="l", col="green")
title("Precision-at-k Curves")
legend(12000, 1, c("Random Forest", "logitstic regression"), lwd=c(3,2), col=c("green","red"), y.intersp=1)
dev.off()

plot(log.plot$rank, log.plot$precision, xlab="The Number of Restaurants", ylab="Precision", type="l", col="red")
lines(ran.plot$rank, ran.plot$precision, type="l", col="green")
title("Precision-at-k Curves")
legend(12000, 1, c("Random Forest", "logitstic regression"), lwd=c(3,2), col=c("green","red"), y.intersp=1)