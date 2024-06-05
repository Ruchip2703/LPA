# Install and load the necessary packages
#install.packages(tibble)
#install.packages(readr)
#install.packages(caret)
#install.packages(tidyverse)
#install.packages(pROC)
#install.packages(glmnet)
library(tibble)
library(readr)
library(caret)
library(tidyverse)
library(pROC)
library(glmnet)

# Initialize vectors to store results
train_accuracy <- numeric(10)
test_accuracy <- numeric(10)
train_auc <- numeric(10)
test_auc <- numeric(10)
all_train_roc <- list()
all_test_roc <- list()

# Loop for ten splits
for (i in 1:10) {
  set.seed(123 + i)  # Set a different seed for each iteration
  
  # Split the data into training and testing sets
  trainIndex <- createDataPartition(results$Class, p = 0.9, list = FALSE)
  data_train <- results[trainIndex, ]
  data_test <- results[-trainIndex, ]
  
  # Prepare data for training
  train <- data_train[, c("var1", "var2",...,"varN")]
  Class_train <- factor(data_train$Class, levels = c("1", "2"))
  
  # Prepare data for testing
  test <- data_test[, c("var1", "var2",...,"varN")]
  Class_test <- factor(data_test$Class, levels = c("1", "2"))
  
  # Define the training control
  train_control <- trainControl(method = "cv", number = 10)
  
  # Train the logistic regression model
  model <- train(train, Class_train, method = "glmnet",
                 trControl = train_control, family = "binomial")
  
  # Make predictions on the training set
  P_train <- predict(model, train, type = "prob")
  predictions_train <- ifelse(P_train[,1] > 0.5, 1, 2)
  predictions_train <- factor(predictions_train, levels = c("1", "2"))
  confusion_train <- confusionMatrix(predictions_train, Class_train)
  
  # Make predictions on the test set
  P_test <- predict(model, test, type = "prob")
  predictions_test <- ifelse(P_test[,1] > 0.5, 1, 2)
  predictions_test <- factor(predictions_test, levels = c("1", "2"))
  confusion_test <- confusionMatrix(predictions_test, Class_test)
  
  # Calculate AUC for training and testing sets
  auc_train <- pROC::auc(ifelse(Class_train == "1", 1, 2), ifelse(predictions_train == "1", 1, 2))
  auc_test <- pROC::auc(ifelse(Class_test == "1", 1, 2), ifelse(predictions_test == "1", 1, 2))
  
  # Store ROC objects for plotting later
  all_train_roc[[i]] <- pROC::roc(ifelse(Class_train == "1", 1, 2), P_train[,1])
  all_test_roc[[i]] <- pROC::roc(ifelse(Class_test == "1", 1, 2), P_test[,1])
  
  # Store results in vectors
  train_accuracy[i] <- confusion_train$overall["Accuracy"]
  test_accuracy[i] <- confusion_test$overall["Accuracy"]
  train_auc[i] <- auc_train
  test_auc[i] <- auc_test
}

# Combine results into a dataframe
results_mlr <- data.frame(
  Train_Accuracy = train_accuracy,
  Test_Accuracy = test_accuracy,
  Train_AUC = train_auc,
  Test_AUC = test_auc
)
# View the results
print(results_mlr)

#CALCULATING MEANS
means_mlr <- results_mlr %>%
  summarise_all(list(mean = ~mean(., na.rm = TRUE), sd = ~round(sd(., na.rm = TRUE), 3)))

# View the summary with mean and sd values by class
print(means_mlr)



# Function to plot ROC curve for a specific iteration with a title
plot_roc_curve <- function(iteration) {
  # Plot the ROC curve for the specified iteration
  plot(all_test_roc[[iteration]], legacy.axes = TRUE, add = FALSE, col = "red",
       main = paste("Test AUROC"))
  
  # Retrieve and round the AUC value for the specified iteration
  test_auc_value <- round(test_auc[iteration], 4)
  
  # Add the AUC value as text on the plot
  text(0.3, 0.2, paste("Test AUC =", test_auc_value), col = "red")
}

# Plot the ROC curve for iteration 6
plot_roc_curve(6)


# Obtain coefficients from the trained model
coefficients <- coef(model$finalModel, s = model$bestTune$lambda)

# View the coefficients
coefficients










