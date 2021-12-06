library(e1071)
library(caret)
library(doParallel)

#' Hyperparameter tuning for SVM
#'
#' @param train_features 
#' @param train_labels 
#'
#' @return SVM with cost-value that gives the smallest cross-validation error
costTuning <- function(train_features, train_labels){
  
  # Set up training parameters
  TrainingParameters <- trainControl(method = "cv", number = 10)
  
  # Enable parallel processing
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  
  # Train SVM with hyperparameter tuning on the cost param.
  tune_svm <- train(
    train_features,
    train_labels,
    trControl= TrainingParameters,
    method = "svmLinear2",
    tuneGrid = expand.grid(cost = seq(0.1, 2, length = 14)),
    scale = F
  )
  
  # Disable parallel processing
  stopCluster(cl)
  
  return(tune_svm)
}

#' Support Vector Machine Classifer
#'
#' @param train_features A matrix of features to train the model
#' @param train_labels The corresponding vector of the train_features labels
#' @param test_features A matrix of features to make predictions on the model
#' @param test_label The corresponding vector of the test_features labels
#'
#' @return A confusion matrix
SVM <- function(train_features, train_labels, test_features, test_label) {
  
  # Train SVM using the optimal cost-value
  train_svm <- costTuning(train_features, train_labels)
  
  # Make predictions on the test set using the best trained model.
  predicted_labels <- predict(
    train_svm,
    test_features
  )

  # Create the confusion matrix
  confusion_matrix <- table(
    Obs = test_label,
    Pred = factor(predicted_labels, levels = 0:9)
  )
}

#confusion_matrix <- SVM(
# train_features = train_set,
#  train_labels = train_labels,
#  test_features = test_set,
#  test_label = test_labels
#)
#print_confusion_matrix(confusion_matrix)

# using all cells low resolution
#confusion_matrix_low <- SVM(
#  train_features = train_set_low,
#  train_labels = train_labels,
#  test_features = test_set_low,
#  test_label = test_labels
#)
#print_confusion_matrix(confusion_matrix_low)