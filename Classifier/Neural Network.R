library(nnet)
library(e1071)
library(caret)
library(doParallel)

#' Hyperparameter tuning for Neural Network
#'
#' @param train_features 
#' @param train_labels 
#'
#' @return NN with size-value that gives the smallest cross-validation error
sizeTuning <- function(train_features, train_labels){
  
  # Set up training parameters
  TrainingParameters <- trainControl(method = "cv", number = 3)
  
  # Enable parallel processing
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  
  # Train NN with hyperparameter tuning on the cost param.
  tune_nn <- train(
    train_features,
    train_labels,
    trControl= TrainingParameters,
    method = "nnet",
    tuneGrid = expand.grid(
      .size = seq(25, 40, by = 5),
      .decay = 0
    ),
    MaxNWts = 15910,
    maxit = 200
  )
  
  # Disable parallel processing
  stopCluster(cl)
  
  return(tune_nn)
}

#' Neural Network Classifier
#'
#' @param train_features A matrix of features to train the model
#' @param train_labels The corresponding vector of the train_features labels
#' @param test_features A matrix of features to make predictions on the model
#' @param test_label The corresponding vector of the test_features labels
#'
#' @return A confusion matrix
neural_network <- function(train_features,
                           train_labels,
                           test_features,
                           test_label) {
  
  # Train Neural Network using the optimal size-value
  train_nn <- sizeTuning(train_features, train_labels)
  
  # Make predictions on the test set.
  predicted_labels <- predict(
    train_nn,
    test_features
  )

  # Create the confusion matrix
  confusion_matrix <- table(
    Obs = test_label,
    Pred = factor(predicted_labels, levels = 0:9)
  )
}


# using all cells
#confusion_matrix <- neural_network(
#  train_features = train_set,
#  train_labels = train_labels,
#  test_features = test_set,
#  test_label = test_labels

#)
#print_confusion_matrix(confusion_matrix)

# using all cells low resolution
#confusion_matrix_low <- neural_network(
#  train_features = train_set_low,
#  train_labels = train_labels,
#  test_features = test_set_low,
#  test_label = test_labels
#)
#print_confusion_matrix(confusion_matrix_low)