library(e1071)

#' Support Vector Machine Classifer
#'
#' @param train_features A matrix of features to train the model 
#' @param train_labels The corresponding vector of the train_features labels
#' @param test_features A matrix of features to make predictions on the model
#' @param test_label The corresponding vector of the test_features labels
#'
#' @return A confusion matrix
SVM <- function(train_features, train_labels, test_features, test_label) {

  # SVM with radial kernel and gamma=1/62
  train_svm <- tune.svm(
    train_features,
    train_labels,
    cost = 1:5
  )

  # Hyperparameter tuning
  # Choose the cost value that gives the smallest cross-validation error
  tuned_svm <- svm(
    train_features,
    train_labels,
    cost = train_svm$best.parameters
  )

  # Make predictions on the test set.
  predicted_labels <- predict(
    tuned_svm,
    test_features
  )

  # Create the confusion matrix
  confusion_matrix <- table(
    Class = test_label,
    Pred = factor(predicted_labels, levels = 0:9)
  )
}

confusion_matrix <- SVM(
  train_features = as.matrix(train_set[-1]),
  train_labels = train_set$label,
  test_features = as.matrix(test_set[-1]),
  test_label = test_set$label
)

# Print the confusion matrix
print(confusion_matrix)

# Compute accuracy on test data
print(paste0("accuracy: ", sum(diag(confusion_matrix)) / sum(confusion_matrix)))