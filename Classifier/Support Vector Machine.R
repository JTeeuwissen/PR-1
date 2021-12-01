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

  # Train SVM with gamma=1/62 and cost=1 (default values)
  train_svm <- svm(
    train_features,
    train_labels,
    kernel = "linear",
    scale = F
  )

  # Make predictions on the test set.
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

confusion_matrix <- SVM(
  train_features = train_set,
  train_labels = train_labels,
  test_features = test_set,
  test_label = test_labels
)
print_confusion_matrix(confusion_matrix)


# using all cells low resolution
confusion_matrix_low <- SVM(
  train_features = train_set_low,
  train_labels = train_labels,
  test_features = test_set_low,
  test_label = test_labels
)
print_confusion_matrix(confusion_matrix_low)