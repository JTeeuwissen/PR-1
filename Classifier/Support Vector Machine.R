library(e1071)

SVM <- function(features, labels) {
  
  # SVM with radial kernel and gamma=1/62
  train_svm <- tune.svm(
    features[train_indices, ],
    labels[train_indices],
    cost = 1:5
  )
  
  # Hyperparameter tuning
  # Choose the cost value that gives the smallest cross-validation error
  tuned_svm <- svm(
    features[train_indices, ],
    labels[train_indices],
    cost = train_svm$best.parameters
  )
  
  # Make predictions on the test set.
  predicted_labels <- predict(
    tuned_svm,
    features[-train_indices, ]
  )

  # Create the confusion matrix
  confusion_matrix <- table(
    Class = labels[-train_indices],
    Pred = factor(predicted_labels, levels = 0:9)
  )
}

confusion_matrix <- SVM(
  features = cbind(scale(ink), scale(edge)),
  labels = data$label
)

# Print the confusion matrix
print(confusion_matrix)

# Compute accuracy on test data
print(paste0("accuracy: ", sum(diag(confusion_matrix)) / sum(confusion_matrix)))