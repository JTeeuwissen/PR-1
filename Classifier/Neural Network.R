library(nnet)
library(e1071)

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

  # # TODO Hyperparameter tuning
  # tuned <- e1071::tune.nnet(
  #   train_features,
  #   train_labels,
  #   size = 10,
  #   MaxNWts = 7861
  # )


  df <- as.data.frame(train_features)
  df$label <- train_labels

  # Neural Network
  train_nn <- nnet.formula(
    label ~ .,
    df,
    size = 20,
    MaxNWts = 10000,
    maxit = 1000
  )

  # Make predictions on the test set.
  predicted_labels <- predict(
    train_nn,
    test_features,
    type = "class"
  )

  # Create the confusion matrix
  confusion_matrix <- table(
    Obs = test_label,
    Pred = factor(predicted_labels, levels = 0:9)
  )
}

# using all cells
confusion_matrix <- neural_network(
  train_features = train_set,
  train_labels = train_labels,
  test_features = test_set,
  test_label = test_labels
)
print_confusion_matrix(confusion_matrix)

# using all cells low resolution
confusion_matrix_low <- neural_network(
  train_features = train_set_low,
  train_labels = train_labels,
  test_features = test_set_low,
  test_label = test_labels
)
print_confusion_matrix(confusion_matrix_low)