library(nnet)
library(glmnet)

#' Multinomial Logit Classifier
#'
#' @param train_features A matrix of features to train the model
#' @param train_labels The corresponding vector of the train_features labels
#' @param test_features A matrix of features to make predictions on the model
#' @param test_label The corresponding vector of the test_features labels
#'
#' @return A confusion matrix
multinom <- function(train_features, train_labels, test_features, test_label) {

  # Multinominal Logit with lasso penalty
  train_glmnet <- cv.glmnet(
    train_features,
    train_labels,
    family = "multinomial",
    type.measure = "class"
  )

  # Hyperparameter tuning
  # Choose the lambda value that gives the smallest cross-validation error
  lambda <- train_glmnet$lambda.min

  # Make predictions on the test set.
  predicted_labels <- predict(
    train_glmnet,
    test_features,
    s = lambda,
    type = "class"
  )

  # Create the confusion matrix
  confusion_matrix <- table(
    Obs = test_label,
    Pred = factor(predicted_labels, levels = 0:9)
  )
}

# ink (train and test set are the same)
confusion_matrix_ink <- multinom(
  train_features = cbind(scale(ink), scale(ink)),
  train_labels = labels,
  test_features = cbind(scale(ink), scale(ink)),
  test_label = labels
)
print_confusion_matrix(confusion_matrix_ink)

# ink and rowchange (train and test set are the same)
confusion_matrix_ink_rowchange <- multinom(
  train_features = cbind(scale(ink), scale(row_change)),
  train_labels = labels,
  test_features = cbind(scale(ink), scale(row_change)),
  test_label = labels
)
print_confusion_matrix(confusion_matrix_ink_rowchange)

# using all cells
confusion_matrix <- multinom(
  train_features = as.matrix(train_set),
  train_labels = train_labels,
  test_features = as.matrix(test_set),
  test_label = test_labels
)
print_confusion_matrix(confusion_matrix)

# using all cells low resolution
confusion_matrix_low <- multinom(
  train_features = as.matrix(train_set_low),
  train_labels = train_labels,
  test_features = as.matrix(test_set_low),
  test_label = test_labels
)
print_confusion_matrix(confusion_matrix_low)