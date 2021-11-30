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
    Class = test_label,
    Obs = factor(predicted_labels, levels = 0:9)
  )
}

# ink and rowchange (train and test set are the same)
confusion_matrix <- multinom(
  train_features = cbind(scale(ink), scale(row_change)),
  train_labels = data$label,
  test_features = cbind(scale(ink), scale(row_change)),
  test_label = data$label
)

# using all cells
confusion_matrix <- multinom(
  train_features = as.matrix(train_set[-1]),
  train_labels = train_set$label,
  test_features = as.matrix(test_set[-1]),
  test_label = test_set$label
)

# Print the confusion matrix
print(confusion_matrix)

# Compute accuracy on test data
print(paste0("accuracy: ", sum(diag(confusion_matrix)) / sum(confusion_matrix)))