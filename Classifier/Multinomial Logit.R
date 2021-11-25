library(nnet)
library(glmnet)

multinom <- function(features, labels) {

  # Multinominal Logit with lasso penalty
  train_glmnet <- cv.glmnet(
    features,
    labels,
    family = "multinomial",
    type.measure = "class"
  )

  # Hyperparameter tuning
  # Choose the lambda value that gives the smallest cross-validation error
  lambda <- train_glmnet$lambda.min

  # Make predictions on the test set.
  predicted_labels <- predict(
    train_glmnet,
    features,
    s = lambda,
    type = "class"
  )

  # Create the confusion matrix
  confusion_matrix <- table(
    Class = labels,
    Pred = factor(predicted_labels, levels = 0:9)
  )
}

confusion_matrix <- multinom(
  features = cbind(scale(ink), scale(edge)),
  labels = data$label
)

# Print the confusion matrix
print(confusion_matrix)

# Compute accuracy on test data
print(paste0("accuracy: ", sum(diag(confusion_matrix)) / sum(confusion_matrix)))