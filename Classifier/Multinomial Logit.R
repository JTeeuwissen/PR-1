library(nnet)
library(glmnet)

multinom <- function(features, labels) {
  
  # Multinominal Logit with lasso penalty
  train_glmnet <- cv.glmnet(
    cbind(scale(ink), scale(ink)),
    data$label,
    family = "multinomial",
    type.measure = "class"
  )
  
  # Hyperparameter tuning
  # Choose the lambda value that gives the smallest cross-validation error
  lambda <- train_glmnet$lambda.min
  
  # Make predictions on the test set.
  predicted_labels <- predict(
    train_glmnet,
    cbind(scale(ink), scale(ink)),
    s = lambda,
    type = "class"
  )
  
  # Create the confusion matrix
  confusion_matrix <- table(data$label, predicted_labels)
}

confusion_matrix <- multinom(
  features = cbind(scale(ink), scale(ink)), 
  labels = data$label
)

# Print the confusion matrix
print(confusion_matrix)

# Compute accuracy on test data
print(sum(diag(confusion_matrix)) / sum(confusion_matrix))