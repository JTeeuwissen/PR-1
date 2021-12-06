#' Write the accuracies of the 3 models to a file
#'
#' @param m1 the confusion matrix of multinom
#' @param m2 the confusion matrix of SVM
#' @param m3 the confusion matrix of Neural Network
#' @param file the file to write the data to.
write_accuracies_csv <- function(m1, m2, m3, file) {
  write.table(
    data.frame(accuracy(m1), accuracy(m2), accuracy(m3)),
    file = file,
    append = T,
    sep = ",",
    row.names = F,
    col.names = F
  )
}

for (i in 0:100) {
  
  # Draw 5000 random samples as the training set
  train_set_indices <- sample(nrow(data), 5000)
  train_labels <- labels[train_set_indices]
  train_set_low <- data_low[train_set_indices, ]
  
  # Use the remaining 37.000 examples as the test set.
  test_labels <- labels[-train_set_indices]
  test_set_low <- data_low[-train_set_indices, ]
  
  cm_multinom <- multinom(
    train_features = train_set_low,
    train_labels = train_labels,
    test_features = test_set_low,
    test_label = test_labels
  )
  
  cm_svm <- SVM(
    train_features = train_set_low,
    train_labels = train_labels,
    test_features = test_set_low,
    test_label = test_labels
  )
  
  cm_nn <- neural_network(
    train_features = train_set_low,
    train_labels = train_labels,
    test_features = test_set_low,
    test_label = test_labels
  )
  
  # Compute the accuracies and write it to the "Output" file
  write_matrix_csv(cm_multinom, cm_svm, cm_nn, "Output.csv")
}