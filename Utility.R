#' Turn a row into a matrix of pixel values
#'
#' @param data A data frame segment
row_to_matrix <- function(data) {
  matrix(as.numeric(data), nrow = 28, ncol = 28, byrow = TRUE)
}

#' Print all information about a confusion matrix
#'
#' @param m the confusion matrix
print_confusion_matrix <- function(m)
{
  # Print the confusion matrix
  print(m)
  
  # Compute and print the accuracy
  print(paste0("accuracy: ", sum(diag(m)) / sum(m)))
}

#' Print an image from a data frame segment
#'
#' @param data A data frame segment
print_image <- function(data) {
  matrix <- t(
    apply(
      row_to_matrix(data),
      2,
      rev
    )
  )
  image(matrix, col = grey(seq(1, 0, length = 256)), axes = FALSE)
}

print_image(data[380, -1])