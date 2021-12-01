#' lower the resolution of the row
#'
#' @param data the row to lower the resolution of
#' @param division the target resoltion
lower_resolution <- function(data, division = 14) {
  matrix <- row_to_matrix(data)
  size <- nrow(matrix) / division

  output <- matrix(, nrow = division, ncol = division)
  for (x in 1:division) {
    for (y in 1:division) {
      output[x, y] <- mean(matrix[
        c(size * (x - 1), size * (x)),
        c(size * (y - 1), size * (y))
      ])
    }
  }

  matrix_to_row(output)
}

#' Turn a row into a matrix of pixel values
#'
#' @param data A data frame segment
row_to_matrix <- function(data) {
  size <- sqrt(length(data))
  matrix(as.numeric(data), nrow = size, ncol = size, byrow = TRUE)
}

#' Turn a row into a matrix of pixel values
#'
#' @param data A data frame segment
matrix_to_row <- function(row) {
  as.vector(t(row))
}


#' Print all information about a confusion matrix
#'
#' @param m the confusion matrix
print_confusion_matrix <- function(m) {
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