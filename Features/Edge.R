#' Sum the difference from the surrounding 9 pixels for each pixel.
#'
#' @param data A matrix of pixel values.
edge_function <- function(data) {
  matrix <- row_to_matrix(data)
  total <- 0
  for (row in 2:(nrow(matrix) - 1)) {
    for (col in 2:(ncol(matrix) - 1)) {
      kernel <- sum(
        matrix[row - 1, col - 1],
        matrix[row - 1, col],
        matrix[row - 1, col + 1],
        matrix[row, col - 1],
        matrix[row, col],
        matrix[row, col + 1],
        matrix[row + 1, col - 1],
        matrix[row + 1, col],
        matrix[row + 1, col + 1]
      ) / 9
      difference <- abs(matrix[row, col] - kernel)
      total <- total + difference
    }
  }

  total
}

# Compute the total edge cost per sample.
edge <- apply(data, 1, edge_function)

# Compute the mean value of edge for each digit.
edge_mean <- tapply(edge, labels, mean)

# Compute the standard deviation of edge cost for each digit.
edge_sd <- tapply(edge, labels, sd)