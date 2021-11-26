#' Sum the difference from the previous row from the previous row.
#'
#' @param data A matrix of pixel values.
row_change_function <- function(data) {
  matrix <- row_to_matrix(data)

  total <- 0

  prev_row <- mean(matrix[1, ])
  for (row in 2:nrow(matrix)) {
    average <- mean(matrix[row, ])
    difference <- abs(prev_row - average)
    prev_row <- average
    total <- total + difference
  }

  total
}

# Compute the total edge cost per sample.
row_change <- apply(data[, -1], 1, row_change_function)

# Compute the mean value of edge for each digit.
row_change_mean <- tapply(row_change, data$label, mean)

# Compute the standard deviation of edge cost for each digit.
row_change_sd <- tapply(row_change, data$label, sd)