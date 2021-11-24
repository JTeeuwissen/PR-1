#' Print an image from a data frame segment
#'
#' @param data A data frame segment
print_image <- function(data) {
  matrix <- matrix(as.numeric(data), nrow = 28, ncol = 28)
  image(matrix, col = grey(seq(1, 0, length = 256)), xaxt = "n", yaxt = "n")
}
print_image(data[380, -1])