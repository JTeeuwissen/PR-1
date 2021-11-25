

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