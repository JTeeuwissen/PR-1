library(OpenImageR)
library(dplyr)

# Assuming mnist.csv is located in the data folder,
# it's gitignored so make sure you download it
csv <- read.csv("mnist.csv")

# Only take data, not labels.
data <- csv[, -1]

# Convert the label to a categorical variable.
labels <- as.factor(csv$label)

# Create low resolution digits.
data_low <- t(apply(data, 1, lower_resolution))


# Set seed
set.seed(123)

# Draw a random sample of size 5.000, and use this as the training set.
train_set_indices <- sample(nrow(data), 5000)

# Split the data into training and test set.
train_set <- data[train_set_indices, ]
train_labels <- labels[train_set_indices]
train_set_low <- data_low[train_set_indices, ]

# Use the remaining 37.000 examples as the test set.
test_set <- data[-train_set_indices, ]
test_labels <- labels[-train_set_indices]
test_set_low <- data_low[-train_set_indices, ]

# Compute the total ink cost per sample.
ink <- rowSums(data)

# Compute the mean value of ink for each digit.
digits_ink_mean <- tapply(ink, labels, mean)

# Compute the standard deviation of ink cost for each digit.
digits_ink_sd <- tapply(ink, labels, sd)

#' Print a summary of the data
print_data_summary <- function() {
    print("Class distribution:")
    print(summary(labels))

    col_means <- colMeans(data)

    print("Mean value per pixel:")
    print(format(col_means, digits = 1, scientific = FALSE))

    print("50 least contributing pixels:")
    print(sort(col_means)[0:50])

    print("50 most varying pixels:")
    print(sort(sapply(data, var), decreasing = TRUE)[0:50])
}

print_data_summary()