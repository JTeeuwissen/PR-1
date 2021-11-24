library(OpenImageR)
library(dplyr)

# Assuming mnist.csv is located in the data folder, it's gitignored so make sure you download it
data <- read.csv("./data/mnist.csv")
# Convert the label to a categorical variable.
data$label <- as.factor(data$label)
# Compute the total ink cost per sample.
ink <- rowSums(data[-1])
# Compute the mean value of ink for each digit.
digits_ink_mean <- tapply(ink, data$label, mean)
# Compute the standard deviation of ink cost for each digit.
digits_ink_sd <- tapply(ink, data$label, sd)

print_data_summary <- function() {
    print("Class distribution:")
    print(summary(data$label))

    col_means <-colMeans(data[, -1])

    print("Mean value per pixel:")
    print(format(col_means, digits=1, scientific = FALSE))

    print("50 least contributing pixels:")
    print(sort(col_means)[0:50])

    print("50 most varying pixels:")
    print(sort(sapply(data[, -1], var), decreasing = TRUE)[0:50])
}

print_data_summary()

ink <- function(pixels) {
    return(sum(pixels))
}

print_ink_summary <- function(){
    apply(data[,-1], 1, sum)
}

print_ink_summary()