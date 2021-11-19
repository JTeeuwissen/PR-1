library(OpenImageR)
library(dplyr)

# Assuming mnist.csv is located in the data folder, it's gitignored so make sure you download it
data <- read.csv("./data/mnist.csv")

print_data_summary <- function() {
    print("Labeled instances per class:")
    data %>% count(label)

    col_means <-colMeans(data)

    print("Mean value per pixel:")
    print(format(col_means, digits=1, scientific = FALSE))

    print("50 least contributing pixels:")
    print(sort(col_means)[0:50])

    print("50 most varying pixels:")
    sort(sapply(data, var), decreasing = TRUE)[0:50]
}

print_data_summary()