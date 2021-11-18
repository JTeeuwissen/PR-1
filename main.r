library(OpenImageR)

# Assuming mnist.csv is located in the data folder, it's gitignored so make sure you download it
mnist.dat <- read.csv("./data/mnist.csv")
imageShow(matrix(as.numeric(mnist.dat[380,-1]),nrow=28,ncol=28,byrow=T))