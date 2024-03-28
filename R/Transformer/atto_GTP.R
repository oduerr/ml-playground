# Load the required libraries
library(keras)
library(tensorflow)

# Define the GPT model
N_Token <- 128
B <- 1
T <- 5
C <- 4

# Data set
data <- matrix(c(0, 3, 6, 9, 12, 15), nrow = 1)

# Creating the input and output pairs
T_input <- tf$Variable(data[, T])  # 0,3,6,9,12 shape (B, T)
T_input
