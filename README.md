# Biostatistics_II

## First Homework: Compare Stata and R Outputs for Linear Regression.

### In order to read the file in R and assign its contents to an object

dat <- read.csv("regrmult_v2.0.csv", row.names = 4)

### In order to change the column names

colnames(dat) <- c("peso", "talla", "edad")
