# First Homework

## Compare Stata and R Outputs for Linear Regression.

### In order to read the file in R and assign its contents to an object

dat <- read.csv("regrmult_v2.0.csv", row.names = 4)

### In order to change the column names

colnames(dat) <- c("peso", "talla", "edad")

### In order to create a scatter plot

plot(peso ~ talla, dat, xlab = "Talla", ylab = "Peso")

### In order to perform the regression analysis

fit <- lm(peso ~ talla, dat)

fit

summary(fit)

anova(fit)

confint(fit, 'talla')

new_dat <- data.frame(talla = 0)

predict(fit, new_dat, interval = "confidence")

## In order to check the residuals

names(fit)

fit$residuals

summary(fit$residuals)

qqnorm(fit$residuals)

plot(fit)
