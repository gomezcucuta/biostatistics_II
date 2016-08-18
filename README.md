# First Homework

## Compare Stata and R Outputs for Linear Regression.

### In order to read the file in R and assign its contents to an object

dat <- read.csv("regrmult_v2.0.csv", row.names = 4)

### In order to change the column names

colnames(dat) <- c("peso", "talla", "edad")

### In order to create a scatter plot

plot(peso ~ talla, dat, xlab = "Talla", ylab = "Peso")

### In order to perform the regression analysis

lm(peso ~ talla, dat)

summary(lm(peso ~ talla, dat))

anova(lm(peso ~ talla, dat))

confint(lm(peso ~ talla, dat), 'talla')

new_dat <- data.frame(talla = 0)

predict(lm(peso ~ talla, dat), new_dat, interval = "confidence")

**OR**

fit <- lm(peso ~ talla, dat)

summary(fit)

anova(fit)

confint(fit, 'talla')

new_dat <- data.frame(talla = 0)

predict(fit, new_dat, interval = "confidence")
