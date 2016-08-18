# Probability Distributions in R

## Normal Distribution

## t Distribution

## In order to carculate the area under the curve (distribution function)

pt(-1.617, 11)

## In order to determine the p value

2*pt(-1.617, 11)

## In order to calculate the t value (quantile function)

qt(0.025, 11)

qt(0.975, 11)

### In order to draw the specific curve and highlight the t values

curve (dt(x, 11), xlim = c(-5, 5), las = 1)

abline(v = c(-2.201, 2.201), col = "green")

abline(v = 1.617, col = "blue")
