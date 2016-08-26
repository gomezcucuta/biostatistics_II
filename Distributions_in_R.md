# Probability Distributions in R

## Normal Distribution

## t Distribution

### In order to calculate the area under the curve (distribution function)

```{r}
pt(-1.617, 11)
```

### In order to determine the p value

```{r}
2*pt(-1.617, 11)
```

### In order to calculate the t value (quantile function)

```{r}
qt(0.025, 11)

qt(0.975, 11)
```

### In order to draw the specific curve and highlight the t values

```{r}
curve (dt(x, 11), xlim = c(-5, 5), las = 1)

abline(v = c(-2.201, 2.201), col = "green")

abline(v = 1.617, col = "blue")
```

## F Distribution

### In order to calculate the area under the curve (distribution function)
```{r}
pf(2.283569, 4, 44)
```

### In order to determine the p value

```{r}
pf(2.283569, 4, 44, lower.tail = FALSE)
```
### In order to calculate the F value (quantile function)

```{r}
qf(0.95, 4, 44)
```

### In order to draw the specific curve and highlight the F values
```{r}
curve(df(x, df1 = 4, df2 = 44), xlim = c(0, 5), las = 1)
abline(v = c(2.583667), col = "red")
abline(v = c(2.283569), col = "blue")
```
