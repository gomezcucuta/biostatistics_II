# Logistic Regression

## Exercise Regression Analysis by Example

```{r}
load("CHP.RData")

dat <- P323

names(dat) <- c("Solvent", "Ret_Earnings", "Earnings_B", "Sales")

head(dat)

attach(dat)

log_model1 <- glm(Solvent ~ Ret_Earnings + Earnings_B + Sales, binomial)

summary(log_model1)

anova(log_model1, test = "Chisq")

confint.default(log_model1)
```

## Exercise Introductory Statistics with R (Chapter 13)

```{r}
# Enter the data

no.yes <- c("No", "Yes")

smoking <- gl(2, 1, 8, no.yes)

obesity <- gl(2, 2, 8, no.yes)

snoring <- gl(2, 4, 8, no.yes)

n.tot <- c(60, 17, 8, 2, 187, 85, 51, 23)

n.hyp <- c(5, 2, 1, 0, 35, 13, 15, 8)

data.frame(smoking, obesity, snoring, n.tot, n.hyp)

# Another way of generating the same regular pattern

expand.grid(smoking = no.yes, obesity = no.yes, snoring = no.yes)

hyp.tbl <- cbind(n.hyp, n.tot - n.hyp)

hyp.tbl

# Specify the logistic regression model

glm(hyp.tbl ~ smoking + obesity + snoring, family = binomial("logit"))

# "logit" is the default for binomial

glm(hyp.tbl ~ smoking + obesity + snoring, family = binomial)

# Another way to specify a logistic model

prop.hyp <- n.hyp/n.tot

glm.hyp <- glm(prop.hyp ~ smoking + obesity + snoring, family = binomial, weights = n.tot)

# Save the result of a fit of a generalized linear model in a variable

glm.hyp <- glm( hyp.tbl ~ smoking + obesity + snoring, family = binomial)

# Obtain a table of regression coefficients

summary(glm.hyp)

# Add a table of correlations between parameter estimates

summary(glm.hyp, corr = TRUE)

# Deviance tables

anova(glm.hyp, test = "Chisq")

# Rearrange the terms for getting a deviance-based test for removal of a variable

glm.hyp <- glm( hyp.tbl ~ obesity + snoring + smoking, family = binomial)

summary(glm.hyp)

anova(glm.hyp, test = "Chisq")

# Simplify the model by taking into account the Z test as well as the deviance-based test

glm.hyp <- glm( hyp.tbl ~ obesity + snoring, family = binomial)

summary(glm.hyp)

anova(glm.hyp, test = "Chisq")

# An alternative method

drop1(glm.hyp, test = "Chisq")

# Wald-based Confidence Intervals

confint.default(glm.hyp)

# Confidence intervals based on inverting the likelihood ratio test

confint(glm.hyp)

# Likelihood profiling

library(MASS)

plot(profile(glm.hyp))

# Odds-ratio estimates

exp(cbind(OR = coef(glm.hyp), confint(glm.hyp)))

# Prediction

predict(glm.hyp)

# To get predicted values on the response scale (i.e., probabilities)

predict(glm.hyp, type = "response")

# Model checking

fitted(glm.hyp)

prop.hyp

# Observed and expected counts instead

fitted(glm.hyp)*n.tot

data.frame(fit = round(fitted(glm.hyp)*n.tot), n.hyp, n.tot)
```
