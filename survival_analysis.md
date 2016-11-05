# Survival Analysis

*Data: Freireich et al., Blood, 1963.* 

```{r}
leukemia <- read.table("leukemia.dat", header = TRUE, skip = 6)

str(leukemia)

head(leukemia); tail(leukemia)

library(survival)

Y <- Surv(leukemia$time, leukemia$status == 1); Y

km_null_model <- survfit(Y ~ 1)

km_null_model

summary(km_null_model)

names(km_null_model)

Expanded_Table <- cbind(time = km_null_model$time, risk_set = km_null_model$n.risk, failures = km_null_model$n.event, censored = km_null_model$n.censor, survival = round(km_null_model$surv, 2))

Expanded_Table

plot(km_null_model, las = 1, xlab = "Time of remission in weeks", main = "Kaplan-Meier Curve")
```
