# Survival Analysis

*Data: Freireich et al., Blood, 1963.* 

```{r}
leukemia <- read.table("leukemia.dat", header = TRUE, skip = 6)

str(leukemia)

head(leukemia); tail(leukemia)

library(survival)

Y <- Surv(leukemia$time, leukemia$status == 1)

null_model <- survfit(Y ~ 1)

null_model

summary(null_model)

names(null_model)

Expanded_Table <- cbind(time = null_model$time, risk_set = null_model$n.risk, failures = null_model$n.event, censored = null_model$n.censor, survival = round(null_model$surv, 2))

Expanded_Table

plot(null_model, las = 1, xlab = "Time of remission in weeks", main = "Kaplan-Meier Curve")
```
