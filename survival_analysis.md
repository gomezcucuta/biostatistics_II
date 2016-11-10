# Survival Analysis

*Data: Freireich et al., Blood, 1963.*

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Kaplan-Meier Curve

```{r, fig.align = 'center'}
rm(list=ls())

setwd("~/Dropbox/Education/UNAL/wd_for_biostats_II")

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

## Kaplan-Meier Curves by Treatment

```{r, fig.align = 'center'}
Yplacebo <- Y[leukemia$treatment==0]

km_placebo <- survfit(Yplacebo ~ 1)

km_placebo

summary(km_placebo)

Placebo_Table <- cbind(time = km_placebo$time, risk_set = km_placebo$n.risk, failures = km_placebo$n.event, censored = km_placebo$n.censor, survival = round(km_placebo$surv, 2))

Placebo_Table

Ytreatment <- Y[leukemia$treatment==1]

km_treatment <- survfit(Ytreatment ~ 1)

km_treatment

summary(km_treatment)

Treatment_Table <- cbind(time = km_treatment$time, risk_set = km_treatment$n.risk, failures = km_treatment$n.event, censored = km_treatment$n.censor, survival = round(km_treatment$surv, 2))

Treatment_Table

km_by_treatment <- survfit(Y ~ leukemia$treatment)

km_by_treatment

summary(km_by_treatment)

plot(km_by_treatment, las = 1, xlab = "Time of remission in weeks", main = "Kaplan-Meier Curves by Treatment", lty = 2:3)

legend("topright", legend = c("Placebo", "Treatment"), lty = 2:3)

survdiff(Y ~ leukemia$treatment)
```

## Kaplan-Meier Curves by Treatment including 95% Confidence Intervals

```{r, fig.align = 'center'}
plot(km_by_treatment, conf.int = "both", las = 1, xlab = "Time of remission in weeks", main = "Kaplan-Meier Curves by Treatment", lty = 2:3, col = c("red", "blue"))

legend("topright", legend = c("Placebo", "Treatment"), lty = 2:3, col = c("red", "blue"))
```

## Cox PH Models

```{r, fig.align = 'center'}
leukemia$treatment <- leukemia$treatment + 1

leukemia$treatment[leukemia$treatment == 2] <- 0

table(leukemia$treatment)

model1 <- coxph(Y ~ treatment, data = leukemia, method = "breslow")

model1

summary(model1)

model2 <- coxph(Y ~ treatment + logwbc, data = leukemia, method = "breslow")

model2

summary(model2)

model3 <- coxph(Y ~ treatment + logwbc + treatment*logwbc, data = leukemia, method = "breslow")

model3

summary(model3)

names(model3)

# Ho: Reduced model is adequate

model3$loglik

LRT_2v3 <- (-2)*(model2$loglik-model3$loglik)

LRT_2v3

P_Value <- 1 - pchisq(LRT_2v3, 1)

P_Value

# Fail to reject the null hypothesis

LRT_1v2 <- (-2)*(model1$loglik-model2$loglik)

LRT_1v2

P_Value <- 1 - pchisq(LRT_1v2, 1)

P_Value

# Reject the null hypothesis
```
