# Survival Analysis

## Leukemia

**Data: Freireich et al., Blood, 1963.**

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### Kaplan-Meier Curve

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

### Kaplan-Meier Curves by Treatment

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

plot(km_by_treatment, las = 1, xlab = "Time of remission in weeks", main = "Kaplan-Meier Curves by Treatment", lty = 2:3, col = c("red", "blue"))

legend("topright", legend = c("Placebo", "Treatment"), lty = 2:3, col = c("red", "blue"))

survdiff(Y ~ leukemia$treatment)
```

### Kaplan-Meier Curves by Treatment including 95% Confidence Intervals

```{r, fig.align = 'center'}
plot(km_by_treatment, conf.int = "both", las = 1, xlab = "Time of remission in weeks", main = "Kaplan-Meier Curves by Treatment", lty = 2:3, col = c("red", "blue"))

legend("topright", legend = c("Placebo", "Treatment"), lty = 2:3, col = c("red", "blue"))
```

### Cox PH Models

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

LRT_2v3 <- (-2)*(model2$loglik[2]-model3$loglik[2]); LRT_2v3

P_value <- 1 - pchisq(LRT_2v3, 1); P_value

# Fail to reject the null hypothesis

LRT_1v2 <- (-2)*(model1$loglik[2]-model2$loglik[2]); LRT_1v2

P_value <- 1 - pchisq(LRT_1v2, 1); P_value

# Reject the null hypothesis
```

## Addicts

**Data: addicts dataset**

```{r}
addicts <- read.table("addicts.dat", skip = 19)

str(addicts)

names(addicts) <- c("id", "clinic", "status", "survt", "prison", "dose")

head(addicts); tail(addicts)

# Outcome variable

library(survival)

Y <- Surv(addicts$survt, addicts$status==1); Y
```

### 1. Estimating survival functions (unadjusted) and comparing them across strata

```{r, fig.align = TRUE}
# Fitting the null model
kmfit1 <- survfit(Y ~ 1)

# Descriptive information
kmfit1

# Kaplan-Meier survival estimates for all event times
summary(kmfit1)

# Kaplan-Meier survival estimates for specific survival times
summary(kmfit1, times = 365)

# Stratify by the variable clinic
kmfit2 <- survfit(Y ~ addicts$clinic)

# Kaplan-Meier survival estimates for specific survival times (every 100 days)
summary(kmfit2, times = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000))

# KM Survival plot
plot(kmfit2, lty = c("solid", "dashed"), col = c("red", "blue"), xlab = "Survival time in days", ylab = "Survival probabilities", main = "Kaplan-Meier Curves by Clinic", las = 1)

legend("topright", c("Clinic 1", "Clinic 2"), lty = c("solid", "dashed"), col = c("red", "blue"))

# Log rank test
survdiff(Y ~ addicts$clinic)

# Variations of the log rank test
survdiff(Y ~ addicts$clinic, rho = 1)

# Stratified log rank test for clinic (stratified by prison)
survdiff(Y ~ addicts$clinic + strata(addicts$prison))
```

### 2. Assessing the PH assumption using graphical approaches

```{r, fig.align = TRUE}
# Plotting log-log KM survival estimates against time (or against the log of time)
plot(kmfit2, fun = "cloglog", lty = c("solid", "dashed"), col = c("red", "blue"), xlab = "Survival time in days using logarithmic scale", ylab = "log-log survival", main = "Log-log Curves by Clinic", las = 1)

legend("topleft", c("Clinic 1", "Clinic 2"), lty = c("solid", "dashed"), col = c("red", "blue"))

# Plotting log-log KM survival estimates against time (with time not plotted on the log scale)

kmfit3 <- summary(kmfit2)

names(kmfit3)

kmfit4 <- data.frame(kmfit3$strata, kmfit3$time, kmfit3$surv)

names(kmfit4) <- c("clinic", "time", "survival")

head(kmfit4); tail(kmfit4)

clinic1 <- kmfit4[kmfit4$clinic == "addicts$clinic=1",]

clinic2 <- kmfit4[kmfit4$clinic == "addicts$clinic=2",]

plot(clinic1$time, log(-log(clinic1$survival)), type = "l", lty = "solid", col = "red", xlab = "Survival time in days", ylab = "log-log survival", main = "Log-log Curves by Clinic", las = 1)

par(new = T)

plot(clinic2$time, log(-log(clinic2$survival)), type = "l", lty = "dashed", col = "blue", xlab = "Survival time in days", ylab = "log-log survival", axes = F)

legend("bottomright", c("Clinic 1", "Clinic 2"), lty = c("solid", "dashed"), col = c("red", "blue"))

par(new = F)
```

### 3. Running a Cox PH model

```{r}
# Run a Cox proportional hazards model with default method (Efron)
cox_m1 <- coxph(Y ~ prison + dose + clinic, data = addicts)

summary(cox_m1)

# Run a Cox proportional hazards model with the Breslow method
cox_m2 <- coxph(Y ~ prison + dose + clinic, data = addicts, method = "breslow")

summary(cox_m2)

# Run a Cox proportional hazards model with the Exact method
cox_m3 <- coxph(Y ~ prison + dose + clinic, data = addicts, method = "exact")

summary(cox_m3)

#Run a Cox proportional hazards model without interaction terms (Reduced model)
mod1 <- coxph(Y ~ prison + dose + clinic, data = addicts)

#Run a Cox proportional hazards model with interaction terms (Full model)
mod2 <- coxph(Y ~ prison + dose + clinic + clinic*prison + clinic*dose, data = addicts)

mod2

# Performing the Likelihood Ratio test
names(mod2)

mod2$loglik

LRT <- (-2)*(mod1$loglik[2]-mod2$loglik[2]); LRT

P_value <- 1 - pchisq(LRT, 2); P_value
```

### 4. Running a stratified Cox model

```{r}
# Stratified Cox model with clinic as the stratified variable
coxph(Y ~ prison + dose + strata(clinic), data = addicts)

# Stratified Cox model with interaction terms and clinic as the stratified variable
coxph(Y ~ prison + dose + clinic:prison + clinic:dose + strata(clinic), data = addicts)

# Hazard ratio prison=1 versus prison=0 for clinic=2
addicts$clinic2 <- addicts$clinic-2

summary(coxph(Y ~ prison + dose + clinic2:prison + clinic2:dose + strata(clinic2), data = addicts))
```

### 5. Assessing the PH assumption with a statistical test

```{r, fig.align = TRUE}
# Testing the ranked survival times against the Schoenfeld residuals
cox.zph(mod1, transform = "rank")

# Plotting the Schoenfeld residuals against each individual's failure time
plot(cox.zph(mod1, transform = "rank"), se = F, var = "clinic")
```

### 6. Obtaining Cox-adjusted survival curves

```{r, fig.align = TRUE}
#  Defining the pattern: prison = 0, dose = 70, and clinic = 2
pattern1 <- data.frame(prison = 0, dose = 70, clinic = 2); pattern1

# Obtaining Cox-adjusted survival estimates
summary(survfit(mod1, newdata = pattern1))

# Plotting a survival curve for the pattern
plot(survfit(mod1, newdata = pattern1), conf.int = F, main = "Adjusted Survival for prison = 0, dose = 70, and clinic = 2", cex = 0.01, las = 1)

# Defining a pattern in order to obtaining stratified Cox-adjusted curves controlling for prison and dose
mod3 <- coxph(Y ~ prison + dose + strata(clinic), data = addicts)

pattern2 <- data.frame(prison = 0.46, dose = 60.40)

# Plotting a survival curve for the new pattern
plot(survfit(mod3, newdata = pattern2), conf.int = F, lty = c("solid", "dashed"), col = c("red", "blue"), main = "Survival Curves for clinic, adjusted for prison and dose", cex = 0.01, las = 1)

legend("topright", c("Clinic 1", "Clinic 2"), lty = c("solid", "dashed"), col = c("red", "blue"))

# Plotting log-log KM survival estimates against time (or against the log of time)
plot(survfit(mod3, newdata = pattern2), fun = "cloglog", main = "Log-log Curves for Clinic, adjusted for prison and dose", cex = 0.01, las = 1)

# Plotting log-log KM survival estimates against time (with time not plotted on the log scale)
sum.mod3 <- summary(survfit(mod3, newdata = pattern2))

sum.mod4 <- data.frame(sum.mod3$strata, sum.mod3$time, sum.mod3$surv)

colnames(sum.mod4) <- c("clinic", "time", "survival")

clinic1 <- sum.mod4[sum.mod4$clinic == "clinic=1", ]

clinic2 <- sum.mod4[sum.mod4$clinic == "clinic=2", ]

plot(clinic1$time, log(-log(clinic1$survival)), type = "l", lty = "solid", col = "red", xlab = "Survival time in days", ylab = "log-log survival", main = "Log-log Curves Stratified by Clinic, adjusted for prison and dose", cex = 0.01, las = 1)

par(new = T)

plot(clinic2$time, log(-log(clinic2$survival)), type = "l", lty = "dashed", col = "blue", xlab = "Survival time in days", ylab = "log-log survival", axes = F)

legend("bottomright", c("Clinic 1", "Clinic 2"), lty = c("solid", "dashed"), col = c("red", "blue"))

par(new = F)
```
