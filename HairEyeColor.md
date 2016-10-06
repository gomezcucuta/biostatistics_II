# HairEyeColor Dataset

data(HairEyeColor)

dat <- HairEyeColor

str(dat)

dat <- HairEyeColor[,,1] + HairEyeColor[,,2]

str(dat)

sum(dat)

head(dat)

chisq.test(dat)

qchisq(0.95, 9)

library(FactoClass)

mycolors <- c("orange", "blue", "red", "darkgreen")

plotct(dat, col = mycolors, profiles = "col")
