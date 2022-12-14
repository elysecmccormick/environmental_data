# Final R Data Analysis

# Clear Environment

rm(list = ls())

# Load packages
library(here)

# Load data
delomys_dat = read.csv(here("data", "delomys.csv"))

#Summary of the data, summary by column, and Shapiro tests for normality
summary(delomys_dat)
summary(delomys_dat$body_mass)
summary(delomys_dat$body_length)
shapiro.test(delomys_dat$body_mass)
shapiro.test(delomys_dat$body_length)

#Data not normally distributed (both p < 0.05)

#Visualize with scatterplot
plot(x = delomys_dat$body_length, y = delomys_dat$body_mass, 
     main = "Delomys Body Length by Mass", xlab = "Body Length (cm)",
     ylab = "Body Mass (g)", pch = 16, col = "lightsalmon3")

#Visualize with histograms
hist(x = delomys_dat$body_mass, main = "Delomys Body Mass", 
     xlab = "Body Mass (g)", col = "lightsalmon")
hist(x = delomys_dat$body_length, main = "Delomys Body Length", 
     xlab = "Body Mass (g)", col = "lightsalmon2")

#Visualize with boxplots
boxplot(body_mass ~ binomial, data = delomys_dat, xlab = "Species", ylab = "Body Mass (g)", col = "lightsalmon4")
boxplot(body_mass ~ sex, data = delomys_dat, xlab = "Sex", ylab = "Body Mass (g)", col = "lightsalmon3")
boxplot(body_mass ~ binomial*sex, data = delomys_dat, xlab = "Species", ylab = "Body Mass (g)", col = "lightsalmon4")


# Create linear models 
fit1 <- lm(delomys_dat$body_length ~ delomys_dat$body_mass)
fit2 <- lm(delomys_dat$body_mass ~ delomys_dat$sex)
fit3 <- lm(delomys_dat$body_mass ~ delomys_dat$binomial)
fit4 <- lm(delomys_dat$body_mass ~ delomys_dat$sex + delomys_dat$binomial)
fit5 <- lm(delomys_dat$body_mass ~ delomys_dat$sex * delomys_dat$binomial)

# Residuals
fit1res <- residuals(fit1)
fit2res <- residuals(fit2)
fit3res <- residuals(fit3)
fit4res <- residuals(fit4)
fit5res <- residuals(fit5)

#Histogram of Residuals
par(mfrow = c(3,2))
hist(fit1res)
hist(fit2res)
hist(fit3res)
hist(fit4res)
hist(fit5res)

#Shapiro-Wilk Test for Normality on Residuals
shapiro.test(fit1res)
shapiro.test(fit2res)
shapiro.test(fit3res)
shapiro.test(fit4res)
shapiro.test(fit5res)
#None of these are normally distributed

#Linear Regression summaries
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)

# ANOVA tables

anova(fit1)
anova(fit2)
anova(fit3)
anova(fit4)
anova(fit5)

knitr::kable(anova(fit1))
knitr::kable(anova(fit2))
knitr::kable(anova(fit3))
knitr::kable(anova(fit4))
knitr::kable(anova(fit5))

#AIC

AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)
AIC(fit5)


