# Using Models Assignment

# Clear R Environment
rm(list = ls())

# Read in Data
require(here)
catrate <- read.csv(here("data", "catrate.csv"))
head(catrate)
summary(catrate)

# Histogram of Catastrophe Rate Data
hist(catrate$cat.rate, xlab = "Catastrophe Rate", main = "Histogram of Catastrophe Rates",
     col = "orangered3")

# Test for Normality - Shapiro-Wilk Test

shapiro.test(catrate$cat.rate)

# p = 0.04097
# This indicates the data are not normally distributed.
# Since p < 0.05, we can reject the null hypothesis (which is that the data are normally distributed).

# Perform a one-sample t.test
# Example syntax: t.test(flipper_length_mm ~ species, data = dat_pen, alternative = "less")

help("t.test")

t.test(catrate$cat.rate, y = NULL, mu = 2/7)

# p = 0.01193
# Reject the null (that true mean is equal to 0.2875143), and fail to reject alt hypothesis. 

#Perform one-sided t.test

t.test(catrate$cat.rate, y = NULL, mu = 0.2875143, alternative = "greater")

# p = 0.006204
# Reject the null. The observed catastrophe rate comes from a population in which 
# catastrophes are solely the result of late pond-filling in the fall

t.test(catrate$cat.rate, y = NULL, mu = 0.2875143, alternative = "less")

# p = 0.9938
# Fail to reject null. 

#Wilcox Test

wilcox.test(catrate$cat.rate, mu = 2 / 7)

#Gets roughly the same result as the first one sided t-test

# Comparing Penguins

require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))       
summary(penguin_dat)       

# Boxplot

boxplot(
  flipper_length_mm ~ species, 
  data = penguin_dat,
  ylab = "Flipper Length (mm)")

#Subsets and Shapiro-Wilk

dat_adelie = subset(penguin_dat, species == "Adelie")
shapiro.test(dat_adelie$flipper_length_mm)

# p = 0.72
# Data are normal

# Two-sample t.test

t.test(flipper_length_mm ~ species, data = penguin_dat)

# p = 6.049e-08 
# Significant difference in flipper length between Adelie and Chinstrap penguins. 

levels(penguin_dat$species)

par(mfrow = c(1,2))   
hist(dat_adelie$flipper_length_mm, main = "Adelie Flipper Length", xlab = "Flipper Length (mm)", col = "royalblue4")
hist(dat_chinstrap$flipper_length_mm, main = "Chinstrap Flipper Length", xlab = "Flipper Length (mm)", col = "royalblue1")
       
       
       
