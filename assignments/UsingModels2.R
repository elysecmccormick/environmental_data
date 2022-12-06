# Using Models 2

#Prep the workspace

rm(list = ls())

require(here)
require(palmerpenguins)

# t-test reviews

# First: are the mean flipper lengths of Gentoo penguins different
# than zero?

t.test(subset(penguins, species == "Gentoo")$flipper_length_mm)

# Yes, p < 2.2e-16

#Now, test whether Gentoo penguin flipper lengths are equal to 218 mm?

t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218
)

# Not equal to 218 mm, p = 0.1669. Fail to reject null. 

# Third: Are Gentoo penguin flippers smaller than 218 mm?

t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218,
  alternative = "less"
)

# No, p = 0.08347. Fail to reject the null that Gentoo penguin flippers
# are not smaller than 218 mm. 95% CI = 218.1561

################################################################################

# Two-Sample t-tests

# Are the flipper lengths of Adelie and Gentoo penguins different than zero? 

# code showing - species != "Chinstrap" - subsets the data to remove Chinstrap penguins

t.test(flipper_length_mm ~ species, data = subset(penguins, species != "Chinstrap"))

# Yes, p < 2.2e-16, alternative hypothesis that the true difference in means between group Adelie 
# and group Gentoo is not equal to 0 supported, reject the null hypothesis.

# To modify this to test whether Adelie penguins have shorter flippers
# than Gentoo, you could add the *alternative = less* portion of the code

################################################################################

# 1-Way ANOVA

# Graphical data exploration 

par(mfrow = c(1, 2))
hist(penguins$body_mass_g, breaks = 80, main = "histogram of body mass", xlab = "body mass (g)")
plot(density(penguins$body_mass_g, na.rm = TRUE), main = "density plot of body mass")

# Add some conditional boxplots

require(palmerpenguins)
boxplot(body_mass_g ~ species, data = penguins)

#Conditional boxplot for both sex and species

require(palmerpenguins)
boxplot(formula = body_mass_g ~ sex * species, data = penguins, 
        names = c(" Female \n Adelie", "Male \n Adelie", "Female \n Chinstrap", "Male \n Chinstrap",
                  "Female \n Gentoo", "Male \n Gentoo"),
        xlab = "",
        ylab = "Body Mass (g)", las = 2, col = "slategray2")


# Testing for normality

dat_chinstrap = subset(penguins, species == "Chinstrap")
mean(dat_chinstrap$body_mass_g, na.rm = TRUE)
shapiro.test(dat_chinstrap$body_mass_g)

# Data are normally distributed, p = 0.56, fail to reject null that
# the data are drawn from a normally distributed popoulation. 

# From Mike: "Here’s a cool shortcut for calculating the species mean body 
# masses using aggregate() and the formula notation:

aggregate(body_mass_g ~ species, data = penguins, FUN = mean)

# Now use this to test each for normality of each species

aggregate(
  body_mass_g ~ species,
  data = penguins,
  FUN = function(x) shapiro.test(x)$p.value)

# Adelie aren't really normal! p = 0.032

###############################################################

# Fitting a linear model

fit_species = lm(body_mass_g ~ species, data = penguins)
summary(fit_species)

##################################################################

# Conduct the ANOVA

anova(fit_species)

# There is a significant effect of species on body mass (p < 2.2e-16)

#################################################################

# Two-Way Additive ANOVA

# Fit a two-way model. First one doesn't include interaction terms, only that 
# we want to see if there is an effect of sex AND species.

fit_additive = lm(body_mass_g ~ sex + species, data = penguins)

anova(fit_additive)

# Significant effect of species and sex! Both have p <2.2e-16

###################################################################

# Two Way (interactive) Factorial ANOVA

# This version looks for an interaction between the two categorical
# variables.

fit_both = lm(body_mass_g ~ sex * species, data = penguins)

summary(fit_both)

anova(fit_both)

#####################################################################

# Simple Linear Regression

require(palmerpenguins)

pen_reg <- lm(bill_length_mm ~ body_mass_g, data = penguins)

summary(pen_reg)

#####################################################################


