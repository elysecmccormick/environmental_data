# Lab 9: Using Models 2

rm(list = ls())

# Chi squared for Birds

require(here)

birds   = read.csv(here("data", "bird.sta.csv"))
hab     = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(
  birds,
  hab, by=c("basin", "sub", "sta"))

# Create a contingency table for edge/interior and brown creeper presence/absence
table(
  birdhab$s.edge,
  birdhab$BRCR > 0)

# set the presence to be in the first column
br_creeper_table = table(
  birdhab$s.edge, 
  birdhab$BRCR > 0)[, 2:1]

br_creeper_table

# Run the Chi-square test for 

rownames(br_creeper_table) <- (c("Forest Edge", "Inside Forest"))
colnames(br_creeper_table) <- (c("Presence", "Absence"))
br_creeper_table
chisq_brcr <- chisq.test(br_creeper_table)
chisq_brcr

# Linear Model Assumptions with Penguins

require(palmerpenguins)
fit_species = 
  lm(
    formula = flipper_length_mm ~ species,
    data = penguins)

fit_sex = 
  lm(
    formula = flipper_length_mm ~ sex,
    data = penguins)

Fit_both = 
  lm(
    formula = flipper_length_mm ~ species * sex, 
    data = penguins)

# Homogeneity of Variance

require(palmerpenguins)

boxplot(formula = flipper_length_mm ~ species, data = penguins, main = "Penguin Flipper Lengths by Species",
        xlab = "Species", ylab = "Flipper Length (mm)", col = "royalblue1")

boxplot(formula = flipper_length_mm ~ sex, data = penguins, main = "Penguin Flipper Lengths by Sex", 
        xlab = "Sex", ylab = "Flipper Length (mm)", col = "steelblue2")

boxplot(formula = flipper_length_mm ~ sex * species, data = penguins, 
        main = "Penguin Flipper Lengths by Sex and Species",
        names = c(" Female \n Adelie", "Male \n Adelie", "Female \n Chinstrap", "Male \n Chinstrap",
                  "Female \n Gentoo", "Male \n Gentoo"),
        xlab = "",
        ylab = "Flipper Length (mm)", las = 2, col = "slategray1")

# Bartlett Tests

bartlett.test(flipper_length_mm ~ species, data = penguins)

bartlett.test(flipper_length_mm ~ sex, data = penguins)

# Bartlett Test for two-way interaction

help("bartlett.test")

dat_groups = aggregate(
  formula = flipper_length_mm ~ species * sex,
  data = penguins,
  FUN = c)
str(dat_groups)

dat_groups$flipper_length_mm

bartlett.test(dat_groups$flipper_length_mm)

# Florida Trees Data

rm(list = ls())

# Read in the data

require(here)
FL_trees <- read.csv(here("data", "trees_FL.csv"))

fail <- table(FL_trees$ProbabilityofFailure)
stfail <- table(FL_trees$Failure_Standardized)
par(mfrow = c(2,2))
barplot(fail, main = "FL Trees Probability of Failure", xlab = "Number of Trees",
        col = "seagreen3")
barplot(stfail, main = "FL Trees Standardized Failture", xlab = "Failure Class",
        col = "seagreen1")
hist(FL_trees$DBH_in, main = "Histogram of DBH", xlab = "DBH", col = "seagreen")
plot(FL_trees$DBH, FL_trees$HeighttoTop_ft, main = "DBH by Height (Ft)", xlab = "DBH", ylab = "Height (Ft)",
     col = "springgreen4", pch = 8)

FL_treesn <- droplevels(subset(FL_trees, Failure_Standardized == "none"))
FL_treesb <- droplevels(subset(FL_trees, Failure_Standardized == "branch"))
FL_treesw <- droplevels(subset(FL_trees, Failure_Standardized == "whole"))


# Kolmogorov-Smirnov test

# Example syntax: ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)

ks.test(FL_treesn$DBH_in, FL_treesw$DBH_in)

# Pearson's correlation coefficient

cor.test(FL_trees$DBH, 
         FL_trees$HeighttoTop_ft,
         use='complete.obs')

# Final Chi-squared tests

FL_trees$fail = factor(FL_trees$Failure_Standardized != "none")

levels(FL_trees$fail) = c("No Fail", "Fail")

fl_table_2 = table(
  FL_trees$ProbabilityofFailure,
  FL_trees$fail)
fl_table_2

chisq_fltr <- chisq.test(fl_table_2)
chisq_fltr

round(chisq_fltr$expected, 1)
round(chisq_fltr$observed, 1)

round(
  chisq_fltr$observed - chisq_fltr$expected,
  digits = 1)







