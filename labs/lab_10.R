# Lab 10 - ANOVA

rm(list = ls())

rope = read.csv(here("data", "rope.csv"))

rope$rope.type = factor(rope$rope.type)

n_obs = length(rope$rope.type)
n_groups = length(unique(rope$rope.type))

ss_tot = sum( (rope$p.cut - mean(rope$p.cut) )^2 )
df_tot = n_obs - 1

agg_sum_sq_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) sum( (x - mean(x) )^2 ))

ss_within = sum(agg_sum_sq_resids$x)

df_within = n_obs - 6

ss_among = ss_tot - ss_within

df_among = n_groups - 1
ms_among = ss_among / (n_groups - 1)
ms_within = ss_within / (n_obs - n_groups)

f_ratio = ms_among / ms_within
f_pval = 1 - pf(f_ratio, df1 = df_among, df2 = df_within)

#Self-test

# number comparison tolerance
digits_check = 5

# Build the reference model using R functions
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)

# Check degrees of freedom
anova_fit_1$Df == c(df_among, df_within)

# Check sums of squares
round(anova_fit_1$`Sum Sq`, digits = digits_check) == round(c(ss_among, ss_within), digits = digits_check)

# Check mean squares
round(anova_fit_1$`Mean Sq`, digits = digits_check) == round(c(ms_among, ms_within), digits = digits_check)

# Check the F-ratio
round(anova_fit_1$`F value`[1], digits = digits_check) == round(f_ratio, digits = digits_check)

# Check the F test statistic p-value
round(anova_fit_1$`Pr(>F)`[1], digits = digits_check) == round(f_pval, digits = digits_check)

#Bartlett test

bartlett.test(p.cut ~ rope.type, data = rope)

fit_rope_1 = lm(p.cut ~ rope.type, data = rope)
summary(fit_rope_1)
residuals(fit_rope_1)

#Residual normality testing

#first, overall

residuals(fit_rope_1)
shapiro.test(residuals(fit_rope_1))

#Within groups

agg_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) residuals(x))





#PENGUINS

require(palmerpenguins)
pen_fem = subset(penguins, sex == "female")

boxplot(pen_fem$body_mass_g ~ pen_fem$species, xlab = "Species", ylab = "Body Mass (g)", 
        col = "skyblue")

bartlett.test(body_mass_g ~ species , data = pen_fem)

#Linear Model for body mass and species from penguin dat

lmodel = lm(body_mass_g ~ species, data = pen_fem)
summary(lmodel)
shapiro.test(lmodel$residuals)

# Tukey post-hoc test

lmodel <- lm(body_mass_g ~ species, data = pen_fem)
penms_hsd <- TukeyHSD(aov(lmodel))
class(penms_hsd)
round(penms_hsd$body_mass_g, digits = 4)







