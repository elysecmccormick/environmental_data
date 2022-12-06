# Lab 08

rm(list = ls())

require(here)
require(palmerpenguins)
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
boxplot(dat_pen$flipper_length_mm ~ dat_pen$species, main = "Penguin Flipper Length by Species",
        xlab = "Species", ylab = "Flipper Length (mm)", col = "skyblue")

# Perform a t-test

t.test(flipper_length_mm ~ species, data = dat_pen, alternative = "less")

# Working through using two.boot()

#Install the package simpleboot

install.packages("simpleboot")
require(simpleboot)

# Look at the two.boot() help entry

help("two.boot")

# Two boot function

set.seed(10000)
sampleA <- droplevels(subset(penguins, species == "Adelie"))
adelieflip <- sampleA$flipper_length_mm
sampleC <- droplevels(subset(penguins, species == "Chinstrap"))
chinflip <- sampleC$flipper_length_mm

pen.boot <- 
  two.boot(na.omit(adelieflip), na.omit(chinflip), mean, R = 10000, student = FALSE, weights = NULL)

str(pen.boot)

# Retrieving named object from list

pen.boot$t

sd(pen.boot$t)

hist(pen.boot$t, xlab = "bootstrap t values", 
     main = "Distribution of Boostrapped Values of Flipper Length", 
     col = "skyblue")

# 95% CIs

quantile(pen.boot$t,c(0.025, 0.975))

mean(pen.boot$t)
median(pen.boot$t)

# Tree Data

treedat <- read.csv(here("data", "vegdata.csv"))
boxplot(pine ~ treatment, dat = treedat)

# Since we’re focusing on two-sample tests, let’s create a new data frame that contains only the observations 
# that received the “clipped” or “control” treatments.You can use the subset() function in conjunction with 
# a new operator: %in%:

dat_tree = droplevels(subset(treedat, treatment %in% c("control", "clipped")))
dat_tree[dat_tree == 0] <- NA
boxplot(pine ~ treatment, dat = dat_tree, col = "darkgreen")

# Now, since this data is fairly obviously not normal, we will do a nonparametric 
# Wilcox test

wilcox.test(pine ~ treatment, dat = dat_tree, alternative = "less")

# Confidence Intervals using two.boot(). Had to make two x factors out of the dat_tree data so that they can be
# compared

require(boot)
require(simpleboot)
pine <- subset(dat_tree, select = -c(birch, fern))

pinecontrol <- droplevels(subset(pine, treatment =="control"))
pineclipped <-droplevels(subset(pine, treatment =="clipped"))
pinecon <- na.omit(pinecontrol)
pineclip <- na.omit(pineclipped)

tree_boot <- 
  two.boot(pinecon$pine, pineclip$pine, mean, R = 10000, student = FALSE, weights = NULL)

boot.ci(tree_boot)
quantile(tree_boot$t, na.rm = TRUE, c(0.025, 0.975))

mean(pinecon$pine) - mean(pineclip$pine)

# ecdf function (Questions 5-7)

#Create the function for ecdf
pen_ecdf <- ecdf(pen.boot$t)

#Now see if it works using x = -4.5
pen_ecdf(-4.5)

1-pen_ecdf(-4.5)

# Bird Data

rm(list = ls())

require(here)
dat_bird = read.csv(here("data", "bird.sub.csv"))
head(dat_bird)
dat_hab = read.csv(here("data", "hab.sub.csv"))
head(dat_hab)

dat_all = merge(
  dat_bird, 
  dat_hab,
  by = c("basin", "sub"))

head(dat_all[, c("b.sidi", "s.sidi")])

b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)
dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd
mean(dat_all$b.sidi.standardized)
sd(dat_all$b.sidi.standardized)

s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd   = sd(dat_all$s.sidi, na.rm = TRUE)
dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd
mean(dat_all$s.sidi.standardized)
sd(dat_all$s.sidi.standardized)

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")

#Fitting a simple linear regression
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)
slope_observed = coef(fit_1)[2]
plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)

dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

set.seed(123)
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)

plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices (MC resampled data)",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)


#

m = 10000
result_mc = numeric(m)
for(i in 1:m)
 {  
  index_1 = sample(nrow(dat_1), replace = TRUE)
    index_2 = sample(nrow(dat_1), replace = TRUE)
    dat_resampled_i =
        data.frame(
           b.sidi = dat_1$b.sidi[index_1],
            s.sidi = dat_1$s.sidi[index_2]
          )
    fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
    result_mc[i] = coef(fit_resampled_i)[2]
  }

quantile(result_mc, c(.05))

hist(
  result_mc,
  main = "Elyse's Null Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed, col = "blue")
abline(v = -0.0132, lty = 2, col = "red", lwd = 2)


# Null v. alternative

set.seed(345)
index_1 = sample(nrow(dat_1), replace = TRUE)

dat_boot = dat_1[index_1, ]
head(dat_boot)

m1 = 10000
result_boot = numeric(m1)
for(i in 1:m1)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  
  dat_boot = dat_1[index_1, ]
  
  fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)
  result_boot[i] = coef(fit_bs1)[2]
}

fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)

coef(fit_bs1)


# Double Density plot

help("plot")

plot(
  density(result_mc),
  main = "Elyse's Null and Alternative Distribution Density Plot",
  xlab = "Slope Coefficient",
  col = "skyblue",
  xlim = c(-0.05,0.05),
  ylim = c(0,70))

lines(
  density(result_boot), col = "deeppink")
  
legend(
  x = "topright",         
  legend = c("null", "alternative"),  
  col = c("skyblue", "deeppink"),
  lty = c(1, 1),
  lwd = 2,
  bty = "n")        
            





