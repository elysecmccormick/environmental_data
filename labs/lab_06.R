# Lab 6

# Question 1 - Making a standard error function

rm(list = ls())

# 1) is.na will give you all the NA values back to you.
# 2) !is.na does the opposite, it gets rid of your NA values
# 3) You have to use this for the SD and the length because otherwise 
# it will measure exactly what it reads (along with the NAs) from the data sheet. 

sse_mean = function(x) sd(x[!is.na(x)])/ sqrt(length(x[!is.na(x)]))
require(palmerpenguins)

sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)

# Questions 2 and 3

# Example two_group_resample_diff function

# Set up data to  work with
dat_pen = subset(penguins, species != "Gentoo")
x = dat_pen$flipper_length_mm
n_1 = 68
n_2 = 152

dat_1 = sample(x, n_1, replace = TRUE)
dat_2 = sample(x, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

print(diff_simulated)

#Building the two_group_resample_diff function
two_group_resample_diff <- function(x, n_1, n_2)
{dat_1 = sample(x[!is.na(x)], n_1, replace = TRUE)
 dat_2 = sample(x[!is.na(x)], n_2, replace = TRUE)
 diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
   return(diff_simulated)}

#Using the two_group_resample_diff function
set.seed(54321)
two_group_resample_diff(dat_pen$flipper_length_mm, 68, 152)

#Do it 2000 times and make a histogram

n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample_diff(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences, main = "Resampled Differences of Penguin Flipper Length", col = "midnightblue")

sum(abs(mean_differences) > 5.8)

#Questions 7-11

#7 - Boxplot of Bill Length
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
boxplot(
  bill_length_mm ~ species, data = dat_pen,
  xlab = "Species",
  ylab = "Bill Length (mm)",
  col = "lightskyblue")

#8 and 9 - t-test of bill length between species

t.test(dat_pen$bill_length_mm ~ dat_pen$species)

t_test = t.test(dat_pen$bill_length_mm ~ dat_pen$species)

t_test$estimate

diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)

#10 - Critical differences and resampling

agg_means = aggregate(
  bill_length_mm ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_crit = diff(agg_means[, 2])

agg_means
diff_crit

#Use function two group resample function
two_group_resample_diff <- function(x, n_1, n_2)
{dat_1 = sample(x[!is.na(x)], n_1, replace = TRUE)
 dat_2 = sample(x[!is.na(x)], n_2, replace = TRUE)
diff_crit = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
return(diff_crit)}

#Using the two_group_resample_diff function
set.seed(54321)
two_group_resample_diff(dat_pen$bill_length_mm, 38.79, 48.83)

#Do it 2000 times and make a histogram

n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample_diff(dat_pen$bill_length_mm, 68, 152)
  )
}
hist(mean_differences, main = "Resampled Differences of Penguin Bill Length", col = "lightsteelblue")

sum(abs(mean_differences) > 1.2)

























