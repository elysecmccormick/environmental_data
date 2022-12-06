#Lab 4 - Uncertainty and Error

#Practicing probability distributions

#Probability density
x = seq(-3, 3, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)

#Practicing histograms

require(palmerpenguins)
hist(
  penguins$body_mass_g,
  main = "Histogram of Penguin Body Mass",
  xlab = "Body Mass (g)")

# Mean, sd, and number of rows in the data. 
# na.rm = TRUE asks R to get rid of missing values in the dataset

mean(penguins$body_mass_g, na.rm = TRUE)
sd(penguins$body_mass_g, na.rm = TRUE)
nrow(penguins)

# generate four vectors of random penguin body masses with variables,
# so that the numbers in the variables can be re-coded

n_samples = 344
pop_sd = 802
pop_mean = 4202

dat_1 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_2 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_3 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_4 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)

# Generate histograms from the random penguin body mass datasets
# par(mfrow = c(2,2)) allows you to make a 2x2 plot of all histograms

par(mfrow = c(2, 2))

hist(dat_1)
hist(dat_2)
hist(dat_3)
hist(dat_4)

# The runif() function will generate random, uniformly-distributed numbers

par(mfrow = c(1, 1))
dat_unif = runif(n = 27, min = 0, max = 4)
hist(dat_unif)

# With a bigger sampling size, you can get a more normal distribution

dat_unif = runif(n = 400, min = 0, max = 4)
hist(dat_unif)

# set.seed() initializes the starting point for random number generation in R
# otherwise, it uses its own RNG based on current time

set.seed(1)
dat_unif_1 = runif(n = 270, min = 0, max = 4)
set.seed(1)
dat_unif_2 = runif(n = 270, min = 0, max = 4)

par(mfrow = c(1, 2))
hist(dat_unif_1)
hist(dat_unif_2)

# These came out identical, because set.seed() is generating the same loops for them. 
# From 1-270, set.seed() starts at the same point, and creates a giant loop from 1-270 to repeat numbers in the same way

#Starting to calculate residuals

set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

par(mfrow = c(1, 1))
plot(x, y, pch = 16)
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

#Create the dataset

n_pts = 10
x_min = 1
x_max = 10

# X values are uniformly distributed
x_random = runif(n = n_pts, min = x_min, max = x_max)

# Y values are normally-distributed.
# I used the default parameters for mean and sd.
y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 8)


#With each run, it should come out differently, because I used runif() as a "true" RNG. Run with squares

n_pts = 10
x_min = 1
x_max = 10

x_random = runif(n = n_pts, min = x_min, max = x_max)

y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 15)

#Run with cicle-x

y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 13)

#Run code with set.seed(123) with squares

set.seed(123)
n_pts = 10
x_min = 1
x_max = 10

x_random = runif(n = n_pts, min = x_min, max = x_max)

y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 15)

#Run code with different set.seed()

set.seed(300)
n_pts = 10
x_min = 1
x_max = 10

x_random = runif(n = n_pts, min = x_min, max = x_max)

y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 15)

# Fit a linear deterministic model

set.seed(123)
n_pts = 10
x_min = 1
x_max = 10

guess_x = 6
guess_y = 0
guess_slope = 0.1

x_random = runif(n = n_pts, min = x_min, max = x_max)

y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 8)
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)

dat_random$y_predicted = line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)

dat_random$resids = dat_random$y - dat_random$y_predicted

sum(dat_random$resids)

sum(abs(dat_random$resids))

plot( x = dat_random$y, xlab = "Observed", y = dat_random$resids, ylab = "Residuals", pch = 8)

hist(dat_random$resids, xlab = "Residuals")

#Question 1 - Create Vectors

n_17 <- 17
n_30 <- 30
n_300 <- 300
n_3000 <- 3000
avg <- 10.4
std <- 2.4

norm_17 <- rnorm(n = n_17, mean = avg, sd = std)
norm_30 <- rnorm(n = n_30, mean = avg, sd = std)
norm_300 <- rnorm(n = n_300, mean = avg, sd = std)
norm_3000 <- rnorm(n = n_3000, mean = avg, sd = std)

# Question 2 - Make and Save Histograms

par(mfrow = c(2,2))
hist(norm_17, xlab = "Elements", main = "Dispersal of 17 Elements")
hist(norm_30, xlab = "Elements", main = "Dispersal of 30 Elements")
hist(norm_300, xlab = "Elements", main = "Dispersal of 300 Elements")
hist(norm_3000, xlab = "Elements", main = "Dispersal of 3000 Elements")

require(here)
png(
  filename = here("lab_04_hist_01.png"),
  width = 1500, height = 1600, 
  res = 180, units = "px")

par(mfrow = c(2,2))
hist(norm_17, xlab = "Elements", main = "Dispersion of 17 Elements")
hist(norm_30, xlab = "Elements", main = "Dispersion of 30 Elements")
hist(norm_300, xlab = "Elements", main = "Dispersion of 300 Elements")
hist(norm_3000, xlab = "Elements", main = "Dispersion of 3000 Elements")
dev.off()

# Question 7
# Example
x = seq(-6, 6, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Standard Normal PDF", type = "l", xlim = c(-3, 3))
abline(h = 0)

#Real
# the mean 10.4 - (2.4*4) is how the sequence limits were obtained. 
# xlims = 10 more and 10 less than the mean
x = seq(0.8, 20, length.out = 1000)
y = dnorm(x, mean = 10.4, sd = 2.4)

plot(x, y, main = "Standard Normal PDF: mean = 10.4, sd = 2.4", type = "l", xlim = c(0.4, 20.4))
abline(h = 0)

# Question 8 - save as a svg
require(here)
svg("norm_1.svg")

x = seq(0.8, 20, length.out = 1000)
y = dnorm(x, mean = 10.4, sd = 2.4)
plot(x, y, main = "Standard Normal PDF: mean = 10.4, sd = 2.4", type = "l", xlim = c(0.4, 20.4))
abline(h = 0)
dev.off()


plot(x, y, main = "Standard Normal PDF: mean = 10.4, sd = 2.4", type = "l", xlim = c(0.4, 20.4))
abline(h = 0)

# Question 9 - random data figures!

par(mfrow = c(2,2))
setseed(119)
n_pts = 119
x_min = 1
x_max = 119

guess_x = 0
guess_y = 0
guess_slope = 0.02

x_random = runif(n = n_pts, min = x_min, max = x_max)

y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 8, col = "steelblue1", main = "Data for Scatterplot 1" )
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

n_samples = 1211
pop_sd = 800
pop_mean = 1905

dat_1 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
hist(dat_1, xlab = "X", main = "Data 2 Histogram", col = "mediumvioletred")

setseed(200)
n_pts = 124
x_min = 1
x_max = 124

guess_x = 0
guess_y = 0
guess_slope = 0.02

x_random = runif(n = n_pts, min = x_min, max = x_max)

y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 10, col = "midnightblue", main = "Data for Scatterplot 3" )
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

setseed(500)
n_pts = 350
x_min = 1
x_max = 350

guess_x = 0
guess_y = 0
guess_slope = 0.02

x_random = runif(n = n_pts, min = x_min, max = x_max)

y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

boxplot(dat_random$x, ylab = "x Values", main = "Boxplot 4", col = "mediumspringgreen")


# Question 10 - save as a png
require(here)
png(
  filename = here("RandomData4.png"),
  width = 1500, height = 1600, 
  res = 180, units = "px")

par(mfrow = c(2,2))
setseed(119)
n_pts = 119
x_min = 1
x_max = 119

guess_x = 0
guess_y = 0
guess_slope = 0.02

x_random = runif(n = n_pts, min = x_min, max = x_max)

y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 8, col = "steelblue1", main = "Data for Scatterplot 1" )
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

n_samples = 1211
pop_sd = 800
pop_mean = 1905

dat_1 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
hist(dat_1, xlab = "X", main = "Data 2 Histogram", col = "mediumvioletred")

setseed(200)
n_pts = 124
x_min = 1
x_max = 124

guess_x = 0
guess_y = 0
guess_slope = 0.02

x_random = runif(n = n_pts, min = x_min, max = x_max)

y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 10, col = "midnightblue", main = "Data for Scatterplot 3" )
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

setseed(500)
n_pts = 350
x_min = 1
x_max = 350

guess_x = 0
guess_y = 0
guess_slope = 0.02

x_random = runif(n = n_pts, min = x_min, max = x_max)

y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

boxplot(dat_random$x, ylab = "x Values", main = "Boxplot 4", col = "mediumspringgreen")

dev.off()


#Q11 - Residuals

set.seed(119)
n_pts = 119
x_min = 1
x_max = 119

guess_x = 0
guess_y = 0
guess_slope = 0.02

x_random = runif(n = n_pts, min = x_min, max = x_max)

y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 8, col = "steelblue1", main = "Data for Scatterplot 1" )
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)

dat_random$y_predicted = line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)

dat_random$resids = dat_random$y - dat_random$y_predicted

sum(dat_random$resids)

sum(abs(dat_random$resids))

plot( x = dat_random$y, xlab = "Observed", y = dat_random$resids, ylab = "Residuals", pch = 8,)

hist(dat_random$resids, xlab = "Residuals")

par(mfrow = c(1,2))
plot( x = dat_random$y, xlab = "Observed", y = dat_random$resids, ylab = "Residuals", pch = 8, col= "steelblue1")

hist(dat_random$resids, xlab = "Residuals", main = "Residuals", col = "steelblue1")

#Q.12 

require(here)
png(
  filename = here("Residuals_lab4.png"),
  width = 1500, height = 1600, 
  res = 180, units = "px")

set.seed(119)
n_pts = 119
x_min = 1
x_max = 119

guess_x = 0
guess_y = 0
guess_slope = 0.02

x_random = runif(n = n_pts, min = x_min, max = x_max)

y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 8, col = "steelblue1", main = "Data for Scatterplot 1" )
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)

dat_random$y_predicted = line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)

dat_random$resids = dat_random$y - dat_random$y_predicted

sum(dat_random$resids)

sum(abs(dat_random$resids))

par(mfrow = c(1,2))
plot( x = dat_random$y, xlab = "Observed", y = dat_random$resids, ylab = "Residuals", pch = 8, col= "steelblue1")

hist(dat_random$resids, xlab = "Residuals", main = "Residuals", col = "steelblue1")
dev.off()


require(here)
png(
  filename = here("Q12_lab4.png"),
  width = 1500, height = 1600, 
  res = 180, units = "px")

set.seed(119)
n_pts = 119
x_min = 1
x_max = 119

guess_x = 0
guess_y = 0
guess_slope = 0.02

x_random = runif(n = n_pts, min = x_min, max = x_max)

y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 8, col = "steelblue1", main = "Data for Scatterplot 1" )
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)
dev.off()


















