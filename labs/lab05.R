# Lab 05

#Creating a Ricker Function. First part is the function,
#second part (after the curve() function) is to plot it

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

#Building an exponential function

exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}
curve(
  exp_fun(x, 1, -1/10), from = 0, to = 10, add = FALSE,
  main = "Exponential Function Test Plot",
  xlab = "x", ylab = "f(x)")

#Self-test exponential function

exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}
curve(
  exp_fun(x, 2.2, 1/15), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

#Simulating Data

#Simulating data on a line

# Seed the RNG so we can reproduce our results
set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)

#Choose an intercept and slope, then plot generated predicted "y" values
param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred, main = "Simulated Data\nNo Errors", xlab = "", ylab = "")

#Normal Errors 1

#Add normally distributed noise

error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")


#Normal errors 2: More Sophisticated Stochasticity

error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)

par(mfrow = c(1, 2))
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")
plot(x_sim, y_observed_2, main = "Normally Distributed Errors\n Increasing Variance", xlab = "", ylab = "")

error_mean = 0
error_sd = 0.30

y_observed_3 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)


#Fitted Linear Models

fit_1 = lm(y_observed ~ x_sim)
fit_2 = lm(y_observed_2 ~ x_sim)
fit_3 = lm(y_observed_3 ~ x_sim)

par(mfrow = c(1, 3))

plot(y_observed ~ x_sim); abline(fit_1)
plot(y_observed_2 ~ x_sim); abline(fit_2)
plot(y_observed_3 ~ x_sim); abline(fit_3)

# Question 1

exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}
curve(
  exp_fun(x, 1, -1/10), from = 0, to = 10, add = FALSE,
  main = "Exponential Function Test Plot",
  xlab = "x", ylab = "f(x)")

#Question 2

exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}

curve(
  exp_fun(x, 1.9, 0.1), col = "black",  
  from = 0, to = 50, add = TRUE, main = "Exponential Function Test Plot",
  xlab = "x", ylab = "f(x)")
curve(
  exp_fun(x, 1.9, 0.3), col = "black", lty = "dotted", 
  from = 0, to = 50, add = TRUE)
curve(
  exp_fun(x, 1.2, 0.2), col = "red", 
  from = 0, to = 50, add = TRUE)
curve(
  exp_fun(x, 1.2, 0.4), col = "red", lty = "dotted", 
  from = 0, to = 50, add = TRUE)

#Question 5

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
curve(
  ricker_fun(x, 25, 0.2), col = "black",
  from = 0, to = 50, ylim = c(0, 100), add = FALSE, main = "Ricker Functions Test Plot",
  ylab = "f(x)", xlab = "x")
curve(
  ricker_fun(x, 20, 0.2), col = "black", lty = "dotted",
  from = 0, to = 50, ylim = c(0, 100), add = TRUE, main = "Ricker Functions Test Plot",
  ylab = "f(x)", xlab = "x")
curve(
  ricker_fun(x, 10, 0.2), col = "black", lty = "dotted",
  from = 0, to = 50, ylim = c(0, 100), add = TRUE, main = "Ricker Functions Test Plot",
  ylab = "f(x)", xlab = "x")
curve(
  ricker_fun(x, 75, 0.3), col = "red", 
  from = 0, to = 50, ylim = c(0, 100), add = TRUE, main = "Ricker Functions Test Plot",
  ylab = "f(x)", xlab = "x")
curve(
  ricker_fun(x, 50, 0.3), col = "red", lty = "dotted",
  from = 0, to = 50, ylim = c(0, 100), add = TRUE, main = "Ricker Functions Test Plot",
  ylab = "f(x)", xlab = "x")
curve(
  ricker_fun(x, 40, 0.3), col = "red", lty = "dotted",
  from = 0, to = 50, ylim = c(0, 100), add = TRUE, main = "Ricker Functions Test Plot",
  ylab = "f(x)", xlab = "x")

#Question 9 

install(psych)
isntall(here)
require(psych)
require(here)
sal_dat = read.csv(here("data", "dispersal.csv"))

plot(
  sal_dat$dist.class,
  sal_dat$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "Distance Class", 
  ylab = "Standardized Dispersal Rate", 
  main = "Marbled Salamander - first time breeders\n(Bad) linear model")
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
curve(line_point_slope(x, 1500, 0, -0.00025), add = TRUE)

# Question 11

plot(sal_dat$dist.class,
     sal_dat$disp.rate.ftb,
     xlim = c(0, 1500),
     xlab = "Distance Class", 
     ylab = "Standardized Dispersal Rate", 
     main = "Marbled Salamander - First Time Breeders Exponential Model")
exp_fun = function(x, a, b) 
     {
       return(a * exp(-b * x))
     }
curve(exp_fun(x, 0.7, 1/300), from = 0, to = 1500, add = TRUE)
 
#Question 13

plot(sal_dat$dist.class,
     sal_dat$disp.rate.ftb,
     xlim = c(0, 1500),
     xlab = "Distance Class", 
     ylab = "Standardized Dispersal Rate", 
     main = "Marbled Salamander - First Time Breeders Ricker Model")
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
curve(
  ricker_fun(x, 0.003, 1/300), from = 0, to = 1500, add = TRUE)

# Question 14 and 15 

guess_x = 1500
guess_y = 0
guess_slope = -0.00025

plot(
  sal_dat$dist.class,
  sal_dat$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "Distance Class", 
  ylab = "Standardized Dispersal Rate", 
  main = "Marbled Salamander - first time breeders\n(Bad) linear model")
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
curve(line_point_slope(x, 1500, 0, -0.00025), add = TRUE)

line_point_slope(sal_dat$dist.class, guess_x, guess_y, guess_slope)

sal_dat$y_predicted = line_point_slope(sal_dat$dist.class, guess_x, guess_y, guess_slope)

sal_dat$resids_linear = sal_dat$disp.rate.ftb - sal_dat$y_predicted

sum(sal_dat$resids_linear, na.rm = TRUE)

plot( x = sal_dat$disp.rate.ftb, xlab = "Observed", y = sal_dat$resids, ylab = "Residuals", pch = 8)

hist(sal_dat$resids_linear, xlab = "Residuals", main = "Histogram of Linear Residuals")

guess_xe = 300
guess_a = 0.7
guess_b = 1/300

plot(sal_dat$dist.class,
     sal_dat$disp.rate.ftb,
     xlim = c(0, 1500),
     xlab = "Distance Class", 
     ylab = "Standardized Dispersal Rate", 
     main = "Marbled Salamander - First Time Breeders Exponential Model")
exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}
curve(exp_fun(x, 0.7, 1/300), from = 0, to = 1500, add = TRUE)

line_point_slope(sal_dat$dist.class, guess_x, guess_a, guess_b)

sal_dat$ey_predicted = line_point_slope(sal_dat$dist.class,guess_xe, guess_a, guess_b)

sal_dat$resids_exp = sal_dat$disp.rate.ftb - sal_dat$ey_predicted

sum(sal_dat$resids_exp, na.rm = TRUE)

sum(abs(sal_dat$resids_exp))

plot( x = sal_dat$disp.rate.ftb, xlab = "Observed", y = sal_dat$resids_exp, ylab = "Residuals", pch = 8)

hist(sal_dat$resids_exp, xlab = "Residuals", main = "Histogram of Exp Residuals")

guess_xr = 300
guess_ar = 0.003
guess_br = 1/300

plot(sal_dat$dist.class,
     sal_dat$disp.rate.ftb,
     xlim = c(0, 1500),
     xlab = "Distance Class", 
     ylab = "Standardized Dispersal Rate", 
     main = "Marbled Salamander - First Time Breeders Ricker Model")
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
curve(
  ricker_fun(x, 0.003, 1/300), from = 0, to = 1500, add = TRUE)
line_point_slope(sal_dat$dist.class, guess_xr, guess_ar, guess_br)

sal_dat$ry_predicted = line_point_slope(sal_dat$dist.class,guess_xr, guess_ar, guess_br)

sal_dat$resids_ricker = sal_dat$disp.rate.ftb - sal_dat$ry_predicted

sum(sal_dat$resids_ricker, na.rm = TRUE)

sum(abs(sal_dat$resids_ricker))

plot( x = sal_dat$disp.rate.ftb, xlab = "Observed", y = sal_dat$resids_ricker, ylab = "Residuals", pch = 8)

hist(sal_dat$resids_ricker, xlab = "Residuals", main = "Histogram of Ricker Residuals")

par(mfrow = c(1, 3))
hist(sal_dat$resids_linear, xlab = "Residuals", main = "Histogram of Linear Residuals", col = "red")
hist(sal_dat$resids_exp, xlab = "Residuals", main = "Histogram of Exp Residuals", col = "yellow")
hist(sal_dat$resids_ricker, xlab = "Residuals", main = "Histogram of Ricker Residuals", col = "blue")






