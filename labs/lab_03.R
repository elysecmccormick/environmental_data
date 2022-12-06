# Lab 3

#Install psych package

install.packages("psych")
require(psych)

#Pairplots for the Iris data

pairs.panels(iris)

#Subsetting data within the iris dataset, then make pairplots with the subset

names(iris)
pairs.panels(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length")])

#Install here package

install.packages("here")
require(here)

#Read in data. Notice that the subdirectory data and the filename
#bird.sta.csv are both in quotation marks since I want R to interpret them as literal text and not expressions to be evaluated.

dat_bird = read.csv(
  here("data", "bird.sta.csv")
)
head(dat_bird)

dat_habitat = read.csv(
  here("data", "hab.sta.csv")
)
head(dat_habitat)

dat_all <- data.frame(merge(dat_bird, dat_habitat))

#Test merge by making a scatterplot

plot(ba.tot ~ elev, data = dat_all)

# counts of Cedar Waxwings at 100 randomly sampled sites:

sample(dat_all$CEWA, 100)

#Build Boolean vector for finding the number of cedar waxwing

cw <- dat_all$CEWA
cw > 1
as.numeric(cw > 1)
cewa_present_absent <- as.numeric(cw > 1)
plot(x = dat_all$elev, y = cewa_present_absent)

# Fitting a logistic curve
# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

#Plotting the logistic curves
#Positive slope
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

#Negative slope
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.005), add = TRUE)

#Make pairplots for elevation, slope, and aspect with total basal area

names(dat_all)
pairs.panels(dat_all[, c("elev", "slope", "aspect", "ba.tot")])

#Make plots of goldfinches and purple finches

#Goldfinches

gf <- dat_all$AMGO
gf > 1
as.numeric(gf > 1)
amgo_present_absent <- as.numeric(gf > 1)
plot(x = dat_all$elev, y = amgo_present_absent)


get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

get_logistic_param_b = function(slope)
{
  return (slope / 4)
}

logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}


logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

plot(x = dat_all$ba.tot, xlab = "Total Basal Area (m^2/hect)", y = amgo_present_absent, ylab = "American Goldfinch Presence", main = "American Goldfinch Presence within a Stand")
curve(logistic_midpoint_slope(x, midpoint = 30, slope = -1), add = TRUE)

# Goldfinch plots with pretty colors

plot(x = dat_all$ba.tot, xlab = "Total Basal Area (m^2/hect)",
     y = amgo_present_absent, ylab = "American Goldfinch Presence", 
     main = "American Goldfinch Presence within a Stand",
     col = adjustcolor(rgb(1,1,0), alpha.f = 0.2), pch = 16, cex = 2)
curve(logistic_midpoint_slope(x, midpoint = 30, slope = -1), add = TRUE)



# Purple Finch

pf <- dat_all$PUFI
pf > 1
as.numeric(pf > 1)
pufi_present_absent <- as.numeric(pf > 1)
plot(x = dat_all$elev, y = pufi_present_absent)


get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

get_logistic_param_b = function(slope)
{
  return (slope / 4)
}

logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}


logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

plot(x = dat_all$ba.tot, xlab = "Total Basal Area (m^2/hect)",
     y = pufi_present_absent, ylab = "Purple Finch Presence",
     main = "Purple Finch Presence within a Stand",
     col = 2, pch = 16, cex = 2)

# Purple Finch plot with pretty colors

plot(x = dat_all$ba.tot, xlab = "Total Basal Area (m^2/hect)",
     y = pufi_present_absent, ylab = "Purple Finch Presence",
     main = "Purple Finch Presence within a Stand",
     col = adjustcolor(rgb(0.7,0,1), alpha.f = 0.2), pch = 16, cex = 2)
curve(logistic_midpoint_slope(x, midpoint = 30, slope = -1), add = TRUE)

# Gray Jays

gj <- dat_all$GRJA
gj > 1
as.numeric(gj > 1)
grja_present_absent <- as.numeric(gj > 0)
sum(grja_present_absent)

## This is how you answer question 9, but to get at 7, you want to get the total abundance... puzzle over it. 

