# Assignment: Data Exploration and Deterministic Functions

#Install packages

install.packages("psych")
require("psych")

install.packages("here")
require("here")

# Read in data

dat.bird <- read.csv(here("data", "bird.sta.csv"))
dat.hab <- read.csv(here("data", "hab.sta.csv"))

# Merge data

dat.birdhab <- data.frame(merge(dat.bird, dat.hab))

#Plot histograms of habitat variables

hist(dat.hab$elev, xlab = "Elevation", main = "Elevation Distribution")
hist(dat.hab$slope, xlab = "Slope", main = "Slope Distribution")
hist(dat.hab$aspect, xlab = "Aspect", main = "Aspect Distribution")

par(mfrow = c(1, 3))
hist(dat.hab$elev, xlab = "Elevation", main = "Elevation Distribution", col = adjustcolor(rgb(0,1,1), alpha.f = 0.2), pch = 16, cex = 2)
hist(dat.hab$slope, xlab = "Slope", main = "Slope Distribution", col = adjustcolor(rgb(1,1,0), alpha.f = 0.2), pch = 16, cex = 2)
hist(dat.hab$aspect, xlab = "Aspect", main = "Aspect Distribution", col = adjustcolor(rgb(0.5,0,1), alpha.f = 0.2), pch = 16, cex = 2)

# Plot Scatterplots of habitat variables

#Elevation

plot(x = dat.hab$elev, xlab = "Elevation", y = dat.hab$ba.tot, ylab = "Total Basal Area (m^2)",
     main = "Total Basal Area Across Elevation", col = adjustcolor(rgb(0,1,1), alpha.f = 0.2), pch = 16, cex = 2)

#Slope

plot(x = dat.hab$slope, xlab = "Slope", y = dat.hab$ba.tot, ylab = "Total Basal Area (m^2)",
     main = "Total Basal Area Across Slopes", col = adjustcolor(rgb(1,1,0), alpha.f = 0.2), pch = 16, cex = 2)

#Aspect

plot(x = dat.hab$aspect, xlab = "Aspect", y = dat.hab$ba.tot, ylab = "Total Basal Area (m^2)",
     main = "Total Basal Area Across Aspects", col = adjustcolor(rgb(0.5,0,1), alpha.f = 0.2), pch = 16, cex = 2)

# Pair plots together

par(mfrow = c(1, 3))
plot(x = dat.hab$elev, xlab = "Elevation", y = dat.hab$ba.tot, ylab = "Total Basal Area (m^2)",
     main = "Total Basal Area Across Elevation", col = adjustcolor(rgb(0,1,1), alpha.f = 0.2), pch = 16, cex = 2)
plot(x = dat.hab$slope, xlab = "Slope", y = dat.hab$ba.tot, ylab = "Total Basal Area (m^2)",
     main = "Total Basal Area Across Slopes", col = adjustcolor(rgb(1,1,0), alpha.f = 0.2), pch = 16, cex = 2)
plot(x = dat.hab$aspect, xlab = "Aspect", y = dat.hab$ba.tot, ylab = "Total Basal Area (m^2)",
          main = "Total Basal Area Across Aspects", col = adjustcolor(rgb(0.5,0,1), alpha.f = 0.2), pch = 16, cex = 2)

# Scatterplots with fitted linear models

#Elevation

plot(x = dat.hab$elev, xlab = "Elevation", y = dat.hab$ba.tot, ylab = "Total Basal Area (m^2)",
     main = "Total Basal Area Across Elevation", col = adjustcolor(rgb(0,1,1), alpha.f = 0.2), pch = 16, cex = 2)
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

#Slope

plot(x = dat.hab$slope, xlab = "Slope", y = dat.hab$ba.tot, ylab = "Total Basal Area (m^2)",
     main = "Total Basal Area Across Slopes", col = adjustcolor(rgb(1,1,0), alpha.f = 0.2), pch = 16, cex = 2)
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

#Aspect

plot(x = dat.hab$aspect, xlab = "Aspect", y = dat.hab$ba.tot, ylab = "Total Basal Area (m^2)",
     main = "Total Basal Area Across Aspects", col = adjustcolor(rgb(0.5,0,1), alpha.f = 0.2), pch = 16, cex = 2)
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

# Pair plots together with linear models

par(mfrow = c(1, 3))
plot(x = dat.hab$elev, xlab = "Elevation", y = dat.hab$ba.tot, ylab = "Total Basal Area (m^2)",
     main = "Total Basal Area Across Elevation", col = adjustcolor(rgb(0,1,1), alpha.f = 0.2), pch = 16, cex = 2)
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
plot(x = dat.hab$slope, xlab = "Slope", y = dat.hab$ba.tot, ylab = "Total Basal Area (m^2)",
     main = "Total Basal Area Across Slopes", col = adjustcolor(rgb(1,1,0), alpha.f = 0.2), pch = 16, cex = 2)
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
plot(x = dat.hab$aspect, xlab = "Aspect", y = dat.hab$ba.tot, ylab = "Total Basal Area (m^2)",
     main = "Total Basal Area Across Aspects", col = adjustcolor(rgb(0.5,0,1), alpha.f = 0.2), pch = 16, cex = 2)
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






