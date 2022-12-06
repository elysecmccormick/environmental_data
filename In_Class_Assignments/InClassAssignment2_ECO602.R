#In Class R Exercise Week 2

#Dataset

data(iris)

#Look at the headers of the data

head(iris)

#Calculate the mean and StDev

iris$Sepal.Width
mean(iris$Sepal.Length)
sd(iris$Sepal.Width)

#Scatterplot

plot(x = iris$Sepal.Width, y = iris$Sepal.Length)

#What's in the center of the data?

data_center_x = mean(iris$Sepal.Width)
data_center_y = mean(iris$Sepal.Length)
c(data_center_x, data_center_y)

#Add point to exisiting plot

plot(x = iris$Sepal.Width, y = iris$Sepal.Length)
points(x = data_center_x, y = data_center_y, col = "red")

#Try Other Variables - - Do it with group

library(MASS)
data(Animals)
head(Animals)

#Mean and SD

Animals$body
mean(Animals$brain)
sd(Animals$body)

#Scatterplot

plot(x = Animals$brain, y = Animals$body)

plot(x = Animals$body, y  = Animals$brain)

data_center_x = mean(Animals$body)
data_center_y = mean(Animals$brain)
c(data_center_x, data_center_y)

plot(x = Animals$brain, y = Animals$body)
points(x = data_center_x, y = data_center_y, col = "blue")

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
line_point_slope(2, 4, 4, -2)

plot(x = Animals$brain, y = Animals$body)
points(x = data_center_x, y = data_center_y, col = "blue")
curve(
  line_point_slope(
    x, 
    data_center_x, 
    data_center_y,
    -0.1), 
  add = TRUE)

plot(x = Animals$body, y = Animals$brain, main = "Elyse's Graph of Animal Brain vs. Body Size", ) 

points(x = data_center_x, y = data_center_y, col = "red") 

curve( 
  
  line_point_slope( 
    
    x,  
    
    5,  
    
    20, 
    
    -10),  
  
  add = TRUE) 







