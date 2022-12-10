# Final R Reference guide

# Loading data and packages

# Loading packages

# Both require() and library() will load packages
require(here)
library(palmerpenguins)

# Loading Data
ginkgo_dat = read.csv(
  here("data", "ginkgo_data_2022.csv")
)
head(ginkgo_dat)

#############################################################################

# Data structures

# The function c() combines or concatenates its arguments into a vector
#(a 1-dimensional data structure consisting of 1 or more elements).
# All of the elements must be of the same type.
# I can’t combine character and numeric types in the same call to c()
# Here’s two examples using numeric and character data types:


## Create a vector of numbers:
num_vec  = c(1, 4, 8, 9, 13)

## Create a vector of characters:
char_vec = c("a", "fish", "data is cool")

# I can show the contents of a vector by typing the name of the vector, 
# or using the print() function.

## Typing the name of the vector into the console prints the contents
num_vec

## The print() function accomplishes the same task:
print(char_vec)

help("length")

length(num_vec)

help("matrix")
 help("data.frame")
 
ginkgo_data <- data.frame(ginkgo_dat)

help("nrow")

help("dim") 

help("cex")

??cex

plot(x = ginkgo_data$max_depth, y = ginkgo_data$max_width,
     xlim = NULL, ylim = NULL, xlab = "Maximum Leaf Depth (cm)", 
     ylab = "Maximum Leaf Width (cm)", main = "Ginkgo Leaf Dimensions",
     pch = 8, cex = 1, col = "gold")



require(palmerpenguins)
dat_pen <- palmerpenguins::penguins
hist(dat_pen$flipper_length_mm, breaks = 5, xlab = "Flipper Length (mm)", main = "Histogram of Penguin Flipper length ", 
     col = "skyblue")

par(mfrow = c(1,2))
boxplot(ginkgo_data$petiole_length, main = "Ginkgo Petiole Lengths", xlab = "", ylab = "Petiole Length", col = "springgreen3")

boxplot(formula = max_width ~ seeds_present, data = ginkgo_data, main = "Ginkgo Leaf Width Explained by Seed Presence",
        xlab = "Seed Presence", ylab = "Maximum Width (cm)", col = "gold")


par(mfrow = c(2,2))

hist(dat_pen$body_mass_g, breaks = 5, xlab = "Body Mass (g)", main = "Histogram of Penguin Body Mass", 
     col = "midnightblue")

hist(dat_pen$bill_length_mm, breaks = 5, xlab = "Bill Length (mm)", main = "Histogram of Penguin Bill length ", 
     col = "skyblue")

hist(dat_pen$bill_depth_mm, breaks = 5, xlab = "Flipper Length (mm)", main = "Histogram of Penguin Bill Depth ", 
     col = "royalblue1")

hist(dat_pen$flipper_length_mm, breaks = 5, xlab = "Flipper Length (mm)", main = "Histogram of Penguin Flipper length ", 
     col = "steelblue")

x = seq(0.8, 20, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)


Distributions

par(mfrow = c(2,2))

x = seq(-3, 3, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal Probability Density", type = "l")

x = seq(-3, 3, length.out = 1000)
y = pnorm(x)

plot(x, y, main = "Cumulative Probability Distribution", type = "l")

x = seq(-3, 3, length.out = 1000)
y = qnorm(x)

plot(x, y, main = "Quantile Function", type = "l")

x = seq(-3, 3, length.out = 1000)
y = rnorm(x)

plot(x, y, main = "Random Numbers", type = "l")


help("pbinom")

















