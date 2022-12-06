#In Class Probability 3

dpois(x = 7, lambda = 10.4)

# Standard normal has mean = 0 and sd = 1
dnorm(0.5, mean = 0, sd = 1)

dnorm(1, mean = 0, sd = 1)

# pnorm calculates the cumulative density of a normal distribution

pnorm(0.5, mean = 0, sd = 1)

# How many points?
n = 13

# Create a vector of x-values from -4 to 4:
x = seq(from = -6, to = 6, length.out = n)

# Create the corresponding y-values:
y = dnorm(x, mean = 0, sd = 1)

# plot!
plot(y ~ x, type = "l")

# How many points?
n = 1000

# Create a vector of x-values from -4 to 4:
x = seq(from = -6, to = 6, length.out = n)

# Create the corresponding y-values:
y = dnorm(x, mean = 0, sd = 1)

# plot!
plot(y ~ x, type = "l", ylab = "Probability Density")

#Mean of 0, SD of 2
y_2 = dnorm(x, mean = 0, sd = 3)

plot(y ~ x, type = "l", ylab = "Probability Density")
points(y_2 ~ x, type = "l", lty  = 2)




#Question 5

n = 6
p= 4/6
1 -  pbinom(q = 3, size = n , p= p)

#Q7

n = 6
p = 4/6
1 - pbinom(q = 1, size = n, p=p)


#Q9

par(mfrow = c(1, 2))
n = 1000
x = seq(from = -6, to = 6, length.out = n)
y = dnorm(x, mean = 0, sd = 1)
y_2 <- dnorm(x, mean = 0, sd = 2)
y_3 <- dnorm(x, mean = -2, sd = 1)
plot(y ~ x, type = "l", ylab = "Probability Density", main = "PDF Plot")
points(y_2 ~ x, type = "l", lty  = 2)
points(y_3 ~ x, type = "l", lty  = 3)

y_cdf_1 = pnorm(x, mean = 0, sd = 1)
plot(y_cdf_1 ~ x, type = "l", ylab = "cumulative density", main = "CDF Plot")
y_cdf_2 = pnorm(x, mean = 0, sd = 2)
points(y_cdf_2 ~ x, type = "l", lty = 2)
y_cdf_3 <- pnorm(x, mean = -2, sd = 1)
points(y_cdf_3 ~ x, type = "l", lty = 3)











