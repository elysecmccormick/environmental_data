# In Class Likelihood Assignment

require(here)

dat_bird = read.csv(here("data", "bird.sta.csv"))
dat_hab = read.csv(here("data", "hab.sta.csv"))

x_observed = c(2, 6)
print(x_observed)

# Poisson distribution's probability mass
dpois(x = 2, lambda = 4.5)

dpois(x = 6, lambda = 4.5)

#I know the likelihood of observing those particular counts together 
#is the product of the individual likelihoods:
dpois(x = 2, lambda = 4.5) * dpois(x = 6, lambda = 4.5)

#I can take advantage of vectorization in R by storing the counts in a single vector:
wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4.5)

#Calculate the product
prod(dpois(x = wiwa_counts, lambda = 4.5))

#And the sum of log likelihoods
sum(log(dpois(x = wiwa_counts, lambda = 4.5)))

sum(log(dpois(x = wiwa_counts, lambda = 4)))

#Merge data
dat_all = merge(dat_bird, dat_hab)

summary(dat_all$WIWA)
hist(dat_all$WIWA)
hist(dat_all$WIWA, breaks = 7)
hist(dat_all$WIWA, breaks = 0:7)

0:7 - 0.5
hist(dat_all$WIWA, breaks = 0:7 - .5)

#Histograms of other counts
par(mfrow = c(1, 2))
dat = dat_all$WIWA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nWilson's Warbler counts")

dat = dat_all$GRJA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nGray Jay counts")

sum(log(dpois(x = dat_all$WIWA, lambda = 1.0)))

# Winter Wrens (WIWR) Data

wiwr_counts = c(2,6)
dpois(x = wiwa_counts, lambda = 4.5)

0:7 - 0.5
hist(dat_all$WIWR, breaks = 0:7 - .5)

sum(log(dpois(x = dat_all$WIWR, lambda = 1.45)))

sum(log(dbinom(x = dat_all$WIWR, size = 6, prob = 0.24))) 

sum(log(dbinom(x = dat_all$WIWR, size = max(dat_all$WIWR), prob = 0.24))) 

set.seed(1)
vec_rnorm = rnorm(n = 10, mean = 0, sd = 1)

print(vec_rnorm)

set.seed(1) 

#Just find the log of the dnorm of the vector you just made
#Uses dnorm because you want to find the likelihood 
#gives a theoretical distribution

vec_rnorm = rnorm(n = 10, mean = 0, sd = 1) 

vec_rnorm 

log(dnorm(vec_rnorm, 0, 1)) 

sum(log(dnorm(vec_rnorm, 0, 1))) 

