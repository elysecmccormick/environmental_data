#Frequentist Concepts Assignment

#Explanation of probability. 
#pnorm is the probability of observing a value of x or less
#What is the probability of observing a value less than 7.5 
#in a normal distribution with mean 10 and standard deviation 3?

pnorm(7.5, mean = 10, sd = 3)

#0.2023284

#Question 1: What is the probability of observing a count of 
#exactly 3 successes in a binomial distribution with parameters n = 4 and p = 0.75?

dbinom(4,size = 3, prob = 0.75, log = FALSE)

#Question 2: What is the probability of observing a count of 3 successes 
#or fewer in a binomial distribution with parameters n = 4 and p = 0.75?

pbinom(4, size = 3, prob = 0.75, log.p = FALSE)

# 1

#What is the probability of observing more than 3 successes in a
#binomial distribution with parameters n = 5 and p = 0.75?

pbinom(5, size = 3, prob = 0.75, log.p = FALSE)

#1



#Question 13

410*40*(80+1)
#1,328,400


(410*40*80)+1

25^1312001
