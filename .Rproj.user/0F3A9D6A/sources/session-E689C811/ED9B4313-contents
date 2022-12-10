# Lab 12

#Review the population dispersion and statistical power simulation you created for Lab 11.
#Re-run the simulation with with a small number of replicates, say 30, so that you get a jagged plot.
#Fit a LOWESS model using 30% of the data.
#Create a plot like the one I showed above of your simulated data and your fitted LOWESS model. Your plot must show:
 # The smooth LOWESS line
 #Points representing the simulation data.
 #An appropriate legend, title, and axis labels

# Clear environment

rm(list = ls())

#Packages

require(here)
require("rgl")
require(htmlwidgets)

# Import Data

dat_bird = read.csv(
  here("data", "bird.sub.csv")
)
head(dat_bird)

dat_hab = read.csv(
  here("data", "hab.sub.csv")
)
head(dat_hab)

birdhab <- data.frame(merge(dat_bird, dat_hab))
head(birdhab)

# Linear functions to insert into the power analysis

#linear model 1
fit_1 = lm(formula = BRCR ~ ls, data = birdhab)

# linear function
linear = function(x, y_int, slope)
{
  return(y_int + x * slope)
}

# linear function 2
linear_simulator = function(x, y_int, slope, st_dev)
{
  y <- linear(x, y_int, slope)
  n <- length(x)
  noise <- rnorm(n, mean = 0, sd = st_dev)
  
  return(y + noise)
}

linear_sim_fit = function(x, slope, y_int, st_dev)
{
  y_sim = linear_simulator(
    x = x,
    y_int = y_int,
    slope = slope,
    st_dev = st_dev
  )
  fit_sim = lm(y_sim ~ x)
  return(fit_sim)
}


# Retrieve the model coefficients

fit_1_coefs = coefficients(fit_1)
str(fit_1_coefs)

# Summary of the fit of the model

fit_1_summary = summary(fit_1)
str(fit_1_summary)
fit_1_summary$sigma
fit_1_summary

# Store observedd values of intercept, slope, and standard deviation

int_obs <- 0.0991
slope_obs <- 0.00584
sd_obs <- fit_1_summary$sigma

###########################################################################

# Sample Size Power analysis

alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)

sample_sizes = seq(5, 100)
sample_size_powers = numeric(length(sample_sizes))

# The maximum x value in the simulation.
# Use the maximum observed x-value in the data
max_x = max(birdhab$ls)

for(j in 1:length(sample_sizes))
{
  # A sequence of equally-spaced x-values:
  x_vals = seq(0, max_x, length.out = sample_sizes[j])
  
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = x_vals,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = sd_obs
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  sample_size_powers[j] = sum(p_vals < alpha) / n_sims
}


sim_sample_size = 
  data.frame(
    sample_size = sample_sizes,
    power       = sample_size_powers)

###########################################################################

# Adding the LOWESS Model

fit_lowess_50 = loess(power ~ sample_size, data = sim_sample_size, span = 0.5)

newdata_sample_size = data.frame(sample_size = seq(2, 20, length.out = 100)) 

plot(
  x = newdata_sample_size$sample_size,
  y = predict(fit_lowess_50, newdata = newdata_sample_size),
  type = "l",
  ylab = "Statistical Power", xlab = "Sample Size")


###########################################################################

# Effect Size Power Analysis

alpha = 0.01
n_sims = 50

p_vals = numeric(n_sims)

n_effect_sizes = 20
effect_sizes = seq(-.01, .01, length.out = n_effect_sizes)

# The maximum x value in the simulation.
# Use the maximum observed x-value in the data
max_x = max(birdhab$ls)

sample_sizes = seq(10, 50)

sim_output_2 = matrix(nrow = length(effect_sizes), ncol = length(sample_sizes))

for(k in 1:length(effect_sizes))
{
  effect_size = effect_sizes[k]
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, max_x, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_int = int_obs,
        slope = effect_size,
        st_dev = sd_obs
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    sim_output_2[k, j] = sum(p_vals < alpha) / n_sims
  }
  print(paste0("computing effect size ", k," of ", length(effect_sizes)))
}

sim_n_effect_size = 
  list(
    power = sim_output_2,
    effect_size = effect_sizes,
    sample_size = sample_sizes
  )


###########################################################################

# Population Dispersion and Sample Size Analysis

alpha = 0.05

# Start with a small number
n_sims = 30
p_vals = numeric(n_sims)

# What was the observed standard deviation?
sd_obs

# specify the number of different standard deviation values to simulate:
# Start with a small number
n_sds = 20
pop_sds = seq(from = 0.05, to = 1.5, length.out = n_sds)

# The maximum x value in the simulation.
# Use the maximum observed x-value in the data
max_x = max(birdhab$ls)

pop_sd_powers = numeric(length(pop_sds))

sample_sizes = seq(5, 100)

sim_output_3 = matrix(nrow = length(pop_sds), ncol = length(sample_sizes))


for(k in 1:length(pop_sds))
{
  pop_sds_k = pop_sds[k]
  
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, max_x, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_int = int_obs,
        slope = effect_size,
        st_dev = pop_sds_k
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    
    sim_output_3[k, j] = sum(p_vals < alpha) / n_sims
  }
  print(paste0("Testing standard deviation ", k, " of ", n_sds))
}

image(sim_output_3)

sim_3_dat = 
  list(
    power       = sim_output_3,
    sample_size = sample_sizes,
    pop_sd      = pop_sds)

# Adding the LOWESS Model

fit_lowess_30 = loess(power ~ sample_size, data = sim_sample_size, span = 0.3)

newdata_sample_size = data.frame(sample_size = seq(2, 20, length.out = 100)) 

plot(
  x = newdata_sample_size$sample_size,
  y = predict(fit_lowess_30, newdata = newdata_sample_size),
  type = "l",
  ylab = "Statistical Power", xlab = "Sample Size")

points(
  x = sim_3_dat$sample_size, 
  y = linear_simulator(
    x = sim_3_dat$sample_size,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  col = "maroon2",
  pch = 16)

legend(
  "bottomright",
  legend = c("smoothed", "original"),
  pch = 16,
  col = "maroon2")






# Question 7

require(rgl)

n = 50

ba.tot = seq(
  from = min(dat_all$ba.tot, na.rm = T),
  to = max(dat_all$ba.tot, na.rm = T),
  length.out = n)
slope = seq(
  from = min(dat_all$slope, na.rm = T),
  to = max(dat_all$slope, na.rm = T),
  length.out = n)

# I can take advantage of the expand.grid() function to make a data.frame 
# that contains every combination of the two predictors:

new_dat_all = expand.grid(
  ba.tot = ba.tot,
  slope = slope)
head(new_dat_all)

tail(new_dat_all)

new_dat_all$pred_add = predict(
  fit_GCKI_both_additive,
  newdata = new_dat_all,
  type = "response")

z_GCKI_add = matrix(
  new_dat_all$pred_add,
  nrow = length(ba.tot),
  byrow = FALSE)

# Create a color ramp function using heat colors
colorfunc = colorRampPalette(
  heat.colors(length(ba.tot)))

# Figure out the indices of the color ramp
col_indices_GCKI_add = findInterval(
  new_dat_all$pred_add,
  seq(
    min(new_dat_all$pred_add),
    max(new_dat_all$pred_add),
    length.out = 50))
require(htmlwidgets)
saveWidget(
  rglwidget(),
  file = here(
    "data", "GCKI heat plot.html"),
  selfcontained = TRUE
)






