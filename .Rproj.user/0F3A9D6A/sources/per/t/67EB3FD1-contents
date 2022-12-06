#Lab 11: Simulation and Power Analysis

# Clear R environment

rm(list = ls())

# Packages

require(here)

# Read in data

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

# Dimensions of the dataframe

dim(birdhab)

# Plot

plot(birdhab$ls, birdhab$BRCR, xlab = "Late-successional forest type", ylab = "Brown Creeper Abundance", 
     pch = 16, col = "lightsalmon3")

# Linear regression

fit_1 = lm(formula = BRCR ~ ls, data = birdhab)

# This is how you get the model coefficient table, not just by calling the model alone
summary(fit_1)

#fits the regression line to the plot
abline(fit_1)

#######################################################################

# Deterministic Model: Linear Function

# Linear function with random points for x and y


linear = function(x, y_int, slope)
{
      return(y_int + x * slope)
}


# Test for the linear function

linear(x = 1, y_int = 1, slope = 1)
linear(x = 3:5, y_int = 1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 0.01)


######################################################################

# Simulation Function 
# y is the linear deterministic function that just got made in the last section
# n is an arbitrary sample size that shows how many x values need to go with a 
#   corresponding y value.If you added real numbers, it would populate how many
#   x values are needed for the length of the line. This is kind of abstract, 
#   but it can be helpful when writing functions to add actual numbers to test how the function works.
# noise is the stochastic portion of the model which operates around a mean of 0
# and you can populate the sd with whatever st_dev you determined from the function of the model.
# Then, you add the deterministic 'y' with the stochastic 'noise' to create the simulator function that
# includes both

linear_simulator = function(x, y_int, slope, st_dev)
{
  y <- linear(x, y_int, slope)
  n <- length(x)
  noise <- rnorm(n, mean = 0, sd = st_dev)
  
    return(y + noise)
}

# Test for the simulator function

n = 200

par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x,
    linear_simulator(x, y_int = 1, slope = 4.5, st_dev = 0.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2),
    axes = FALSE)
  box()
}

n = 400

par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x, linear_simulator(x, y_int = 10, slope = -6.5, st_dev = 1.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2),
    axes = FALSE)
  box()
}

#############################################################################

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

# Simulate Data

plot(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  main = "Simulated Data",
  xlab = "late-successional forest",
  ylab = "Brown Creeper Abundance")

# Plot observed data first, then add the simulated data

plot(
  birdhab$ls, birdhab$BRCR, 
  xlab = "late-successional forest extent",
  ylab = "Brown Creeper abundance",
  pch = 19)

points(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  col = adjustcolor("red", alpha = 0.3),
  pch = 16)

legend(
  "topleft",
  legend = c("data", "simulation"),
  pch = 16,
  col = c(1, adjustcolor("red", alpha = 0.3)))

###############################################################################

# Starting the power analysis

# Single Simulation: Can we reject the null in one experiment?

y_sim = linear_simulator(
  x = birdhab$ls,
  y_int = int_obs,
  slope = slope_obs,
  st_dev = sd_obs
)

fit_sim = lm(y_sim ~ birdhab$ls)
summary(fit_sim)

#Coefficient matrix

sum_1 = summary(fit_sim)
sum_1$coefficients

###############################################################################

# Repeated Simulations

n_sims = 1000
p_vals = numeric(n_sims)
alpha = 0.05
for(i in 1:n_sims)
{
  y_sim = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  )
  fit_sim = lm(y_sim ~ birdhab$ls)
  
  p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
}
sum(p_vals < alpha) / n_sims

# Quick function to simplify the simulation loops and save time

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

# Running the full repeated simulation.Note that this code is just an elaboration
# of the code we used to perform the simulation above:

#    We created a sequence of effect sizes to try.
#    We nested the original code inside an outer loop that iterates over each of the effect sizes.
#    Instead of saving p-values directly, we saved a vector of statistical powers:
#        The p-values were calculated in the inner loop for a specific effect size.
#    The statistical powers were calculated in the outer loop and stored in effect_size_powers.
#    We saved the results as a data.frame with columns for effect size and statistical power.

alpha = 0.05
n_sims = 100
p_vals = numeric(n_sims)

n_effect_sizes = 20
effect_sizes_1 = seq(-.01, .01, length.out = n_effect_sizes)

effect_size_powers = numeric(n_effect_sizes)

for(j in 1:n_effect_sizes)
{
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = birdhab$ls,
      y_int = int_obs,
      slope = effect_sizes_1[j],
      st_dev = sd_obs
    )
    
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  effect_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_effect_size = 
  data.frame(
    effect_size = effect_sizes_1,
    power       = effect_size_powers)

# We can plot the result and add a vertical line to show the slope of our original data set.

plot(
  power ~ effect_size, data = sim_effect_size,
  type = 'l', xlab = 'Effect size', ylab = 'Power')
abline(v = slope_obs, lty = 2, col = 'red')

##############################################################################

# We can use the same approach to determine the power of different sample sizes

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


# Plotting this in the same way we plotted the power analysis from repeated simulations

plot(
  power ~ sample_size, data = sim_sample_size,
  type = 'l', xlab = 'Sample size', ylab = 'Power')
abline(v = nrow(birdhab), lty = 2, col = 'red')

##############################################################################

# Effect Size AND Sample Size in a Power Analysis

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

# Note, the only difference in this code is that we added a third outer loop 
# and created a matrix to store the results, since we have a power result for 
# each combination of slope and sample size

# Our output power data are now stored in a matrix (instead of a vector),
# so we need a way to visualize 2-dimensional gridded data, i.e. raster data

# Packaged the output power matrix, the effect sizes, and the sample sizes into a list for future use

###########################################################################

# Plotting a Matrix

image(
  sim_n_effect_size$power,
  xlab = "Effect size",
  ylab = "Sample Size",
  axes = FALSE)

# add x-axis labels
axis(
  1, 
  at = c(0, 0.5, 1), 
  labels = c(-.01, 0.0, .01))

# add y=axis labels
axis(
  2, 
  at = c(0, 1), 
  labels = c(sample_sizes[1], tail(sample_sizes, 1)))

#############################################################################

# Plotting 3D Data

# Contour plotting is like making a topographical map of your data

# Contour plot
contour(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "effect size",
  ylab = "sample size",
  main = "Contour Plot of Statistical Power",
  levels = seq(0, 1, length.out = 9),
  drawlabels = TRUE,
  # method = "simple")
  method = "edge")

# Perspective plot
persp(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "beta", ylab = "n", zlab = "power",
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

# Interactive plots! 

install.packages("rgl")
require("rgl")

persp3d(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "beta", ylab = "n", zlab = "power",
  col = 'lightblue',
  theta = 30, phi = 30, expand = 0.75,
  ticktype = 'detailed')

#Save the plot

require(htmlwidgets)
saveWidget(
  rglwidget(),
  file = here(
    "data", "n_effect_size_power_sim_plot.html"),
  selfcontained = TRUE
)

##############################################################################

# Saving R Data Simulations and Objects

save(
  sim_n_effect_size,
  file = here::here("data", "lab_11_n_effect_sizes.Rdata"))
load(file = here::here("data", "lab_11_n_effect_sizes.Rdata"))

##############################################################################

# DISPERSION SIMULATIONS

alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)

# What was the observed standard deviation?
sd_obs

# specify the number of different standard deviation values to simulate:
n_sds = 20
pop_sds = seq(from = 0.01, to = 1.5, length.out = n_sds)

pop_sd_powers = numeric(n_sds)

for(j in 1:n_sds)
{
    for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(x = birdhab$ls,
                             y_int = int_obs,
                             slope = slope_obs,
                             st_dev = sd_obs)
    p_vals[i] = summary(fit_sim)$coefficients[2, "Pr(>|t|)"]
  }
  pop_sd_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_output_dispersion = data.frame(
  sd = pop_sds,
  power = pop_sd_powers)

# You should save your simulation results so you don't have to run it every time.
save(
  sim_output_dispersion, 
  file = here::here("data", "lab_ll_dat_dispersion_sim.RData"))

# Line plot of standard deviation (x-axis) and statistical power (y-axis)
# Add a dotted vertical red line at the observed population standard deviation value.

plot(power ~ sd, data = sim_output_dispersion,
     type = 'l', xlab = 'Dispersion', ylab = 'Power')
abline(v = nrow(birdhab), lty = 2, col = 'red')

#############################################################################

# Population Dispersion and Sample Size Analysis

alpha = 0.05

# Start with a small number
n_sims = 10
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

sim_output_3 = matrix(numeric(nrow = length(pop_sds),ncol = length(sample_sizes)))

for(k in 1:length(pop_sds))
{
  pop_sd_k = pop_sds[k]
  
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


# You should save your simulation results so you don't have to run it every time.
save(
  sim_3_dat, 
  file = here::here("data", "lab_ll_sim_output_dispersion_n_1000.RData"))
















