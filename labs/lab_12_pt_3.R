#Lab 12 Part 3

rm(list = ls())

#Packages

require(here)

#Import Data

dat_bird = read.csv(here("data", "bird.sta.csv"))
dat_habitat = read.csv(here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)

# Vector of presence/absence data for hermit warblers

dat_all$HEWA_pres = dat_all$HEWA > 0

# Create model fits
fit_hewa_slope = glm(HEWA_pres ~ slope, data = dat_all, family = binomial)
fit_hewa_ba_tot = glm(HEWA_pres ~ ba.tot, data = dat_all, family = binomial)
fit_hewa_both_additive = glm(HEWA_pres ~ slope + ba.tot, data = dat_all, family = binomial)
fit_hewa_both_interactive = glm(HEWA_pres ~ slope * ba.tot, data = dat_all, family = binomial)

# Summary
summary(fit_hewa_both_additive)

# Plotting a simple fitted logistic model

# We can use the predict() function to predict response values for given sets of predictor values.  
# But… with a logistic model we have to decide whether we want to predict the outcomes (presence/absence) 
# or the probability of success. We’ll go with the probability of success, since it’s what we are really modeling.

# For the slope, I can make a data.frame object that contains a sequence of new slopes. 
# I’ll create a sequence of 500 slopes so that I get a nice smooth curve.

n = 500

slope_newdata = data.frame(
  slope = seq(
    from = min(dat_all$slope, na.rm = T),
    to = max(dat_all$slope, na.rm = T),
    length.out = n
  )
)

ba_newdata = data.frame(
  ba.tot = seq(
    from = min(dat_all$ba.tot, na.rm = T),
    to = max(dat_all$ba.tot, na.rm = T),
    length.out = n
  )
)

# Predicted Data

slope_newdata$hewa_predicted = 
  predict(
    fit_hewa_slope,
    newdata = slope_newdata,
    type = "response"
  )

ba_newdata$hewa_predicted = 
  predict(
    fit_hewa_ba_tot,
    newdata = ba_newdata,
    type = "response"
  )

# Fitting the model

par(mfrow = c(2, 1))

# Presence/absence data, translucent points:
plot(
  HEWA_pres ~ slope, data = dat_all,
  xlab = "Percent Slope",
  ylab = "HEWA presence/absence",
  pch = 16, cex = 1.5, col = gray(0, 0.2)
)

lines(hewa_predicted ~ slope, data = slope_newdata)

plot(
  HEWA_pres ~ ba.tot, data = dat_all,
  xlab = "Basal Area",
  ylab = "HEWA presence/absence",
  pch = 16, cex = 1.5, col = gray(0, 0.2)
)

lines(hewa_predicted ~ ba.tot, data = ba_newdata)

# Plot parameters

AIC(
  fit_hewa_ba_tot,
  fit_hewa_slope,
  fit_hewa_both_additive,
  fit_hewa_both_interactive)

# Data Setup

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

# I can use predict() to make a vector of model predictions:

new_dat_all$pred_add = predict(
  fit_hewa_both_additive,
  newdata = new_dat_all,
  type = "response")


# The contour and 3D plotting methods require matrices for the data on the z-axis:

z_hewa_add = matrix(
  new_dat_all$pred_add,
  nrow = length(ba.tot),
  byrow = FALSE)
z_hewa_int = matrix(
  new_dat_all$pred_int,
  nrow = length(ba.tot),
  byrow = FALSE)

# 3D Plots

require(rgl)

rgl::persp3d(
  x = ba.tot,
  y = slope,
  z = z_hewa_add,
  col = "steelblue",
  xlab = "Basal Area",
  ylab = "Slope",
  zlab = "Pr(present)",
  alpha = 0.4)
rglwidget()

# Contour plots

par(mfrow = c(1, 2))
contour(
  x = ba.tot, y = slope,
  z = z_hewa_add,
  xlab = "Total Basal Area",
  ylab = "Percent Slope",
  main = "Additive")
contour(
  x = ba.tot,
  y = slope,
  z = z_hewa_int,
  xlab = "Total Basal Area",
  ylab = "Percent Slope",
  main = "Interactive")

# Even fancier 3D plots (optional)
# We could make our 3D plots even fancier by color-coding the surface based on the z-value.

# Note: this code is a little clunky, don’t worry if it doesn’t make sense, it’s totally optional,
# but the colors make the difference between the interactive and additive model easier to see.


Copy
# Create a color ramp function using heat colors
colorfunc = colorRampPalette(
  heat.colors(length(ba.tot)))

# Figure out the indices of the color ramp
col_indices_hewa_add = findInterval(
  new_dat_all$pred_add,
  seq(
    min(new_dat_all$pred_add),
    max(new_dat_all$pred_add),
    length.out = 50))

rgl::persp3d(
  x = ba.tot,
  y = slope,
  z = z_hewa_add,
  xlab = "Basal Area",
  ylab = "Slope",
  zlab = "Pr(present)",
  alpha = 0.9,
  col = colorfunc(50)[col_indices_hewa_add]
)
rglwidget()

################################################################################

# Questions 3-5
# Fit Logistic Regression for Golden Crowned Kinglet

# Vector of presence/absence data for GCKI

dat_all$GCKI_pres = dat_all$GCKI > 0

# Create model fits
fit_GCKI_slope = glm(GCKI_pres ~ slope, data = dat_all, family = binomial)
fit_GCKI_ba_tot = glm(GCKI_pres ~ ba.tot, data = dat_all, family = binomial)
fit_GCKI_both_additive = glm(GCKI_pres ~ slope + ba.tot, data = dat_all, family = binomial)
fit_GCKI_both_interactive = glm(GCKI_pres ~ slope * ba.tot, data = dat_all, family = binomial)

# Summary
summary(fit_GCKI_both_interactive)

# Plotting a simple fitted logistic model

# We can use the predict() function to predict response values for given sets of predictor values.  
# But… with a logistic model we have to decide whether we want to predict the outcomes (presence/absence) 
# or the probability of success. We’ll go with the probability of success, since it’s what we are really modeling.

# For the slope, I can make a data.frame object that contains a sequence of new slopes. 
# I’ll create a sequence of 500 slopes so that I get a nice smooth curve.

n = 500

slope_newdata = data.frame(
  slope = seq(
    from = min(dat_all$slope, na.rm = T),
    to = max(dat_all$slope, na.rm = T),
    length.out = n
  )
)

ba_newdata = data.frame(
  ba.tot = seq(
    from = min(dat_all$ba.tot, na.rm = T),
    to = max(dat_all$ba.tot, na.rm = T),
    length.out = n
  )
)

# Predicted Data

slope_newdata$GCKI_predicted = 
  predict(
    fit_GCKI_slope,
    newdata = slope_newdata,
    type = "response"
  )

ba_newdata$GCKI_predicted = 
  predict(
    fit_GCKI_ba_tot,
    newdata = ba_newdata,
    type = "response"
  )

# Fitting the model

par(mfrow = c(2, 1))

# Presence/absence data, translucent points:
plot(
  GCKI_pres ~ slope, data = dat_all,
  xlab = "Percent Slope",
  ylab = "GCKI presence/absence",
  pch = 16, cex = 1.5, col = gray(0, 0.2)
)

lines(GCKI_predicted ~ slope, data = slope_newdata)

plot(
  GCKI_pres ~ ba.tot, data = dat_all,
  xlab = "Basal Area",
  ylab = "GCKI presence/absence",
  pch = 16, cex = 1.5, col = gray(0, 0.2)
)

lines(GCKI_predicted ~ ba.tot, data = ba_newdata)

# Plot parameters

AIC(
  fit_GCKI_ba_tot,
  fit_GCKI_slope,
  fit_GCKI_both_additive,
  fit_GCKI_both_interactive)

# Data Setup

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

# I can use predict() to make a vector of model predictions:

new_dat_all$pred_add = predict(
  fit_GCKI_both_additive,
  newdata = new_dat_all,
  type = "response")


# The contour and 3D plotting methods require matrices for the data on the z-axis:

z_GCKI_add = matrix(
  new_dat_all$pred_add,
  nrow = length(ba.tot),
  byrow = FALSE)
z_GCKI_int = matrix(
  new_dat_all$pred_int,
  nrow = length(ba.tot),
  byrow = FALSE)

# 3D Plots

require(rgl)

rgl::persp3d(
  x = ba.tot,
  y = slope,
  z = z_GCKI_add,
  col = "steelblue",
  xlab = "Basal Area",
  ylab = "Slope",
  zlab = "Pr(present)",
  alpha = 0.4)
rglwidget()

# Contour plots

par(mfrow = c(1, 2))
contour(
  x = ba.tot, y = slope,
  z = z_GCKI_add,
  xlab = "Total Basal Area",
  ylab = "Percent Slope",
  main = "Additive")
contour(
  x = ba.tot,
  y = slope,
  z = z_GCKI_int,
  xlab = "Total Basal Area",
  ylab = "Percent Slope",
  main = "Interactive")

# Even fancier 3D plots (optional)
# We could make our 3D plots even fancier by color-coding the surface based on the z-value.

# Note: this code is a little clunky, don’t worry if it doesn’t make sense, it’s totally optional,
# but the colors make the difference between the interactive and additive model easier to see.


Copy
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

rgl::persp3d(
  x = ba.tot,
  y = slope,
  z = z_GCKI_add,
  xlab = "Basal Area",
  ylab = "Slope",
  zlab = "Pr(present)",
  alpha = 0.9,
  col = colorfunc(50)[col_indices_GCKI_add]
)
rglwidget()














