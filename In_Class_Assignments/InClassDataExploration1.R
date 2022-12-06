# In class data exploration 1
  
install.packages("palmerpenguins")
install.packages("here")
require(palmerpenguins)
require(here)

class(penguins)

penguins = data.frame(penguins)

mean(penguins$body_mass_g)

head(penguins)
help("mean")
mean(penguins, trim = 0, na.rm = TRUE)
summary(penguins)

plot(x = penguins$bill_length_mm, y = penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)
par(mfrow = c(1, 2))
boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)

coplot(body_mass_g ~ bill_depth_mm | sex, data = penguins)
