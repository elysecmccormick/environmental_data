#In Class Activity Week 3

#Install here package

install.packages("here")
require(here)

# Read in Data

dat_catrate = read.csv(
  here("data", "catrate.csv")
)
head(dat_catrate)

dat_delomys = read.csv(
  here("data", "delomys.csv")
)
head(dat_delomys)

dat_rope = read.csv(
  here("data", "rope.csv")
)
head(dat_rope)

install.packages("psych")
require(psych)

plot(x = dat_delomys$body_length, xlab = "Body Length",
     y = dat_delomys$body_mass, ylab = "Body Mass",
     main = "D. dorsalis Body Length to Body Mass Elyse McCormick",
     col = adjustcolor(rgb(0.3,0.9,0.4), alpha.f = 0.2), pch = 16, cex = 2)

boxplot(body_length ~ sex, data = dat_delomys, 
        main = "Sex Differences in Body Length Elyse McCormick")

coplot(body_mass ~ body_length | sex, data = dat_delomys
       main = "Sex Differences in Body Mass and Size Elyse")




