install.packages("psych")
require(psych)

install.packages("here")
require(here)

dat_bird = read.csv(
  here("data", "bird.sta.csv")
)
head(dat_bird)

dat_habitat = read.csv(
  here("data", "hab.sta.csv")
)
head(dat_habitat)

pairs.panels(dat_habitat[, c("elev", "slope", "ba.con", "ba.tot")] )

hist(dat_bird$CEWA, xlab = "Number of Birds Counted", breaks = 0:7 - 0.5, main = "Cedar Waxwing Abundance")
max(dat_bird$CEWA)
