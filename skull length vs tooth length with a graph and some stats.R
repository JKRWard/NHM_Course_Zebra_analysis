# looking for allometry
# does tooth area increase at the same rate as skull area

library(ggplot2)
library(tidyverse)
library(stringr)

#zebra graphs and analysis
zebra <- read_csv("2017-05-15_zebra-collection-data.csv")
glimpse(zebra)

#--------------------------------
# plot a graphics
ggplot(zebra, aes(skull_length , tooth_p1_length)) +
  geom_point(size = 3 , colour = "coral")

#-----------------------------
# making the model

model1 <- lm(tooth_p1_length ~ skull_length, data = zebra)


# check the assumptions
autoplot(model1, smooth.colour = NA)


anova(model1)
summary(model1)
# yes, skull length does have a significat on tooth length p< 0.001, F = 18.174 dF= 25

#------------------------------------
# plotting graph looking for allometry

newX <- expand.grid(skull_length = seq(from = 500, to = 624, length = 100))

# Look at newX
head(newX)


newY <- predict(model1, newdata = newX, interval = "confidence")

# Look at newY
head(newY)

addThese <- data.frame(newX, newY)

# Look at addThese
head(addThese)

addThese <- rename(addThese, tooth_p1_length = fit)

# Look at addThese
head(addThese)


ggplot(zebra, aes(x = skull_length, y = tooth_p1_length)) + 
  geom_point(col = "coral", size = 3) +
  labs(x = "Skull length (mm)", y = "Length of first pre molar (mm)") +
  theme_bw() +
  geom_smooth(data = addThese, aes(ymin = lwr, ymax = upr), stat = "identity")
#---------------------------------------------------------
# line has a gradient of 1:1 , tooth length increases proprtioanly with skull length
#------------------------------------------------------