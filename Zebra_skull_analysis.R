# Analysing zebra data 
library(tidyverse)
library(ggfortify)

# Reading in the data
rawd <- read_csv("2017-05-15_zebra-collection-data.csv")
# Checking the data
tbl_df(rawd)
glimpse(rawd)
# Plotting the sull length against skull width
pl <- ggplot(data = rawd)+
geom_point(mapping = aes(x= skull_length, y = skull_width, colour= species)) +
  theme_classic()
pl

# Adding sex to the plot by coding the shape to differ with sex 
pl1 <-
  ggplot(data = bur, aes(x = skull_length, y = skull_width, colour = species)) +
  geom_point(aes(shape = sex)) + theme_classic()
pl1

# Creating a boxplot to look at the variation in skull length in the different species 
box <- ggplot(rawd) +
  geom_boxplot(aes(species, skull_length)) +
  coord_flip()

# now that the skull length width data has been plotted we can add a model 
# adding a simple regression model 
zmodel1 <- lm(skull_length ~ skull_width, data = rawd)
#looking at the residuals of the model 
autoplot(zmodel1, smooth.colour = NA)
# looking at the output/ results of the model, then lookin in more detail
anova(zmodel1)
summary(zmodel1)

# adding species as a covariate 

zmodel2 <- lm(skull_length~ tooth_p1_length *species, data = rawd)
autoplot(zmodel2, smooth.colour = NA)

anova(zmodel2)
summary(zmodel2)

#########################################################
# looking only at the burcelli species, is there variation within the four
# subspecies that we were looking at?
bur <- rawd %>% filter(!species =="Equus_greyvi")
ggplot(bur) +
  geom_boxplot(aes(species, skull_length)) +
  coord_flip()

# selecting only the skull length and skull width to look at 
bur_skull <- bur %>% select(skull_length,skull_width, species)

#plot the data
ggplot(data = bur_skull, aes(x = skull_length, y = skull_width, colour = species)) +
  geom_point(aes()) + theme_classic()

# using a  regression model
zmodel1 <- lm(skull_length ~ skull_width, data = rawd)
autoplot(zmodel1, smooth.colour = NA)

#look at the results 
anova(zmodel1)
summary(zmodel1)

# can we use skull length to predict skull width 
# create new x and new y 

#this is a change that I am making but it is wrong 

#this is a change that I want to keep 