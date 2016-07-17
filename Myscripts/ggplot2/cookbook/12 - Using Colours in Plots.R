rm(list = ls())
library(ggplot2)
library(gcookbook)
library(MASS)
library(data.table)
library(plyr)
library(scales)

### Setting the colours of an object
# in the call to the geom, set the values of colour or fill

ggplot(mtcars, aes(wt, mpg)) + geom_point(colour = "red")

ggplot(birthwt, aes(bwt)) + geom_histogram(fill = "red", colour = "black")

# In ggplot2, there is a difference between setting and mapping aesthetic properties
# Generally, colour controls the fill and the outline of polygon and fill controls
# the area of the polygon, except for certain point shaped which do have an outline
# as well as fill




### Mapping variables to colour
# map the variable to fill/colour either in the call to the geom or in ggplot
ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) + 
  geom_bar(colour = "black", position = "dodge", stat = "identity")

ggplot(mtcars, aes(wt, mpg, colour = cyl)) + geom_point()
# the legend treats cyl as a continuous variable. We have to change these to factors

ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point()
# another method is to convert the variable to factor in the data




### Using a different palette for a discrete variable
# 








