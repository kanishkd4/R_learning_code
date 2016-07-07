rm(list = ls())
library(ggplot2)
library(gcookbook)
library(MASS)
library(data.table)
library(plyr)

# Finally, fine tuning the appearance of axes

### Swapping x and y axes
# use coord_flip()
ggplot(PlantGrowth, aes(group, weight)) + geom_boxplot()
ggplot(PlantGrowth, aes(group, weight)) + geom_boxplot() + coord_flip()

# For a scatter plot, we can just change the x and y and we'll be good but not all geoms treat
# the x and y axis equally eg box plots summarize the data along the y axis, the lines in line 
# graphs move along the x axis, error bars have a range of y values and a single x value
# For these geoms, we need coord_flip for them to behave as if the axes have swapped
# Sometimes, when we swap axes, the order of the items will reverse
# left to right can be changes to top to bottom. this may or may not be a problem
# If x is a factor, then the order can be set as follows

ggplot(PlantGrowth, aes(group, weight)) + geom_boxplot() + coord_flip() +
  scale_x_discrete(limits = rev(levels(PlantGrowth$group)))


### Setting the range of a continuous axis
# use xlim or ylim
p <- ggplot(PlantGrowth, aes(group, weight)) + geom_boxplot()
p





