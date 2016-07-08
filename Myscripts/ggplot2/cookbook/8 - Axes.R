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
p + ylim(0, max(PlantGrowth$weight))
# ylim is shorthand for setting the limits with scale_y_continuous. the same is true for xlim and
# scale_x_continuous. ylim(0, 10) is equivalent to scale_y_continuous(limits = c(0, 10))
# If we need somthing else in scale_y_continuous, don't use ylim()
# There are 2 ways of setting limits, modifying scale and applying a coordinate transform
# Modifying scale means that the data will be completely removed from consideration
# With a coordinate transform, the data is not clipped

p + scale_y_continuous(limits = c(5, 6.5)) # same as using ylim; gives a boxplot of only the selected data
p + coord_cartesian(ylim = c(5, 6.5)) # just zooming in on the graph

# It is also possible to expand the range in one direction 
p + expand_limits(y = 0) # this cannot be used to shrink the limits though

### Reversing a continuous axis
# use scale_x_reverse or scale_y_reverse; the direction can also be reversed by specifying the 
# limits in the reverse order
ggplot(PlantGrowth, aes(group, weight)) + geom_boxplot() + scale_y_reverse()

# Similar effect by specifying the limits in reverse order
ggplot(PlantGrowth, aes(group, weight)) + geom_boxplot() + ylim(6.5, 3.5)
# like scale_y_continuous, scale_y_reverse does not work well with ylim
# so to reverse the axis and set a range, we must do it within scale_y_reverse


### Changing the order of items on a categorical axis
# For a categorical axis(one with a factor mapped to it), the order can be changed by setting
# limits in scale_y_discrete or scale_x_discrete. To manually set the order of items 


