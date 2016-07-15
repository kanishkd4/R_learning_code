rm(list = ls())
library(ggplot2)
library(gcookbook)
library(MASS)
library(data.table)
library(plyr)
library(scales)

### Removing the legend
# use guides and specify the scales that will have the legend removed

p <- ggplot(PlantGrowth, aes(group, weight, fill = group)) + geom_boxplot()
p
p + guides(fill = F)

# Another way to remove the legend is to set guide = F in the scale
p + scale_fill_discrete(guide = F) # as the variable is mapped to fill
# yet another way is to use the theming system
p + theme(legend.position = "none")

# other scales that can be used with aesthetics are given in the book



### Changing the position of a legend
# use theme(legend.position = ...)
p <- ggplot(PlantGrowth, aes(group, weight, fill = group)) + geom_boxplot() +
  scale_fill_brewer(palette = "Pastel2")
p + theme(legend.position = "top")

p + theme(legend.position = c(.5, .65)) # can place legend inside a table by using a coordinate 

# legend.justification can be used to decide which part of the legend will get be set to the position by
# legend.position
p + theme(legend.position = c(1, 0), legend.justification = c(1, 0))
p + theme(legend.position = c(1, 1), legend.justification = c(1, 1))

# when placing the legend inside the plot, it may be useful to add an opaque border
p + theme(legend.position = c(.85, .2)) + theme(legend.background = element_rect(fill = "white",
                                                                                 colour = "black"))
# We can also remove the border around its elements so that it blends in 
p + theme(legend.position = c(.85, .2)) + theme(legend.background = element_blank()) +
  theme(legend.key = element_blank())



### Changing the order of items in the legend
# set the limits in the scale to the desired order
p <- ggplot(PlantGrowth, aes(group, weight, fill = group)) + geom_boxplot()
p
p + scale_fill_discrete(limits = c("trt1", "trt2", "ctrl"))
# the order of the items on the x axis does not change. To do that, we would have to change the 
# limits of scale_x_discrete or change the data to have a different factor level order
# scale_fill_discrete is the same as scale_fill_hue and maps the factor levels to colours that are
# spaced equally well around the colour wheel. we can use a different scale_fill_xxxx

p + scale_fill_grey(start = .5, end = 1, limits = c("trt1", "trt2", "ctrl"))
p + scale_fill_brewer(palette = "Pastel2", limits = c("trt1", "trt2", "ctrl")) # we can also use other palettes



### Reversing the order of items in a legend
# add guides(fill = guide_legend(reverse = T)); for other aesthetics, replace fill with the name
# of the aesthetic
p + guides(fill = guide_legend(reverse = T))



### Changing a legend title
# use labs() and set the value of fill, colour, shape, or whatever aesthetic is applicable 
# for the legend
p + labs(fill = "Condition")

# It is also possible to set the legend title in the in the scale justification
# Since legend and axis are both guides, it works in the same way
p + scale_fill_discrete(name = "Condition")

# If there are multiple variables mapped to aesthetics, we can name each individually
hw <- ggplot(heightweight, aes(ageYear, heightIn, colour = "sex")) +
  geom_point(aes(size = weightLb)) + scale_size_continuous(range = c(1, 4))
hw
# With new legend titles
hw + labs(colour = "Male/Female", size = "Weight\n(pounds)")

# If you have just one variable mapped to two separate aesthetics, the default is to have a single
# legend that combines both 

hw1 <- ggplot(heightweight, aes(ageYear, heightIn, shape = sex, colour = sex)) + geom_point()
hw1
# To change the title, you need to change the title for both of them; if we change the name for
# just one, it will result in two different legends



### Changing the appearance of a legend title
# use theme(legend.title = element_text())
p <- ggplot(PlantGrowth, aes(group, weight, fill = group)) + geom_boxplot()
p + theme(legend.title = element_text(face = "italic", family = "Times", colour = "red", size = 14))

# It is also possible to specify the legend's appearance via guides() but this method can be
# a bit verbose
p + guides(fill = guide_legend(title.theme = element_text(face = "italic", family = "times", colour = "red", size = 14,
                                                          angle = 0)))




### Removing a legend title
# add guides(fill = guide_legend(title = NULL)); change fill as per the aesthetic used

ggplot(PlantGrowth, aes(group, weight, fill = group)) + geom_boxplot() +
  guides(fill = guide_legend(title = NULL))
# It is possible to set the legend title while specifying the scale
ggplot(PlantGrowth, aes(group, weight, fill = group)) + geom_boxplot() +
  scale_fill_hue(guide = guide_legend(title = NULL))



### Changing the labels in a legend
# Set the labels in the scale
p <- ggplot(PlantGrowth, aes(group, weight, fill = group)) + geom_boxplot()
p + scale_fill_discrete(labels = c("Control", "Treatment 1", "Treatment 2"))

# The labels of the x axis don't change just like the labels of the labels of the legend
# don't change when we change the axis. To change that, change labels in scale_x_discrete
# Other fill scales change the labels the same way for a variable mapped to fill
p + scale_fill_grey(start = .5, end = 1, labels = c("Control", "Treatment 1", "Treatment 2"))

# Labels are matched by position so if we are changing the order of the items,
# we need to set the labels in the same order

p + scale_fill_discrete(limits=c("trt1", "trt2", "ctrl"),
                        labels=c("Treatment 1", "Treatment 2", "Control"))

# If we have one variable mapped to two separate aesthetics, the default is one legend
# Here also, we have to change the labels for both scales
p <- ggplot(heightweight, aes(ageYear, heightIn, shape = sex, colour = sex)) + geom_point()
p
# chaging labels for one scale
p + scale_shape_discrete(labels = c("F", "M"))
# changing labels for both scales
p + scale_shape_discrete(labels = c("F", "M")) + scale_colour_discrete(labels = c("F", "M"))



### Changing appearance of legend labels
# use theme(legend.text = element_text())
p <- ggplot(PlantGrowth, aes(group, weight, fill = group)) + geom_boxplot()

# Change the legend label appearance
p + theme(legend.text = element_text(face = "italic", family = "Zurich",
                                     colour = "red", size = 14))
# It is also possible to specify the legend label appearance by using guides
p + guides(fill = guide_legend(label.theme = element_text(face = "italic", family = "Zurich",
                                                          colour = "red", size = 14, angle = 0)))




### Using labels with multiple lines of text
# use \n 
p + scale_fill_discrete(labels = c("Control", "Type 1\ntreatment", "Type 2\ntreatment"))
# it's overlapping so we need to increase the height of the legend keys and decrease the 
# spacing between lines using theme by using the unit() function from the package grid  

library(grid)
p + scale_fill_discrete(labels = c("Control", "Type 1\ntreatment", "Type 2\ntreatment")) +
  theme(legend.text = element_text(lineheight = .8),
        legend.key.height = unit(1, "cm"))


