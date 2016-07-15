rm(list = ls())
library(ggplot2)
library(gcookbook)
library(MASS)
library(data.table)
library(plyr)
library(scales)

# It's really great to be able to render groups of date next to each other and analyse
# These kind of displays are known as Trellis displays and in ggplot2 called facets

### Splitting data into subplots with facets
# use facet_grid or facet_wrap
# With facet_grid, you can specify a variable to split the data into vertical subpanels and another
# variable to split it into horizontal subpanels
mpg <- data.table(mpg) # checking if ggplot works with data.table
p <- ggplot(mpg, aes(displ, hwy)) + geom_point()

# Faceted by drv in vertically arranged subpanels
p + facet_grid(drv ~ .)

# Faceted by cyl, in horizontally arranged subpanels
p + facet_grid(. ~ cyl)

# Split by drv(vertical) and cyl(horizontal)
p + facet_grid(drv ~ cyl)

# With facet_wrap, subplots are laid out horizontally and wrap around, like words on a page
p + facet_wrap( ~ class)
# the default in facet_wrap is to use the same number of rows and columns; this can be changed by
# using nrow or ncol

p + facet_wrap( ~ class, nrow = 2)
p + facet_wrap( ~ class, ncol = 4) # both will give the same plot





### Using facets with different axis
# we want subplots with different ranges or items on their axis
# set the scales to free_x, free_y, or free
p <- ggplot(mpg, aes(displ, hwy)) + geom_point()

# with free y scales
p + facet_grid(drv ~ cyl, scales = "free_y") # the y axis changes as per the data 

# with free x scales
p + facet_grid(drv ~ cyl, scales = "free_x")

# with free x and y scales
p + facet_grid(drv ~ cyl, scales = "free") # no difference in size of the graphs

# It is not possible to set the ranges of individual graphs but it can be set by using a 
# subset of the data



### Changing the text of facet labels 
# change the names of the factor levels
mpg2 <- mpg
mpg2[, drv := ifelse(drv == "4", "4wd",
                     ifelse(drv == "f", "front", "Rear"))]
ggplot(mpg2, aes(displ, hwy)) + geom_point() + facet_grid(drv ~ .)

# Unlike scales where we can set the labels, with facets we need to change the data
# There is no way to show the name of the faceting variable as a header for the facets so it 
# is a good idea to use descriptive facet labels
# With facet_grid, it is also possible to use a labeller function to set the labels

ggplot(mpg2, aes(displ, hwy)) + geom_point() + facet_grid(drv ~ ., labeller = label_both)

# Another useful labeller is label_parsed, which takes strings and treats them as R math expressions

mpg3 <- mpg
mpg3[, drv := ifelse(drv == "4", "4^{wd}",
                     ifelse(drv == "f", "- Front %.% e^{pi * i}", "4^{wd} - Front"))]
ggplot(mpg3, aes(displ, hwy)) + geom_point() + facet_grid(drv ~ ., labeller = label_parsed)

# If the faceting variable is not a factor but a character, changing the values is somewhat different




### Changing the appearance of facet labels and headers
# With the theming system, set strip.text to control the appearance and 
# strip.background to change the background
ggplot(cabbage_exp, aes(Cultivar, Weight)) + geom_bar(stat = "identity") +
  facet_grid(. ~ Date) + theme(strip.text = element_text(face = "bold", size = rel(1.5)),
                               strip.background = element_rect(fill = "lightblue", colour = "black",
                                                               size = 1))
