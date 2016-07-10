rm(list = ls())
library(ggplot2)
library(gcookbook)
library(MASS)
library(data.table)
library(plyr)
library(scales)

# The grammar of graphics is about how data is displayed; it does not give a fuck about appearance

### Setting the title of a graph
# set title with ggtitle()
p <- ggplot(heightweight, aes(ageYear, heightIn)) + geom_point()
p + ggtitle("Age + height of school children")
p + ggtitle("Age + height\nof school children")

# ggtitle is equivalent of using labs(title = "")
# to move the title, we can use a negetive vjust in ggtitle but that leaves blank space at the 
# top. a better method is to use a text annotation by setting the x positiont to the middle of
# the range and y to Inf

p + annotate("text", x = mean(range(heightweight$ageYear)), y = Inf,
             label = "Age and Height of school children", vjust = 1.5, size = 6)


### Change the appearance of text
# we can use theme() and set the item we want to change to element_text()
p <- ggplot(heightweight, aes(ageYear, heightIn)) + geom_point()
p + theme(axis.title.x = element_text(size = 16, lineheight = .9, family = "times",
                                      face = "bold.italic", colour = "red"))

p + ggtitle("Age and Height of children") +
  theme(plot.title = element_text(size = rel(1.5), lineheight = .9, family = "times",
                                  face = "bold.italic", colour = "red")) 
# size is 1.5 times bigger than base font of the plot

p + annotate("text", x = 15, y = 53, label = "Some text", size = 7, family = "times",
             fontface = "bold.italic", colour = "red")
p + geom_text(aes(label = weightLb), size = 4, family = "times", colour = "red")
# there are theme elements and text geoms in ggplot2. geoms are a part of the data itself

# Element name Description
# axis.title Appearance of axis labels on both axes
# axis.title.x Appearance of x-axis label
# axis.title.y Appearance of y-axis label
# axis.ticks
# Appearance of tick labels on both axes
# axis.ticks.x Appearance of x tick labels
# axis.ticks.y Appearance of y tick labels
# legend.title Appearance of legend title
# legend.text Appearance of legend items
# plot.title Appearance of overall plot title
# strip.text Appearance of facet labels in both directions
# strip.text.x Appearance of horizontal facet labels
# strip.text.y Appearance of vertical facet labels



### Using themes
# To use a premade theme, use theme_bw or theme_grey
p <- ggplot(heightweight, aes(ageYear, heightIn)) + geom_point()

# grey theme(default)
p + theme_grey()

# Black and white theme
p + theme_bw()

# We can create our own themes as well
p + theme_grey(base_size = 16, base_family = "Times")

# The default theme for the R session can be set using theme_set()



### Changing the appearance of theme elements
p <- ggplot(heightweight, aes(ageYear, heightIn, colour = sex)) + geom_point()

# Formatting options for the plotting area
p + theme(
  panel.grid.major = element_line(colour = "red"),
  panel.grid.minor = element_line(colour = "red", linetype = "dashed", size = .2),
  panel.background = element_rect(fill = "lightblue"),
  panel.border = element_rect(colour = "blue", fill = NA, size = 2)
)

# options for the text items
p + ggtitle("Plot title here") +
  theme(
    axis.title.x = element_text(colour = "red", size = 14),
    axis.text.x = element_text(colour = "blue"),
    axis.title.y = element_text(colour = "red", size = 14, angle = 90),
    axis.text.y = element_text(colour = "blue"),
    plot.title = element_text(colour = "red", size = 20, face = "bold")
  )

# options for the legend
p + theme(
  legend.background = element_rect(fill = "grey85", colour = "red", size = 1),
  legend.title = element_text(colour = "blue", face = "bold", size = 14),
  legend.text = element_text(colour = "red"),
  legend.key = element_rect(colour = "blue", size = .25)
)

# Options for facets
p + facet_grid(sex ~ .) + theme(
  strip.background = element_rect(fill = "pink"),
  strip.text.y = element_text(size = 14, angle = -90, face = "bold")
) # strip.text.x is for horizontal facets
# way more details on theme elements in the book




### Creating your own themess
# we can create our own themes by adding elements to existing themes
my_theme <- theme_bw() + theme(
  text = element_text(colour = "red"),
  axis.title = element_text(size = rel(1.25))
)

p + my_theme


### Hiding gridlines
p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

# it is possible to hide just the horizontal or vertical gridlines
p + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) 
