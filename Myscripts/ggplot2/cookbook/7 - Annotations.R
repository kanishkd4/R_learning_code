library(ggplot2)
library(gcookbook)
library(MASS)
library(data.table)
library(plyr)

### Adding text annotations
# use annotate and a text geom
p <- ggplot(faithful, aes(eruptions, waiting)) + geom_point()
p + annotate("text", x = 3, y = 48, label = "Group1") + annotate("text", x = 4.5, y = 66, label = "Group2")
# annotate can be used to add any type of geometric object; used to add text above

p + annotate("text", x = 3, y = 48, label = "Group1", family = "serif", fontface = "italic", colour = "darkred", size = 3) + 
  annotate("text", x = 4.5, y = 66, label = "Group2", family = "serif", fontface = "italic", colour = "darkred", size = 3)

# Don't use geom_text while adding individual text objects; while annotate will add a single text, geom_text will
# add multiple text objects depending on the data

p + annotate("text", x = 3, y = 48, label = "Group1", alpha = .1) +
  geom_text(x = 4.5, y = 66, label = "Group2", alpha = .1) # even at 90% transparent, it is clear (overplotted)

p + annotate("text", x = -Inf, y = Inf, label = "Upper left", hjust = -.2, vjust= 2) +
  annotate("text", x = mean(range(faithful$eruptions)), y = -Inf, vjust = -.4, label = "Bottom Middle")


### Using mathematical expressions in annotations
# use annotate(geom = "text") and set parse = T
# A normal curve

p <- ggplot(data.frame(x = c(-3, 3)), aes(x)) + stat_function(fun = dnorm)
p + annotate("text", x = 2, y = .3, parse = T, label = "frac(1, sqrt(2 * pi)) * e^{-x^2/2}") #FUCK!

# Mathematical expressions made with text geoms using parse = T have a format similar to those made with plotmath
# and expressions in base R except that they are stored as strings rather than expression objects

### Adding lines
# use geom_hline and geom_vline for horizontal and vertical lines; for angled lines, use geom_abline()
p <- ggplot(heightweight, aes(ageYear, heightIn, colour = sex)) + geom_point()

# add horizontal and vertical lines
p + geom_hline(yintercept = 60) + geom_vline(xintercept = 14)

p + geom_abline(intercept = 37.4, slope = 1.75)
# It is also possible to map values from the data to xintercept and yintercept

hw_means <- ddply(heightweight, "sex", summarise, heightIn = mean(heightIn))
hw_means
p + geom_hline(aes(yintercept = heightIn, colour = sex), data = hw_means, linetype = "dashed", size = 1)

pg <- ggplot(PlantGrowth, aes(group, weight)) + geom_point()
pg + geom_vline(xintercept = 2)
pg + geom_vline(xintercept = which(levels(PlantGrowth$group) == "ctrl"))# line through x that is a factor

# In future versions of ggplot2, annotate will also work with lines


### Adding line segments and arrows
# Use annotate("segment")

p <- ggplot(subset(climate, Source == "Berkeley"), aes(Year, Anomaly10y)) + geom_line()
p + annotate("segment", x = 1950, xend = 1980, y = -.25, yend = -.25)

# It's possible to add arrows to the ends of the line segment using arrow() from the grid package
library(grid)
p + annotate("segment", x = 1850, xend = 1820, y = -.8, yend = -.95, colour = "blue", size = 2, arrow = arrow()) +
  annotate("segment", x = 1950, xend = 1980, y = -.25, yend = -.25, arrow = arrow(ends = "both", angle = 90, 
                                                                                  length = unit(.2, "cm")))
# If one or both axis are discrete, then x and y positions are such that the categorical items have coordinate values 1, 2, 3 and so on



### Adding a shaded rectangle
# use annotate("rect")

p <- ggplot(subset(climate, Source == "Berkeley"), aes(Year, Anomaly10y)) + geom_line()
p + annotate("rect", xmin = 1950, xmax = 1980, ymin = -1, ymax = 1, alpha = .1, fill = "blue")
# Any geom can be used with annotate, in this case, it is geom_rect()

### Highlighting an item
# To highlight one or more columns, add a new column in the data and map it to colour

pg <- PlantGrowth
pg$hl <- "no"
pg$hl[pg$group == "trt2"] <- "yes"
ggplot(pg, aes(group, weight, fill = hl)) + geom_boxplot() + scale_fill_manual(values = c("grey85", "#FFDDCC"),
                                                                               guide = F)
# For a small number of items, instead of creating a new column, we can use the original one and specify the colours 
# for every level of the variable
ggplot(PlantGrowth, aes(group, weight, fill = group)) + geom_boxplot() + scale_fill_manual(values = c("grey85", "grey85", "#FFDDCC"),
                                                                                           guide = F)


### Adding error bars
# use geom_errorbar and map variables to the values for ymin and ymax. Adding error bars is done the same way for 
# bar graphs and line graphs
ce <- subset(cabbage_exp, Cultivar == "c39")

# With a bar graph
ggplot(ce, aes(Date, Weight)) + geom_bar(fill = "white", colour = "black", stat = "identity") +
  geom_errorbar(aes(ymin = Weight - se, ymax = Weight + se), width = .2)

# With a line graph
ggplot(ce, aes(Date, Weight)) + geom_line(aes(group = 1)) + geom_point(size = 4) + 
  geom_errorbar(aes(ymin = Weight - se, ymax = Weight + se), width = .2)
# We generally have to experiment and find the value of width that looks good on the graph

# Working with the full dataset this time

# While plotting bars as well as errorbars, we need to make sure the dodge is the same for both
# the default for bar is 0.9 so we should specify the same for error bar (unless we change the default)
# dodge with width not specified
ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) + geom_bar(position = "dodge", stat = "identity") + 
  geom_errorbar(aes(ymin = Weight - se, ymax = Weight + se), position = "dodge", width = .2) # sort of sucks!

# dodge with width specified
ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) + geom_bar(position = "dodge", stat = "identity") + 
  geom_errorbar(aes(ymin = Weight - se, ymax = Weight + se), position = position_dodge(0.9), width = .2) # sort of sucks!

# For line graphs, if the error bars are a different colour than the lines and points, we should draw the error bars 
# first, so that they are underneath the points and lines
# we should also dogde all geometric elements so that they will align with the error bars 
pd <- position_dodge(.3)

ggplot(cabbage_exp, aes(Date, Weight, colour = Cultivar, group = Cultivar)) +
  geom_errorbar(aes(ymin = Weight - se, ymax = Weight + se), width = .2, size = .25, colour = "black", position = pd) +
  geom_line(position = pd) + geom_point(position = pd, size = 2.5)
# We change the colour of the error bars so that their colour will be different from the lines
# When a discrete variable is mapped to a colour like aesthetic, that variable is used for grouping the data


### Adding annotations to individual facets
# create a data frame with the faceting variables and a value to use in each facet; then use geom_text
# with the new data frame
p <- ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(. ~ drv)

# A data frame with labels for each facet
f_labels <- data.frame(drv = c("4", "f", "r"), label = c("4wd", "Front", "Rear"))

p + geom_text(x = 6, y = 40, aes(label = label), data = f_labels)

# on using annotate, the label will appear in all facets
p + annotate("text", x = 6, y = 42, label = "label text")

# The following function gives a data frame with the value of r squared with each facet
