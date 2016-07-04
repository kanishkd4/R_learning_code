library(ggplot2)
library(gcookbook)
library(plyr)
library(dplyr)
library(MASS)
library(data.table)
### Summarized data distributions

ggplot(faithful, aes(waiting)) + geom_histogram()
ggplot(faithful, aes(waiting)) + geom_bar() # the 2 are not the same
# a vector can be plottoed by giving ggplot a NULL in the data argument and using the vector in x

ggplot(faithful, aes(waiting)) + geom_histogram(binwidth = 5, fill = "white", colour = "black")


# devide the x range into 15 bins
binsize <- diff(range(faithful$waiting))/15
ggplot(faithful, aes(waiting)) + geom_histogram(binwidth = binsize, fill = "white", col = "black")
# the appearance of the histogram can depend on the width of the bins and where the boundaries of the bins are

?geom_histogram

h <- ggplot(faithful, aes(waiting))
h + geom_histogram(binwidth = 8, fill = "white", colour = "black", boundary = 31)
h + geom_histogram(binwidth = 8, fill = "white", colour = "black", boundary = 35)
# the results look different even though the bin size is the same

### making multiple histograms of binned data

ggplot(birthwt, aes(bwt)) + geom_histogram(fill = "white", colour = "black") + facet_grid(smoke ~ .)
# to make these plots, the data needs to be in one data frame
# the labels in the facet labels are 0 and 1; the levels need to be changed

birthwt1 <- birthwt
birthwt1$smoke <- factor(birthwt1$smoke)
levels(birthwt1$smoke)

birthwt1$smoke <- revalue(birthwt1$smoke, c("0" = "no smoke", "1" = "smoke")) # I generally use ifelse
ggplot(birthwt1, aes(bwt)) + geom_histogram(fill = "white", colour = "black") + facet_grid(smoke ~ .)
# shows the new levels on the graph

# it can be hard to compare the shape of distribution if the groups have different sizes
ggplot(birthwt, aes(bwt)) + geom_histogram(fill = "white", colour = "black") + facet_grid(race ~ .)
# to allow the y scales to be resized independently, use scales = free
ggplot(birthwt, aes(bwt)) + geom_histogram(fill = "white", colour = "black") + facet_grid(race ~ ., scales = "free")
# scales = free changes the y axis for each plot

# Another approach is to map the variable to fill
birthwt1$smoke <- as.factor(birthwt1$smoke)

# map smoke to fill, make the bars not stacked and make them semi transparent
ggplot(birthwt1, aes(bwt, fill = smoke)) + geom_histogram(position = "identity", alpha = 0.4)
# this is sort of a sad graph
# Without position  = "identity", ggplot will stack the histograms on top of each other

### Making a density curve

# use geom_density and map a continuous variable to x
ggplot(faithful, aes(waiting)) + geom_density() # there are lines at the ends of the graph and at the bottom

# to remove them
ggplot(faithful, aes(waiting)) + geom_line(stat = "density") + expand_limits(y = 0)

?expand_limits # expands the plot limits to include the value provided

# geom_histogram and geom_density work on a vector if we simply pass a null in the data in ggplot

# Kernel density curve: an estimation of the population distribution based on a sample
# the amount of smoothening depends on the kernel bandwidth; larger bandwidth leads to more smoothening 
# the bandwidth can be set with the adjust parameter
ggplot(faithful, aes(waiting)) + 
  geom_line(stat = "density", adjust = .25, colour = "red") +
  geom_line(stat = "density") +
  geom_line(stat = "density", adjust = 2, colour = "blue") # the edge of the curved gets clipped

ggplot(faithful, aes(waiting)) + geom_density(fill = "blue", alpha = .2) + xlim(35, 105)

# This adds a blue polygon with geom_density, then adds a line on top
# basically, makes the base dissapear from the previous graph
ggplot(faithful, aes(waiting)) + geom_density(fill = "blue", colour = NA, alpha = .2) + 
  geom_line(stat = "density") + xlim(35, 105)

# To compare the theoretical and observed distributions, you can overlay the density curve with the histogram
# the area under a density curve is 1 so the histogram needs to be scaled down. this is done with ..density..

ggplot(faithful, aes(x = waiting, y = ..density..)) + # ..density.. scales down the histogram
  geom_histogram(fill = "cornsilk", colour = "grey60", size = .2) + xlim(35, 105) + geom_density()

### Making multiple density curves with grouped data
# use geom_density and map the grouped variable to an aesthetic like fill or colour

birthwt1 <- birthwt
birthwt1$smoke <- as.factor(birthwt1$smoke)
ggplot(birthwt1, aes(bwt, colour = smoke)) + geom_density()

# map smoke to fill and make the fill translucent 
ggplot(birthwt1, aes(bwt, fill = smoke)) + geom_density(alpha = .3)

# we can also use facets which give us multiple graphs
birthwt1$smoke <- revalue(birthwt1$smoke, c("0" = "no smoke", "1" = "smoke")) # I generally use ifelse

ggplot(birthwt1, aes(bwt)) + geom_density() + facet_grid(smoke ~ .)

# If we want to see histograms next to density plots, facets are a great way to compare
ggplot(birthwt1, aes(bwt, ..density..)) + geom_histogram(binwidth = 200, fill = "cornsilk", colour = "grey60", size = .2) + 
  geom_density() + facet_grid(smoke ~ .)

### Making a frequency polygon
# use geom_freqpoly()
ggplot(faithful, aes(waiting)) + geom_freqpoly()
ggplot(faithful, aes(waiting)) + geom_freqpoly(binwidth = 4)

# using a specific number of bins
binwidth <- diff(range(faithful$waiting))/15
ggplot(faithful, aes(waiting)) + geom_freqpoly(binwidth = binwidth)


### Making a basic box plot
# use geom_boxplot(), map a continuous variable to y and a discrete variable to x

ggplot(birthwt, aes(factor(race), bwt)) + geom_boxplot() # x has to be discrete

# we can set the width of the boxes
ggplot(birthwt, aes(factor(race), bwt)) + geom_boxplot(width = .5)

# we can also change outlier shape and size
ggplot(birthwt, aes(factor(race), bwt)) + geom_boxplot(outlier.size = 1.5, outlier.shape = 21)

# To make a boxplot of a single group
ggplot(birthwt, aes(x = 1, y = bwt)) + geom_boxplot() + scale_x_continuous(breaks = NULL) + 
  theme(axis.title.x = element_blank()) # changing x in aes does not change the plot



### Adding notches to a boxplot
# in geom_boxplot(), set notch = T

ggplot(birthwt, aes(factor(race), bwt)) + geom_boxplot(notch = T)
# notches are used to see if the medians of the boxplots differ (seems pretty useless mostly)


### Adding means to a boxplot
# use stat_summary()

ggplot(birthwt, aes(factor(race), bwt)) + geom_boxplot() + stat_summary(fun.y = "mean", geom = "point", shape = 23,
                                                                       size = 3, fill = "white")


### Making a violin plot
# this will help us compare the densities of different groups
# use geom_violin()
p <- ggplot(heightweight, aes(sex, heightIn))
p + geom_violin()
# these can be more convenient than density plots to compare densities

# We can also overlay narrow box plots with a dot for a median and remove outliers
p + geom_violin() + geom_boxplot(width = .1, fill = "black", outlier.color = NA) +
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 21, size = 2.5)

p + geom_violin(trim = F) # this does not trim the tails of the plot (the default is to trim)

# we can scale the area as per our requirement
p + geom_violin(scale = "count")

# more smoothening
p + geom_violin(adjust = 2)

# less smoothening 
p + geom_violin(adjust = .5)


### Making a dot plot
# use geom_dotplot()

countries2009 <- subset(countries, Year == 2009 & healthexp > 2000)

p <- ggplot(countries2009, aes(infmortality))
p + geom_dotplot()
# this is also called a wilkinson dot plot; there is also the cleveland dot plot seen in chapter 3
# the placement of the bins depends on the data, and the width of each 

par(mfrow = c(1,2))
p + geom_dotplot(); p + geom_histogram()

# the tick marks on the Y axis are meaningless
# note that the plot is binned on the x axis and stacked on the y axis
p + geom_dotplot(binwidth = .25) + geom_rug() + scale_y_continuous(breaks = NULL) + # removes ticks
  theme(axis.title.y = element_blank()) # removes axis title

# the stacks can be regularly spaced on the x axis if we want
# to use bins that are arranged with a fixed, regular spacing like a histogram, use method = "histdot"

p + geom_dotplot(method = "histdot", binwidth = .25) + geom_rug() + scale_y_continuous(breaks = NULL) +
  theme(axis.title.y = element_blank())

# the dots can be stacked or centered in a way that stacks with even and odd quantities stay aligned

p + geom_dotplot(binwidth = .25, stackdir = "center") + scale_y_continuous(breaks = NULL) +
  theme(axis.title.y = element_blank())
p + geom_dotplot(binwidth = .25, stackdir = "centerwhole") + scale_y_continuous(breaks = NULL) +
  theme(axis.title.y = element_blank())



### Making multiple dot plots for grouped data
# to compare multiple groups, we can stack the dots along the y axis and group them along the x axis by setting
# binaxis = "y"

ggplot(heightweight, aes(sex, heightIn)) + geom_dotplot(binaxis = "y", binwidth = .5, stackdir = "center")

# dot plots are sometimes overlaid with boxplots; in this case it may be helpful to make the dots hollow and 
# have the boxplots not show outliers

ggplot(heightweight, aes(sex, heightIn)) + geom_boxplot(outlier.color = NA, width = .4) +
  geom_dotplot(binaxis = "y", binwidth = .5, stackdir = "center", fill = NA)

# we can also show boxplots next to dotplots by treating the x variable as numeric and shifting the plots just a little  
# when the x variable is treated as numeric, you must also specify the group or the data is treated as one group
ggplot(heightweight, aes(sex, heightIn)) + geom_boxplot(aes(x = as.numeric(sex) + .2, group = sex), width = .25) +
  geom_dotplot(aes(x = as.numeric(sex) - .2, group = sex), binaxis = "y", binwidth = .5, stackdir = "center") +
  scale_x_continuous(breaks = 1:nlevels(heightweight$sex), labels = levels(heightweight$sex))


### Making a density plot of two dimentional data
# use stat_density2d()
# First, we'll plot the density contour along with data points
p <- ggplot(faithful, aes(eruptions, waiting))
p + geom_point() + geom_density2d()

# It is also possible to map the height of the density curve to the colour of the contour lines by using ..level..
p + stat_density2d(aes(colour = ..level..))
# The 2d density kernel estimate is analogous to the one dimensional density estimate generated by stat_density but
# it needs to be viewed in a differnt way

# map density estimate to fill colour
p + stat_density2d(aes(fill = ..density..), geom = "raster", contour = F)

# with points and map density estimate to alpha
p + geom_point() + stat_density2d(aes(alpha = ..density..), geom = "tile", contour = F)

# the raster geom renders more efficiently than tile
# As with the one dimensional density estimate, we can control the bandwidth of the estimate
# to do that, pass a vector for x and y to h

p + stat_density2d(aes(fill = ..density..), geom = "raster", contour = F, h = c(.5, 5))
