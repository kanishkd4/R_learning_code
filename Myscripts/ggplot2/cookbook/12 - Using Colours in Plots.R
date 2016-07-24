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
# use one of the scales listed in the table in the book
# scale_fill_discrete/hue/grey/brewer/manual

p <- ggplot(uspopage, aes(Year, Thousands, fill = AgeGroup)) + geom_area()

# the following 3 have the same effect
p
p + scale_fill_discrete()
p + scale_fill_hue()

# Colour brewer palette 
p + scale_fill_brewer()

# While these scales are good for fills, they might be light for points
# use l(luminance/lightness) for darker colours; default is 65 (0 - 100)
h <- ggplot(heightweight, aes(ageYear, heightIn, colour = sex)) + geom_point()
h
h + scale_color_hue(l = 45) # slightly darker plot

# The colour brewer package provides a number of palettes and we can generate a graphic showing
# all of them
library(RColorBrewer)
display.brewer.all()
# the palettes can be selected by name 
p + scale_fill_brewer(palette = "Oranges")

# we can also use a grey palette; good for a black and white output
# we can also change the range from black(0) to white(1)

p + scale_fill_grey()
p + scale_fill_grey(start = 0.7, end = 0)



### Using a manually defined palette for a discrete variable

# using colour names
h + scale_colour_manual(values = c("red", "blue"))

# using RGB values
h + scale_colour_manual(values = c("#CC6666", "#7777DD"))
# for fill, use scale_fill_manual instead

# the order of the items matches the order of the factor levels 
levels(heightweight$sex) # to see the levels

# it is possible to specify colours in a different order by using a named vector
h + scale_colour_manual(values = c(m = "blue", f = "red"))

# There is a large set of names colours in R; some basic, others not very informative from 
# name like thistle3, seashell so it is often easier to specify RGB values to specify colours
# each colour is represented by 2 digits and goes from 00 to FF (255 in base 10)
# FF0099 has a value 255 for red, 0 for green and 155 for blue
# Thumb rules for specifying RGB colours
# higher numbers are brighter and lower are darker
# to get a shade of grey, set all channels to the same value
# The opposites of RGB are CMY; cyan, magenta and yellow



### Using a colourblind friendly palette
# use the palette cb_palette in scale_colour_manual
p <- ggplot(uspopage, aes(Year, Thousands, fill = AgeGroup)) + geom_area()
# The palette with grey:
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7")

p + scale_fill_manual(values = cb_palette)
# In some cases, it may be useful to use black instead of grey;
# to do that, replace #999999 with #000000




### Using a manually defined palette for a continuous variable
# specify colours using a gradient scales
p <- ggplot(heightweight, aes(ageYear, heightIn, colour = weightLb)) + geom_point(size = 3)
p

# with a gradient betweene the 2 colours
p + scale_colour_gradient(low = "black", high = "white")


# a gradient with a white midpoint: using package scales
p + scale_colour_gradient2(low = muted("red"), mid = "white", high = muted("blue"),
                           midpoint = 110)

# A gradient of n colours
p + scale_colour_gradientn(colours = c("darkred", "orange", "yellow", "white"))

# for fill, use scale_fill_xxxx() instead for the same result
# Mapping continuous variables to a colour scale requires a continuously changing 
# palette of colours. 
# muted returns a value that is much less saturated version of the colour chosen




### Colour a shaded region based on value
# Add a column that categorizes the y values and then map that column to fill
cb <- subset(climate, Source=="Berkeley")
cb$valence[cb$Anomaly10y >= 0] <- "pos"
cb$valence[cb$Anomaly10y < 0] <- "neg"

cb
ggplot(cb, aes(Year, Anomaly10y)) + geom_area(aes(fill = valence)) + geom_line() +
  geom_hline(yintercept = 0)
# there are some stray shaded areas near the 0 line. to solve this, we can interpolate the data
# to a 1000 points by using approx()
# approx returns a list with x and y vectors
interp <- approx(cb$Year, cb$Anomaly10y, n = 1000)
# Put in a data frame and recalculate valence
cbi <- data.frame(Year=interp$x, Anomaly10y=interp$y)
cbi$valence[cbi$Anomaly10y >= 0] <- "pos"
cbi$valence[cbi$Anomaly10y < 0] <- "neg"

# it would be more precise to interpolate exactly where the line crosses 0, but
# approx works just fine here
ggplot(cbi, aes(Year, Anomaly10y)) + geom_area(aes(fill = valence), alpha = .4) +
  geom_line() + geom_hline(yintercept = 0) + scale_fill_manual(values = c("#CCEEFF", "#FFDDDD"), guide = F) +
  scale_x_continuous(expand = c(0, 0))
