# TOOLBOX
# lists geoms and stats in ggplot2, broken down by their purpose
# 3 purposes of a layer: display data, display statistical summary or
# add additional metadata, context and annotations
# example of metadata is a map as a background layer for spatial data
library(ggplot2)
df <- data.frame(
  x = c(3, 1, 5),
  y = c(2, 4, 6),
  label = c("a", "b", "c")
)

p <- ggplot(df, aes(x, y, label = label)) + xlab(NULL) + ylab(NULL)
p + geom_point() + labs(title = "geom_point") # opts is defunct; using labs instead
p + geom_bar(stat = "identity") + labs(title = "geom_bar(stat = \"identity\")")
# geom_bar does not work with the default stat ie bin as a value for y is specified

p + geom_line() + labs(title = "geom_line")
p + geom_area() + labs(title = "geom_area")
p + geom_path() + labs(title = "geom_path")
p + geom_text() + labs(title = "geom_text")
p + geom_tile() + labs(title = "geom_tile")
p + geom_polygon() + labs(title = "geom_polygon")

# DISPLAYING DISTRIBUTIONS

# depends on dimensionality of distribution, whether it's continuous or
# discrete, and whether you're interested in conditional or joint distribution

# histogram is great for 1d continuous distributions
# bin placement is important and we can change binwidth and specify the exact location of breaks
# to compare distributions between groups, create small multiples of the histogram, facets = .~var;
# use a frequency polygon, geom = "freqpoly"; or create a conditional density plot, position = "fill"
# never rely on the default parameters to get a reveling view of the distributions!!!

depth_dist <- ggplot(diamonds, aes(depth)) + xlim(58, 68) 
depth_dist + geom_histogram(aes(y = ..density..), binwidth = 0.1) + facet_grid(cut ~ .)
depth_dist + geom_histogram(aes(fill = cut), binwidth = 0.1, position = "fill")
depth_dist + geom_freqpoly(aes(y = ..density.., colour =  cut), binwidth = 0.1)

# both histogram and freqpoly use stat_bin. the statistic produces two output variables count and density
# density is count divided by total count

# geom_boxplot =  stat_boxplot + geom_boxplot: box and whisker plot
# for a continuous variable conditioned by a categorical variable. this is a useful display when a 
# categorical variable has many distinct values. histogram and freqpoly can be better when there are few
library(plyr)
qplot(cut, depth, data = diamonds, geom = "boxplot")
qplot(cut, depth, data = diamonds, geom = "boxplot", 
      group = round_any(carat, 0.1, floor), xlim = c(0, 3)) # error here
# boxplot can be made for continuous variables using the group aesthetic 
# not working currently

# geom_jitter = position_jitter + geom_point: a crude way of looking at discrete distributions 
# by adding random noise to discrete values so they don't overplot
qplot(class, cty, data = mpg, geom = "jitter")
qplot(class, drv, data = mpg, geom = "jitter")
qplot(class, cty, data = mpg, geom = "point") # this thing is just weird!

# geom_density = stat_density + geom_area: a smoothed version of frequency polygons based on
# kernal smoothers. Use a density plot when you know the underlying density is 
# smooth, continuous and unbounded. USe the adjust parameter to make the density more or less smooth
qplot(depth, data = diamonds, geom = "density", xlim = c(54, 70))
qplot(depth, data = diamonds, geom = "density", xlim = c(54, 70),
      fill = cut, alpha = I(0.2))
# next section is visualizing a continuous 2d distribution

# DEALING WITH OVERPLOTTING
# small amount of overplotting can be overcome by making the points smaller, or using hollow glyphs
df <- data.frame(x = rnorm(2000), y = rnorm(2000))
norm <- ggplot(df, aes(x, y))
norm + geom_point()
norm + geom_point(shape = 1)
norm + geom_point(shape = ".") # pixel sized

# for larger datasets, aplha blending can be used to make the points transparent
# if aplha is specified as a ratio, the denominator gives the number of points that must be overplotted
# to give a solid colour. The lowest amount of transparency R can use is 1/256
# so it will not be effective for heavy overplotting
library(scales)
norm + geom_point(colour = alpha("black", 1/3))
norm + geom_point(colour = alpha("black", 1/5))
norm + geom_point(colour = alpha("black", 1/10))

# if there is some discreteness in the data, you can randomly jitter the points to alleviate 
# some overlaps. This is useful in conjunction with transparency. By default, the amount of jitter added is 40% 
# of the resolution of the data which leaves a small gap between adjacent regions
td <- ggplot(diamonds, aes(table, depth)) + xlim(50, 70) + ylim(50, 70)
td + geom_point()
td + geom_jitter()
jit <- position_jitter(width = 0.5)
td + geom_jitter(position = jit)
td + geom_jitter(position = jit, colour = alpha("black", 1/10))
td + geom_jitter(position = jit, colour = alpha("black", 1/50))
td + geom_jitter(position = jit, colour = alpha("black", 1/200))

# we can think of overplotting as a 2d density estimation problem and get two more approaches
# bin the points and count the number in each bin, then visualize the count in some way
d <- ggplot(diamonds, aes(carat, price)) + xlim(1, 3) + labs(legend.position = "none")
d + stat_bin2d()
d + stat_bin2d(bins = 10)
d + stat_bin2d(binwidth = c(0.02, 200))
install.packages("hexbin")
library(hexbin)
d + stat_binhex()
d + stat_binhex(bins = 10)
d + stat_binhex(binwidth = c(0.02, 200))

# estimate the 2d density with stat_density2d and overlay contours from this distribution t
# the scatterplot, or display the density by itself as coloured tiles, or points
# with size proportional to density
d <- ggplot(diamonds, aes(carat, price)) + xlim(1, 3) + labs(legend.position = "none")
d + geom_point() + geom_density2d()
d + stat_density2d(geom = "point", aes(size = ..density..), 
                   contour = F) + scale_area(to = c(0.2, 1.5)) # deprecated

d + stat_density2d(geom = "point", aes(size = ..density..), 
                   contour = F)
# if you want the conditional distribution of y on a given x, then 2.5.3

# Another way to handle overplotting is to add data sumamries to guide the eye to the true shape of the
# pattern within the data eg smooth line (later)
# MAPS (mostly US centric)

# REVEALING UNCERTAINTY
# there are four basic families of geoms that can be used for this job depending on
# whether x values are continuous or discrete adn whether or not you want to display the middle of the 
# interval or just the extant
# as there are so many ways to calculate standard errors, the calculation is upto you
# for very simple cases, ggplot2 gives tools in the form of statical summaries
# effects package: check it out

d <- subset(diamonds, carat < 2.5 & rbinom (nrow(diamonds), 1, 0.2) == 1)
d$lcarat <- log10(d$carat)
d$lprice <- log10(d$price)

# remove overall linear trend
detrend <- lm(lprice ~ lcarat, data = d)
d$lprice2 <- resid(detrend)
mod <- lm(lprice2 ~ lcarat * color, data = d )

install.packages("effects")
library(effects)

effectsdf <- function(...) {
  suppressWarnings(as.data.frame(effect(...)))
}
color <- effectsdf("color", mod)
both1 <- effectsdf("lcarat:color", mod)

carat <- effectsdf("lcarat", mod, default.levels = 50)
both2 <- effectsdf("lcarat:color", mod, default.levels = 3)




# STATISTICAL SUMMARIES
?stat_summary
# individual summary functions
midm <- function(x) mean(x, trim = 0.5)
m2 + stat_summary(aes(colour = "trimmed")) # never made m2 in the book!

# Single summary function
# something like m2 here too!

# Annotating a plot
# Annotations can be added one at a time or many at once
# one at a time works best for small number of annotations with varying 
# aesthetics. you just set all the values to give desired properties
# if you have multiple annotations with similar properties, it makes sense to 
# put all of them in a data frame and apply all at once
# below, we will add info about presidents to economic data
names(economics)
unemp <- qplot(x = date, y = unemploy, data = economics, geom = "line", xlab = "",
               ylab = "No. Unemployed(1000s")
presidential <- presidential[-(1:3), ]
yrng <- range(economics$unemploy)
xrng <- range(economics$date)
unemp + geom_vline(aes(xintercept = start), data = presidential)

unemp + geom_rect(aes(NULL, NULL, xmin = start, xmax = end, fill = party),
                  ymin = yrng[1], ymax = yrng[2], ddata = presidential) + 
  scale_fill_manual(values = alpha(c("blue", "red"), 0.2))
