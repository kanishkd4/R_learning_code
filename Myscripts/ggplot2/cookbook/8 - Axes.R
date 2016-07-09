rm(list = ls())
library(ggplot2)
library(gcookbook)
library(MASS)
library(data.table)
library(plyr)
library(scales)

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
# To manually set the order of items, specify the limits with a vector of the levels in the
# desired order. We can also omit items with this vector
p <- ggplot(PlantGrowth, aes(group, weight)) + geom_boxplot()
p + scale_x_discrete(limits = c("trt1", "cntr", "trt2")) # we can also use this method to show a subset of the data

# To reverse the order
p + scale_x_discrete(limits = rev(levels(PlantGrowth$group)))

### Setting the scaling ratio of the x and y axes
# use coord_fixed. this will result in a 1:1 scaling between the x and y axis
sp <- ggplot(marathon, aes(Half, Full)) + geom_point()
sp
sp + coord_fixed()
# Sometimes, it is useful for both the axis to have the same scale
# It is also helpful to set the tick spacing to be the same by setting breaks in scale_y_continuous
# and scale_x_continuous
sp + coord_fixed() + scale_y_continuous(breaks = seq(0, 420, 30)) +
  scale_x_continuous(breaks = seq(0, 420, 30))

# If we need a different ratio
sp + coord_fixed(ratio = 1/2) + scale_y_continuous(breaks = seq(0, 420, 30)) +
  scale_x_continuous(breaks = seq(0, 420, 15)) # 1/2 doubles the x axis wrt y axis


### Setting the positions of tick marks
# isn't generally necessary, but if needed, set breaks in scale
ggplot(PlantGrowth, aes(group, weight)) + geom_boxplot()
ggplot(PlantGrowth, aes(group, weight)) + geom_boxplot() +
  scale_y_continuous(breaks = c(4, 4.25, 4.5, 5, 6, 8))

# The location of the tick marks defines where the major gridlines are drawn.
# If the axis represents a continuous variable, minor gridlines (fainter and unlabeled)
# will be drawn by default halfway between each major gridline
# the seq function can also be set in breaks to decrease typing


### Removing tick marks and labels
# use theme(axis.text.y = element_blank()). this will work for both continuous and categorical

p <- ggplot(PlantGrowth, aes(group, weight)) + geom_boxplot()
p
p + theme(axis.text.y = element_blank())
# to remove tick marks, use theme(axis.ticks = elements_blank()). It is not possible to remove 
# the tick marks on only one axis
p + theme(axis.text.y = element_blank(), axis.ticks = element_blank())

# To remove tick marks, labels and gridlines, use breaks = NULL for continuous axis only
# if we remove items from a categorical axis using limits, the data from the value won't be 
# shown at all
p + scale_y_continuous(breaks = NULL)


### Changing the text of tick labels

hwp <- ggplot(heightweight, aes(ageYear, heightIn)) + geom_point()
hwp
# To set arbitrary labels, pass values to breaks and labels in the scale. 
# One of the labels has a newline character which tells ggplot to put a line break there
hwp + scale_y_continuous(breaks = c(50, 56, 60, 66, 72),
                         labels = c("Tiny", "Really\nshort", "Short", "Medium",
                                    "Tallish"))
# Instead of having data stored in completely arbitrary labels, it is more common to have the data
# stored in one format, while wanting the labels to be displayed in another eg
# we might want height to be displayed in feet and inches instead of just inches.
# To do this, we define a formatter function

footinch_formatter <- function(x) {
  foot <- floor(x/12)
  inch <- x %% 12
  return(paste(foot, "'", inch, "\"", sep = ""))
}

footinch_formatter(56:64)
# We can pass this function to scale using the labels parameter
hwp + scale_y_continuous(labels = footinch_formatter) # Damn!
# automatic labels are put at every 5" but that's a little off for this data

hwp + scale_y_continuous(breaks = seq(48, 72, 4), labels = footinch_formatter)

# Another common task is to convert time measurements to HH:MM:SS format or something similar
# this function will take numeric minutes and convert them to the desired format

timeHMS_formatter <- function(x) {
  h <- floor(x/60)
  m <- floor(x %% 60)
  s <- round(60 * (x %% 1)) # Round to the nearest second
  lab <- sprintf("%02d:%02d:%02d", h, m, s) # format strings as HH:MM:SS
  lab <- gsub("^00:", "", lab) # removing leading 00: if present
  lab <- gsub("^0", "", lab)# removing leading 0 if present
  return(lab)
}

timeHMS_formatter(c(.33, .5, 50, 51,25, 59,32, 70))
# the scales library comes with many in built formatting function
# like comma(), dollar(), percent(), scientific(


### Changing the appearance of tick labels
bp <- ggplot(PlantGrowth, aes(group, weight)) + geom_boxplot() + 
  scale_x_discrete(breaks = c("ctrl", "trt1", "trt2"), 
                   labels = c("control", "Treatment1", "Treatment2"))
bp
# To rotate the text 90 degrees counterclockwise, use
bp + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

bp + theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
bp + theme(axis.text.x = element_text(angle = -30, hjust = 1, vjust = 1)) # just weird

# Other properties of the text can also be set using element_text()
bp + theme(axis.text.x = element_text(family = "times", face = "italic", colour = "darkred",
                                      size = rel(0.9)))
bp + theme(axis.text.x = element_text(family = "zurich", face = "italic", colour = "darkred",
                                      size = rel(0.9)))
# this command only affects one axis and does not affect other axis or the legend or title
# to make a theme change in everything, we can use the theming system shown in chapter 9



### Changing the text of an axis label
# use xlab or ylab 
hwp <- ggplot(heightweight, aes(ageYear, heightIn, colour = sex)) + geom_point()
hwp
hwp + xlab("Age in Years") + ylab("Height in Inches")
# Instead of usign xlab and ylab, we can use labs()

hwp + labs(x = "Age in Years", y = "Height in Inches")
# Another way
hwp + scale_x_continuous(name = "Age in Years")
hwp + scale_x_continuous(name = "Age\nYears") # we can also add line breaks


### Removing axis labels
# theme(axis.title = element_blank()

p <- ggplot(PlantGrowth, aes(group, weight)) + geom_boxplot()
p + theme(axis.title.x = element_blank())
p + xlab("") # Another way



### Change the appearance of axis labels
# again use axis.title.x
hwp <- ggplot(heightweight, aes(ageYear, heightIn)) + geom_point()
hwp + theme(axis.title.x = element_text(face = "italic", colour = "darkred", size = 14))
hwp + ylab("Height\n(inches)") +
  theme(axis.title.y = element_text(angle = 0, face = "italic", size = 14))
hwp + ylab("Height\n(inches)") +
  theme(axis.title.y = element_text(angle = 90, face = "italic", size = 14, colour = "darkred"))



### Showing lines along the axis
p <- ggplot(heightweight, aes(ageYear, heightIn)) + geom_point()
p
p + theme(axis.line = element_line(colour = "black")) # looks like it doesn't work anymore

# If you are starting with a theme that has a border around the plotting area, like theme_bw
# you also need to unset panel.border
p + theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(colour = "black"))

# If the lines are thick, they will partially overlap. To make them fully overlap, set
# lineed = "square"
# With thick lines, only half overlaps
p + theme_bw() + theme(panel.border = element_blank(), 
                       axis.line = element_line(colour = "black", size = 4, lineend = "square"))
# Still nothing


### Using a logarithmic axis
# use scale_x_log10 or scale_y_log10
p <- ggplot(Animals, aes(body, brain, label = rownames(Animals))) + geom_text(size = 3)
p
p + scale_x_log10() + scale_y_log10()
# ggplot2 will try to place tick marks where it can but that can be changed using breaks and labels
p + scale_x_log10(breaks = 10^(-1:5)) + scale_y_log10(breaks = 10^(0:3))

# To instead use the expoential notation for the break labels, use the trans_format
# from the scales package
p + scale_x_log10(breaks = 10^(-1:5), labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10(breaks = 10^(0:3), labels = trans_format("log10", math_format(10^.x)))
# Cool

# We can also use other log values but we have to spell them out

# natural log on x and log2 on y
p + scale_x_continuous(trans = log_trans(), breaks = trans_breaks("log", function(x) exp(x)),
                       labels = trans_format("log", math_format(e^.x))) + 
  scale_y_continuous(trans = log2_trans(), breaks = trans_breaks("log", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x)))

# It is possible to use a log axis for only one axis and often done for financial data
ggplot(aapl, aes(date, adj_price)) + geom_line()
ggplot(aapl, aes(date, adj_price)) + geom_line() + scale_y_log10(breaks = c(2, 10, 50, 250))


### Adding ticks for a log axis
# use annotation_logticks
ggplot(Animals, aes(body, brain, label = rownames(Animals))) + geom_text(size = 3) +
  annotation_logticks() +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))
# the tick marks created by annotation_logticks are geoms inside the plotting area
# to get the colours matching better, you can use theme_bw 
# minor gridlines appear halfway between the major gridlines but this is not the same place as
# the "5" tick mark on the log scale. this can also be manually set
# We can manually set the scales's minor breaks. We need to set them to log10(5*10^(minpow:maxpow))

ggplot(Animals, aes(body, brain, label = rownames(Animals))) + geom_text(size = 3) +
  annotation_logticks() +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                minor_breaks = log10(5) + -2:5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                minor_breaks = log10(5) + -1:3)



### Making a circular graph
# use coord_polar
wind
ggplot(wind, aes(DirCat, fill = SpeedCat)) + geom_histogram(binwidth = 15, boundary = -7.5) +
  coord_polar() + scale_x_continuous(limits = c(0, 360))
# Polar plots can perpetually distort the data
# we can make the plot a little prettier by reversing the legend, using a different pallete,
# adding an outline and setting breaks to some more familiar numbers

ggplot(wind, aes(DirCat, fill = SpeedCat)) +
  geom_histogram(binwidth = 15, boundary = -7.5, colour = "black", size = .25) +
  guides(fill = guide_legend(reverse = T)) + coord_polar() +
  scale_x_continuous(limits = c(0, 360), breaks = seq(0, 360, by = 15)) + scale_fill_brewer()
# much better
# it may also be better to set the starting angle with the start argument
# the starting angle is specified in radians so it may need to be converted from degrees
# coord_polar(start = -45 * pi/180)
# polar coordinates can be used with other geoms, like lines and points
# the smallest value gets mapped to the center
# The smallest and largest x values are merged when using a continuous x
# this may or may not be desirable
# Put mdeaths time series data into a data frame
md <- data.frame(deaths = as.numeric(mdeaths),
                 month = as.numeric(cycle(mdeaths)))

library(plyr) # For the ddply() function
md <- ddply(md, "month", summarise, deaths = mean(deaths))
md

# Make the base plot
p <- ggplot(md, aes(x=month, y=deaths)) + geom_line() +
  scale_x_continuous(breaks=1:12)
# With coord_polar
p + coord_polar()
# The first problem is that the data values (ranging from about 1000 to 2100) are mapped
# to the radius such that the smallest data value is at radius 0. We’ll fix this by setting the
# y (or r) limits from 0 to the maximum data value

# With coord_polar and y (r) limits going to zero
p + coord_polar() + ylim(0, max(md$deaths))

# 1 and 12 are shown in the same angle
p + coord_polar() + ylim(0, max(md$deaths)) + xlim(0, 12)
# this has one last issue that the beginning and ends are not connected
# to fix that, we need to modify the data frame
# Connect the lines by adding a value for 0 that is the same as 12
mdx <- md[md$month==12, ]
mdx$month <- 0
mdnew <- rbind(mdx, md)
# Make the same plot as before, but with the new data, by using %+%
p %+% mdnew + coord_polar() + ylim(0, max(md$deaths))



### Using dates on an axis
# map a column of class Date to the x or y axis
str(economics)
ggplot(economics, aes(date, psavert)) + geom_line()
# It can handle dates and POSIXt

# specifying breaks is similar as with a numeric axis; the main difference is in specifying 
# the sequence of dates to use
econ <- subset(economics, date >= as.Date("1992-05-01") & date < as.Date("1993-06-01"))
p <- ggplot(econ, aes(date, psavert)) + geom_line()
p
# breaks can be created using seq
datebreaks <- seq(as.Date("1992-06-01"), as.Date("1993-06-01"), by = "2 month")
p + scale_x_date(breaks = datebreaks) + theme(axis.text.x = element_text(angle = 30, hjust = 1))

# the formatting can be specified using the date_format function from scales
p + scale_x_date(breaks = datebreaks, labels = date_format("%Y %b")) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
# pretty useful date format info given in the book
# %Y Year with century (2012)
# %y Year without century (12)
# %m Month as a decimal number (08)
# %b Abbreviated month name in current locale (Aug)
# %B Full month name in current locale (August)
# %d Day of month as a decimal number (04)
# %U Week of the year as a decimal number, with Sunday as the first day of the week (00–53)
# %W Week of the year as a decimal number, with Monday as the first day of the week (00–53)
# %w Day of week (0–6, Sunday is 0)
# %a Abbreviated weekday name (Thu)
# %A Full weekday name (Thursday)


### Using relative times on an axis
# Times are commonly stored as numbers
WWWusage # time series data

# convert data to a data frame
www <- data.frame(minute = as.numeric(time(WWWusage)), 
                  users = as.numeric(WWWusage))

# define a formatter function
timeHM_formatter <- function(x) {
  h <- floor(x/60)
  m <- floor(x %% 60)
  lab <- sprintf("%d:%02d", h, m) # Format the strings as HH:MM
  return(lab)
}

# default x axis
ggplot(www, aes(minute, users)) + geom_line()

# formatted axis
ggplot(www, aes(minute, users)) + geom_line() +
  scale_x_continuous(name = "time", breaks = seq(0, 100, by = 10), labels = timeHM_formatter)

# in some cases it may be simpler to specify the labels manually
# to convert into HH:MM:SS formatter, use the timeHMS_formatter function created above



