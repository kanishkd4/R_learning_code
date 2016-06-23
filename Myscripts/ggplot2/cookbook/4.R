# CHAPTER 4: Line Graphs
library(ggplot2)
library(gcookbook)
library(plyr)
library(dplyr)

ggplot(BOD, aes(Time, demand)) + geom_line()
BOD
# x variables will generally be continous but can be used as discrete by
# converting to factor and must be used with aes(group = 1) to ensure that ggplot()
# knows that the data points belong together and should be connected with a line
BOD1 <- BOD
BOD1$Time <- factor(BOD1$Time)
ggplot(BOD1, aes(Time, demand, group = 1)) + geom_line()
ggplot(BOD1, aes(Time, demand)) + geom_line() # this does not work!! (without group)

# add geom_point() to add points to the line
ggplot(BOD, aes(Time, demand)) + geom_line() + geom_point()
ggplot(worldpop, aes(Year, Population)) + geom_line() + geom_point()
# intervals between data points are not consistent in Worldpop
ggplot(worldpop, aes(Year, Population)) + geom_line() + geom_point() +
  scale_y_log10() # same with a log Y axis

# Making a line graph with multiple lines
tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length = mean(len))

# map supp to colour
ggplot(tg, aes(dose, length, colour = supp)) + geom_line()

# map supp to linetype
ggplot(tg, aes(dose, length, linetype = supp)) + geom_line()

# if the x variable is a factor, R must be told to group by the same variable
ggplot(tg, aes(factor(dose), length, colour = supp, group = supp)) +
  geom_line()
# without grouping, this will give an error

# incorrect grouping can also lead to a jagged sawtooth pattern
ggplot(tg, aes(dose, length)) + geom_line()
# this happens as there are multiple data points at each y location and the 
# formula thinks they are all in one group
# discrete variables mapped to colour or linetype are automatically used as 
# grouping variables

# if the plot has points along with lines, you can also map variables to the
# properties of the points such as shape and fill
ggplot(tg, aes(dose, length, shape = supp)) + geom_line() +
  geom_point(size = 4)
ggplot(tg, aes(dose, length, fill = supp)) + geom_line() +
  geom_point(size = 4, shape = 21)

#overlapping points can be dodged. points and lines muct be dodged together
ggplot(tg, aes(dose, length, shape = supp)) +
  geom_line(position = position_dodge(0.2)) +
  geom_point(position = position_dodge(0.2), size = 4)


# Changing the appearance of lines
ggplot(BOD, aes(Time, demand)) + 
  geom_line(linetype = "dashed", size = 1, colour = "blue")
# if there are multiple lines, setting aesthetic properties will affect all lines
# but if variables are mapped to properties, then each line will look different

ggplot(tg, aes(dose, length, colour = supp)) + geom_line() +
  scale_colour_brewer(palette = "Set1")
# to set a single colour for all lines, set colour outside aes. the same works for
# size, linetype, and point shape. grouping variable may have to be specified

ggplot(tg, aes(dose, length, group = supp)) + 
  geom_line(colour = "darkgreen", size = 1.5)

ggplot(tg, aes(dose, length, colour = supp)) + geom_line(linetype = "dashed") +
  geom_point(shape = 22, size = 3, fill = "white")




# changing the appearance of points
# in geom_point() set the size, shape, colour, and/or fill outside aes
ggplot(BOD, aes(Time, demand)) + geom_line() +
  geom_point(size = 4, shape = 22, colour = "darkred", fill = "pink")
ggplot(BOD, aes(Time, demand)) + geom_line() +
  geom_point(size = 4, shape = 21, fill = "white")
# if points and lines have different colours, we should specify the points after lines
# so they are drawn on top

tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length = mean(len))
pd <- position_dodge(0.2)
ggplot(tg, aes(dose, length, fill = supp)) + geom_line(position = pd) +
  geom_point(size = 3, shape = 21, position = pd) +
  scale_fill_manual(values = c("black", "white"))
# we manually specified fills for the points and added a slight dodge

# Making a graph with a shaded area
# use geom_area()
sunspotyear <- data.frame(
  Year = as.numeric(time(sunspot.year)),
  Sunspots = as.numeric(sunspot.year)
)

ggplot(sunspotyear, aes(Year, Sunspots)) + geom_area()
# default colour is black with no outline
# change the colour to blue and make it 80% transparent by setting alpha to 0.2
# so that gridlines can be seen through the area
# also set an outline by setting colour
ggplot(sunspotyear, aes(Year, Sunspots)) + 
  geom_area(colour = "black", fill = "blue", alpha = 0.2)
# having an outline in the entire area may not be desirable as it puts a verticle line at
# the beginning and end of the plot
# to avoid this, we can use geom_line() instead of colour
ggplot(sunspotyear, aes(Year, Sunspots)) + geom_area(fill = "blue", alpha = 0.2) +
  geom_line()



# Making a stacked area graph
# use geom_area and map a factor to fill
ggplot(uspopage, aes(Year, Thousands, fill = AgeGroup)) +  geom_area()
# legend inverted again
# legend can be reversed by setting breaks in the scale
ggplot(uspopage, aes(Year, Thousands, fill = AgeGroup)) +
  geom_area(colour = "black", size = .2, alpha = .4) +
  scale_fill_brewer(palette = "Blues", breaks = rev(levels(uspopage$AgeGroup)))

# to reverse the stacking order, we'll put order = desc(AgeGroup) inside aes()
ggplot(uspopage, aes(Year, Thousands, fill = AgeGroup, order = desc(AgeGroup))) +
  geom_area(colour = "black", size = .2, alpha = .4) +
  scale_fill_brewer(palette = "Blues")

# if the left and right side is drawn with an outline like it is in the above graph,
# it can be misleading
# to correct it, draw stacked areas without an outline and then use geom_line()
ggplot(uspopage, aes(Year, Thousands, fill = AgeGroup, order = desc(AgeGroup))) +
  geom_area(colour = NA, alpha = .4) + scale_fill_brewer(palette = "Blues")
  geom_line(position = "stack", size = .2)

uspopage_prop <- ddply(uspopage, "Year", transform,
                       percent = Thousands/sum(Thousands) * 100)
# plotting is the same as a regular stacked graph once proportions have been calculated
ggplot(uspopage_prop, aes(Year, percent, fill = AgeGroup)) +
  geom_area(colour = "black", size = .2, alpha = .4)+
  scale_fill_brewer(palette = "Blues", breaks = rev(levels(uspopage_prop$AgeGroup)))


# Adding a confidence region
# use geom_ribbon() to map values to ymin and ymax
# in the climate data set, Anomaly10y is a 10 year running average of the deviation
# from the average 1950-80 temperature and Unc10y is the 95% confidence interval
clim <- subset(climate, Source == "Berkeley",
               select = c("Year", "Anomaly10y", "Unc10y"))
clim

# shaded region
ggplot(clim, aes(Year, Anomaly10y)) + 
  geom_ribbon(aes(ymin = Anomaly10y - Unc10y, ymax = Anomaly10y + Unc10y), alpha = 0.2)+
  geom_line()
# transparency is set by alpha here which makes it 80% transparent
# geom_ribbon() is before geom_line() so the line is drawn on top of the ribbon
# instead of a shaded regeion, a dotted line can also be used to represent the upper and lower bounds
ggplot(clim, aes(Year, Anomaly10y))+
  geom_line(aes(y = Anomaly10y + Unc10y), colour = "grey50", linetype = "dotted")+
  geom_line(aes(y = Anomaly10y - Unc10y), colour = "grey50", linetype = "dotted")+
  geom_line()

?geom_line()
