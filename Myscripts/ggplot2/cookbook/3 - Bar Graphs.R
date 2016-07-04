# CHAPTER 3: BAR GRAPHS
library(plyr)
library(dplyr)
library(ggplot2)
library(gcookbook)

# You have a data frame where one column represents the x position of each bar, and
# another column represents the vertical (y) height of each bar.

# use geom_bar with stat = "identity"
str(pg_mean)

ggplot(pg_mean, aes(x = group, y = weight)) + geom_bar(stat = "identity")

# when x is numeric or continuous, bars behave differently
# instead of a bar for each value, there will be one at each possible value of x
# so, convert to factors

BOD # no value for 6
ggplot(BOD, aes(x = factor(Time), y = demand)) + geom_bar(stat = "identity")

# use fill for colour
ggplot(pg_mean, aes(x = group, y = weight)) + geom_bar(stat = "identity", 
                                                       fill = "lightblue",
                                                       colour = "black")

# GROUPING
# group bars together with a 2nd variable 
# map variable to fill and use geom_bar(position = "dodge)
str(cabbage_exp)
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) + 
  geom_bar(stat = "identity", position = "dodge")
# mapping a variable to fill gives a different colour to the factors in that variable
# position = "dodge" tells the bars to dodge each other horizontally so you don't
# end up with a stacked bar
# use colour inside geom_bar() to give outline
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) + 
  geom_bar(stat = "identity", position = "dodge", colour = "black")
# to set colours, you can use scale_fill_brewer() or scale_fill_manual()

ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) + 
  geom_bar(stat = "identity", position = "dodge", colour = "black") + 
  scale_fill_brewer(palette = "Pastel1")


# Making bar graphs of counts
# use geom_bar() without mapping anything to y

ggplot(diamonds, aes(cut)) + geom_bar() # default stat = "bin"
# continous variables on the x axis give a histogram

# using colours
# use fill
str(uspopchange)
upc <- subset(uspopchange, rank(Change) > 40)
upc
ggplot(upc, aes(x = Abb, y = Change, fill = Region)) + geom_bar(stat = "identity")
# scale_fill_brewer and scale_fill_manual can change colours
ggplot(upc, aes(reorder(Abb, Change), y = Change, fill = Region)) +
  geom_bar(stat = "identity", colour = "black") + 
  scale_fill_manual(values = c("#669933", "#FFCC66")) + xlab("State")
# this is also sorted by % change

# colouring negetive and positive bars differently
# use a subset of the data and create a new column that dictates whether a value is positive or negetive
library(dplyr)
str(climate)
csub <- filter(climate, Source == "Berkeley" & Year >= 1900)
csub
csub$pos <- csub$Anomaly10y >= 0
str(csub)
# we can make the graph and map pos to fill colour. pos is the field for +ve and -ve
# position = "identity" is used to not get an error of 
ggplot(csub, aes(Year, Anomaly10y, fill = pos)) + 
         geom_bar(stat = "identity", position = "identity")

# guide = F removes the legend and the colours in the first graph are reversed

ggplot(csub, aes(x = Year, Anomaly10y, fill = pos)) +
  geom_bar(stat = "identity", position = "identity", colour = "black", size = 0.25) +
  scale_fill_manual(values = c("#CCEEFF", "#FFDDDD"), guide = F)

# Adjust bar width and spacing
# default bar width is 0.9
ggplot(pg_mean, aes(group, weight)) + geom_bar(stat = "identity")
ggplot(pg_mean, aes(group, weight)) + geom_bar(stat = "identity", width = 0.5)
ggplot(pg_mean, aes(group, weight)) + geom_bar(stat = "identity", width = 1)

# for grouped bars, the default is to have no sapce between bars within each group

# for grouped bars with narrow bars
ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) +
  geom_bar(stat = "identity", width = 0.5, position = "dodge")
# And with some space between the bars
ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) + 
  geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.7))

# position_dodge() is the longform of position = "dodge"

# Making a stacked bar graph
ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) + 
  geom_bar(stat = "identity")
# one problem is that the stacking order is the opposite of the legend
# can be reversed by using guides() and specifying the aesthetic for which the legend should be reversed
ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) +
  geom_bar(stat = "identity") + guides(fill = guide_legend(reverse = T))
# to reverse the stacking order, 
library(plyr)
ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar, order = desc(Cultivar))) +
  geom_bar(stat = "identity")

# we can also modify the columns of the data frame so that the factor levels
# are in a differnt order
# for more polished graphs, use scale_fill_brewer
ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) +
  geom_bar(stat = "identity", colour = "black") + 
  guides(fill = guide_legend(reverse = T)) +
  scale_fill_brewer(palette = "Pastel1")

?palette
cabbage_exp

# Making a proportional stacked bar graph
# scale the data to 100% within each stack
# do a group wise transfiormation splitting on date

# Do a group-wise transform(), splitting on "Date"
ce <- ddply(cabbage_exp, "Cultivar", transform,
            percent_weight = Weight / sum(Weight) * 100)
ggplot(ce, aes(x=Date, y=percent_weight, fill=Cultivar)) +
  geom_bar(stat="identity")
# using dplyr for the same
cf <- cabbage_exp %>% group_by(Date) %>% summarise(sum = sum(Weight))
cf3 <- cabbage_exp 
cf3$sum <- cf$sum[match(x = cf$Date, table = cf3$Date)]
cf1 <- cf3 %>% arrange(Date) %>% mutate(
  perc = Weight/sum * 100
)



ggplot(ce, aes(Date, percent_weight, fill = Cultivar)) + 
  geom_bar(stat = "identity", colour = "black") + 
  guides(fill = guide_legend(reverse = T)) + 
  scale_fill_brewer(palette = "Pastel1")

# Add labels to the graph
# add geom_text() to the graph. vjust can be used to move the text

# below the top
ggplot(cabbage_exp, aes(interaction(Date, Cultivar), Weight)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Weight), vjust = 1.5, colour = "white")

# Above the top
ggplot(cabbage_exp, aes(interaction(Date, Cultivar), Weight)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Weight), vjust = -0.2)

# adjust y limits to be a little higher
ggplot(cabbage_exp, aes(interaction(Date, Cultivar), Weight)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Weight), vjust = -0.2) +
  ylim(0, max(cabbage_exp$Weight * 1.05))

# map y positions slightly above the bar top - y range of plot will auto adjust
ggplot(cabbage_exp, aes(interaction(Date, Cultivar), Weight)) + 
  geom_bar(stat = "Identity") + 
  geom_text(aes(y = Weight + 0.1, label = Weight))

# for grouped bar graphs, position = "dodge" needs to be used to avoid stacking
# label size can be made smaller to fit thinner bars
ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Weight), vjust = 1.5, colour = "white")
# position = "dodge" also needs to be used for the labels
ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Weight), vjust = 1.5, colour = "white", 
            position = position_dodge(0.9), size = 3)

# putting labels on a stacked bar requires finding the cummulative sum for each stack
# sort the data properly for this
ce <- arrange(cabbage_exp, Date, Cultivar)
# Get the cumulative sum
ce <- ddply(ce, "Date", transform, label_y=cumsum(Weight))
ce # again, learn how to do this using 

ggplot(ce, aes(Date, Weight, fill = Cultivar)) + geom_bar(stat = "identity") +
  geom_text(aes(y = label_y, label = Weight), vjust = 1.5, colour= "white")

# to put the labels in the middle of the bar, adjustment to the cummulative sum need to be made
ce <- arrange(cabbage_exp, Date, Cultivar)
ce <- ddply(ce, "Date", transform, label_y=cumsum(Weight) - 0.5 * Weight)

ggplot(ce, aes(Date, Weight, fill = Cultivar)) + geom_bar(stat = "identity") +
  geom_text(aes(y = label_y, label = Weight), vjust = 1.5, colour= "white")

# for a more polished graph
ggplot(ce, aes(Date, Weight, fill = Cultivar)) + 
  geom_bar(stat = "identity", colour = "black") + 
  geom_text(aes(y = label_y, label = paste(format(Weight, nsmall = 2), "kg")),
            size = 4) +
  guides(fill = guide_legend(reverse = T)) + 
  scale_fill_brewer(palette = "Pastel1")


# Making a cleveland dot plot
# can reduce visual clutter and be easier to read than bar graphs
tophit <- tophitters2001[1:25, ]
ggplot(tophit, aes(avg, name)) + geom_point()
# sorted by name here. We may want it sorted by average

ggplot(tophit, aes(avg, reorder(name, avg))) + 
  geom_point(size = 3) + # for a bigger dot
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed"))
?reorder
# axes can also be swapped and labels can be rotated
ggplot(tophit, aes(reorder(name, avg), avg)) + geom_point(size = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed"))

# grouping by another variable. by lg here
# reorder only reorders by one variable and herem we need to reorder by two so we have to do it manually
nameorder <- tophit$name[order(tophit$lg, tophit$avg)]
# turn name into a factor, with levels in order of nameorder
tophit$name <- factor(tophit$name, levels = nameorder)

# we will also add the mapping of lg to the colour of the points
# instead of using gridlines, make the lines go only up to the points 
# by using geom_segment()

ggplot(tophit, aes(avg, name)) +
  geom_segment(aes(yend = name), xend = 0, colour = "grey50") + 
  geom_point(size = 3, aes(colour = lg)) + 
  scale_color_brewer(palette = "Set1", limits = c("NL", "AL")) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(), # no horizontal gridlines
        legend.position = c(1, 0.55),   # put legend inside plot area
        legend.justification = c(1, 0.5))

# Another way to separate the groups is by using facets
# the order in which facets are displayed is different from the sorting order in the above figure
# need to change the order of factor levels in the lg variable to change the display order

ggplot(tophit, aes(avg, name)) +
  geom_segment(aes(yend = name), xend = 0, colour = "grey50") +
  geom_point(size = 3, aes(colour = lg)) + 
  scale_color_brewer(palette = "Set1", limits = c("NL", "AL"), guide = F) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(lg ~ ., scales = "free_y", space = "free_y")

?facet_grid
?theme_bw
?theme
?scale_color_brewer
?geom_segment
?geom_text
