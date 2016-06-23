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




