rm(list = ls())
library(ggplot2)
library(gcookbook)
library(MASS)
library(data.table)
library(plyr)
library(scales)

### Removing the legend
# use guides and specify the scales that will have the legend removed

p <- ggplot(PlantGrowth, aes(group, weight, fill = group)) + geom_boxplot()
p
p + guides(fill = F)
