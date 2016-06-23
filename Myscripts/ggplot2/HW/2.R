set.seed(1410)
?set.seed
dsmall <- diamonds[sample(nrow(diamonds), 100), ]
library("ggplot2")
qplot(x = diamonds$carat, y = diamonds$price)
qplot(x = log(diamonds$carat), y = log(diamonds$price))
qplot(x = carat, y = x * y * z, data = diamonds)
qplot(x = carat, y = price, data = dsmall, colour = color) # gives colour based on column "color"
qplot(x = carat, y = price, data = dsmall, shape = cut) #gives shape based on column "cut"
qplot(x = carat, y = price, data = dsmall, colour = I("red"), size = I(2))

qplot(x = carat, y = price, data = diamonds, alpha = I(1/10))
qplot(x = carat, y = price, data = diamonds, alpha = I(1/100))
# geom = geometric object
# Adding a smoother to the plot
?qplot
qplot(x = carat, y = price, data = dsmall, geom = c("point", "smooth")) # small so uses loess
qplot(x = carat, y = price, data = diamonds, geom = c("point", "smooth")) # lasrge so uses gam
?
#loess: local polynomial regression fitting
#gam: generalized additive models with integrated smoothness resolutio (library(mgcv))
#pointwise confidence interval shown in grey. se = FALSE to turn off
# sue method argument to decide method

#span to control wiggliness of the line
qplot(x = carat, y = price, data = dsmall, geom = c("point", "smooth"), span = 0.2)
qplot(x = carat, y = price, data = dsmall, geom = c("point", "smooth"), span = 1)
library("splines")
library("mgcv")
# we can use formula with gam

qplot(x = carat, y = price, data = dsmall, geom = c("point", "smooth"), 
      method = "gam", formula = y ~ s(x))
qplot(x = carat, y = price, data = dsmall, goem = c("point", "smooth"),
      method = "gam", formula = y ~ s(x, bs = "cs"))
qplot(x = carat, y = price, data = dsmall, geom = c("point", "smooth"),
      method = "lm")
qplot(x = carat, y = price, data = dsmall, geom = c("point", "smooth"),
      method = "lm", formula = y ~ ns(x,5))# 2nd parameter is degrees of fredoms (here: 5)
# rlm works like lm, but uses a more robust fitting algorithm so that outliers don't affect
# the fit as much; part of "MASS"package
library("MASS")
qplot(x = carat, y = price, data = dsmall, geom = c("point", "smooth"),
      method = "rlm")
# boxplots and jitterplots
qplot(x = color, y = price/carat, data = diamonds, geom = "boxplot")
qplot(x = color, y = price/carat, data = diamonds, geom = "jitter", alpha = I(1/20))
# jitter density can be changed using alpha; boxplot is still better

qplot(carat, data = diamonds, geom = "histogram")
qplot(carat,data = diamonds, geom = "density")
# density uses adjust to smooth out the line, histogram uses binwidth for width

#aesthetic mapping can be added
qplot(x = carat, data = diamonds, geom = "density", colour = color)
qplot(carat, data = diamonds, geom = "histogram", fill = color)
#mapping categorical variale to an aesthetic automatically splits up the geom by the variable

# bar charts
qplot(color, data = diamonds, geom = "bar")
qplot(color,  data = diamonds, geom = "bar", weight = carat) + scale_y_continuous("carat")
# 2nd is a bar chart of diamond color weighted by carat... total weight of diamonds of each colour
table(diamonds$color)
tapply(X = diamonds$carat, INDEX = diamonds$color, FUN = sum)
# used the above 2 to figure out the bar charts

# time series with line and path plots

qplot(x = date, y = unemploy/pop, data = economics, geom = "line")
qplot(x = date, y = uempmed, data = economics, geom = "line")

# we can draw both time series on the same plot to compare with each other
# we have to join points adjacent in time with line segments forming a path plot

year <- function(x) as.POSIXlt(x)$year + 1900
qplot(x = unemploy/pop, y = uempmed, data = economics, geom = c("point", "path"))
qplot(x = unemploy/pop, y = uempmed, data = economics, geom = "path", colour = year(date))
      + scale_area()

?plotmath

