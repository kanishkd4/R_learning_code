install.packages("gcookbook")
library(gcookbook)
library(ggplot2)
qplot(x = temperature, y = pressure, data = pressure, geom = "line")
#equivalent to 
ggplot(data = pressure, aes(x = temperature, y = pressure)) + geom_line()

barplot(table(mtcars$cyl))
qplot(BOD$Time, BOD$demand, geom = "bar", stat = "identity")
# convert x variable to factor so it can be treated as discrete
qplot(factor(BOD$Time), BOD$demand, geom = "bar", stat = "identity")
# this qplot formula no longer works... can still be used with ggplot
ggplot(data = BOD, aes(x = Time, y = demand)) + geom_bar(stat = "identity")

qplot(mtcars$cyl) # qplot by default makes a graph of counts
qplot(factor(mtcars$cyl))

qplot(mpg, data = mtcars, binwidth = 4)
# equivalent to
ggplot(mtcars, aes(mpg)) + geom_histogram(binwidth = 4)

# boxplot
plot(ToothGrowth$supp, ToothGrowth$len) # automatic boxplot when x is a factor
# quivalent to 
boxplot(len ~ supp, data = ToothGrowth)

#put interaction of 2 variables
boxplot(len ~ supp + dose, data = ToothGrowth)

qplot(x = supp, y = len, data = ToothGrowth, geom = "boxplot")
ggplot(ToothGrowth, aes(supp, len)) + geom_boxplot()
# we can make boxplots for multiple variables with interaction()
# numeric variables need to be converted to factors for grouping 

# Using three separate vectors
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len,
      geom="boxplot")
# Alternatively, get the columns from the data frame
qplot(interaction(supp, dose), len, data=ToothGrowth, geom="boxplot")
# This is equivalent to:
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len)) + geom_boxplot()

# base graphics methods differ as they contain quantiles

# Plot a user-defined function
myfun <- function(xvar) {
  1/(1 + exp(-xvar + 10))
}
curve(myfun(x), from=0, to=20)
# Add a line:
curve(1-myfun(x), add = TRUE, col = "red")

# the same resust can be obtained in ggplot2 using stat = "function" and geom = "line"



