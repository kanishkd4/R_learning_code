# calculate frequencies for a categorical variable
rm(list = ls())
groups <- c(rep("blue", 3990),
            rep("red", 4140),
            rep("orange", 1890),
            rep("green", 3770),
            rep("purple", 855))

# create frequencies table
t1 <- table(groups)
t1 # it changes things into alphavetical order
t1 <- sort(t1, decreasing = T)
t1 # sorted in descending frequencies

# we can also get proportion
round(prop.table(t1), 4) * 100 # perfect percentage only without the % sign





# Calculatuing descriptives
require(datasets)
?cars
cars
# we have sped and distances
data(cars) # to load the dataset into variables
summary(cars)
summary(cars$speed)
fivenum(cars$speed)
# fivnumber summary doesn't give labels
# minimum, lower-hinge, median, upper-hinge, maximum

boxplot.stats(cars$speed)
# this is the same as the fivenum summary

help(package = "psych")
require("psych")
describe(cars)
# describe is more advanced than summary!!
# gives trimmed mean, skewness, kurtosis as well as std error






# Single Proportion: hypothesis test and confidence
# baseball data. is 60% significantly greater than 50%

# PROP TEST
prop.test(98, 162)
# it assumes a null probability of 0.5
# gives dof and p-value and 95% confidence interval
# this is a 2 tailed test

# One tailed test with 90% CI
prop.test(98, 162, alt = "greater", conf.level = .90)
# because we are looking at greater than 50%, the alternate hypothesis has to be greter
# we have a different alternate hypothesis
# this is the most basic inferencial test







# Single mean: HYpothesis test and CI
?quakes
head(quakes)
mag <- quakes$mag
head(mag)
# we will do a one sample t test. the default of a t test compares the mean of the sample to a hypothesized value and the default is 0
t.test(mag)
# a very small p value, the 95% CI is very narrow
# null is assumed as 0

# one sided t test
t.test(mag, alternative = "greater", mu = 4)
# t value is much lower; it is a one sided CI
? t.test
rm(list = ls())







# SINGLE CATEGORICAL VARIABLE
?HairEyeColor
str(HairEyeColor)
HairEyeColor
margin.table(HairEyeColor, 2)
eyes <- margin.table(HairEyeColor, 2)

round(prop.table(eyes), 2)

# Pearson;'s chi square test
# need one dimensional goodness of fit test

# default test
chi1 <- chisq.test(eyes)
chi1
# it gives x-sqauared, df and p value (very small)


# Compare to population distribution
# Population data from:
browseURL("http://www.statisticbrain.com/eye-color-distribution-percentages/")
# Approximate proportions:
#  Brown: .41 (Combining Brown Irises with Specks & Dark Brown Irises)
#  Blue:  .32 (Blue / Grey Irises)
#  Hazel: .15 (Blue / Grey / Green Irises with Brown / Yellow Specks)
#  Green  .12 (Green / Light Brown Irises with Minimal Specks)
# p = c(.41, .32, .15, .12)

# the bottom line is a vector of population proportion
chi2 <- chisq.test(eyes, p = c(.41, .32, .15, .12))
chi2
# we provide explicit proportions to the test instead of it assuming 25% for all
# chi square was 133 earlier, but is now 6.47 compared to 133
# this group os students does not differ statistically from the population as the p value is now 0.09







# ROBUST STATISTICAL FOR UNIVARIATE ANALYSIS
browseURL("http://j.mp/12YPV5L") # paper on robust analysis
# can also look up robust procedures at CRAN

?state.area
area <- state.area
area
hist(area)
# we can see an outlier
boxplot(area)
boxplot.stats(area)
# boxplots can show more outliers
summary(area)
# mean is much higher than median (very strong positive skewness)

mean(area) # not robust
median(area)
# trimmed mean(throw away a part of the data at the ends)
mean(area, trim = 0.05) # 5% trimmed mean(10% trimmed total)

# robust methods for describing variation
sd(area) # standard deviation; not robust
# sd here is higher than the mean

mad(area) # meadian absolute deviation
IQR(area) # interquartile range (can select many methods)
fivenum(area)

rm(list = ls())


