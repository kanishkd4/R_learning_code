# CORRELATIONS

?swiss
data(swiss)
cor(swiss)
round(cor(swiss), 2)
# we have correlations for all variables with each other
# we may want to gett hypothesis tests with CI

cor.test(swiss$Fertility, swiss$Education)
# we get the pearson's product moment correlatioon
# alternate hypothesisis true

# to get p values for the entire matrix
install.packages("Hmisc")
library(Hmisc)
# we need to take the data frame and convert it into a matrix
rcorr(as.matrix(swiss))
# two tables show up; first is the correlation table and the second is the p values
# a correlation coefficient is really flexible to look at associatoins between variables
# Look at the Hmisc package






# COMPUTING A BIVARIATE REGRESSION
# a very common approach
?trees
data(trees)
head(trees)
hist(trees$Girth)
plot(trees$Girth, trees$Height)
abline(lm(trees$Height ~ trees$Girth)) # this plots a regression line in the plot

# linear regression model
reg1 <- lm(Height ~ Girth, data = trees)
reg1 # gives us intercept and coeficcients
summary(reg1) # gives a lot more info
# residuals are indicators of how well the regression line works
# the std cut off of the p value is 0.05.it is lower here so girth is a good predictor of height
# r square tells us how much of the variation is described by the girth
# overall p value is the same as P value in girth as the regression is bivariate
# it will be different if we have more than one variable

# CI for coefficients
confint(reg1)

# predict values based on regression equations
predict(reg1)
# prediction of height for different values of girth

# we can also get CI for our prediction
predict(reg1, interval = "prediction") # CI for predicted height
# gives lower and upper value of the prediction with 95% CI

# regression diagnostics
lm.influence(reg1)
influence.measures(reg1)
?lm.influence
?influence.measures






# COMPARING MEANS WITH T TEST
?sleep
head(sleep)
sd <- sleep[, 1:2]
head(sd)
# good idea to plot before we do statistical tests
hist(sd$extra, col = "lightgray")
boxplot(extra ~ group, data = sd)

# independent 2 group t test
t.test(extra ~ group, data = sd)
# does a welch 2 sample t test
# p value is higher than 0.05; just a little

# t test with options
t.test(extra ~ group,
       data = sd,
       alternative = "less", # one tailed
       conf.level = 0.80) # 80% instead of 95%
# most of it is the same: t value and dof
# p value is different
# a directional test gives a statistically significant difference but the two way test does not
# CI goes from -inf to -0.8: inf just shows that it is a one tail test

# create two groups of random data in separate variables
x <- rnorm(30, mean = 20, sd = 5)
y <- rnorm(30, mean = 22, sd = 5)
t.test(x, y) # x and y are two variables in different data sets
# means are close
# we can run it a few times and we can see if it is significantly different at any time






# COMPARED PAIRED MEANS: PAIRED T TEST
# we can pair one group before and after
t1 <- rnorm(50, mean = 52, sd = 6) # time 1
diff <- rnorm(50, mean = 6, sd = 12) # Difference
t2 <- t1 + diff # time 2
hist(t1)
hist(diff)
hist(t2) 
boxplot(t1, t2) # t2 is much more spread out cause of high sd of diff

# Use MASS to create a parallel coordinate plot
install.packages("MASS")
library(MASS)
pairs <- data.frame(t1, t2)

parcoord(pairs, var.label = T)
# we can just do pairs function too but parcoord lets us use var.label
# are the lines on average are going up or down or if they are flat

# paired t test (with defaults)
t.test(t2, t1, paired = T)
# this comes out statistically significant

# paired t test (with options)
t.test(t2, t1,
       paired = T,
       mu = 6, # specify non 0 null value
       alternative = "greater", # one tail
       conf.level = 0.99) # 99% confidence level
# 6 is chosen as it is the mean difference chosen
# it is not significantly different from 6




# COMPARING MEANS WITH AVOVA
x1 <- rnorm(30, mean = 40, sd = 8)
x2 <- rnorm(30, mean = 41, sd = 8)
x3 <- rnorm(30, mean = 45, sd = 8)
x4 <- rnorm(30, mean = 45, sd = 8)
boxplot(x1, x2, x3, x4)
# this can be run a few times to get different data sets

xdf <- data.frame(cbind(x1, x2, x3, x4))
summary(xdf)

# stack data to get one column with outcome
xs <- stack(xdf)
View(xs)

# conduct one way anova
anova1 <- aov(values ~ ind, data = xs)
anova1
# we get the sum of squares, dof etc
summary(anova1)
# gives a lot more info
# this is not statistically significant
# a difference means that there are differences somewhere but doesn't tell us where the differneces are
# tests that can be used are shafey or bon fironi (spell check)
?p.adjust
# p.adjust will give us idea of tests

# TukeyHSD (post hawk comparison)
TukeyHSD(anova1)
# it compares every one of these groups
# here, no pair is statistically significant







# COMPAFRE PROPORTIONS
# need to create two vectors
n5 <- c(rep(100, 5)) # total no. of people in each trial group
x5 <- c(65, 60, 60, 50, 45) # no. of successes

prop.test(x5, n5)
# 5 sample test for equality of proportion without continuity correction
# this does a chi square test
# there is a statistically significant difference between the groups

# if there are only two groups, it even gives CI
n2 <- c(40, 40) # no. of trials
x2 <- c(30, 20) # no. of successes
prop.test(x2, n2, conf.level = 0.8)
# it shows a 25 % difference






# CREATING CROSSTABS FOR CATEGORICAL VARIABLES
?Titanic
Titanic
ftable(Titanic) # can also be converted to a flat table

tdf <- as.data.frame(lapply(as.data.frame.table(Titanic), function(x)
  rep(x, as.data.frame.table(Titanic)$Freq)))[, -5]
View(tdf)

# create contingency table
ttab <- table(tdf$Class, tdf$Survived)
ttab

# change frequencies to %
round(prop.table(ttab, 1), 2) * 100
# we see more % survived in first class
# we can also get column % or cell %

# Chi squared test
tchi <- chisq.test(ttab)
tchi
# very strongly significant
summary(tchi)
tchi$observed # same as the contingency table with the formula table
tchi$expected
tchi$residuals
tchi$stdres


?chisq.test




# CREATING ROBUST STATISTICS FOR BIVARIATE ASSOCIATIONS
help(package = "robustbase")
help(package = "robust")
help(package = "MASS")
help(package = "quantreg")

# robust stats are to remove outlier effects

install.packages("quantreg")
# it is a very sophisticated topic

require(quantreg)
?rq  # Help on "quantile regression" in quantreg package
data(engel)
?engel
attach(engel)

plot(income,  # Create plot frame
     foodexp,
     xlab = "Household Income",
     ylab = "Food Expenditure",
     type = "n", 
     cex = .5)
points(income,  # Points in plot
       foodexp,
       pch = 16,
       col = "lightgray")
taus <- c(.05, .1, .25, .75, .9, .95)  # Quantiles
xx <- seq(min(income), max(income), 100)  # X values
f <- coef(rq((foodexp)~(income), tau=taus))  # Coefficients
yy <- cbind(1, xx)%*%f  # Y values
for(i in 1:length(taus)){  # For each quantile value...
  lines(xx, yy[, i], col = "darkgray")  # Draw regression
}
abline(lm(foodexp ~ income),  # Standard LS regression
       col = "darkred",
       lwd = 2)
abline(rq(foodexp ~ income),  # Median regression
       col = "blue",
       lwd = 2)
legend(3000, 1000,  # Plot legend
       c("mean fit", "median fit"),
       col = c("darkred", "blue"),
       lty = 1,
       lwd = 2)

# Clean up
detach(engel)
detach("package:robust", unload=TRUE)
detach("package:quantreg", unload=TRUE)
detach("package:MASS", unload=TRUE)
detach("package:rrcov", unload=TRUE)
rm(list = ls())  
