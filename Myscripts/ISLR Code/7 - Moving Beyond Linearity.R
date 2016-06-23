
rm(list = ls())
library(ISLR)
# LOOCV can be very easily performed for smoothening splines to choose a value of lambda

head(Wage); names(Wage)
### polynomial regression and step functions

fit <- lm(wage ~ poly(age, 4), data = Wage)
coef(summary(fit))
?poly
# poly returns a matrix whose columns are a basis of orthogonal polynomials, which means that each 
# column is a linear combination of the variables age, age^2... age^4
# we can also use poly to obtain actual powers by using raw = T

View(poly(Wage$age, 4))
View(poly(Wage$age, 4, raw = T))

fit2 <- lm(wage ~ poly(age, 4, raw = T), data = Wage)
coef(summary(fit2)) # the coefficients change a little
# there are many other ways of fitting the same model; we can use I(age^4) or cbind(all 4 powers)

# we now create a grid of values for age at which we want predictions and call predict()

agelims <- range(Wage$age)
age.grid <- seq(from = agelims[1], to = agelims[2])
preds <- predict(fit, newdata = list(age = age.grid), se = T)
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

# finally, we plot the data and add the fit from the degree 4 polynomial
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(Wage$age, Wage$wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Degree-4 Polynomial", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

preds2 <- predict(fit2, newdata = list(age = age.grid), se = T)
max(abs(preds$fit - preds2$fit))

# in performing polynomial regression, we must decide on the degree of the polynomial to use.
# One way to do this is hypothesis tests. we look for the simplest model that is sufficient to 
# explain the relationship between wage and age. We use anova to test the null hypothesis 
# to use anova(), the models must be nested. predictors in M1 must be a subset of predictors in M2

fit.1 <- lm(wage ~ age, data = Wage)
fit.2 <- lm(wage ~ poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ poly(age, 3), data = Wage)
fit.4 <- lm(wage ~ poly(age, 4), data = Wage)
fit.5 <- lm(wage ~ poly(age, 5), data = Wage)

anova(fit.1, fit.2, fit.3, fit.4, fit.5)

# The p value comparing the linear model to the quadratc model is almost; hence, linear is not enough
# the quadratic as compared to the cubic model is also almost 0, hence, the quadratic is also not enough
# degree 4 model and cubic have a p value of 0.05
# hence, a cubic or quartic fit appear to provide a reasonable fit to the data, but lower or higher degree models are not justified

# in this specific case, since poly() creates orthogonal polynomials, we could have obtained the same
# p values without anova()
coef(summary(fit.5)) # note that p values are the same
# anova() works better whether or not we use orthogonal polynomials

# instead of using anova, we can also use cross validation

# consider the task of predicting whether an individual earns more than 250k a year
fit <- glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = "binomial")
preds <- predict(fit, newdata = list(age = age.grid), se = T)
# calculating the confidence interval is slightly more involved than lm()
# the default predictor is type = "link" and we get predictions for the logit (in log form)
# transformations have to be used as below
pfit <- exp(preds$fit)/(1 + exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
se.bands <- exp(se.bands.logit)/(1 + exp(se.bands.logit))
# we could have directly computed probabilities by using type = "response" in predict() but 
# the corresponding confidence intervals would not have been sensible as we would have ended u
# with negetive probabilities

plot(Wage$age, I(Wage$wage > 250), xlim = agelims, type = "n", ylim = c(0, .2))
points(jitter(Wage$age), I((Wage$wage > 250)/5), cex = .5, pch = "|", col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 3, col = "blue", lty = 3)
# we have drawn age values corresponding to wage values above 250 at the top of the plot
# 250 or below at the bottom of the plot. used a jitter so that the same age values don't overlap

# to fit a step function, we use cut()
table(cut(Wage$age, 4))
# it automatically picked up the cut points which can also be manually specificied using the breaks option

fit <- lm(wage ~ cut(age, 4), data = Wage); coef(summary(fit))
# age < 33.5 category is left out so intercept can be interpreted as the average salary for those 
# under 33.5 years of age and other coefficients can be interpreted as averaeg additional salary for
# those in other age groups. predictions and plots can be made just like in case of polynomial regression

### Splines
library(splines)
# regression splines can be fit constructing an approximate matrix of basis functions.
# the bs() function generates the entire matrix of basis functions  for splines with the specified set of knots
# by default, cubic splines are produced. 

fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
pred <- predict(fit, newdata = list(age = age.grid), se = T)
plot(Wage$age, Wage$wage, col = "gray")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2 * pred$se, lty = "dashed")
lines(age.grid, pred$fit - 2 * pred$se, lty = "dashed")
# the 3 knots produce a spline with 6 basis functions ( a cubic spline with 3 knots has 7 degrees of freedom)
# we could also use the df option to produce a spline with knots at uniform quantities of data

dim(bs(Wage$age, knots = c(25, 40, 60)))
dim(bs(Wage$age, df = 6))
attr(bs(Wage$age, df = 6), "knots")
# the knots chosen by the software correspond to the 25th, 50th and 75th percentiles of age
# bs() also has a degree argument so we can fit splines of any degree rather than the default of 3

# to fit a natural spline, we use ns()
fit2 <- lm(wage ~ ns(age, df = 4), data = Wage)
pred2 <- predict(fit2, newdata = list(age = age.grid), se = T)
lines(age.grid, pred2$fit, col = "red", lwd = 2) # can also specify the knots as with the bs() function

# to fit a smoothing spline, we use smooth.spline() function
plot(Wage$age, Wage$wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(Wage$age, Wage$wage, df = 16)
fit2 <- smooth.spline(Wage$age, Wage$wage, cv = T)
fit2$df
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"), col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

# to perform local regresion, we use loess() function
plot(Wage$age, Wage$wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Local Regression")
fit <- loess(wage ~ age, span = .2, data = Wage)
fit2 <- loess(wage ~ age, span = .5, data = Wage)
lines(age.grid, predict(fit, newdata = data.frame(age = age.grid), col = "red", lwd = 2))
lines(age.grid, predict(fit2, newdata = data.frame(age = age.grid), col = "blue", lwd = 2))
legend("topright", legend = c("span = .2", "span = .5"), col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)
# lines not giving colour here (maybe check later, or screw it)


### GAM's
# we now fit a GAM to predict the wage using natural spline functions of year and age, treating
# education as a qualitative predictor. this is just a big linear regression model using an
# appropriate choice of basis functions, we can simply use lm()

gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)
# install.packages("gam")
library(gam)
# we can also fit the model using smoothing splines instead of natural splines
# to fit more general sort of gam's, using components that cannot be expressed in terms of basis functions
# and then fit linear regression, we use the library(gam)
# the s() function is used to specify we want to use a smoothing spline;
# we can also specify the degrees of freedom and use the gam() function to fit a gam using the components as shown below

gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)
par(mfrow = c(1, 3))
plot(gam.m3, se = T, col = "blue")
# plot recognises gam and runs plot.gam(). we can also run plot.gam if we use lm()
plot.gam(gam1, se = T, col = "red")
# the function of year looks rather linear. 
# we can use a series of anova() tests to determine which of the 3 models is the best
# 1. a gam that excludes year
# 2. a gam that uses a linear function of year
# 3. a gam that uses a spline function of year

gam.m1 <- gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 <- gam(wage ~ year + s(age, 5) + education, data = Wage)
anova(gam.m1, gam.m2, gam.m3, test = "F")
# we see that m2 is significantly better than m1 (one that does not include year at all)
# but there is no evidence that a non linear function may be needed
# based on anova, m2 may be preffered

summary(gam.m3)
# the p values for year and age correspond to a null hypothesis of a linear relationship vs the
# alternative hypothesis of a non-linear relationship. It is clear that a non-linear term is required 
# for age, but not for year
# predictions can be made using predict() for class gam just like for class lm()

preds <- predict(gam.m2, newdata = Wage)

# we can also use lo() to use local regression fits as building blocks in a gam
gam.lo <- gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education, data = Wage)
plot.gam(gam.lo, se = T, col = "green")
# we have used local regression for the age term; we can also use lo() to create interactions before
# calling the gam() function
gam.lo.i <- gam(wage ~ lo(age, year, span = 0.5) + education, data = Wage)
# this fits a two term model, first being an interaction between year and age, fit by 
# a local regression surface
# the two dimensional surface can be plotted using the package akima
# 1
library(akima)
plot(gam.lo.i)

# to plot a logistic regression model, we use I() to construct a binary response variable
gam.lr <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, data = Wage, family = "binomial")
par(mfrow = c(1, 3))
plot(gam.lr, se = T, col = "green")
table(Wage$education, I(Wage$wage > 250))
# as there are no high earners in lower education categories, we can also fit a logistic regression
# model using all by this category

gam.lr.s <- gam(I(wage > 250) ~ year + s(age, 5) + education, data = Wage, family = "binomial",
                subset = (education != "1. < HS Grad"))
plot(gam.lr.s, se = T, col = "green")
