rm(list = ls())
install.packages("boot")
require(ISLR)
require(boot)
?cv.glm
plot(mpg ~ horsepower, data = Auto)

### LOOCV
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.glm(glm.fit, data = Auto)
# leave one out fits a model repeatedly leaving one observation each time
# cv.glm does that by brute force
cv.glm(glm.fit, data = Auto)$delta
# 2ND number is a biased corrected number

LOOCV <- function(fit){
  h = lm.influence(fit)$h
  mean((residuals(fit)/(1 - h))^2)
} # this is the formula used for LOOCV as given in the vide to calculate the same value as above but 
# do it very fast

LOOCV(glm.fit)

cv.error <- rep(0, 5)
# we will use polynomials of degree 1 to 5 and do LOOCV
for (i in 1:5){
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}

cv.error
plot(1:5, cv.error, type = "b")

### 10 fold CV

cv.error10 <- rep(0, 10)
for (i in 1:10){
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

cv.error10
plot(cv.error10); lines(1:10, cv.error10, type = "b", col = "red")

cv.error5 <- rep(0, 5)
for (i in 1:5){
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error5[i] <- cv.glm(Auto, glm.fit, K = 5)$delta[1]
}

lines(1:5, cv.error5, type = "b", col = "red")

# in general, 10 fold is used over leave one out


### Bootstrap
# Bootstraping is done in 2 steps; creating a function that computes the statistic of interest and using boot()


alpha <- function(x, y){
  vx <- var(x)
  vy <- var(y)
  cxy <- cov(x, y)
  (vy - cxy)/(vx + vy - 2*cxy)
}
# value for alpha can be found by plugging in values for x and y
# what's the sampling variability of alpha? std error of alpha?

alpha(Portfolio$X, Portfolio$Y)
# this is the value of alpha

# we need to make a wrapper to make the bootstrap work

alpha.fn <- function(data, index)
  with(data[index, ], alpha(X, Y)) # the function returns an estimate for alpha base on applying the formula 
# to observations indexed by the argument index. the formula for minimizing portfolio risk given in the book

alpha.fn(Portfolio, 1:100)

set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T)) # randomly selects 100 observations from the range 1 to 100,
# with replacement
# we can implement the bootstrap analysis by performing this many times and recording the alpha every time and
# computing the standard deviation. the boot() function automates this!

boot.out <- boot(Portfolio, alpha.fn, R = 1000) # a thousand bootstraps
boot.out
plot(boot.out)
# very handy way of getting reliable estimates of standard error


# Estimating the accuracy of a linear regression model
boot.fn <- function(data, index) # {} is only needed for functions that are > 1 line long
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))

boot.fn(Auto, 1:392)
# boot.fn() can also be used to create bootstrap estimates for the intercept and slope terms by randomly 
# sampling from among the observations with replacement
boot.fn(Auto, sample(392, 392, T))

# using the boot function to compute the standard errors of a 1000 bootstrap estimates for the intercept and slope
boot(Auto, boot.fn, 1000)
summary(lm(mpg ~ horsepower, data = Auto))$coef

boot.fn <- function(data, index) # {} is only needed for functions that are > 1 line long
  return(coef(lm(mpg ~ horsepower + I(horsepower^2), data = data, subset = index)))
set.seed(1)
boot(Auto, boot.fn, 1000)

# problems
rm(list = ls())
load("E:/R/Scripts/ISLR Code/5.R.RData")
summary(lm(y ~ X1 + X2, data = Xy))
names(Xy)
?matplot
matplot(Xy,type="l")



boot.fn <- function(data, index)
  return(coef(lm(y ~ X1 + X2, data = Xy, subset = index)))

boot.fn(Xy, 1:1000)
boot.out <- boot(Xy, boot.fn, R = 1000) # a thousand bootstraps
boot.out


new.rows = c(101:200, 401:500, 101:200, 901:1000, 301:400, 1:100, 1:100, 801:900, 201:300, 701:800)

new.Xy = Xy[new.rows, ]
head(new.Xy)
boot.fn(Xy, 1:1000)
boot.out <- boot(new.Xy, boot.fn, R = 10) # a thousand bootstraps
boot.out


#Finally, use the block bootstrap to estimate s.e.(B^1). 
#Use blocks of 100 contiguous observations, and resample ten whole blocks with replacement 
#then paste them together to construct each bootstrap time series. For example, one of your 
#bootstrap resamples could be:
  
 # new.rows = c(101:200, 401:500, 101:200, 901:1000, 301:400, 1:100, 1:100, 801:900, 201:300, 701:800)

#new.Xy = Xy[new.rows, ]

#To within 10%, what do you get?