rm(list = ls())
library(ISLR)
library(MASS)
names(Boston)
plot(medv ~ lstat, Boston)
fit1 <- lm(medv ~ lstat, data = Boston)
fit1
summary(fit1)
abline(fit1, col = "red")
names(fit1)
confint(fit1)
predict(fit1, data.frame(lstat = c(5, 10, 15)), interval = "confidence")

### Multiple Linear regression

fit2 <- lm(medv ~ lstat + age, data = Boston)
summary(fit2)
fit3 <- lm(medv ~ ., Boston)
summary(fit3)
par(mfrow = c(2, 2))# 2 x 2 as there are 4 plots
plot(fit3)
# residuals vs fitted values. we can look for non linearities
fit4 <- update(fit3, ~. - age - indus)
summary(fit4)

### non linear terms and interactions
fit5 <- lm(medv ~ lstat*age, Boston); summary(fit5)
fit6 <- lm(medv ~ lstat + I(lstat^2), Boston); summary(fit6)
# I is identity function

par(mfrow = c(1, 1))
plot(Boston$medv ~ Boston$lstat)
# cannot use abline now as this it works for a straight line fit only
points(Boston$lstat, fitted(fit6), col = "red", pch = 0)
?points
fit7 <- lm(medv ~ poly(lstat^4), Boston)
points(Boston$lstat, fitted(fit7), col = "blue", pch = 0)
plot(1:20, 1:20, pch = 1:20, cex = 2)

### Qualitative predictors

fix(Carseats)
fit1 <- lm(Sales ~ . + Income:Advertising + Age:Price, Carseats)
summary(fit1)
contrasts(Carseats$ShelveLoc) # shows how R uses a categorical variable in the model
?contrasts

### Writing R funtions
regplot <- function(x, y){
  fit = lm(y ~ x)
  plot(x, y)
  abline(fit, col = "red")
}

regplot(Carseats$Price, Carseats$Sales)

regplot <- function(x, y, ...){
  fit = lm(y ~ x)
  plot(x, y, ...)
  abline(fit, col = "red")
}
# ... means unnamed arguments; 
regplot(Carseats$Price, Carseats$Sales, xlab = "Price", ylab = "Sales", col = "blue", pch = 20)
