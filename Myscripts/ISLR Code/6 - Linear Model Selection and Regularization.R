rm(list = ls())
library(ISLR)
summary(Hitters)
sum(is.na(Hitters$Salary))
contrasts(is.na(Hitters$Salary))

Hitters <- na.omit(Hitters) # to remove all NA values from Hitters
# install.packages("leaps")
library(leaps)

### Best subset regression
regfit.full <- regsubsets(Salary ~., data = Hitters) # regsubsets gives the best model using RSS; same as lm()
summary(regfit.full)
regfit.full = regsubsets(Salary ~., data = Hitters, nvmax = 19) #only goes to subsets of 8 but we want to take it to size 19
reg.summary = summary(regfit.full)
names(reg.summary)
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp") # 10 variables is the smallest
# min() can be used to find out the smallest
which.min(reg.summary$cp)
points(10,reg.summary$cp[10], pch = 20, col = "red") # plot can be annonated

reg.summary$adjr2

par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "no. of variables", ylab = "RSS", type = "l") # type = l tells R to connect the dots with a line
plot(reg.summary$adjr2, xlab = "no. of variables", ylab = "Adjusted R square", type = "l") # type = l tells R to connect the dots with a line

which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 2, pch = 20) # puts points on an existing plot

plot(reg.summary$cp, xlab = "no. of variables", ylab = "Cp", type = "l") # type = l tells R to connect the dots with a line
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = "red", cex = 2, pch = 20)

which.min(reg.summary$bic)

plot(reg.summary$bic, xlab = "no. of variables", ylab = "BIC", type = "l") # type = l tells R to connect the dots with a line
points(6, reg.summary$bic[10], col = "red", cex = 2, pch = 20)

# there is a plot method for regsubsets objects
plot(regfit.full,scale = "r2")
plot(regfit.full,scale = "adjr2")
plot(regfit.full,scale = "Cp")
plot(regfit.full,scale = "bic")
coef(regfit.full, 10)



par(mfrow = c(1, 1))
### Forward/Backward stepwise selection 

regfit.fwd <- regsubsets(Salary~., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary~., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)
plot(regfit.fwd,scale="Cp")

###Model Selection Using a Validation Set; can also use Cp, BIC/AIC or r squared
--------------------------------------- 
#  Lets make a training and validation set, so that we can choose a good subset model.
#We will do it using a slightly different approach from what was done in the the book.

dim(Hitters)
set.seed(1)
train <- sample(seq(263), 180, replace = FALSE)# sample 180 indexes of observations
train
regfit.fwd <- regsubsets(Salary~., data = Hitters[train,], nvmax=19, method="forward")

summary(regfit.fwd)


# Now we will make predictions on the observations not used for training. We know there are 19 models (as there are 19 possible x), 
# so we set up some vectors to record the errors. We have to do a bit of work here, because there is no predict method for regsubsets.

val.errors <- rep(NA, 19) # 19 models as there are 19 variables
x.test <- model.matrix(Salary~., data = Hitters[-train,])# notice the -index!; for test set
for(i in 1:19){
  coefi <- coef(regfit.fwd,id=i)
  pred <- x.test[,names(coefi)]%*%coefi
  val.errors[i] <- mean((Hitters$Salary[-train] - pred)^2)
}# there isn't a predict method for regsubsets
which.min(val.errors)
coef(regfit.fwd, 5)

plot(sqrt(val.errors), ylab = "Root MSE", ylim = c(300,400), pch = 19, type = "b")
points(sqrt(regfit.fwd$rss[-1]/180), col = "blue", pch = 19, type = "b")
legend("topright", legend = c("Training", "Validation"), col = c("blue", "black"), pch=19)

#As we expect, the training error goes down monotonically as the model gets bigger, but not so for the validation error.

#This was a little tedious - not having a predict method for regsubsets. So we will write one!
  
predict.regsubsets  <-  function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object,id <- id)
  mat[, names(coefi)] %*% coefi
}

# finally, we do subset selection on the whole data and to obtain more accurate coefficients and get the best 5
# variable model for forward selection


### Model Selection by Cross-Validation
#-----------------------------------
#  We will do 10-fold cross-validation. Its really easy!


set.seed(1)
folds <- sample(rep(1:10, length = nrow(Hitters)))
folds
table(folds)
cv.errors <- matrix(NA, 10, 19)
for(k in 1:10){
  best.fit <- regsubsets(Salary~., data = Hitters[folds!=k,], nvmax = 19, method = "forward")
  for(i in 1:19){
    pred <- predict(best.fit, Hitters[folds == k,], id = i)
    cv.errors[k, i] <- mean((Hitters$Salary[folds == k] - pred)^2)
  }
}
rmse.cv <- sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, pch = 19, type = "b")


### Ridge Regression and the Lasso
-------------------------------
# We will use the package glmnet, which does not use the model formula language, so we will set up an x and y.
# install.packages("glmnet")
library(glmnet)
x <- model.matrix(Salary~., data = Hitters) 
y <- Hitters$Salary

#First we will fit a ridge-regression model. This is achieved by calling glmnet with alpha=0 (see the helpfile). 
# There is also a cv.glmnet function which will do the cross-validation for us. 

fit.ridge <- glmnet(x, y, alpha = 0)
plot(fit.ridge,xvar = "lambda", label = TRUE)
cv.ridge <- cv.glmnet(x,y,alpha=0)
plot(cv.ridge)

grid <- 10^seq(10, -2, length = 100)
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid)
# we have chosen to perform ridge regression over a grid of lambda values from 10^10 to 10^-2
# glmnet standardizes the variables so that they are on the same scale 
# a vector of coefficients associated with each value of lambda can be called by coef()
dim(coef(ridge.mod))

ridge.mod$lambda[50] # when lambda  = 11497
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))

ridge.mod$lambda[60]# when lambda  = 705
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))

# we can do a lot with the predict() function; we can even get ridge regression coefficients for new values of lambda
predict(ridge.mod, s = 50, type = "coefficients")[1:20,]

# we can now split the data into training and test sets
# subsets can be chosen randomly by splitting the dataset into 2 by 2 ways
# 1. randomly allocating a value of true/flase to the data points and selecting one for the training set
# 2. randomly choose a subset of numbers between 1 and n and use these as indices for training observations

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2) # x is the model matrix
test <- (-train)
y.test <- y[test]
# we fit a ridge regression on the training set and run it on the test set using lambda = 4 and instead of type = "coefficients",
# we use the newx argument
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test,])
mean((ridge.pred - y.test)^2)

# if we had simply it a model with just an intercept, we would have predicted each test observation using the mean of 
#training observations
mean((mean(y[train]) - y.test)^2)

# Ridge regression with a very large value of lambda also gives the same result
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test,])
mean((ridge.pred - y.test)^2)
# hence, a ridge model with lambda = 4 gives a much lower test MSE than fitting the model with just the intercept.
# note that least squares is just ridge regression with lambda = 0

ridge.pred <- predict(ridge.mod, s = 0, newx = x[test,], exact = T) # exact used as value of lambda is differnt 
mean((ridge.pred - y.test)^2)

lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, newx = x[test,], exact = T, type = "coefficients")

# lm() function provides a much better output if we want to fit an unpenalized least squares model like std errors and p values

# using cross validation to choose lambda
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
# to find the test MSE associated with this value of lambda
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
mean((ridge.pred - y.test)^2)

# after coming up with the best model, we apply ridge regression on the wholw dataset
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam, newx = x[test,])

# Now we fit a lasso model; for this we use the default alpha=1 (lasso is code from the video, not book)

fit.lasso <- glmnet(x, y)
plot(fit.lasso, xvar = "lambda", label = TRUE)
cv.lasso <- cv.glmnet(x, y)
plot(cv.lasso)
coef(cv.lasso)


# Suppose we want to use our earlier train/validation division to select the lambda for the lasso.
# This is easy to do.

lasso.tr <- glmnet(x[train,], y[train])
lasso.tr
pred <- predict(lasso.tr, x[-train,])
dim(pred)
rmse <- sqrt(apply((y[-train] - pred)^2, 2, mean))
plot(log(lasso.tr$lambda), rmse, type="b", xlab="Log(lambda)")
lam.best=lasso.tr$lambda[order(rmse)[1]]
lam.best
coef(lasso.tr, s = lam.best)

### PCR and PSL 

### Principal components regression
# install.packages("pls")
# options(timeout = 999999)
library(pls)
# syntax for pcr is similar to lm()
set.seed(2)
pcr.fit <- pcr(Salary ~ ., data = Hitters, scale = T, validation = "CV") 
# scale  = T standardises each predictor; CV makes it do 10 fold cross validation for each value of M (the number of principal components used)

summary(pcr.fit)
# pcr gives the root mean squared error; have to square it to get MSE
# cross validation scores can also be plotted

validationplot(pcr.fit, val.type = "MSEP")
# M = 19 amounts to simply performing least squares. the lowest is at M = 16. It is also roughly the same when M = 1
# % variance explained is the % of variance explained in the predictors and in response using differnt number of components

# evaluating test set performance
set.seed(2)
pcr.fit <- pcr(Salary ~ ., data = Hitters, subset = train, scale = T, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
# we see that the lowest errors occur when M = 7 components are used

pcr.pred <- predict(pcr.fit, Hitters[test,], ncomp = 7) # error here so leavin it for now
mean((pcr.pred - y.test)^2)
# Finally, we fit a PCR on all the data using M = 7 as the final model


### partial Least squares
# syntax of plsr is like pcr
set.seed(1)
pls.fit <- plsr(Salary ~ ., data = Hitters, subset = train, scale = T, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
# lowest errors occur at M = 2

# for test set MSE
pls.pred <- predict(pls.fit, Hitters[test,], ncomp = 2)
mean((pls.pred - y.test)^2)
