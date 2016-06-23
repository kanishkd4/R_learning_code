rm(list = ls())
library(e1071)
library(ISLR)

### Support Vector Classifier

# svm() function can be used to fit a support vector classifier when the argument kernel = "linear" is used. The cost argument lets us
# specify the cost of violation to the margin. when cost is small, the margin will be wide and many support vectors will be on it or
# will violate the margin. when cost is high, the margin will be narrow and there will be few support vectors on the margin or a few 
# will violate the margin

# we can build random observations which belong to 2 classes and check if they are linearly separable
set.seed(1)
x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1] <- x[y == 1] + 1
plot(x, col = (3 - y))
# it looks sort of separable but not perfecty separable by a linear boundary
# svm() can perform regression as well as classification. for classification, we need to convert the response into factors

# create a data frame with the response coded as a factor

dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = F)
# scale = F tells the function to not scale the data so every feature will have a mean of 0 and sd of 1. We may prefer to scale for 
# certain applications.

plot(svmfit, dat)
# the arguments to plot are the fitted object as well as the original data for plot.svm()
# the decision boundary is linear because of the kernel we have chosen. It looks jagged because of the way the plot function is implimented in the library
# the support vectors are plotted as crosses and other observations are plotted as circles. 
# we can check their identities as 
svmfit$index

summary(svmfit)

# what if we used a smaller value of cost?
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 0.1, scale = F)
plot(svmfit, dat)
svmfit$index
# we obtain a much larger number of support vectors when a smaller cost parameter is used as the margin is now wider

# e1071 includes a function tune() for cross validation
# by default, tune() performs 10 fold cross validation on a set of models of interest
# the following shows tune() for a set of costs for a linear kernel

set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
# we can access the cross validation errors for each of these models using the summary function

summary(tune.out)
# tune stores the best model obtained which can be accessed 
bestmod <- tune.out$best.model # an svm() output

# the predict function can be used to predict the class labels on a set of test observations
xtest <- matrix(rnorm(20 * 2), ncol = 2)
ytest <- sample(c(-1, 1), 20, replace = T)
xtest[ytest == 1,] <- xtest[ytest == 1] + 1
testdat <- data.frame(xtest, as.factor(ytest))
names(testdat) <- names(dat)
ypred <- predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y)
# it classified 4 incorrectly

# we can also test for other cost values
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 0.01, scale = F)
ypred <- predict(svmfit, testdat)
table(predict = ypred, truth = testdat$y)
# classified 6 incorrectly. 

# consider a situation where the classes are linearly separable. 
# in that case, we can find a separating hyperplane using the svm function
# we can further separate the two classes in our simulated data so they are linearly separable
x[y == 1,] <- x[y == 1,] + 0.5
plot(x, col = (y + 5)/2, pch = 19)
# the observations are just barely separable

# we fit the support vector classifier and plot the resulting hyperplane using a very large value of 
# cost so that no observations are misclassified

svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 1e5)
summary(svmfit)
plot(svmfit, dat)
# the model has very low training error and the margin is very small.
# however, as the margin is small, it might give a higher test error than one where the margin is big

# a higher cost and bigger margin would have more support vectors and probably perform better on a test set
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 1)
summary(svmfit)
plot(svmfit, dat)


### Support Vector Machine
# we use the same function but use a different kernel. kernels can be polynomial/radial
# the degree can be specified for a polynomial kernel and gamma for a radial one

# generating data with a non linear class boundary
set.seed(1)
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1:100,] <- x[1:100,] + 2
x[1:150,] <- x[1:150,] - 2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))
plot(x, col = y) # the boundary is non linear

train <- sample(200, 100)
svmfit <- svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat[train,])
summary(svmfit)
# there are a lot of training errors in the fit. we can increase the cost and reduce the number
# of training errors but at the cost of overfitting to the training data
svmfit <- svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 1, cost = 1e5)
plot(svmfit, dat[train,])
# we can use tune() to perform cross validation and choose the best value of cost and gamma with 
# a radial kernel
set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat[train,], kernel = "radial",
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000), 
                               gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)
# it gives the best performing gamma and cost combination; cost .1 and gamma .5

table(true <- dat[-train, "y"], pred <- predict(tune.out$best.model, newdata = dat[-train,]))
# misclassified 23%
# but the model is classifying all into 1 category only! stupid1


### ROC curves
# ROCR package can be used to produce ROC curves. 
# we first write a function to plot a ROC curve given a vector containing a numerical store for each observation, pred and 
# a vector containing the class label for each observation, truth

# install.packages("ROCR")
library(ROCR)
rocplot <- function(pred, truth, ...) {
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)
}

# SVM's and support vectors output class labels for each observation. It is also possible to obtain fitted values for each
# observation, which are numerical scores used to obtain class labels. 
# Relationship between fitted values and class predictions:
# If the fitted value exceeds 0 then the observation is assigned to one class, if it is less than 0 it is assigned to the other class
# to obtain the fitted values for a given SVM model fit, we use decision.values = T

svmfit.opt <- svm(y ~ ., data = dat[train,], kernel = "radial", gamma = .5, cost = .1, decision.values = T)
fitted <- attributes(predict(svmfit.opt, newdata = dat[train,], decision.values = T))$decision.values
par(mfrow = c(1, 2))
rocplot(fitted, dat[train, "y"], main = "training data")
# SVM seems to be doing well on the training data. By increasing gamma, we can produce a more flexible fit
# and generate further improvements in accuracy

svmfit.flex <- svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 25, cost = .1, decision.values = T)
fitted <- attributes(predict(svmfit.flex, newdata = dat[train,], decision.values = T))$decision.values
rocplot(fitted, dat[train, "y"], col = "red", add = T)
# these curves are on the training data but we are more interested in the level of prediction accuracy on the test set

fitted <- attributes(predict(svmfit.opt, newdata = dat[-train,], decision.values = T))$decision.values
rocplot(fitted, dat[train, "y"], main = "training data")

fitted <- attributes(predict(svmfit.flex, newdata = dat[-train,], decision.values = T))$decision.values
rocplot(fitted, dat[train, "y"], col = "red", add = T)
# the one with smaller gamma does better on the test set which shows overfitting by the larger value of gamma

### SVM's with multiple classes
# it will just do it without us needing to do anything special. The response just needs to have more than two classes as factors

set.seed(1)
x <- rbind(x, matrix(rnorm(50 * 2), ncol = 2))
y = c(y, rep(0, 50))
x[y == 0, 2] <- x[y == 0, 2] + 2
dat <- data.frame(x, as.factor(y))
par(mfrow = c(1, 1))
plot(x, col = (y + 1))
# we now fit an SVM to the data

svmfit <- svm(y ~ ., data = dat, kernel = "radial", cost = 10, gamma = 1)
plot(svmfit, dat)
# the e1071 library can also be used to perform regression if the response variable is numeric intead of factor

### Application to gene expression data
# the khan data set
names(Khan)
dim(Khan) # null as it is a pure list
dim(Khan$xtest)
# the data contains measurements for 2308 genes. training and test sets have 63 and 20 observations respectively
# we will use a support vector approach to predict cancer sub type
# the number of features in the data are very large as compared to the number of observations.
# this suggests we use a linear kernel because the additional flexibility that will result from
# using a polynomial or radial kernel is unnecessary

dat <- data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
out <- svm(y ~ ., data = dat, kernel = "linear", cost = 10)
summary(out)
table(out$fitted, dat$y)
# there are no training errors!
# the large number of variables relative to observations implies that it is easy to find hyperplanes that fully separate classes
# We are most interested not in support vector classifier's performance not on the training observations but the test set

dat.te <- data.frame(x = Khan$xtest, y = Khan$ytest)
pred.te <- predict(out, newdata = dat.te)
table(pred.te, dat.te$y)
# we see that there are two test errors

