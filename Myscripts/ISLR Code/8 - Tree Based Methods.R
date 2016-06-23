rm(list = ls())
library(ISLR)
library(tree)

### Classification Trees

High <- ifelse(Carseats$Sales <= 8, "No", "Yes")
Carseats <- data.frame(Carseats, High)

# making a simple tree
tree.carseats <- tree(High ~ . - Sales, data = Carseats)
# the summary function lists the variables that were used 
summary(tree.carseats)
# the misclassification error rate is the training error
plot(tree.carseats)
text(tree.carseats, pretty = 0) # pretty = 0 instructs R to include category names for any 
# qualitative predictors, rather than simply displaying a letter for each category
# just typing the name of the tree object, R prints the output corresponding to each branch 
# of the tree. It displays the split criteria, no. of observations, deviance, overall prediction
# for that branch adn the fraction of observations in that branch that take on values of yes or no
tree.carseats

# but we need to estimate the test error instead of the training error
# split the observations and use predict. in case of classification, we use type = "class"

set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
High.test <- High[-train]
tree.carseats <- tree(High ~ .-Sales, data = Carseats, subset = train)
tree.pred <- predict(tree.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
mean(tree.pred == High.test)
# training error is way higher than test error. high overfitting

# we will see if pruning the tree helps us reduce the test error by using cross validation to
# determine the optimal level of complexity. Cost complexity pruning is is used to select a sequence
# of trees for consideration. 

# the default guide for the classification and pruning process in cv.tree is deviance, but we
# want to use misclassification error as the guide, so we use FUN = prune.misclass

# the object used here will be the old tree built
set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
# The function gives the size of tree, deviance, cost complexity parameter used (k corresponds to alpha)
cv.carseats
# dev here corresponds to the cross validation error rate 
# the tree with 9 terminal nodes has the lowest cv error rate getting 50 cv errors
# we can plot the error rate as a function of size and k

par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

# once we know which tree we want, we can use the prune.misclass() function to prune the tree as
# we want to know how well the pruned tree performs with the test set

prune.carseats <- prune.misclass(tree.carseats, best = 9) # to fit the best 9 node tree
# the tree object here needs to be the tree on the subset train

plot(prune.carseats)
text(prune.carseats, pretty = 0)

tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(High.test, tree.pred)
mean(High.test == tree.pred)
# We get an accuracy of 77%, over the earlier 72 % on the unpruned tree
# we can increase the value of best to get the bigger tree but but the classification accuracy 
# will drop
prune.carseats <- prune.misclass(tree.carseats, best = 15) # to fit the best 15 node tree

plot(prune.carseats)
text(prune.carseats, pretty = 0)

tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(High.test, tree.pred)
mean(High.test == tree.pred) # goes down to 74%. barely better than the overfitted tree


### Regression Trees
library(MASS)
head(Boston)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv ~ ., Boston, subset = train)
summary(tree.boston)
# we see that only 3 variables have been used to construct the tree
# the dev is simply the sum of squared errors of the tree; here it is 12.65 (training error)

plot(tree.boston)
text(tree.boston, pretty = 0)
# now, we use cv.tree to see if pruning it will improve performance
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b")
# in this case, the most complex tree is chosen by cross validation; we can still prune the tree if we want

prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)
# we can check the test errors of these trees

yhat <- predict(tree.boston, newdata = Boston[-train,])
boston.test <- Boston[-train, "medv"] # damn! data frames and data tables are different

plot(yhat, boston.test)
abline(0, 1)
mean((yhat - boston.test)^2) # test error of 25.05

# checking for pruned tree
yhat_pruned <- predict(prune.boston, newdata = Boston[-train,])
mean((yhat_pruned - boston.test)^2) # a little higher than the best tree but simpler and easier to understand


### Bagging and random forests
library(randomForest)
# bagging is just a special case of random forest with m = p. the same function can be used to perform both
set.seed(1)
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13, importance = T)
# mtry = 13 means that all 13 variables will be used
bag.boston
# to see how well the bagged model performs at the test set

yhat.predict <- predict(bag.boston, newdata = Boston[-train,])
plot(yhat.predict, boston.test)
abline(0, 1)
mean((yhat.predict - boston.test)^2) #M*****Fucker!

# the test MSE is almost half of the optimally pruned single tree!
# we can change the number of trees by using ntree
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13, ntree = 25)
yhat.predict <- predict(bag.boston, newdata = Boston[-train,])
mean((yhat.predict - boston.test)^2) # little increase in MSE but still much better than single tree

# growing a random forest proceeds the same way, except we use a smaller value in mtry argument.
# By default, random forest uses p/3 variables when building regression trees and square root of p
# variables while using classification trees; here we use mtry = 6

set.seed(1)
rf.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 6, importance = T)
yhat.rf <- predict(rf.boston, newdata = Boston[-train,])
mean((yhat.rf - boston.test)^2) # a little better than bagging

# using the importance function, we can view the importance of each variable. 
importance(rf.boston)

# two measures of variable importance are reported. 
# %IncMSE is bases upon the mean decrease of accuracy in predictions on the out of bag samples when
# a given variable is excluded from the model. 
# IncNodePurity is a measure of total decrease in node impurity that results from from splits over that 
# variable. averaged over all trees. for regression trees, node impurity is measured by the training RSS.
# For classification trees, the node impurity is measured by the deviance. Plots of the importance 
# measures can be producde using the varImpPlot() function

varImpPlot(rf.boston)
# it indicates that the wealth level of the community(lstat) and house size(rm) are by far
# the most important variables


### Boosting
# install.packages("gbm")
library(gbm)
# we will fit boosted regression trees on the Boston data set 
# we use distribution = "gaussian" for regression problems and distribution = "bernoulli" for binary classification problems
# n.trees gives the number of trees and interaction.depth limits the depth of each tree
set.seed(1)
boost.boston <- gbm(medv ~ ., data = Boston[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
# summary() produces a relative influence plot and outputs the relative influence statistics

summary(boost.boston)
# lstat and rm are the most important variables. we can also produce partial dependance plots for these two variables. 
# these plots illustrate the matginal effect of the selected variables on the response after integrating out other variables
par(mfrow = c(1, 2))
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")
# median house prices are increasing with rm and decraesing with lstat

# checking the boosted model for predictions
yhat.boost <- predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test)^2)

# the test MSE is similar to random forest and better than bagging. we can perform boosting with a different value of the 
# shrinkage parameter. the default is 0.001

boost.boston <- gbm(medv ~ ., data = Boston[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4, 
                    shrinkage = 0.2, verbose = F)
yhat.boost <- predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test)^2) # reduces the test error! but a small enough difference that may not matter depending on the data


