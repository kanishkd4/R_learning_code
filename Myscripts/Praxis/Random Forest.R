
# random forest is for multiple decision trees. maybe as flexible as SVM but just as difficult to interpret
# bagging and random forests
# bagging reduces the variance of base classifiers
# bagging has a lower propensity to overfit than boosting
# issues with bagging: in ppt
# improvement is possible only by reducing the variation
# Random forest can also be used for regression

install.packages("MASS")
library(MASS)
??sgm
install.packages("randomForest")
library(randomForest)
library(e1071)
install.packages("ipred")
library(ipred)
data(fgl)

install.packages("party")
str(fgl)
library(party)

# we will try 4 models
# Conditional binary tree
# 
# SVM
# Random Forest
?errorest
er.NB <- sapply(1:10, function(x) errorest(type~.,
                                             data = fgl,
                                             model = naiveBayes,
                                             estimator = "cv")$error)

er.tree <- sapply(1:10, function(x) errorest(type~.,
                                             data = fgl,
                                             model = ctree,
                                             estimator = "cv")$error)

er.sv <- sapply(1:10, function(x) errorest(type~.,
                                             data = fgl,
                                             model = svm,
                                             estimator = "cv")$error)
er.rf <- sapply(1:10, function(x) errorest(type~.,
                                             data = fgl,
                                             model = randomForest,
                                             estimator = "cv")$error)

compResult <- round(as.data.frame(cbind(er.NB, er.tree, er.sv, er.rf)), 4)
summary(compResult)
# Random forest is performing better than others

# NB is performing poorly as variables are not distributed as per gausian distribution
?randomForest
# mtry is the tuning parameter
# the m value is important for improving accuracy
# we may have to tune the operator
# tuning by caret package
# randonForest gave a mean 20% error with default mtry

# we can supply out own mtry value and we can see which value of mtry to use by caret
install.packages("caret")
library(caret)

table(fgl$type)
cntrl <- trainControl(method = "cv", number = 5)
model.rf <- train(type~.,
             data = fgl,
             method = "rf",
             trControl = cntrl,
             metric = "Kappa",
             tuneGrid = expand.grid(.mtry = seq(2, 7)))

?train
# tuneGrid is telling which parameters to tune for (mtry is the only tuning parameter for randomForest)

model.rf
# Kappa is maximum for 3; very close for 4
# > 0.5 Kappa values are considered to be reasonable good

pred.rf <- predict(model.rf, newdata = fgl)
confusionMatrix(fgl$type, pred.rf)
# Accuracy is 1 as the training and test data is same

set.seed(12345); index = createDataPartition(y = fgl$type, p = 0.8, list = F)
traindata <- fgl[index,]
testdata <- fgl[-index,]


head(traindata)
model.rf.train <- train(type~.,
                  data = traindata,
                  method = "rf",
                  trControl = cntrl,
                  metric = "Kappa",
                  tuneGrid = expand.grid(.mtry = seq(2, 7)))
model.rf.train
pred.rf.train <- predict(model.rf.train, newdata = testdata)
confusionMatrix(testdata$type, pred.rf.train)

varImp(model.rf.train)

# Working on bankloan dataset
head(bankloan)
loandata <- bankloan
loandata$default <- as.factor(as.numeric(as.character(loandata$default)))

table(loandata$default)
loandata$ed <- as.factor(loandata$ed)

unknowndata <- loandata[is.na(loandata$default), ]
knowndata <- loandata[!is.na(loandata$default), ]

set.seed(12345); index = createDataPartition(knowndata$default, p = 0.8, list = F)
trainset <- knowndata[index,]
testset <- knowndata[-index,]

cntrl.loan <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

model.loan.rf <- train(default~., data = trainset,
                       method = "rf",
                       metric = "Kappa",
                       trControl = cntrl.loan,
                       tuneGrid = expand.grid(.mtry = seq(2, 7)))


model.loan.nnet <- train(default~., data = trainset,
                       method = "nnet",
                       metric = "Kappa",
                       trControl = cntrl.loan,
                       tuneGrid = expand.grid(.size = seq(3, 6), .decay = c(0.05, 0.07, 0.09, 0.11)),
                       maxit = 1000)


model.loan.NB <- train(default~., data = trainset,
                       method = "nb",
                       metric = "Kappa",
                       trControl = cntrl.loan)
# there are warnings for NB
# caret cannot use svm

pred.rf <- predict(model.loan.rf, newdata = testset)
pred.nnet <- predict(model.loan.nnet, newdata = testset)
pred.nb <- predict(model.loan.NB, newdata = testset)

# again warnings for NB
warnings()
model.loan.NB
# on running the model, we see that false is not a number

# trying wit GLM
model.loan.glm <- train(default~., data = trainset,
                       method = "glm",
                       metric = "Kappa",
                       trControl = cntrl.loan)
pred.glm <- predict(model.loan.glm, newdata = testset)

(classAgreement(table(pred.rf, testset$default)))$kappa
(classAgreement(table(pred.nnet, testset$default)))$kappa
(classAgreement(table(pred.glm, testset$default)))$kappa

# glm (logistic regression) is better than nnet for this dataset
# nnet is better than rf

# we can use all the 3 models and combine them for the analysis

combinedResult <- data.frame(RF = pred.rf, 
                             NNET = pred.nnet,
                             GLM = pred.glm)


final.pred <- apply(combinedResult, 1, function(x) names(which.max(table(x))))

names(which.max(table(as.matrix(combinedResult))))
table(as.matrix(combinedResult))
(classAgreement(table(final.pred, testset$default)))$kappa
# when do we use accuracy and when do we use kappa values?

