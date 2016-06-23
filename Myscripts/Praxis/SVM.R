# Support Vector Machines

# Optimal plane: plane at the middle of the two planes that have minimum distance between training examples

# the problem is to optimize the margin

install.packages("e1071")
library(e1071)

?svm
model.svm <- svm(formula = Species~., data = iris)
summary(model.svm)
plot(model.svm, iris, Petal.Width ~ Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))
head(iris)

# building a svm model to predict defaulters in bankloan dataset

head(bankloan)
str(bankloan)
bankloan$default <- as.numeric(as.character(bankloan$default))
bankloan.test <- bankloan[is.na(bankloan$default), ]

bankloan.train <- bankloan[!is.na(bankloan$default), ]
bankloan.train$default <- as.factor(bankloan.train$default)

head(bankloan.train)
set.seed(12345); trainindex = sample(700, 490)
trainData <- bankloan.train[trainindex, ]

validationData <- bankloan.train[-trainindex, ]

model.svm1 <- svm(default~. -ed, data = trainData, scale = T)
pred.svm1 <- predict(model.svm1, newdata = validationData)
classAgreement(table(validationData$default, pred.svm1))

tuneModel <- tune.svm(default~., data = trainData, gamma = 10^(-5:-1), cost = c(10, 25, 50, 100),
                      class.weights = c(c("1" = 0.6, "0" = 0.4), c("1" = 0.7, "0" = 0.3)))
summary(tuneModel)
new.svm.model <- svm(default~., data = trainData, cost = 25, kernel = "radial", gamma = 0.0001,
                     class.weights = c(c("1" = 0.6, "0" = 0.4)))
summary(new.svm.model)
pred.svm2 <- predict(new.svm.model, newdata = validationData)
classAgreement(table(validationData$default, pred.svm2))

table(validationData$default, pred.svm2)
# the model suggested by the tune model is more accurate

install.packages("carat")
caret::confusionMatrix(validationData$default, pred.svm2)

# How to increase dummy variables
??class.ind
library(nnet)
class.ind(bankloan.train$default)
