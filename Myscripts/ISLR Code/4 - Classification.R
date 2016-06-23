rm(list= ls())
library(ISLR)
summary(Smarket)
pairs(Smarket, col = Smarket$Direction)
cor(Smarket[, -9]) # 9 is not numeric

glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
               data = Smarket, family = binomial)
summary(glm.fit) # nothing is significant here

glm.probs <- predict(glm.fit, type = "response")
# type = "response tells R to give the output in the form of probabilities
contrasts(Smarket$Direction) # this tells that 1 is up

glm.probs[1:5]

glm.pred <- ifelse(glm.probs> 0.5, "Up", "Down")
table(glm.pred, Smarket$Direction)
mean(glm.pred == Smarket$Direction)

# the above is trainin error; test error needs to be on different datasets
train <- Smarket$Year < 2005
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
               data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fit, newdata = Smarket[!train,], type = "response")
glm.probs[1:5]
glm.pred <- ifelse(glm.probs> 0.5, "Up", "Down")
DIrection.2015 <- Smarket$Direction[!train]
table(glm.pred, DIrection.2015)
mean(glm.pred == DIrection.2015)
mean(glm.pred != DIrection.2015)

glm.fit <- glm(Direction ~ Lag1 + Lag2, 
               data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fit, newdata = Smarket[!train,], type = "response")
glm.probs[1:5]
glm.pred <- ifelse(glm.probs> 0.5, "Up", "Down")
DIrection.2015 <- Smarket$Direction[!train]
table(glm.pred, DIrection.2015)
mean(glm.pred == DIrection.2015)
# samller model is better predictor here
summary(glm.fit) # still, nothing became significant


### LDA

require(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = Year < 2005)
lda.fit
plot(lda.fit)
Smarket.2005 <- subset(Smarket, Year == 2005)
lda.pred <- predict(lda.fit, Smarket.2005)
lda.pred[1:5]
class(lda.pred) # this is a list
data.frame(lda.pred)[1:5,]
table(lda.pred$class, Smarket.2005$Direction)
mean(lda.pred$class == Smarket.2005$Direction) # not huge but might be a decent edge in the stock market

sum(lda.pred$posterior[, 1] >= .5)
sum(lda.pred$posterior[, 1] < .5)

### QDA
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit
# the output has grouped means but not the coefficient of LDA
qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, DIrection.2015)
mean(qda.class == DIrection.2015) # accurate almost 60% of the time

?options

### K nearest neighbours classification
install.packages("class")
library(class)
?knn
Xlag <- cbind(Smarket$Lag1, Smarket$Lag2) # making a training set matrix
train <- Smarket$Year < 2005
set.seed(1)
knn.pred <- knn(Xlag[train, ], Xlag[!train, ], Smarket$Direction[train], k = 1)

table(knn.pred, Smarket$Direction[!train])
mean(knn.pred == Smarket$Direction[!train])

knn.pred <- knn(Xlag[train, ], Xlag[!train, ], Smarket$Direction[train], k = 3)

table(knn.pred, Smarket$Direction[!train])
mean(knn.pred == Smarket$Direction[!train])

# An application to caravan insurance data
dim(Caravan)
summary(Caravan$Purchase) # target variable
348/5822 # prior probability
# scale is important in KNN classification and has an effect on distance between observations
# if one variable has a very high range, it will have a much larger effect than one with lower range (eg age/income)
# to nullify the effects, we can standardise the data (exclusing column 86, ie Purchase)

standardised.X <- scale(Caravan[, -86])
var(Caravan[, 1])
var(Caravan[, 2])
var(standardised.X[, 1])
var(standardised.X[, 2])
# every column has a mean of 0 and sd of 1

test <- 1:1000
train.X <- standardised.X[-test,]
test.X <- standardised.X[test,]
train.Y <- Caravan$Purchase[-test]
test.Y <- Caravan$Purchase[test]
set.seed(1)

knn.pred <- knn(train.X, test.X, train.Y, 1)
mean(knn.pred != test.Y) # error is 11.8% but the prior probability is less than 6%
# error rate would be 6% if we always predicted no
# knn here does far better than random guessing as conversion would increase from 6% to 11.2% for that small base
# which can be approached first, even though it is only 77/1000 customers

table(knn.pred, test.Y)
# k = 3/5 would further increase accuracy here; but as the accuracy increases, the base goes down
# hence, the sizing and capacity should also be considered

knn.pred <- knn(train.X, test.X, train.Y, 3)
table(knn.pred, test.Y)
knn.pred <- knn(train.X, test.X, train.Y, 5)
table(knn.pred, test.Y)



