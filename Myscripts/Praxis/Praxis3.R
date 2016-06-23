Cluster Analysis - Expectation Maximization

Likelihood function - function of parameteres of a model (used for data imputation)
It is not a probability function. has utlity in model building, prediction and inferences

Maximum likelihood estimation 

X <- c(rnorm(100, 15, 5), rnorm(50, 25, 3), rnorm(50, 5, 1))
library("mclust")

hist(X)
clust <- Mclust(X) #Mclust is EM clustering technique
plot(clust)
1
2
3
4

clust1 <- Mclust(X, G = 3)
4
?Mclust
0

plot(clust1)
4
0 #have to exit using 0 if we go into plot of an Mclust
# praxis dataset imported
str(clust)
head(clust)
head(autompg)

autompg1 <- autompg[, c(3:6)]
head(autompg1)
Clust.Auto <- Mclust(na.omit(autompg1))
# outliers can be a problem and they can be removed in Mclust. There are better methods to remove outliers

plot(Clust.Auto)
1
2
3
4
0
# we see than all models are being tested in BIC
# teacher says ellipsoidal; I acn't tell shit! (highest BIC apparently)

# http://manuals.bioinformatics.ucr.edu/home/programming-in-r

auto1 <- na.omit(auto)
trainindex <- sample(nrow(auto1), size = 100)
traindata <- auto1[trainindex, -c(1, 2, 5)]

# for categorical variable, decision tree is a good method for clustering

trainclass <- auto1[trainindex, "type"]
testdata <- auto1[-trainindex, -c(1, 2, 5)]

testclass <- auto1[-trainindex, "type"]

model <- MclustDA(traindata, trainclass, G = 1:2, modelNames = c("EII", "EEE"))

summary(model)
summary(model, newdata = testdata, newclass = testclass)
# the second sumamry tests it. training error is 0.2





