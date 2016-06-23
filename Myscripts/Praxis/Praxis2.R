# https://onlinecourses.science.psu.edu/stat505/node/138

str(autompg)
str(auto)
with(auto, plot(horsepow, price)) #praxis dude code
plot(x = auto$horsepow, auto$price) #code I learnt to do the same thing

# pairs makes multiple graphs to show all relationships. Realle awesome even if not very useful
pairs(auto[, c("price", "horsepow", "curb_wgt", "engine_s")])

bp <- boxplot(x = auto$price, xlab = "Price", main = "Which car do I buy?")
View(auto)

bp$out #this directly shows outliers
# boxplot outliers by default are q3 + 1.5*IQR and q1 - 1.5*IQR
str(bp)
View(bp)

boxplot(auto$price ~ auto$type)

hist(auto$price)

hist(lapply(unique(auto$type), function(x) hist(auto$price[auto$type == x])))

par(mfrow = c(1,2))

install.packages("igraph")
install.packages("pvclust") #heirarchial clustering with P values
install.packages("mclust")
install.packages("cluster")
install.packages("fpc")
install.packages("RWeka")
install.packages("NbClust")
install.packages("snow")
# code shall be emailed to us for graphs

# Clustering (K mean clustering)

# K means clustering assumes that data is wellcontained withing a multi dimensional cue without any
# irregular pattern. K mean clustering can fail if applied to the wrong data set

# we apply different clustering algorithms and seee which one works (K means is the simplest and fastest)

# partiotioning algorithm is the most preffered for k mean cluistering. if the data points are extremely
# sparse, k mean falis. the sparse matrix needs to be converted into a dense matrix for it to work
# otherwise, it will fail

# rnorm can create a random variable from a normal distribution

ex <- matrix(c(rnorm(50, 20, 2), rnorm(20, 8, 3), rnorm(50, 10, 2), rnorm(20, 25, 3)), nrow = 70)
plot(ex)
View(ex)

par(mfrow = c(1,1))

?mfrow
?par

# running k mean clustering on data points

cluster <- kmeans(ex, centers = 2)
plot(ex, pch = 20, xlab = "1st", ylab = "2nd", col = cluster$cluster)
points(cluster$centers, pch = "+", col = c("red", "blue", cex = 2))


str(cluster)

# k means can give differnt results every time we run it
# the solution is to run the k mean cluster mnultiple times to get the optimal cluster by giving an nstart

cluster <- kmeans(ex, centers = 4, nstart = 30)
plot(ex, pch = 19, xlab = "1st", ylab = "2nd", col = cluster$cluster, cex = .5)

# Deciding the optimal no. of clusters

wss <- lapply(1:15, function(x) kmeans(ex, centers = x, nstart = 30)$tot.withinss)
plot(1:15, wss, type = "l", xlab = "no. of clusters", ylab = "total within SS")

df <- na.omit(auto)
df1 <- df[, -c(1, 2, 5)]

wss <- with(df1, lapply(1:15, function(x) kmeans(df1, x, nstart = 30)$tot.withinss))
#the above code is only storing withinss

plot(1:15, wss, type = "l") # l stands for line; 5 is the optimal no. of clusters

?with

wss <- lapply(1:15, function(x) kmeans(df1, x, nstart = 30)$tot.withinss)
plot(1:15, wss, type = "l") # This code to do it wihtutu with

cluster5 <- kmeans(df1, 5, nstart = 30)
str(cluster5)
cluster5$centers


# in RWeka, there is a function "xmeans"

library("RWeka")
xclust <- XMeans(x = df1)

install.packages("XMeans")

# K mediod method.. better than mean (mediod is not median)

str(auto)
auto1 <- auto
auto1$manufact <- as.character(auto1$manufact)
str(auto1)
?pam # partitioning around medoids

df_medoid <- pamk(auto1$manufact)
str


library("sensitivity")
install.packages("sensitivity")

# heirarchichal clustering
# two measures of distance (cosine and euclidian); both genrally give different results

# cutree function in R

?cutree
# if the user is completely at a loss, density based clustering can work

# bootsttrapping can be used to check clustering
# it calculates whether the formation of a cluster is significantly differnet than the other clusters

library("pvclust")

autompg <- na.omit(autompg[1:50, c(3:6, 9)])
library(snow)
cl <- makeCluster(10, "mpi")
pvclust <- parPvclust(cl, t(autompg), "com", "euc", nboot = 1000)
plot(pvclust)
pvrect(pvclust, alpha = 0.95)

#dendogram showsus significance (as in the ppt)

# gower dist

autompg1 <- autompg[, -1]
for(i in c(1, 6, 7)) autompg1[, i] <- as.factor(autompg[, i])
d <- daisy(autompg1, metric = 'gower')
h1 <- hclust(d, method = 'complete')
plot(h1, hang = -1)
tree = cutree(h1, k = 2)
tree
